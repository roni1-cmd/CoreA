/*
 * CoreA Slab Allocator
 * 
 * A high-performance memory allocator for fixed-size objects.
 * Implements the slab allocation algorithm for efficient memory management
 * with reduced fragmentation and fast allocation/deallocation.
 *
 * Author: Ron Asnahon
 * License: To be licensed
 */

#include <kernel/mm/slab.h>
#include <kernel/mm/pmm.h>
#include <kernel/mm/vmm.h>
#include <kernel/mm/heap.h>
#include <kernel/kernel.h>
#include <kernel/panic.h>
#include <lib/string.h>
#include <lib/list.h>
#include <lib/stdio.h>

/* Slab allocator configuration */
#define SLAB_MAGIC          0x5LAB5LAB
#define SLAB_MIN_OBJECTS    8
#define SLAB_MAX_OBJECTS    512
#define SLAB_ALIGN_SIZE     16
#define SLAB_COLOR_MAX      64
#define SLAB_BUFCTL_END     0xFFFFFFFF

/* Slab flags */
#define SLAB_FLAGS_NONE     0x00
#define SLAB_FLAGS_POISON   0x01
#define SLAB_FLAGS_DEBUG    0x02
#define SLAB_FLAGS_HWCACHE  0x04

/* Cache flags */
#define CACHE_FLAGS_NONE    0x00
#define CACHE_FLAGS_POISON  0x01
#define CACHE_FLAGS_DEBUG   0x02
#define CACHE_FLAGS_RECLAIM 0x04

/* Buffer control structure for tracking free objects */
typedef uint32_t slab_bufctl_t;

/* Slab structure - tracks a contiguous block of memory */
struct slab {
    struct list_head list;          /* List of slabs in cache */
    void *s_mem;                    /* Start of object data */
    slab_bufctl_t free;             /* Index of first free object */
    uint32_t inuse;                 /* Number of allocated objects */
    uint32_t magic;                 /* Magic number for validation */
    uint32_t colouroff;             /* Color offset for cache line alignment */
};

/* Cache structure - manages slabs of same-sized objects */
struct slab_cache {
    struct list_head list;          /* List of all caches */
    struct list_head slabs_partial; /* Partially used slabs */
    struct list_head slabs_full;    /* Fully used slabs */
    struct list_head slabs_free;    /* Empty slabs */
    
    char name[32];                  /* Cache name */
    size_t object_size;             /* Size of each object */
    size_t align;                   /* Object alignment */
    uint32_t flags;                 /* Cache flags */
    uint32_t num_objs;              /* Objects per slab */
    uint32_t gfporder;              /* Pages per slab (2^order) */
    uint32_t colour;                /* Number of cache colors */
    uint32_t colour_off;            /* Color offset increment */
    uint32_t colour_next;           /* Next color to use */
    
    /* Statistics */
    uint32_t num_active;            /* Active objects */
    uint32_t num_allocations;       /* Total allocations */
    uint32_t num_frees;             /* Total frees */
    uint32_t slab_count;            /* Number of slabs */
    
    /* Constructor/destructor functions */
    void (*ctor)(void *obj);
    void (*dtor)(void *obj);
    
    spinlock_t lock;                /* Cache lock */
};

/* Global slab system state */
static struct {
    struct list_head cache_list;    /* List of all caches */
    struct slab_cache *cache_cache; /* Cache for cache structures */
    struct slab_cache *slab_cache;  /* Cache for slab structures */
    spinlock_t lock;                /* Global slab system lock */
    bool initialized;               /* Initialization flag */
} slab_system;

/* Forward declarations */
static struct slab *slab_create(struct slab_cache *cache, uint32_t flags);
static void slab_destroy(struct slab_cache *cache, struct slab *slab);
static void *slab_alloc_from_slab(struct slab_cache *cache, struct slab *slab);
static void slab_free_to_slab(struct slab_cache *cache, struct slab *slab, void *obj);
static size_t slab_calculate_objects(size_t size, size_t align, size_t *left_over);
static uint32_t slab_calculate_order(size_t size);

/*
 * Calculate the optimal number of objects per slab
 */
static size_t slab_calculate_objects(size_t size, size_t align, size_t *left_over)
{
    size_t objects = 0;
    size_t waste = PAGE_SIZE;
    int order;
    
    /* Try different slab orders to find the best fit */
    for (order = 0; order <= 5; order++) {
        size_t slab_size = PAGE_SIZE << order;
        size_t obj_size = ALIGN_UP(size, align);
        size_t mgmt_size = sizeof(struct slab);
        size_t bufctl_size = 0;
        size_t obj_count;
        size_t current_waste;
        
        /* Calculate buffer control overhead if objects are small */
        if (obj_size < sizeof(slab_bufctl_t)) {
            bufctl_size = sizeof(slab_bufctl_t);
        }
        
        /* Calculate number of objects that fit */
        obj_count = (slab_size - mgmt_size) / (obj_size + bufctl_size);
        
        if (obj_count < SLAB_MIN_OBJECTS) {
            continue;
        }
        
        if (obj_count > SLAB_MAX_OBJECTS) {
            obj_count = SLAB_MAX_OBJECTS;
        }
        
        /* Calculate waste */
        current_waste = slab_size - mgmt_size - (obj_count * (obj_size + bufctl_size));
        
        if (current_waste < waste) {
            waste = current_waste;
            objects = obj_count;
            if (left_over) {
                *left_over = current_waste;
            }
        }
        
        /* If waste is acceptable, stop searching */
        if (waste <= slab_size / 8) {
            break;
        }
    }
    
    return objects;
}

/*
 * Calculate the optimal page order for a slab
 */
static uint32_t slab_calculate_order(size_t size)
{
    uint32_t order = 0;
    size_t objects;
    size_t waste;
    
    while (order <= 5) {
        objects = slab_calculate_objects(size, SLAB_ALIGN_SIZE, &waste);
        if (objects >= SLAB_MIN_OBJECTS) {
            break;
        }
        order++;
    }
    
    return order;
}

/*
 * Initialize the slab allocator system
 */
int slab_init(void)
{
    if (slab_system.initialized) {
        return 0;
    }
    
    /* Initialize global state */
    INIT_LIST_HEAD(&slab_system.cache_list);
    spinlock_init(&slab_system.lock);
    
    /* Create cache for cache structures */
    slab_system.cache_cache = slab_cache_create("slab_cache", 
                                               sizeof(struct slab_cache),
                                               SLAB_ALIGN_SIZE,
                                               CACHE_FLAGS_NONE,
                                               NULL, NULL);
    if (!slab_system.cache_cache) {
        return -ENOMEM;
    }
    
    /* Create cache for slab structures */
    slab_system.slab_cache = slab_cache_create("slab_slab",
                                              sizeof(struct slab),
                                              SLAB_ALIGN_SIZE,
                                              CACHE_FLAGS_NONE,
                                              NULL, NULL);
    if (!slab_system.slab_cache) {
        return -ENOMEM;
    }
    
    slab_system.initialized = true;
    
    printk(KERN_INFO "Slab allocator initialized\n");
    return 0;
}

/*
 * Create a new slab cache
 */
struct slab_cache *slab_cache_create(const char *name, size_t size, size_t align,
                                    uint32_t flags, void (*ctor)(void *),
                                    void (*dtor)(void *))
{
    struct slab_cache *cache;
    size_t left_over;
    
    if (!name || size == 0 || align == 0) {
        return NULL;
    }
    
    /* Allocate cache structure */
    if (slab_system.cache_cache) {
        cache = slab_cache_alloc(slab_system.cache_cache);
    } else {
        /* Bootstrap allocation */
        cache = heap_alloc(sizeof(struct slab_cache));
    }
    
    if (!cache) {
        return NULL;
    }
    
    /* Initialize cache structure */
    memset(cache, 0, sizeof(struct slab_cache));
    strncpy(cache->name, name, sizeof(cache->name) - 1);
    cache->object_size = ALIGN_UP(size, align);
    cache->align = align;
    cache->flags = flags;
    cache->ctor = ctor;
    cache->dtor = dtor;
    
    /* Initialize lists */
    INIT_LIST_HEAD(&cache->slabs_partial);
    INIT_LIST_HEAD(&cache->slabs_full);
    INIT_LIST_HEAD(&cache->slabs_free);
    
    /* Calculate slab parameters */
    cache->gfporder = slab_calculate_order(cache->object_size);
    cache->num_objs = slab_calculate_objects(cache->object_size, align, &left_over);
    
    /* Calculate cache coloring parameters */
    cache->colour = left_over / align;
    if (cache->colour > SLAB_COLOR_MAX) {
        cache->colour = SLAB_COLOR_MAX;
    }
    cache->colour_off = align;
    cache->colour_next = 0;
    
    /* Initialize spinlock */
    spinlock_init(&cache->lock);
    
    /* Add to global cache list */
    spin_lock(&slab_system.lock);
    list_add(&cache->list, &slab_system.cache_list);
    spin_unlock(&slab_system.lock);
    
    return cache;
}

/*
 * Destroy a slab cache
 */
void slab_cache_destroy(struct slab_cache *cache)
{
    struct slab *slab, *tmp;
    
    if (!cache) {
        return;
    }
    
    /* Remove from global list */
    spin_lock(&slab_system.lock);
    list_del(&cache->list);
    spin_unlock(&slab_system.lock);
    
    spin_lock(&cache->lock);
    
    /* Free all slabs */
    list_for_each_entry_safe(slab, tmp, &cache->slabs_free, list) {
        slab_destroy(cache, slab);
    }
    
    list_for_each_entry_safe(slab, tmp, &cache->slabs_partial, list) {
        slab_destroy(cache, slab);
    }
    
    list_for_each_entry_safe(slab, tmp, &cache->slabs_full, list) {
        slab_destroy(cache, slab);
    }
    
    spin_unlock(&cache->lock);
    
    /* Free cache structure */
    if (slab_system.cache_cache && cache != slab_system.cache_cache) {
        slab_cache_free(slab_system.cache_cache, cache);
    } else {
        heap_free(cache);
    }
}

/*
 * Create a new slab for a cache
 */
static struct slab *slab_create(struct slab_cache *cache, uint32_t flags)
{
    struct slab *slab;
    void *page;
    size_t slab_size = PAGE_SIZE << cache->gfporder;
    slab_bufctl_t *bufctl;
    int i;
    
    /* Allocate pages for the slab */
    page = pmm_alloc_pages(cache->gfporder);
    if (!page) {
        return NULL;
    }
    
    /* Map pages to kernel virtual space */
    page = (void *)((uintptr_t)page + KERNEL_VIRTUAL_BASE);
    
    /* Allocate slab structure */
    if (slab_system.slab_cache) {
        slab = slab_cache_alloc(slab_system.slab_cache);
    } else {
        slab = heap_alloc(sizeof(struct slab));
    }
    
    if (!slab) {
        pmm_free_pages((void *)((uintptr_t)page - KERNEL_VIRTUAL_BASE), 
                      cache->gfporder);
        return NULL;
    }
    
    /* Initialize slab structure */
    slab->magic = SLAB_MAGIC;
    slab->colouroff = cache->colour_next * cache->colour_off;
    slab->s_mem = (char *)page + slab->colouroff;
    slab->inuse = 0;
    slab->free = 0;
    
    /* Update color for next slab */
    cache->colour_next++;
    if (cache->colour_next >= cache->colour) {
        cache->colour_next = 0;
    }
    
    /* Initialize free list */
    bufctl = (slab_bufctl_t *)((char *)slab->s_mem + 
                              (cache->num_objs * cache->object_size));
    
    for (i = 0; i < cache->num_objs - 1; i++) {
        bufctl[i] = i + 1;
    }
    bufctl[cache->num_objs - 1] = SLAB_BUFCTL_END;
    
    /* Initialize objects with constructor */
    if (cache->ctor) {
        for (i = 0; i < cache->num_objs; i++) {
            void *obj = (char *)slab->s_mem + (i * cache->object_size);
            cache->ctor(obj);
        }
    }
    
    /* Poison objects if debugging */
    if (cache->flags & CACHE_FLAGS_POISON) {
        for (i = 0; i < cache->num_objs; i++) {
            void *obj = (char *)slab->s_mem + (i * cache->object_size);
            memset(obj, 0x5A, cache->object_size);
        }
    }
    
    return slab;
}

/*
 * Destroy a slab
 */
static void slab_destroy(struct slab_cache *cache, struct slab *slab)
{
    void *page;
    int i;
    
    if (!slab || slab->magic != SLAB_MAGIC) {
        return;
    }
    
    /* Call destructor for all objects */
    if (cache->dtor) {
        for (i = 0; i < cache->num_objs; i++) {
            void *obj = (char *)slab->s_mem + (i * cache->object_size);
            cache->dtor(obj);
        }
    }
    
    /* Remove from list */
    list_del(&slab->list);
    
    /* Free pages */
    page = (void *)((uintptr_t)slab->s_mem - slab->colouroff - KERNEL_VIRTUAL_BASE);
    pmm_free_pages(page, cache->gfporder);
    
    /* Clear magic */
    slab->magic = 0;
    
    /* Free slab structure */
    if (slab_system.slab_cache && slab != slab_system.slab_cache) {
        slab_cache_free(slab_system.slab_cache, slab);
    } else {
        heap_free(slab);
    }
    
    cache->slab_count--;
}

/*
 * Allocate an object from a specific slab
 */
static void *slab_alloc_from_slab(struct slab_cache *cache, struct slab *slab)
{
    void *obj;
    slab_bufctl_t *bufctl;
    
    if (slab->free == SLAB_BUFCTL_END) {
        return NULL;
    }
    
    /* Get object address */
    obj = (char *)slab->s_mem + (slab->free * cache->object_size);
    
    /* Update free list */
    bufctl = (slab_bufctl_t *)((char *)slab->s_mem + 
                              (cache->num_objs * cache->object_size));
    slab->free = bufctl[slab->free];
    slab->inuse++;
    
    /* Clear object if poisoning */
    if (cache->flags & CACHE_FLAGS_POISON) {
        memset(obj, 0, cache->object_size);
    }
    
    return obj;
}

/*
 * Free an object to a specific slab
 */
static void slab_free_to_slab(struct slab_cache *cache, struct slab *slab, void *obj)
{
    slab_bufctl_t *bufctl;
    slab_bufctl_t objnr;
    
    /* Calculate object number */
    objnr = ((char *)obj - (char *)slab->s_mem) / cache->object_size;
    
    /* Poison object if debugging */
    if (cache->flags & CACHE_FLAGS_POISON) {
        memset(obj, 0x6B, cache->object_size);
    }
    
    /* Update free list */
    bufctl = (slab_bufctl_t *)((char *)slab->s_mem + 
                              (cache->num_objs * cache->object_size));
    bufctl[objnr] = slab->free;
    slab->free = objnr;
    slab->inuse--;
}

/*
 * Allocate an object from a cache
 */
void *slab_cache_alloc(struct slab_cache *cache)
{
    struct slab *slab;
    void *obj = NULL;
    
    if (!cache) {
        return NULL;
    }
    
    spin_lock(&cache->lock);
    
    /* Try to allocate from partial slabs first */
    if (!list_empty(&cache->slabs_partial)) {
        slab = list_first_entry(&cache->slabs_partial, struct slab, list);
        obj = slab_alloc_from_slab(cache, slab);
        
        /* Move to full list if slab is now full */
        if (slab->inuse == cache->num_objs) {
            list_move(&slab->list, &cache->slabs_full);
        }
    }
    /* Try to allocate from free slabs */
    else if (!list_empty(&cache->slabs_free)) {
        slab = list_first_entry(&cache->slabs_free, struct slab, list);
        obj = slab_alloc_from_slab(cache, slab);
        
        /* Move to appropriate list */
        if (slab->inuse == cache->num_objs) {
            list_move(&slab->list, &cache->slabs_full);
        } else {
            list_move(&slab->list, &cache->slabs_partial);
        }
    }
    /* Create new slab if needed */
    else {
        slab = slab_create(cache, 0);
        if (slab) {
            obj = slab_alloc_from_slab(cache, slab);
            
            /* Add to appropriate list */
            if (slab->inuse == cache->num_objs) {
                list_add(&slab->list, &cache->slabs_full);
            } else {
                list_add(&slab->list, &cache->slabs_partial);
            }
            
            cache->slab_count++;
        }
    }
    
    if (obj) {
        cache->num_active++;
        cache->num_allocations++;
    }
    
    spin_unlock(&cache->lock);
    
    return obj;
}

/*
 * Free an object to a cache
 */
void slab_cache_free(struct slab_cache *cache, void *obj)
{
    struct slab *slab = NULL;
    struct list_head *lists[] = {
        &cache->slabs_partial,
        &cache->slabs_full,
        &cache->slabs_free
    };
    int i;
    
    if (!cache || !obj) {
        return;
    }
    
    spin_lock(&cache->lock);
    
    /* Find the slab containing this object */
    for (i = 0; i < 3; i++) {
        struct slab *s;
        list_for_each_entry(s, lists[i], list) {
            if (obj >= s->s_mem && 
                obj < (char *)s->s_mem + (cache->num_objs * cache->object_size)) {
                slab = s;
                break;
            }
        }
        if (slab) break;
    }
    
    if (!slab) {
        printk(KERN_WARNING "Object %p not found in cache %s\n", obj, cache->name);
        spin_unlock(&cache->lock);
        return;
    }
    
    /* Free object to slab */
    slab_free_to_slab(cache, slab, obj);
    
    /* Move slab to appropriate list */
    if (slab->inuse == 0) {
        list_move(&slab->list, &cache->slabs_free);
    } else if (slab->inuse == cache->num_objs - 1) {
        /* Was full, now partial */
        list_move(&slab->list, &cache->slabs_partial);
    }
    
    cache->num_active--;
    cache->num_frees++;
    
    spin_unlock(&cache->lock);
}

/*
 * Shrink a cache by freeing empty slabs
 */
int slab_cache_shrink(struct slab_cache *cache)
{
    struct slab *slab, *tmp;
    int freed = 0;
    
    if (!cache) {
        return 0;
    }
    
    spin_lock(&cache->lock);
    
    /* Free empty slabs */
    list_for_each_entry_safe(slab, tmp, &cache->slabs_free, list) {
        slab_destroy(cache, slab);
        freed++;
    }
    
    spin_unlock(&cache->lock);
    
    return freed;
}

/*
 * Get cache information
 */
void slab_cache_info(struct slab_cache *cache, struct slab_cache_info *info)
{
    if (!cache || !info) {
        return;
    }
    
    spin_lock(&cache->lock);
    
    strncpy(info->name, cache->name, sizeof(info->name) - 1);
    info->object_size = cache->object_size;
    info->objects_per_slab = cache->num_objs;
    info->active_objs = cache->num_active;
    info->num_objs = cache->slab_count * cache->num_objs;
    info->active_slabs = (!list_empty(&cache->slabs_partial) ? 
                         list_count(&cache->slabs_partial) : 0) +
                        (!list_empty(&cache->slabs_full) ? 
                         list_count(&cache->slabs_full) : 0);
    info->num_slabs = cache->slab_count;
    info->pages_per_slab = 1 << cache->gfporder;
    
    spin_unlock(&cache->lock);
}

/*
 * List all caches (for debugging)
 */
void slab_cache_list(void)
{
    struct slab_cache *cache;
    struct slab_cache_info info;
    
    printk(KERN_INFO "Slab Cache Information:\n");
    printk(KERN_INFO "Name                 Size  PPSlab  ObjSize  Active    Total    Slabs\n");
    
    spin_lock(&slab_system.lock);
    list_for_each_entry(cache, &slab_system.cache_list, list) {
        slab_cache_info(cache, &info);
        printk(KERN_INFO "%-20s %4zu  %6u  %7zu  %6u  %7u  %6u\n",
               info.name, info.pages_per_slab, info.objects_per_slab,
               info.object_size, info.active_objs, info.num_objs, info.num_slabs);
    }
    spin_unlock(&slab_system.lock);
}

/*
 * Initialize pre-defined caches for common sizes
 */
int slab_init_general_caches(void)
{
    static const size_t sizes[] = {
        32, 64, 96, 128, 192, 256, 512, 1024, 2048, 4096
    };
    int i;
    
    for (i = 0; i < ARRAY_SIZE(sizes); i++) {
        char name[32];
        snprintf(name, sizeof(name), "kmalloc-%zu", sizes[i]);
        
        if (!slab_cache_create(name, sizes[i], SLAB_ALIGN_SIZE,
                              CACHE_FLAGS_NONE, NULL, NULL)) {
            return -ENOMEM;
        }
    }
    
    printk(KERN_INFO "General purpose slab caches initialized\n");
    return 0;
}
