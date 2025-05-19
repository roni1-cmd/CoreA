#include <kernel/mm/heap.h>
#include <kernel/mm/vmm.h>
#include <kernel/mm/pmm.h>
#include <kernel/kernel.h>
#include <lib/string.h>
#include <lib/stdio.h>

// Pool configuration
#define POOL_MAGIC              0xDEADPOOL
#define POOL_BLOCK_MAGIC        0xB10CB10C
#define POOL_MIN_BLOCKS         8
#define POOL_MAX_BLOCKS         65536
#define POOL_ALIGNMENT          sizeof(void*)

// Pool block header (for free blocks)
typedef struct pool_block {
    uint32_t magic;             // Magic number for validation
    struct pool_block *next;    // Next free block
} pool_block_t;

// Pool validation macros
#define POOL_VALIDATE(pool) \
    do { \
        if (!pool || pool->magic != POOL_MAGIC) { \
            kernel_panic("Invalid memory pool: %p", pool); \
        } \
    } while(0)

#define POOL_BLOCK_VALIDATE(block) \
    do { \
        if (!block || block->magic != POOL_BLOCK_MAGIC) { \
            kernel_panic("Invalid pool block: %p", block); \
        } \
    } while(0)

// Static function declarations
static void pool_init_blocks(memory_pool_t *pool);
static void pool_add_free_block(memory_pool_t *pool, void *block);
static void *pool_remove_free_block(memory_pool_t *pool);
static bool pool_contains_block(memory_pool_t *pool, void *ptr);
static void pool_validate_integrity(memory_pool_t *pool);

/**
 * Create a new memory pool for fixed-size allocations
 * 
 * @param block_size Size of each block in bytes
 * @param block_count Number of blocks to allocate
 * @param flags Pool creation flags
 * @return Pointer to created pool or NULL on failure
 */
memory_pool_t *pool_create(uint32_t block_size, uint32_t block_count, uint32_t flags) {
    // Validate parameters
    if (block_size == 0 || block_count == 0) {
        printf("pool_create: Invalid parameters (size=%u, count=%u)\n", 
               block_size, block_count);
        return NULL;
    }
    
    if (block_count < POOL_MIN_BLOCKS || block_count > POOL_MAX_BLOCKS) {
        printf("pool_create: Block count out of range (%u)\n", block_count);
        return NULL;
    }
    
    // Ensure minimum block size for free block header
    if (block_size < sizeof(pool_block_t)) {
        block_size = sizeof(pool_block_t);
    }
    
    // Align block size to pointer size
    block_size = HEAP_ALIGN_UP(block_size, POOL_ALIGNMENT);
    
    // Allocate pool structure
    memory_pool_t *pool = kmalloc(sizeof(memory_pool_t));
    if (!pool) {
        printf("pool_create: Failed to allocate pool structure\n");
        return NULL;
    }
    
    // Calculate total pool memory needed
    uint32_t total_size = block_size * block_count;
    uint32_t pages_needed = vmm_get_page_count(total_size);
    
    // Allocate pool memory
    void *pool_memory = NULL;
    if (flags & HEAP_FLAG_KERNEL) {
        pool_memory = vmm_kernel_alloc_pages(pages_needed);
    } else {
        pool_memory = vmm_alloc_pages(pages_needed, 
                                     PAGE_PRESENT | PAGE_WRITE | 
                                     (flags & HEAP_FLAG_USER ? PAGE_USER : 0));
    }
    
    if (!pool_memory) {
        printf("pool_create: Failed to allocate %u pages for pool\n", pages_needed);
        kfree(pool);
        return NULL;
    }
    
    // Initialize pool structure
    pool->magic = POOL_MAGIC;
    pool->pool_start = pool_memory;
    pool->block_size = block_size;
    pool->total_blocks = block_count;
    pool->free_blocks = block_count;
    pool->free_list = NULL;
    pool->flags = flags;
    
    // Initialize all blocks as free
    pool_init_blocks(pool);
    
    printf("pool_create: Created pool with %u blocks of %u bytes each\n",
           block_count, block_size);
    
    return pool;
}

/**
 * Destroy a memory pool and free all its resources
 * 
 * @param pool Pool to destroy
 */
void pool_destroy(memory_pool_t *pool) {
    if (!pool) {
        return;
    }
    
    POOL_VALIDATE(pool);
    
    // Check for memory leaks
    if (pool->free_blocks != pool->total_blocks) {
        printf("pool_destroy: WARNING - Pool has %u leaked blocks\n",
               pool->total_blocks - pool->free_blocks);
    }
    
    // Calculate pages to free
    uint32_t total_size = pool->block_size * pool->total_blocks;
    uint32_t pages_to_free = vmm_get_page_count(total_size);
    
    // Free pool memory
    if (pool->flags & HEAP_FLAG_KERNEL) {
        vmm_kernel_free_pages(pool->pool_start, pages_to_free);
    } else {
        vmm_free_pages(pool->pool_start, pages_to_free);
    }
    
    // Clear magic to prevent reuse
    pool->magic = 0;
    
    // Free pool structure
    kfree(pool);
    
    printf("pool_destroy: Destroyed pool and freed %u pages\n", pages_to_free);
}

/**
 * Allocate a block from the memory pool
 * 
 * @param pool Pool to allocate from
 * @return Pointer to allocated block or NULL if pool is full
 */
void *pool_alloc(memory_pool_t *pool) {
    if (!pool) {
        return NULL;
    }
    
    POOL_VALIDATE(pool);
    
    // Check if pool has free blocks
    if (pool->free_blocks == 0) {
        return NULL;
    }
    
    // Remove a block from free list
    void *block = pool_remove_free_block(pool);
    if (!block) {
        return NULL;
    }
    
    // Update statistics
    pool->free_blocks--;
    
    // Clear the block memory for security
    memset(block, 0, pool->block_size);
    
    return block;
}

/**
 * Free a block back to the memory pool
 * 
 * @param pool Pool to return block to
 * @param ptr Pointer to block to free
 */
void pool_free(memory_pool_t *pool, void *ptr) {
    if (!pool || !ptr) {
        return;
    }
    
    POOL_VALIDATE(pool);
    
    // Validate that the pointer belongs to this pool
    if (!pool_contains_block(pool, ptr)) {
        kernel_panic("pool_free: Pointer %p does not belong to pool %p", ptr, pool);
        return;
    }
    
    // Check for double free
    if (pool->free_blocks >= pool->total_blocks) {
        kernel_panic("pool_free: Double free detected in pool %p", pool);
        return;
    }
    
    // Clear block data for security
    memset(ptr, 0xDD, pool->block_size);  // Debug pattern
    
    // Add block back to free list
    pool_add_free_block(pool, ptr);
    
    // Update statistics
    pool->free_blocks++;
}

/**
 * Get the number of free blocks in the pool
 * 
 * @param pool Pool to query
 * @return Number of free blocks
 */
uint32_t pool_get_free_count(memory_pool_t *pool) {
    if (!pool) {
        return 0;
    }
    
    POOL_VALIDATE(pool);
    return pool->free_blocks;
}

/**
 * Get the number of used blocks in the pool
 * 
 * @param pool Pool to query
 * @return Number of used blocks
 */
uint32_t pool_get_used_count(memory_pool_t *pool) {
    if (!pool) {
        return 0;
    }
    
    POOL_VALIDATE(pool);
    return pool->total_blocks - pool->free_blocks;
}

/**
 * Check if the pool is full
 * 
 * @param pool Pool to check
 * @return true if pool is full, false otherwise
 */
bool pool_is_full(memory_pool_t *pool) {
    if (!pool) {
        return true;
    }
    
    POOL_VALIDATE(pool);
    return pool->free_blocks == 0;
}

/**
 * Check if the pool is empty
 * 
 * @param pool Pool to check
 * @return true if pool is empty (all blocks free), false otherwise
 */
bool pool_is_empty(memory_pool_t *pool) {
    if (!pool) {
        return true;
    }
    
    POOL_VALIDATE(pool);
    return pool->free_blocks == pool->total_blocks;
}

/**
 * Get pool statistics
 * 
 * @param pool Pool to get stats for
 * @param total_blocks Output: total number of blocks
 * @param free_blocks Output: number of free blocks
 * @param block_size Output: size of each block
 */
void pool_get_stats(memory_pool_t *pool, uint32_t *total_blocks, 
                   uint32_t *free_blocks, uint32_t *block_size) {
    if (!pool) {
        if (total_blocks) *total_blocks = 0;
        if (free_blocks) *free_blocks = 0;
        if (block_size) *block_size = 0;
        return;
    }
    
    POOL_VALIDATE(pool);
    
    if (total_blocks) *total_blocks = pool->total_blocks;
    if (free_blocks) *free_blocks = pool->free_blocks;
    if (block_size) *block_size = pool->block_size;
}

/**
 * Print pool statistics for debugging
 * 
 * @param pool Pool to print stats for
 */
void pool_print_stats(memory_pool_t *pool) {
    if (!pool) {
        printf("Pool: NULL\n");
        return;
    }
    
    POOL_VALIDATE(pool);
    
    printf("Memory Pool Statistics:\n");
    printf("  Pool address: %p\n", pool);
    printf("  Memory start: %p\n", pool->pool_start);
    printf("  Block size: %u bytes\n", pool->block_size);
    printf("  Total blocks: %u\n", pool->total_blocks);
    printf("  Free blocks: %u\n", pool->free_blocks);
    printf("  Used blocks: %u\n", pool->total_blocks - pool->free_blocks);
    printf("  Utilization: %.1f%%\n", 
           100.0 * (pool->total_blocks - pool->free_blocks) / pool->total_blocks);
    printf("  Total memory: %u bytes\n", pool->block_size * pool->total_blocks);
    printf("  Free memory: %u bytes\n", pool->block_size * pool->free_blocks);
    printf("  Flags: 0x%x\n", pool->flags);
}

/**
 * Validate pool integrity
 * 
 * @param pool Pool to validate
 * @return Number of errors found
 */
uint32_t pool_validate(memory_pool_t *pool) {
    if (!pool) {
        return 1;
    }
    
    uint32_t errors = 0;
    
    // Check pool magic
    if (pool->magic != POOL_MAGIC) {
        printf("pool_validate: Invalid pool magic: 0x%x\n", pool->magic);
        errors++;
    }
    
    // Check pool parameters
    if (pool->block_size == 0) {
        printf("pool_validate: Invalid block size: %u\n", pool->block_size);
        errors++;
    }
    
    if (pool->total_blocks == 0) {
        printf("pool_validate: Invalid total blocks: %u\n", pool->total_blocks);
        errors++;
    }
    
    if (pool->free_blocks > pool->total_blocks) {
        printf("pool_validate: Free blocks (%u) > total blocks (%u)\n",
               pool->free_blocks, pool->total_blocks);
        errors++;
    }
    
    // Validate free list
    uint32_t free_count = 0;
    pool_block_t *current = (pool_block_t *)pool->free_list;
    
    while (current && free_count <= pool->total_blocks) {
        // Check if block is within pool bounds
        if (!pool_contains_block(pool, current)) {
            printf("pool_validate: Free block %p outside pool bounds\n", current);
            errors++;
            break;
        }
        
        // Check block magic
        if (current->magic != POOL_BLOCK_MAGIC) {
            printf("pool_validate: Invalid free block magic: 0x%x\n", current->magic);
            errors++;
        }
        
        current = current->next;
        free_count++;
    }
    
    // Check free list count matches
    if (free_count != pool->free_blocks) {
        printf("pool_validate: Free list count (%u) != free_blocks (%u)\n",
               free_count, pool->free_blocks);
        errors++;
    }
    
    if (errors == 0) {
        printf("pool_validate: Pool %p passed validation\n", pool);
    } else {
        printf("pool_validate: Pool %p has %u errors\n", pool, errors);
    }
    
    return errors;
}

// Static helper functions

/**
 * Initialize all blocks in the pool as free
 */
static void pool_init_blocks(memory_pool_t *pool) {
    uint8_t *block_ptr = (uint8_t *)pool->pool_start;
    
    // Link all blocks together in the free list
    for (uint32_t i = 0; i < pool->total_blocks; i++) {
        pool_block_t *block = (pool_block_t *)block_ptr;
        
        // Set up block header
        block->magic = POOL_BLOCK_MAGIC;
        
        // Link to next block, or NULL if last
        if (i < pool->total_blocks - 1) {
            block->next = (pool_block_t *)(block_ptr + pool->block_size);
        } else {
            block->next = NULL;
        }
        
        block_ptr += pool->block_size;
    }
    
    // Set free list head to first block
    pool->free_list = pool->pool_start;
}

/**
 * Add a block to the free list
 */
static void pool_add_free_block(memory_pool_t *pool, void *block) {
    pool_block_t *free_block = (pool_block_t *)block;
    
    // Set up block header
    free_block->magic = POOL_BLOCK_MAGIC;
    free_block->next = (pool_block_t *)pool->free_list;
    
    // Update free list head
    pool->free_list = block;
}

/**
 * Remove and return a block from the free list
 */
static void *pool_remove_free_block(memory_pool_t *pool) {
    if (!pool->free_list) {
        return NULL;
    }
    
    pool_block_t *block = (pool_block_t *)pool->free_list;
    
    // Validate block
    POOL_BLOCK_VALIDATE(block);
    
    // Update free list head
    pool->free_list = block->next;
    
    // Clear block header
    block->magic = 0;
    block->next = NULL;
    
    return block;
}

/**
 * Check if a pointer belongs to this pool
 */
static bool pool_contains_block(memory_pool_t *pool, void *ptr) {
    if (!ptr) {
        return false;
    }
    
    uint8_t *pool_start = (uint8_t *)pool->pool_start;
    uint8_t *pool_end = pool_start + (pool->block_size * pool->total_blocks);
    uint8_t *block_ptr = (uint8_t *)ptr;
    
    // Check if pointer is within pool bounds
    if (block_ptr < pool_start || block_ptr >= pool_end) {
        return false;
    }
    
    // Check if pointer is aligned to block boundary
    uint32_t offset = block_ptr - pool_start;
    if (offset % pool->block_size != 0) {
        return false;
    }
    
    return true;
}

/**
 * Validate pool integrity (internal version)
 */
static void pool_validate_integrity(memory_pool_t *pool) {
    uint32_t errors = pool_validate(pool);
    if (errors > 0) {
        kernel_panic("Pool integrity check failed with %u errors", errors);
    }
}
