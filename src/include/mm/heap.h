#ifndef _KERNEL_MM_HEAP_H
#define _KERNEL_MM_HEAP_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

// Heap configuration constants
#define HEAP_MAGIC              0xDEADBEEF
#define HEAP_MIN_SIZE           0x100000    // 1MB minimum heap size
#define HEAP_MAX_SIZE           0x10000000  // 256MB maximum heap size
#define HEAP_BLOCK_SIZE         0x1000      // 4KB block size
#define HEAP_ALIGNMENT          8           // 8-byte alignment
#define HEAP_MIN_ALLOC          16          // Minimum allocation size

// Heap flags
#define HEAP_FLAG_KERNEL        0x01        // Kernel heap
#define HEAP_FLAG_USER          0x02        // User heap
#define HEAP_FLAG_EXECUTABLE    0x04        // Executable pages
#define HEAP_FLAG_WRITABLE      0x08        // Writable pages
#define HEAP_FLAG_NOCACHE       0x10        // Non-cacheable pages

// Block flags
#define BLOCK_FREE              0x00        // Block is free
#define BLOCK_USED              0x01        // Block is allocated
#define BLOCK_MAGIC_FREE        0xFEEDFACE  // Magic for free blocks
#define BLOCK_MAGIC_USED        0xCAFEBABE  // Magic for used blocks

// Forward declarations
struct heap;
struct heap_block;
struct heap_stats;

// Heap block header structure
typedef struct heap_block {
    uint32_t magic;             // Magic number for validation
    uint32_t size;              // Size of the block (including header)
    uint32_t flags;             // Block flags (free/used)
    struct heap_block *next;    // Next block in list
    struct heap_block *prev;    // Previous block in list
    uint32_t checksum;          // Checksum for integrity checking
} __attribute__((packed)) heap_block_t;

// Heap footer structure (for boundary tags)
typedef struct heap_footer {
    uint32_t magic;             // Magic number
    heap_block_t *header;       // Pointer to corresponding header
} __attribute__((packed)) heap_footer_t;

// Heap statistics structure
typedef struct heap_stats {
    uint32_t total_size;        // Total heap size
    uint32_t used_size;         // Currently used size
    uint32_t free_size;         // Currently free size
    uint32_t largest_free;      // Largest contiguous free block
    uint32_t total_blocks;      // Total number of blocks
    uint32_t free_blocks;       // Number of free blocks
    uint32_t used_blocks;       // Number of used blocks
    uint32_t allocations;       // Total allocations performed
    uint32_t deallocations;     // Total deallocations performed
    uint32_t coalesces;         // Number of block coalesces
    uint32_t expansions;        // Number of heap expansions
    uint32_t contractions;      // Number of heap contractions
    double fragmentation;       // Fragmentation percentage
} heap_stats_t;

// Heap structure
typedef struct heap {
    uint32_t magic;             // Heap magic number
    uint32_t start_addr;        // Start virtual address
    uint32_t end_addr;          // End virtual address  
    uint32_t max_addr;          // Maximum allowed address
    uint32_t flags;             // Heap flags
    heap_block_t *free_list;    // Free blocks list
    heap_stats_t stats;         // Heap statistics
    uint32_t lock;              // Spinlock for thread safety
} heap_t;

// Heap iterator structure for debugging
typedef struct heap_iterator {
    heap_t *heap;               // Target heap
    heap_block_t *current;      // Current block
    bool include_free;          // Include free blocks
    bool include_used;          // Include used blocks
} heap_iterator_t;

// Function prototypes

// Heap creation and destruction
heap_t *heap_create(uint32_t start_addr, uint32_t initial_size, 
                    uint32_t max_size, uint32_t flags);
void heap_destroy(heap_t *heap);

// Memory allocation functions
void *heap_alloc(heap_t *heap, uint32_t size, bool zero);
void *heap_realloc(heap_t *heap, void *ptr, uint32_t new_size);
void heap_free(heap_t *heap, void *ptr);

// Aligned allocation
void *heap_alloc_aligned(heap_t *heap, uint32_t size, uint32_t alignment);
void *heap_calloc(heap_t *heap, uint32_t num, uint32_t size);

// Kernel heap functions (global kernel heap)
int heap_init_kernel(void);
void *kmalloc(uint32_t size);
void *kcalloc(uint32_t num, uint32_t size);
void *krealloc(void *ptr, uint32_t new_size);
void kfree(void *ptr);
void *kmalloc_aligned(uint32_t size, uint32_t alignment);

// User heap functions
heap_t *heap_create_user(uint32_t initial_size);
void heap_destroy_user(heap_t *heap);

// Heap management
int heap_expand(heap_t *heap, uint32_t additional_size);
int heap_contract(heap_t *heap, uint32_t size_to_remove);
void heap_coalesce_free_blocks(heap_t *heap);
void heap_validate(heap_t *heap);

// Block management utilities
heap_block_t *heap_find_free_block(heap_t *heap, uint32_t size);
heap_block_t *heap_split_block(heap_t *heap, heap_block_t *block, uint32_t size);
void heap_merge_blocks(heap_t *heap, heap_block_t *block1, heap_block_t *block2);
void heap_add_free_block(heap_t *heap, heap_block_t *block);
void heap_remove_free_block(heap_t *heap, heap_block_t *block);

// Block information functions
uint32_t heap_get_block_size(void *ptr);
bool heap_is_valid_pointer(heap_t *heap, void *ptr);
heap_block_t *heap_get_block_header(void *ptr);
heap_footer_t *heap_get_block_footer(heap_block_t *header);

// Statistics and debugging
heap_stats_t heap_get_stats(heap_t *heap);
void heap_print_stats(heap_t *heap);
void heap_dump_blocks(heap_t *heap);
void heap_dump_free_list(heap_t *heap);
uint32_t heap_check_integrity(heap_t *heap);

// Heap iteration for debugging
heap_iterator_t heap_iterator_create(heap_t *heap, bool include_free, bool include_used);
heap_block_t *heap_iterator_next(heap_iterator_t *iter);
void heap_iterator_destroy(heap_iterator_t *iter);

// Memory pool functions (for fixed-size allocations)
typedef struct memory_pool {
    void *pool_start;           // Start of pool memory
    uint32_t block_size;        // Size of each block
    uint32_t total_blocks;      // Total number of blocks
    uint32_t free_blocks;       // Number of free blocks
    void *free_list;            // Free blocks list
    uint32_t flags;             // Pool flags
} memory_pool_t;

memory_pool_t *pool_create(uint32_t block_size, uint32_t block_count, uint32_t flags);
void pool_destroy(memory_pool_t *pool);
void *pool_alloc(memory_pool_t *pool);
void pool_free(memory_pool_t *pool, void *ptr);
uint32_t pool_get_free_count(memory_pool_t *pool);

// Cache allocator for frequently used objects
typedef struct object_cache {
    const char *name;           // Cache name for debugging
    uint32_t object_size;       // Size of each object
    uint32_t align;             // Alignment requirement
    void (*constructor)(void *); // Constructor function
    void (*destructor)(void *);  // Destructor function
    memory_pool_t *pool;        // Underlying memory pool
    uint32_t allocs;            // Allocation count
    uint32_t frees;             // Free count
} object_cache_t;

object_cache_t *cache_create(const char *name, uint32_t size, uint32_t align,
                             void (*ctor)(void *), void (*dtor)(void *));
void cache_destroy(object_cache_t *cache);
void *cache_alloc(object_cache_t *cache);
void cache_free(object_cache_t *cache, void *obj);

// Slab allocator functions
int slab_init(void);
object_cache_t *slab_cache_create(const char *name, uint32_t size, uint32_t align);
void slab_cache_destroy(object_cache_t *cache);

// Memory debugging and leak detection
#ifdef HEAP_DEBUG
typedef struct alloc_info {
    void *ptr;                  // Allocated pointer
    uint32_t size;              // Allocation size
    const char *file;           // Source file
    uint32_t line;              // Source line
    uint32_t timestamp;         // Allocation timestamp
    struct alloc_info *next;    // Next allocation
} alloc_info_t;

void heap_debug_enable(heap_t *heap);
void heap_debug_disable(heap_t *heap);
void heap_check_leaks(heap_t *heap);
void heap_print_leaks(heap_t *heap);

#define heap_debug_alloc(heap, size) \
    heap_debug_track_alloc(heap_alloc(heap, size, false), size, __FILE__, __LINE__)
#define heap_debug_free(heap, ptr) \
    heap_debug_track_free(heap, ptr, __FILE__, __LINE__)

void *heap_debug_track_alloc(void *ptr, uint32_t size, const char *file, uint32_t line);
void heap_debug_track_free(heap_t *heap, void *ptr, const char *file, uint32_t line);
#endif

// Utility macros
#define HEAP_ALIGN_UP(x, align)     (((x) + (align) - 1) & ~((align) - 1))
#define HEAP_ALIGN_DOWN(x, align)   ((x) & ~((align) - 1))
#define HEAP_IS_ALIGNED(x, align)   (((x) & ((align) - 1)) == 0)

// Block size calculations
#define HEAP_BLOCK_HEADER_SIZE      sizeof(heap_block_t)
#define HEAP_BLOCK_FOOTER_SIZE      sizeof(heap_footer_t)
#define HEAP_BLOCK_OVERHEAD         (HEAP_BLOCK_HEADER_SIZE + HEAP_BLOCK_FOOTER_SIZE)
#define HEAP_MIN_BLOCK_SIZE         (HEAP_BLOCK_OVERHEAD + HEAP_MIN_ALLOC)

// Error codes
#define HEAP_SUCCESS                0
#define HEAP_ERROR                 -1
#define HEAP_OUT_OF_MEMORY         -2
#define HEAP_INVALID_POINTER       -3
#define HEAP_CORRUPTION            -4
#define HEAP_DOUBLE_FREE           -5
#define HEAP_SIZE_TOO_LARGE        -6
#define HEAP_ALIGNMENT_ERROR       -7

// Global kernel heap pointer
extern heap_t *kernel_heap;

// Inline utility functions
static inline uint32_t heap_calculate_checksum(heap_block_t *block) {
    uint32_t checksum = 0;
    checksum ^= block->magic;
    checksum ^= block->size;
    checksum ^= block->flags;
    checksum ^= (uint32_t)block->next;
    checksum ^= (uint32_t)block->prev;
    return checksum;
}

static inline bool heap_verify_block(heap_block_t *block) {
    if (!block) return false;
    
    // Check magic numbers
    if (block->flags == BLOCK_FREE && block->magic != BLOCK_MAGIC_FREE) 
        return false;
    if (block->flags == BLOCK_USED && block->magic != BLOCK_MAGIC_USED) 
        return false;
    
    // Verify checksum
    uint32_t expected = heap_calculate_checksum(block);
    return block->checksum == expected;
}

static inline void *heap_block_to_ptr(heap_block_t *block) {
    return (void *)((uint8_t *)block + HEAP_BLOCK_HEADER_SIZE);
}

static inline heap_block_t *heap_ptr_to_block(void *ptr) {
    return (heap_block_t *)((uint8_t *)ptr - HEAP_BLOCK_HEADER_SIZE);
}

#endif // _KERNEL_MM_HEAP_H
