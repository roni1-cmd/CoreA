#ifndef PMM_H
#define PMM_H

#include <kernel/kernel.h>

// Physical memory frame size
#define FRAME_SIZE 4096

// Memory types from multiboot
#define MEMORY_AVAILABLE      1
#define MEMORY_RESERVED       2
#define MEMORY_ACPI_RECLAIMABLE 3
#define MEMORY_NVS            4
#define MEMORY_BADRAM         5

// Physical memory statistics
typedef struct {
    u64 total_memory;      // Total physical memory in bytes
    u64 used_memory;       // Used physical memory in bytes
    u64 free_memory;       // Free physical memory in bytes
    u32 total_frames;      // Total number of frames
    u32 used_frames;       // Number of used frames
    u32 free_frames;       // Number of free frames
} pmm_stats_t;

// Memory map entry structure
typedef struct memory_map_entry {
    u64 base_addr;         // Base address of memory region
    u64 length;            // Length of memory region
    u32 type;              // Type of memory region
    u32 acpi_attrs;        // ACPI attributes
    struct memory_map_entry* next;
} memory_map_entry_t;

// Function prototypes

/**
 * Initialize the physical memory manager
 * @param mboot_info Multiboot information structure
 * @return KERNEL_OK on success, error code on failure
 */
kernel_error_t pmm_init(struct multiboot_info* mboot_info);

/**
 * Allocate a single physical frame
 * @return Physical address of allocated frame, 0 on failure
 */
u64 pmm_alloc_frame(void);

/**
 * Allocate multiple contiguous physical frames
 * @param count Number of frames to allocate
 * @return Physical address of first frame, 0 on failure
 */
u64 pmm_alloc_frames(u32 count);

/**
 * Free a single physical frame
 * @param frame_addr Physical address of frame to free
 */
void pmm_free_frame(u64 frame_addr);

/**
 * Free multiple contiguous physical frames
 * @param frame_addr Physical address of first frame
 * @param count Number of frames to free
 */
void pmm_free_frames(u64 frame_addr, u32 count);

/**
 * Mark a physical frame as used (for kernel/reserved areas)
 * @param frame_addr Physical address of frame to mark
 */
void pmm_mark_frame_used(u64 frame_addr);

/**
 * Mark multiple physical frames as used
 * @param frame_addr Physical address of first frame
 * @param count Number of frames to mark
 */
void pmm_mark_frames_used(u64 frame_addr, u32 count);

/**
 * Check if a physical frame is allocated
 * @param frame_addr Physical address of frame to check
 * @return true if allocated, false if free
 */
bool pmm_is_frame_allocated(u64 frame_addr);

/**
 * Get physical memory statistics
 * @param stats Pointer to stats structure to fill
 */
void pmm_get_stats(pmm_stats_t* stats);

/**
 * Get the memory map
 * @return Pointer to first memory map entry
 */
memory_map_entry_t* pmm_get_memory_map(void);

/**
 * Reserve a physical memory region
 * @param base_addr Base address of region
 * @param length Length of region in bytes
 * @return KERNEL_OK on success, error code on failure
 */
kernel_error_t pmm_reserve_region(u64 base_addr, u64 length);

/**
 * Find the largest available memory region
 * @param base_addr Pointer to store base address
 * @param length Pointer to store length
 * @return KERNEL_OK on success, error code on failure
 */
kernel_error_t pmm_find_largest_region(u64* base_addr, u64* length);

/**
 * Convert physical address to frame number
 * @param addr Physical address
 * @return Frame number
 */
static inline u32 pmm_addr_to_frame(u64 addr) {
    return (u32)(addr / FRAME_SIZE);
}

/**
 * Convert frame number to physical address
 * @param frame Frame number
 * @return Physical address
 */
static inline u64 pmm_frame_to_addr(u32 frame) {
    return (u64)frame * FRAME_SIZE;
}

/**
 * Get total number of physical frames
 * @return Total frame count
 */
u32 pmm_get_total_frames(void);

/**
 * Get number of free physical frames
 * @return Free frame count
 */
u32 pmm_get_free_frames(void);

/**
 * Print memory map for debugging
 */
void pmm_print_memory_map(void);

/**
 * Defragment physical memory (if possible)
 */
void pmm_defragment(void);

// Bitmap manipulation functions (internal)
void pmm_set_frame(u32 frame);
void pmm_clear_frame(u32 frame);
bool pmm_test_frame(u32 frame);
u32 pmm_first_free_frame(void);
u32 pmm_first_free_frames(u32 count);

#endif // PMM_H
