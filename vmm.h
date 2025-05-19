#ifndef VMM_H
#define VMM_H

#include <kernel/kernel.h>

// Page directory and table entries
#define PAGE_PRESENT    0x001
#define PAGE_WRITE      0x002
#define PAGE_USER       0x004
#define PAGE_WRITETHROUGH 0x008
#define PAGE_CACHE_DISABLE 0x010
#define PAGE_ACCESSED   0x020
#define PAGE_DIRTY      0x040
#define PAGE_SIZE_4MB   0x080
#define PAGE_GLOBAL     0x100
#define PAGE_COPY_ON_WRITE 0x200  // Custom flag
#define PAGE_SHARED     0x400     // Custom flag

// Virtual memory regions
#define VMM_REGION_KERNEL    0x01
#define VMM_REGION_USER      0x02
#define VMM_REGION_HEAP      0x04
#define VMM_REGION_STACK     0x08
#define VMM_REGION_SHARED    0x10
#define VMM_REGION_DEVICE    0x20

// Page fault error codes
#define FAULT_PRESENT   0x01
#define FAULT_WRITE     0x02
#define FAULT_USER      0x04
#define FAULT_RESERVED  0x08
#define FAULT_FETCH     0x10

// Forward declarations
struct process;

// Page directory entry
typedef union {
    struct {
        u32 present     : 1;
        u32 write       : 1;
        u32 user        : 1;
        u32 writethrough: 1;
        u32 cache_disable: 1;
        u32 accessed    : 1;
        u32 reserved    : 1;
        u32 page_size   : 1;
        u32 global      : 1;
        u32 available   : 3;
        u32 address     : 20;
    } __packed;
    u32 raw;
} pde_t;

// Page table entry
typedef union {
    struct {
        u32 present     : 1;
        u32 write       : 1;
        u32 user        : 1;
        u32 writethrough: 1;
        u32 cache_disable: 1;
        u32 accessed    : 1;
        u32 dirty       : 1;
        u32 attribute   : 1;
        u32 global      : 1;
        u32 available   : 3;
        u32 address     : 20;
    } __packed;
    u32 raw;
} pte_t;

// Page directory (1024 entries)
typedef struct {
    pde_t entries[1024];
} __aligned(PAGE_SIZE) page_directory_t;

// Page table (1024 entries)
typedef struct {
    pte_t entries[1024];
} __aligned(PAGE_SIZE) page_table_t;

// Virtual memory area structure
typedef struct vm_area {
    u32 start;              // Start virtual address
    u32 end;                // End virtual address
    u32 flags;              // Protection and type flags
    u32 offset;             // Offset in file (if mapped)
    struct vm_area* next;   // Next VMA in list
    struct vm_area* prev;   // Previous VMA in list
} vm_area_t;

// Memory mapping structure
typedef struct {
    page_directory_t* page_directory;
    vm_area_t* vm_areas;
    u32 vm_start;           // Start of user virtual memory
    u32 vm_end;             // End of user virtual memory
    u32 heap_start;         // Start of heap
    u32 heap_end;           // End of heap
    u32 stack_start;        // Start of stack
    u32 stack_end;          // End of stack
} memory_map_t;

// Function prototypes

/**
 * Initialize the virtual memory manager
 * @return KERNEL_OK on success, error code on failure
 */
kernel_error_t vmm_init(void);

/**
 * Create a new page directory for a process
 * @return Pointer to new page directory, NULL on failure
 */
page_directory_t* vmm_create_page_directory(void);

/**
 * Destroy a page directory and free its resources
 * @param pd Page directory to destroy
 */
void vmm_destroy_page_directory(page_directory_t* pd);

/**
 * Switch to a different page directory
 * @param pd Page directory to switch to
 */
void vmm_switch_page_directory(page_directory_t* pd);

/**
 * Get the current page directory
 * @return Current page directory
 */
page_directory_t* vmm_get_current_directory(void);

/**
 * Map a virtual page to a physical frame
 * @param virtual_addr Virtual address to map
 * @param physical_addr Physical address to map to
 * @param flags Page flags
 * @return KERNEL_OK on success, error code on failure
 */
kernel_error_t vmm_map_page(u32 virtual_addr, u32 physical_addr, u32 flags);

/**
 * Unmap a virtual page
 * @param virtual_addr Virtual address to unmap
 */
void vmm_unmap_page(u32 virtual_addr);

/**
 * Map multiple contiguous pages
 * @param virtual_addr Starting virtual address
 * @param physical_addr Starting physical address
 * @param count Number of pages to map
 * @param flags Page flags
 * @return KERNEL_OK on success, error code on failure
 */
kernel_error_t vmm_map_pages(u32 virtual_addr, u32 physical_addr, u32 count, u32 flags);

/**
 * Unmap multiple contiguous pages
 * @param virtual_addr Starting virtual address
 * @param count Number of pages to unmap
 */
void vmm_unmap_pages(u32 virtual_addr, u32 count);

/**
 * Get the physical address for a virtual address
 * @param virtual_addr Virtual address
 * @return Physical address, 0 if not mapped
 */
u32 vmm_get_physical_addr(u32 virtual_addr);

/**
 * Check if a virtual address is mapped
 * @param virtual_addr Virtual address to check
 * @return true if mapped, false otherwise
 */
bool vmm_is_mapped(u32 virtual_addr);

/**
 * Allocate virtual memory in kernel space
 * @param size Size in bytes to allocate
 * @param flags Page flags
 * @return Virtual address of allocated memory, 0 on failure
 */
u32 vmm_alloc_kernel(u32 size, u32 flags);

/**
 * Free virtual memory in kernel space
 * @param addr Virtual address to free
 * @param size Size in bytes to free
 */
void vmm_free_kernel(u32 addr, u32 size);

/**
 * Allocate virtual memory in user space
 * @param mm Memory mapping structure
 * @param size Size in bytes to allocate
 * @param flags Page flags
 * @return Virtual address of allocated memory, 0 on failure
 */
u32 vmm_alloc_user(memory_map_t* mm, u32 size, u32 flags);

/**
 * Free virtual memory in user space
 * @param mm Memory mapping structure
 * @param addr Virtual address to free
 * @param size Size in bytes to free
 */
void vmm_free_user(memory_map_t* mm, u32 addr, u32 size);

/**
 * Handle page fault
 * @param fault_addr Address that caused the fault
 * @param error_code Error code from CPU
 * @param regs Register state at time of fault
 * @return KERNEL_OK if handled, error code if not
 */
kernel_error_t vmm_handle_page_fault(u32 fault_addr, u32 error_code, struct registers* regs);

/**
 * Create a memory mapping for a process
 * @return Pointer to new memory mapping, NULL on failure
 */
memory_map_t* vmm_create_memory_map(void);

/**
 * Destroy a memory mapping
 * @param mm Memory mapping to destroy
 */
void vmm_destroy_memory_map(memory_map_t* mm);

/**
 * Clone a memory mapping (for fork)
 * @param mm Memory mapping to clone
 * @return Pointer to cloned memory mapping, NULL on failure
 */
memory_map_t* vmm_clone_memory_map(memory_map_t* mm);

/**
 * Add a virtual memory area
 * @param mm Memory mapping
 * @param start Start address
 * @param end End address
 * @param flags Flags
 * @return KERNEL_OK on success, error code on failure
 */
kernel_error_t vmm_add_area(memory_map_t* mm, u32 start, u32 end, u32 flags);

/**
 * Remove a virtual memory area
 * @param mm Memory mapping
 * @param start Start address
 * @param end End address
 */
void vmm_remove_area(memory_map_t* mm, u32 start, u32 end);

/**
 * Find a virtual memory area containing an address
 * @param mm Memory mapping
 * @param addr Address to search for
 * @return Pointer to VMA if found, NULL otherwise
 */
vm_area_t* vmm_find_area(memory_map_t* mm, u32 addr);

/**
 * Invalidate TLB for a specific page
 * @param addr Virtual address of page to invalidate
 */
static inline void vmm_invlpg(u32 addr) {
    __asm__ volatile("invlpg (%0)" : : "r" (addr) : "memory");
}

/**
 * Flush entire TLB
 */
static inline void vmm_flush_tlb(void) {
    u32 cr3;
    __asm__ volatile("mov %%cr3, %0" : "=r" (cr3));
    __asm__ volatile("mov %0, %%cr3" : : "r" (cr3) : "memory");
}

/**
 * Enable paging
 */
static inline void vmm_enable_paging(void) {
    u32 cr0;
    __asm__ volatile("mov %%cr0, %0" : "=r" (cr0));
    cr0 |= 0x80000000;
    __asm__ volatile("mov %0, %%cr0" : : "r" (cr0));
}

/**
 * Set page directory base register
 * @param pd_physical Physical address of page directory
 */
static inline void vmm_set_cr3(u32 pd_physical) {
    __asm__ volatile("mov %0, %%cr3" : : "r" (pd_physical) : "memory");
}

/**
 * Get page directory base register
 * @return Physical address of current page directory
 */
static inline u32 vmm_get_cr3(void) {
    u32 cr3;
    __asm__ volatile("mov %%cr3, %0" : "=r" (cr3));
    return cr3;
}

// Helper macros
#define VMM_PAGE_ALIGN(addr)    ALIGN_DOWN(addr, PAGE_SIZE)
#define VMM_PAGE_ALIGN_UP(addr) ALIGN_UP(addr, PAGE_SIZE)
#define VMM_PAGE_OFFSET(addr)   ((addr) & PAGE_MASK)
#define VMM_PAGE_NUMBER(addr)   ((addr) >> PAGE_SHIFT)

#define VMM_PD_INDEX(addr)      (((addr) >> 22) & 0x3FF)
#define VMM_PT_INDEX(addr)      (((addr) >> 12) & 0x3FF)

// Global kernel page directory
extern page_directory_t* kernel_page_directory;

#endif // VMM_H
