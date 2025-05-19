#ifndef _KERNEL_MM_VMM_H
#define _KERNEL_MM_VMM_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

// Page size constants
#define PAGE_SIZE           4096
#define PAGE_SHIFT          12
#define PAGE_MASK           (~(PAGE_SIZE - 1))
#define PAGES_PER_TABLE     1024
#define TABLES_PER_DIR      1024

// Virtual memory layout constants
#define KERNEL_VIRTUAL_BASE     0xC0000000  // 3GB - Kernel space starts here
#define USER_VIRTUAL_BASE       0x00400000  // 4MB - User space starts here
#define USER_STACK_TOP          0xC0000000  // User stack grows down from kernel space
#define USER_HEAP_START         0x40000000  // 1GB - User heap starts here
#define KERNEL_HEAP_START       0xD0000000  // Kernel heap starts here
#define KERNEL_HEAP_SIZE        0x10000000  // 256MB kernel heap

// Page flags
#define PAGE_PRESENT        0x001   // Page is present in memory
#define PAGE_WRITE          0x002   // Page is writable
#define PAGE_USER           0x004   // Page is accessible in user mode
#define PAGE_WRITETHROUGH   0x008   // Write-through caching
#define PAGE_NOCACHE        0x010   // Disable caching
#define PAGE_ACCESSED       0x020   // Page has been accessed
#define PAGE_DIRTY          0x040   // Page has been written to
#define PAGE_SIZE_4MB       0x080   // 4MB page (in page directory)
#define PAGE_GLOBAL         0x100   // Global page (TLB not flushed)
#define PAGE_COW            0x200   // Copy-on-write (custom flag)
#define PAGE_SHARED         0x400   // Shared page (custom flag)

// Memory protection flags
#define PROT_NONE           0x0     // No access
#define PROT_READ           0x1     // Read access
#define PROT_WRITE          0x2     // Write access
#define PROT_EXEC           0x4     // Execute access

// Memory mapping flags
#define MAP_SHARED          0x01    // Share changes
#define MAP_PRIVATE         0x02    // Changes are private
#define MAP_FIXED           0x10    // Interpret addr exactly
#define MAP_ANONYMOUS       0x20    // Don't use a file

// Forward declarations
struct page_directory;
struct page_table;
struct vm_area;
struct vm_space;

// Page directory entry structure
typedef struct {
    uint32_t present    : 1;    // Present in memory
    uint32_t write      : 1;    // Writable
    uint32_t user       : 1;    // User accessible
    uint32_t pwt        : 1;    // Page-level write-through
    uint32_t pcd        : 1;    // Page-level cache disable
    uint32_t accessed   : 1;    // Accessed
    uint32_t reserved   : 1;    // Reserved (0)
    uint32_t size       : 1;    // Page size (0 = 4KB, 1 = 4MB)
    uint32_t ignored    : 4;    // Ignored
    uint32_t frame      : 20;   // Physical frame number
} __attribute__((packed)) pde_t;

// Page table entry structure
typedef struct {
    uint32_t present    : 1;    // Present in memory
    uint32_t write      : 1;    // Writable
    uint32_t user       : 1;    // User accessible
    uint32_t pwt        : 1;    // Page-level write-through
    uint32_t pcd        : 1;    // Page-level cache disable
    uint32_t accessed   : 1;    // Accessed
    uint32_t dirty      : 1;    // Dirty
    uint32_t pat        : 1;    // Page attribute table
    uint32_t global     : 1;    // Global
    uint32_t avail      : 3;    // Available for system use
    uint32_t frame      : 20;   // Physical frame number
} __attribute__((packed)) pte_t;

// Page table structure
typedef struct page_table {
    pte_t pages[PAGES_PER_TABLE];
} __attribute__((aligned(PAGE_SIZE))) page_table_t;

// Page directory structure
typedef struct page_directory {
    pde_t tables[TABLES_PER_DIR];
    page_table_t *tables_physical[TABLES_PER_DIR];
    uint32_t physical_addr;
} __attribute__((aligned(PAGE_SIZE))) page_directory_t;

// Virtual memory area structure
typedef struct vm_area {
    uint32_t start;             // Start virtual address
    uint32_t end;               // End virtual address
    uint32_t flags;             // Protection flags
    uint32_t offset;            // File offset (if file-backed)
    struct vm_area *next;       // Next VM area
    struct vm_area *prev;       // Previous VM area
    // File backing info (for mmap)
    void *file;                 // File object (if file-backed)
    bool is_file_backed;        // Is this area file-backed?
} vm_area_t;

// Virtual memory space structure
typedef struct vm_space {
    page_directory_t *page_dir; // Page directory
    vm_area_t *vm_areas;        // List of memory areas
    uint32_t heap_start;        // Heap start address
    uint32_t heap_end;          // Current heap end
    uint32_t stack_start;       // Stack start address
    uint32_t brk;               // Current break (end of data segment)
    uint32_t ref_count;         // Reference count
} vm_space_t;

// VMM statistics structure
typedef struct vmm_stats {
    uint32_t total_pages;       // Total virtual pages
    uint32_t used_pages;        // Currently used pages
    uint32_t shared_pages;      // Shared pages
    uint32_t cow_pages;         // Copy-on-write pages
    uint32_t page_faults;       // Total page faults
    uint32_t cow_faults;        // COW page faults
    uint32_t swap_in;           // Pages swapped in
    uint32_t swap_out;          // Pages swapped out
} vmm_stats_t;

// Function prototypes

// Initialization
int vmm_init(void);
int vmm_init_paging(void);

// Page directory management
page_directory_t *vmm_create_page_directory(void);
void vmm_destroy_page_directory(page_directory_t *dir);
int vmm_clone_page_directory(page_directory_t *dest, page_directory_t *src);
void vmm_switch_page_directory(page_directory_t *dir);
page_directory_t *vmm_get_kernel_directory(void);
page_directory_t *vmm_get_current_directory(void);

// Page table management
page_table_t *vmm_get_page_table(page_directory_t *dir, uint32_t virtual_addr, 
                                  bool create);
pte_t *vmm_get_page(page_directory_t *dir, uint32_t virtual_addr, bool create);

// Memory mapping
int vmm_map_page(page_directory_t *dir, uint32_t virtual_addr, 
                 uint32_t physical_addr, uint32_t flags);
int vmm_unmap_page(page_directory_t *dir, uint32_t virtual_addr);
int vmm_map_pages(page_directory_t *dir, uint32_t virtual_start,
                  uint32_t physical_start, uint32_t count, uint32_t flags);
int vmm_unmap_pages(page_directory_t *dir, uint32_t virtual_start, 
                    uint32_t count);

// Address translation
uint32_t vmm_get_physical_addr(page_directory_t *dir, uint32_t virtual_addr);
bool vmm_is_mapped(page_directory_t *dir, uint32_t virtual_addr);

// Virtual memory space management
vm_space_t *vmm_create_vm_space(void);
void vmm_destroy_vm_space(vm_space_t *space);
vm_space_t *vmm_clone_vm_space(vm_space_t *space);
int vmm_switch_vm_space(vm_space_t *space);

// Virtual memory area management
vm_area_t *vmm_find_vm_area(vm_space_t *space, uint32_t addr);
vm_area_t *vmm_create_vm_area(vm_space_t *space, uint32_t start, uint32_t end,
                              uint32_t flags);
int vmm_destroy_vm_area(vm_space_t *space, vm_area_t *area);
int vmm_merge_vm_areas(vm_space_t *space);

// Memory allocation
void *vmm_alloc_pages(uint32_t count, uint32_t flags);
void vmm_free_pages(void *virtual_addr, uint32_t count);
void *vmm_alloc_page(uint32_t flags);
void vmm_free_page(void *virtual_addr);

// User space memory management
int vmm_mmap(vm_space_t *space, uint32_t addr, uint32_t length, 
             uint32_t prot, uint32_t flags, void *file, uint32_t offset);
int vmm_munmap(vm_space_t *space, uint32_t addr, uint32_t length);
int vmm_mprotect(vm_space_t *space, uint32_t addr, uint32_t length, 
                 uint32_t prot);
void *vmm_sbrk(vm_space_t *space, int32_t increment);

// Copy-on-write support
int vmm_mark_cow(page_directory_t *dir, uint32_t virtual_addr);
int vmm_handle_cow_fault(page_directory_t *dir, uint32_t virtual_addr);

// Page fault handling
int vmm_handle_page_fault(uint32_t fault_addr, uint32_t error_code);

// Cache and TLB management
void vmm_flush_tlb(void);
void vmm_flush_tlb_page(uint32_t virtual_addr);
void vmm_invalidate_page(uint32_t virtual_addr);

// Kernel memory management
void *vmm_kernel_alloc_pages(uint32_t count);
void vmm_kernel_free_pages(void *virtual_addr, uint32_t count);
int vmm_map_kernel_page(uint32_t virtual_addr, uint32_t physical_addr);
int vmm_unmap_kernel_page(uint32_t virtual_addr);

// User/kernel space utilities
bool vmm_is_user_address(uint32_t virtual_addr);
bool vmm_is_kernel_address(uint32_t virtual_addr);
int vmm_copy_to_user(void *dest, const void *src, size_t size);
int vmm_copy_from_user(void *dest, const void *src, size_t size);
int vmm_strncpy_from_user(char *dest, const char *src, size_t size);

// Statistics and debugging
vmm_stats_t vmm_get_stats(void);
void vmm_print_stats(void);
void vmm_dump_page_directory(page_directory_t *dir);
void vmm_dump_vm_space(vm_space_t *space);

// Address space utilities
uint32_t vmm_find_free_pages(vm_space_t *space, uint32_t count);
bool vmm_check_user_string(const char *str, size_t max_len);
bool vmm_check_user_buffer(const void *buf, size_t size, bool write);

// Inline utility functions
static inline uint32_t vmm_page_align_down(uint32_t addr) {
    return addr & PAGE_MASK;
}

static inline uint32_t vmm_page_align_up(uint32_t addr) {
    return (addr + PAGE_SIZE - 1) & PAGE_MASK;
}

static inline uint32_t vmm_get_page_count(uint32_t size) {
    return (size + PAGE_SIZE - 1) / PAGE_SIZE;
}

static inline uint32_t vmm_pde_index(uint32_t virtual_addr) {
    return (virtual_addr >> 22) & 0x3FF;
}

static inline uint32_t vmm_pte_index(uint32_t virtual_addr) {
    return (virtual_addr >> 12) & 0x3FF;
}

static inline uint32_t vmm_page_offset(uint32_t virtual_addr) {
    return virtual_addr & 0xFFF;
}

// Error codes
#define VMM_SUCCESS             0
#define VMM_ERROR              -1
#define VMM_OUT_OF_MEMORY      -2
#define VMM_INVALID_ADDRESS    -3
#define VMM_PERMISSION_DENIED  -4
#define VMM_ALREADY_MAPPED     -5
#define VMM_NOT_MAPPED         -6
#define VMM_INVALID_RANGE      -7

#endif // _KERNEL_MM_VMM_H
