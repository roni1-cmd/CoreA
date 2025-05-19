#ifndef KERNEL_H
#define KERNEL_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

// Kernel version information
#define KERNEL_VERSION_MAJOR 0
#define KERNEL_VERSION_MINOR 1
#define KERNEL_VERSION_PATCH 0
#define KERNEL_NAME "CoreA"

// Common type definitions
typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t   i8;
typedef int16_t  i16;
typedef int32_t  i32;
typedef int64_t  i64;

// Memory alignment macros
#define ALIGN_UP(addr, align)   (((addr) + (align) - 1) & ~((align) - 1))
#define ALIGN_DOWN(addr, align) ((addr) & ~((align) - 1))
#define IS_ALIGNED(addr, align) (((addr) & ((align) - 1)) == 0)

// Page size definitions
#define PAGE_SIZE     4096
#define PAGE_SHIFT    12
#define PAGE_MASK     (PAGE_SIZE - 1)

// Virtual memory layout
#define KERNEL_VIRTUAL_BASE   0xC0000000
#define KERNEL_HEAP_START     0xC1000000
#define KERNEL_HEAP_END       0xE0000000
#define USER_VIRTUAL_BASE     0x00000000
#define USER_VIRTUAL_END      0xBFFFFFFF

// Error codes
typedef enum {
    KERNEL_OK = 0,
    KERNEL_ERROR = -1,
    KERNEL_ENOMEM = -2,
    KERNEL_EINVAL = -3,
    KERNEL_ENOSYS = -4,
    KERNEL_EACCES = -5,
    KERNEL_ENOENT = -6,
    KERNEL_EIO = -7
} kernel_error_t;

// Forward declarations
struct multiboot_info;
struct registers;

// Core kernel functions
void kernel_main(struct multiboot_info* mboot_info);
void kernel_init(struct multiboot_info* mboot_info);
void kernel_panic(const char* message);
void kernel_halt(void);

// Debug and logging functions
void kdebug(const char* format, ...);
void kinfo(const char* format, ...);
void kwarn(const char* format, ...);
void kerror(const char* format, ...);

// Atomic operations
static inline void atomic_inc(volatile u32* ptr) {
    __asm__ volatile("lock incl %0" : "+m" (*ptr));
}

static inline void atomic_dec(volatile u32* ptr) {
    __asm__ volatile("lock decl %0" : "+m" (*ptr));
}

static inline u32 atomic_read(volatile u32* ptr) {
    return *ptr;
}

static inline void atomic_set(volatile u32* ptr, u32 value) {
    *ptr = value;
}

// Memory barriers
static inline void mb(void) {
    __asm__ volatile("mfence" ::: "memory");
}

static inline void rmb(void) {
    __asm__ volatile("lfence" ::: "memory");
}

static inline void wmb(void) {
    __asm__ volatile("sfence" ::: "memory");
}

// I/O port operations
static inline void outb(u16 port, u8 value) {
    __asm__ volatile("outb %1, %0" : : "dN" (port), "a" (value));
}

static inline u8 inb(u16 port) {
    u8 result;
    __asm__ volatile("inb %1, %0" : "=a" (result) : "dN" (port));
    return result;
}

static inline void outw(u16 port, u16 value) {
    __asm__ volatile("outw %1, %0" : : "dN" (port), "a" (value));
}

static inline u16 inw(u16 port) {
    u16 result;
    __asm__ volatile("inw %1, %0" : "=a" (result) : "dN" (port));
    return result;
}

static inline void outl(u16 port, u32 value) {
    __asm__ volatile("outl %1, %0" : : "dN" (port), "a" (value));
}

static inline u32 inl(u16 port) {
    u32 result;
    __asm__ volatile("inl %1, %0" : "=a" (result) : "dN" (port));
    return result;
}

// CPU control functions
static inline void cli(void) {
    __asm__ volatile("cli");
}

static inline void sti(void) {
    __asm__ volatile("sti");
}

static inline void hlt(void) {
    __asm__ volatile("hlt");
}

static inline void pause(void) {
    __asm__ volatile("pause");
}

// Get current CPU flags
static inline u32 save_flags(void) {
    u32 flags;
    __asm__ volatile("pushfl; popl %0" : "=r" (flags));
    return flags;
}

// Restore CPU flags
static inline void restore_flags(u32 flags) {
    __asm__ volatile("pushl %0; popfl" : : "r" (flags) : "memory", "cc");
}

// Critical section helpers
#define INTERRUPTS_ENABLED   (save_flags() & 0x200)
#define ENTER_CRITICAL()     do { cli(); } while (0)
#define EXIT_CRITICAL()      do { sti(); } while (0)

// Min/Max macros
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))

// Container of macro
#define container_of(ptr, type, member) \
    ((type*)((char*)(ptr) - offsetof(type, member)))

// Likely/unlikely for branch prediction
#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)

// Compiler attributes
#define __packed     __attribute__((packed))
#define __aligned(x) __attribute__((aligned(x)))
#define __noreturn   __attribute__((noreturn))
#define __unused     __attribute__((unused))
#define __weak       __attribute__((weak))

// Bootstrap processor flag
extern bool bsp_initialized;

// Global kernel state
extern volatile bool kernel_running;
extern u32 kernel_ticks;

#endif // KERNEL_H
