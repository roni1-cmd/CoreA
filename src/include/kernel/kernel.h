/*
 * CoreA OS Kernel Header
 * Main kernel definitions and declarations
 */

#ifndef _KERNEL_H_
#define _KERNEL_H_

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

// Kernel version information
#define COREA_VERSION_MAJOR 0
#define COREA_VERSION_MINOR 1
#define COREA_VERSION_PATCH 0
#define COREA_VERSION_STRING "0.1.0"

// Memory layout definitions
#define KERNEL_VIRTUAL_BASE     0xC0000000
#define KERNEL_PHYSICAL_BASE    0x00100000
#define KERNEL_HEAP_START       0xC1000000
#define KERNEL_HEAP_SIZE        0x10000000
#define KERNEL_STACK_SIZE       0x4000

// Page size definitions
#define PAGE_SIZE               4096
#define PAGE_ALIGN(addr)        (((addr) + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1))
#define PAGE_ALIGN_DOWN(addr)   ((addr) & ~(PAGE_SIZE - 1))

// Utility macros
#define ALIGN(x, a)             (((x) + (a) - 1) & ~((a) - 1))
#define MIN(a, b)               ((a) < (b) ? (a) : (b))
#define MAX(a, b)               ((a) > (b) ? (a) : (b))
#define ARRAY_SIZE(x)           (sizeof(x) / sizeof((x)[0]))

// Compiler attributes
#define PACKED                  __attribute__((packed))
#define NORETURN                __attribute__((noreturn))
#define UNUSED                  __attribute__((unused))
#define ALWAYS_INLINE           __attribute__((always_inline)) inline

// Kernel subsystem initialization functions
void gdt_init(void);
void idt_init(void);
void drivers_init(void);
void vfs_init(void);
void process_init(void);
void scheduler_init(void);
void create_init_process(void);

// Interrupt control
void enable_interrupts(void);
void disable_interrupts(void);
bool interrupts_enabled(void);

// Scheduler control
void scheduler_start(void);
void schedule(void);

// Kernel panic
NORETURN void kernel_panic(const char* message);

// Assembly helpers
static ALWAYS_INLINE void outb(uint16_t port, uint8_t value) {
    __asm__ __volatile__("outb %1, %0" : : "dN"(port), "a"(value));
}

static ALWAYS_INLINE uint8_t inb(uint16_t port) {
    uint8_t ret;
    __asm__ __volatile__("inb %1, %0" : "=a"(ret) : "dN"(port));
    return ret;
}

static ALWAYS_INLINE void outw(uint16_t port, uint16_t value) {
    __asm__ __volatile__("outw %1, %0" : : "dN"(port), "a"(value));
}

static ALWAYS_INLINE uint16_t inw(uint16_t port) {
    uint16_t ret;
    __asm__ __volatile__("inw %1, %0" : "=a"(ret) : "dN"(port));
    return ret;
}

static ALWAYS_INLINE void outl(uint16_t port, uint32_t value) {
    __asm__ __volatile__("outl %1, %0" : : "dN"(port), "a"(value));
}

static ALWAYS_INLINE uint32_t inl(uint16_t port) {
    uint32_t ret;
    __asm__ __volatile__("inl %1, %0" : "=a"(ret) : "dN"(port));
    return ret;
}

static ALWAYS_INLINE void io_wait(void) {
    outb(0x80, 0);
}

static ALWAYS_INLINE void cpu_relax(void) {
    __asm__ __volatile__("pause" ::: "memory");
}

static ALWAYS_INLINE void memory_barrier(void) {
    __asm__ __volatile__("" ::: "memory");
}

#endif /* _KERNEL_H_ */
