/*
 * CoreA OS Kernel Main Entry Point
 * Author: CoreA Development Team
 */

#include <kernel/kernel.h>
#include <kernel/mm/pmm.h>
#include <kernel/mm/vmm.h>
#include <drivers/vga.h>
#include <lib/stdio.h>

// Multiboot information structure
typedef struct {
    uint32_t flags;
    uint32_t mem_lower;
    uint32_t mem_upper;
    uint32_t boot_device;
    uint32_t cmdline;
    uint32_t mods_count;
    uint32_t mods_addr;
    // ... more fields
} multiboot_info_t;

// Kernel entry point
void kernel_main(uint32_t magic, multiboot_info_t* mboot_info) {
    // Initialize VGA text mode
    vga_init();
    vga_clear();
    
    // Display welcome message
    kprintf("CoreA Operating System v0.1.0\n");
    kprintf("Copyright (c) 2024 CoreA Development Team\n\n");
    
    // Check multiboot magic
    if (magic != 0x2BADB002) {
        kprintf("PANIC: Invalid multiboot magic number: 0x%x\n", magic);
        kernel_panic("Invalid multiboot magic");
    }
    
    kprintf("Multiboot magic verified: 0x%x\n", magic);
    
    // Initialize memory management
    kprintf("Initializing memory management...\n");
    
    // Calculate available memory
    uint32_t total_memory = (mboot_info->mem_upper + 1024) * 1024;
    kprintf("Total memory: %d MB\n", total_memory / (1024 * 1024));
    
    // Initialize physical memory manager
    pmm_init(mboot_info);
    kprintf("Physical memory manager initialized\n");
    
    // Initialize virtual memory manager
    vmm_init();
    kprintf("Virtual memory manager initialized\n");
    
    // Initialize interrupt system
    kprintf("Initializing interrupt system...\n");
    gdt_init();
    idt_init();
    kprintf("Interrupt system initialized\n");
    
    // Initialize drivers
    kprintf("Initializing device drivers...\n");
    drivers_init();
    kprintf("Device drivers initialized\n");
    
    // Initialize filesystem
    kprintf("Initializing filesystem...\n");
    vfs_init();
    kprintf("Filesystem initialized\n");
    
    // Initialize process management
    kprintf("Initializing process management...\n");
    process_init();
    scheduler_init();
    kprintf("Process management initialized\n");
    
    // Start init process
    kprintf("Starting init process...\n");
    create_init_process();
    
    kprintf("\nKernel initialization completed successfully!\n");
    kprintf("CoreA OS is now running.\n\n");
    
    // Enable interrupts and start scheduler
    enable_interrupts();
    scheduler_start();
    
    // Kernel should never reach here
    kernel_panic("Kernel main returned unexpectedly");
}

// Kernel panic function
void kernel_panic(const char* message) {
    disable_interrupts();
    
    vga_set_color(VGA_COLOR_WHITE, VGA_COLOR_RED);
    vga_clear();
    
    kprintf("KERNEL PANIC: %s\n", message);
    kprintf("System halted.\n");
    
    // Halt the system
    while (1) {
        __asm__ __volatile__("hlt");
    }
}
