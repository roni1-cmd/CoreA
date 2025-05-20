/* CoreA Interrupt Management
 * Handles interrupt dispatching and control
 */
#include <kernel/interrupt.h>
#include <kernel/syscall.h>
#include <kernel/panic.h>
#include <stdint.h>

void (*interrupt_handlers[256])(uint64_t);

/* Initialize interrupt management */
void interrupt_init(void) {
    /* Load IDT (set up in idt.c) */
    idt_load();

    /* Enable interrupts */
    asm volatile ("sti");
}

/* Register an interrupt handler */
void interrupt_register(uint64_t vector, void (*handler)(uint64_t)) {
    if (vector < 256) {
        interrupt_handlers[vector] = handler;
    }
}

/* C-level interrupt handler */
void interrupt_handler_c(uint64_t vector) {
    if (vector == 0x80) {
        /* System call dispatch */
        syscall_dispatch();
    } else if (interrupt_handlers[vector]) {
        interrupt_handlers[vector](vector);
    } else {
        /* Unhandled interrupt */
        kernel_panic("Unhandled interrupt: %d", vector);
    }
}

/* Enable interrupts */
void interrupt_enable(void) {
    asm volatile ("sti");
}

/* Disable interrupts */
void interrupt_disable(void) {
    asm volatile ("cli");
}
