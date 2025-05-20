/* CoreA Interrupt Descriptor Table (IDT)
 * Initializes and configures the IDT
 */
#include <kernel/idt.h>
#include <kernel/interrupt.h>
#include <stdint.h>

#define IDT_ENTRIES 256
#define IDT_GATE_TYPE 0x8E /* Interrupt gate, 64-bit mode */

struct idt_entry {
    uint16_t offset_low;
    uint16_t selector;
    uint8_t  ist;
    uint8_t  type_attr;
    uint16_t offset_mid;
    uint32_t offset_high;
    uint32_t zero;
} __attribute__((packed));

struct idt_ptr {
    uint16_t limit;
    uint64_t base;
} __attribute__((packed));

static struct idt_entry idt[IDT_ENTRIES];
static struct idt_ptr idt_ptr;

extern uint64_t interrupt_handlers[];

/* Set an IDT entry */
static void idt_set_gate(uint8_t vector, uint64_t handler, uint16_t selector, uint8_t type_attr) {
    idt[vector].offset_low = handler & 0xFFFF;
    idt[vector].offset_mid = (handler >> 16) & 0xFFFF;
    idt[vector].offset_high = (handler >> 32) & 0xFFFFFFFF;
    idt[vector].selector = selector;
    idt[vector].ist = 0;
    idt[vector].type_attr = type_attr;
    idt[vector].zero = 0;
}

/* Initialize the IDT */
void idt_init(void) {
    idt_ptr.limit = sizeof(idt) - 1;
    idt_ptr.base = (uint64_t)&idt;

    /* Set up interrupt handlers (from interrupt.asm) */
    for (int i = 0; i < 32; i++) {
        idt_set_gate(i, interrupt_handlers[i], 0x08, IDT_GATE_TYPE);
    }
    idt_set_gate(0x80, interrupt_handlers[32], 0x08, IDT_GATE_TYPE); /* Syscall */

    /* Load IDT */
    idt_load();
}

/* Load the IDT */
void idt_load(void) {
    asm volatile ("lidt %0" : : "m"(idt_ptr));
}
