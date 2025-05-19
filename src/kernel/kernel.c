#define VGA_WIDTH 80
#define VGA_HEIGHT 25
#define VGA_BUFFER 0xB8000
#define PIT_HZ 1193182
#define TICK_RATE 100
#define MEM_START 0x100000
#define MEM_SIZE (1 << 20)
struct mem_block {
    struct mem_block *next;
    unsigned int size;
    int free;
};
struct idt_entry {
    unsigned short base_low;
    unsigned short sel;
    unsigned char zero;
    unsigned char flags;
    unsigned short base_high;
} __attribute__((packed));
struct idt_ptr {
    unsigned short limit;
    unsigned int base;
} __attribute__((packed));
struct mem_block *mem_list = (struct mem_block *)MEM_START;
volatile unsigned int ticks = 0;
char kbd_buffer[256];
int kbd_pos = 0;
struct idt_entry idt[256];
struct idt_ptr idtp;
void outb(unsigned short port, unsigned char val) {
    asm volatile("outb %0, %1" : : "a"(val), "Nd"(port));
}
unsigned char inb(unsigned short port) {
    unsigned char ret;
    asm volatile("inb %1, %0" : "=a"(ret) : "Nd"(port));
    return ret;
}
void vga_clear(void) {
    unsigned short *vga = (unsigned short *)VGA_BUFFER;
    for (int i = 0; i < VGA_WIDTH * VGA_HEIGHT; i++)
        vga[i] = 0x0700;
}
void vga_print(const char *str, int row, int col) {
    unsigned short *vga = (unsigned short *)VGA_BUFFER;
    int pos = row * VGA_WIDTH + col;
    for (; *str; str++, pos++)
        vga[pos] = (0x07 << 8) | *str;
}
void vga_scroll(void) {
    unsigned short *vga = (unsigned short *)VGA_BUFFER;
    for (int i = VGA_WIDTH; i < VGA_WIDTH * VGA_HEIGHT; i++)
        vga[i - VGA_WIDTH] = vga[i];
    for (int i = 0; i < VGA_WIDTH; i++)
        vga[(VGA_HEIGHT - 1) * VGA_WIDTH + i] = 0x0700;
}
void idt_set_gate(unsigned char num, unsigned int base, unsigned short sel, unsigned char flags) {
    idt[num].base_low = base & 0xffff;
    idt[num].base_high = (base >> 16) & 0xffff;
    idt[num].sel = sel;
    idt[num].zero = 0;
    idt[num].flags = flags;
}
void init_idt(void) {
    idtp.limit = sizeof(idt) - 1;
    idtp.base = (unsigned int)&idt;
    for (int i = 0; i < 256; i++)
        idt_set_gate(i, 0, 0, 0);
    idt_set_gate(0x20, (unsigned int)timer_handler, 0x08, 0x8e);
    idt_set_gate(0x21, (unsigned int)kbd_handler, 0x08, 0x8e);
    asm volatile("lidt %0" : : "m"(idtp));
}
void init_pit(void) {
    unsigned int divisor = PIT_HZ / TICK_RATE;
    outb(0x43, 0x36);
    outb(0x40, divisor & 0xff);
    outb(0x40, (divisor >> 8) & 0xff);
}
void init_pic(void) {
    outb(0x20, 0x11);
    outb(0xa0, 0x11);
    outb(0x21, 0x20);
    outb(0xa1, 0x28);
    outb(0x21, 0x04);
    outb(0xa1, 0x02);
    outb(0x21, 0x01);
    outb(0xa1, 0x01);
    outb(0x21, 0x0);
    outb(0xa1, 0x0);
}
void init_mem(void) {
    mem_list->size = MEM_SIZE - sizeof(struct mem_block);
    mem_list->free = 1;
    mem_list->next = 0;
}
void *kmalloc(unsigned int size) {
    struct mem_block *curr = mem_list;
    while (curr) {
        if (curr->free && curr->size >= size) {
            if (curr->size > size + sizeof(struct mem_block)) {
                struct mem_block *new_block = (struct mem_block *)((char *)curr + sizeof(struct mem_block) + size);
                new_block->size = curr->size - size - sizeof(struct mem_block);
                new_block->free = 1;
                new_block->next = curr->next;
                curr->size = size;
                curr->next = new_block;
            }
            curr->free = 0;
            return (char *)curr + sizeof(struct mem_block);
        }
        curr = curr->next;
    }
    return 0;
}
void kfree(void *ptr) {
    struct mem_block *block = (struct mem_block *)((char *)ptr - sizeof(struct mem_block));
    block->free = 1;
}
void timer_handler(void);
asm("timer_handler: pusha; inc %0; mov $0x20, %al; outb $0x20, %al; popa; iret" : : "m"(ticks));
void kbd_handler(void);
asm("kbd_handler: pusha; inb $0x60, %al; mov %al, %cl; and $0x80, %al; jnz .done; mov %cl, %al; mov %bx, %si; movb %al, (%si); inc %0; mov $0x20, %al; outb $0x20, %al; .done: popa; iret" : "=m"(kbd_pos) : "b"(kbd_buffer));
void run_shell(void);
void switch_to_user(void) {
    asm volatile(
        "cli;"
        "mov $0x23, %ax;"
        "mov %ax, %ds;"
        "mov %ax, %es;"
        "mov %ax, %fs;"
        "mov %ax, %gs;"
        "mov %esp, %eax;"
        "push $0x23;"
        "push %eax;"
        "pushf;"
        "push $0x1b;"
        "push $1f;"
        "iret;"
        "1:;"
        ::: "eax"
    );
    run_shell();
}
void kmain(void) {
    vga_clear();
    vga_print("Kernel loaded!", 0, 0);
    init_idt();
    init_pic();
    init_pit();
    init_mem();
    asm("sti");
    switch_to_user();
    while (1) {
        asm("hlt");
    }
}
