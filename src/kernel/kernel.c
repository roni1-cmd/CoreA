#define VGA_WIDTH 80
#define VGA_HEIGHT 25
#define VGA_BUFFER 0xB8000
#define PIT_HZ 1193182
#define TICK_RATE 100
#define MEM_START 0x100000
#define MEM_SIZE (1 << 20)
#define MAX_TASKS 8

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

struct task {
    unsigned int esp;
    unsigned int eip;
    unsigned int state; // 0: inactive, 1: running, 2: waiting
    unsigned int *page_dir;
    unsigned int mutex_held;
};

struct mutex {
    int locked;
    int owner;
};

struct mem_block *mem_list = (struct mem_block *)MEM_START;
volatile unsigned int ticks = 0;
char kbd_buffer[256];
int kbd_pos = 0;
struct idt_entry idt[256];
struct idt_ptr idtp;
struct task tasks[MAX_TASKS];
struct mutex mutexes[16];
int current_task = 0;

void outb(unsigned short port, unsigned char val) {
    asm volatile("outb %0, %1" : : "a"(val), "Nd"(port));
}

unsigned char inb(unsigned short port) {
    unsigned char ret;
    asm volatile("inb %1, %0" : "=a"(ret) : "Nd"(port));
    return ret;
}

void outw(unsigned short port, unsigned short val) {
    asm volatile("outw %0, %1" : : "a"(val), "Nd"(port));
}

unsigned short inw(unsigned short port) {
    unsigned short ret;
    asm volatile("inw %1, %0" : "=a"(ret) : "Nd"(port));
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
    idt_set_gate(0x2c, (unsigned int)mouse_handler, 0x08, 0x8e);
    idt_set_gate(0x80, (unsigned int)syscall_handler, 0x08, 0x8e);
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
    if (!ptr) return;
    struct mem_block *block = (struct mem_block *)((char *)ptr - sizeof(struct mem_block));
    block->free = 1;
}

extern void init_paging(void);
extern void switch_page_dir(unsigned int *dir);
extern unsigned int *clone_page_dir(unsigned int *src);
extern void disk_read(unsigned int sector, unsigned char *buffer);
extern void disk_write(unsigned int sector, unsigned char *buffer);
extern int vfs_create(const char *name);
extern int vfs_read(const char *name, char *buf, int size);
extern int vfs_write(const char *name, const char *buf, int size);
extern void vfs_list(const char *name, char *buf, int size);
extern void init_net(void);
extern int net_send_packet(unsigned char *data, int len);
extern int net_receive_packet(unsigned char *buf, int size);
extern void init_rtc(void);
extern void rtc_get_time(unsigned char *buf);
extern int load_program(const char *name);
extern void init_usb(void);
extern void init_gui(void);
extern void init_threads(void);
extern void init_mouse(void);
extern int ipc_send(int receiver, const char *data, int size);
extern int ipc_receive(int *sender, char *buf, int size);
extern void init_acpi(void);
extern void acpi_shutdown(void);
extern void acpi_reboot(void);
extern void init_audio(void);
extern void play_tone(unsigned int freq, unsigned int duration);
extern void init_vfs(void);
extern void init_profiler(void);
extern void profiler_get_stats(char *buf, int size);

void init_tasks(void) {
    for (int i = 0; i < MAX_TASKS; i++) {
        tasks[i].state = 0;
        tasks[i].mutex_held = -1;
    }
    for (int i = 0; i < 16; i++) {
        mutexes[i].locked = 0;
        mutexes[i].owner = -1;
    }
    tasks[0].eip = (unsigned int)run_shell;
    tasks[0].esp = 0x30000;
    tasks[0].state = 1;
    tasks[0].page_dir = clone_page_dir(0);
}

void schedule(void) {
    int next = (current_task + 1) % MAX_TASKS;
    int start = next;
    do {
        if (tasks[next].state == 1) {
            if (next != current_task) {
                unsigned int old_esp;
                asm volatile("mov %%esp, %0" : "=r"(old_esp));
                tasks[current_task].esp = old_esp;
                current_task = next;
                switch_page_dir(tasks[next].page_dir);
                asm volatile(
                    "mov %0, %%esp;"
                    "push $0x23;"
                    "push %0;"
                    "pushf;"
                    "push $0x1b;"
                    "push %1;"
                    "iret;"
                    :: "r"(tasks[next].esp), "r"(tasks[next].eip)
                );
            }
            return;
        }
        next = (next + 1) % MAX_TASKS;
    } while (next != start);
}

void timer_handler(void);
asm("timer_handler: pusha; inc %0; call profiler_update; call schedule; mov $0x20, %al; outb $0x20, %al; popa; iret" : : "m"(ticks));

void kbd_handler(void);
asm("kbd_handler: pusha; inb $0x60, %al; mov %al, %cl; and $0x80, %al; jnz .done; mov %cl, %al; mov %bx, %si; movb %al, (%si); inc %0; mov $0x20, %al; outb $0x20, %al; .done: popa; iret" : "=m"(kbd_pos) : "b"(kbd_buffer));

extern void mouse_handler(void);
extern void usb_handler(void);

void syscall_handler(void);
asm("syscall_handler: pusha; call do_syscall; popa; iret");

int do_fork(void) {
    for (int i = 0; i < MAX_TASKS; i++) {
        if (!tasks[i].state) {
            tasks[i].eip = tasks[current_task].eip;
            tasks[i].esp = tasks[current_task].esp;
            tasks[i].state = 1;
            tasks[i].page_dir = clone_page_dir(tasks[current_task].page_dir);
            return i;
        }
    }
    return -1;
}

void do_exit(void) {
    tasks[current_task].state = 0;
    kfree(tasks[current_task].page_dir);
    schedule();
}

int do_mutex_lock(int id) {
    if (id < 0 || id >= 16) return -1;
    if (mutexes[id].locked) {
        tasks[current_task].state = 2;
        schedule();
        return -1;
    }
    mutexes[id].locked = 1;
    mutexes[id].owner = current_task;
    tasks[current_task].mutex_held = id;
    return 0;
}

void do_mutex_unlock(int id) {
    if (id < 0 || id >= 16 || mutexes[id].owner != current_task) return;
    mutexes[id].locked = 0;
    mutexes[id].owner = -1;
    tasks[current_task].mutex_held = -1;
    for (int i = 0; i < MAX_TASKS; i++) {
        if (tasks[i].state == 2) tasks[i].state = 1;
    }
}

void do_syscall(void) {
    unsigned int eax, ebx, ecx, edx;
    asm volatile("mov %%eax, %0; mov %%ebx, %1; mov %%ecx, %2; mov %%edx, %3"
                 : "=r"(eax), "=r"(ebx), "=r"(ecx), "=r"(edx));
    switch (eax) {
        case 1: // sys_create
            eax = vfs_create((const char *)ebx);
            break;
        case 2: // sys_read
            eax = vfs_read((const char *)ebx, (char *)ecx, edx);
            break;
        case 3: // sys_write
            eax = vfs_write((const char *)ebx, (const char *)ecx, edx);
            break;
        case 4: // sys_list
            vfs_list((const char *)ebx, (char *)ecx, edx);
            eax = 0;
            break;
        case 5: // sys_fork
            eax = do_fork();
            break;
        case 6: // sys_exit
            do_exit();
            eax = 0;
            break;
        case 7: // sys_net_send
            eax = net_send_packet((unsigned char *)ebx, ecx);
            break;
        case 8: // sys_net_receive
            eax = net_receive_packet((unsigned char *)ebx, ecx);
            break;
        case 9: // sys_get_time
            rtc_get_time((unsigned char *)ebx);
            eax = 0;
            break;
        case 10: // sys_load_program
            eax = load_program((const char *)ebx);
            break;
        case 11: // sys_mutex_lock
            eax = do_mutex_lock(ebx);
            break;
        case 12: // sys_mutex_unlock
            do_mutex_unlock(ebx);
            eax = 0;
            break;
        case 13: // sys_ipc_send
            eax = ipc_send(ebx, (const char *)ecx, edx);
            break;
        case 14: // sys_ipc_receive
            eax = ipc_receive((int *)ebx, (char *)ecx, edx);
            break;
        case 15: // sys_shutdown
            acpi_shutdown();
            eax = 0;
            break;
        case 16: // sys_reboot
            acpi_reboot();
            eax = 0;
            break;
        case 17: // sys_play_tone
            play_tone(ebx, ecx);
            eax = 0;
            break;
        case 18: // sys_profiler_stats
            profiler_get_stats((char *)ebx, ecx);
            eax = 0;
            break;
        default:
            eax = -1;
    }
    asm volatile("mov %0, %%eax" :: "r"(eax));
}

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
    init_paging();
    init_disk();
    init_vfs();
    init_net();
    init_rtc();
    init_usb();
    init_gui();
    init_threads();
    init_mouse();
    init_ipc();
    init_acpi();
    init_audio();
    init_profiler();
    init_tasks();
    asm("sti");
    switch_to_user();
    while (1) {
        asm("hlt");
    }
}
