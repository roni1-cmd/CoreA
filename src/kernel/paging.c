#define PAGE_SIZE 4096
#define PAGE_TABLE_ENTRIES 1024
#define KERNEL_VIRT_BASE 0xc0000000

unsigned int *kernel_page_dir;
extern void *kmalloc(unsigned int size);
extern void kfree(void *ptr);

void switch_page_dir(unsigned int *dir) {
    asm volatile("mov %0, %%cr3" :: "r"(dir));
}

unsigned int *clone_page_dir(unsigned int *src) {
    unsigned int *new_dir = kmalloc(PAGE_SIZE);
    if (!new_dir) return 0;
    for (int i = 0; i < PAGE_TABLE_ENTRIES; i++) {
        new_dir[i] = src ? src[i] : 0;
        if (new_dir[i] & 1) {
            unsigned int *new_table = kmalloc(PAGE_SIZE);
            unsigned int *old_table = (unsigned int *)(new_dir[i] & ~0xfff);
            for (int j = 0; j < PAGE_TABLE_ENTRIES; j++)
                new_table[j] = old_table[j];
            new_dir[i] = (unsigned int)new_table | (new_dir[i] & 0xfff);
        }
    }
    return new_dir;
}

void init_paging(void) {
    kernel_page_dir = kmalloc(PAGE_SIZE);
    for (int i = 0; i < PAGE_TABLE_ENTRIES; i++)
        kernel_page_dir[i] = 0;
    unsigned int *page_table = kmalloc(PAGE_SIZE);
    for (int i = 0; i < PAGE_TABLE_ENTRIES; i++)
        page_table[i] = (i * PAGE_SIZE) | 3;
    kernel_page_dir[0] = (unsigned int)page_table | 3;
    kernel_page_dir[PAGE_TABLE_ENTRIES - 1] = (unsigned int)page_table | 3;
    asm volatile(
        "mov %0, %%cr3;"
        "mov %%cr0, %%eax;"
        "or $0x80000000, %%eax;"
        "mov %%eax, %%cr0;"
        :: "r"(kernel_page_dir)
        : "eax"
    );
}
