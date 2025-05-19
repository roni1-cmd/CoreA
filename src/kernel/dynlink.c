extern int fs_read(const char *name, char *buf, int size);
extern void *kmalloc(unsigned int size);
extern struct task tasks[8];
extern int current_task;

int load_program(const char *name) {
    char buffer[512];
    int size = fs_read(name, buffer, 512);
    if (size <= 0) return -1;
    void *prog = kmalloc(size);
    if (!prog) return -1;
    for (int i = 0; i < size; i++)
        ((char *)prog)[i] = buffer[i];
    for (int i = 0; i < 8; i++) {
        if (!tasks[i].state) {
            tasks[i].eip = (unsigned int)prog;
            tasks[i].esp = 0x40000 + i * 0x1000;
            tasks[i].state = 1;
            tasks[i].page_dir = clone_page_dir(tasks[current_task].page_dir);
            return 0;
        }
    }
    return -1;
}
