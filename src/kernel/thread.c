#define MAX_THREADS 16
#define MAX_TASKS 8

struct thread {
    unsigned int esp;
    unsigned int eip;
    int task_id;
    int active;
};

extern struct task {
    unsigned int esp;
    unsigned int eip;
    unsigned int state;
    unsigned int *page_dir;
    unsigned int mutex_held;
} tasks[MAX_TASKS];
extern int current_task;
struct thread threads[MAX_THREADS];
extern void *kmalloc(unsigned int size);

void init_threads(void) {
    for (int i = 0; i < MAX_THREADS; i++)
        threads[i].active = 0;
}

int create_thread(void (*entry)(void)) {
    for (int i = 0; i < MAX_THREADS; i++) {
        if (!threads[i].active) {
            threads[i].esp = (unsigned int)kmalloc(4096) + 4096;
            threads[i].eip = (unsigned int)entry;
            threads[i].task_id = current_task;
            threads[i].active = 1;
            asm volatile(
                "mov %0, %%esp;"
                "push $0x23;"
                "push %0;"
                "pushf;"
                "push $0x1b;"
                "push %1;"
                "mov %%esp, %0;"
                : "+r"(threads[i].esp)
                : "r"(threads[i].eip)
            );
            return i;
        }
    }
    return -1;
}

void schedule_threads(void) {
    int i;
    for (i = 0; i < MAX_THREADS; i++) {
        if (threads[i].active && threads[i].task_id == current_task) {
            unsigned int old_esp;
            asm volatile("mov %%esp, %0" : "=r"(old_esp));
            tasks[current_task].esp = old_esp;
            asm volatile(
                "mov %0, %%esp;"
                "push $0x23;"
                "push %0;"
                "pushf;"
                "push $0x1b;"
                "push %1;"
                "iret;"
                :: "r"(threads[i].esp), "r"(threads[i].eip)
            );
        }
    }
}
