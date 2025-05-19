#define MAX_TASKS 4
extern struct task {
    unsigned int esp;
    unsigned int eip;
    unsigned int state;
} tasks[MAX_TASKS];
extern int current_task;

void init_tasks(void);
void schedule(void);
