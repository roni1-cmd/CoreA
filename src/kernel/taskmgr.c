#define MAX_TASKS 8

struct task_info {
    int id;
    int active;
    unsigned int eip;
};

extern int sys_gui_create_window(int x, int y, int w, int h, unsigned char color);
extern int sys_gui_create_button(int win_id, int x, int y, int w, int h, const char *label);
extern int sys_gui_get_event(int *type, int *x, int *y, unsigned char *data);
extern void sys_get_tasks(struct task_info *tasks, int max_tasks);
extern int sys_kill_task(int id);

int win_id = -1;
struct task_info tasks[MAX_TASKS];
int kill_buttons[MAX_TASKS];

void taskmgr_init(void) {
    for (int i = 0; i < MAX_TASKS; i++)
        tasks[i].active = 0;
    win_id = sys_gui_create_window(60, 60, 200, 100, 7); // Light gray
    if (win_id < 0) return;

    // Get task list
    sys_get_tasks(tasks, MAX_TASKS);
    int y_offset = 10;
    for (int i = 0; i < MAX_TASKS; i++) {
        if (tasks[i].active) {
            char label[16];
            for (int j = 0; j < 15; j++) label[j] = 0;
            label[0] = 'T'; label[1] = 'a'; label[2] = 's'; label[3] = 'k';
            label[4] = ' '; label[5] = '0' + tasks[i].id;
            sys_gui_create_button(win_id, 10, y_offset, 80, 15, label);
            kill_buttons[i] = sys_gui_create_button(win_id, 100, y_offset, 50, 15, "Kill");
            y_offset += 20;
        }
    }
}

void taskmgr_run(void) {
    if (win_id < 0) return;
    int event_type, event_x, event_y;
    unsigned char event_data[32];

    while (1) {
        if (sys_gui_get_event(&event_type, &event_x, &event_y, event_data) > 0) {
            if (event_type == 3) { // Button click
                int button_id = event_data[0];
                for (int i = 0; i < MAX_TASKS; i++) {
                    if (tasks[i].active && button_id == kill_buttons[i]) {
                        if (sys_kill_task(tasks[i].id) == 0) {
                            tasks[i].active = 0;
                            taskmgr_init(); // Refresh
                        }
                        break;
                    }
                }
            }
        }
        asm("hlt");
    }
}
