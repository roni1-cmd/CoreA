#define MAX_TASKS 16
#define MAX_NAME 32
#define MAX_STATS 128

extern int sys_gui_create_window(int x, int y, int w, int h, unsigned char color);
extern int sys_gui_create_button(int win_id, int x, int y, int w, int h, const char *label);
extern int sys_gui_create_text_field(int win_id, int x, int y, int w, int h);
extern int sys_gui_get_event(int *type, int *x, int *y, unsigned char *data);
extern void sys_profiler_get_stats(char *buf, int size);
extern int sys_task_list(char *buf, int size);
extern int sys_net_get_stats(char *buf, int size);
extern void sys_task_terminate(int task_id);
extern void sys_logger_add_event(const char *message);

struct task_entry {
    char name[MAX_NAME];
    int id;
    int active;
};

int win_id = -1;
int cpu_field_id = -1;
int mem_field_id = -1;
int net_field_id = -1;
int refresh_button_id = -1;
struct task_entry tasks[MAX_TASKS];
int task_buttons[MAX_TASKS];

void sysmon_init(void) {
    win_id = sys_gui_create_window(140, 140, 240, 160, 7); // Light gray
    if (win_id < 0) return;

    cpu_field_id = sys_gui_create_text_field(win_id, 10, 10, 220, 20);
    mem_field_id = sys_gui_create_text_field(win_id, 10, 40, 220, 20);
    net_field_id = sys_gui_create_text_field(win_id, 10, 70, 220, 20);
    refresh_button_id = sys_gui_create_button(win_id, 10, 130, 60, 20, "Refresh");

    // Initialize task list
    for (int i = 0; i < MAX_TASKS; i++) {
        tasks[i].active = 0;
        task_buttons[i] = -1;
    }

    sysmon_update();
    sys_logger_add_event("System monitor launched");
}

void sysmon_update(void) {
    char stats[MAX_STATS] = {0};

    // CPU usage
    sys_profiler_get_stats(stats, MAX_STATS);
    sys_gui_create_text_field(win_id, 10, 10, 220, 20);

    // Memory usage
    sys_profiler_get_stats(stats, MAX_STATS);
    sys_gui_create_text_field(win_id, 10, 40, 220, 20);

    // Network stats
    sys_net_get_stats(stats, MAX_STATS);
    sys_gui_create_text_field(win_id, 10, 70, 220, 20);

    // Task list
    char task_list[256] = {0};
    sys_task_list(task_list, 256);
    int idx = 0, y_offset = 100;
    for (int i = 0; task_list[i] && idx < MAX_TASKS; i++) {
        if (task_list[i] == '\n') {
            task_list[i] = 0;
            if (i > 0 && task_list[i-1]) {
                for (int j = 0; j < MAX_NAME && task_list[i-j-1]; j++)
                    tasks[idx].name[j] = task_list[i-j-1];
                tasks[idx].id = idx; // Simplified ID
                tasks[idx].active = 1;
                task_buttons[idx] = sys_gui_create_button(win_id, 80, y_offset, 150, 15, tasks[idx].name);
                y_offset += 20;
                idx++;
            }
        }
    }
    for (; idx < MAX_TASKS; idx++) {
        tasks[idx].active = 0;
        task_buttons[idx] = -1;
    }
}

void sysmon_run(void) {
    if (win_id < 0) return;
    int event_type, event_x, event_y;
    unsigned char event_data[32];

    while (1) {
        if (sys_gui_get_event(&event_type, &event_x, &event_y, event_data) > 0) {
            if (event_type == 3) { // Button click
                int button_id = event_data[0];
                if (button_id == refresh_button_id) {
                    sysmon_update();
                    sys_logger_add_event("System monitor refreshed");
                } else {
                    for (int i = 0; i < MAX_TASKS; i++) {
                        if (tasks[i].active && task_buttons[i] == button_id) {
                            sys_task_terminate(tasks[i].id);
                            sysmon_update();
                            sys_logger_add_event("Task terminated");
                            break;
                        }
                    }
                }
            }
        }
        asm("hlt");
    }
}
