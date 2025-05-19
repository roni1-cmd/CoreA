#define MAX_LOGS 16
#define MAX_LOG_SIZE 64

struct log_entry {
    char message[MAX_LOG_SIZE];
    int active;
};

extern int sys_gui_create_window(int x, int y, int w, int h, unsigned char color);
extern int sys_gui_create_button(int win_id, int x, int y, int w, int h, const char *label);
extern int sys_gui_get_event(int *type, int *x, int *y, unsigned char *data);

struct log_entry logs[MAX_LOGS];
int log_count = 0;
int win_id = -1;

void logger_add_event(const char *message) {
    if (log_count >= MAX_LOGS) {
        for (int i = 0; i < MAX_LOGS - 1; i++)
            logs[i] = logs[i + 1];
        log_count--;
    }
    for (int i = 0; i < MAX_LOG_SIZE - 1 && message[i]; i++)
        logs[log_count].message[i] = message[i];
    logs[log_count].message[MAX_LOG_SIZE - 1] = 0;
    logs[log_count].active = 1;
    log_count++;
}

void logger_init(void) {
    win_id = sys_gui_create_window(100, 100, 200, 140, 7); // Light gray
    if (win_id < 0) return;

    int y_offset = 10;
    for (int i = 0; i < log_count; i++) {
        if (logs[i].active) {
            sys_gui_create_button(win_id, 10, y_offset, 180, 15, logs[i].message);
            y_offset += 20;
        }
    }
}

void logger_run(void) {
    if (win_id < 0) return;
    int event_type, event_x, event_y;
    unsigned char event_data[32];

    while (1) {
        if (sys_gui_get_event(&event_type, &event_x, &event_y, event_data) > 0) {
            // No actions needed for now
        }
        asm("hlt");
    }
}
