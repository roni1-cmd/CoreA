#define MAX_CONTENT 256
#define MAX_NAME 32

extern int sys_gui_create_window(int x, int y, int w, int h, unsigned char color);
extern int sys_gui_create_button(int win_id, int x, int y, int w, int h, const char *label);
extern int sys_gui_create_text_field(int win_id, int x, int y, int w, int h);
extern int sys_gui_get_event(int *type, int *x, int *y, unsigned char *data);
extern int sys_vfs_read(const char *name, char *buf, int size);
extern int sys_vfs_write(const char *name, const char *buf, int size);
extern void sys_clipboard_copy(const char *data, int len);
extern int sys_clipboard_paste(char *buf, int max_len);

int win_id = -1;
int text_field_id = -1;
int save_button_id = -1;
int copy_button_id = -1;
int paste_button_id = -1;
char filename[MAX_NAME];
char content[MAX_CONTENT];

void texteditor_init(const char *name) {
    for (int i = 0; i < MAX_NAME && name[i]; i++)
        filename[i] = name[i];
    filename[MAX_NAME - 1] = 0;
    content[0] = 0;

    win_id = sys_gui_create_window(80, 80, 200, 120, 7); // Light gray
    if (win_id < 0) return;

    // Load file content if it exists
    int len = sys_vfs_read(filename, content, MAX_CONTENT);
    if (len > 0)
        content[len] = 0;

    text_field_id = sys_gui_create_text_field(win_id, 10, 10, 180, 60);
    save_button_id = sys_gui_create_button(win_id, 10, 80, 50, 20, "Save");
    copy_button_id = sys_gui_create_button(win_id, 70, 80, 50, 20, "Copy");
    paste_button_id = sys_gui_create_button(win_id, 130, 80, 50, 20, "Paste");
}

void texteditor_run(void) {
    if (win_id < 0) return;
    int event_type, event_x, event_y;
    unsigned char event_data[256];

    while (1) {
        if (sys_gui_get_event(&event_type, &event_x, &event_y, event_data) > 0) {
            if (event_type == 3) { // Button click
                int button_id = event_data[0];
                if (button_id == save_button_id) {
                    for (int i = 0; i < MAX_CONTENT && event_data[i + 1]; i++)
                        content[i] = event_data[i + 1];
                    content[MAX_CONTENT - 1] = 0;
                    if (sys_vfs_write(filename, content, MAX_CONTENT) >= 0)
                        sys_gui_create_text_field(win_id, 10, 100, 180, 15); // Status
                } else if (button_id == copy_button_id) {
                    for (int i = 0; i < MAX_CONTENT && event_data[i + 1]; i++)
                        content[i] = event_data[i + 1];
                    content[MAX_CONTENT - 1] = 0;
                    sys_clipboard_copy(content, MAX_CONTENT);
                    sys_gui_create_text_field(win_id, 10, 100, 180, 15); // Status
                } else if (button_id == paste_button_id) {
                    char paste_data[MAX_CONTENT] = {0};
                    int len = sys_clipboard_paste(paste_data, MAX_CONTENT);
                    if (len > 0) {
                        for (int i = 0; i < len; i++)
                            content[i] = paste_data[i];
                        content[len] = 0;
                        sys_gui_create_text_field(win_id, 10, 10, 180, 60);
                    }
                }
            }
        }
        asm("hlt");
    }
}
