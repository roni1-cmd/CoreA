#define MAX_FILES 16
#define MAX_NAME 32
#define MAX_CONTENT 256

struct file_entry {
    char name[MAX_NAME];
    int active;
};

extern int sys_gui_create_window(int x, int y, int w, int h, unsigned char color);
extern int sys_gui_create_button(int win_id, int x, int y, int w, int h, const char *label);
extern int sys_gui_create_text_field(int win_id, int x, int y, int w, int h);
extern int sys_gui_get_event(int *type, int *x, int *y, unsigned char *data);
extern void sys_vfs_list(const char *path, char *buf, int size);
extern int sys_vfs_read(const char *name, char *buf, int size);
extern int sys_p2p_share_file(const char *filename, unsigned int ip, unsigned short port);

struct file_entry files[MAX_FILES];
int win_id = -1;
int text_field_id = -1;
int share_button_id = -1;
char selected_file[MAX_NAME];
int selected_file_idx = -1;

void file_explorer_init(void) {
    for (int i = 0; i < MAX_FILES; i++)
        files[i].active = 0;
    selected_file[0] = 0;
    win_id = sys_gui_create_window(50, 50, 220, 120, 7); // Light gray window
    if (win_id < 0) return;

    // Create file list area (as clickable labels)
    char file_list[256] = {0};
    sys_vfs_list("/ram", file_list, 256);
    int idx = 0, y_offset = 10;
    for (int i = 0; file_list[i] && idx < MAX_FILES; i++) {
        if (file_list[i] == '\n') {
            file_list[i] = 0;
            if (i > 0 && file_list[i-1]) {
                for (int j = 0; j < MAX_NAME && file_list[i-j-1]; j++)
                    files[idx].name[j] = file_list[i-j-1];
                files[idx].active = 1;
                sys_gui_create_button(win_id, 10, y_offset, 100, 15, files[idx].name);
                y_offset += 20;
                idx++;
            }
            file_list[i] = 0;
        }
    }

    // Create text field for file contents
    text_field_id = sys_gui_create_text_field(win_id, 120, 10, 90, 60);
    if (text_field_id < 0) return;

    // Create share button
    share_button_id = sys_gui_create_button(win_id, 120, 80, 50, 20, "Share");
}

void file_explorer_run(void) {
    if (win_id < 0) return;
    int event_type, event_x, event_y;
    unsigned char event_data[32];

    while (1) {
        if (sys_gui_get_event(&event_type, &event_x, &event_y, event_data) > 0) {
            if (event_type == 3) { // Button click
                int button_id = event_data[0];
                if (button_id == share_button_id && selected_file_idx >= 0) {
                    // Share file via P2P (hardcoded IP/port for simplicity)
                    if (sys_p2p_share_file(selected_file, 192168001, 1000) == 0)
                        sys_gui_create_text_field(win_id, 120, 100, 90, 15); // Show "Shared"
                } else {
                    for (int i = 0; i < MAX_FILES; i++) {
                        if (files[i].active && button_id == i) {
                            selected_file_idx = i;
                            for (int j = 0; j < MAX_NAME; j++)
                                selected_file[j] = files[i].name[j];
                            char content[MAX_CONTENT] = {0};
                            if (sys_vfs_read(selected_file, content, MAX_CONTENT) > 0)
                                sys_gui_create_text_field(win_id, 120, 10, 90, 60); // Update text field
                            break;
                        }
                    }
                }
            }
        }
        asm("hlt");
    }
}
