#define MAX_FILES 16
#define MAX_NAME 32
#define MAX_CONTENT 256
#define MAX_STATUS 64

struct file_entry {
    char name[MAX_NAME];
    int active;
    int is_remote; // 0: local, 1: remote
};

extern int sys_gui_create_window(int x, int y, int w, int h, unsigned char color);
extern int sys_gui_create_button(int win_id, int x, int y, int w, int h, const char *label);
extern int sys_gui_create_text_field(int win_id, int x, int y, int w, int h);
extern int sys_gui_create_context_menu(int win_id, int x, int y, const char *items);
extern int sys_gui_get_event(int *type, int *x, int *y, unsigned char *data);
extern void sys_vfs_list(const char *path, char *buf, int size);
extern int sys_vfs_read(const char *name, char *buf, int size);
extern int sys_vfs_delete(const char *name);
extern int sys_p2p_share_file(const char *filename, unsigned int ip, unsigned short port);
extern int sys_p2p_list_files(unsigned int ip, unsigned short port, char *buf, int size);
extern int sys_p2p_download_file(unsigned int ip, unsigned short port, const char *filename, char *buf, int size);

struct file_entry files[MAX_FILES];
int win_id = -1;
int text_field_id = -1;
int share_button_id = -1;
int status_bar_id = -1;
char selected_file[MAX_NAME];
int selected_file_idx = -1;
char status_message[MAX_STATUS];
int focused_file_idx = -1;

void update_status(const char *msg) {
    if (status_bar_id < 0) return;
    for (int i = 0; i < MAX_STATUS && msg[i]; i++)
        status_message[i] = msg[i];
    status_message[MAX_STATUS - 1] = 0;
    sys_gui_create_text_field(win_id, 10, 100, 200, 15); // Update status bar
}

void file_explorer_init(void) {
    for (int i = 0; i < MAX_FILES; i++)
        files[i].active = files[i].is_remote = 0;
    selected_file[0] = status_message[0] = 0;
    win_id = sys_gui_create_window(50, 50, 220, 120, 7); // Light gray
    if (win_id < 0) return;

    // File list area
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
        }
    }

    // Text field for contents
    text_field_id = sys_gui_create_text_field(win_id, 120, 10, 90, 60);
    if (text_field_id < 0) return;

    // Share button
    share_button_id = sys_gui_create_button(win_id, 120, 80, 50, 20, "Share");

    // Status bar
    status_bar_id = sys_gui_create_text_field(win_id, 10, 100, 200, 15);
    update_status("Ready");
}

void file_explorer_run(void) {
    if (win_id < 0) return;
    int event_type, event_x, event_y;
    unsigned char event_data[32];

    while (1) {
        if (sys_gui_get_event(&event_type, &event_x, &event_y, event_data) > 0) {
            if (event_type == 1 && event_data[0] == 2) { // Right-click
                for (int i = 0; i < MAX_FILES; i++) {
                    if (files[i].active && event_y >= 10 + i * 20 && event_y < 25 + i * 20 && event_x >= 10 && event_x < 110) {
                        selected_file_idx = i;
                        for (int j = 0; j < MAX_NAME; j++)
                            selected_file[j] = files[i].name[j];
                        sys_gui_create_context_menu(win_id, event_x, event_y, "Open\0Delete\0Share\0");
                        break;
                    }
                }
            } else if (event_type == 3) { // Button click
                int button_id = event_data[0];
                if (button_id == share_button_id && selected_file_idx >= 0) {
                    if (sys_p2p_share_file(selected_file, 192168001, 1000) == 0)
                        update_status("File shared");
                    else
                        update_status("Share failed");
                } else {
                    for (int i = 0; i < MAX_FILES; i++) {
                        if (files[i].active && button_id == i) {
                            selected_file_idx = i;
                            for (int j = 0; j < MAX_NAME; j++)
                                selected_file[j] = files[i].name[j];
                            char content[MAX_CONTENT] = {0};
                            if (sys_vfs_read(selected_file, content, MAX_CONTENT) > 0)
                                sys_gui_create_text_field(win_id, 120, 10, 90, 60);
                            update_status("File opened");
                            focused_file_idx = i;
                            break;
                        }
                    }
                }
            } else if (event_type == 4) { // Context menu click
                int item_id = event_data[0];
                if (selected_file_idx >= 0) {
                    if (item_id == 0) { // Open
                        char content[MAX_CONTENT] = {0};
                        if (sys_vfs_read(selected_file, content, MAX_CONTENT) > 0)
                            sys_gui_create_text_field(win_id, 120, 10, 90, 60);
                        update_status("File opened");
                    } else if (item_id == 1) { // Delete
                        if (sys_vfs_delete(selected_file) == 0) {
                            files[selected_file_idx].active = 0;
                            selected_file_idx = -1;
                            selected_file[0] = 0;
                            file_explorer_init(); // Refresh
                            update_status("File deleted");
                        } else {
                            update_status("Delete failed");
                        }
                    } else if (item_id == 2) { // Share
                        if (sys_p2p_share_file(selected_file, 192168001, 1000) == 0)
                            update_status("File shared");
                        else
                            update_status("Share failed");
                    }
                }
            } else if (event_type == 2) { // Key press
                if (event_data[0] == 0x48 && focused_file_idx > 0) { // Up arrow
                    focused_file_idx--;
                    selected_file_idx = focused_file_idx;
                    for (int j = 0; j < MAX_NAME; j++)
                        selected_file[j] = files[focused_file_idx].name[j];
                    update_status("Selected up");
                } else if (event_data[0] == 0x50 && focused_file_idx < MAX_FILES - 1 && files[focused_file_idx + 1].active) { // Down arrow
                    focused_file_idx++;
                    selected_file_idx = focused_file_idx;
                    for (int j = 0; j < MAX_NAME; j++)
                        selected_file[j] = files[focused_file_idx].name[j];
                    update_status("Selected down");
                } else if (event_data[0] == 0x1c && selected_file_idx >= 0) { // Enter
                    char content[MAX_CONTENT] = {0};
                    if (sys_vfs_read(selected_file, content, MAX_CONTENT) > 0)
                        sys_gui_create_text_field(win_id, 120, 10, 90, 60);
                    update_status("File opened");
                }
            }
        }
        asm("hlt");
    }
}

void file_explorer_browse_p2p(unsigned int ip, unsigned short port) {
    char file_list[256] = {0};
    if (sys_p2p_list_files(ip, port, file_list, 256) <= 0) {
        update_status("P2P list failed");
        return;
    }
    for (int i = 0; i < MAX_FILES; i++)
        files[i].active = 0;
    int idx = 0, y_offset = 10;
    for (int i = 0; file_list[i] && idx < MAX_FILES; i++) {
        if (file_list[i] == '\n') {
            file_list[i] = 0;
            if (i > 0 && file_list[i-1]) {
                for (int j = 0; j < MAX_NAME && file_list[i-j-1]; j++)
                    files[idx].name[j] = file_list[i-j-1];
                files[idx].active = 1;
                files[idx].is_remote = 1;
                sys_gui_create_button(win_id, 10, y_offset, 100, 15, files[idx].name);
                y_offset += 20;
                idx++;
            }
        }
    }
    update_status("P2P files loaded");
}
