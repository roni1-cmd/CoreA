#define MAX_FILES 16
#define MAX_NAME 32
#define MAX_BACKUP_SIZE 4096
#define MAX_MESSAGE 64

extern int sys_gui_create_window(int x, int y, int w, int h, unsigned char color);
extern int sys_gui_create_button(int win_id, int x, int y, int w, int h, const char *label);
extern int sys_gui_create_text_field(int win_id, int x, int y, int w, int h);
extern int sys_gui_get_event(int *type, int *x, int *y, unsigned char *data);
extern void sys_vfs_list(const char *path, char *buf, int size);
extern int sys_vfs_read(const char *name, char *buf, int size);
extern int sys_vfs_write(const char *name, const char *buf, int size);
extern int sys_p2p_send_file(const char *filename, unsigned int ip, unsigned short port);
extern int sys_p2p_receive_file(const char *filename, unsigned int ip, unsigned short port, char *buf, int size);
extern void sys_logger_add_event(const char *message);

int win_id = -1;
int backup_button_id = -1;
int restore_button_id = -1;
int status_field_id = -1;

void backup_init(void) {
    win_id = sys_gui_create_window(130, 130, 200, 100, 7); // Light gray
    if (win_id < 0) return;

    backup_button_id = sys_gui_create_button(win_id, 10, 10, 80, 20, "Backup");
    restore_button_id = sys_gui_create_button(win_id, 110, 10, 80, 20, "Restore");
    status_field_id = sys_gui_create_text_field(win_id, 10, 40, 180, 20);

    sys_logger_add_event("Backup tool launched");
}

void backup_run(void) {
    if (win_id < 0) return;
    int event_type, event_x, event_y;
    unsigned char event_data[256];

    while (1) {
        if (sys_gui_get_event(&event_type, &event_x, &event_y, event_data) > 0) {
            if (event_type == 3) { // Button click
                int button_id = event_data[0];
                if (button_id == backup_button_id) {
                    char file_list[256] = {0};
                    sys_vfs_list("/ram", file_list, 256);
                    char backup_data[MAX_BACKUP_SIZE] = {0};
                    int offset = 0;

                    // Simple backup: concatenate all files
                    for (int i = 0; file_list[i] && offset < MAX_BACKUP_SIZE; i++) {
                        if (file_list[i] == '\n') {
                            file_list[i] = 0;
                            if (i > 0 && file_list[i-1]) {
                                char filename[MAX_NAME] = {0};
                                for (int j = 0; j < MAX_NAME && file_list[i-j-1]; j++)
                                    filename[j] = file_list[i-j-1];
                                char content[256] = {0};
                                int len = sys_vfs_read(filename, content, 256);
                                if (len > 0 && offset + len + MAX_NAME + 2 < MAX_BACKUP_SIZE) {
                                    for (int j = 0; j < MAX_NAME && filename[j]; j++)
                                        backup_data[offset++] = filename[j];
                                    backup_data[offset++] = ':';
                                    for (int j = 0; j < len; j++)
                                        backup_data[offset++] = content[j];
                                    backup_data[offset++] = '\n';
                                }
                            }
                        }
                    }

                    // Save locally and send to P2P peer
                    if (sys_vfs_write("backup.dat", backup_data, offset) >= 0 &&
                        sys_p2p_send_file("backup.dat", 192168001, 1000) >= 0) {
                        sys_gui_create_text_field(win_id, 10, 40, 180, 20);
                        sys_logger_add_event("Backup completed");
                    } else {
                        sys_gui_create_text_field(win_id, 10, 40, 180, 20);
                        sys_logger_add_event("Backup failed");
                    }
                } else if (button_id == restore_button_id) {
                    char backup_data[MAX_BACKUP_SIZE] = {0};
                    int len = sys_p2p_receive_file("backup.dat", 192168001, 1000, backup_data, MAX_BACKUP_SIZE);
                    if (len <= 0)
                        len = sys_vfs_read("backup.dat", backup_data, MAX_BACKUP_SIZE);

                    if (len > 0) {
                        char filename[MAX_NAME] = {0};
                        char content[256] = {0};
                        int fname_idx = 0, content_idx = 0, parsing_fname = 1;

                        for (int i = 0; i < len; i++) {
                            if (backup_data[i] == ':') {
                                parsing_fname = 0;
                                fname_idx = 0;
                                continue;
                            } else if (backup_data[i] == '\n') {
                                content[content_idx] = 0;
                                filename[fname_idx] = 0;
                                if (content_idx > 0 && fname_idx > 0)
                                    sys_vfs_write(filename, content, content_idx);
                                parsing_fname = 1;
                                fname_idx = content_idx = 0;
                                continue;
                            }
                            if (parsing_fname && fname_idx < MAX_NAME - 1)
                                filename[fname_idx++] = backup_data[i];
                            else if (!parsing_fname && content_idx < 256)
                                content[content_idx++] = backup_data[i];
                        }
                        sys_gui_create_text_field(win_id, 10, 40, 180, 20);
                        sys_logger_add_event("Restore completed");
                    } else {
                        sys_gui_create_text_field(win_id, 10, 40, 180, 20);
                        sys_logger_add_event("Restore failed");
                    }
                }
            }
        }
        asm("hlt");
    }
}
