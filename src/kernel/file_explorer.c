#define MAX_FILES 16
#define MAX_NAME 32
#define MAX_CONTENT 256
#define MAX_STATUS 64
#define MAX_PASS 32

struct file_entry {
    char name[MAX_NAME];
    int active;
    int is_remote; // 0: local, 1: remote
    int is_encrypted; // 0: plain, 1: encrypted
};

extern int sys_gui_create_window(int x, int y, int w, int h, unsigned char color);
extern int sys_gui_create_button(int win_id, int x, int y, int w, int h, const char *label);
extern int sys_gui_create_text_field(int win_id, int x, int y, int w, int h);
extern int sys_gui_create_context_menu(int win_id, int x, int y, const char *items);
extern int sys_gui_get_event(int *type, int *x, int *y, unsigned char *data);
extern void sys_vfs_list(const char *path, char *buf, int size);
extern int sys_vfs_read(const char *name, char *buf, int size);
extern int sys_vfs_write(const char *name, const char *buf, int size);
extern int sys_vfs_delete(const char *name);
extern int sys_p2p_share_file(const char *filename, unsigned int ip, unsigned short port);
extern int sys_p2p_list_files(unsigned int ip, unsigned short port, char *buf, int size);
extern int sys_p2p_download_file(unsigned int ip, unsigned short port, const char *filename, char *buf, int size);
extern int sys_crypto_encrypt_buffer(char *buf, int len);
extern int sys_crypto_decrypt_buffer(char *buf, int len);
extern int sys_crypto_sign_buffer(char *buf, int len, char *signature);
extern int sys_crypto_verify_buffer(const char *buf, int len, const char *signature);
extern int sys_vfs_compress_buffer(char *buf, int len, char *out, int max_out);
extern int sys_vfs_decompress_buffer(char *buf, int len, char *out, int max_out);
extern void sys_keymgmt_generate_key(char *name, unsigned char *key);
extern int sys_keymgmt_get_key(char *name, unsigned char *key);
extern int sys_keymgmt_authenticate(const char *password);
extern void sys_netbrowser_init(void);
extern void sys_netbrowser_run(void);
extern void sys_taskmgr_init(void);
extern void sys_taskmgr_run(void);
extern void sys_clipboard_copy(const char *data, int len);
extern int sys_clipboard_paste(char *buf, int max_len);
extern void sys_get_time(unsigned char *buf);
extern void sys_profiler_get_stats(char *buf, int size);

struct file_entry files[MAX_FILES];
int win_id = -1;
int text_field_id = -1;
int share_button_id = -1;
int status_bar_id = -1;
int tray_time_id = -1;
int tray_net_id = -1;
int tray_cpu_id = -1;
int tray_taskmgr_id = -1;
int tray_netbrowser_id = -1;
char selected_file[MAX_NAME];
int selected_file_idx = -1;
char status_message[MAX_STATUS];
int focused_file_idx = -1;
int auth_win_id = -1;
int auth_text_id = -1;
int auth_button_id = -1;
char current_key[MAX_NAME] = "default";

void update_status(const char *msg) {
    if (status_bar_id < 0) return;
    for (int i = 0; i < MAX_STATUS && msg[i]; i++)
        status_message[i] = msg[i];
    status_message[MAX_STATUS - 1] = 0;
    sys_gui_create_text_field(win_id, 10, 100, 200, 15);
}

void update_tray(void) {
    // Time
    unsigned char time_buf[32] = {0};
    sys_get_time(time_buf);
    tray_time_id = sys_gui_create_text_field(win_id, 10, 120, 60, 15);
    // Network status (simplified)
    tray_net_id = sys_gui_create_text_field(win_id, 80, 120, 60, 15);
    // CPU usage
    char stats[256] = {0};
    sys_profiler_get_stats(stats, 256);
    tray_cpu_id = sys_gui_create_text_field(win_id, 150, 120, 60, 15);
    // Quick-launch buttons
    tray_taskmgr_id = sys_gui_create_button(win_id, 220, 120, 50, 15, "Tasks");
    tray_netbrowser_id = sys_gui_create_button(win_id, 280, 120, 50, 15, "Network");
}

void file_explorer_init(void) {
    for (int i = 0; i < MAX_FILES; i++)
        files[i].active = files[i].is_remote = files[i].is_encrypted = 0;
    selected_file[0] = status_message[0] = 0;
    win_id = sys_gui_create_window(50, 50, 340, 140, 7); // Expanded for tray
    if (win_id < 0) return;

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
                files[idx].is_encrypted = (files[idx].name[0] == 'e');
                sys_gui_create_button(win_id, 10, y_offset, 100, 15, files[idx].name);
                y_offset += 20;
                idx++;
            }
        }
    }

    text_field_id = sys_gui_create_text_field(win_id, 120, 10, 90, 60);
    if (text_field_id < 0) return;
    share_button_id = sys_gui_create_button(win_id, 120, 80, 50, 20, "Share");
    status_bar_id = sys_gui_create_text_field(win_id, 10, 100, 200, 15);
    update_status("Ready");
    update_tray();
}

void file_explorer_auth_init(void) {
    auth_win_id = sys_gui_create_window(80, 80, 160, 80, 7);
    if (auth_win_id < 0) return;
    sys_gui_create_button(auth_win_id, 10, 10, 80, 15, "Password:");
    auth_text_id = sys_gui_create_text_field(auth_win_id, 10, 30, 140, 15);
    auth_button_id = sys_gui_create_button(auth_win_id, 60, 50, 40, 20, "Login");
}

void file_explorer_run(void) {
    if (auth_win_id >= 0) {
        int event_type, event_x, event_y;
        unsigned char event_data[32];
        while (sys_gui_get_event(&event_type, &event_x, &event_y, event_data) > 0) {
            if (event_type == 3 && event_data[0] == auth_button_id) {
                if (sys_keymgmt_authenticate((char *)event_data + 1) == 0) {
                    auth_win_id = -1;
                    file_explorer_init();
                    break;
                } else {
                    sys_gui_create_text_field(auth_win_id, 10, 50, 140, 15);
                }
            }
        }
        asm("hlt");
        return;
    }
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
                        sys_gui_create_context_menu(win_id, event_x, event_y, "Open\0Delete\0Share\0Encrypt\0Compress\0SetKey\0Copy\0Paste\0");
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
                } else if (button_id == tray_taskmgr_id) {
                    sys_taskmgr_init();
                    sys_taskmgr_run();
                } else if (button_id == tray_netbrowser_id) {
                    sys_netbrowser_init();
                    sys_netbrowser_run();
                } else {
                    for (int i = 0; i < MAX_FILES; i++) {
                        if (files[i].active && button_id == i) {
                            selected_file_idx = i;
                            for (int j = 0; j < MAX_NAME; j++)
                                selected_file[j] = files[i].name[j];
                            char content[MAX_CONTENT] = {0};
                            int len = sys_vfs_read(selected_file, content, MAX_CONTENT);
                            if (len > 0) {
                                if (files[i].is_encrypted)
                                    len = sys_crypto_decrypt_buffer(content, len);
                                sys_gui_create_text_field(win_id, 120, 10, 90, 60);
                                update_status("File opened");
                            } else {
                                update_status("Read failed");
                            }
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
                        int len = sys_vfs_read(selected_file, content, MAX_CONTENT);
                        if (len > 0) {
                            if (files[selected_file_idx].is_encrypted)
                                len = sys_crypto_decrypt_buffer(content, len);
                            sys_gui_create_text_field(win_id, 120, 10, 90, 60);
                            update_status("File opened");
                        } else {
                            update_status("Read failed");
                        }
                    } else if (item_id == 1) { // Delete
                        if (sys_vfs_delete(selected_file) == 0) {
                            files[selected_file_idx].active = 0;
                            selected_file_idx = -1;
                            selected_file[0] = 0;
                            file_explorer_init();
                            update_status("File deleted");
                        } else {
                            update_status("Delete failed");
                        }
                    } else if (item_id == 2) { // Share
                        if (sys_p2p_share_file(selected_file, 192168001, 1000) == 0)
                            update_status("File shared");
                        else
                            update_status("Share failed");
                    } else if (item_id == 3) { // Encrypt
                        char content[MAX_CONTENT] = {0};
                        int len = sys_vfs_read(selected_file, content, MAX_CONTENT);
                        if (len > 0) {
                            len = sys_crypto_encrypt_buffer(content, len);
                            char new_name[MAX_NAME] = "e";
                            for (int j = 1; j < MAX_NAME && selected_file[j-1]; j++)
                                new_name[j] = selected_file[j-1];
                            if (sys_vfs_write(new_name, content, len) == 0)
                                update_status("File encrypted");
                            else
                                update_status("Encrypt failed");
                            file_explorer_init();
                        }
                    } else if (item_id == 4) { // Compress
                        char content[MAX_CONTENT] = {0}, compressed[256] = {0};
                        int len = sys_vfs_read(selected_file, content, MAX_CONTENT);
                        if (len > 0) {
                            int comp_len = sys_vfs_compress_buffer(content, len, compressed, 256);
                            if (comp_len > 0 && sys_vfs_write(selected_file, compressed, comp_len) == 0)
                                update_status("File compressed");
                            else
                                update_status("Compress failed");
                            file_explorer_init();
                        }
                    } else if (item_id == 5) { // SetKey
                        char key_name[MAX_NAME] = "key1";
                        unsigned char key[16];
                        sys_keymgmt_generate_key(key_name, key);
                        for (int j = 0; j < MAX_NAME && key_name[j]; j++)
                            current_key[j] = key_name[j];
                        update_status("Key set");
                    } else if (item_id == 6) { // Copy
                        char content[MAX_CONTENT] = {0};
                        int len = sys_vfs_read(selected_file, content, MAX_CONTENT);
                        if (len > 0) {
                            if (files[selected_file_idx].is_encrypted)
                                len = sys_crypto_decrypt_buffer(content, len);
                            sys_clipboard_copy(content, len);
                            update_status("File copied to clipboard");
                        } else {
                            update_status("Copy failed");
                        }
                    } else if (item_id == 7) { // Paste
                        char content[MAX_CONTENT] = {0};
                        int len = sys_clipboard_paste(content, MAX_CONTENT);
                        if (len > 0) {
                            if (sys_vfs_write("pasted_file", content, len) == 0)
                                update_status("File pasted from clipboard");
                            else
                                update_status("Paste failed");
                            file_explorer_init();
                        } else {
                            update_status("Paste failed");
                        }
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
                    int len = sys_vfs_read(selected_file, content, MAX_CONTENT);
                    if (len > 0) {
                        if (files[selected_file_idx].is_encrypted)
                            len = sys_crypto_decrypt_buffer(content, len);
                        sys_gui_create_text_field(win_id, 120, 10, 90, 60);
                        update_status("File opened");
                    }
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
                files[idx].is_encrypted = (files[idx].name[0] == 'e');
                sys_gui_create_button(win_id, 10, y_offset, 100, 15, files[idx].name);
                y_offset += 20;
                idx++;
            }
        }
    }
    update_status("P2P files loaded");
    update_tray();
}
