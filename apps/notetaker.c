#define MAX_NOTES 16
#define MAX_NAME 32
#define MAX_CONTENT 256
#define MAX_MESSAGE 64

extern int sys_gui_create_window(int x, int y, int w, int h, unsigned char color);
extern int sys_gui_create_button(int win_id, int x, int y, int w, int h, const char *label);
extern int sys_gui_create_text_field(int win_id, int x, int y, int w, int h);
extern int sys_gui_get_event(int *type, int *x, int *y, unsigned char *data);
extern void sys_vfs_list(const char *path, char *buf, int size);
extern int sys_vfs_read(const char *name, char *buf, int size);
extern int sys_vfs_write(const char *name, const char *buf, int size);
extern int sys_vfs_delete(const char *name);
extern int sys_crypto_encrypt_buffer(char *buf, int len);
extern int sys_crypto_decrypt_buffer(char *buf, int len);
extern void sys_logger_add_event(const char *message);

struct note_entry {
    char name[MAX_NAME];
    int active;
    int is_encrypted;
};

int win_id = -1;
int input_field_id = -1;
int save_button_id = -1;
int encrypt_button_id = -1;
int delete_button_id = -1;
struct note_entry notes[MAX_NOTES];
int note_buttons[MAX_NOTES];
int selected_note_idx = -1;

void notetaker_init(void) {
    win_id = sys_gui_create_window(150, 150, 240, 180, 7); // Light gray
    if (win_id < 0) return;

    input_field_id = sys_gui_create_text_field(win_id, 10, 10, 220, 40);
    save_button_id = sys_gui_create_button(win_id, 10, 60, 60, 20, "Save");
    encrypt_button_id = sys_gui_create_button(win_id, 80, 60, 80, 20, "Save Encrypted");
    delete_button_id = sys_gui_create_button(win_id, 170, 60, 60, 20, "Delete");

    // Initialize note list
    for (int i = 0; i < MAX_NOTES; i++) {
        notes[i].active = 0;
        note_buttons[i] = -1;
    }
    notetaker_update_notes();
    sys_logger_add_event("Note taker launched");
}

void notetaker_update_notes(void) {
    char file_list[256] = {0};
    sys_vfs_list("/ram/notes", file_list, 256);
    int idx = 0, y_offset = 90;
    for (int i = 0; file_list[i] && idx < MAX_NOTES; i++) {
        if (file_list[i] == '\n') {
            file_list[i] = 0;
            if (i > 0 && file_list[i-1]) {
                for (int j = 0; j < MAX_NAME && file_list[i-j-1]; j++)
                    notes[idx].name[j] = file_list[i-j-1];
                notes[idx].active = 1;
                notes[idx].is_encrypted = (notes[idx].name[0] == 'e');
                note_buttons[idx] = sys_gui_create_button(win_id, 10, y_offset, 220, 15, notes[idx].name);
                y_offset += 20;
                idx++;
            }
        }
    }
    for (; idx < MAX_NOTES; idx++) {
        notes[idx].active = 0;
        note_buttons[idx] = -1;
    }
}

void notetaker_run(void) {
    if (win_id < 0) return;
    int event_type, event_x, event_y;
    unsigned char event_data[256];

    while (1) {
        if (sys_gui_get_event(&event_type, &event_x, &event_y, event_data) > 0) {
            if (event_type == 3) { // Button click
                int button_id = event_data[0];
                char content[MAX_CONTENT] = {0};
                int content_len = 0;
                for (int i = 0; i < MAX_CONTENT && event_data[i + 1]; i++)
                    content[content_len++] = event_data[i + 1];

                if (button_id == save_button_id && content_len > 0) {
                    char filename[MAX_NAME] = "note_";
                    int note_num = 0;
                    for (int i = 0; i < MAX_NOTES; i++)
                        if (!notes[i].active) break; else note_num++;
                    for (int i = 0; i < 10 && note_num; i++) {
                        filename[5 + i] = '0' + (note_num % 10);
                        note_num /= 10;
                    }
                    if (sys_vfs_write(filename, content, content_len) >= 0) {
                        sys_gui_create_text_field(win_id, 10, 10, 220, 40); // Clear input
                        notetaker_update_notes();
                        sys_logger_add_event("Note saved");
                    }
                } else if (button_id == encrypt_button_id && content_len > 0) {
                    char filename[MAX_NAME] = "enote_";
                    int note_num = 0;
                    for (int i = 0; i < MAX_NOTES; i++)
                        if (!notes[i].active) break; else note_num++;
                    for (int i = 0; i < 10 && note_num; i++) {
                        filename[6 + i] = '0' + (note_num % 10);
                        note_num /= 10;
                    }
                    content_len = sys_crypto_encrypt_buffer(content, content_len);
                    if (sys_vfs_write(filename, content, content_len) >= 0) {
                        sys_gui_create_text_field(win_id, 10, 10, 220, 40); // Clear input
                        notetaker_update_notes();
                        sys_logger_add_event("Encrypted note saved");
                    }
                } else if (button_id == delete_button_id && selected_note_idx >= 0) {
                    if (sys_vfs_delete(notes[selected_note_idx].name) >= 0) {
                        notes[selected_note_idx].active = 0;
                        selected_note_idx = -1;
                        notetaker_update_notes();
                        sys_logger_add_event("Note deleted");
                    }
                } else {
                    for (int i = 0; i < MAX_NOTES; i++) {
                        if (notes[i].active && note_buttons[i] == button_id) {
                            selected_note_idx = i;
                            char note_content[MAX_CONTENT] = {0};
                            int len = sys_vfs_read(notes[i].name, note_content, MAX_CONTENT);
                            if (len > 0) {
                                if (notes[i].is_encrypted)
                                    len = sys_crypto_decrypt_buffer(note_content, len);
                                sys_gui_create_text_field(win_id, 10, 10, 220, 40);
                                sys_logger_add_event("Note opened");
                            }
                            break;
                        }
                    }
                }
            }
        }
        asm("hlt");
    }
}
