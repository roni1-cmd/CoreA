#define MAX_NAME 32

extern int sys_gui_create_window(int x, int y, int w, int h, unsigned char color);
extern int sys_gui_create_button(int win_id, int x, int y, int w, int h, const char *label);
extern int sys_gui_create_text_field(int win_id, int x, int y, int w, int h);
extern int sys_gui_get_event(int *type, int *x, int *y, unsigned char *data);
extern void sys_keymgmt_generate_key(char *name, unsigned char *key);
extern int sys_set_brightness(int level);
extern int sys_set_network_config(unsigned int ip, unsigned int mask);

int win_id = -1;
int brightness_field_id = -1;
int ip_field_id = -1;
int mask_field_id = -1;
int key_field_id = -1;
int apply_button_id = -1;

void settings_init(void) {
    win_id = sys_gui_create_window(90, 90, 200, 140, 7); // Light gray
    if (win_id < 0) return;

    sys_gui_create_button(win_id, 10, 10, 80, 15, "Brightness:");
    brightness_field_id = sys_gui_create_text_field(win_id, 100, 10, 90, 15);
    sys_gui_create_button(win_id, 10, 30, 80, 15, "IP Address:");
    ip_field_id = sys_gui_create_text_field(win_id, 100, 30, 90, 15);
    sys_gui_create_button(win_id, 10, 50, 80, 15, "Subnet Mask:");
    mask_field_id = sys_gui_create_text_field(win_id, 100, 50, 90, 15);
    sys_gui_create_button(win_id, 10, 70, 80, 15, "Default Key:");
    key_field_id = sys_gui_create_text_field(win_id, 100, 70, 90, 15);
    apply_button_id = sys_gui_create_button(win_id, 70, 100, 60, 20, "Apply");
}

void settings_run(void) {
    if (win_id < 0) return;
    int event_type, event_x, event_y;
    unsigned char event_data[256];

    while (1) {
        if (sys_gui_get_event(&event_type, &event_x, &event_y, event_data) > 0) {
            if (event_type == 3 && event_data[0] == apply_button_id) {
                // Parse brightness
                int brightness = 0;
                for (int i = 1; event_data[i] && event_data[i] != ' '; i++)
                    brightness = brightness * 10 + (event_data[i] - '0');
                sys_set_brightness(brightness);

                // Parse IP and mask
                unsigned int ip = 0, mask = 0;
                int offset = 1;
                for (; event_data[offset] != ' '; offset++);
                offset++;
                for (int i = offset; event_data[i] && event_data[i] != ' '; i++)
                    ip = ip * 10 + (event_data[i] - '0');
                for (; event_data[offset] != ' '; offset++);
                offset++;
                for (int i = offset; event_data[i] && event_data[i] != ' '; i++)
                    mask = mask * 10 + (event_data[i] - '0');
                sys_set_network_config(ip, mask);

                // Set default key
                char key_name[MAX_NAME] = {0};
                for (int i = offset; event_data[i] && i - offset < MAX_NAME; i++)
                    key_name[i - offset] = event_data[i];
                unsigned char key[16];
                sys_keymgmt_generate_key(key_name, key);

                sys_gui_create_text_field(win_id, 10, 120, 180, 15); // Status
            }
        }
        asm("hlt");
    }
}
