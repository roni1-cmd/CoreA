#define MAX_NAME 32
#define BMP_HEADER_SIZE 14
#define DIB_HEADER_SIZE 40

extern int sys_gui_create_window(int x, int y, int w, int h, unsigned char color);
extern int sys_gui_create_button(int win_id, int x, int y, int w, int h, const char *label);
extern int sys_gui_get_event(int *type, int *x, int *y, unsigned char *data);
extern int sys_vfs_read(const char *name, char *buf, int size);
extern void sys_gui_draw_pixel(int win_id, int x, int y, unsigned char color);
extern void sys_logger_add_event(const char *message);

int win_id = -1;
char filename[MAX_NAME];

void imageviewer_init(const char *name) {
    for (int i = 0; i < MAX_NAME && name[i]; i++)
        filename[i] = name[i];
    filename[MAX_NAME - 1] = 0;

    win_id = sys_gui_create_window(120, 120, 200, 160, 7); // Light gray
    if (win_id < 0) return;

    char buffer[4096] = {0};
    int len = sys_vfs_read(filename, buffer, 4096);
    if (len < BMP_HEADER_SIZE + DIB_HEADER_SIZE) return;

    // Basic BMP parsing (16-color, uncompressed)
    int width = *(int *)&buffer[18];
    int height = *(int *)&buffer[22];
    int bits_per_pixel = *(unsigned short *)&buffer[28];
    int data_offset = *(int *)&buffer[10];

    if (bits_per_pixel != 4 || width > 180 || height > 140) return;

    // Draw pixels
    int row_size = ((width * bits_per_pixel + 31) / 32) * 4;
    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            int pixel_idx = data_offset + (height - 1 - y) * row_size + x / 2;
            unsigned char pixel = buffer[pixel_idx];
            unsigned char color = (x % 2 == 0) ? (pixel >> 4) : (pixel & 0x0F);
            sys_gui_draw_pixel(win_id, 10 + x, 10 + y, color);
        }
    }

    sys_gui_create_button(win_id, 80, 130, 40, 20, "Close");
    sys_logger_add_event("Image viewer opened");
}

void imageviewer_run(void) {
    if (win_id < 0) return;
    int event_type, event_x, event_y;
    unsigned char event_data[32];

    while (1) {
        if (sys_gui_get_event(&event_type, &event_x, &event_y, event_data) > 0) {
            if (event_type == 3) { // Button click
                sys_logger_add_event("Image viewer closed");
                return; // Exit on close
            }
        }
        asm("hlt");
    }
}
