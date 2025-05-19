#define VGA_WIDTH 320
#define VGA_HEIGHT 200
#define VGA_BUFFER 0xa0000
#define MAX_WINDOWS 4

struct window {
    int x, y, w, h;
    unsigned char color;
    int active;
};

struct window windows[MAX_WINDOWS];
unsigned char *vga_buffer;
extern void *kmalloc(unsigned int size);

void init_gui(void) {
    asm volatile(
        "mov $0x13, %ax;"
        "int $0x10;"
        ::: "eax"
    );
    vga_buffer = (unsigned char *)VGA_BUFFER;
    for (int i = 0; i < MAX_WINDOWS; i++)
        windows[i].active = 0;
}

void gui_draw_pixel(int x, int y, unsigned char color) {
    if (x >= 0 && x < VGA_WIDTH && y >= 0 && y < VGA_HEIGHT)
        vga_buffer[y * VGA_WIDTH + x] = color;
}

void gui_draw_rect(int x, int y, int w, int h, unsigned char color) {
    for (int i = y; i < y + h && i < VGA_HEIGHT; i++)
        for (int j = x; j < x + w && j < VGA_WIDTH; j++)
            gui_draw_pixel(j, i, color);
}

int gui_create_window(int x, int y, int w, int h, unsigned char color) {
    for (int i = 0; i < MAX_WINDOWS; i++) {
        if (!windows[i].active) {
            windows[i].x = x;
            windows[i].y = y;
            windows[i].w = w;
            windows[i].h = h;
            windows[i].color = color;
            windows[i].active = 1;
            gui_draw_rect(x, y, w, h, color);
            gui_draw_rect(x, y, w, 10, color + 1);
            return i;
        }
    }
    return -1;
}

void gui_update(void) {
    for (int i = 0; i < MAX_WINDOWS; i++) {
        if (windows[i].active)
            gui_draw_rect(windows[i].x, windows[i].y, windows[i].w, windows[i].h, windows[i].color);
    }
}
