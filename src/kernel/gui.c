#define VGA_WIDTH 320
#define VGA_HEIGHT 200
#define VGA_BUFFER 0xa0000
#define MAX_WINDOWS 4
#define MAX_BUTTONS 8
#define MAX_EVENTS 32

struct window {
    int x, y, w, h;
    unsigned char color;
    int active;
};

struct button {
    int x, y, w, h;
    int win_id;
    char label[16];
    int active;
};

struct gui_event {
    int type; // 0: none, 1: mouse_click, 2: key_press
    int x, y;
    unsigned char data;
    int valid;
};

struct window windows[MAX_WINDOWS];
struct button buttons[MAX_BUTTONS];
struct gui_event event_queue[MAX_EVENTS];
int event_pos = 0;
unsigned char *vga_buffer;
extern void draw_string(int x, int y, const char *str, unsigned char color);
extern int mouse_x, mouse_y;
extern struct mouse_event {
    int x, y;
    unsigned char buttons;
    int valid;
} mouse_queue[32];
extern int mouse_pos;
extern char usb_kbd_buffer[32];
extern int usb_kbd_pos;

void init_gui(void) {
    asm volatile(
        "mov $0x13, %ax;"
        "int $0x10;"
        ::: "eax"
    );
    vga_buffer = (unsigned char *)VGA_BUFFER;
    for (int i = 0; i < MAX_WINDOWS; i++)
        windows[i].active = 0;
    for (int i = 0; i < MAX_BUTTONS; i++)
        buttons[i].active = 0;
    for (int i = 0; i < MAX_EVENTS; i++)
        event_queue[i].valid = 0;
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
    if (x < 0 || y < 0 || w <= 0 || h <= 0 || x + w > VGA_WIDTH || y + h > VGA_HEIGHT)
        return -1;
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
            draw_string(x + 2, y + 1, "Window", color + 2);
            return i;
        }
    }
    return -1;
}

int gui_create_button(int win_id, int x, int y, int w, int h, const char *label) {
    if (win_id < 0 || win_id >= MAX_WINDOWS || !windows[win_id].active || !label)
        return -1;
    if (x < 0 || y < 0 || w <= 0 || h <= 0 || x + w > windows[win_id].w || y + h > windows[win_id].h)
        return -1;
    for (int i = 0; i < MAX_BUTTONS; i++) {
        if (!buttons[i].active) {
            buttons[i].x = windows[win_id].x + x;
            buttons[i].y = windows[win_id].y + y;
            buttons[i].w = w;
            buttons[i].h = h;
            buttons[i].win_id = win_id;
            for (int j = 0; j < 15 && label[j]; j++)
                buttons[i].label[j] = label[j];
            buttons[i].label[15] = 0;
            buttons[i].active = 1;
            gui_draw_rect(buttons[i].x, buttons[i].y, w, h, 7);
            draw_string(buttons[i].x + 2, buttons[i].y + 2, buttons[i].label, 0);
            return i;
        }
    }
    return -1;
}

void gui_handle_events(void) {
    while (mouse_pos > 0) {
        mouse_pos--;
        if (mouse_queue[mouse_pos].valid && event_pos < MAX_EVENTS) {
            if (mouse_queue[mouse_pos].buttons & 0x01) {
                event_queue[event_pos].type = 1;
                event_queue[event_pos].x = mouse_queue[mouse_pos].x;
                event_queue[event_pos].y = mouse_queue[mouse_pos].y;
                event_queue[event_pos].data = 0;
                event_queue[event_pos].valid = 1;
                event_pos++;
            }
            mouse_queue[mouse_pos].valid = 0;
        }
    }
    while (usb_kbd_pos > 0) {
        char c = usb_kbd_buffer[0];
        usb_kbd_pos--;
        if (event_pos < MAX_EVENTS && c >= 32 && c <= 127) {
            event_queue[event_pos].type = 2;
            event_queue[event_pos].x = 0;
            event_queue[event_pos].y = 0;
            event_queue[event_pos].data = c;
            event_queue[event_pos].valid = 1;
            event_pos++;
        }
    }
}

int gui_get_event(int *type, int *x, int *y, unsigned char *data) {
    if (!type || !x || !y || !data || event_pos <= 0) return -1;
    event_pos--;
    if (event_queue[event_pos].valid) {
        *type = event_queue[event_pos].type;
        *x = event_queue[event_pos].x;
        *y = event_queue[event_pos].y;
        *data = event_queue[event_pos].data;
        event_queue[event_pos].valid = 0;
        return 0;
    }
    return -1;
}

void gui_update(void) {
    for (int i = 0; i < MAX_WINDOWS; i++) {
        if (windows[i].active) {
            gui_draw_rect(windows[i].x, windows[i].y, windows[i].w, windows[i].h, windows[i].color);
            gui_draw_rect(windows[i].x, windows[i].y, windows[i].w, 10, windows[i].color + 1);
            draw_string(windows[i].x + 2, windows[i].y + 1, "Window", windows[i].color + 2);
        }
    }
    for (int i = 0; i < MAX_BUTTONS; i++) {
        if (buttons[i].active) {
            gui_draw_rect(buttons[i].x, buttons[i].y, buttons[i].w, buttons[i].h, 7);
            draw_string(buttons[i].x + 2, buttons[i].y + 2, buttons[i].label, 0);
        }
    }
    gui_draw_pixel(mouse_x, mouse_y, 15);
}
