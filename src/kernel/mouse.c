#define MOUSE_PORT 0x60
#define MOUSE_CMD 0x64
#define MAX_EVENTS 32

struct mouse_event {
    int x, y;
    unsigned char buttons;
    int valid;
};

struct mouse_event mouse_queue[MAX_EVENTS];
int mouse_pos = 0;
int mouse_x = 160, mouse_y = 100; // Center of 320x200 VGA
extern void outb(unsigned short port, unsigned char val);
extern unsigned char inb(unsigned short port);

void init_mouse(void) {
    for (int i = 0; i < MAX_EVENTS; i++)
        mouse_queue[i].valid = 0;
    outb(MOUSE_CMD, 0xa8);
    outb(MOUSE_CMD, 0x20);
    unsigned char status = inb(MOUSE_PORT) | 2;
    outb(MOUSE_CMD, 0x60);
    outb(MOUSE_PORT, status);
    outb(MOUSE_CMD, 0xd4);
    outb(MOUSE_PORT, 0xf4);
    while (inb(MOUSE_PORT) != 0xfa);
}

void mouse_handler(void) {
    static unsigned char packet[3];
    static int packet_pos = 0;
    unsigned char data = inb(MOUSE_PORT);
    if (packet_pos == 0 && !(data & 0x08)) return; // Sync check
    packet[packet_pos++] = data;
    if (packet_pos == 3) {
        packet_pos = 0;
        if (mouse_pos < MAX_EVENTS) {
            int dx = packet[1] - ((packet[0] & 0x10) ? 256 : 0);
            int dy = packet[2] - ((packet[0] & 0x20) ? 256 : 0);
            mouse_x += dx;
            mouse_y -= dy;
            if (mouse_x < 0) mouse_x = 0;
            if (mouse_x >= 320) mouse_x = 319;
            if (mouse_y < 0) mouse_y = 0;
            if (mouse_y >= 200) mouse_y = 199;
            mouse_queue[mouse_pos].x = mouse_x;
            mouse_queue[mouse_pos].y = mouse_y;
            mouse_queue[mouse_pos].buttons = packet[0] & 0x07;
            mouse_queue[mouse_pos].valid = 1;
            mouse_pos++;
        }
        outb(0x20, 0x20);
    }
}
