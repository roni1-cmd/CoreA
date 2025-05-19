#define RTL8139_BASE 0xc000
#define MAC0 0x00
#define RBSTART 0x30
#define CMD 0x37
#define IMR 0x3c
#define ISR 0x3e
#define MAX_PACKET 1500

unsigned char rx_buffer[8192 + 16];
extern void outb(unsigned short port, unsigned char val);
extern unsigned char inb(unsigned short port);
extern void *kmalloc(unsigned int size);

void outl(unsigned short port, unsigned int val) {
    asm volatile("outl %0, %1" : : "a"(val), "Nd"(port));
}

unsigned int inl(unsigned short port) {
    unsigned int ret;
    asm volatile("inl %1, %0" : "=a"(ret) : "Nd"(port));
    return ret;
}

void init_net(void) {
    outb(RTL8139_BASE + CMD, 0x10);
    while (inb(RTL8139_BASE + CMD) & 0x10);
    outl(RTL8139_BASE + RBSTART, (unsigned int)rx_buffer);
    outb(RTL8139_BASE + CMD, 0x0c);
    outb(RTL8139_BASE + IMR, 0x0005);
}

int net_send_packet(unsigned char *data, int len) {
    if (len > MAX_PACKET) return -1;
    outl(RTL8139_BASE + 0x20, (unsigned int)data);
    outl(RTL8139_BASE + 0x10, len);
    while (inl(RTL8139_BASE + 0x10) & 0x1000);
    return 0;
}

int net_receive_packet(unsigned char *buf, int size) {
    if (inb(RTL8139_BASE + ISR) & 0x01) {
        unsigned int *header = (unsigned int *)rx_buffer;
        int len = header[1] & 0xffff;
        if (len > size) len = size;
        for (int i = 0; i < len; i++)
            buf[i] = rx_buffer[4 + i];
        outb(RTL8139_BASE + ISR, 0x01);
        return len;
    }
    return 0;
}
