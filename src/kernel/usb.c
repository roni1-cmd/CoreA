#define UHCI_BASE 0xc100
#define USBCMD 0x00
#define USBSTS 0x02
#define USBINTR 0x04
#define FRNUM 0x06
#define FLBASEADD 0x08
#define SOF 0x0c
#define PORTSC1 0x10
#define PORTSC2 0x12
#define MAX_KEYS 32

struct uhci_td {
    unsigned int link;
    unsigned int cs;
    unsigned int status;
    unsigned int buffer;
};

struct uhci_qh {
    unsigned int head;
    unsigned int element;
};

unsigned char usb_kbd_buffer[MAX_KEYS];
int usb_kbd_pos = 0;
extern void outb(unsigned short port, unsigned char val);
extern unsigned char inb(unsigned short port);
extern void outw(unsigned short port, unsigned short val);
extern unsigned short inw(unsigned short port);
extern void *kmalloc(unsigned int size);

void init_usb(void) {
    outw(UHCI_BASE + USBCMD, 0);
    outw(UHCI_BASE + USBSTS, 0x3f);
    outw(UHCI_BASE + USBINTR, 0x0f);
    outw(UHCI_BASE + FRNUM, 0);
    struct uhci_qh *qh = kmalloc(sizeof(struct uhci_qh));
    struct uhci_td *td = kmalloc(sizeof(struct uhci_td));
    qh->head = 0;
    qh->element = (unsigned int)td | 1;
    td->link = 0;
    td->cs = (8 << 21) | (0x80 << 16);
    td->status = 0x80;
    td->buffer = (unsigned int)usb_kbd_buffer;
    outw(UHCI_BASE + FLBASEADD, (unsigned int)qh);
    outw(UHCI_BASE + SOF, 0x40);
    outw(UHCI_BASE + PORTSC1, 0x0080);
    outw(UHCI_BASE + PORTSC2, 0x0080);
    outw(UHCI_BASE + USBCMD, 0x01);
}

void usb_handler(void) {
    unsigned short status = inw(UHCI_BASE + USBSTS);
    if (status & 0x01) {
        struct uhci_td *td = (struct uhci_td *)(inw(UHCI_BASE + FLBASEADD) & ~0xf);
        if (td->status & 0x80) return;
        for (int i = 0; i < 8 && usb_kbd_pos < MAX_KEYS; i++) {
            if (usb_kbd_buffer[i] >= 0x04 && usb_kbd_buffer[i] <= 0x1d)
                usb_kbd_buffer[usb_kbd_pos++] = 'a' + usb_kbd_buffer[i] - 0x04;
        }
        td->status = 0x80;
        outw(UHCI_BASE + USBSTS, 0x01);
    }
}
