#define VGA_WIDTH 80
#define VGA_HEIGHT 25
#define VGA_BUFFER 0xB8000
extern char kbd_buffer[256];
extern int kbd_pos;
extern volatile unsigned int ticks;
void outb(unsigned short port, unsigned char val) {
    asm volatile("outb %0, %1" : : "a"(val), "Nd"(port));
}
void vga_clear(void) {
    unsigned short *vga = (unsigned short *)VGA_BUFFER;
    for (int i = 0; i < VGA_WIDTH * VGA_HEIGHT; i++)
        vga[i] = 0x0700;
}
void vga_print(const char *str, int row, int col) {
    unsigned short *vga = (unsigned short *)VGA_BUFFER;
    int pos = row * VGA_WIDTH + col;
    for (; *str; str++, pos++)
        vga[pos] = (0x07 << 8) | *str;
}
void vga_scroll(void) {
    unsigned short *vga = (unsigned short *)VGA_BUFFER;
    for (int i = VGA_WIDTH; i < VGA_WIDTH * VGA_HEIGHT; i++)
        vga[i - VGA_WIDTH] = vga[i];
    for (int i = 0; i < VGA_WIDTH; i++)
        vga[(VGA_HEIGHT - 1) * VGA_WIDTH + i] = 0x0700;
}
void run_shell(void) {
    char input[256];
    int input_pos = 0;
    int row = 1;
    vga_print("shell> ", row, 0);
    int col = 7;
    while (1) {
        if (kbd_pos > 0) {
            char c = kbd_buffer[0];
            kbd_pos = 0;
            if (c == '\n' || c == '\r') {
                input[input_pos] = 0;
                if (input_pos > 0) {
                    row++;
                    if (row >= VGA_HEIGHT) {
                        vga_scroll();
                        row--;
                    }
                    if (input[0] == 'c' && input[1] == 'l' && input[2] == 'e' && input[3] == 'a' && input[4] == 'r' && input[5] == 0) {
                        vga_clear();
                        row = 1;
                    } else if (input[0] == 'e' && input[1] == 'c' && input[2] == 'h' && input[3] == 'o' && input[4] == ' ') {
                        vga_print(input + 5, row, 0);
                        row++;
                    } else {
                        vga_print("Unknown command", row, 0);
                        row++;
                    }
                    if (row >= VGA_HEIGHT) {
                        vga_scroll();
                        row--;
                    }
                    vga_print("shell> ", row, 0);
                    col = 7;
                    input_pos = 0;
                }
            } else if (c >= 32 && c <= 126) {
                input[input_pos++] = c;
                unsigned short *vga = (unsigned short *)VGA_BUFFER;
                vga[row * VGA_WIDTH + col] = (0x07 << 8) | c;
                col++;
            }
        }
        asm("hlt");
    }
}
