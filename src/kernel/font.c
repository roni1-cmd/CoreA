#define FONT_WIDTH 8
#define FONT_HEIGHT 8

static const unsigned char font_data[128][8] = {
    {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}, // Space
    {0x00, 0x18, 0x18, 0x18, 0x18, 0x00, 0x18, 0x00}, // !
    {0x00, 0x66, 0x66, 0x00, 0x00, 0x00, 0x00, 0x00}, // "
    {0x00, 0x66, 0xff, 0x66, 0x66, 0xff, 0x66, 0x00}, // #
    {0x18, 0x3c, 0x60, 0x3c, 0x06, 0x7c, 0x18, 0x00}, // $
    {0x00, 0x66, 0x6c, 0x18, 0x30, 0x66, 0x46, 0x00}, // %
    {0x1c, 0x36, 0x1c, 0x38, 0x6f, 0x66, 0x3b, 0x00}, // &
    {0x00, 0x18, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00}, // '
    {0x00, 0x0c, 0x18, 0x18, 0x18, 0x18, 0x0c, 0x00}, // (
    {0x00, 0x30, 0x18, 0x18, 0x18, 0x18, 0x30, 0x00}, // )
    {0x00, 0x66, 0x3c, 0xff, 0x3c, 0x66, 0x00, 0x00}, // *
    {0x00, 0x18, 0x18, 0x7e, 0x18, 0x18, 0x00, 0x00}, // +
    {0x00, 0x00, 0x00, 0x00, 0x00, 0x18, 0x18, 0x30}, // ,
    {0x00, 0x00, 0x00, 0x7e, 0x00, 0x00, 0x00, 0x00}, // -
    {0x00, 0x00, 0x00, 0x00, 0x00, 0x18, 0x18, 0x00}, // .
    {0x00, 0x06, 0x0c, 0x18, 0x30, 0x60, 0x40, 0x00}, // /
    {0x3c, 0x66, 0x6e, 0x76, 0x66, 0x66, 0x3c, 0x00}, // 0
    {0x18, 0x18, 0x38, 0x18, 0x18, 0x18, 0x7e, 0x00}, // 1
    {0x3c, 0x66, 0x06, 0x0c, 0x30, 0x60, 0x7e, 0x00}, // 2
    {0x3c, 0x66, 0x06, 0x1c, 0x06, 0x66, 0x3c, 0x00}, // 3
    {0x06, 0x0e, 0x1e, 0x66, 0x7f, 0x06, 0x06, 0x00}, // 4
    {0x7e, 0x60, 0x7c, 0x06, 0x06, 0x66, 0x3c, 0x00}, // 5
    {0x3c, 0x66, 0x60, 0x7c, 0x66, 0x66, 0x3c, 0x00}, // 6
    {0x7e, 0x66, 0x0c, 0x18, 0x18, 0x18, 0x18, 0x00}, // 7
    {0x3c, 0x66, 0x66, 0x3c, 0x66, 0x66, 0x3c, 0x00}, // 8
    {0x3c, 0x66, 0x66, 0x3e, 0x06, 0x66, 0x3c, 0x00}, // 9
    {0x00, 0x18, 0x18, 0x00, 0x18, 0x18, 0x00, 0x00}, // :
    {0x00, 0x18, 0x18, 0x00, 0x18, 0x18, 0x30, 0x00}, // ;
    {0x00, 0x0c, 0x18, 0x30, 0x18, 0x0c, 0x00, 0x00}, // <
    {0x00, 0x00, 0x7e, 0x00, 0x7e, 0x00, 0x00, 0x00}, // =
    {0x00, 0x30, 0x18, 0x0c, 0x18, 0x30, 0x00, 0x00}, // >
    {0x3c, 0x66, 0x06, 0x0c, 0x18, 0x00, 0x18, 0x00}, // ?
    {0x3c, 0x66, 0x6e, 0x6e, 0x60, 0x62, 0x3c, 0x00}, // @
    {0x3c, 0x66, 0x66, 0x7e, 0x66, 0x66, 0x66, 0x00}, // A
    {0x7c, 0x66, 0x66, 0x7c, 0x66, 0x66, 0x7c, 0x00}, // B
    {0x3c, 0x66, 0x60, 0x60, 0x60, 0x66, 0x3c, 0x00}, // C
    {0x78, 0x6c, 0x66, 0x66, 0x66, 0x6c, 0x78, 0x00}, // D
    {0x7e, 0x60, 0x60, 0x7c, 0x60, 0x60, 0x7e, 0x00}, // E
    {0x7e, 0x60, 0x60, 0x7c, 0x60, 0x60, 0x60, 0x00}, // F
    {0x3c, 0x66, 0x60, 0x6e, 0x66, 0x66, 0x3c, 0x00}, // G
    {0x66, 0x66, 0x66, 0x7e, 0x66, 0x66, 0x66, 0x00}, // H
    {0x3c, 0x18, 0x18, 0x18, 0x18, 0x18, 0x3c, 0x00}, // I
    {0x1e, 0x0c, 0x0c, 0x0c, 0x0c, 0x6c, 0x38, 0x00}, // J
    {0x66, 0x6c, 0x78, 0x70, 0x78, 0x6c, 0x66, 0x00}, // K
    {0x60, 0x60, 0x60, 0x60, 0x60, 0x60, 0x7e, 0x00}, // L
    {0x63, 0x77, 0x7f, 0x6b, 0x63, 0x63, 0x63, 0x00}, // M
    {0x66, 0x66, 0x76, 0x7e, 0x6e, 0x66, 0x66, 0x00}, // N
    {0x3c, 0x66, 0x66, 0x66, 0x66, 0x66, 0x3c, 0x00}, // O
    {0x7c, 0x66, 0x66, 0x7c, 0x60, 0x60, 0x60, 0x00}, // P
    {0x3c, 0x66, 0x66, 0x66, 0x6e, 0x3c, 0x0e, 0x00}, // Q
    {0x7c, 0x66, 0x66, 0x7c, 0x78, 0x6c, 0x66, 0x00}, // R
    {0x3c, 0x66, 0x60, 0x3c, 0x06, 0x66, 0x3c, 0x00}, // S
    {0x7e, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x00}, // T
    {0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x3c, 0x00}, // U
    {0x66, 0x66, 0x66, 0x66, 0x66, 0x3c, 0x18, 0x00}, // V
    {0x63, 0x63, 0x63, 0x6b, 0x7f, 0x77, 0x63, 0x00}, // W
    {0x66, 0x66, 0x3c, 0x18, 0x3c, 0x66, 0x66, 0x00}, // X
    {0x66, 0x66, 0x66, 0x3c, 0x18, 0x18, 0x18, 0x00}, // Y
    {0x7e, 0x06, 0x0c, 0x18, 0x30, 0x60, 0x7e, 0x00}, // Z
    // ... (simplified; remaining chars follow similar bitmap patterns)
};

void draw_char(int x, int y, char c, unsigned char color) {
    if (c < 32 || c > 127) return;
    unsigned char *vga = (unsigned char *)0xa0000;
    for (int i = 0; i < FONT_HEIGHT; i++) {
        for (int j = 0; j < FONT_WIDTH; j++) {
            if (font_data[c - 32][i] & (1 << (7 - j))) {
                if (x + j < 320 && y + i < 200)
                    vga[(y + i) * 320 + (x + j)] = color;
            }
        }
    }
}

void draw_string(int x, int y, const char *str, unsigned char color) {
    if (!str) return;
    for (int i = 0; str[i]; i++) {
        draw_char(x + i * FONT_WIDTH, y, str[i], color);
    }
}
