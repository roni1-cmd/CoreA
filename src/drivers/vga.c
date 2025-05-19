/*
 * CoreA OS VGA Text Mode Driver
 * Provides basic text output functionality
 */

#include <drivers/vga.h>
#include <lib/string.h>

// VGA text mode buffer
static uint16_t* vga_buffer = (uint16_t*)0xB8000;
static size_t vga_row = 0;
static size_t vga_column = 0;
static uint8_t vga_color = VGA_COLOR_LIGHT_GREY | (VGA_COLOR_BLACK << 4);

// Hardware text mode color constants
static const uint8_t vga_entry_color(enum vga_color fg, enum vga_color bg) {
    return fg | (bg << 4);
}

static uint16_t vga_entry(unsigned char uc, uint8_t color) {
    return (uint16_t) uc | (uint16_t) color << 8;
}

void vga_init(void) {
    vga_row = 0;
    vga_column = 0;
    vga_color = vga_entry_color(VGA_COLOR_LIGHT_GREY, VGA_COLOR_BLACK);
}

void vga_set_color(uint8_t fg, uint8_t bg) {
    vga_color = vga_entry_color(fg, bg);
}

void vga_clear(void) {
    for (size_t y = 0; y < VGA_HEIGHT; y++) {
        for (size_t x = 0; x < VGA_WIDTH; x++) {
            const size_t index = y * VGA_WIDTH + x;
            vga_buffer[index] = vga_entry(' ', vga_color);
        }
    }
    vga_row = 0;
    vga_column = 0;
}

void vga_set_cursor(size_t x, size_t y) {
    if (x < VGA_WIDTH && y < VGA_HEIGHT) {
        vga_column = x;
        vga_row = y;
    }
}

static void vga_scroll(void) {
    // Move all lines up by one
    for (size_t y = 0; y < VGA_HEIGHT - 1; y++) {
        for (size_t x = 0; x < VGA_WIDTH; x++) {
            vga_buffer[y * VGA_WIDTH + x] = vga_buffer[(y + 1) * VGA_WIDTH + x];
        }
    }
    
    // Clear the last line
    for (size_t x = 0; x < VGA_WIDTH; x++) {
        vga_buffer[(VGA_HEIGHT - 1) * VGA_WIDTH + x] = vga_entry(' ', vga_color);
    }
    
    vga_row = VGA_HEIGHT - 1;
    vga_column = 0;
}

void vga_putchar(char c) {
    switch (c) {
        case '\n':
            vga_column = 0;
            if (++vga_row >= VGA_HEIGHT) {
                vga_scroll();
            }
            break;
            
        case '\r':
            vga_column = 0;
            break;
            
        case '\t':
            vga_column = (vga_column + 8) & ~(8 - 1);
            if (vga_column >= VGA_WIDTH) {
                vga_column = 0;
                if (++vga_row >= VGA_HEIGHT) {
                    vga_scroll();
                }
            }
            break;
            
        case '\b':
            if (vga_column > 0) {
                vga_column--;
                vga_buffer[vga_row * VGA_WIDTH + vga_column] = vga_entry(' ', vga_color);
            }
            break;
            
        default:
            vga_buffer[vga_row * VGA_WIDTH + vga_column] = vga_entry(c, vga_color);
            if (++vga_column >= VGA_WIDTH) {
                vga_column = 0;
                if (++vga_row >= VGA_HEIGHT) {
                    vga_scroll();
                }
            }
            break;
    }
}

void vga_write(const char* data, size_t size) {
    for (size_t i = 0; i < size; i++) {
        vga_putchar(data[i]);
    }
}

void vga_writestring(const char* data) {
    vga_write(data, strlen(data));
}
