/*
 * CoreA OS VGA Driver Header
 * VGA text mode definitions and functions
 */

#ifndef _DRIVERS_VGA_H_
#define _DRIVERS_VGA_H_

#include <stdint.h>
#include <stddef.h>

// VGA text mode dimensions
#define VGA_WIDTH   80
#define VGA_HEIGHT  25

// VGA color constants
enum vga_color {
    VGA_COLOR_BLACK = 0,
    VGA_COLOR_BLUE = 1,
    VGA_COLOR_GREEN = 2,
    VGA_COLOR_CYAN = 3,
    VGA_COLOR_RED = 4,
    VGA_COLOR_MAGENTA = 5,
    VGA_COLOR_BROWN = 6,
    VGA_COLOR_LIGHT_GREY = 7,
    VGA_COLOR_DARK_GREY = 8,
    VGA_COLOR_LIGHT_BLUE = 9,
    VGA_COLOR_LIGHT_GREEN = 10,
    VGA_COLOR_LIGHT_CYAN = 11,
    VGA_COLOR_LIGHT_RED = 12,
    VGA_COLOR_LIGHT_MAGENTA = 13,
    VGA_COLOR_LIGHT_BROWN = 14,
    VGA_COLOR_WHITE = 15,
};

// VGA driver functions
void vga_init(void);
void vga_set_color(uint8_t fg, uint8_t bg);
void vga_clear(void);
void vga_set_cursor(size_t x, size_t y);
void vga_putchar(char c);
void vga_write(const char* data, size_t size);
void vga_writestring(const char* data);

#endif /* _DRIVERS_VGA_H_ */
