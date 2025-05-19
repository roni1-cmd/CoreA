#define ACPI_PORT 0x4000
#define PM1_CNT 0x04
#define SLP_TYPa 0x1000
#define SLP_EN 0x2000

extern void outw(unsigned short port, unsigned short val);
extern unsigned short inw(unsigned short port);

void init_acpi(void) {
    outw(ACPI_PORT + PM1_CNT, 0);
}

void acpi_shutdown(void) {
    unsigned short val = inw(ACPI_PORT + PM1_CNT);
    val |= SLP_TYPa | SLP_EN;
    outw(ACPI_PORT + PM1_CNT, val);
    asm("hlt");
}

void acpi_reboot(void) {
    outb(0x64, 0xfe);
    asm("hlt");
}
