#define RTC_INDEX 0x70
#define RTC_DATA 0x71

extern void outb(unsigned short port, unsigned char val);
extern unsigned char inb(unsigned short port);

void init_rtc(void) {
    outb(RTC_INDEX, 0x0b);
    unsigned char status = inb(RTC_DATA);
    status |= 0x02;
    outb(RTC_INDEX, 0x0b);
    outb(RTC_DATA, status);
}

void rtc_get_time(unsigned char *buf) {
    outb(RTC_INDEX, 0x00);
    unsigned char sec = inb(RTC_DATA);
    outb(RTC_INDEX, 0x02);
    unsigned char min = inb(RTC_DATA);
    outb(RTC_INDEX, 0x04);
    unsigned char hour = inb(RTC_DATA);
    outb(RTC_INDEX, 0x07);
    unsigned char day = inb(RTC_DATA);
    outb(RTC_INDEX, 0x08);
    unsigned char month = inb(RTC_DATA);
    outb(RTC_INDEX, 0x09);
    unsigned char year = inb(RTC_DATA);
    buf[0] = '2';
    buf[1] = '0';
    buf[2] = (year >> 4) + '0';
    buf[3] = (year & 0xf) + '0';
    buf[4] = '-';
    buf[5] = (month >> 4) + '0';
    buf[6] = (month & 0xf) + '0';
    buf[7] = '-';
    buf[8] = (day >> 4) + '0';
    buf[9] = (day & 0xf) + '0';
    buf[10] = ' ';
    buf[11] = (hour >> 4) + '0';
    buf[12] = (hour & 0xf) + '0';
    buf[13] = ':';
    buf[14] = (min >> 4) + '0';
    buf[15] = (min & 0xf) + '0';
    buf[16] = ':';
    buf[17] = (sec >> 4) + '0';
    buf[18] = (sec & 0xf) + '0';
    buf[19] = 0;
}
