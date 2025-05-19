#define PIT_CHANNEL2 0x42
#define PIT_CMD 0x43
#define SPEAKER_PORT 0x61

extern void outb(unsigned short port, unsigned char val);
extern unsigned char inb(unsigned short port);

void init_audio(void) {
    outb(SPEAKER_PORT, inb(SPEAKER_PORT) & 0xfc);
}

void play_tone(unsigned int freq, unsigned int duration) {
    if (freq < 20 || freq > 20000) return;
    unsigned int div = 1193182 / freq;
    outb(PIT_CMD, 0xb6);
    outb(PIT_CHANNEL2, div & 0xff);
    outb(PIT_CHANNEL2, (div >> 8) & 0xff);
    outb(SPEAKER_PORT, inb(SPEAKER_PORT) | 0x03);
    for (volatile int i = 0; i < duration * 1000; i++);
    outb(SPEAKER_PORT, inb(SPEAKER_PORT) & 0xfc);
}
