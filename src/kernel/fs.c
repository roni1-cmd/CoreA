#define MAX_FILES 16
#define MAX_NAME 32
#define MAX_CONTENT 256
#define FS_SECTOR 1
#define SECTOR_SIZE 512

struct file {
    char name[MAX_NAME];
    unsigned int sector;
    unsigned int size;
    int used;
};

struct file fs[MAX_FILES];
unsigned char sector_buffer[SECTOR_SIZE];
extern void *kmalloc(unsigned int size);
extern void kfree(void *ptr);
extern void disk_read(unsigned int sector, unsigned char *buffer);
extern void disk_write(unsigned int sector, unsigned char *buffer);

void init_fs(void) {
    disk_read(FS_SECTOR, sector_buffer);
    for (int i = 0; i < MAX_FILES; i++) {
        fs[i].used = sector_buffer[i * 32];
        for (int j = 0; j < MAX_NAME; j++)
            fs[i].name[j] = sector_buffer[i * 32 + 1 + j];
        fs[i].sector = FS_SECTOR + 1 + i;
        fs[i].size = sector_buffer[i * 32 + 29] | (sector_buffer[i * 32 + 30] << 8);
        if (fs[i].used && (fs[i].size > MAX_CONTENT || fs[i].sector > 2880))
            fs[i].used = 0;
    }
}

int fs_create(const char *name) {
    if (!name || !name[0]) return -1;
    for (int i = 0; i < MAX_FILES; i++) {
        if (!fs[i].used) {
            int len = 0;
            for (; len < MAX_NAME - 1 && name[len]; len++)
                fs[i].name[len] = name[len];
            fs[i].name[len] = 0;
            fs[i].used = 1;
            fs[i].sector = FS_SECTOR + 1 + i;
            fs[i].size = 0;
            sector_buffer[i * 32] = 1;
            for (int j = 0; j < MAX_NAME; j++)
                sector_buffer[i * 32 + 1 + j] = fs[i].name[j];
            sector_buffer[i * 32 + 29] = 0;
            sector_buffer[i * 32 + 30] = 0;
            disk_write(FS_SECTOR, sector_buffer);
            return 0;
        }
    }
    return -1;
}

int fs_read(const char *name, char *buf, int size) {
    if (!name || !buf || size <= 0) return -1;
    for (int i = 0; i < MAX_FILES; i++) {
        if (fs[i].used) {
            int j;
            for (j = 0; fs[i].name[j] && name[j] && fs[i].name[j] == name[j]; j++);
            if (fs[i].name[j] == 0 && name[j] == 0) {
                disk_read(fs[i].sector, sector_buffer);
                int len = fs[i].size < size - 1 ? fs[i].size : size - 1;
                for (j = 0; j < len && sector_buffer[j]; j++)
                    buf[j] = sector_buffer[j];
                buf[j] = 0;
                return j;
            }
        }
    }
    return -1;
}

int fs_write(const char *name, const char *buf, int size) {
    if (!name || !buf || size <= 0 || size > MAX_CONTENT) return -1;
    for (int i = 0; i < MAX_FILES; i++) {
        if (fs[i].used) {
            int j;
            for (j = 0; fs[i].name[j] && name[j] && fs[i].name[j] == name[j]; j++);
            if (fs[i].name[j] == 0 && name[j] == 0) {
                for (j = 0; j < size - 1 && buf[j]; j++)
                    sector_buffer[j] = buf[j];
                sector_buffer[j] = 0;
                fs[i].size = j;
                sector_buffer[i * 32 + 29] = j & 0xff;
                sector_buffer[i * 32 + 30] = (j >> 8) & 0xff;
                disk_write(fs[i].sector, sector_buffer);
                disk_write(FS_SECTOR, sector_buffer);
                return j;
            }
        }
    }
    return -1;
}

void fs_list(char *buf, int size) {
    if (!buf || size <= 0) {
        buf[0] = 0;
        return;
    }
    int pos = 0;
    for (int i = 0; i < MAX_FILES && pos < size - 1; i++) {
        if (fs[i].used) {
            for (int j = 0; fs[i].name[j] && pos < size - 1; j++)
                buf[pos++] = fs[i].name[j];
            if (pos < size - 1)
                buf[pos++] = '\n';
        }
    }
    buf[pos] = 0;
}
