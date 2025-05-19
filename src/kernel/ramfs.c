#define MAX_FILES 16
#define MAX_NAME 32
#define MAX_CONTENT 256

struct file {
    char name[MAX_NAME];
    char content[MAX_CONTENT];
    int used;
};

struct file fs[MAX_FILES];
extern void *kmalloc(unsigned int size);
extern void kfree(void *ptr);

void init_ramfs(void) {
    for (int i = 0; i < MAX_FILES; i++)
        fs[i].used = 0;
}

int ramfs_create(const char *name) {
    for (int i = 0; i < MAX_FILES; i++) {
        if (!fs[i].used) {
            for (int j = 0; j < MAX_NAME - 1 && name[j]; j++)
                fs[i].name[j] = name[j];
            fs[i].name[MAX_NAME - 1] = 0;
            fs[i].content[0] = 0;
            fs[i].used = 1;
            return 0;
        }
    }
    return -1;
}

int ramfs_read(const char *name, char *buf, int size) {
    for (int i = 0; i < MAX_FILES; i++) {
        if (fs[i].used) {
            int j;
            for (j = 0; fs[i].name[j] && name[j] && fs[i].name[j] == name[j]; j++);
            if (fs[i].name[j] == 0 && name[j] == 0) {
                for (j = 0; j < size - 1 && fs[i].content[j]; j++)
                    buf[j] = fs[i].content[j];
                buf[j] = 0;
                return j;
            }
        }
    }
    return -1;
}

int ramfs_write(const char *name, const char *buf, int size) {
    for (int i = 0; i < MAX_FILES; i++) {
        if (fs[i].used) {
            int j;
            for (j = 0; fs[i].name[j] && name[j] && fs[i].name[j] == name[j]; j++);
            if (fs[i].name[j] == 0 && name[j] == 0) {
                for (j = 0; j < size - 1 && buf[j]; j++)
                    fs[i].content[j] = buf[j];
                fs[i].content[j] = 0;
                return j;
            }
        }
    }
    return -1;
}

void ramfs_list(char *buf, int size) {
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
