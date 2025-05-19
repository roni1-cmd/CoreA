#define MAX_FILES 16
#define MAX_NAME 32
#define MAX_MOUNTS 4

struct vfs_file {
    char name[MAX_NAME];
    void *fs_data;
    int (*read)(void *fs_data, char *buf, int size);
    int (*write)(void *fs_data, const char *buf, int size);
    int used;
};

struct vfs_mount {
    char path[MAX_NAME];
    int (*create)(const char *name);
    int (*read)(const char *name, char *buf, int size);
    int (*write)(const char *name, const char *buf, int size);
    void (*list)(char *buf, int size);
    int used;
};

struct vfs_mount mounts[MAX_MOUNTS];
extern void *kmalloc(unsigned int size);
extern void kfree(void *ptr);
extern int ramfs_create(const char *name);
extern int ramfs_read(const char *name, char *buf, int size);
extern int ramfs_write(const char *name, const char *buf, int size);
extern void ramfs_list(char *buf, int size);
extern int fs_create(const char *name);
extern int fs_read(const char *name, char *buf, int size);
extern int fs_write(const char *name, const char *buf, int size);
extern void fs_list(char *buf, int size);

void init_vfs(void) {
    for (int i = 0; i < MAX_MOUNTS; i++)
        mounts[i].used = 0;
    mounts[0].path[0] = '/';
    mounts[0].path[1] = 'r';
    mounts[0].path[2] = 'a';
    mounts[0].path[3] = 'm';
    mounts[0].path[4] = 0;
    mounts[0].create = ramfs_create;
    mounts[0].read = ramfs_read;
    mounts[0].write = ramfs_write;
    mounts[0].list = ramfs_list;
    mounts[0].used = 1;
    mounts[1].path[0] = '/';
    mounts[1].path[1] = 'd';
    mounts[1].path[2] = 'i';
    mounts[1].path[3] = 's';
    mounts[1].path[4] = 'k';
    mounts[1].path[5] = 0;
    mounts[1].create = fs_create;
    mounts[1].read = fs_read;
    mounts[1].write = fs_write;
    mounts[1].list = fs_list;
    mounts[1].used = 1;
}

int vfs_create(const char *path) {
    if (!path || !path[0]) return -1;
    for (int i = 0; i < MAX_MOUNTS; i++) {
        if (mounts[i].used) {
            int j;
            for (j = 0; mounts[i].path[j] && path[j] && mounts[i].path[j] == path[j]; j++);
            if (mounts[i].path[j] == 0 && (path[j] == '/' || path[j] == 0)) {
                return mounts[i].create(path + j + (path[j] == '/' ? 1 : 0));
            }
        }
    }
    return -1;
}

int vfs_read(const char *path, char *buf, int size) {
    if (!path || !buf || size <= 0) return -1;
    for (int i = 0; i < MAX_MOUNTS; i++) {
        if (mounts[i].used) {
            int j;
            for (j = 0; mounts[i].path[j] && path[j] && mounts[i].path[j] == path[j]; j++);
            if (mounts[i].path[j] == 0 && (path[j] == '/' || path[j] == 0)) {
                return mounts[i].read(path + j + (path[j] == '/' ? 1 : 0), buf, size);
            }
        }
    }
    return -1;
}

int vfs_write(const char *path, const char *buf, int size) {
    if (!path || !buf || size <= 0) return -1;
    for (int i = 0; i < MAX_MOUNTS; i++) {
        if (mounts[i].used) {
            int j;
            for (j = 0; mounts[i].path[j] && path[j] && mounts[i].path[j] == path[j]; j++);
            if (mounts[i].path[j] == 0 && (path[j] == '/' || path[j] == 0)) {
                return mounts[i].write(path + j + (path[j] == '/' ? 1 : 0), buf, size);
            }
        }
    }
    return -1;
}

void vfs_list(const char *path, char *buf, int size) {
    if (!path || !buf || size <= 0) {
        buf[0] = 0;
        return;
    }
    for (int i = 0; i < MAX_MOUNTS; i++) {
        if (mounts[i].used) {
            int j;
            for (j = 0; mounts[i].path[j] && path[j] && mounts[i].path[j] == path[j]; j++);
            if (mounts[i].path[j] == 0 && (path[j] == '/' || path[j] == 0)) {
                mounts[i].list(buf, size);
                return;
            }
        }
    }
    buf[0] = 0;
}
