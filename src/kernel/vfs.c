#define MAX_FILES 16
#define MAX_NAME 32
#define MAX_CONTENT 256
#define MAX_BLOCKS 64

struct file {
    char name[MAX_NAME];
    char data[MAX_CONTENT];
    int size;
    int active;
    int is_compressed;
};

struct ramfs {
    struct file files[MAX_FILES];
    int block_map[MAX_BLOCKS];
};

struct ramfs ramfs;

void init_vfs(void) {
    for (int i = 0; i < MAX_FILES; i++)
        ramfs.files[i].active = ramfs.files[i].is_compressed = 0;
    for (int i = 0; i < MAX_BLOCKS; i++)
        ramfs.block_map[i] = 0;
}

int vfs_create(const char *name) {
    for (int i = 0; i < MAX_FILES; i++) {
        if (!ramfs.files[i].active) {
            for (int j = 0; j < MAX_NAME - 1 && name[j]; j++)
                ramfs.files[i].name[j] = name[j];
            ramfs.files[i].name[MAX_NAME - 1] = 0;
            ramfs.files[i].size = 0;
            ramfs.files[i].active = 1;
            return 0;
        }
    }
    return -1;
}

int vfs_read(const char *name, char *buf, int size) {
    for (int i = 0; i < MAX_FILES; i++) {
        if (ramfs.files[i].active) {
            int match = 1;
            for (int j = 0; j < MAX_NAME && name[j]; j++) {
                if (ramfs.files[i].name[j] != name[j]) {
                    match = 0;
                    break;
                }
            }
            if (match) {
                int len = ramfs.files[i].size < size ? ramfs.files[i].size : size;
                for (int j = 0; j < len; j++)
                    buf[j] = ramfs.files[i].data[j];
                if (ramfs.files[i].is_compressed) {
                    char decompressed[256];
                    len = vfs_decompress_buffer(buf, len, decompressed, 256);
                    for (int j = 0; j < len; j++)
                        buf[j] = decompressed[j];
                }
                return len;
            }
        }
    }
    return -1;
}

int vfs_write(const char *name, const char *buf, int size) {
    for (int i = 0; i < MAX_FILES; i++) {
        if (ramfs.files[i].active) {
            int match = 1;
            for (int j = 0; j < MAX_NAME && name[j]; j++) {
                if (ramfs.files[i].name[j] != name[j]) {
                    match = 0;
                    break;
                }
            }
            if (match) {
                int len = size < MAX_CONTENT ? size : MAX_CONTENT;
                for (int j = 0; j < len; j++)
                    ramfs.files[i].data[j] = buf[j];
                ramfs.files[i].size = len;
                ramfs.files[i].is_compressed = (name[0] == 'c');
                return len;
            }
        }
    }
    return vfs_create(name) == 0 ? vfs_write(name, buf, size) : -1;
}

void vfs_list(const char *path, char *buf, int size) {
    int pos = 0;
    for (int i = 0; i < MAX_FILES && pos < size - 1; i++) {
        if (ramfs.files[i].active) {
            for (int j = 0; j < MAX_NAME && ramfs.files[i].name[j] && pos < size - 1; j++)
                buf[pos++] = ramfs.files[i].name[j];
            buf[pos++] = '\n';
        }
    }
    buf[pos] = 0;
}

int vfs_delete(const char *name) {
    for (int i = 0; i < MAX_FILES; i++) {
        if (ramfs.files[i].active) {
            int match = 1;
            for (int j = 0; j < MAX_NAME && name[j]; j++) {
                if (ramfs.files[i].name[j] != name[j]) {
                    match = 0;
                    break;
                }
            }
            if (match) {
                ramfs.files[i].active = 0;
                return 0;
            }
        }
    }
    return -1;
}

// Run-length encoding compression
int vfs_compress_buffer(char *buf, int len, char *out, int max_out) {
    if (len <= 0 || max_out < len) return -1;
    int out_pos = 0;
    for (int i = 0; i < len && out_pos < max_out - 2; ) {
        int count = 1;
        while (i + count < len && buf[i] == buf[i + count] && count < 255)
            count++;
        out[out_pos++] = buf[i];
        out[out_pos++] = count;
        i += count;
    }
    return out_pos;
}

// Run-length encoding decompression
int vfs_decompress_buffer(char *buf, int len, char *out, int max_out) {
    if (len <= 0 || len % 2 != 0) return -1;
    int out_pos = 0;
    for (int i = 0; i < len - 1 && out_pos < max_out; i += 2) {
        char c = buf[i];
        int count = (unsigned char)buf[i + 1];
        for (int j = 0; j < count && out_pos < max_out; j++)
            out[out_pos++] = c;
    }
    return out_pos;
}
