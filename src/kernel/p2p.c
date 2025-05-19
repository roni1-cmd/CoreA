#define MAX_PEERS 8
#define MAX_FILES 16
#define MAX_NAME 32

struct peer {
    unsigned int ip;
    unsigned short port;
    int active;
};

struct peer peers[MAX_PEERS];
extern void *slab_alloc(int size);
extern void slab_free(void *ptr);
extern int tcp_connect(unsigned short local_port, unsigned short remote_port);
extern int tcp_send(int conn_id, const unsigned char *data, int len);
extern int tcp_receive(int conn_id, unsigned char *buf, int size);
extern int vfs_read(const char *name, char *buf, int size);

void init_p2p(void) {
    for (int i = 0; i < MAX_PEERS; i++)
        peers[i].active = 0;
}

int p2p_add_peer(unsigned int ip, unsigned short port) {
    for (int i = 0; i < MAX_PEERS; i++) {
        if (!peers[i].active) {
            peers[i].ip = ip;
            peers[i].port = port;
            peers[i].active = 1;
            return i;
        }
    }
    return -1;
}

int p2p_share_file(const char *filename, unsigned int target_ip, unsigned short target_port) {
    if (!filename || !target_ip || !target_port) return -1;
    char buf[256];
    int len = vfs_read(filename, buf, 256);
    if (len <= 0) return -1;
    int conn_id = tcp_connect(1000, target_port);
    if (conn_id < 0) return -1;
    unsigned char packet[256 + 32];
    packet[0] = 'F';
    packet[1] = 'I';
    packet[2] = 'L';
    packet[3] = 'E';
    for (int i = 0; i < MAX_NAME && filename[i]; i++)
        packet[4 + i] = filename[i];
    packet[4 + MAX_NAME] = 0;
    for (int i = 0; i < len; i++)
        packet[4 + MAX_NAME + 1 + i] = buf[i];
    return tcp_send(conn_id, packet, 4 + MAX_NAME + 1 + len);
}
