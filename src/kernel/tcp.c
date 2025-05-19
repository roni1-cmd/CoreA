#define MAX_CONNECTIONS 4
#define TCP_MSS 512
#define TCP_WINDOW 4096

struct tcp_header {
    unsigned short src_port;
    unsigned short dst_port;
    unsigned int seq_num;
    unsigned int ack_num;
    unsigned char data_offset;
    unsigned char flags;
    unsigned short window;
    unsigned short checksum;
    unsigned short urgent_ptr;
};

struct tcp_connection {
    int state; // 0: closed, 1: syn_sent, 2: established
    unsigned short local_port;
    unsigned short remote_port;
    unsigned int local_seq;
    unsigned int remote_seq;
    unsigned char *buffer;
    int buffer_len;
    int used;
};

struct tcp_connection connections[MAX_CONNECTIONS];
extern void *slab_alloc(int size);
extern void slab_free(void *ptr);
extern int net_send_packet(unsigned char *data, int len);
extern int net_receive_packet(unsigned char *buf, int size);

void init_tcp(void) {
    for (int i = 0; i < MAX_CONNECTIONS; i++)
        connections[i].used = 0;
}

int tcp_connect(unsigned short local_port, unsigned short remote_port) {
    for (int i = 0; i < MAX_CONNECTIONS; i++) {
        if (!connections[i].used) {
            connections[i].local_port = local_port;
            connections[i].remote_port = remote_port;
            connections[i].local_seq = 1000;
            connections[i].remote_seq = 0;
            connections[i].state = 1;
            connections[i].buffer = slab_alloc(TCP_WINDOW);
            if (!connections[i].buffer) return -1;
            connections[i].buffer_len = 0;
            connections[i].used = 1;
            struct tcp_header hdr = {local_port, remote_port, 1000, 0, 5 << 4, 0x02, TCP_WINDOW, 0, 0};
            net_send_packet((unsigned char *)&hdr, sizeof(hdr));
            return i;
        }
    }
    return -1;
}

int tcp_send(int conn_id, const unsigned char *data, int len) {
    if (conn_id < 0 || conn_id >= MAX_CONNECTIONS || !connections[conn_id].used || len <= 0 || len > TCP_MSS)
        return -1;
    struct tcp_connection *conn = &connections[conn_id];
    if (conn->state != 2) return -1;
    unsigned char packet[sizeof(struct tcp_header) + TCP_MSS];
    struct tcp_header *hdr = (struct tcp_header *)packet;
    hdr->src_port = conn->local_port;
    hdr->dst_port = conn->remote_port;
    hdr->seq_num = conn->local_seq;
    hdr->ack_num = conn->remote_seq;
    hdr->data_offset = 5 << 4;
    hdr->flags = 0x18;
    hdr->window = TCP_WINDOW - conn->buffer_len;
    for (int i = 0; i < len; i++)
        packet[sizeof(struct tcp_header) + i] = data[i];
    conn->local_seq += len;
    return net_send_packet(packet, sizeof(struct tcp_header) + len);
}

int tcp_receive(int conn_id, unsigned char *buf, int size) {
    if (conn_id < 0 || conn_id >= MAX_CONNECTIONS || !connections[conn_id].used || !buf || size <= 0)
        return -1;
    struct tcp_connection *conn = &connections[conn_id];
    if (conn->buffer_len == 0) return 0;
    int len = conn->buffer_len < size ? conn->buffer_len : size;
    for (int i = 0; i < len; i++)
        buf[i] = conn->buffer[i];
    for (int i = len; i < conn->buffer_len; i++)
        conn->buffer[i - len] = conn->buffer[i];
    conn->buffer_len -= len;
    return len;
}
