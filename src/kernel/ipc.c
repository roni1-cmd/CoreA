#define MAX_MESSAGES 32
#define MAX_MSG_SIZE 64
#define MAX_TASKS 8

struct message {
    int sender;
    int receiver;
    char data[MAX_MSG_SIZE];
    int size;
    int used;
};

struct message msg_queue[MAX_MESSAGES];
extern void *kmalloc(unsigned int size);
extern void kfree(void *ptr);

void init_ipc(void) {
    for (int i = 0; i < MAX_MESSAGES; i++)
        msg_queue[i].used = 0;
}

int ipc_send(int receiver, const char *data, int size) {
    if (receiver < 0 || receiver >= MAX_TASKS || size <= 0 || size > MAX_MSG_SIZE || !data)
        return -1;
    for (int i = 0; i < MAX_MESSAGES; i++) {
        if (!msg_queue[i].used) {
            msg_queue[i].sender = current_task;
            msg_queue[i].receiver = receiver;
            for (int j = 0; j < size && j < MAX_MSG_SIZE - 1; j++)
                msg_queue[i].data[j] = data[j];
            msg_queue[i].data[size < MAX_MSG_SIZE ? size : MAX_MSG_SIZE - 1] = 0;
            msg_queue[i].size = size < MAX_MSG_SIZE ? size : MAX_MSG_SIZE - 1;
            msg_queue[i].used = 1;
            return 0;
        }
    }
    return -1;
}

int ipc_receive(int *sender, char *buf, int size) {
    if (!sender || !buf || size <= 0)
        return -1;
    for (int i = 0; i < MAX_MESSAGES; i++) {
        if (msg_queue[i].used && msg_queue[i].receiver == current_task) {
            *sender = msg_queue[i].sender;
            int len = msg_queue[i].size < size - 1 ? msg_queue[i].size : size - 1;
            for (int j = 0; j < len; j++)
                buf[j] = msg_queue[i].data[j];
            buf[len] = 0;
            msg_queue[i].used = 0;
            return len;
        }
    }
    return -1;
}
