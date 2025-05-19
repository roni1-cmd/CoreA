#define MAX_MESSAGE 64
#define MAX_NAME 32

extern int sys_gui_create_window(int x, int y, int w, int h, unsigned char color);
extern int sys_gui_create_button(int win_id, int x, int y, int w, int h, const char *label);
extern int sys_gui_create_text_field(int win_id, int x, int y, int w, int h);
extern int sys_gui_get_event(int *type, int *x, int *y, unsigned char *data);
extern int sys_p2p_send_message(unsigned int ip, unsigned short port, const char *message, int len);
extern int sys_p2p_receive_message(unsigned int ip, unsigned short port, char *buf, int size);
extern void sys_logger_add_event(const char *message);

int win_id = -1;
int input_field_id = -1;
int output_field_id = -1;
int send_button_id = -1;
unsigned int peer_ip = 192168001;
unsigned short peer_port = 1000;

void chat_init(void) {
    win_id = sys_gui_create_window(110, 110, 200, 140, 7); // Light gray
    if (win_id < 0) return;

    output_field_id = sys_gui_create_text_field(win_id, 10, 10, 180, 60);
    input_field_id = sys_gui_create_text_field(win_id, 10, 80, 140, 20);
    send_button_id = sys_gui_create_button(win_id, 160, 80, 30, 20, "Send");

    sys_logger_add_event("Chat client launched");
}

void chat_run(void) {
    if (win_id < 0) return;
    int event_type, event_x, event_y;
    unsigned char event_data[256];
    char received[MAX_MESSAGE] = {0};

    while (1) {
        // Check for incoming messages
        if (sys_p2p_receive_message(peer_ip, peer_port, received, MAX_MESSAGE) > 0) {
            sys_gui_create_text_field(win_id, 10, 10, 180, 60);
            sys_logger_add_event("Message received");
        }

        if (sys_gui_get_event(&event_type, &event_x, &event_y, event_data) > 0) {
            if (event_type == 3 && event_data[0] == send_button_id) {
                char message[MAX_MESSAGE] = {0};
                for (int i = 0; i < MAX_MESSAGE && event_data[i + 1]; i++)
                    message[i] = event_data[i + 1];
                if (sys_p2p_send_message(peer_ip, peer_port, message, MAX_MESSAGE) >= 0) {
                    sys_gui_create_text_field(win_id, 10, 10, 180, 60);
                    sys_logger_add_event("Message sent");
                }
                sys_gui_create_text_field(win_id, 10, 80, 140, 20); // Clear input
            }
        }
        asm("hlt");
    }
}
