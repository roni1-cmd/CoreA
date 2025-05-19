#define MAX_PEERS 8
#define MAX_NAME 32
#define MAX_FILES 16

struct peer_entry {
    unsigned int ip;
    unsigned short port;
    char name[MAX_NAME];
    int active;
};

extern int sys_gui_create_window(int x, int y, int w, int h, unsigned char color);
extern int sys_gui_create_button(int win_id, int x, int y, int w, int h, const char *label);
extern int sys_gui_get_event(int *type, int *x, *y, unsigned char *data);
extern int sys_p2p_list_peers(struct peer_entry *peers, int max_peers);
extern int sys_p2p_list_files(unsigned int ip, unsigned short port, char *buf, int size);
extern int sys_p2p_download_file(unsigned int ip, unsigned short port, const char *filename, char *buf, int size);
extern void sys_file_explorer_browse_p2p(unsigned int ip, unsigned short port);

struct peer_entry peers[MAX_PEERS];
int win_id = -1;
int connect_buttons[MAX_PEERS];
int selected_peer_idx = -1;

void netbrowser_init(void) {
    for (int i = 0; i < MAX_PEERS; i++)
        peers[i].active = 0;
    win_id = sys_gui_create_window(70, 70, 200, 100, 7); // Light gray
    if (win_id < 0) return;

    // Get peer list
    sys_p2p_list_peers(peers, MAX_PEERS);
    int y_offset = 10;
    for (int i = 0; i < MAX_PEERS; i++) {
        if (peers[i].active) {
            char label[32] = {0};
            for (int j = 0; j < MAX_NAME && peers[i].name[j]; j++)
                label[j] = peers[i].name[j];
            sys_gui_create_button(win_id, 10, y_offset, 80, 15, label);
            connect_buttons[i] = sys_gui_create_button(win_id, 100, y_offset, 50, 15, "Connect");
            y_offset += 20;
        }
    }
}

void netbrowser_run(void) {
    if (win_id < 0) return;
    int event_type, event_x, event_y;
    unsigned char event_data[32];

    while (1) {
        if (sys_gui_get_event(&event_type, &event_x, &event_y, event_data) > 0) {
            if (event_type == 3) { // Button click
                int button_id = event_data[0];
                for (int i = 0; i < MAX_PEERS; i++) {
                    if (peers[i].active && button_id == connect_buttons[i]) {
                        selected_peer_idx = i;
                        sys_file_explorer_browse_p2p(peers[i].ip, peers[i].port);
                        return; // Switch to file explorer
                    }
                }
            }
        }
        asm("hlt");
    }
}
