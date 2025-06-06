#define VGA_WIDTH 80
#define VGA_HEIGHT 25
#define VGA_BUFFER 0xB8000
extern char kbd_buffer[256];
extern int kbd_pos;
extern volatile unsigned int ticks;

void outb(unsigned short port, unsigned char val) {
    asm volatile("outb %0, %1" : : "a"(val), "Nd"(port));
}

void vga_clear(void) {
    unsigned short *vga = (unsigned short *)VGA_BUFFER;
    for (int i = 0; i < VGA_WIDTH * VGA_HEIGHT; i++)
        vga[i] = 0x0700;
}

void vga_print(const char *str, int row, int col) {
    unsigned short *vga = (unsigned short *)VGA_BUFFER;
    int pos = row * VGA_WIDTH + col;
    for (; *str; str++, pos++)
        vga[pos] = (0x07 << 8) | *str;
}

void vga_scroll(void) {
    unsigned short *vga = (unsigned short *)VGA_BUFFER;
    for (int i = VGA_WIDTH; i < VGA_WIDTH * VGA_HEIGHT; i++)
        vga[i - VGA_WIDTH] = vga[i];
    for (int i = 0; i < VGA_WIDTH; i++)
        vga[(VGA_HEIGHT - 1) * VGA_WIDTH + i] = 0x0700;
}

int sys_create(const char *name) {
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(1), "b"(name));
    return ret;
}

int sys_read(const char *name, char *buf, int size) {
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(2), "b"(name), "c"(buf), "d"(size));
    return ret;
}

int sys_write(const char *name, const char *buf, int size) {
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(3), "b"(name), "c"(buf), "d"(size));
    return ret;
}

void sys_list(const char *path, char *buf, int size) {
    asm volatile("int $0x80" :: "a"(4), "b"(path), "c"(buf), "d"(size));
}

int sys_net_send(unsigned char *data, int len) {
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(7), "b"(data), "c"(len));
    return ret;
}

int sys_net_receive(unsigned char *buf, int size) {
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(8), "b"(buf), "c"(size));
    return ret;
}

void sys_get_time(unsigned char *buf) {
    asm volatile("int $0x80" :: "a"(9), "b"(buf));
}

int sys_load_program(const char *name) {
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(10), "b"(name));
    return ret;
}

int sys_mutex_lock(int id) {
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(11), "b"(id));
    return ret;
}

void sys_mutex_unlock(int id) {
    asm volatile("int $0x80" :: "a"(12), "b"(id));
}

int sys_ipc_send(int receiver, const char *data, int size) {
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(13), "b"(receiver), "c"(data), "d"(size));
    return ret;
}

int sys_ipc_receive(int *sender, char *buf, int size) {
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(14), "b"(sender), "c"(buf), "d"(size));
    return ret;
}

void sys_shutdown(void) {
    asm volatile("int $0x80" :: "a"(15));
}

void sys_reboot(void) {
    asm volatile("int $0x80" :: "a"(16));
}

void sys_play_tone(unsigned int freq, unsigned int duration) {
    asm volatile("int $0x80" :: "a"(17), "b"(freq), "c"(duration));
}

void sys_profiler_stats(char *buf, int size) {
    asm volatile("int $0x80" :: "a"(18), "b"(buf), "c"(size));
}

int sys_gui_create_window(int x, int y, int w, int h, unsigned char color) {
    int params[5] = {x, y, w, h, color};
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(19), "b"(params));
    return ret;
}

int sys_gui_create_button(int win_id, int x, int y, int w, int h, const char *label) {
    int params[6] = {win_id, x, y, w, h, (int)label};
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(20), "b"(params));
    return ret;
}

int sys_gui_get_event(int *type, int *x, int *y, unsigned char *data) {
    int params[4] = {(int)type, (int)x, (int)y, (int)data};
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(21), "b"(params));
    return ret;
}

int sys_tcp_connect(unsigned short local_port, unsigned short remote_port) {
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(22), "b"(local_port), "c"(remote_port));
    return ret;
}

int sys_tcp_send(int conn_id, const unsigned char *data, int len) {
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(23), "b"(conn_id), "c"(data), "d"(len));
    return ret;
}

int sys_tcp_receive(int conn_id, unsigned char *buf, int size) {
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(24), "b"(conn_id), "c"(buf), "d"(size));
    return ret;
}

int sys_p2p_add_peer(unsigned int ip, unsigned short port) {
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(25), "b"(ip), "c"(port));
    return ret;
}

int sys_p2p_share_file(const char *filename, unsigned int ip, unsigned short port) {
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(26), "b"(filename), "c"(ip), "d"(port));
    return ret;
}

int sys_gui_create_text_field(int win_id, int x, int y, int w, int h) {
    int params[5] = {win_id, x, y, w, h};
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(27), "b"(params));
    return ret;
}

int sys_gui_create_context_menu(int win_id, int x, int y, const char *items) {
    int params[4] = {win_id, x, y, (int)items};
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(28), "b"(params));
    return ret;
}

int sys_vfs_delete(const char *name) {
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(29), "b"(name));
    return ret;
}

int sys_p2p_list_files(unsigned int ip, unsigned short port, char *buf, int size) {
    int params[4] = {ip, port, (int)buf, size};
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(30), "b"(params));
    return ret;
}

int sys_p2p_download_file(unsigned int ip, unsigned short port, const char *filename, char *buf, int size) {
    int params[5] = {ip, port, (int)filename, (int)buf, size};
    int ret;
    asm volatile("int $0x80" : "=a"(ret) : "a"(31), "b"(params));
    return ret;
}

void execute_command(char *input, int *row) {
    if (input[0] == 'c' && input[1] == 'l' && input[2] == 'e' && input[3] == 'a' && input[4] == 'r' && input[5] == 0) {
        vga_clear();
        *row = 1;
    } else if (input[0] == 'e' && input[1] == 'c' && input[2] == 'h' && input[3] == 'o' && input[4] == ' ') {
        vga_print(input + 5, *row, 0);
        (*row)++;
    } else if (input[0] == 'c' && input[1] == 'r' && input[2] == 'e' && input[3] == 'a' && input[4] == 't' && input[5] == 'e' && input[6] == ' ') {
        if (sys_create(input + 7) == 0)
            vga_print("File created", *row, 0);
        else
            vga_print("Create failed", *row, 0);
        (*row)++;
    } else if (input[0] == 'r' && input[1] == 'e' && input[2] == 'a' && input[3] == 'd' && input[4] == ' ') {
        char output[256] = {0};
        if (sys_read(input + 5, output, 256) > 0)
            vga_print(output, *row, 0);
        else
            vga_print("Read failed", *row, 0);
        (*row)++;
    } else if (input[0] == 'w' && input[1] == 'r' && input[2] == 'i' && input[3] == 't' && input[4] == 'e' && input[5] == ' ') {
        char *name = input + 6;
        char *content = 0;
        for (int i = 6; input[i]; i++) {
            if (input[i] == ' ') {
                input[i] = 0;
                content = input + i + 1;
                break;
            }
        }
        if (content && sys_write(name, content, 256) > 0)
            vga_print("Write successful", *row, 0);
        else
            vga_print("Write failed", *row, 0);
        (*row)++;
    } else if (input[0] == 'l' && input[1] == 's' && input[2] == ' ') {
        char output[256] = {0};
        sys_list(input + 3, output, 256);
        vga_print(output, *row, 0);
        (*row)++;
    } else if (input[0] == 'p' && input[1] == 'i' && input[2] == 'n' && input[3] == 'g' && input[4] == ' ') {
        unsigned char packet[64] = "PING";
        if (sys_net_send(packet, 64) == 0)
            vga_print("Ping sent", *row, 0);
        else
            vga_print("Ping failed", *row, 0);
        (*row)++;
    } else if (input[0] == 't' && input[1] == 'i' && input[2] == 'm' && input[3] == 'e' && input[4] == 0) {
        char output[32] = {0};
        sys_get_time(output);
        vga_print(output, *row, 0);
        (*row)++;
    } else if (input[0] == 'r' && input[1] == 'u' && input[2] == 'n' && input[3] == ' ') {
        if (sys_load_program(input + 4) == 0)
            vga_print("Program loaded", *row, 0);
        else
            vga_print("Load failed", *row, 0);
        (*row)++;
    } else if (input[0] == 'g' && input[1] == 'u' && input[2] == 'i' && input[3] == 0) {
        vga_print("Switching to GUI...", *row, 0);
        int win_id = sys_gui_create_window(50, 50, 200, 100, 1);
        if (win_id >= 0) {
            sys_gui_create_button(win_id, 10, 20, 50, 20, "OK");
            vga_print("GUI window created", *row, 0);
        } else {
            vga_print("GUI window creation failed", *row, 0);
        }
        (*row)++;
    } else if (input[0] == 's' && input[1] == 'h' && input[2] == 'u' && input[3] == 't' && input[4] == 'd' && input[5] == 'o' && input[6] == 'w' && input[7] == 'n' && input[8] == 0) {
        vga_print("Shutting down...", *row, 0);
        sys_shutdown();
    } else if (input[0] == 'r' && input[1] == 'e' && input[2] == 'b' && input[3] == 'o' && input[4] == 'o' && input[5] == 't' && input[6] == 0) {
        vga_print("Rebooting...", *row, 0);
        sys_reboot();
    } else if (input[0] == 't' && input[1] == 'o' && input[2] == 'n' && input[3] == 'e' && input[4] == ' ') {
        unsigned int freq = 0, duration = 0;
        for (int i = 5; input[i] && input[i] != ' '; i++)
            freq = freq * 10 + (input[i] - '0');
        for (int i = 5; input[i]; i++) {
            if (input[i] == ' ') {
                for (int j = i + 1; input[j]; j++)
                    duration = duration * 10 + (input[j] - '0');
                break;
            }
        }
        sys_play_tone(freq, duration);
        vga_print("Tone played", *row, 0);
        (*row)++;
    } else if (input[0] == 's' && input[1] == 'c' && input[2] == 'r' && input[3] == 'i' && input[4] == 'p' && input[5] == 't' && input[6] == ' ') {
        char script[256] = {0};
        if (sys_read(input + 7, script, 256) > 0) {
            char *cmd = script;
            for (int i = 0; script[i]; i++) {
                if (script[i] == '\n') {
                    script[i] = 0;
                    execute_command(cmd, row);
                    cmd = script + i + 1;
                }
            }
            if (cmd[0])
                execute_command(cmd, row);
        } else {
            vga_print("Script read failed", *row, 0);
            (*row)++;
        }
    } else if (input[0] == 'p' && input[1] == 'r' && input[2] == 'o' && input[3] == 'f' && input[4] == 'i' && input[5] == 'l' && input[6] == 'e' && input[7] == 0) {
        char output[256] = {0};
        sys_profiler_stats(output, 256);
        vga_print(output, *row, 0);
        (*row)++;
    } else if (input[0] == 't' && input[1] == 'c' && input[2] == 'p' && input[3] == ' ') {
        unsigned short local_port = 0, remote_port = 0;
        for (int i = 4; input[i] && input[i] != ' '; i++)
            local_port = local_port * 10 + (input[i] - '0');
        for (int i = 4; input[i]; i++) {
            if (input[i] == ' ') {
                for (int j = i + 1; input[j]; j++)
                    remote_port = remote_port * 10 + (input[j] - '0');
                break;
            }
        }
        if (sys_tcp_connect(local_port, remote_port) >= 0)
            vga_print("TCP connection initiated", *row, 0);
        else
            vga_print("TCP connection failed", *row, 0);
        (*row)++;
    } else if (input[0] == 'p' && input[1] == '2' && input[2] == 'p' && input[3] == 'a' && input[4] == 'd' && input[5] == 'd' && input[6] == ' ') {
        unsigned int ip = 0;
        unsigned short port = 0;
        for (int i = 7; input[i] && input[i] != ' '; i++)
            ip = ip * 10 + (input[i] - '0');
        for (int i = 7; input[i]; i++) {
            if (input[i] == ' ') {
                for (int j = i + 1; input[j]; j++)
                    port = port * 10 + (input[j] - '0');
                break;
            }
        }
        if (sys_p2p_add_peer(ip, port) >= 0)
            vga_print("P2P peer added", *row, 0);
        else
            vga_print("P2P peer add failed", *row, 0);
        (*row)++;
    } else if (input[0] == 'p' && input[1] == '2' && input[2] == 'p' && input[3] == 's' && input[4] == 'h' && input[5] == 'a' && input[6] == 'r' && input[7] == 'e' && input[8] == ' ') {
        char *filename = input + 9;
        unsigned int ip = 0;
        unsigned short port = 0;
        for (int i = 9; input[i]; i++) {
            if (input[i] == ' ') {
                input[i] = 0;
                for (int j = i + 1; input[j] && input[j] != ' '; j++)
                    ip = ip * 10 + (input[j] - '0');
                for (int j = i + 1; input[j]; j++) {
                    if (input[j] == ' ') {
                        for (int k = j + 1; input[k]; k++)
                            port = port * 10 + (input[k] - '0');
                        break;
                    }
                }
                break;
            }
        }
        if (sys_p2p_share_file(filename, ip, port) >= 0)
            vga_print("File shared via P2P", *row, 0);
        else
            vga_print("P2P file share failed", *row, 0);
        (*row)++;
    } else if (input[0] == 'e' && input[1] == 'x' && input[2] == 'p' && input[3] == 'l' && input[4] == 'o' && input[5] == 'r' && input[6] == 'e' && input[7] == 'r' && input[8] == 0) {
        vga_print("Launching file explorer...", *row, 0);
        file_explorer_init();
        file_explorer_run();
        (*row)++;
    } else if (input[0] == 'p' && input[1] == '2' && input[2] == 'p' && input[3] == 'l' && input[4] == 'i' && input[5] == 's' && input[6] == 't' && input[7] == ' ') {
        unsigned int ip = 0;
        unsigned short port = 0;
        for (int i = 8; input[i] && input[i] != ' '; i++)
            ip = ip * 10 + (input[i] - '0');
        for (int i = 8; input[i]; i++) {
            if (input[i] == ' ') {
                for (int j = i + 1; input[j]; j++)
                    port = port * 10 + (input[j] - '0');
                break;
            }
        }
        file_explorer_init();
        file_explorer_browse_p2p(ip, port);
        file_explorer_run();
        (*row)++;
    } else {
        vga_print("Unknown command", *row, 0);
        (*row)++;
    }
}

void run_shell(void) {
    char input[256];
    int input_pos = 0;
    int row = 1;
    vga_print("shell> ", row, 0);
    int col = 7;
    while (1) {
        if (kbd_pos > 0) {
            char c = kbd_buffer[0];
            kbd_pos = 0;
            if (c == '\n' || c == '\r') {
                input[input_pos] = 0;
                if (input_pos > 0) {
                    row++;
                    if (row >= VGA_HEIGHT) {
                        vga_scroll();
                        row--;
                    }
                    execute_command(input, &row);
                    if (row >= VGA_HEIGHT) {
                        vga_scroll();
                        row--;
                    }
                    vga_print("shell> ", row, 0);
                    col = 7;
                    input_pos = 0;
                }
            } else if (c >= 32 && c <= 126) {
                input[input_pos++] = c;
                unsigned short *vga = (unsigned short *)VGA_BUFFER;
                vga[row * VGA_WIDTH + col] = (0x07 << 8) | c;
                col++;
            }
        }
        asm("hlt");
    }
}
