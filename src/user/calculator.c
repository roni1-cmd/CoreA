#define MAX_INPUT 32

extern int sys_gui_create_window(int x, int y, int w, int h, unsigned char color);
extern int sys_gui_create_button(int win_id, int x, int y, int w, int h, const char *label);
extern int sys_gui_create_text_field(int win_id, int x, int y, int w, int h);
extern int sys_gui_get_event(int *type, int *x, int *y, unsigned char *data);
extern void sys_clipboard_copy(const char *data, int len);
extern void sys_logger_add_event(const char *message);

int win_id = -1;
int input_field_id = -1;
int result_field_id = -1;
int add_button_id = -1;
int sub_button_id = -1;
int mul_button_id = -1;
int div_button_id = -1;

void calculator_init(void) {
    win_id = sys_gui_create_window(100, 100, 200, 120, 7); // Light gray
    if (win_id < 0) return;

    input_field_id = sys_gui_create_text_field(win_id, 10, 10, 180, 20);
    result_field_id = sys_gui_create_text_field(win_id, 10, 40, 180, 20);
    add_button_id = sys_gui_create_button(win_id, 10, 70, 40, 20, "+");
    sub_button_id = sys_gui_create_button(win_id, 60, 70, 40, 20, "-");
    mul_button_id = sys_gui_create_button(win_id, 110, 70, 40, 20, "*");
    div_button_id = sys_gui_create_button(win_id, 160, 70, 40, 20, "/");

    sys_logger_add_event("Calculator launched");
}

void calculator_run(void) {
    if (win_id < 0) return;
    int event_type, event_x, event_y;
    unsigned char event_data[256];

    while (1) {
        if (sys_gui_get_event(&event_type, &event_x, &event_y, event_data) > 0) {
            if (event_type == 3) { // Button click
                int button_id = event_data[0];
                char input[MAX_INPUT] = {0};
                for (int i = 0; i < MAX_INPUT && event_data[i + 1]; i++)
                    input[i] = event_data[i + 1];

                int num1 = 0, num2 = 0, op_pos = -1;
                for (int i = 0; input[i]; i++) {
                    if (input[i] == '+' || input[i] == '-' || input[i] == '*' || input[i] == '/') {
                        op_pos = i;
                        break;
                    }
                    num1 = num1 * 10 + (input[i] - '0');
                }
                if (op_pos >= 0) {
                    for (int i = op_pos + 1; input[i]; i++)
                        num2 = num2 * 10 + (input[i] - '0');
                }

                int result = 0;
                char result_str[32] = {0};
                if (button_id == add_button_id || (op_pos >= 0 && input[op_pos] == '+')) {
                    result = num1 + num2;
                } else if (button_id == sub_button_id || (op_pos >= 0 && input[op_pos] == '-')) {
                    result = num1 - num2;
                } else if (button_id == mul_button_id || (op_pos >= 0 && input[op_pos] == '*')) {
                    result = num1 * num2;
                } else if (button_id == div_button_id || (op_pos >= 0 && input[op_pos] == '/')) {
                    if (num2 != 0) result = num1 / num2;
                }

                int i = 0;
                if (result < 0) {
                    result_str[i++] = '-';
                    result = -result;
                }
                int temp = result, digits = 0;
                do {
                    digits++;
                    temp /= 10;
                } while (temp);
                for (int j = digits - 1; j >= 0; j--) {
                    result_str[i + j] = '0' + (result % 10);
                    result /= 10;
                }
                sys_clipboard_copy(result_str, digits + (i ? 1 : 0));
                sys_gui_create_text_field(win_id, 10, 40, 180, 20);
                sys_logger_add_event("Calculation performed");
            }
        }
        asm("hlt");
    }
}
