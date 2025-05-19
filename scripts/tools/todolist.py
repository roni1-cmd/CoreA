import corea_syscalls as sys
import time

MAX_TASKS = 16
MAX_NAME = 32
MAX_MESSAGE = 64

class TaskEntry:
    def __init__(self, name="", active=0):
        self.name = name
        self.active = active
        self.completed = 0

win_id = -1
input_field_id = -1
add_button_id = -1
complete_button_id = -1
delete_button_id = -1
tasks = [TaskEntry() for _ in range(MAX_TASKS)]
task_buttons = [-1] * MAX_TASKS
selected_task_idx = -1

def notetaker_init():
    global win_id, input_field_id, add_button_id, complete_button_id, delete_button_id
    win_id = sys.sys_gui_create_window(160, 160, 240, 180, 7)  # Light gray
    if win_id < 0:
        return

    input_field_id = sys.sys_gui_create_text_field(win_id, 10, 10, 220, 40)
    add_button_id = sys.sys_gui_create_button(win_id, 10, 60, 60, 20, "Add")
    complete_button_id = sys.sys_gui_create_button(win_id, 80, 60, 80, 20, "Complete")
    delete_button_id = sys_gui_create_button(win_id, 170, 60, 60, 20, "Delete")

    update_tasks()
    sys.sys_logger_add_event("Todo list launched")

def update_tasks():
    global tasks, task_buttons
    file_list = sys.sys_vfs_list("/ram/tasks", 256)
    idx = 0
    y_offset = 90
    for i in range(len(file_list)):
        if file_list[i] == '\n':
            file_list[i] = '\0'
            if i > 0 and file_list[i-1]:
                name = ''.join(file_list[i-j-1] for j in range(1, min(MAX_NAME, i+1)) if file_list[i-j-1])
                if idx < MAX_TASKS:
                    tasks[idx].name = name
                    tasks[idx].active = 1
                    tasks[idx].completed = name.startswith('c_')
                    task_buttons[idx] = sys.sys_gui_create_button(win_id, 10, y_offset, 220, 15, name)
                    y_offset += 20
                    idx += 1
    for i in range(idx, MAX_TASKS):
        tasks[i].active = 0
        task_buttons[i] = -1

def notetaker_run():
    global selected_task_idx
    if win_id < 0:
        return
    while True:
        event = sys.sys_gui_get_event()
        if event:
            event_type, event_x, event_y, data = event
            if event_type == 3:  # Button click
                button_id = data[0]
                content = ''.join(chr(c) for c in data[1:] if c)
                content_len = len(content)

                if button_id == add_button_id and content_len > 0:
                    task_num = sum(1 for t in tasks if t.active)
                    filename = f"task_{task_num}"
                    if sys.sys_vfs_write(filename, content, content_len) >= 0:
                        sys.sys_gui_create_text_field(win_id, 10, 10, 220, 40)  # Clear input
                        update_tasks()
                        sys.sys_logger_add_event("Task added")
                elif button_id == complete_button_id and selected_task_idx >= 0:
                    old_name = tasks[selected_task_idx].name
                    new_name = f"c_{old_name}"
                    content = sys.sys_vfs_read(old_name, 256)
                    if content and sys.sys_vfs_write(new_name, content, len(content)) >= 0:
                        sys.sys_vfs_delete(old_name)
                        update_tasks()
                        sys.sys_logger_add_event("Task marked complete")
                elif button_id == delete_button_id and selected_task_idx >= 0:
                    if sys.sys_vfs_delete(tasks[selected_task_idx].name) >= 0:
                        tasks[selected_task_idx].active = 0
                        selected_task_idx = -1
                        update_tasks()
                        sys.sys_logger_add_event("Task deleted")
                else:
                    for i in range(MAX_TASKS):
                        if tasks[i].active and task_buttons[i] == button_id:
                            selected_task_idx = i
                            content = sys.sys_vfs_read(tasks[i].name, 256)
                            if content:
                                sys.sys_gui_create_text_field(win_id, 10, 10, 220, 40)
                                sys.sys_logger_add_event("Task opened")
                            break
        time.sleep(0.1)  # Simulate hlt

if __name__ == "__main__":
    notetaker_init()
    notetaker_run()
