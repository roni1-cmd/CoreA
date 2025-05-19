#define MAX_TASKS 8
#define SAMPLE_RATE 100

struct task_stats {
    unsigned int cycles;
    unsigned int switches;
    int active;
};

struct task_stats profiler[MAX_TASKS];
extern volatile unsigned int ticks;
extern int current_task;

void init_profiler(void) {
    for (int i = 0; i < MAX_TASKS; i++)
        profiler[i].active = 0;
}

void profiler_update(void) {
    if (ticks % SAMPLE_RATE == 0) {
        if (profiler[current_task].active)
            profiler[current_task].cycles++;
        profiler[current_task].switches++;
        profiler[current_task].active = 1;
    }
}

void profiler_get_stats(char *buf, int size) {
    if (!buf || size <= 0) {
        buf[0] = 0;
        return;
    }
    int pos = 0;
    for (int i = 0; i < MAX_TASKS && pos < size - 1; i++) {
        if (profiler[i].active) {
            char tmp[32];
            int t = 0;
            tmp[t++] = 'T';
            tmp[t++] = 'a';
            tmp[t++] = 's';
            tmp[t++] = 'k';
            tmp[t++] = ' ';
            tmp[t++] = i + '0';
            tmp[t++] = ':';
            tmp[t++] = ' ';
            unsigned int c = profiler[i].cycles;
            if (c == 0) tmp[t++] = '0';
            while (c > 0 && t < 32 - 1) {
                tmp[t++] = (c % 10) + '0';
                c /= 10;
            }
            tmp[t++] = ' ';
            tmp[t++] = 'c';
            tmp[t++] = 'y';
            tmp[t++] = 'c';
            tmp[t++] = 'l';
            tmp[t++] = 'e';
            tmp[t++] = 's';
            tmp[t++] = '\n';
            for (int j = 0; j < t && pos < size - 1; j++)
                buf[pos++] = tmp[j];
        }
    }
    buf[pos] = 0;
}
