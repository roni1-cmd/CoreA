#define MAX_KEYS 8
#define KEY_SIZE 16
#define MAX_PASS 32

struct key_entry {
    unsigned char key[KEY_SIZE];
    char name[MAX_NAME];
    int active;
};

struct key_entry keys[MAX_KEYS];
char system_password[MAX_PASS] = "corea123"; // Hardcoded for simplicity

// Simple PRNG for key generation
unsigned int prng_state = 12345;
unsigned int prng_next(void) {
    prng_state = (prng_state * 1103515245 + 12345) & 0x7fffffff;
    return prng_state;
}

// Generate a 128-bit key
void keymgmt_generate_key(char *name, unsigned char *key) {
    for (int i = 0; i < KEY_SIZE; i++)
        key[i] = prng_next() & 0xff;
    for (int i = 0; i < MAX_KEYS; i++) {
        if (!keys[i].active) {
            for (int j = 0; j < KEY_SIZE; j++)
                keys[i].key[j] = key[j];
            for (int j = 0; j < MAX_NAME && name[j]; j++)
                keys[i].name[j] = name[j];
            keys[i].name[MAX_NAME - 1] = 0;
            keys[i].active = 1;
            break;
        }
    }
}

// Retrieve a key by name
int keymgmt_get_key(char *name, unsigned char *key) {
    for (int i = 0; i < MAX_KEYS; i++) {
        if (keys[i].active) {
            int match = 1;
            for (int j = 0; j < MAX_NAME && name[j]; j++) {
                if (keys[i].name[j] != name[j]) {
                    match = 0;
                    break;
                }
            }
            if (match) {
                for (int j = 0; j < KEY_SIZE; j++)
                    key[j] = keys[i].key[j];
                return 0;
            }
        }
    }
    return -1;
}

// Authenticate user
int keymgmt_authenticate(const char *password) {
    for (int i = 0; i < MAX_PASS && password[i]; i++)
        if (password[i] != system_password[i])
            return -1;
    return 0;
}
