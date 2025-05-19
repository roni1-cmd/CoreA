#define TEA_KEY_SIZE 16
#define HASH_SIZE 4

// Hardcoded 128-bit key for TEA (replace with key exchange later)
static unsigned char tea_key[TEA_KEY_SIZE] = {
    0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0,
    0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88
};

// TEA encryption (in-place, 32-bit blocks)
void tea_encrypt(unsigned int *v, int len, const unsigned char *key) {
    unsigned int k[4] = { *(unsigned int *)(key + 0), *(unsigned int *)(key + 4),
                          *(unsigned int *)(key + 8), *(unsigned int *)(key + 12) };
    unsigned int delta = 0x9e3779b9;
    for (int i = 0; i < len; i += 2) {
        unsigned int sum = 0;
        for (int j = 0; j < 32; j++) {
            sum += delta;
            v[i] += ((v[i + 1] << 4) + k[0]) ^ (v[i + 1] + sum) ^ ((v[i + 1] >> 5) + k[1]);
            v[i + 1] += ((v[i] << 4) + k[2]) ^ (v[i] + sum) ^ ((v[i] >> 5) + k[3]);
        }
    }
}

// TEA decryption (in-place, 32-bit blocks)
void tea_decrypt(unsigned int *v, int len, const unsigned char *key) {
    unsigned int k[4] = { *(unsigned int *)(key + 0), *(unsigned int *)(key + 4),
                          *(unsigned int *)(key + 8), *(unsigned int *)(key + 12) };
    unsigned int delta = 0x9e3779b9, sum = delta * 32;
    for (int i = 0; i < len; i += 2) {
        for (int j = 0; j < 32; j++) {
            v[i + 1] -= ((v[i] << 4) + k[2]) ^ (v[i] + sum) ^ ((v[i] >> 5) + k[3]);
            v[i] -= ((v[i + 1] << 4) + k[0]) ^ (v[i + 1] + sum) ^ ((v[i + 1] >> 5) + k[1]);
            sum -= delta;
        }
    }
}

// Simple 32-bit hash for signatures
unsigned int crypto_hash(const char *data, int len) {
    unsigned int hash = 0;
    for (int i = 0; i < len; i++)
        hash = (hash * 31 + data[i]) ^ (hash >> 3);
    return hash;
}

// Encrypt a buffer (padded to 8-byte blocks)
int crypto_encrypt_buffer(char *buf, int len) {
    if (len <= 0 || len > 256) return -1;
    int padded_len = (len + 7) & ~7; // Pad to multiple of 8
    for (int i = len; i < padded_len; i++)
        buf[i] = 0;
    tea_encrypt((unsigned int *)buf, padded_len / 4, tea_key);
    return padded_len;
}

// Decrypt a buffer
int crypto_decrypt_buffer(char *buf, int len) {
    if (len <= 0 || len % 8 != 0 || len > 256) return -1;
    tea_decrypt((unsigned int *)buf, len / 4, tea_key);
    for (int i = len - 1; i >= 0 && buf[i] == 0; i--)
        len--;
    return len;
}

// Sign a buffer (append 4-byte hash)
int crypto_sign_buffer(char *buf, int len, char *signature) {
    if (len <= 0 || len > 256) return -1;
    unsigned int hash = crypto_hash(buf, len);
    *(unsigned int *)signature = hash;
    return HASH_SIZE;
}

// Verify a buffer's signature
int crypto_verify_buffer(const char *buf, int len, const char *signature) {
    if (len <= 0 || len > 256) return -1;
    unsigned int hash = crypto_hash(buf, len);
    return *(unsigned int *)signature == hash ? 0 : -1;
}
