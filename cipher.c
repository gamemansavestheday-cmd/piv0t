#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>
#include <stdbool.h>

const uint8_t PI_DIGITS[100] = {
    1,4,1,5,9,2,6,5,3,5, 8,9,7,9,3,2,3,8,4,6, 2,6,4,3,3,8,3,2,7,9, 5,0,2,8,8,4,1,9,7,1, 6,9,3,9,9,3,7,5,1,0,
    5,8,2,0,9,7,4,9,4,4, 5,9,2,3,0,7,8,1,6,4, 0,6,2,8,6,2,0,8,9,9, 8,6,2,8,0,3,4,8,2,5, 3,4,2,1,1,7,0,6,7,9
};

const int PRIMES_UNDER_100[] = {
    2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97
};

#define BI_MAX_DIGITS 256

typedef struct {
    uint8_t digits[BI_MAX_DIGITS];
    int len;
} BigInt;

void bi_init(BigInt* n, uint64_t val) {
    memset(n->digits, 0, BI_MAX_DIGITS);
    n->len = 0;
    if (val == 0) {
        n->len = 1;
        return;
    }
    while (val > 0) {
        n->digits[n->len++] = val % 10;
        val /= 10;
    }
}

void bi_mul(BigInt* res, const BigInt* a, const BigInt* b) {
    BigInt temp;
    memset(temp.digits, 0, BI_MAX_DIGITS);
    temp.len = 0;

    if (a->len == 0 || b->len == 0) {
        bi_init(res, 0);
        return;
    }

    for (int i = 0; i < a->len; i++) {
        int carry = 0;
        for (int j = 0; j < b->len || carry; j++) {
            int cur = temp.digits[i + j] + (a->digits[i] * (j < b->len ? b->digits[j] : 0)) + carry;
            temp.digits[i + j] = cur % 10;
            carry = cur / 10;
        }
    }

    temp.len = a->len + b->len;
    while (temp.len > 1 && temp.digits[temp.len - 1] == 0) {
        temp.len--;
    }
    *res = temp;
}

void bi_pow_u64(BigInt* res, const BigInt* base, int exp) {
    BigInt p = *base;
    BigInt r;
    bi_init(&r, 1);

    while (exp > 0) {
        if (exp % 2 == 1) {
            BigInt next_r;
            bi_mul(&next_r, &r, &p);
            r = next_r;
        }
        BigInt next_p;
        bi_mul(&next_p, &p, &p);
        p = next_p;
        exp /= 2;
    }
    *res = r;
}

int bi_mod_int(const BigInt* n, int m) {
    int rem = 0;
    for (int i = n->len - 1; i >= 0; i--) {
        rem = (rem * 10 + n->digits[i]) % m;
    }
    return rem;
}

int bi_get_digit(const BigInt* n, int index) {
    if (index >= n->len) return 0;
    return n->digits[index];
}

int fib(int n) {
    if (n <= 0) return 0;
    if (n == 1) return 1;
    int a = 0, b = 1;
    for (int i = 2; i <= n; i++) {
        int temp = a + b;
        a = b;
        b = temp;
    }
    return b;
}

int digit_sum_recursive(uint64_t n) {
    int sum = 0;
    uint64_t temp = n;
    while (temp > 0) {
        sum += temp % 10;
        temp /= 10;
    }
    if (sum >= 10) return digit_sum_recursive(sum);
    return sum;
}

int digit_sum_simple(uint64_t n) {
    int sum = 0;
    while (n > 0) {
        sum += n % 10;
        n /= 10;
    }
    return sum;
}

int gcd(int a, int b) {
    while (b) {
        int t = b;
        b = a % b;
        a = t;
    }
    return a;
}

int mod_inverse(int a, int m) {
    a %= m;
    for (int x = 1; x < m; x++) {
        if ((a * x) % m == 1) return x;
    }
    return 1; 
}

int get_largest_prime_less_than(int n) {
    if (n <= 2) return 2;
    for (int i = (sizeof(PRIMES_UNDER_100) / sizeof(int)) - 1; i >= 0; i--) {
        if (PRIMES_UNDER_100[i] < n) return PRIMES_UNDER_100[i];
    }
    return 2;
}

typedef struct {
    int n;
    int L;
} State;

int char_to_pos(char c) {
    if (c >= 'A' && c <= 'Z') return c - 'A' + 1;
    if (c >= 'a' && c <= 'z') return c - 'a' + 1;
    return 0;
}

char pos_to_char(int p) {
    while (p <= 0) p += 26;
    p = (p - 1) % 26 + 1;
    return 'A' + (p - 1);
}

char encode_char(State* state, char p_char) {
    int P = char_to_pos(p_char);
    if (P == 0) return p_char;

    uint64_t base_val = (uint64_t)(state->n * state->n) + fib(state->L);
    BigInt Base, E;
    bi_init(&Base, base_val);
    bi_pow_u64(&E, &Base, P);

    int R1_raw = bi_mod_int(&E, 26);
    int k = digit_sum_recursive(state->n);
    
    
    int k_adj = (gcd(k, 26) != 1) ? (k + 1) : k;
    if (gcd(k_adj, 26) != 1) k_adj = 3; 

    int x = mod_inverse(k_adj, 26);
    int R_val = (R1_raw == 0) ? 26 : R1_raw;

    int sum_sieve = 0;
    for (int i = 0; i < E.len; i += 2) {
        sum_sieve += E.digits[i];
    }
    int S = sum_sieve;
    while (S > 26) S -= 26;
    if (S == 0) S = 26;

    uint64_t V = ((uint64_t)R_val * k_adj) + state->L;
    int D1 = bi_get_digit(&E, 1);
    int D2 = bi_get_digit(&E, 0);
    int idx_val = (D1 * 10 + D2) % 100;
    int tau = PI_DIGITS[idx_val];

    
    int B_mod = (R_val + state->L + tau) % 26;
    if (B_mod == 0) B_mod = 26;

    uint64_t B_loop = B_mod;
    int first_digit_n = state->n;
    while (first_digit_n >= 10) first_digit_n /= 10;
    int E_loop = (first_digit_n == 0) ? 1 : (first_digit_n % 5) + 1;

    for (int i = 0; i < state->L; i++) {
        uint64_t res = 1;
        for (int j = 0; j < E_loop; j++) res *= B_loop;
        B_loop = digit_sum_recursive(res);
    }

    int A = get_largest_prime_less_than(state->n);

    
    int F = (B_mod + (int)B_loop + A) % 26;
    if (F <= 0) F += 26;

    char cipher_char = pos_to_char(F);

    
    state->n = (state->n + F + tau) % 100;
    int shift = digit_sum_recursive((uint64_t)R_val * tau);
    state->L = (state->L + shift) % 26;
    if (state->L == 0) state->L = 26;

    return cipher_char;
}

void print_usage() {
    printf("Usage: cipher.exe <mode> <key> <text>\n");
    printf("  mode: -e (encode) or -d (decode)\n");
    printf("  key:  Seed ID (e.g., '3A')\n");
    printf("  text: String to process\n");
}

int main(int argc, char* argv[]) {
    if (argc != 4) {
        print_usage();
        return 1;
    }

    bool encode = (strcmp(argv[1], "-e") == 0);
    char* key = argv[2];
    int n = 0, L = 0, i = 0;

    while (key[i] && isdigit(key[i])) {
        n = n * 10 + (key[i] - '0');
        i++;
    }
    if (key[i] && isalpha(key[i])) {
        L = char_to_pos(key[i]);
    } else {
        printf("Error: Invalid key format.\n");
        return 1;
    }

    State state = {n % 100, L};
    char* text = argv[3];
    int len = strlen(text);
    char* result = malloc(len + 1);

    for (int j = 0; j < len; j++) {
        if (!isalpha(text[j])) {
            result[j] = text[j];
            continue;
        }

        bool is_lower = islower(text[j]);
        char input_upper = toupper(text[j]);

        if (encode) {
            char out = encode_char(&state, input_upper);
            result[j] = is_lower ? tolower(out) : out;
        } else {
            State backup = state;
            bool found = false;
            for (int candidate = 1; candidate <= 26; candidate++) {
                State temp = backup;
                char test_c = pos_to_char(candidate);
                if (encode_char(&temp, test_c) == input_upper) {
                    result[j] = is_lower ? tolower(test_c) : test_c;
                    state = temp; 
                    found = true;
                    break;
                }
            }
            if (!found) result[j] = '?';
        }
    }
    result[len] = '\0';
    printf("%s\n", result);
    free(result);

    return 0;
}