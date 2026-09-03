#include <stddef.h>
#include <stdint.h>

uint64_t zyffi_zero(void) { return UINT64_MAX; }

uint64_t zyffi_echo(uint64_t value) { return value; }

uint64_t zyffi_bytes(const void *data, size_t length) {
    return length == 0 ? 0 : ((const uint8_t *)data)[0] + UINT64_C(257) * length;
}

uint64_t zyffi_mixed(uint64_t seed, const void *left, size_t left_length,
                    const void *right, size_t right_length, uint64_t tail) {
    return seed ^ zyffi_bytes(left, left_length)
           ^ (zyffi_bytes(right, right_length) << 32) ^ tail;
}

uint64_t zyffi_three_bytes(const void *first, size_t first_length,
                          const void *second, size_t second_length,
                          const void *third, size_t third_length) {
    return zyffi_bytes(first, first_length) + 3 * zyffi_bytes(second, second_length)
           + 5 * zyffi_bytes(third, third_length);
}

uint64_t zyffi_six(uint64_t a, uint64_t b, uint64_t c,
                   uint64_t d, uint64_t e, uint64_t f) {
    return a + 3 * b + 5 * c + 7 * d + 11 * e + 13 * f;
}
