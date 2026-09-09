#include <stddef.h>
#include <stdint.h>
#include <string.h>

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

/* Matches align 64 (product uint8 uint32) on the supported little-endian C targets. */
struct zyffi_record {
    _Alignas(64) uint8_t tag;
    uint32_t payload;
};
_Static_assert(sizeof(struct zyffi_record) == 64, "record size");
_Static_assert(offsetof(struct zyffi_record, payload) == 4, "field offset");

uint64_t zyffi_record(const void *data, size_t length) {
    if (length != sizeof(struct zyffi_record) || (uintptr_t)data % 64 != 0) {
        return 0;
    }
    /* memcpy avoids imposing a C effective type on an immutable byte allocation. */
    struct zyffi_record record;
    const uint8_t *source = data;
    memcpy(&record, data, sizeof(record));
    if (record.tag != 7 || record.payload != UINT32_C(16909060)) {
        return 0;
    }
    for (size_t index = 1; index < 4; ++index) {
        if (source[index] != 0) return 0;
    }
    for (size_t index = 8; index < sizeof(record); ++index) {
        if (source[index] != 0) return 0;
    }
    return 1;
}
