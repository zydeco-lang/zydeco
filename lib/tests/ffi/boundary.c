#include <stddef.h>
#include <stdint.h>
#include <string.h>

uint64_t zyffi_first(const void *data) { return ((const uint8_t *)data)[0]; }

uint64_t zyffi_options(const void *data, uint64_t a, uint64_t b,
                        uint64_t c, uint64_t d, uint64_t e) {
    return zyffi_first(data) + a + 3 * b + 5 * c + 7 * d + 11 * e;
}

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

/* Fixed-width declarations exercise each signedness/width independently. */
#define ZYFFI_ECHO(type, name) type zyffi_##name(type value) { return value; }
ZYFFI_ECHO(int8_t, int8)
ZYFFI_ECHO(int16_t, int16)
ZYFFI_ECHO(int32_t, int32)
ZYFFI_ECHO(int64_t, int64)
ZYFFI_ECHO(uint8_t, uint8)
ZYFFI_ECHO(uint16_t, uint16)
ZYFFI_ECHO(uint32_t, uint32)
ZYFFI_ECHO(uint64_t, uint64)

/* The ABI defines only the low bits of a narrow integer return register. */
#if defined(__x86_64__)
#define ZYFFI_DIRTY(type, name) \
    __attribute__((naked)) type zyffi_dirty_##name(void) { \
        __asm__("movabs $0x5a5a5a5a80008080, %rax\nret"); \
    }
#else
#define ZYFFI_DIRTY(type, name) \
    type zyffi_dirty_##name(void) { return (type)UINT64_C(0x5a5a5a5a80008080); }
#endif
ZYFFI_DIRTY(int8_t, int8)
ZYFFI_DIRTY(int16_t, int16)
ZYFFI_DIRTY(int32_t, int32)
ZYFFI_DIRTY(uint8_t, uint8)
ZYFFI_DIRTY(uint16_t, uint16)
ZYFFI_DIRTY(uint32_t, uint32)

int64_t zyffi_integer_mix(int8_t a, int16_t b, int32_t c,
                          uint8_t d, uint16_t e, uint32_t f) {
    return (int64_t)a + 3 * (int64_t)b + 5 * (int64_t)c
           + 7 * (int64_t)d + 11 * (int64_t)e + 13 * (int64_t)f;
}

static int64_t zyffi_saved;
void zyffi_save(int64_t value) { zyffi_saved = value; }
int64_t zyffi_saved_value(void) { return zyffi_saved; }

/* A status-returning output operation: failed capacity preflight performs no writes. */
int32_t zyffi_write_record(void *data, uint64_t capacity, uint8_t tag, uint32_t payload) {
    if (capacity < 8) return -1;
    uint8_t *bytes = data;
    bytes[0] = tag;
    memcpy(bytes + 4, &payload, sizeof payload);
    return 0;
}
