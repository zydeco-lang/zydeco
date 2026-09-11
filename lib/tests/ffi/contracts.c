/* Concrete foreign interfaces used to evaluate the next Zydeco FFI extension. */
#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

struct sample_record {
    uint8_t tag;
    uint32_t payload;
};

_Static_assert(sizeof(struct sample_record) == 8, "example record size");
_Static_assert(_Alignof(struct sample_record) == 4, "example record alignment");
_Static_assert(offsetof(struct sample_record, payload) == 4, "example field offset");

/* Already expressible: Bytes supplies the pointer and its visible length. */
uint64_t sample_checksum(const void *source, size_t length, uint64_t seed) {
    const uint8_t *bytes = source;
    for (size_t index = 0; index < length; ++index) {
        seed = seed * 33 + bytes[index];
    }
    return seed;
}

/* A single pointer, with extent and alignment required by the binding. */
uint64_t sample_inspect(const struct sample_record *source) {
    struct sample_record record;
    memcpy(&record, source, sizeof(record));
    return ((uint64_t)record.tag << 32) | record.payload;
}

/* The adapter required by today's pointer-and-length-only Bytes transport. */
uint64_t sample_inspect_bytes(const void *source, size_t length) {
    if (length != sizeof(struct sample_record)
        || (uintptr_t)source % _Alignof(struct sample_record) != 0) {
        return UINT64_MAX;
    }
    return sample_inspect(source);
}

/* Same storage layout, but a different ABI contract from the pointer form. */
uint64_t sample_inspect_value(struct sample_record source) {
    return sample_inspect(&source);
}

/* Six C components; the current Bytes classifier would describe seven. */
uint64_t sample_inspect_with_options(const struct sample_record *source,
                                   uint64_t a, uint64_t b, uint64_t c,
                                   uint64_t d, uint64_t e) {
    return sample_inspect(source) + a + 3 * b + 5 * c + 7 * d + 11 * e;
}

/* C output need not satisfy Zydeco's canonical zero-padding convention. */
int32_t sample_write(void *destination, size_t capacity,
                     uint8_t tag, uint32_t payload) {
    if (capacity < sizeof(struct sample_record)) return -1;
    uint8_t *bytes = destination;
    memset(bytes, 0x58, sizeof(struct sample_record));
    memcpy(bytes + offsetof(struct sample_record, tag), &tag, sizeof(tag));
    memcpy(bytes + offsetof(struct sample_record, payload), &payload, sizeof(payload));
    return 0;
}

typedef int64_t (*sample_step)(void *context, int64_t value);

/* Synchronous, repeated callback invocation; neither pointer is retained. */
int64_t sample_visit(const int64_t *values, size_t length,
                     sample_step step, void *context) {
    int64_t total = 0;
    for (size_t index = 0; index < length; ++index) {
        total += step(context, values[index]);
    }
    return total;
}

struct sample_context {
    int64_t increment;
    size_t calls;
};

static int64_t sample_increment(void *context, int64_t value) {
    struct sample_context *state = context;
    state->calls += 1;
    return value + state->increment;
}

int main(void) {
    const uint8_t bytes[] = {1, 2, 3};
    assert(sample_checksum(NULL, 0, 7) == 7);
    assert(sample_checksum(bytes, sizeof(bytes), 0) == 1158);

    struct sample_record record;
    memset(&record, 0, sizeof(record));
    uint8_t original[sizeof(record)];
    memcpy(original, &record, sizeof(record));
    assert(sample_write(&record, sizeof(record) - 1, 7, UINT32_C(0x01020304)) == -1);
    assert(memcmp(&record, original, sizeof(record)) == 0);
    assert(sample_write(&record, sizeof(record), 7, UINT32_C(0x01020304)) == 0);
    uint64_t expected = (UINT64_C(7) << 32) | UINT64_C(0x01020304);
    assert(sample_inspect(&record) == expected);
    assert(sample_inspect_value(record) == expected);
    assert(sample_inspect_bytes(&record, sizeof(record)) == expected);
    assert(sample_inspect_bytes(&record, sizeof(record) - 1) == UINT64_MAX);
    assert(sample_inspect_with_options(&record, 1, 2, 3, 4, 5) == expected + 105);

    const int64_t values[] = {2, 4, 8};
    struct sample_context first = {10, 0};
    struct sample_context second = {20, 0};
    assert(sample_visit(NULL, 0, sample_increment, &first) == 0);
    assert(first.calls == 0);
    assert(sample_visit(values, 3, sample_increment, &first) == 44);
    assert(sample_visit(values, 3, sample_increment, &second) == 74);
    assert(first.calls == 3 && second.calls == 3);

    /* The Rust test feeds these actual C-produced bytes to the source decoder. */
    return fwrite(&record, 1, sizeof(record), stdout) == sizeof(record) ? 0 : 1;
}
