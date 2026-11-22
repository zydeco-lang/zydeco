# Primitive Values and Capabilities

## Builtin Contract and Capabilities

[Builtin](../../../../lib/std/builtin.zy) exposes canonical kinds and fixed-representation types as manifest fields.
Repeating an intrinsic splice denotes the same canonical kind or type across independently checked sources.
`Addr`, `Reader`, `Writer`, and `OS` are abstract provider capabilities sharing one opening.
`Addr` is an unmanaged data address
of kind `VType` supporting [manual memory](../../library/src/memory/manual-memory.md#manual-memory);
typed pointers, layouts, initialization states, views, and byte buffers are ordinary library types specified
in the [standard library reference](../../library/src/introduction.md).

Pure libraries can name canonical carriers without requiring a numeric or resource provider merely to name a type.
Resource operations instead share their provider's abstract opening,
so a composition root must forward the capability and its operations together.
The `numeric`, `text`, and `system` groups contain host operations.
The [standard library](../../../../lib/std/README.md) assembles ordinary module factories and defines `Bool`, `Option`,
`Either`, `List`, and abstract `Bytes`; host operations select continuations instead of constructing those types.

## Primitive Value Families

| Family | Source behavior |
| --- | --- |
| `Int`, `UInt` | Tagged machine integers; arithmetic wraps within the payload |
| `Int8/16/32/64`, `UInt8/16/32/64` | Exact signed/unsigned widths; arithmetic wraps at the chosen width |
| `Float32`, `Float64` | IEEE 754 arithmetic at the chosen width |
| `Char` | One Unicode scalar, excluding surrogates |
| `String` | Immutable valid UTF-8; indexed operations count Unicode scalars |

## Scalar Ranges and Conversions

`Int` and `UInt` use every payload bit of a tagged runtime word.
The current interpreter, AMD64, and Wasm profiles have 64-bit words and 63-bit integer payloads:
`Int` ranges from `-2^62` through `2^62 - 1`, and `UInt` from zero through `2^63 - 1`.
Their arithmetic wraps modulo `2^63`; a signed result uses two's-complement interpretation.
`Int64` ranges from `-2^63` through `2^63 - 1`, and `UInt64` from zero through `2^64 - 1`.
Their arithmetic wraps modulo `2^64`.
The [scalar value boundary](../../compiler/src/native-runtime.md#scalar-value-boundaries) specifies their
boxed runtime representation.

Integer and float literals default to `Int` and `Float64`.
An expected primitive type selects another width; integers must fit and floats round to that width.
Finite-range overflow is rejected, while float underflow may round to zero.
Existing numeric values have no implicit cross-width conversion.
`int64/from_int : Thk (Int -> Ret Int64)` and `uint64/from_uint : Thk (UInt -> Ret UInt64)` preserve the value.
The reverse operations `int64/to_int` and `uint64/to_uint` accept a result protocol,
a value, a failure thunk, and a success thunk receiving the machine integer.
They select success exactly when the mathematical value fits the destination range; they never truncate.
These operations are available in both the Builtin numeric modules and std.
Integer division or remainder by zero terminates unsuccessfully; signed minimum divided
by `-1` wraps, with remainder zero.
Integer parsing rejects values outside the `Int` range through its failure continuation;
random integers range over that same domain.
Float rendering uses the selected width's Rust Display spelling, including signed zero, `inf`, `-inf`, and `NaN`.

## Text and Raw Scalar Storage

`String` indices are scalar positions, not byte offsets or grapheme clusters; `byte_length` observes UTF-8 bytes.
Negative or out-of-range positions, invalid Unicode scalars, and invalid UTF-8 select failure branches;
the public library reifies these as `Option` or `Either`.
The source-defined `Bytes` abstraction, its immutable retention contract, and `Buffer` builders are specified
in [Memory Views, Bytes, and Buffers](../../library/src/memory/views-and-buffers.md#immutable-owners-and-source-bytes).

Every scalar Builtin module provides raw `store_le` and `load_le` operations over `Addr`.
Callers establish writable or initialized readable extents; source byte codecs validate their own lengths.
They preserve exact little-endian bits; float loads and stores preserve NaN payloads without arithmetic.
The [source codecs](../../../../lib/std/numeric/codecs.zy) provide `to_le_bytes` and `from_le_bytes`,
and std includes them in its numeric modules; decoders require the exact scalar width.

## Streams and Process Arguments

`args/at : Thk (forall (R : CType) . Int -> Thk R -> Thk (String -> R) -> R)` looks up a zero-based argument
in the invocation's stable sequence, excluding the executable name.
Negative and out-of-range indices select the first continuation; valid indices supply the string to the second.
Lookup neither advances nor consumes the sequence.
The [argument library](../../../../lib/std/system/arguments.zy) builds a lazy right fold in ordinary CBPV
whose item computation receives a reusable `Thk R`, and `process/arg_list` uses that fold to build `List String`.

I/O uses blocking byte streams and opaque reader/writer handles.
Copying a handle aliases the same resource; closing it invalidates all aliases, whose later operations report `Closed`.
Reserved standard streams are not closed by ordinary close operations.
Fallible effects use explicit `OS` continuations with structured errors.
A positive-length byte read at EOF returns empty; line reads distinguish EOF from an empty line.
Line reading removes `\n` and a preceding `\r`.
Filesystem helpers use UTF-8 paths, and write/create versus append behavior is explicit in the API.
The [library guide](../../../../lib/std/README.md#streams-and-files) is the operation inventory.

Immutable bytes are data; readers and writers grant access to resources whose observations can change.
Their `OS` protocols make that distinction explicit without requiring linear ownership of handles.
The host reports an error kind and display message, while the source library constructs its own algebraic result.
Whole-file helpers centralize open/operate/close sequencing and error precedence.
`io` owns shared streams, `fs` supplies file capabilities, and `stdio` supplies reserved process streams;
process control is separate.
A UTF-8 `Path` prevents text/path interchange but cannot represent every native path. Growable writers, seeking,
and asynchronous protocols remain [open ideas](../../../ideas/README.md#growable-byte-builders-and-incremental-streams).
