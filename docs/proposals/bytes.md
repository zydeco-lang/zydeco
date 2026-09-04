# Immutable byte buffers

The first `Bytes` interface could create buffers and measure them, but never inspect one:
`empty`, `length`, `append`, `from_string`, and `to_string` were the whole surface.
A program that read a file into `Bytes`, or received a buffer from a foreign call,
could only convert it to `String` or report its size.
Binary formats, hashing, protocols, and the existing FFI seed were all blocked on the same
missing operation: reading an octet at a position.

This design completes the interface without giving up immutability.
Every operation is either a pure observation or a construction of a new buffer;
nothing mutates a buffer in place, and no operation's result can change over time.
The question this document answers is which operations and representation make that
interface *system-level* rather than merely complete.

## Goals and constraints

- **Equational theory.** `Bytes` values are values, not locations.
  Replacing a buffer by an equal-content buffer never changes an observation,
  and no operation creates an observation channel through shared storage.
  Structural equality and immutability are therefore load-bearing, not conveniences.
- **System-level cost.** Random access should be constant-time, decomposition should not
  copy, and the representation handed to a C call should already be contiguous.
- **Totality discipline.** Partial inputs select continuations; malformed input never panics
  the host. New operations follow the existing branch ABI rather than introducing sentinels.
- **Library purity.** Builtins never construct library-defined `Option`, `Bool`, or `List`;
  they expose computation-polymorphic branches that the public library reifies.

## Interface

Five host operations join the builtin text group.
Branch style marks the partial ones.
Octets cross the boundary as `UInt8`, the type an octet already is;
indices and lengths remain `Int64`, matching the other indexed operations.

```text
bytes/get       : Bytes -> Int64 -> <none | some(UInt8)>
bytes/slice     : Bytes -> Int64(start) -> Int64(length) -> <none | some(Bytes)>
bytes/singleton : UInt8 -> Ret Bytes
bytes/eq        : Bytes -> Bytes -> <true | false>
bytes/lt        : Bytes -> Bytes -> <true | false>
```

`bytes/slice` takes a start and a length, not two indices.
A window then names exactly what a pointer-and-length foreign call receives,
and `start + length` overflow is the only composed bound to check.
An empty window at the end of a buffer is valid; a window that starts past the end,
a negative component, or a window extending past the end reports `none`.

`bytes/singleton` is total: its `UInt8` parameter makes every input a valid octet,
so no rejection branch exists and the derived `from_list` performs no revalidation.
It exists because builtins cannot consume library lists;
it is the minimal primitive from which the public `from_list` derives,
and it gives byte-at-a-time construction a total form.
`bytes/eq` is structural; `bytes/lt` is lexicographic octet order,
which is the order protocols and keyed collections expect of keys.

The public `bytes` module reifies the branches and derives the rest:

```text
get, slice                              reified to Option
singleton                              total, no branch at any layer
eq, lt, gt, le, ge                     reified and derived order
is_empty, concat, starts_with, ends_with  list- and window-level conveniences
to_list, from_list                     the List UInt8 bridge
```

`gt` flips `lt`; `le` and `ge` complement it.
`starts_with` and `ends_with` are defined by slicing and equality rather than new primitives,
which keeps the host ABI small and the definitions checkable against the primitives.

## Representation

The interpreter stores a buffer as one shared allocation plus a window:

```text
SharedBytes { buffer: Rc<[u8]>, start, length }
```

`slice` re-windows the same allocation in constant time, so decomposing a buffer
allocates nothing, and `as_slice` is always contiguous, which keeps foreign calls
a plain pointer-and-length borrow of `view.as_slice()`.
The WASM host windows through `Uint8Array` views.
The AMD64 runtime currently copies windows into fresh allocations; its buffers are
outside the collector, so sharing a parent requires a representation change
deferred with the runtime's broader memory work (below).

Cross-backend performance divergence is deliberate: the ABI fixes semantics,
and each backend may share or copy behind it.
Equality compares windows, so sharing is unobservable,
which is exactly the property the equational theory needs.

## Cost model

| Operation | Cost |
| --------- | ---- |
| `length`, `get`, `slice` | O(1) (AMD64 `slice` copies: O(n)) |
| `eq`, `lt` | O(min length), content comparison |
| `append`, `concat` | O(n + m) |
| `to_list`, `from_list` | O(n) calls, each a constant-time step |

Assembly by repeated `append` is quadratic.
That is accepted for now, with two sanctioned escapes recorded below,
because the alternative representations trade away either contiguity or simplicity.

## Alternatives considered

- **Rope or RRB-tree buffers** give O(1) pure append and O(log n) functional update,
  but every FFI boundary then needs flattening, the WASM host needs a tree,
  and the AMD64 runtime needs collector-managed nodes.
  Deferred until pure concatenation is measured as a bottleneck.
- **An OS-scoped builder capability** (amortized O(1) pushes, one `freeze` into `Bytes`)
  is the CBPV-shaped answer to Rust's `Vec<u8>`: construction is an effect,
  the result is a value, and the equational theory of the pure layer is untouched.
  A natural form is an in-memory `Writer` plus a contents observation,
  reusing the existing capability machinery rather than a new builtin family.
  Deferred until the io layer grows a memory-backed capability.
- **Byte-level mutation** (`set`-style update) is out of scope by design.
  Functional update returns with the representations above if it returns at all;
  effectful update belongs to a builder or a future linear-capability layer.

## Backend contract

Each execution surface implements the five operations with identical semantics:
the interpreter in `lang/dynamics`, the AMD64 runtime in `runtime/stub.rs`,
and the WASM host in `lang/tests/wasm-host.mjs`.
The statics classifier accepts the new annotations as `optional`, `branch`, and `pure` shapes,
and StackIR marks every continuation-selecting operation `Control`.
Foreign calls borrow a contiguous view and must treat it as read-only;
a callee that writes through the borrowed pointer breaks the immutability every
other layer guarantees.

## Remaining uncertainty

- Whether `bytes/lt` should grow a three-way `compare` companion,
  or whether an `Order` data type at the library level is the better shape
  once keyed collections join the standard library.
- When quadratic `concat` on the AMD64 backend matters in practice,
  which of the two sanctioned escapes to take first.

An earlier open question asked whether octets should surface as `UInt8` rather than `Int64`.
Resolved in favor of `UInt8` on both sides of the boundary:
reading an octet and constructing one carry the same type,
and the `Int64`-in-`[0, 255]` convention survives only where values are genuinely
larger than a byte, as with `char/codepoint`.
