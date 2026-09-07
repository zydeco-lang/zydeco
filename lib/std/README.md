# Zydeco Standard Library

## Start with the types

Select the foundational kinds and types directly from the Builtin contract:

```zydeco
param (/VType; /CType; /Ret; /Thk; /Int64; /Float32; /Float64) : @[import("lib/std/builtin.zy")] _ in
...
```

The path above is relative to a source file in the repository root.
The contract's surface carries the compiler-canonical kinds and types as manifest fields,
so one field search reaches every public name and every selection shares one type identity.
A pure library function states its interface with just this parameter:

```zydeco
param (/Ret; /Int64) : @[import("lib/std/builtin.zy")] _ in
fn (value : Int64) => (ret value : Ret Int64)
```

The available surface names are:

| Family | Names |
| --- | --- |
| CBPV | `VType`, `CType`, `Thk`, `Ret`, `Unit` |
| Signed integers | `Int8`, `Int16`, `Int32`, `Int64` |
| Unsigned integers | `UInt8`, `UInt16`, `UInt32`, `UInt64` |
| Floating point | `Float32`, `Float64` |
| Text and bytes | `Char`, `String`, `Bytes` |
| Capabilities | `Reader`, `Writer`, `OS` |

Host operations live in the same contract under the `numeric`, `text`, and `system` groups, and the search descends
through them: `param (/stdio; /process) : @[import("lib/std/builtin.zy")] _ in` selects two operations
from the `system` group without naming it.
The [integer example](../tests/std/minimal.zy) selects `Int64` and `Ret` this way,
then uses the assembled standard package for arithmetic.

## Library boundaries

Runtime operations have two boundaries.
[`builtin.zy`](builtin.zy) is the typed contract between Zydeco programs and the host runtime.
Its operations expose representation-independent observations and effects,
but never construct library-defined `Bool`, `Option`, `Result`, or `List` values.
[`std.zy`](std.zy) applies the topic packages in this directory and assembles the public package,
whose sealed type its final `pack` introduction synthesizes.

Each topic owns exactly one implementation: `data`, `text`, `system`, and `numeric` each provide `package.zy`,
sealed by a final `pack` introduction whose existential type the checker synthesizes,
so importers splice the implementation without a companion annotation.
Implementations annotate their parameters in place: the Builtin group through `builtin.zy`,
and the shared algebraic base through `data/package.type.zy`.
The implementation defines its topic's data types and operations in one dependency-scheduled block,
so derived operations sit next to the types they observe and no per-module contract split remains.
`data/package.type.zy` names that shared base type directly, carrying the data topic's existential witnesses
and module telescopes in one declaration.
Type files bind `VType` and `CType` once at the top of the file and use those aliases in every classifier below.

This separation keeps algebraic data in the language.
The interpreter and native runtime only need to agree on the small Builtin ABI,
while the files under `data/` and the derived operations in the topic packages remain ordinary Zydeco code.

## Source layout

The files at the root of this directory define the public entry points:

```text
builtin.zy                 complete host ABI: surface kinds and types, operation groups
std.zy                     wiring for the public package

builtin/numeric/*.zy       exact-width primitive operations
builtin/text/*.zy          Char, String, and Bytes host operations
builtin/system/*.zy        I/O, filesystem, streams, arguments, randomness, process

data/package.zy            Bool, Option, Result, List, and every derived operation
data/package.type.zy       DataPackage existential wrapper with the module telescopes
data/bool.type.zy          BoolModule telescope shared with the numeric builders

numeric/{integer,float}.zy explicitly polymorphic derived numeric builders
numeric/package.zy         the ten width modules and their capability dictionaries

text/package.zy            cross-representation text operations

system/package.zy          system data types and capability-preserving assembly

control/*.zy               monadic basis, State, Exception, and their combination

**/*.type.zy               reusable type terms imported by implementations and companions
```

Topic implementations are independently checkable package functions.
The public package keeps one opening for `Reader`, `Writer`, and `OS`; splitting
that opening would give related I/O operations incompatible abstract types.
No compatibility forwarding files remain at the old flat paths.

## Builtin packages

The host contract is one launcher-supplied value.
Its complete leading telescope carries every public static name as a manifest field — the CBPV kinds,
constructors, and fixed-representation types — followed by the three generative system capabilities,
and its body groups the runtime operations:

- Surface: `VType`, `CType`, `Thk`, `Ret`, `Unit`, the ten fixed-width numeric types, `Char`, `String`,
  and `Bytes`, then abstract `Reader`, `Writer`, and `OS`.
- `numeric`: exact-width arithmetic, branch comparisons, and rendering, one plain operation module per representation.
- `text`: operations crossing `Char`, `String`, `Bytes`, and `Int64`.
- `system`: the re-exposed capabilities plus I/O, filesystem, standard stream, argument,
  randomness, and process operations.

Each public name is unique across the complete package, so one field search reaches kinds, types,
operations, and capabilities alike, and every selection shares the contract's identities.
Fixed representations are compiler-canonical intrinsics, so independent selections share one `Int64` identity.
Only the runtime-owned system capabilities are generative existential types.
A composition root that must pass the dependency onward keeps the whole-alias `builtin` beside its selections.
See [Modular primitive packages](../../docs/proposals/primitive-packages.md) for the design and usage examples.
Compiler intrinsics are spliced inline as `@(intrinsic(name))` wherever a contract needs the canonical term,
so no one-line indirection files sit between type expressions and the compiler metadata they name.
Builtin leaves bind the intrinsic kinds and constructors they use at the top of the file,
so their classifiers read as ordinary type expressions.

## Text model

`String` is immutable, valid UTF-8 text.
Its indexed operations use zero-based Unicode scalar positions:

- `string/length` counts Unicode scalar values.
- `string/byte_length` counts bytes in the UTF-8 encoding.
- `string/get` returns `Option Char`; negative and out-of-range positions return `none`.
- `string/split_at` splits at a scalar boundary and returns `none` for an invalid position.
- `string/to_chars` and `string/from_chars` convert between text and `List Char`.

A `Char` is one Unicode scalar value.
`char/codepoint` returns its integer value, and `char/from_codepoint` rejects negative numbers,
surrogate code points, and values above the Unicode range with `none`.

`Bytes` is an immutable sequence of octets with no encoding attached.
Positions and lengths count octets, not scalars, and `bytes/get` reports one octet as a `UInt8`.
`bytes/slice value start length` returns the window `[start, start + length)` sharing storage
with `value` where the backend supports it, so decomposing a buffer does not copy it.
Two buffers are equal exactly when their octet sequences are equal;
`bytes/lt` compares buffers lexicographically octet by octet.
Construction from single octets goes through `bytes/singleton`, which is total because every `UInt8` is a valid octet;
the byte-level FFI contract treats borrowed buffers as read-only.

Unicode scalar values are deliberately different from user-perceived grapheme clusters.
For example, a combining mark occupies its own position.
Grapheme segmentation and normalization should be added as a separate text layer rather than changing the meaning
of these foundational operations.

## Total operations

Operations whose inputs may be invalid report that fact in their types:

```zydeco
string/get          : String -> Int64 -> Ret (Option Char)
string/split_at     : String -> Int64 -> Ret (Option (String * String))
string/parse_int    : String -> Ret (Option Int64)
char/from_codepoint : Int64 -> Ret (Option Char)
bytes/get           : Bytes -> Int64 -> Ret (Option UInt8)
bytes/slice         : Bytes -> Int64 -> Int64 -> Ret (Option Bytes)
list/get            : forall (A : VType) . List A -> Int64 -> Ret (Option A)
```

`bytes/get` returns the octet at a position as a `UInt8`, the type an octet already is.
`bytes/singleton` is the dual construction and stays total: its `UInt8` parameter makes every input valid,
so no branch is needed.
`bytes/slice` takes a start and a length rather than two indices,
so a window names the same things a pointer-and-length foreign call would.
An empty window at the end of a buffer is valid; negative components, overlong windows,
and positions past the end report `none`.
Backends may back slices by shared storage, and structural equality never observes that sharing.

The Builtin forms implement these results as computation-polymorphic branches.
The public library reifies a successful branch with `option/some` and a failed branch with `option/none`.
Neither backend has a hidden sentinel, and malformed input does not panic the host runtime.

The integer types are `Int8`, `Int16`, `Int32`, `Int64`, `UInt8`, `UInt16`, `UInt32`, and `UInt64`.
Their representations and arithmetic domains correspond directly to Rust's `i8` through `i64` and `u8` through `u64`;
arithmetic wraps at the selected width, and signed and unsigned comparisons remain distinct.
Integer division and remainder are not yet wrapped in checked operations.
The generic numeric capability layer deliberately excludes them;
a future checked-arithmetic capability should make their failure behavior explicit.

`Float32` and `Float64` are IEEE-754 binary32 and binary64 values backed by Rust's `f32` and `f64`.
Decimal and scientific literals use an expected `Float32` or `Float64` type and default to `Float64` otherwise.
The float modules provide arithmetic, comparisons, negation, and shortest round-trippable decimal rendering.
Division by zero, infinities, signed zero, and NaN follow IEEE-754 behavior.
In particular, every ordered comparison with NaN is false, while `float32/ne` and `float64/ne` report true.

The `dictionaries` module contains one explicit capability dictionary for each fixed-width numeric representation,
naming it after its width, such as `int64_dictionary`.
Each dictionary nests additive, multiplicative, equality, and ordering capabilities.
Generic functions accept these dictionaries as ordinary arguments;
the standard library does not perform implicit instance search.

The exact host-facing operations live directly in the Builtin contract's `numeric` group, one module per width.
Their comparisons select one of two computation continuations directly,
avoiding a dependency on the library's `Bool` representation.
The width-specific modules in the public package reify those branches as `Bool` and add derived helpers.

## Public modules

- `bool`: constants, logical connectives, equality, and conditional elimination.
- `option`: construction, elimination, mapping, chaining, defaults, and zipping.
- `result`: successful and failed results, elimination, mapping, chaining, defaults, and predicates.
- `list`: construction, right and left folds, append, map, reverse, length, safe indexing, head, and tail.
- `dictionaries`: one explicitly passed capability dictionary per numeric representation.
- `int8` through `int64` and `uint8` through `uint64`: arithmetic, complete comparisons,
  successor/predecessor, wrapping negation, extrema, and string rendering.
- `float32` and `float64`: IEEE-754 arithmetic, comparisons, negation, and string rendering.
- `char`: UTF-8 text rendering and checked Unicode codepoint conversion.
- `string`: scalar-aware observation, safe decomposition, character-list conversion, concatenation, and parsing.
- `bytes`: immutable octet buffers with indexed access, sharing slices, singleton construction,
  structural equality, lexicographic order, list conversion, concatenation, UTF-8 encoding, and checked UTF-8 decoding.
- `io`: shared byte-stream reads and writes, flushing, closing, and structured I/O errors.
- `fs`: typed paths, file-backed capabilities, and whole-file byte and UTF-8 text operations.
- `stdio`: standard stream capabilities and UTF-8 terminal conveniences built from `io` operations.
- `process`: process arguments, randomness, successful halt, panic, and explicit exit.

Filesystem contents are bytes by default. Text conveniences explicitly validate or produce UTF-8,
and every fallible operation reports `Result A IoError` to its `OS` continuation.
EOF is represented as `Option` by line reads; it is not conflated with an empty line or an I/O failure.
The full rationale and lifecycle contract are documented
in [`docs/proposals/filesystem.md`](../../docs/proposals/filesystem.md).

The topic files are independently importable value functions.
`std.zy` is the composition root used by most programs; its public package carries the library-defined types
and modules, while the Builtin contract remains the home of every host type and capability.
Its public record nests one sub-record per topic, and its sealed type is synthesized from the final `pack` introduction,
so no restated contract sits between the implementation and its consumers.
Consumers still select individual modules and types directly,
such as `let make_std ~> (/option; /process) = builtin in`, because slash projection searches the nested structure.
Here `~>` is a view pattern over the imported value function; the function itself is an ordinary value.
