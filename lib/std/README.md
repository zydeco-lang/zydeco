# Zydeco Standard Library

The standard library has two boundaries.
[`builtin.zy`](builtin.zy) is the typed contract between Zydeco programs and the host runtime.
Its operations expose representation-independent observations and effects,
but never construct library-defined `Bool`, `Option`, `Result`, or `List` values.
[`std.zyi`](std.zyi) annotates the public package independently from its implementation.
[`std.zy`](std.zy) applies the ordinary Zydeco modules in this directory, derives higher-level operations,
and assembles a value of that package type. Topic implementations use the same adjacent `.zy`/`.zyi` pairing.
Reusable package-type constructors live in `.type.zy` sources and are imported by both leaf companions and
aggregate signatures, so the contracts have one definition even when several boundaries expose them.
Modules whose composed package adds operations on top of an independently checkable leaf split the type into a
`*-core.type.zy` leaf contract and an extended `.type.zy` package type.
Signature and type files bind `VType` and `CType` once at the top of the file and use those aliases in
every classifier below.

This separation keeps algebraic data in the language.
The interpreter and native runtime only need to agree on the small Builtin ABI, while the files under `data/`
and the derived operations in the topic packages remain ordinary Zydeco code.

## Source layout

The files at the root of this directory are composition boundaries, not implementation buckets:

```text
builtin.zy                 complete host ABI and shared system witness telescope
std.type.zy                reusable public `Std` package-type constructor
std.zyi                    companion annotation for `std.zy`
std.zy                     wiring for the public package

builtin/core.zy            CBPV kinds and constructors
builtin/intrinsic/*.zy     one canonical source term for each compiler intrinsic
builtin/representations.zy fixed representation packages
builtin/numeric/*.zy       exact-width primitive operations
builtin/text/*.zy          Char, String, and Bytes host operations
builtin/system/*.zy        I/O, filesystem, streams, arguments, randomness, process

data/*.zy                  Bool, Option, Result, List, and their composed package
numeric/{integer,float}.zy explicitly polymorphic derived numeric builders
text/package.zy            cross-representation text operations
system/{types,package}.zy  shared system data and capability-preserving assembly
control/*.zy               monadic basis, State, Exception, and their combination

**/*.zyi                   optional whole-file annotations beside value implementations
**/*.type.zy               reusable type terms imported by companions and aggregate types
**/*-core.type.zy          leaf module contract shared by a `.zyi` and its extended `.type.zy`
```

Leaf sources are independently checkable package values or package functions. Topic package files assemble values
whose operations genuinely share several leaves. In particular, the public system package keeps one opening for
`Reader`, `Writer`, and `OS`; splitting that opening would give related I/O operations incompatible abstract types.
No compatibility forwarding files remain at the old flat paths.

## Builtin packages

The host contract is one launcher-supplied value divided into structural dependency groups:

- `core`: `VType`, `CType`, `Thk`, `Ret`, and `Unit`.
- `representations`: manifest `Scalar` packages for `i8` through `i64`, `u8` through `u64`, `f32`, `f64`,
  `char`, `string`, and `bytes`.
- `numeric`: exact-width arithmetic, branch comparisons, and rendering, with a manifest associated `Scalar`.
- `text`: operations crossing `Char`, `String`, `Bytes`, and `Int64`.
- `system`: abstract `Reader`, `Writer`, and `OS` capabilities plus I/O, filesystem, standard stream,
  argument, randomness, and process operations.

Fixed representations are compiler-canonical intrinsics, so independent packages that select `i64` share one
`Int64` identity. Only the runtime-owned system capabilities are generative existential types. Consumers first
project the groups they need and then open those narrower packages; the composition root retains the complete
Builtin value when it must pass the dependency onward. See
[Modular primitive packages](../../docs/proposals/primitive-packages.md) for the design and usage examples.
Each intrinsic splice is authored once under `builtin/intrinsic/`; contracts elsewhere import those canonical
terms rather than spelling compiler metadata again.
Builtin leaves bind the intrinsic kinds and constructors they use at the top of the file, so their classifiers
read as ordinary type expressions.

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
list/get            : forall (A : VType) . List A -> Int64 -> Ret (Option A)
```

The Builtin forms implement these results as computation-polymorphic branches.
The public library reifies a successful branch with `option/some` and a failed branch with `option/none`.
Neither backend has a hidden sentinel, and malformed input does not panic the host runtime.

The integer types are `Int8`, `Int16`, `Int32`, `Int64`, `UInt8`, `UInt16`, `UInt32`, and `UInt64`.
Their representations and arithmetic domains correspond directly to Rust's `i8` through `i64` and `u8` through
`u64`; arithmetic wraps at the selected width, and signed and unsigned comparisons remain distinct.
Integer division and remainder are not yet wrapped in checked operations.
The generic numeric capability layer deliberately excludes them;
a future checked-arithmetic capability should make their failure behavior explicit.

`Float32` and `Float64` are IEEE-754 binary32 and binary64 values backed by Rust's `f32` and `f64`.
Decimal and scientific literals use an expected `Float32` or `Float64` type and default to `Float64` otherwise.
The float modules provide arithmetic, comparisons, negation, and shortest round-trippable decimal rendering.
Division by zero, infinities, signed zero, and NaN follow IEEE-754 behavior.
In particular, every ordered comparison with NaN is false, while `float32/ne` and `float64/ne` report true.

The `numeric` module contains one explicit dictionary for each fixed-width numeric representation.
Each instance discloses its `Scalar` carrier through a manifest type and nests additive,
multiplicative, equality, and ordering capabilities.
Generic functions accept these dictionaries as ordinary arguments;
the standard library does not perform implicit instance search.

The `primitives` module exposes the exact host-facing operations for low-level code under the corresponding Rust
representation names: `i8` through `i64`, `u8` through `u64`, `f32`, and `f64`.
Its comparisons select one of two computation continuations directly, avoiding a dependency on the library's
`Bool` representation. The width-specific top-level modules reify those branches as `Bool` and add derived helpers.

## Public modules

- `bool`: constants, logical connectives, equality, and conditional elimination.
- `option`: construction, elimination, mapping, chaining, defaults, and zipping.
- `result`: successful and failed results, elimination, mapping, chaining, defaults, and predicates.
- `list`: construction, right and left folds, append, map, reverse, length, safe indexing, head, and tail.
- `numeric`: manifest instances for all ten numeric representations and explicitly passed capability dictionaries.
- `primitives`: exact-width arithmetic, branch comparisons, and rendering under Rust representation names.
- `int8` through `int64` and `uint8` through `uint64`: arithmetic, complete comparisons,
  successor/predecessor, wrapping negation, extrema, and string rendering.
- `float32` and `float64`: IEEE-754 arithmetic, comparisons, negation, and string rendering.
- `char`: UTF-8 text rendering and checked Unicode codepoint conversion.
- `string`: scalar-aware observation, safe decomposition, character-list conversion, concatenation, and parsing.
- `bytes`: immutable octet buffers, concatenation, length, UTF-8 encoding, and checked UTF-8 decoding.
- `io`: shared byte-stream reads and writes, flushing, closing, and structured I/O errors.
- `fs`: typed paths, file-backed capabilities, and whole-file byte and UTF-8 text operations.
- `stdio`: standard stream capabilities and UTF-8 terminal conveniences built from `io` operations.
- `process`: process arguments, randomness, successful halt, panic, and explicit exit.

Filesystem contents are bytes by default. Text conveniences explicitly validate or produce UTF-8,
and every fallible operation reports `Result A IoError` to its `OS` continuation.
EOF is represented as `Option` by line reads; it is not conflated with an empty line or an I/O failure.
The full rationale and lifecycle contract are documented
in [`docs/proposals/filesystem.md`](../../docs/proposals/filesystem.md).

The component files are independently importable pure package functions.
`std.zy` is the composition root used by most programs and re-exports their abstract type witnesses in one package.
Its companion keeps the exposed contract reviewable without reading the implementation machinery, while imported
`.type.zy` constructors let topic and aggregate signatures share the same definitions.
