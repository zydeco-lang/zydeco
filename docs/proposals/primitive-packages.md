# Modular primitive packages

The host boundary has two different kinds of primitive type. Fixed representations such as `Int8` and
`Float64` denote a stable machine-level choice, while `Reader`, `Writer`, and `OS` describe capabilities owned
by one runtime provider. Treating both kinds as abstract fields in one Builtin telescope made every consumer
open a large package and incorrectly gave fixed representations generative identities.

Builtin now exposes five structural packages:

```text
core             VType CType Thk Ret Unit
representations  i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 char string bytes
numeric          int8 int16 int32 int64 uint8 uint16 uint32 uint64 float32 float64
text             char string bytes
system           Reader Writer OS io fs stdio args random process
```

This boundary follows one general rule: a representation is compiler-canonical when its layout and primitive
semantics are part of the language ABI; a capability remains abstract when its identity belongs to a particular
provider. The rule explains both the package layout and the type-checker treatment, instead of requiring one
exception per primitive.

## Core structure

The `core` package publishes the CBPV universes and constructors used to state every other interface:

```zydeco
let (/VType; /CType; /Thk; /Ret; /Unit) = core in
...
```

`VType` and `CType` also occur as leading manifest kind components of the complete host contract. The current
checker needs those universes while checking the rest of the dependent package. `core` is the consumer-facing
view; the leading components are an implementation requirement of the provider telescope.

`Thk`, `Ret`, and `Unit` are compiler intrinsics with applicative identities. `core` gives consumers their primary
package view. Independently checkable Builtin leaves may repeat the intrinsic spellings in their own contracts;
each spelling resolves to the same compiler-canonical identity.

## Fixed representations

Each child of `representations` is a small manifest package whose field carries the public name of the
representation it discloses:

```zydeco
let (/Int64) = representations/i64 in
let (/String) = representations/string in
...
```

The manifest label names the disclosed type, so a consumer binds that type through an ordinary projection pun
rather than renaming a role label at every open. The package paths state the corresponding Rust
representation directly:

| Representation package | Zydeco type | Rust semantic representation |
| --- | --- | --- |
| `i8`, `i16`, `i32`, `i64` | `Int8`, `Int16`, `Int32`, `Int64` | `i8`, `i16`, `i32`, `i64` |
| `u8`, `u16`, `u32`, `u64` | `UInt8`, `UInt16`, `UInt32`, `UInt64` | `u8`, `u16`, `u32`, `u64` |
| `f32`, `f64` | `Float32`, `Float64` | `f32`, `f64` |
| `char` | `Char` | `char` |
| `string` | `String` | valid UTF-8 Rust `String`/`str` semantics |
| `bytes` | `Bytes` | Rust byte slices and owned `Vec<u8>` buffers |

The checked literal forms retain the corresponding Rust-width payload. Native floating values preserve the
`f32` or `f64` bit pattern, `Char` is one Unicode scalar value, native `String` values are Rust `String`s, and
native `Bytes` values are Rust `Vec<u8>` buffers. The interpreter may share immutable strings and bytes, but it
preserves the same observable representation and operations.

These types use typed `@[intrinsic(i8)]` through `@[intrinsic(bytes)]` splices. Repeating a splice denotes the
same canonical type, including across independently assembled packages. Fixed primitives therefore need no
`@[builtin(int64)]`-style abstract witness, and those fixed-type roles are retired. The only remaining Builtin
type roles are `reader`, `writer`, and `os`.

## Numeric operation packages

The `numeric` package separates operations from representation names. Every width-specific package discloses
its carrier under that carrier's public name and contains only operations for that carrier:

```zydeco
let (#Int64 = Int64, int64) = numeric/int64 in
do sum <- ! (int64/add) left right;
! (int64/lt) Result sum limit when_true when_false
```

A concrete manifest label lets any consumer bind the disclosed type under its established name, which is the
name the standard library itself re-exports. An earlier design shared one `#Scalar` label across all widths so
that a single generic pattern could open any package; no generic consumer of that shape existed, while every
concrete consumer paid a renaming step at each open, so the shared label was retired. Low-level code still
binds the manifest to an exact Rust-shaped type, and arithmetic and comparison packages
can evolve independently of text conversion, I/O, and unrelated widths.

Signed integers expose `Int8` through `Int64`, unsigned integers expose `UInt8` through `UInt64`, and floats
expose `Float32` and `Float64`. There are no width-erasing `Int` or `Float` types. Unsuffixed integer and float
literals still default to `Int64` and `Float64`; an expected primitive type selects another exact width.

## Text operations

`text` owns operations that cross fixed representation boundaries:

- `char` renders a `Char`, observes its codepoint, and performs checked codepoint construction.
- `string` provides UTF-8 scalar and byte observations, indexing, splitting, equality, concatenation, and
  integer parsing.
- `bytes` provides immutable buffer construction, concatenation, observation, UTF-8 encoding, and checked
  decoding.

The types themselves remain in `representations`. A source that stores a `String` but performs no text operation
can open only `representations/string`; a source that calls operations additionally selects `text/string`.

## System capabilities

`system` contains the runtime-owned identities and all operations that depend on them:

```zydeco
let (/Reader; /Writer; /OS; /io; /fs; /stdio; /args; /random; /process) = system in
...
```

`Reader`, `Writer`, and `OS` remain abstract at the host-provider boundary. Opening a single `system` package
ensures that `fs/open_reader`, `io/read`, and `stdio/stdin` share exactly the same capability identities. Fixed
types mentioned by these operations, such as `String`, `Bytes`, and `Int64`, refer directly to their canonical
primitive identities and do not have to be threaded through the system telescope.

This is the intended abstraction boundary: fixed data representation is applicative, while owned resources and
effect execution are generative.

## Composition and erasure

The launcher still supplies one complete Builtin value. Its fields are ordinary named products, so a consumer
selects only the structural group it needs and then opens that group with an ordinary pattern. The standard
library accepts the complete value for composition, while each component source projects its narrower
dependency.

Manifest kind and type fields erase during elaboration. Nested products lower to the existing tuple layouts, and
host operations lower from their typed roles as before. No runtime type tag, dynamic field table, module object,
or new calling convention is introduced.

Every package product has an explicit `Unit` tail, including nested groups. The interpreter and Stack IR derive
the physical Builtin value recursively from the checked product shape; an explicit tail prevents the last nested
group from being flattened into its parent. This is a representation invariant of the current product encoding,
not an extra source-level capability.

## Dependency examples

A pure integer consumer needs only three groups:

```zydeco
param (
  (/core; /numeric) :
  @[import("builtin.zy")] _
) in
let (/Ret) = core in
let (#Int64 = Int64, int64) = numeric/int64 in
...
```

An executable can select system services without opening unrelated numeric or text operations:

```zydeco
param (
  (/core; /representations; /system) :
  @[import("builtin.zy")] _
) in
let (/Thk) = core in
let (/String) = representations/string in
let (/OS; /stdio; /process) = system in
...
```

The complete package remains available through an ordinary whole-value alias when a composition root must pass
it to another library:

```zydeco
param ((/core; /system; builtin) : @[import("builtin.zy")] _) in
let std = make_std builtin in
...
```
