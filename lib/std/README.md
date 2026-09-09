# Zydeco Standard Library

## Start with the types

Select the foundational kinds and types directly from the Builtin contract:

```zydeco
param (/VType; /CType; /Ret; /Thk; /Int64; /Float32; /Float64) : @(import("lib/std/builtin.zy")) in
...
```

The path above is relative to a source file in the repository root.
The contract's surface carries the compiler-canonical kinds and types as manifest fields,
so one field search reaches every public name and every selection shares one type identity.
A pure library function states its interface with just this parameter:

```zydeco
param (/Ret; /Int64) : @(import("lib/std/builtin.zy")) in
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
through them: `param (/stdio; /process) : @(import("lib/std/builtin.zy")) in` selects two operations
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
The [language reference](../../docs/references/language.md#13-primitive-values-and-capabilities) defines primitive
identity; the [package design](../../docs/proposals/package-modularization.md#primitive-identity-and-package-boundaries)
explains how it determines these boundaries.
Compiler intrinsics are spliced inline as `@(intrinsic(name))` wherever a contract needs the canonical term,
so no one-line indirection files sit between type expressions and the compiler metadata they name.
Builtin leaves bind the intrinsic kinds and constructors they use at the top of the file,
so their classifiers read as ordinary type expressions.

## Package composition

Select related types and operations in one projection group so they share one abstract opening.
A whole-package alias passes that same dependency to a factory:

```zydeco check
param (/Bytes; /Reader; /io; builtin) : @(import("builtin.zy")) in
let make_std = @(import("std.zy")) in
let (/bytes; /fs) = builtin |> make_std in
! bytes/empty
```

Here `Bytes`, `Reader`, and `io` come from one Builtin opening; `builtin` forwards the original package.
Slash selection follows the [language rules](../../docs/references/language.md#9-polymorphism-and-packages),
including nested products and ambiguity.
A view such as `make_std ~> (/bytes; /fs)` combines application and opening.

A final `pack` introduction is useful when an implementation should synthesize its exported type evidence.
Use an explicit existential annotation when the contract must prescribe an abstract payload type;
use a companion `.zyi` source when the contract deserves independent authorship.
Named product values also synthesize their types, so naming fields alone does not require a companion.

Kind fields currently need an annotated package introduction rather than `pack`:

```zydeco check
let VType = @(intrinsic(vtype)) in
let CType = @(intrinsic(ctype)) in
let types =
  pack (= Thk as @(intrinsic(thk)) : CType -> VType)
  where () end
in
((VType, CType, types) :
  exists (VType as @(intrinsic(vtype))) (CType as @(intrinsic(ctype))) . @[typeof] types)
```

The annotation discloses the kind equations, while the inner `pack` supplies a type constructor.
The shared [package design](../../docs/proposals/package-modularization.md) explains when inferred
and authored interfaces serve different maintenance needs.

## Explicit runtime contracts

A value functor is a total package-to-package value function and undergoes static elimination.
A computation functor receives a package through a computation protocol; its thunk may remain dynamically selectable.
The callee body need not be statically known when its signature and the argument's witnesses are available.

For `Sig = exists (X : K) . A X`, explicit adapters connect `pi ((X, x) : Sig) . C X`
and `forall (X : K) . A X -> C X`: they group or separate the same type witness and payload.
The result may depend on `X`, not on the arbitrary runtime value `x`.
For example, both interfaces can expose codata methods:

```zydeco check
param (/VType; /Thk; /Ret; /Unit) : @(import("builtin.zy")) in
let Box = exists (X : VType) . X in
let Methods (X : VType) = codata
  | .get : Ret X
  | .replace : X -> Ret X
end in
let Curried = forall (X : VType) . X -> Methods X in
let Packaged = pi ((X, _) : Box) . Methods X in
let curried : Thk Curried = {
  fn (X : VType) (value : X) => comatch
    | .get => ret value
    | .replace replacement => ret replacement
  end
} in
let packaged : Thk Packaged = {
  fn ((X, value) : Box) => ! curried X value
} in
let restored : Thk Curried = {
  fn (X : VType) (value : X) => ! packaged ((X, value) : Box)
} in
! restored Unit () .get
```

These are explicit adapters, not definitional equality between `PackPi` and `Forall`
or a theorem equating arbitrary effectful protocols.
A package `pi` outside codata opens once for the residual protocol; inside a method arm it opens for that observation.
Moving it changes where payloads and witnesses are shared.

When the provider chooses a hidden type, a polymorphic callback can give the consumer one scoped opening:

```zydeco check
param (/VType; /CType; /Thk; /Ret; /Int64) : @(import("builtin.zy")) in
let Entry = exists (X : VType) . X * Thk (X -> Ret Int64) in
let Hidden = codata
  | .open : forall (R : CType) . Thk (pi ((X, _, _) : Entry) . R) -> R
end in
ret ()
```

The callback receives a value and an operation at the same abstract `X`.
Its result protocol `R` is chosen outside the opening, so the private witness cannot escape through that result.
The corresponding curried callback takes `forall (X : VType) . X -> Thk (X -> Ret Int64) -> R`.
Neither callback type promises purity or single invocation.

## Relative monads and control examples

The [control modules](control) supply `Monad`, `Algebra`, State, Exception, and their combination.
[L11](../../docs/references/language.md#11-relative-monads) owns monadic-block translation.
For delimited control, the repository explores a library encoding with `Kont R A = Thk (A -> R) -> R`,
explicit `reset` and `shift`, and ordinary thunks.
Working examples are [reset/shift with an explicit continuation](../tests/delimcc/reset-shift-k.zy),
[reset/shift with a return interface](../tests/delimcc/reset-shift-r.zy),
and [try/catch](../tests/delimcc/try-catch.zy).
They are examples, not an exported standard-library delimited-control API.

The design motivation is to express control through a relative monad and evaluate
that encoding before adding an ambient continuation primitive to `Ret`.
The historical exploration points to [the HOPE talk](http://maxsnew.com/publications.html#hope22)
and [the delimcc API discussion](https://okmij.org/ftp/continuations/implementations.html#delimcc-paper).
These encodings do not capture native frame tokens;
[native lifetime rules](../../docs/references/compiler.md#c11-native-preparation-activation-frames-and-amd64-emission)
remain a separate implementation contract.

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
`bytes/slice value start length` returns the window `[start, start + length)`.
It shares storage in the interpreter and Wasm host; the native runtime currently copies the window.
Two buffers are equal exactly when their octet sequences are equal;
`bytes/lt` compares buffers lexicographically octet by octet.
Construction from single octets goes through `bytes/singleton`, which is total because every `UInt8` is a valid octet;
the byte-level FFI contract treats borrowed buffers as read-only.

Unicode scalar values are deliberately different from user-perceived grapheme clusters.
For example, a combining mark occupies its own position.
Grapheme segmentation and normalization should be added as a separate text layer rather than changing the meaning
of these foundational operations.

## Byte operation costs

These costs describe the current contiguous-buffer implementations, excluding general allocation/GC overhead.
The [compiler reference](../../docs/references/compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls)
owns backend storage; [text/package.zy](text/package.zy) defines the derived operations.

| Operation | Work |
| --- | --- |
| `length`, `get`, `singleton` | Constant time. |
| `slice` | Constant-time window in the interpreter and Wasm host; native copies the selected length. |
| `eq`, `lt` | At most the shorter buffer's length in byte comparisons, with early exit. |
| `append` | Copies both inputs: O(n + m). |
| `to_list` | O(n) indexed observations and list cells. |
| `from_list` | Repeated append of a singleton to the accumulated tail: O(n²) copied bytes. |
| `concat` | Sum of the lengths copied by the right fold; quadratic for a list of equal-sized chunks. |

A future [memory-backed writer](../../docs/proposals/filesystem.md#memory-backed-writer-and-byte-builder)
would provide incremental construction without changing immutable-byte observations.

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
The exact spelling follows the shared [numeric representation rules](../../DESIGN.md#numeric-representations).
Division by zero, infinities, signed zero, and NaN follow IEEE-754 behavior.
In particular, every ordered comparison with NaN is false, while `float32/ne` and `float64/ne` report true.

The exact host-facing operations live directly in the Builtin contract's `numeric` group, one module per width.
Their comparisons select one of two computation continuations directly,
avoiding a dependency on the library's `Bool` representation.
The width-specific modules in the public package reify those branches as `Bool` and add derived helpers.

## Numeric capabilities and explicit instances

Generic numeric functions receive the operations they need as ordinary arguments.
The public package exports five capability type constructors; their linked definitions give the exact field types:

| Interface | Fields |
| --- | --- |
| [Additive A](numeric/additive.type.zy) | `zero`, `add`, `sub`, `negate` |
| [Multiplicative A](numeric/multiplicative.type.zy) | `one`, `mul` |
| [PartialEquality Bool A](numeric/partial-equality.type.zy) | `eq`, `ne`, returning the supplied `Bool` |
| [PartialOrder Bool A](numeric/partial-order.type.zy) | `equality`, `lt`, `le`, `gt`, `ge` |
| [Numeric Bool A](numeric/numeric.type.zy) | `additive`, `multiplicative`, `order` |

Larger dictionaries contain smaller ones as named fields.
This preserves paths such as `dictionary/additive/add` and `dictionary/order/equality/eq`,
and lets a consumer receive just `Additive A` when it only needs addition.
The `dictionaries` module contains one dictionary per fixed-width representation,
from `int8_dictionary` through `float64_dictionary`.
Generic code explicitly selects and passes one:

```zydeco check
param (/VType; /Ret; /Int64; builtin) : @(import("builtin.zy")) in
let make_std = @(import("std.zy")) in
let (/Additive; /dictionaries) = builtin |> make_std in
let ! twice (A : VType) (operations : Additive A) (value : A) : Ret A =
  ! operations/add value value
in
! twice Int64 (dictionaries/int64_dictionary/additive) 21
```

These interfaces describe operations rather than proving algebraic laws.
For example, IEEE equality is not reflexive on NaN, which motivates `PartialEquality`.
Division, integer remainder, rendering, extrema,
and other representation-specific operations stay in the width modules. The numeric layer inherits the
[literal and conversion rules](../../docs/references/language.md#13-primitive-values-and-capabilities).

A library can also expose the selected carrier together with its dictionary in a manifest package.
The following complete example names the disclosed type `Int64`, renames it to `Carrier` when opening,
and checks that the operations use that same carrier:

```zydeco check
param (/VType; builtin) : @(import("builtin.zy")) in
let make_std = @(import("std.zy")) in
let (/Bool; /Numeric; /dictionaries) = builtin |> make_std in
let int64_instance =
  pack (= Int64 as @(intrinsic(i64)) : VType)
  where #operations = dictionaries/int64_dictionary end
in
let (/Int64 = Carrier; /operations) = int64_instance in
let _ : Numeric Bool Carrier = operations in
! operations/additive/add (21 : Carrier) 21
```

The [manifest type rules](../../docs/references/language.md#9-polymorphism-and-packages) supply the disclosed equation;
[package selection](../../docs/references/language.md#9-polymorphism-and-packages) governs the shared opening.
Naming a manifest field after its carrier avoids imposing a generic role label on each consumer.
When exporting several instances, use distinctive value names such as `int64_instance` and `float32_instance`
so their selection is unambiguous.

Selection remains explicit value flow: lexical bindings and arguments determine which dictionary is used.
Several implementations for one carrier can coexist without global instance search or coherence checking.
A wrapper can carry additional abstract or manifest type fields under the same package scope rules.
Its static components obey the [static elimination contract](../../docs/references/language.md#10-static-elimination);
ordinary dictionary thunks may remain at runtime.
The [runtime contract design](README.md#explicit-runtime-contracts) describes adapters
when operations must be dynamically selectable.

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
The [stream guide](#streams-and-files) gives the operations and lifecycle contract;
[filesystem design](../../docs/proposals/filesystem.md) retains the rationale and extension questions.

The topic files are independently importable value functions.
`std.zy` is the composition root used by most programs; its public package carries the library-defined types
and modules, while the Builtin contract remains the home of every host type and capability.
Its public record nests one sub-record per topic, and its sealed type is synthesized from the final `pack` introduction,
so no restated contract sits between the implementation and its consumers.
Consumers still select individual modules and types directly,
such as `let make_std ~> (/option; /process) = builtin in`, because slash projection searches the nested structure.
Here `~>` is a view pattern over the imported value function; the function itself is an ordinary value.

## Streams and files

The shared `io` layer is byte-oriented.
The following computation protocols apply after forcing the exported operation thunks.
Fallible resource operations use the standard library's continuation-passing `OS` convention:

```text
io/read       : Reader -> Int64 -> Thk (Result Bytes IoError -> OS) -> OS
io/read_line  : Reader -> Thk (Result (Option Bytes) IoError -> OS) -> OS
io/read_all   : Reader -> Thk (Result Bytes IoError -> OS) -> OS
io/write_all  : Writer -> Bytes -> Thk (Result Unit IoError -> OS) -> OS
io/flush      : Writer -> Thk (Result Unit IoError -> OS) -> OS
io/close_reader : Reader -> Thk (Result Unit IoError -> OS) -> OS
io/close_writer : Writer -> Thk (Result Unit IoError -> OS) -> OS
```

`read` rejects negative byte counts. `write_all` either writes the complete buffer or reports an error;
exposing a partial-write primitive would force every caller to duplicate the same retry loop.

The `fs` module uses a typed `Path` wrapper around a UTF-8 `String`.
This wrapper prevents ordinary text from being passed accidentally where the host expects a path,
while preserving the current language's portable UTF-8 model.
It does not claim that every native path can be represented on every operating system;
a future platform-specific path representation can replace the wrapper without changing stream operations.

```text
fs/path          : String -> Ret Path
fs/path_string   : Path -> Ret String
fs/open_reader   : Path -> Thk (Result Reader IoError -> OS) -> OS
fs/create_writer : Path -> Thk (Result Writer IoError -> OS) -> OS
fs/append_writer : Path -> Thk (Result Writer IoError -> OS) -> OS
fs/read_bytes    : Path -> Thk (Result Bytes IoError -> OS) -> OS
fs/read_text     : Path -> Thk (Result String IoError -> OS) -> OS
fs/write_bytes   : Path -> Bytes -> Thk (Result Unit IoError -> OS) -> OS
fs/write_text    : Path -> String -> Thk (Result Unit IoError -> OS) -> OS
```

`create_writer` creates a missing file and truncates an existing one.
`append_writer` creates a missing file and places every write at the end.
Whole-file helpers open, operate, and close internally.
If the data operation fails, that error wins; otherwise a close error is returned.

The `stdio` module exposes `stdin`, `stdout`, and `stderr` as capabilities.
Its `read_line` checks UTF-8 and returns `Result (Option String) IoError`; its `write`, `write_line`,
and error-stream variants encode `String` to `Bytes`, delegate to `io/write_all`, and preserve write or flush failures.

`IoError` carries a stable kind and a display message.
The kinds are `NotFound`, `PermissionDenied`, `AlreadyExists`, `InvalidInput`, `InvalidData`,
`BrokenPipe`, `Closed`, and `Other`; branch on the kind rather than parsing the message.
A read at EOF returns empty bytes; `read_line` returns `Ok None` instead.
An empty line is `Ok (Some empty)`, and a final line without a newline is still returned.
Line reads strip a trailing LF and its immediately preceding CR; arbitrary byte reads preserve their contents.
A zero-byte read also returns empty bytes, so it cannot by itself establish EOF.

Handles are alias-visible capabilities.
Closing a file-backed reader or writer closes it for every alias, and subsequent operations report `Closed`.
Reserved standard streams remain available after ordinary close requests.
The current streams block. [Capability extensions](../../docs/proposals/filesystem.md) cover buffering,
seeking, memory-backed writers, and future asynchronous protocols.
