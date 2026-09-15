# Zydeco Standard Library

## Start with the types

Select the foundational kinds and types directly from the Builtin contract:

```zydeco
param (/VType; /CType; /Ret; /Thk; /Int; /Float32; /Float64) : @(import(std/builtin)) in
...
```

Run CLI commands from the repository root to use its automatically detected package catalog.
The contract's surface carries the compiler-canonical kinds and types as manifest fields,
so one field search reaches every public name and every selection shares one type identity.
A pure library function states its interface with just this parameter:

```zydeco
param (/Ret; /Int) : @(import(std/builtin)) in
fn (value : Int) => (ret value : Ret Int)
```

The available surface names are:

| Family | Names |
| --- | --- |
| CBPV | `VType`, `CType`, `Thk`, `Ret`, `Unit` |
| Signed integers | `Int8`, `Int16`, `Int32`, `Int64`, `Int` |
| Unsigned integers | `UInt8`, `UInt16`, `UInt32`, `UInt64`, `UInt` |
| Floating point | `Float32`, `Float64` |
| Text | `Char`, `String` |
| Capabilities | `Addr`, `Reader`, `Writer`, `OS` |

Host operations live in the same contract under the `numeric`, `text`, and `system` groups,
and the search descends through them: `param (/stdio; /process) : @(import(std/builtin)) in` selects two operations
from the `system` group without naming it.
The [integer example](../tests/std/minimal.zy) selects `Int` and `Ret` this way,
then uses the assembled standard-library value for arithmetic.

## Library boundaries

Runtime operations have two boundaries.
[`builtin.zy`](builtin.zy) is the typed contract between Zydeco programs and the host runtime.
Its operations expose representation-independent observations and effects,
but never construct library-defined `Bool`, `Option`, `Result`, `List`, or `Bytes` values.
[`std.zy`](std.zy) applies the factories supplied by topic packages and assembles the public packed value,
whose sealed type its final `pack` introduction synthesizes.

Each topic owns exactly one implementation: `data`, `text`, `system`, and `numeric` each provide `package.zy`,
sealed by a final `pack` introduction whose existential type the checker synthesizes,
so importers splice the implementation without a companion annotation.
Implementations annotate their parameters in place: the Builtin group through `builtin.zy`,
and the shared algebraic base through `data/package.type.zy`.
The implementation defines its topic's data types and operations in one dependency-scheduled block,
so derived operations sit next to the types they observe and no per-module contract split remains for those topics.
`data/package.type.zy` names that shared base type directly, carrying the data topic's existential witnesses
and module telescopes in one declaration.
Type files bind `VType` and `CType` once at the top of the file and use those aliases in every classifier below.

This separation keeps algebraic data in the language.
The interpreter and native runtime only need to agree on the small Builtin ABI,
while the files under `data/` and the derived operations in the topic packages remain ordinary Zydeco code.

`system/arguments.zy` builds a lazy `fold` from the Builtin `args/at` lookup.
Apply it to the Builtin packed value directly when a list is unnecessary; `process/arg_list` uses the same builder.
Callers of the former Builtin `args/fold` should instantiate this builder and call its `fold` field.
Tails are ordinary reusable computations, and host runtimes need no special closure implementation.
[Argument semantics](../../docs/references/language.md#13-primitive-values-and-capabilities) specify lookup failures
and repeated forcing.

The optional `memory/package.zy` builder is imported directly, like the control libraries.
It supplies fixed and dynamic layouts, typed pointers, and explicit allocation operations.
See [explicit storage](#explicit-storage).

## Source packages and tests

[`std.zy`](std.zy) declares the whole-file package `std`.
Builtin is `std/builtin`; topic entries are `std/data`, `std/text`, `std/numeric`, `std/system`, and `std/memory`.
These names identify source terms; fields and factory applications remain ordinary language structure.
The [source-package rules](../../docs/references/language.md#source-packages) govern selection and relationships.

The repository's [workspace.zy](../../workspace.zy) explicitly includes entry files and the std test tree.
From the repository root:

```sh
zydeco show
zydeco check std
zydeco test std
zydeco test std -t all
```

Within that catalog, use `@(import(std))` or `@(import(std/memory))`.
Explicit file imports remain valid without a catalog.
Self-contained tests declare `@[package(test(of(std)))]` on their roots, including tests of optional libraries.
Adding a test in the declared scope requires no edit to the library or catalog.

The package runner defaults to the interpreter and uses empty stdin and arguments on every selected backend.
`arg-list.zy`, `filesystem.zy`, and `read-line-as-int.zy` need supplied arguments,
fixtures, or input, so they have no standalone test annotation.
Keep the [Rust registrations](../../lang/tests/tests/std.rs) for fixture setup;
that harness shares the execution runner and explicitly selects the repository catalog too.

Use std from the same compiler release or revision, preserving its internal relative imports.
There is no separate version resolver or hosted registry.

## Source layout

The files at the root of this directory define the public entry points:

```text
builtin.zy                 complete host ABI: surface kinds and types, operation groups
std.zy                     wiring for the public packed value

builtin/numeric/*.zy       primitive numeric operations
builtin/text/*.zy          Char and String host operations
builtin/system/*.zy        I/O, filesystem, streams, arguments, randomness, process

data/package.zy            Bool, Option, Result, List, and every derived operation
data/package.type.zy       DataPackage existential wrapper with the module telescopes
data/bool.type.zy          BoolModule telescope shared with the numeric builders

numeric/{integer,float}.zy explicitly polymorphic derived numeric builders
numeric/package.zy         the twelve numeric modules and their capability dictionaries
numeric/codecs.zy          source byte codecs over scalar memory leaves

text/bytes.zy              abstract immutable byte sequences over retained raw allocations
text/bytes.type.zy         shared byte-type witness and primitive-free sequence API
text/package.zy            text conveniences and byte collection operations

memory/package.zy          manual-memory interface and shared pointer/state witnesses
memory/types.zy            erased Ptr<L,S> abstraction and state markers
memory/storage.zy          sealed fixed/runtime geometry
memory/codecs.zy           independent fixed/runtime scalar codecs
memory/layout.zy           static layout plans and typed destination operations
memory/dynamic-layout.zy   explicitly dynamic placement and realization
memory/size.zy             checked static size and alignment calculations
memory/shape.zy            inspectable scalar widths and product placement
memory/buffer.zy           incremental byte construction, snapshots, retention, release
memory/allocation.zy       explicit allocator services
memory/slice.zy            counted typed pointers and checked index arithmetic
memory/native.zy           raw memory adapter and source fault values
memory/representation.type.zy  witness with separate storage and codec components

system/package.zy          system data types and capability-preserving assembly

control/*.zy               monadic basis, State, Exception, and their combination

**/*.type.zy               reusable type terms imported by implementations and companions
```

Topic implementations are independently checkable module factories.
The public packed value keeps one opening for `Reader`, `Writer`, and `OS`;
splitting that opening would give related I/O operations incompatible abstract types.
No compatibility forwarding files remain at the old flat paths.

## Builtin interface

The launcher supplies one packed value implementing the [Builtin contract](builtin.zy).
The signature begins with manifest fields for the CBPV kinds, constructors, and fixed-representation types,
followed by abstract witnesses for runtime-owned capabilities.
Its value payload groups the runtime operations:

- Surface: `VType`, `CType`, `Thk`, `Ret`, `Unit`, the twelve numeric types, `Char`,
  `String`, then abstract `Addr`, `Reader`, `Writer`, and `OS`.
- `numeric`: typed arithmetic, branch comparisons, rendering, and checked scalar loads/stores.
- `text`: operations crossing `Char`, `String`, and `Int`.
- `system`: the re-exposed capabilities plus checked memory, I/O, filesystem, standard stream, argument,
  randomness, and process operations.

Each public name is unique across the complete packed value, so one field search reaches kinds, types,
operations, and capabilities alike, and every selection shares the contract's identities.
Fixed representations are compiler-canonical intrinsics, so independent selections share one `Int` identity.
Opening the runtime-owned system capabilities introduces fresh abstract witnesses.
A composition root that must pass the dependency onward keeps the whole-alias `builtin` beside its selections.
The [primitive and capability rules](../../docs/references/language.md#13-primitive-values-and-capabilities)
define these identity boundaries.
Compiler intrinsics are spliced inline as `@(intrinsic(name))` wherever a contract needs the canonical term,
so no one-line indirection files sit between type expressions and the compiler metadata they name.
Builtin leaves bind the intrinsic kinds and constructors they use at the top of the file,
so their classifiers read as ordinary type expressions.

## Packed value composition

Select related types and operations in one projection group so they share one abstract opening.
A whole-value alias passes that same dependency to a factory:

```zydeco check
param (/Reader; /io; builtin) : @(import("builtin.zy")) in
let make_std = @(import("std.zy")) in
let (/Bytes; /bytes; /fs) = builtin |> make_std in
do value <- ! bytes/empty;
! bytes/length value
```

Here `Reader` and primitive `io` come from one Builtin opening; `builtin` forwards that packed value.
`Bytes`, convenient `bytes`, and `fs` share the std opening.
For low-level memory composition, instantiate `text/bytes.zy` once and pass that packed value to dependent builders.
Slash selection follows the [language rules](../../docs/references/language.md#9-polymorphism-and-packed-values),
including nested products and ambiguity.
A view such as `make_std ~> (/bytes; /fs)` combines application and opening.

A final `pack` introduction is useful when an implementation should synthesize its exported type evidence.
Use an explicit existential annotation when the contract must prescribe an abstract payload type;
use a companion `.zyi` source when the contract deserves independent authorship.
Named product values also synthesize their types, so naming fields alone does not require a companion.

Kind fields currently need an annotated packed value introduction rather than `pack`:

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
The [module-interface rules](../../docs/references/language.md#module-interfaces-and-shared-openings) explain
when inferred and authored interfaces serve different maintenance needs.

## Explicit runtime contracts

A value functor is a total function between packed values and undergoes static elimination.
A computation functor receives a packed value through a computation protocol;
its thunk may remain dynamically selectable.
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
A witness-dependent `pi` outside codata opens once for the residual protocol;
inside a method arm it opens for that observation.
Moving it changes where payloads and witnesses are shared.

When the provider chooses a hidden type, a polymorphic callback can give the consumer one scoped opening:

```zydeco check
param (/VType; /CType; /Thk; /Ret; /Int) : @(import("builtin.zy")) in
let Entry = exists (X : VType) . X * Thk (X -> Ret Int) in
let Hidden = codata
  | .open : forall (R : CType) . Thk (pi ((X, _, _) : Entry) . R) -> R
end in
ret ()
```

The callback receives a value and an operation at the same abstract `X`.
Its result protocol `R` is chosen outside the opening, so the private witness cannot escape through that result.
The corresponding curried callback takes `forall (X : VType) . X -> Thk (X -> Ret Int) -> R`.
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

`Bytes` is an abstract source-defined immutable sequence of octets with no encoding attached.
[text/bytes.zy](text/bytes.zy) supplies the type and algorithms using explicitly retained memory;
[the owning design](../../docs/references/language.md#immutable-owners-and-source-bytes) specifies publication
and retention.
The compiler has no byte-sequence type or operation family.
Positions and lengths count octets, not scalars, and `bytes/get` reports one octet as a `UInt8`.
`bytes/slice value start length` returns the window `[start, start + length)`.
It shares the immutable allocation on every backend.
Two buffers are equal exactly when their octet sequences are equal;
`bytes/lt` compares buffers lexicographically octet by octet.
Construction from single octets goes through `bytes/singleton`, which is total because every `UInt8` is a valid octet;
the byte-level FFI contract treats borrowed buffers as read-only.

Unicode scalar values are deliberately different from user-perceived grapheme clusters.
For example, a combining mark occupies its own position.
Grapheme segmentation and normalization should be added as a separate text layer rather than changing the meaning
of these foundational operations.

## Explicit storage

Import [std/memory](memory/package.zy) directly with a shared Builtin opening.
Its `fixed` builder calculates placement in source value functions; `dynamic` accepts runtime inputs.
The [language reference](../../docs/references/language.md#manual-memory) owns memory state,
layout laws, unsafe obligations, and retention.
The library introduces no lifetimes or borrow checker.

```zydeco check
param (/VType; /CType; /Thk; /OS; /UInt8; /Unit; /process; builtin) : @(import("builtin.zy")) in
let (/Uninit; /Init; /Ptr; /fixed; /allocation; /codecs) = builtin |> (@(import("memory/package.zy"))) in
let (= Plan, = Layout, layouts) = fixed in
let (/Alloc; /heap) = allocation in
let Fault = @(import("memory/fault.zy")) in
let fail = { ! process/exit 1 } in
let no = { fn (_ : Fault) => ! fail } in
match layouts/uint8
| +Err(_) => ! fail
| +Ok(plan) =>
  let (= L, repr) = layouts/realize UInt8 plan in
  ! (allocation/reserve L Unit allocation/static_heap () repr/storage) OS no { fn vacant =>
    ! (repr/codec/init vacant 7) OS { fn live =>
      ! (codecs/unsafe/take L UInt8 repr/codec live) OS { fn vacant value =>
        ! (allocation/unsafe/release L Unit allocation/static_heap () repr/storage vacant) OS no { ! process/exit 0 }
      }
    }
  }
end
```

`Ptr L S` contains one address. `L`, `Uninit`, `Init`, and partial-record `Fields S T` are erased type parameters.
The types guide transitions, while the caller remains responsible for stale aliases,
allocation lifetime, initialization, and matching release.
`unsafe` is an ordinary library namespace documenting those obligations.

Typed records compose storage geometry independently of logical codecs
and expose field paths plus partial-initialization accessors.
Arrays supply direct element access and an explicit initialized-prefix builder.
General views interpret thin, fat, header, or indirect handles independently of the storage layout.
Fixed recipes specialize through value functions; `DynamicView` and `DynamicField` make runtime selection explicit.
The [view and layout contracts](../../docs/references/language.md#typed-records-and-field-paths) describe their types,
costs, and caller obligations.

| Need | API or example |
| --- | --- |
| Independent geometry and codecs | [storage.zy](memory/storage.zy), [codecs.zy](memory/codecs.zy), [static and runtime example](../tests/std/storage-codecs.zy) |
| Fixed placement, padding, and alignment | [layout.zy](memory/layout.zy), [aligned record](../tests/std/static-layout.zy) |
| Typed fields and partial record initialization | [record.zy](memory/record.zy), [field.zy](memory/field.zy), [example](../tests/std/general-views.zy) |
| Fixed arrays and element builders | [array.zy](memory/array.zy), [construction and cleanup](../tests/std/array-memory.zy) |
| Thin, fat, header, and indirect views | [view.zy](memory/view.zy), [header and payload composition](../tests/std/header-array.zy) |
| Runtime field paths, views, and arrays | [runtime example](../tests/std/runtime-memory.zy) |
| Runtime placement | [dynamic-layout.zy](memory/dynamic-layout.zy), [example](../tests/std/representation.zy) |
| Explicit allocation service | [allocation.zy](memory/allocation.zy) and [Alloc](memory/allocator.type.zy) |
| Typed pointer access | [representation interface](memory/representation.type.zy) |
| Expose/assert a raw pointer interpretation | `pointer/unsafe/address` and `pointer/unsafe/from_address` |
| Counted pointers with layout-derived indexing | `slices/for_layout` in [slice.zy](memory/slice.zy), [example](../tests/std/memory-views.zy) |
| Raw memory effects | [native.zy](memory/native.zy) |
| Share a pointer with another source module | [CPS worker](../tests/std/represented-call/main.zy) |
| C input and mutable output | [aligned input](../tests/ffi/static-layout.zy), [output](../tests/ffi/mutable-output.zy) |

An explicitly selected array codec copies whole logical values through the abstract `Values A` managed list.
Use `elements/unsafe/init_each` and the prefix builder to construct storage directly without that intermediate value.
These costs are separate from the compiler's ordinary product, thunk, and frame representation.

The [byte builder](memory/buffer.zy) is also exposed as `std/buffer`, with a source-defined abstract `Buffer`.
It allocates a capacity, advances an initialized prefix with `unsafe/push` or `unsafe/extend`,
and provides copying snapshots or explicit release.
`unsafe/finish_heap` transfers a builtin-heap allocation into retained `Bytes` without copying its prefix.
See the [builder example](../tests/std/buffer.zy).
Callers must retire old handles after mutation, publication, or release.

## Byte operation costs

These costs describe the current contiguous-buffer implementations, excluding general allocation/GC overhead.
[text/bytes.zy](text/bytes.zy) implements sequence operations;
[text/package.zy](text/package.zy) adds collection and option conveniences.
The runtime retains immutable backing allocations until teardown, so small slices can retain larger owners.
The counts below describe payload work; Wasm host address lookup and ordinary source allocations are additional costs.

| Operation | Work |
| --- | --- |
| `length`, `get`, `singleton` | Constant time. |
| `slice` | Constant byte work: shares the immutable owner on every backend. |
| `eq`, `lt` | At most the shorter buffer's length in byte comparisons, with early exit. |
| `append` | Copies both inputs: O(n + m). |
| Low-level `aligned` | Allocates aligned storage and copies O(n) visible bytes; host allocation costs are separate. |
| `to_list` | O(n) indexed observations and list cells. |
| `from_list` | Repeated append of a singleton to the accumulated tail: O(n²) copied bytes. |
| `concat` | Sum of the lengths copied by the right fold; quadratic for a list of equal-sized chunks. |

[numeric/codecs.zy](numeric/codecs.zy) gives every scalar `to_le_bytes` and `from_le_bytes`;
std includes these operations in its numeric modules.
The low-level packed value for bytes groups `from_retained`, `with_window`, `copy_to`,
and fixed-size `build` under `bytes/unsafe`.
Their [caller contracts](../../docs/references/language.md#immutable-owners-and-source-bytes) cover initialized extents,
retention, immutable aliases, and one completion per fill.
A future [memory-backed writer](../../docs/proposals/filesystem.md#memory-backed-writer-and-byte-builder)
would provide incremental construction without changing immutable-byte observations.

## Total operations

Operations whose inputs may be invalid report that fact in their types:

```zydeco
string/get          : String -> Int -> Ret (Option Char)
string/split_at     : String -> Int -> Ret (Option (String * String))
string/parse_int    : String -> Ret (Option Int)
char/from_codepoint : Int -> Ret (Option Char)
bytes/get           : Bytes -> Int -> Ret (Option UInt8)
bytes/slice         : Bytes -> Int -> Int -> Ret (Option Bytes)
list/get            : forall (A : VType) . List A -> Int -> Ret (Option A)
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

The integer types are `Int8`, `Int16`, `Int32`, `Int64`, `Int`, `UInt8`, `UInt16`, `UInt32`, `UInt64`, and `UInt`.
Their domains and arithmetic follow [L13](../../docs/references/language.md#13-primitive-values-and-capabilities).
`Int` and `UInt` are tagged machine integers; the numbered integer types have exact widths.
`int64/from_int` and `uint64/from_uint` widen machine integers without loss.
`int64/to_int` and `uint64/to_uint` select failure or success continuations according to the destination range;
see the [conversion contract](../../docs/references/language.md#13-primitive-values-and-capabilities).
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
The width-specific modules in the public packed value reify those branches as `Bool` and add derived helpers.

## Numeric capabilities and explicit instances

Generic numeric functions receive the operations they need as ordinary arguments.
The public packed value exposes five capability type constructors; their linked definitions give the exact field types:

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
The `dictionaries` module contains one dictionary per numeric representation,
from `int8_dictionary` through `float64_dictionary`.
Generic code explicitly selects and passes one:

```zydeco check
param (/VType; /Ret; /Int; builtin) : @(import("builtin.zy")) in
let make_std = @(import("std.zy")) in
let (/Additive; /dictionaries) = builtin |> make_std in
let ! twice (A : VType) (operations : Additive A) (value : A) : Ret A =
  ! operations/add value value
in
! twice Int (dictionaries/int_dictionary/additive) 21
```

These interfaces describe operations rather than proving algebraic laws.
For example, IEEE equality is not reflexive on NaN, which motivates `PartialEquality`.
Division, integer remainder, rendering, extrema,
and other representation-specific operations stay in the width modules. The numeric layer inherits the
[literal and conversion rules](../../docs/references/language.md#13-primitive-values-and-capabilities).

A library can also expose the selected carrier together with its dictionary in a packed value with a manifest carrier.
The following complete example names the disclosed type `Int`, renames it to `Carrier` when opening,
and checks that the operations use that same carrier:

```zydeco check
param (/VType; builtin) : @(import("builtin.zy")) in
let make_std = @(import("std.zy")) in
let (/Bool; /Numeric; /dictionaries) = builtin |> make_std in
let int_instance =
  pack (= Int as @(intrinsic(int)) : VType)
  where #operations = dictionaries/int_dictionary end
in
let (/Int = Carrier; /operations) = int_instance in
let _ : Numeric Bool Carrier = operations in
! operations/additive/add (21 : Carrier) 21
```

The [manifest type rules](../../docs/references/language.md#9-polymorphism-and-packed-values)
supply the disclosed equation;
[field selection](../../docs/references/language.md#9-polymorphism-and-packed-values) governs the shared opening.
Naming a manifest field after its carrier avoids imposing a generic role label on each consumer.
When exporting several instances, use distinctive value names such as `int_instance` and `float32_instance`
so their selection is unambiguous.

Selection remains explicit value flow: lexical bindings and arguments determine which dictionary is used.
Several implementations for one carrier can coexist without global instance search or coherence checking.
A wrapper can carry additional abstract or manifest type fields under the same witness scope rules.
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
- `int8` through `int` and `uint8` through `uint`: arithmetic, complete comparisons, successor/predecessor,
  wrapping negation, extrema, and string rendering.
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
`std.zy` is the composition root used by most programs; its public packed value carries the library-defined types
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
io/read       : Reader -> Int -> Thk (Result Bytes IoError -> OS) -> OS
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
