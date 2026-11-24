# Zydeco Standard Library

## Start with the types

Common kinds and types are available without prelude setup:

```zydeco
{ fn (value : Int) => (ret value : Ret Int) }
```

Run CLI commands from the repository root to use its automatically detected project.
Provider-independent libraries need no Builtin parameter.
Programs using host operations select their provider explicitly:

```zydeco
param (/process; builtin) : @(import(/std/builtin)) in
let stop : Thk (Int -> OS) = process/exit in
! stop 0
```

The [core prelude contract](../../docs/references/language/src/sources/core-prelude.md) owns the vocabulary,
lookup rules, and `@[prelude(none)]` / `@[prelude(from(view))]` exceptions.
Keep dependent operations and the retained provider alias on one shared opening.

The Builtin contract exposes these surface names:

| Family | Names |
| --- | --- |
| CBPV | `VType`, `CType`, `Thk`, `Ret`, `Unit` |
| Signed integers | `Int8`, `Int16`, `Int32`, `Int64`, `Int` |
| Unsigned integers | `UInt8`, `UInt16`, `UInt32`, `UInt64`, `UInt` |
| Floating point | `Float32`, `Float64` |
| Text | `Char`, `String` |
| Capabilities | `Addr`, `Reader`, `Writer`, `OS` |

Runtime host operations live under `memory_ops` (`memory` and `scalar`) and `system` (`io`,
`fs`, `stdio`, `args`, `random`, and `process`), and field search descends through them:
`param (/stdio; /process) : @(import(/std/builtin)) in` selects two groups from `system` without naming it.
Pure computational primitive operations (`numeric` and `text`) live separately
in [`primitive/package.zy`](primitive/package.zy) (`std/primitive/ops`).
The [integer example](../tests/std/minimal.zy) takes `Int` and `Ret` from the core prelude,
then uses the assembled standard-library value for arithmetic.

## Library boundaries

Runtime operations have two boundaries.
[`builtin.zy`](builtin.zy) is the typed contract between Zydeco programs and the host runtime.
Its operations expose representation-independent observations and effects,
but never construct library-defined `Bool`, `Option`, `Either`, `List`, or `Bytes` values.
[`std.zy`](std.zy) opens `Builtin` once, instantiates the capability-parameterized topic modules,
and re-exports their sealed types as manifest existential witnesses (`= Bytes as Bytes`,
`= Buffer as Buffer`, `= Path as Path`, `= IoErrorKind as IoErrorKind`, `= IoError as IoError`).

Types and the definitions that use them live in the same source package.
The [organization rule](../../docs/references/library/src/organization.md) distinguishes manifest structural interfaces
from abstract representation witnesses.
Modules that define sealed abstract types use applicative sealing (`seal term : Sig`) directly over a local signature
so independent callers that pass the same static capability witness share one canonical abstract type
without forwarding intermediate type or operation packages:

```zydeco
param val (/Addr; memory_builtin) : @(import("builtin/memory.zy")) in
let (/Bytes; /bytes) = memory_builtin |> (@(import("text/bytes.zy"))) in
bytes/empty
```

The whole-value alias forwards the `Addr` opening to `text/bytes.zy`, which seals `Bytes`
as a type family indexed by `Addr` without requiring any `OS` witness or stream capabilities.
The pure and zero-parameter modules—`data/package.zy`, `numeric/package.zy`, `text/package.zy`, `effect/nondet.zy`,
`memory/storage.zy`, and `memory/dynamic-size.zy`—need no factory parameters and are imported directly with `let`;
`memory/storage.zy` uses zero-parameter applicative sealing for `Storage` and `DynamicStorage`,
while keeping fresh layout witness creation (`constant/create`, `runtime/create`) generative under nested `exists`.
Explicit signatures remain local to their implementations wherever they prescribe abstraction;
colocation does not replace those signatures with inferred interfaces.

`text/bytes.zy`, `text/bytes-ops.zy`, `numeric/codecs.zy`, `memory/*`,
and `system/package.zy` take only their capability roots (`MemoryBuiltin` / `Addr` or `Builtin`,
plus genuine template parameters such as `Geometry` in `memory/array-core.zy`),
and `std.zy` instantiates them once to assemble its public packed value.
Shared representation packages such as `data/either.zy` expose manifest type fields with a `()` payload.
The host-independent `effect/monad.zy` and `algebra/monoid.zy` use that same type-only packaging convention.

This separation keeps algebraic data, numeric modules, and pure text utilities in concrete library terms.
The interpreter and native runtime only need to agree on intrinsic primitive operations
and the capability contracts ([`builtin.zy`](builtin.zy), [`builtin/memory.zy`](builtin/memory.zy),
and [`builtin/system.zy`](builtin/system.zy)), while pure library modules under `data/`,
`numeric/`, `text/`, and `effect/` remain ordinary Zydeco code.

`system/arguments.zy` builds a lazy `fold` from the Builtin `args/at` lookup.
Apply it to the `args` field from Builtin or `std/builtin/system` directly when a list is unnecessary;
`process/arg_list` uses the same builder.
Callers of the former Builtin `args/fold` should instantiate this builder and call its `fold` field.
Tails are ordinary reusable computations, and host runtimes need no special closure implementation.
[Argument semantics](../../docs/references/language/src/primitives-and-capabilities.md) specify lookup failures
and repeated forcing.

The optional `memory/package.zy` builder is imported directly, like the effect libraries.
It supplies fixed and dynamic layouts, typed pointers, and explicit allocation operations.
See [explicit storage](#explicit-storage).

## Source packages and tests

[`std.zy`](std.zy) declares the whole-file package `std`.
The primitive type leaf, pure primitive operations, and closed default view are `std/primitive`,
`std/primitive/ops`, and `std/prelude`.
Builtin contracts are `std/builtin`, `std/builtin/memory`, and `std/builtin/system`;
topic entries are `std/data`, `std/text`, `std/numeric`, `std/system`, and `std/memory`.
These names identify source terms; fields and factory applications remain ordinary language structure.
The [source-package rules](../../docs/references/language/src/sources/packages.md#source-packages) govern selection
and relationships.

The repository's [workspace.zy](../../workspace.zy) explicitly includes entry files and the std test tree.
From the repository root:

```sh
zydeco show
zydeco check -p /std
zydeco test lib/tests/std/bool.zy
zydeco test lib/tests/std/bool.zy -t all
```

Within that project, use `@(import(/std))` or `@(import(/std/memory))`.
Explicit file imports remain valid without a project.
Self-contained tests declare `@[package(test(of(/std)))]` on their roots, including tests of optional libraries.
Discovery exposes tests in that scope to project inspection.
[Suite selection from relationships](../../docs/ideas/README.md#project-context-inspection) remains planned;
select test files explicitly for execution.

The package runner defaults to the interpreter and uses empty stdin and arguments on every selected backend.
Support programs that need supplied arguments, fixtures, or input live under `lib/tests/std/support/`;
they have no standalone package test.
Their leading directives declare the execution setup,
and the [std_directives runner](../../lang/tests/tests/directives.rs) supplies it.

Use std from the same compiler release or revision, preserving its internal relative imports.
There is no separate version resolver or hosted registry.

## Source layout

The [organization rule](../../docs/references/library/src/organization.md) separates value data,
general-purpose dictionaries, effects, and runtime capabilities.
The current files are:

```text
builtin.zy                 complete host ABI bridging memory and system capabilities
builtin/memory.zy          isolated unmanaged-memory capability (Addr, memory, scalar)
builtin/memory/*.zy        raw memory operations and little-endian scalar accesses
builtin/system.zy          isolated system capability (Reader, Writer, OS, fs, stdio, args, random, process)
builtin/system/*.zy        I/O, filesystem, streams, arguments, randomness, process
primitive.zy               provider-independent canonical kinds and types
primitive/package.zy       concrete std/primitive/ops pure numeric, char, and string operations
prelude.zy                 closed core vocabulary used by the implicit default
std.zy                     wiring for the public packed value

data/package.zy            concrete Bool, Option, Either, List, and derived operations
data/either.zy             shared binary sum for data, effects, I/O, and memory APIs

numeric/{integer,float}.zy module contracts and explicitly polymorphic make builders
numeric/dictionary.zy      shared numeric capability constructors, with a unit payload
numeric/package.zy         concrete twelve numeric modules and their capability dictionaries
numeric/codecs.zy          source byte codecs over scalar memory leaves

text/bytes.zy              applicatively sealed immutable Bytes abstraction parameterized by MemoryBuiltin
text/bytes-ops.zy          byte collection and option conveniences over MemoryBuiltin
text/package.zy            concrete Char and String convenience operations

memory/package.zy          manual-memory interface re-exporting sealed pointer, state, storage, and slice types
memory/types.zy            applicatively sealed Ptr<L,S>, Uninit, Init, and Fields indexed by Addr
memory/storage.zy          zero-parameter applicatively sealed fixed/runtime geometry and generative layout creation
memory/codecs.zy           fixed/runtime scalar codecs and the Representation constructor over MemoryBuiltin
memory/layout.zy           applicatively sealed Plan and Layout over MemoryBuiltin
memory/dynamic-layout.zy   applicatively sealed dynamic Layout over MemoryBuiltin
memory/size.zy             checked static size and alignment calculations
memory/shape.zy            inspectable scalar widths and product placement
memory/buffer.zy           applicatively sealed Buffer builder for incremental byte construction over MemoryBuiltin
memory/allocation.zy       explicit allocator services over MemoryBuiltin
memory/slice.zy            applicatively sealed Slice over MemoryBuiltin and checked index arithmetic
memory/native.zy           raw memory adapter and source fault values

system/package.zy          zero-parameter sealed system data types and capability-preserving assembly over Builtin

algebra/monoid.zy          general-purpose monoid dictionary

effect/monad.zy            Top, Monad, Algebra, MonadInstance, and WithMonad; no host argument
effect/{ret,reader,writer,option}.zy  provider-independent carriers and derived transformers
effect/transformer*.zy              shared transformer interfaces and derivation adapters
effect/{state,exception,state-exn}.zy  provider-independent state, exceptions, and their combination
effect/nondet.zy           concrete MonadNonDet interface, ordered search instances, and observers
```

The public `Either` type and `either` operation bundle follow the
[shared binary-sum contract](../../docs/references/library/src/organization.md#either-values).
Fallible APIs put errors on the left and successful values on the right.

### Either operations

All `either` operations take the input branch types `L` and `R` first.
Transformations then take their new payload type `S`; `fold` instead takes an arbitrary computation result `B`.
Computation callbacks are passed as thunks and run only for the matching branch:

| Operation | Contract |
| --- | --- |
| `left L R value`, `right L R value` | Construct the corresponding branch under `Ret` |
| `fold L R B value on_left on_right` | Apply the handler for the selected branch, returning `B` |
| `map L R S value transform` | Transform a right payload through `R -> Ret S`, returning `Ret (Either L S)` |
| `map_left L R S value transform` | Transform a left payload through `L -> Ret S`, returning `Ret (Either S R)` |
| `and_then L R S value next` | Sequence a right payload through `R -> Ret (Either L S)`; preserve a left payload |
| `unwrap_or L R value fallback` | Return the right payload, or the fallback for a left value |
| `is_left L R value`, `is_right L R value` | Return the corresponding branch test as `Ret Bool` |

Topic implementations are independently checkable source terms that take their needed capabilities directly.
Import `algebra/monoid.zy` and the `effect/` modules directly; `std.zy` does not assemble them.
The public packed value keeps one opening for `Reader`, `Writer`, and `OS`;
splitting that opening would give related I/O operations incompatible abstract types.
No compatibility forwarding files remain at the old flat paths.

## Builtin interface

The launcher supplies one packed value implementing the [Builtin contract](builtin.zy) (for full executables),
or [std/builtin/system](builtin/system.zy) for programs that need system/I/O operations without unmanaged memory.
Library components that allocate or access raw memory
without invoking `OS` accept [std/builtin/memory](builtin/memory.zy).
The complete Builtin signature begins with manifest fields for the CBPV kinds, constructors,
and fixed-representation types, followed by abstract witnesses for runtime-owned capabilities.
Its value payload groups only host capabilities:

- Surface: `VType`, `CType`, `Thk`, `Ret`, `Unit`, the twelve numeric types, `Char`,
  `String`, then abstract `Addr`, `Reader`, `Writer`, and `OS`.
- `memory_ops`: checked `memory` operations (`allocate`, `null`, `offset`, `copy`,
  `fill`, `free`) and raw `scalar` `store_le`/`load_le` accesses over `Addr`.
- `system`: the re-exposed capabilities (`Addr`, `Reader`, `Writer`, `OS`) plus `io`, `fs`, `stdio`, `args`,
  `random`, and `process` operations.

Because `Builtin` discloses `#Addr` inside `#system`
and exports `#memory_ops` using the same underlying `SystemAddr` witness,
a consumer holding an opened `Builtin` bridges directly to `MemoryBuiltin`
with `let memory_builtin : @(import("builtin/memory.zy")) = (= Addr, memory_ops) in`.
Pure arithmetic, conversions, character helpers, and string operations instead live
in [primitive/package.zy](primitive/package.zy) (`numeric` and `text`), built directly over `@(intrinsic(...))`.

Each public name is unique across the complete packed value, so one field search reaches kinds, types,
operations, and capabilities alike, and every selection shares the contract's identities.
Fixed representations are compiler-canonical intrinsics, so independent selections share one `Int` identity.
Opening the runtime-owned system capabilities introduces fresh abstract witnesses.
A composition root that must pass the dependency onward keeps the whole-alias `builtin` beside its selections.
The [primitive and capability rules](../../docs/references/language/src/primitives-and-capabilities.md)
define these identity boundaries.
The Builtin contracts and primitive leaf bootstrap their canonical types with `@(intrinsic(name))`.
Higher-level libraries use the default vocabulary instead of repeating those bindings or importing the leaf.
Value-level intrinsics used for static arithmetic remain explicit; the primitive leaf only supplies kinds and types.
Focused tests of the complete Builtin signature and shared openings also retain explicit projections.

## Packed value composition

Select related types and operations in one projection group so they share one abstract opening.
A whole-value alias passes that same dependency to a factory:

```zydeco
param (/Reader; /io; builtin) : @(import("builtin.zy")) in
let make_std = @(import("std.zy")) in
let (/Bytes; /bytes; /fs) = builtin |> make_std in
do value <- ! bytes/empty;
! bytes/length value
```

Here `Reader` and primitive `io` come from one Builtin opening; `builtin` forwards that packed value.
`Bytes`, convenient `bytes`, and `fs` share the std opening.
For low-level memory composition, every dependent module instantiates `text/bytes.zy`, `memory/types.zy`,
`memory/storage.zy`, or `memory/field.zy` directly against its shared `Builtin` or `MemoryBuiltin` opening,
and applicative sealing keeps their sealed types unified across modules.
Slash selection follows the [language rules](../../docs/references/language/src/polymorphism-and-packed-values.md),
including nested products and ambiguity.
A view such as `make_std ~> (/bytes; /fs)` combines application and opening.

A final `pack` introduction is useful when an implementation should synthesize its exported type evidence.
Use an explicit existential annotation when the contract must prescribe an abstract payload type;
use a companion `.zyi` source when the contract deserves independent authorship.
Named product values also synthesize their types, so naming fields alone does not require a companion.

Kind fields currently need an annotated packed value introduction rather than `pack`:

```zydeco
let types =
  pack (= Thk as Thk)
  where () end
in
((VType, CType, types) :
  exists (VType as VType) (CType as CType) . @[typeof] types)
```

The annotation discloses the kind equations, while the inner `pack` supplies a type constructor. The
[module-interface rules](../../docs/references/language/src/polymorphism-and-packed-values.md#module-interfaces-and-shared-openings)
explain when inferred and authored interfaces serve different maintenance needs.

## Explicit runtime contracts

A value functor is a total function between packed values and undergoes static elimination.
A computation functor receives a packed value through a computation protocol;
its thunk may remain dynamically selectable.
The callee body need not be statically known when its signature and the argument's witnesses are available.

For `Sig = exists (X : K) . A X`, explicit adapters connect `pi ((X, x) : Sig) . C X`
and `forall (X : K) . A X -> C X`: they group or separate the same type witness and payload.
The result may depend on `X`, not on the arbitrary runtime value `x`.
For example, both interfaces can expose codata methods:

```zydeco
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

```zydeco
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

## Relative monads and effect examples

The [effect modules](effect) supply `Monad`, `Algebra`, `MonadInstance`, `WithMonad`,
and carrier-indexed Reader, Writer, Option, State, Exception, and nondeterminism interfaces.
`WithMonad` packages one carrier and monad dictionary with the chosen operations;
the combined `IStateExn` interface composes both effects inside that boundary.
Each effect module's `instance` builder supplies a concrete carrier and its dictionaries;
generic clients use a witness-dependent `pi`.
Ret, Optional, and NonDet use an `instance` value rather than a parameterized builder.
The [packed-effect reference](../../docs/references/library/src/effects/relative-monads.md#packed-effect-instances)
owns the packed boundary and its connection to monadic-block translation.
The [capability walkthrough](../../docs/tutorial/zydeco-guide.md#10-capability-dictionaries) shows how
to open a concrete instance and pass it to a generic client.

The
[elementary effect reference](../../docs/references/library/src/effects/transformers-and-instances.md#elementary-effect-instances)
specifies Reader's scoped replacement, Writer's pure monoid contract, and Optional's short-circuiting. Import
[Ret](effect/ret.zy), [Reader](effect/reader.zy), [Writer](effect/writer.zy), or [Option](effect/option.zy) directly
without a Builtin argument. Reader, Writer, Option, State, Exception, and NonDet use the shared
[derived transformer interface](../../docs/references/library/src/effects/transformers-and-instances.md#derived-relative-monad-transformers).
Their transformer instances package a lift together with the monad and effect operations. The
[translated example](../tests/effects/effect-monadic.zy) shows their carrier-polymorphic clients, the
[composition example](../tests/effects/effect-operations.zy) exercises the transformers together, and the
[law suite](../tests/effects/effect-laws.zy) (paired with
[law counterexamples](../tests/effects/effect-law-counterexamples.zy),
[derived-law checks](../tests/effects/transformer-laws.zy),
[State/Exception stack tests](../tests/effects/state-exception-stack.zy), and
[transformer classifier tests](../../lang/tests/tests/transformers.rs)) validates the equations and boundaries.
[Further effect libraries](../../docs/ideas/README.md#further-effect-libraries) lists fair search, control, parsing, and
other unimplemented families.

### Derived transformers

Use each module's `transformer_instance` builder when the program needs that effect's operations and a lift. Effect
configuration comes first, followed by the base carrier and its monad dictionary. The computation-level `transformer`
exposes the common `MonadTrans` interface for generic transformer clients, accepting the base as one `MonadInstance`
argument after effect configuration: `! state/transformer S (M, base)`.
[The packed-base contract](../../docs/references/library/src/effects/transformers-and-instances.md#derived-relative-monad-transformers)
keeps the result indexed by the supplied carrier while the generated monadic interface stays curried internally.
[The reference](../../docs/references/library/src/effects/transformers-and-instances.md#elementary-transformers) lists
the exact constructors and runners.

For example, StateT over Writer keeps state operations and base output at one transformed carrier:

```zydeco
let (/text) = @(import("primitive/package.zy")) in
let (/string) = text in
let writer = @(import("effect/writer.zy")) in
let state = @(import("effect/state.zy")) in
let (W, w) = writer/instance String (#empty = "", #append = string/append) in
let (SW, sw) = state/transformer_instance Int W w/monad in
! state/run_state_t Int W Int 3 {
  ! sw/monad .bind Unit Int { ! sw/state .put 8 } {
    fn _ => ! sw/lift Int { ret (7, "changed") }
  }
}
```

It returns `((7, 8), "changed")`: answer, final state, and base output.
To call an ordinary State client, project `(SW, #monad = sw/monad, #state = sw/state)`;
a lift-aware client can instead request `WithMonad (ITransformer W)` from [the shared module](effect/transformer.zy).
The [uniform transformer example](../tests/effects/transformer-operations.zy) covers all six families,
and the [translated stack example](../tests/effects/transformer-monadic.zy) contrasts State/Exception order.

### Delimited control examples

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
These encodings do not capture native suspension IDs;
[native lifetime rules](../../docs/references/compiler/src/native-emission.md) remain a separate
implementation contract.

### Ordered depth-first search

Import `List` from [data/package.zy](data/package.zy) and `nondet` directly from [NonDet](effect/nondet.zy).
Package merging shares the same `List` identity for `choose` inputs and list observations across consumers:

```zydeco
let (/list) = @(import("data/package.zy")) in
let nondet = @(import("effect/nondet.zy")) in
let (N, n) = nondet/instance in
do answers <- ! nondet/observe_many Int 2 {
  ! n/nondet .choice Int { ! n/monad .return Int 3 } { ! n/monad .return Int 1 }
};
! list/length Int answers
```

This library term observes the two answers `3, 1` and returns their length.
The module supplies `choose`, `guard`, `observe_first`, `observe_many`, and `observe_all`.
Use `transformer_instance M base` for an explicit base carrier and invoke `lift A computation` on that instance.
The `_t` observers also take `M base` to build their observation algebras.
The [search reference](../../docs/references/library/src/effects/nondet.md) owns the continuation protocol,
observation limits, and interaction with base effects.
The [backtracking example](../tests/monadic/backtrack.zydeco) is a complete executable client;
the [translated clients](../tests/effects/nondet-monadic.zy),
[operation fixture](../tests/effects/nondet-operations.zy), [law fixture](../tests/effects/nondet-laws.zy),
[returned-list counterexample](../tests/effects/nondet-law-counterexamples.zy),
and [transformer interactions](../tests/effects/nondet-transformers.zy) exercise these boundaries.

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
[the owning design](../../docs/references/library/src/memory/views-and-buffers.md#immutable-owners-and-source-bytes)
specifies publication and retention, and [source-bytes.zy](../tests/std/source-bytes.zy) exercises slicing,
bounds, UTF-8, copying, and retention.
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
The [standard library reference](../../docs/references/library/src/memory/manual-memory.md#manual-memory)
owns memory state, layout laws, unsafe obligations, and retention.
The library introduces no lifetimes or borrow checker.

```zydeco
param (/Addr; /memory_ops; /process) : @(import("builtin.zy")) in
let memory_builtin : @(import("builtin/memory.zy")) = (= Addr, memory_ops) in
let (/Uninit; /Init; /Ptr; /fixed; /allocation; /codecs) = memory_builtin |> (@(import("memory/package.zy"))) in
let (= Plan, = Layout, layouts) = fixed in
let (/Alloc; /heap) = allocation in
let (/Fault) = @(import("memory/fault.zy")) in
let fail = { ! process/exit 1 } in
let no = { fn (_ : Fault) => ! fail } in
match layouts/uint8
| +Left(_) => ! fail
| +Right(plan) =>
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

Typed records compose storage geometry independently of logical codecs and expose field paths plus
partial-initialization accessors. Arrays supply direct element access and an explicit initialized-prefix builder.
General views interpret thin, fat, header, or indirect handles independently of the storage layout. Fixed recipes
specialize through value functions; `DynamicView` and `DynamicField` make runtime selection explicit. The
[view and layout contracts](../../docs/references/library/src/memory/layouts-and-aggregates.md#typed-records-and-field-paths)
describe their types, costs, and caller obligations.

| Need | API or example |
| --- | --- |
| Independent geometry and codecs | [storage.zy](memory/storage.zy), [codecs.zy](memory/codecs.zy), [static and runtime example](../tests/std/storage-codecs.zy) |
| Fixed placement, padding, and alignment | [layout.zy](memory/layout.zy), [aligned record](../tests/std/static-layout.zy), [phase tests](../../lang/tests/tests/static_layout.rs) |
| Typed fields and partial record initialization | [record.zy](memory/record.zy), [field.zy](memory/field.zy), [example](../tests/std/general-views.zy), [state tests](../../lang/tests/tests/general_memory.rs) |
| Fixed arrays and element builders | [array.zy](memory/array.zy), [construction and cleanup](../tests/std/array-memory.zy), [padded stride](../tests/std/storage-arrays.zy) |
| Thin, fat, header, and indirect views | [view.zy](memory/view.zy), [header and payload composition](../tests/std/header-array.zy) |
| Runtime field paths, views, and arrays | [runtime example](../tests/std/runtime-memory.zy) |
| Runtime placement | [dynamic-layout.zy](memory/dynamic-layout.zy), [example](../tests/std/representation.zy) |
| Explicit allocation service | [allocation.zy](memory/allocation.zy) and [Alloc](memory/allocation.zy) |
| Typed pointer access | [representation interface](memory/codecs.zy) |
| Expose/assert a raw pointer interpretation | `pointer/unsafe/address` and `pointer/unsafe/from_address` |
| Counted pointers with layout-derived indexing | `slices/for_layout` in [slice.zy](memory/slice.zy), [example](../tests/std/memory-views.zy) |
| Raw memory effects | [native.zy](memory/native.zy) |
| Share a pointer with another source module | [CPS worker](../tests/std/represented-call/main.zy) |
| C input, mutable output, and decoding | [aligned input](../tests/ffi/static-layout.zy), [output](../tests/ffi/mutable-output.zy), [C specimen](../tests/ffi/contracts.c), [decoder](../tests/ffi/record-input.zy), [FFI tests](../../lang/tests/tests/ffi_examples.rs) |

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
The low-level packed value for bytes groups `from_retained`,
`with_window`, `copy_to`, and fixed-size `build` under `bytes/unsafe`. Their
[caller contracts](../../docs/references/library/src/memory/views-and-buffers.md#immutable-owners-and-source-bytes)
cover initialized extents, retention, immutable aliases, and one completion per fill.
A future [memory-backed writer](../../docs/ideas/README.md#growable-byte-builders-and-incremental-streams)
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

The integer types are `Int8`, `Int16`, `Int32`, `Int64`, `Int`, `UInt8`, `UInt16`, `UInt32`, `UInt64`, and `UInt`. Their
domains and arithmetic follow
[Primitive Values and Capabilities](../../docs/references/language/src/primitives-and-capabilities.md#scalar-ranges-and-conversions).
`Int` and `UInt` are tagged machine integers; the numbered integer types have exact widths. `int64/from_int` and
`uint64/from_uint` widen machine integers without loss. `int64/to_int` and `uint64/to_uint` select failure or success
continuations according to the destination range; see the
[conversion contract](../../docs/references/language/src/primitives-and-capabilities.md). Integer division and remainder
are not yet wrapped in checked operations. The generic numeric capability layer deliberately excludes them; a future
checked-arithmetic capability should make their failure behavior explicit.

`Float32` and `Float64` are IEEE-754 binary32 and binary64 values backed by Rust's `f32` and `f64`.
Decimal and scientific literals use an expected `Float32` or `Float64` type and default to `Float64` otherwise.
The float modules provide arithmetic, comparisons, negation, and shortest round-trippable decimal rendering.
The exact spelling follows the shared [numeric representation rules](../../DESIGN.md#numeric-representations).
Division by zero, infinities, signed zero, and NaN follow IEEE-754 behavior.
In particular, every ordered comparison with NaN is false, while `float32/ne` and `float64/ne` report true.

The exact host-facing primitive operations live in [primitive/package.zy](primitive/package.zy) (`numeric`),
one module per width.
Their comparisons select one of two computation continuations directly,
avoiding a dependency on the library's `Bool` representation.
The width-specific modules in the public packed value reify those branches as `Bool` and add derived helpers.

## Numeric capabilities and explicit instances

Generic numeric functions receive the operations they need as ordinary arguments.
`numeric/dictionary.zy` and the public packed value expose five capability type constructors over canonical `Bool`;
their linked definitions give the exact field types:

| Interface | Fields |
| --- | --- |
| [Additive A](numeric/dictionary.zy) | `zero`, `add`, `sub`, `negate` |
| [Multiplicative A](numeric/dictionary.zy) | `one`, `mul` |
| [PartialEquality A](numeric/dictionary.zy) | `eq`, `ne`, returning `Bool` |
| [PartialOrder A](numeric/dictionary.zy) | `equality`, `lt`, `le`, `gt`, `ge` |
| [Numeric A](numeric/dictionary.zy) | `additive`, `multiplicative`, `order` |

Larger dictionaries contain smaller ones as named fields.
This preserves paths such as `dictionary/additive/add` and `dictionary/order/equality/eq`,
and lets a consumer receive just `Additive A` when it only needs addition.
The `dictionaries` module contains one dictionary per numeric representation,
from `int8_dictionary` through `float64_dictionary`.
Generic code explicitly selects and passes one:

```zydeco
let (/Additive; /dictionaries) = @(import("numeric/package.zy")) in
let ! twice (A : VType) (operations : Additive A) (value : A) : Ret A =
  ! operations/add value value
in
! twice Int (dictionaries/int_dictionary/additive) 21
```

These interfaces describe operations rather than proving algebraic laws.
For example, IEEE equality is not reflexive on NaN, which motivates `PartialEquality`.
Division, integer remainder, rendering, extrema,
and other representation-specific operations stay in the width modules. The numeric layer inherits the
[literal and conversion rules](../../docs/references/language/src/primitives-and-capabilities.md).

A library can also expose the selected carrier together with its dictionary in a packed value with a manifest carrier.
The following complete example names the disclosed type `Int`, renames it to `Carrier` when opening,
and checks that the operations use that same carrier:

```zydeco
let (/Numeric; /dictionaries) = @(import("numeric/package.zy")) in
let int_instance =
  pack (= Int as Int)
  where #operations = dictionaries/int_dictionary end
in
let (/Int = Carrier; /operations) = int_instance in
let _ : Numeric Carrier = operations in
! operations/additive/add (21 : Carrier) 21
```

The [manifest type rules](../../docs/references/language/src/polymorphism-and-packed-values.md)
supply the disclosed equation;
[field selection](../../docs/references/language/src/polymorphism-and-packed-values.md) governs the shared opening.
Naming a manifest field after its carrier avoids imposing a generic role label on each consumer.
When exporting several instances, use distinctive value names such as `int_instance` and `float32_instance`
so their selection is unambiguous.

Selection remains explicit value flow: lexical bindings and arguments determine which dictionary is used.
Several implementations for one carrier can coexist without global instance search or coherence checking.
A wrapper can carry additional abstract or manifest type fields under the same witness scope rules.
Its static components obey the [static elimination contract](../../docs/references/language/src/static-elimination.md);
ordinary dictionary thunks may remain at runtime.
The [runtime contract design](README.md#explicit-runtime-contracts) describes adapters
when operations must be dynamically selectable.

## Public modules

- `bool`: constants, logical connectives, equality, and conditional elimination.
- `option`: construction, elimination, mapping, chaining, defaults, and zipping.
- `either`: left and right alternatives, elimination, mapping, chaining, defaults, and predicates.
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
and every fallible operation reports `Either IoError A` to its `OS` continuation.
EOF is represented as `Option` by line reads; it is not conflated with an empty line or an I/O failure.
The [stream guide](#streams-and-files) gives the operations and lifecycle contract;
[stream ideas](../../docs/ideas/README.md#growable-byte-builders-and-incremental-streams)
retain the extension questions.

The topic files are independently importable terms parameterized only by the capabilities they use.
`std.zy` is the composition root used by most programs;
its public packed value re-exports the sealed library types manifestly and bundles topic modules,
while the Builtin contract remains the home of every host type and capability.
Its public record nests one sub-record per topic, with manifest equations for the sealed library types,
so no restated contract sits between the implementation and its consumers.
Consumers still select individual modules and types directly,
such as `let make_std ~> (/option; /process) = builtin in`, because slash projection searches the nested structure.
Here `~>` is a view pattern over the imported value function; the function itself is an ordinary value.

## Streams and files

The shared `io` layer is byte-oriented.
The following computation protocols apply after forcing the exported operation thunks.
Fallible resource operations use the standard library's continuation-passing `OS` convention:

```text
io/read       : Reader -> Int -> Thk (Either IoError Bytes -> OS) -> OS
io/read_line  : Reader -> Thk (Either IoError (Option Bytes) -> OS) -> OS
io/read_all   : Reader -> Thk (Either IoError Bytes -> OS) -> OS
io/write_all  : Writer -> Bytes -> Thk (Either IoError Unit -> OS) -> OS
io/flush      : Writer -> Thk (Either IoError Unit -> OS) -> OS
io/close_reader : Reader -> Thk (Either IoError Unit -> OS) -> OS
io/close_writer : Writer -> Thk (Either IoError Unit -> OS) -> OS
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
fs/open_reader   : Path -> Thk (Either IoError Reader -> OS) -> OS
fs/create_writer : Path -> Thk (Either IoError Writer -> OS) -> OS
fs/append_writer : Path -> Thk (Either IoError Writer -> OS) -> OS
fs/read_bytes    : Path -> Thk (Either IoError Bytes -> OS) -> OS
fs/read_text     : Path -> Thk (Either IoError String -> OS) -> OS
fs/write_bytes   : Path -> Bytes -> Thk (Either IoError Unit -> OS) -> OS
fs/write_text    : Path -> String -> Thk (Either IoError Unit -> OS) -> OS
```

`create_writer` creates a missing file and truncates an existing one.
`append_writer` creates a missing file and places every write at the end.
Whole-file helpers open, operate, and close internally.
If the data operation fails, that error wins; otherwise a close error is returned.

The `stdio` module exposes `stdin`, `stdout`, and `stderr` as capabilities.
Its `read_line` checks UTF-8 and returns `Either IoError (Option String)`; its `write`, `write_line`,
and error-stream variants encode `String` to `Bytes`, delegate to `io/write_all`, and preserve write or flush failures.

`IoError` carries a stable kind and a display message.
The kinds are `NotFound`, `PermissionDenied`, `AlreadyExists`, `InvalidInput`, `InvalidData`,
`BrokenPipe`, `Closed`, and `Other`; branch on the kind rather than parsing the message.
A read at EOF returns empty bytes; `read_line` returns `Right None` instead.
An empty line is `Right (Some empty)`, and a final line without a newline is still returned.
Line reads strip a trailing LF and its immediately preceding CR; arbitrary byte reads preserve their contents.
A zero-byte read also returns empty bytes, so it cannot by itself establish EOF.

Handles are alias-visible capabilities.
Closing a file-backed reader or writer closes it for every alias, and subsequent operations report `Closed`.
Reserved standard streams remain available after ordinary close requests.
The current streams block.
[Stream ideas](../../docs/ideas/README.md#growable-byte-builders-and-incremental-streams) cover buffering,
seeking, memory-backed writers, and future asynchronous protocols.
