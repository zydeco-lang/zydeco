# Parameterization and Local Bindings in the Standard Library

## Motivation and Problem Statement

Zydeco relies on ordinary term forms to express both reusable abstractions and modular program composition.
A component can accept dependencies through `param`, `param val`, or `fn`,
or fix concrete dependencies locally through `let` and `@(import(...))`.
Each choice trades definition-site concreteness for call-site flexibility in the opposite direction:

- **Parameterized components** (`param`, `param val`, `fn`) check separately
  against declared interfaces and can be instantiated across multiple arguments.
  During definition, however, the body sees only what its interface discloses:
  abstract witnesses remain opaque, equations not exported in the signature are unavailable,
  and closed annotations such as `@[monadic]` cannot implicitly capture enclosing bindings.
  Worse, every opening of a factory's existential result creates fresh abstract identities,
  forcing downstream modules to thread dependencies upward to a single composition root.
- **Locally bound components** (`let` with `@(import(...))`) retain their concrete definitions and benefit
  from [Copy → Resolve → Merge → Analyze](../references/language/src/sources/packages.md#copy-resolve-merge-analyze),
  which deduplicates identical package nodes and preserves shared nominal `def` identities across diamond imports.
  In exchange, the component fixes one static definition rather than varying by caller.

This trade-off is fundamental to separate checking, yet in `lib/std/` it causes disproportionate friction
because `param val` currently bundles three unrelated concerns behind `builtin : @(import("builtin.zy"))`.
Modules that neither vary per caller nor consume host capabilities still wrap themselves in factories,
splitting data definitions from their operations and forcing callers through cascading `builtin |>` wiring.

## Shared Rule: Separating Three Orthogonal Concerns

Decoupling standard-library organization requires treating three questions independently instead of routing all
of them through `param val`:

1. **Variance**: Does the component genuinely vary across instantiations (`Integer`/`Float` over scalar width,
   `MonadTrans` over a base monad, `Writer` over a `Monoid`, `Alloc`), does it require a runtime capability (`OS`,
   `Reader`, `Writer`, `Addr`, `io`, `fs`, `memory`), or does it have one canonical fixed semantics (`Bool`,
   `List`, pure arithmetic, `StateT`)?
2. **Identity and diamond sharing**: Should two consumers of the same logical component observe compatible types
   through merged definitions and applicative identity, or must each opening mint a fresh generative witness?
3. **Visibility and separate checking**: Must the implementation seal a representation invariant behind an existential
   or `.zyi` boundary, or should consumers see a transparent alias or nominal `data`/`codata` definition?

Canonical fixed-semantics definitions need neither parameterization nor generative sealing.
True multi-instantiation templates need `fn` or `param val` with narrow signatures.
Only runtime capability boundaries and sealed invariants need `param val` or existential packaging,
and their intermediate diamond wiring should not force pure libraries into factory waterfalls.

## Observed Evidence in the Standard Library

An audit of `lib/std/` shows three distinct patterns behind the current split:

### 1. Dead Parameters

Several modules accept `builtin` or canonical type parameters that their implementations do not require:

- **Pure effect modules**: [`ret.zy`](../../lib/std/effect/ret.zy), [`reader.zy`](../../lib/std/effect/reader.zy),
  [`writer.zy`](../../lib/std/effect/writer.zy), [`option.zy`](../../lib/std/effect/option.zy),
  [`state.zy`](../../lib/std/effect/state.zy), [`exception.zy`](../../lib/std/effect/exception.zy),
  and [`state-exn.zy`](../../lib/std/effect/state-exn.zy) wrap their implementations
  in `val ((/OS = _; builtin) : @(import("../builtin.zy"))) => ...` solely to forward `builtin` to `ret.zy`,
  which immediately discards it.
- **Numeric builders**: [`integer.zy`](../../lib/std/numeric/integer.zy)
  and [`float.zy`](../../lib/std/numeric/float.zy) take an unused `Builtin` parameter alongside `Bool : VType`
  and `String : VType`, even though `Bool` and `String` are already canonical
  in the [core prelude](../references/language/src/sources/core-prelude.md#vocabulary-and-lookup).

### 2. False Parameterization from Bundling Pure Operations into Builtin

[Core prelude](../references/language/src/sources/core-prelude.md) moved canonical primitive types out
of `Builtin` through `@(intrinsic(...))` in [`primitive.zy`](../../lib/std/primitive.zy)
and [`prelude.zy`](../../lib/std/prelude.zy).
Static calculation in [`size.zy`](../../lib/std/memory/size.zy) stays a concrete `let` module only because the four
[integer value intrinsics](../references/language/src/value-functions-and-views.md#value-matches-and-integer-intrinsics)
(`int_add`, `int_sub`, `int_and`, `int_compare`) are already canonical intrinsics.

By contrast, pure *runtime* primitive operations (`numeric/*/add`, `eq`,
`lt`, `to_string`, `char/*`, and pure `string/*` operations) remain fields
of [`builtin.zy`](../../lib/std/builtin.zy) alongside genuine host capabilities (`OS`,
`Reader`, `Writer`, `Addr`, `memory`, `io`, `fs`, `stdio`, `args`, `random`, `process`).
That coupling propagates parameterization across pure libraries:

- [`data/package.zy`](../../lib/std/data/package.zy) wraps `Bool`, `Option`, `Either`, `List`, and all their operations
  in `make : Builtin -> DataPackage` solely because `List.length` and `List.get` call `numeric/int/{add,sub,eq,lt}`.
  This forced [`bool.zy`](../../lib/std/data/bool.zy), [`option.zy`](../../lib/std/data/option.zy),
  and [`either.zy`](../../lib/std/data/either.zy) to split type declarations away from their operations
  and duplicate a `DataPackage` contract.
- [`effect/nondet.zy`](../../lib/std/effect/nondet.zy) depends on `Builtin` and `DataPackage` only for `List`
  and `numeric/int` inside `observe_many_t`.
- [`numeric/package.zy`](../../lib/std/numeric/package.zy) and the `char`/`string` parts
  of [`text/package.zy`](../../lib/std/text/package.zy) remain `Builtin`-parameterized only because scalar operations
  and text operations live inside `Builtin`.

### 3. Generative Factory Waterfalls Across Diamonds

Every `param val` factory returning an existential package—such as `make_data`, `make_bytes`, `make_types`,
`make_storage`, and `make_field`—is generative: opening the factory result at two sites creates incompatible abstract
witnesses
([Module Interfaces and Shared Openings](../references/language/src/polymorphism-and-packed-values.md#module-interfaces-and-shared-openings)).
Intermediate libraries therefore cannot import and instantiate upstream factories locally with `let`; instead they must
take every transitive dependency as another `param val` parameter so a top-level wiring hub
([`std.zy`](../../lib/std/std.zy) or [`memory/package.zy`](../../lib/std/memory/package.zy)) can open each factory once.

[`memory/record.zy`](../../lib/std/memory/record.zy), [`memory/header.zy`](../../lib/std/memory/header.zy),
and [`memory/product-codec.zy`](../../lib/std/memory/product-codec.zy) exhibit the steepest cost:
up to six cascading `param val` binders (`builtin`, `types`, `Field`, `DynamicField`, `field_ops`, `storage`),
supported by duplicated contract definitions (`Types`, `FieldOperations`, `StoragePackage`, `BytesPackage`,
`DataPackage`) authored mainly to type those intermediate parameter lists.

## Phased Roadmap

### Phase 1A: Stdlib Dead-Parameter Cleanup (Zero Compiler Changes)

Remove parameters that are already unused without altering the compiler or bootstrap prelude:

1. **Pure effect modules**: Remove the `Builtin` parameter
   from `effect/{ret,reader,writer,option,state,exception,state-exn}.zy`, remove `builtin |>`
   at internal `ret.zy` imports (including inside `effect/nondet.zy`), and update callers, templates, tests,
   and documentation to import them directly with `let`.
2. **Numeric builders**: Remove the unused `Builtin` parameter and the canonical `Bool`
   and `String` parameters from [`numeric/integer.zy`](../../lib/std/numeric/integer.zy)
   and [`numeric/float.zy`](../../lib/std/numeric/float.zy),
   and update [`numeric/package.zy`](../../lib/std/numeric/package.zy).

### Phase 1B: Pure Primitive Operations as Canonical Intrinsics (Compiler + Stdlib)

Move pure computational operations on canonical primitive types out of `Builtin`
into canonical `@(intrinsic(...))` terms, matching how canonical kinds and types moved
in [core prelude](../references/language/src/sources/core-prelude.md).
Expose them through a pure primitive-operations package (extending [`primitive.zy`](../../lib/std/primitive.zy)
or adding a sibling leaf) while keeping [`builtin.zy`](../../lib/std/builtin.zy) strictly for runtime capabilities:
`Addr`, `Reader`, `Writer`, `OS`, `memory` (including `store_le`/`load_le`), `io`, `fs`,
`stdio`, `args`, `random`, and `process`.

Target outcome in `lib/std/`:

- Reunite `data/` types and operations into concrete `let`-importable packages (`def List` sharing identity
  via package merging), retiring `DataPackage` and `make_data`.
- Convert [`effect/nondet.zy`](../../lib/std/effect/nondet.zy),
  [`numeric/package.zy`](../../lib/std/numeric/package.zy) (minus byte codecs),
  and the `char`/`string` modules of `text/` into concrete `let`-importable packages.
- Keep `numeric/integer.zy` and `numeric/float.zy` as genuinely parameterized internal templates,
  and keep `text/bytes.zy`, `memory/*`, `numeric/codecs.zy`, and `system/*` capability-parameterized.

#### Open Design Decisions Before Starting Phase 1B

1. **Intrinsic naming collision**: `@(intrinsic(int_add))`, `@(intrinsic(int_sub))`,
   `@(intrinsic(int_and))`, and `@(intrinsic(int_compare))` already denote `ValueIntOp` total value functions
   (`val pi (left : Int) (right : Int) . Int` in [`lang/abi/src/builtin.rs`](../../lang/abi/src/builtin.rs)),
   whereas runtime arithmetic operations are suspended computations (`Thk (Int -> Int -> Ret Int)`).
   Decide how to name, namespace, or unify total value intrinsics vs.
   computation-level primitive intrinsics.
2. **Builtin contract migration**: Decide whether pure computational roles are removed from `BuiltinValueRole`
   and `builtin.zy` outright—updating `lang/abi`, `lang/ir`, `lang/dynamics`, `lang/stackir`, `lang/cranelift`,
   `lang/wasm-sps`, and `lib/tests/builtin/*`—or retained through a deprecation window.
3. **Unified `data/` and dictionary signatures**: Decide whether `List` becomes a single `def List` sharing identity
   by package merging (matching `Bool`, `Option`, `Either` outside the bootstrap prelude
   in [`lang/session/src/source/prelude.rs`](../../lang/session/src/source/prelude.rs))
   and whether `numeric/dictionary.zy` drops its `Bool` parameter in favor of canonical `Bool`.
4. **Capability split inside `Builtin`**: Decide whether memory capabilities (`Addr`, `memory`,
   `store_le`/`load_le`) should be separable from OS/IO capabilities (`OS`, `Reader`, `Writer`, `io`, `fs`,
   `stdio`, `args`, `random`, `process`) so pure memory modules need not carry `OS` in their parameter contracts.

### Phase 2: Diamond Wiring for Capability and Invariant Modules (Open Language Design)

After Phase 1B, capability-bearing and invariant-sealing modules (`text/bytes.zy`, `memory/*`,
`system/*`) still form diamond dependency graphs whose nodes seal abstract types.
Two design directions remain open:

#### Option A: Applicative Sealing for Pure `val pi` Factories

In ML module theory, pure functors can safely be **applicative**: applying a pure factory `F` twice to equivalent static
paths yields equal abstract types. Zydeco already isolates total value transformations (`val pi`) from effectful
computations (`Thk`) and already uses
[applicative nominal translation](../references/language/src/translations/monadic-blocks.md#applicative-nominal-translation)
for `@[monadic]` blocks.

Under this option, generative `exists`/`pack` stays available
where equal inputs must still produce distinct types (for example, `storage/constant/create 8 8`
in [Manual Memory](../references/library/src/memory/manual-memory.md#independent-storage-and-codecs)
or independent Builtin openings), while pure module constructors such as `make_types`, `make_bytes`, `make_storage`,
and `make_field` can produce applicative abstract types indexed by the factory and static argument paths.
Intermediate modules like `record.zy`, `header.zy`, `slice.zy`, `allocation.zy`, `text/package.zy`,
and `system/package.zy` then accept only the root capability (`builtin` or `Addr`/`memory`),
bind `let bytes = make_bytes builtin` locally, and share unified abstract witnesses
without forwarding six intermediate `param val` packages.

#### Option B: Package-Level Parameterization

Zydeco's [source-package model](../references/language/src/sources/packages.md#the-opaque-root) already defines
namespace substitution (`ρB ↦ ρA/vendor/b`) and structural merging of identical resolved package graphs.
Extending packages with overridable import slots (virtual packages
with optional defaults) would let libraries use `let` + `@(import(...))` uniformly
while letting consumers swap implementations and retain automatic diamond merging.

This choice trades off against separate checking and runtime capabilities:

- Checking an overridable slot against a `.zyi` companion eliminates manual wiring
  while preserving separate compilation, **but** the module body remains abstract against `.zyi` during definition.
- Omitting the signature and re-checking per override gives full definition-site transparency against the default,
  **but** sacrifices separate-checking guarantees when overridden.
- Package substitution handles static library swaps, whereas runtime host capabilities (`Builtin`) still enter
  as values at the program boundary, where Option A applies directly.

## Target Governing Convention

Once Phase 1A and Phase 1B (and ideally Phase 2) land, the choice between `let` and `param`/`fn` follows one rule:

| Component dependency profile | Binding mechanism | Rationale |
| --- | --- | --- |
| Canonical types, pure data, pure primitive ops, pure effects (`Bool`, `List`, `String`, `Int`, `StateT`, `NonDet`) | **`let` + `@(import(...))`** | One canonical meaning; nominal/transparent identity shared automatically by package merging; zero wiring. |
| Genuine multi-instantiation templates (`numeric/{integer,float}`, `MonadTrans`, `Writer` over `Monoid`, custom `Alloc`) | **`fn` / `param val`** with narrow signature | Abstraction during definition is the desired parametricity boundary; never parameterize canonical types. |
| Runtime capabilities and sealed invariants (`Builtin`, `Addr`, `OS`, `Bytes`, `memory/*`, `system/*`) | **`param val` at capability roots** (+ Phase 2 diamond sharing) | Preserves explicit authority and representation invariants without forcing intermediate modules through multi-layer `param` funnels. |

## Validation Criteria

- **Phase 1A**:
  - `provider_independent_libraries_need_no_manual_primitive_import` verifies
    that `effect/{transformer,ret,reader,writer,option,state,exception,state-exn}.zy` check with no dependency
    on `builtin.zy`.
  - Paired tests verify direct import of pure effect modules succeeds while applying them to `builtin` is rejected.
  - `zydeco check -p /std`, numeric tests, effect tests, and `cargo xtask docs check` pass.
- **Phase 1B**:
  - Pure `data/` modules, `effect/nondet.zy`, `numeric/package.zy` (excluding byte codecs),
    and `text/{char,string}` check without importing `builtin.zy`.
  - Paired tests establish identity sharing of `List` across independent `let` imports
    and preserve `tyck.static-elimination` boundaries between static value intrinsics
    and runtime computation intrinsics.
  - All `std`, `builtin`, `effects`, `nondet`, and backend execution tests pass.
- **Phase 2**:
  - Two independent local applications of an applicative factory (or package-slot substitution)
    to the same capability path unify their sealed types, while applications
    to distinct capability openings remain rejected with `tyck.type-mismatch`.
  - Per-instance generative seals (`storage/constant/create`) continue to reject cross-opening unification.
