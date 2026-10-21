# Implicit Core Prelude

This proposal adds syntactic sugar for a few lines of ordinary Zydeco code:
open commonly used names from an explicitly supplied, provider-relative packed value.
Each use has exactly the semantics of that explicit code, with no project-level prelude configuration.
The feature is unimplemented;
the existing [source-boundary rules](../references/language.md#source-boundaries) remain authoritative
until adoption and implementation.

## Motivation and Governing Rule

Ordinary Zydeco code repeatedly introduces the same names for CBPV kinds, primitive types, and computation protocols.
Repeating those bindings adds little information once their provider is known.
Choosing a provider and sharing its abstract opening, however, establishes
which capabilities and identities those names refer to.
Removing repetitive bindings must preserve that dependency.

The governing rule is **implicit bindings, explicit provider, preserved opening**.
The abbreviation fixes a set of names, not globally fixed definitions for those names.
It opens a prelude view supplied by the chosen provider using ordinary lexical bindings.
The type checker needs no ambient provider environment, special equality rule, or implicit capability acquisition.

### Vocabulary Relative to a Provider

Prelude membership is a convention for commonly used names within a composition,
not a claim that their meanings are independent of every provider.
A provider's prelude can contain both compiler-canonical primitives and aliases to its own abstract capabilities.
What matters is that each alias retains the identity expected by that provider's operations.
Different providers can supply different definitions behind the same names.

`OS` illustrates this distinction. Naming it permits signatures such as `Thk OS`;
it does not supply a computation of that type or a host operation such as `exit`.
Its identity nevertheless matters: a continuation accepted by one provider need not be accepted by another.
The prelude therefore exposes that provider's `OS`, not a new universal root protocol.
Its static nature concerns binding names, not the purity of computations described by those names.

The library convention is to build a prelude view from an already shared provider opening.
Manifest fields disclose equations such as `OS = HostOS`, where `HostOS` can remain abstract.
They do not disclose its representation, create another abstract witness, or identify it with another provider's `OS`.
The same opening continues to govern the operations selected explicitly alongside the view.

## Syntactic Expansion

The proposed `@[prelude(core, from(p))]` meta annotation abbreviates a fixed projection pattern over `p`.
Here `p` is an ordinary identifier resolving to a packed prelude view in lexical scope.
There is no implicit choice of provider or lookup of a distinguished `builtin` variable.
Bind any provider selection, import, or adapter application explicitly before using the annotation.

For example, a composition root can construct a prelude view from its existing Builtin opening:

```zydeco
param (/process; builtin) : @(import(/std/builtin)) in
let (/make = make_prelude) = @(import(/std/prelude)) in
let prelude = make_prelude builtin in
@[prelude(core, from(prelude))]
let stop : Thk (Int -> OS) = process/exit in
! stop 0
```

The `builtin` whole-value alias retains the opening that supplied `process`.
The explicit adapter application preserves that opening in its result.
Only the annotation is sugar: for any annotated term `e`, its entire semantic expansion is:

```zydeco
let (
  /VType; /CType; /Thk; /Ret; /OS; /Unit;
  /Int8; /Int16; /Int32; /Int64; /Int;
  /UInt8; /UInt16; /UInt32; /UInt64; /UInt;
  /Float32; /Float64; /Char; /String;
  /Bool; /Option; /Either
) = p in
e
```

The provider expression is not inferred or reconstructed by this expansion.
The identifier in `from(p)` resolves in the surrounding scope before the introduced bindings,
just as the bindee of the displayed `let` does.
Only the core names are introduced; their usual lexical shadowing behavior is unchanged.
The sugar does not add exports to the authored result.

The annotation acts only where it is written. Without it, no prefix is inserted.
There are no project defaults, inherited profiles, or `none` overrides.
The required arguments are `core` and one `from(identifier)`; missing, duplicate,
unknown, or malformed arguments are diagnosed.
There is no provider-free `prelude(core)` fallback.
Nested uses expand at their respective positions over the values they name.
Clients can always write the explicit opening instead, or select only the fields they need.

Provider imports and selection follow ordinary dependency resolution and lexical scope.
The abbreviation creates no import, factory application, or provider instance.
For a manifest prelude view, the projection substitutes its existing equations without opening another provider witness.
It adds no setting to `Project`, no special namespace policy, and no installed-library fallback or network lookup.

## The Core Names

The `core` abbreviation selects exactly the names in this table, in the displayed order.
The table describes the supplied standard-library adapter; its intrinsic equations follow the existing language rules.
Other providers supply their own views, checked by the same projection and use-site typing rules as handwritten code.
The abbreviation does not assign identity based on a field's spelling.

| Names | Meaning in the standard view | Source |
| --- | --- | --- |
| `VType`, `CType` | The value and computation kinds | Manifest kind exports from the primitive leaf |
| `Thk` | `Thk B` is the value type of suspended computations with protocol `B` | Manifest export of `intrinsic(thk)` |
| `Ret` | `Ret A` is the computation type whose protocol expects a continuation accepting an `A` | Manifest export of `intrinsic(ret)` |
| `OS` | The selected provider's root computation protocol, of kind `CType` | Manifest alias to the `OS` witness from the shared provider opening |
| `Unit` | The intrinsic unit value type | Manifest export from the primitive leaf |
| `Int8`, `Int16`, `Int32`, `Int64`, `Int` | Signed primitive value types | Manifest exports of `i8`, `i16`, `i32`, `i64`, `int` |
| `UInt8`, `UInt16`, `UInt32`, `UInt64`, `UInt` | Unsigned primitive value types | Manifest exports of `u8`, `u16`, `u32`, `u64`, `uint` |
| `Float32`, `Float64`, `Char`, `String` | Floating-point and text primitive value types | Manifest exports of `f32`, `f64`, `char`, `string` |
| `Bool`, `Option`, `Either` | Transparent data types of kinds `VType`, `VType -> VType`, and `VType -> VType -> VType` | Manifest re-exports of the adapter's existing data definitions |

The constructors have kinds `Thk : CType -> VType` and `Ret : VType -> CType`.
Their protocol meanings follow the [language model](../references/language.md#1-conventions-and-the-language-model).
Introducing these static aliases performs no computation;
`Ret` itself is [not evidence of purity](../references/language.md#ret-and-explicit-cps).

Every selected name comes from the supplied packed view, including `VType` and `CType`.
Although [`pack` currently introduces only type witnesses](../references/language.md#9-polymorphism-and-packed-values),
an explicitly annotated packed value can supply manifest kind witnesses as well.
No new kind-witness syntax or intrinsic is required.

### Preserving the Provider's `OS`

[Builtin](../../lib/std/builtin.zy) introduces `SystemOS` as an abstract witness tagged `@[builtin(os)]`
and re-exports it as `OS`.
That representation and its existing host-operation and executable-entry checks remain intact.
The prelude view exposes a manifest equation to the selected opening's witness;
it neither changes the Builtin contract nor requires a canonical `intrinsic(os)`.
The [current capability rules](../references/language.md#13-primitive-values-and-capabilities) continue to apply.

A provider may supply the view directly beneath its outer witness telescope.
Then select the view, required operations, and any whole-value alias in one projection group.
Alternatively, a composition root can construct the view by passing that whole-value alias
to an explicit adapter, as in the example above.
Both routes preserve one opening rather than opening an independently acquired provider.

Matching field names, source paths, or package layouts do not establish shared identity.
Two independent openings can yield incompatible `OS` witnesses even when they originate from the same package.
Repeated views derived from one retained opening must instead preserve the same witness.
Opening a provider separately for the prelude and for its operations is not a substitute for sharing that evidence.
Never seal the view's `OS` behind a fresh existential or reconstruct the provider to populate the prelude.

### The Primitive Leaf

The data definitions need primitive kinds and types before the complete prelude can exist.
Centralize those definitions in `lib/std/primitive.zy`, declaring the library package `/std/primitive`.
This leaf exports the intrinsic kinds and types in the table, excluding `OS`, `Bool`, `Option`,
and `Either`, using manifest kind and type equations followed by a `()` payload.
It binds these definitions directly with `@(intrinsic(...))` and has no imports or companion dependencies.
It contains no data declarations, abstract witnesses, host capabilities, operations, or factory parameters.

The following reduced example shows the introduction form, retaining four of the leaf's type exports:

```zydeco
let VType = @(intrinsic(vtype)) in
let CType = @(intrinsic(ctype)) in
let types =
  pack (= Thk as @(intrinsic(thk))) (= Ret as @(intrinsic(ret)))
    (= Unit as @(intrinsic(unit))) (= Int as @(intrinsic(int)))
  where () end
in
((VType, CType, types) :
  exists (VType as @(intrinsic(vtype))) (CType as @(intrinsic(ctype))) . @[typeof] types)
```

The outer annotation discloses the kind equations; the inner `pack` supplies the type equations.
Ordinary projection patterns can then select `/VType`, `/CType`, and the type fields from one opening.
The distinction is in how the provider introduces its witnesses, not in how consumers obtain the names.

### The Provider-Relative Prelude View

Add `lib/std/prelude.zy`, declaring the library package `/std/prelude`, as an ordinary adapter package exporting `make`.
Its result is the packed prelude view; importing the adapter does not select a host provider or create that view.
The adapter imports the primitive leaf and the existing [`Bool`](../../lib/std/data/bool.zy),
[`Option`](../../lib/std/data/option.zy), and [`Either`](../../lib/std/data/either.zy) definitions
by relative file paths.
It accepts an explicitly supplied Builtin package through the ordinary value-function interface.

A reduced adapter corresponding to the leaf example and the `Option` definition illustrates the composition:

```zydeco
let (/VType; /CType; /Thk; /Ret; /Unit; /Int) = @(import("primitive.zy")) in
let (/Option) = @(import("data/option.zy")) in
let make =
  begin
    param val (/OS) : @(import("builtin.zy")) that
    let types =
      pack (= Thk as Thk) (= Ret as Ret) (= OS as OS)
        (= Unit as Unit) (= Int as Int) (= Option as Option)
      where () end
    in
    ((VType, CType, types) :
      exists (VType as VType) (CType as CType) . @[typeof] types)
  end
in
(#make = make)
```

The manifest `OS` equation in the result refers to the argument's witness.
Passing a whole-value alias from the composition root preserves its relationship to that root's selected operations,
using the existing [shared-opening rules](../references/language.md#module-interfaces-and-shared-openings).
The value function is total and statically eliminated; the caller writes its application explicitly.
Providers with different interfaces can supply an equivalent view directly or through their own explicit adapters.
The sugar does not require the standard adapter or a Builtin argument specifically.

The complete standard view exports every kind and type name in the table once, followed by a unit payload.
It contains manifest aliases, including aliases to the provider's abstract witness, rather than fresh abstract fields.
It contains no operation fields, thunks, or hidden factory parameters.
These are contracts of the supplied library adapter, checked with ordinary annotations and library regression tests.
The desugaring performs no additional provider validation: its behavior is exactly the displayed projection.

The three data-definition sources import only the primitive leaf for their primitive bindings.
They do not depend on a host capability or the full prelude adapter.
For example, `data/option.zy` remains a host-independent packed value:

```zydeco
let (/VType; /Unit) = @(import("../primitive.zy")) in
let Option (A : VType) =
  data
  | +None : Unit
  | +Some : A
  end
in
pack (= Option as Option)
where () end
```

The selected core names exclude `Addr`, `Reader`, `Writer`, `builtin`, stream and memory operations,
numeric operation dictionaries, and value-level arithmetic intrinsics.
They also exclude `List`, `Bytes`, `Monad`, `Algebra`, monad dictionaries, and effect instances.
This is the proposed convenience set, not a rule that provider-relative types cannot appear in a prelude.
Operations and other modules remain explicitly selected; data constructors such as `+Some` follow ordinary data typing.
Changing the core name set changes lexical scope and requires the same review as other changes to the abbreviation.

## Source Boundaries and Type Identity

Expansion preserves ordinary lexical scope and independent source inference.
An imported source does not inherit a provider or prelude value from its importer.
It must bind any such dependency through its own ordinary parameters or imports.
A companion is checked independently of implementation bindings.
An independently selected nested package does not inherit an enclosing prelude annotation or provider binding,
just as it does not inherit an enclosing handwritten `let`.

Other meta annotations compose with the expanded code at their authored positions.
Prelude handling does not move annotations across binders or reinterpret their subjects.
In particular, it neither supplies a monadic basis nor changes the ordinary `@[monadic]` capture rules.
The generated bindings are subject to the same checks as handwritten code.

### Type-Only Results

A term using a supplied prelude view can explicitly package a type-only result.
The following excerpts assume `prelude` is already bound in their surrounding scope:

```zydeco
@[prelude(core, from(prelude))]
let Callback = Thk (Int -> OS) in
pack (= Callback as Callback)
where () end
```

The following counterpart is rejected because the expanded packed-value opening has a type-valued body:

```zydeco
@[prelude(core, from(prelude))]
Thk (Int -> OS)
```

A raw contract instead omits the sugar and uses ordinary explicit type parameters or intrinsic aliases.
The host-independent primitive leaf can be used directly when a packed type-exporting result needs no provider.
Raw type-root declarations remain an exception to centralizing intrinsic aliases in that leaf:
opening its packed value would impose the same value-`let` body constraint.
The ordinary requirement that a `.zyi` source denote a type remains unchanged.

An exported type that mentions the provider's `OS` retains that dependency.
A source must accept the provider through an appropriate dependent interface or keep the result
within its opening's scope; packaging an alias does not authorize an abstract witness to escape.
Diagnostics should identify the generated opening and apply the ordinary scope and result-sort rules.
Do not relax the value-`let` body rule, silently wrap a type in `pack`, or retry without expansion after checking fails.

### Shared and Abstract Identities

Manifest openings disclose equations; they do not introduce fresh abstract witnesses.
Consumers of views derived from the same retained provider opening therefore share its `OS` identity.
Consumers of independently opened providers do not acquire that equality merely by using the same abbreviation,
importing the same adapter source, or obtaining views with the same field names.
The provider and any dependent operations must be forwarded together across composition boundaries.

Compiler-canonical primitives retain their existing identities.
Transparent data definitions share identity according to their resolved definitions
and the existing [copy, resolve, merge, analyze](../references/language.md#copy-resolve-merge-analyze) rules.
Neither fact turns a provider-specific abstract witness into a canonical type.
`Addr`, `Reader`, `Writer`, `Bytes`, and `List` retain their existing explicit identity boundaries as well.

## Bootstrap, Execution, and Tooling

The source dependency graph stays acyclic:

```text
prelude.zy -> primitive.zy
prelude.zy -> data/{bool,option,either}.zy -> primitive.zy
prelude.zy -> builtin.zy (the explicit parameter contract)
```

The primitive leaf has no outgoing dependencies and contains no `OS` definition.
Data definitions need only that leaf; the adapter receives provider-specific witnesses through its explicit argument.
Raw Builtin contracts retain their bootstrap bindings and do not import the adapter.
These sources simply omit the prelude sugar; there is no bootstrap override or loader policy.
Any source cycle receives the ordinary dependency-cycle diagnostic without dropping an import.

The prelude view carries only static kind and type aliases plus unit.
Static elimination removes its explicit value-function construction and opening, leaving no initialization computation,
runtime dictionary, extra parameter, or runtime metadata attributable to the sugar.
The provider and operation dependencies were already explicit and remain so.
An executable still accepts its Builtin package and ends in that opening's `OS` protocol.
All backends consume the same expanded program; none implements prelude lookup or injection.

Retain the annotation's location as the origin of generated bindings.
Diagnostics identify the supplied view and, through ordinary binding provenance, its provider opening.
Hover, completion, and go-to-definition use those resolved identities,
marking shorthand-introduced bindings accordingly.
Inspection can show the equivalent explicit projection.
Formatting preserves authored sugar instead of writing generated bindings into the source.

## Implementation Plan

1. **Supply the primitive leaf and provider adapter.** Implement `lib/std/primitive.zy`
   with existing intrinsic kind/type exports and a unit payload, without `OS` or provider dependencies.
   Update the three data-definition modules to import that leaf, retaining their existing definitions.
   Implement `/std/prelude` with an explicit `make` value function whose manifest result retains its argument's `OS`.
   Keep the Builtin abstract-witness contract and its validation unchanged.
   Test direct provider-supplied views and adapter-derived views against the shared-opening rules;
   do not add a prelude-specific provider validator or a canonical OS intrinsic.

2. **Implement one syntactic expansion.** Add the annotation and its required lexical `from` argument
   to the [meta annotation catalog](../../lang/surface/src/metadata.rs), including diagnostics and editor descriptions.
   Decode it into a domain type and replace each use with the fixed projection bindings over that identifier.
   Resolve the identifier as an ordinary lexical bindee, not as a global provider setting or an import path.
   Reuse shared traversals, keeping annotation analysis independent from reconstruction, and use an `Explicit` folder
   where suitable under the [traversal selection](../references/compiler.md#choosing-and-composing-traversals)
   and [resumable execution](../references/compiler.md#resumable-folder-execution) contracts.
   Do not insert provider acquisition, factory application, or project configuration.

3. **Use that expansion across frontends.** CLI, REPL, editor, inspection,
   and embedded callers must see the same expanded term and ordinary binding identities.
   Retain annotation provenance and use existing dependency tracking for the explicit provider and adapter code.
   Update examples and user documentation together.
   After approval and complete implementation, move settled rules into the appropriate references,
   removing this proposal's superseded account and updating incoming links.

## Validation and Acceptance Criteria

The central oracle is equivalence with handwritten code: replacing the sugar
by its specified projection must preserve accepted classifiers, rejected programs,
witness identity, dependency resolution, and execution behavior.
Use focused [source tests](../../lang/session/src/source/tests.rs),
[package tests](../../lang/session/src/source/package/tests.rs), [CLI tests](../../cli/tests/package.rs),
editor tests, and language regressions under `lang/tests/`.
Each rejected case asserts the intended diagnostic and absence of a successful downstream artifact.

| Boundary | Accepted case | Rejected counterpart or failure invariant |
| --- | --- | --- |
| Expansion | Sugar and its handwritten opening have equivalent classifiers and behavior | Missing or ambiguous selected fields fail exactly as the explicit projection does; sugar acquires no provider and applies no factory |
| Provider selection | `from(p)` resolves to a lexical view supplied by the chosen provider | An unbound view or omitted `from` is diagnosed without looking for a global prelude or a conventional `builtin` name |
| Scope | Local names shadow supplied names; nested uses select their stated views | Imported sources and independently selected terms cannot borrow surrounding provider bindings |
| Shared opening | The prelude's `OS` and selected host operations come from one projection group, directly or through a retained whole-value alias | Mixing a prelude from one abstract provider opening with operations from another fails type equality, even for the same source package |
| Repeated views | Two manifest views derived from the same retained opening preserve one `OS` identity | An adapter must not reseal `OS`, instantiate a new provider, or use independently imported capability evidence |
| Provider variation | Distinct providers each type-check with their own prelude and operations | Matching field names and adapter source identity do not make their abstract witnesses interchangeable |
| Source sorts | A manifest type-exporting result stays within its provider scope or uses a suitable dependent interface | Raw type bodies fail the ordinary value-`let` rule, and provider witnesses cannot escape via a packaged alias |
| Library contracts | The primitive leaf exports existing intrinsic kinds/types without dependencies; the adapter discloses the supplied `OS` equation | Regressions catch new leaf dependencies, a fabricated OS definition, fresh abstract result fields, or accidental operations in the view |
| Capabilities | Programs select host operations and other modules explicitly | A prelude alone supplies no `stdio`, `process`, resource handles, or monadic basis |
| Tooling and refresh | Provider and adapter edits follow the same analysis, sharing, refresh, and editor paths as explicit code | A stale analysis cannot hide a newly incompatible provider/view pairing |
| Entry and lowering | Both versions satisfy the same provider-relative entry contract and have the same residual behavior | Sugar cannot make a result in another provider's `OS` valid, or add initialization, hidden arguments, or runtime metadata |

Include unannotated sources, independently selected packages, and REPL inputs in the integration cases.
Completion means that the abbreviation preserves ordinary composition and witness sharing
through every public entry point, without a separate policy that approximates those semantics.
