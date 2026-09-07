# Compile-Time Normalization

Consider a library that exports a transparent type together with values using that type:

```zydeco
-- counter.zy
begin
  let Counter = Int64 that
  let zero : Counter = 0 that
  let inc : Thk (Counter -> Ret Counter) =
    { fn n => ! add n 1 }
  that

  (= Counter, = zero, = inc)
end
```

The library author determines the package signature assigned to the resulting term:

```text
exists (= Counter as Int64 : VType).
  (zero :: Counter) *
  (inc :: Thk (Counter -> Ret Counter))
```

A consumer binds the library as a parameter without repeating that package signature:

```zydeco
begin
  param (= Counter, = zero, = inc) that

  let one : Counter = 1 that
  ! inc one
end
```

The consumer may annotate the `param` pattern explicitly.
When the annotation is omitted, the compiled provider package signature supplies the expected type
from which it is inferred.
Opening the parameter therefore contributes `Counter ≡ Int64`, `zero`, and `inc` to the block context.
The consumer is checked using the package signature produced by the library, and linking applies the consumer
to the library implementation.

## Static Elimination and Residual Code

Transparent type equations explain one part of compile-time normalization.
The same phase boundary must also account for value functions and package composition:
their static structure is resolved before execution, while ordinary value fields
and explicitly authored computations may remain.
This is a static elimination requirement, independent of whether a source term appears
in a product, an argument, a result, or a package field.
The rules in this section define the common phase contract; [value functions and views](value-pi.md)
and [package modularization](package-modularization.md) specify the constructs and interfaces governed by it.

### Static Reduction

Value functions and packages may therefore be freely composed within the supported static fragment.
The static evaluator handles lexical bindings, type and value beta reduction, structural projections,
and package introduction and opening, carrying functions and mixed static/runtime structures in its environment.
A compiler-internal closure for this evaluator is not a runtime closure in the emitted program.
Its lexical environment may contain references to residual runtime values,
which remain shared when the static function uses them more than once.
Static function parameters may range over other functions, and static results may themselves be functions.
Matching a known constructor specializes the corresponding arm: inspecting its tag is pure,
and the chosen arm's computations remain suspended in the residual program.
An unknown scrutinee retains its runtime match and any remaining literal comparisons.

Runtime inputs need not be known as concrete values for that reduction to succeed:

```text
(val x => (x, x)) y  ==>  let x = y in (x, x)
```

The application disappears even when `y` is supplied at runtime.
The residual binding and product preserve the runtime data flow.
Likewise, a package may contain an erased type identity, a statically eliminated value function,
and an ordinary runtime thunk.
Its components need not all belong to one phase merely because they share a source-level product.
Normal forms may retain abstract type identities and variables at runtime value types.
Static elimination does not require replacing an abstract type with its concrete representation.

### Residual Validation and Execution

The shared boundary has the following obligations:

- Static reduction terminates for its admitted fragment and produces well-typed residual code.
- Executable runtime values contain no surviving `ValPi` function or unresolved static package component.
  Explicit computation thunks, their captures, and ordinary package payload data may remain.
  An eliminated value function contributes no runtime closure, environment tuple, or indirect application of its own.
- Residualization preserves lexical binding, runtime value sharing, and the order and multiplicity of effects.
  It never runs a computation or general recursion to discover a static function or witness.
- An unresolved static requirement reports the missing information at its source span.
  It does not implicitly convert to a runtime object or thunk.
  Checking an executable, interpreting it, and compiling it through different backends must agree on this boundary.

These obligations are enforced after the specified static reductions, not by blanket bans
on source occurrences and not by optional backend optimizations.
An imported library may export an unapplied static function or a package containing one.
Its parameters are discharged when a consumer instantiates it; residual validation applies
to the resulting executable boundary, not to an unapplied library interface.
Additional reductions across computation forms need their own soundness argument and do not follow merely
from the requirement that value formation is total.

### Explicit Runtime Boundary

Runtime implementations are exposed as computation contracts stored behind `Thk`.
The callable may be selected dynamically while the type evidence needed to instantiate its signature remains static.
An explicit adapter can close over a static module while compiling a runtime thunk.
The adapter must satisfy the same residual validation:
`Thk` does not excuse an unresolved `ValPi` value inside its residual body.
[Package Modularization](package-modularization.md#explicit-runtime-contracts) specifies these runtime interfaces,
their `pi`/`forall` adapters, and the scope conditions for hidden types.

### Implementation Status

The arena-wide finalizer below normalizes kinds and types.
During dependent application checking, `elaborate::static_values` also inspects value structure
with the same lexical reducer to recover package witnesses through higher-order applications,
products, and named projections.
This inspection leaves computations opaque and exposes only witnesses already visible in the caller's scope;
opening and repacking a hidden runtime package cannot disclose its witness.
After local checking and coverage validation, `elaborate::static_values` evaluates the checked source
with lexical static closures and shared references to runtime values.
It creates fresh typed residual nodes in the checker's unpublished arena
and records the executable root alongside the original source root; source annotations
and query facts remain available for inspection.
Both interpreter linking and SPS lowering select that residual root.
The type lint checks both representations, while SPS demand analysis consumes the residual program directly.

Runtime reification rejects surviving value functions and runtime interfaces requiring their representation
with `tyck.static-elimination` at the responsible source term.
Residual computation classifiers receive the same representation check,
including result contracts after polymorphic instantiation.
Unapplied static library exports remain available to consumers and interactive inspection.
The old occurrence validators and backend-specific definition-spine resolver have been removed.

Recursive type aliases can encode self-application even without value-level `fix`.
The implementation bounds each reduction request to 128 nested applications
and 65,536 applications, including view applications.
Exhausting either limit during source residualization reports `tyck.static-elimination`;
an evidence inspection that cannot finish supplies no witnesses to the dependent application checker.
These resource bounds protect compiler termination without claiming a termination proof for arbitrary recursive types.
Computation application, `do`, forcing, effects, and general recursion remain residual forms;
the evaluator does not execute them to obtain static information.
The [value-function implementation account](value-pi.md#implementation-boundary)
and [package implementation account](package-modularization.md#current-implementation-and-validation) record
the construct-specific regression cases and remaining optimizations.

## Package Signatures

A package signature is an ordered telescope whose later entries may refer to earlier type entries.
In the following metalanguage, `type` and `val` distinguish the sorts of entries;
they do not introduce a declaration sort into the source language.

```text
Θ ::= ·
    | static X as D : S; Θ
    | type X : K; Θ
    | type X as A : K; Θ
    | val x : A; Θ
```

`static X as D : S` binds a transparent compile-time component,
where the classifier `S` may be `Set` or an ordinary kind.
`type X : K` is abstract. `type X as A : K` is manifest and contributes the definitional equality `X ≡ A`.
The `as` keyword states an equation on an otherwise ordinary existential type binder.
A value entry may carry ordinary runtime data or a value function used during static composition;
its type may mention preceding type entries.
Its source-level classifier is `#x :: A`, while `#x = v` introduces the corresponding named value.
Only its representable residual data survives static elimination.

The package signature can be represented by nested package and product types:

```text
⟦ type X : K; Θ ⟧      = exists (X : K). ⟦ Θ ⟧
⟦ type X as A : K; Θ ⟧ = exists (X as A : K). ⟦ Θ ⟧
⟦ static X as D : S; Θ ⟧ = exists (X as D : S). ⟦ Θ ⟧
⟦ val x : B; Θ ⟧       = (#x :: B) * ⟦ Θ ⟧
```

This translation preserves the order of the telescope and permits type and value entries to be interleaved.
It does not require a normal form in which every existential precedes every product.

The second line is the package representation of a manifest type.
Its witness is packaged like an existential type component, but its defining equation is disclosed
by the package signature.
Here `X` is an ordinary type variable. Manifest existential types add only the equation `X ≡ A`.
The `as` decoration belongs to the binder payload and composes with ordinary pattern structure rather
than introducing a separate naming form:

```text
exists (#field = ((X as A) : K)) . B
exists (= X as A : K) . B  ≡  exists (#X = ((X as A) : K)) . B
```

The second line uses ordinary named-pattern punning.
Its leading `=` derives the field from the head binder `X`, while `as A` and `: K` remain decorations of that payload.

## Manifest Static Fields

The classifier of a manifest field may be omitted when the definition determines it:

```text
Γ ⊢ D : S
Γ, X ≡ D : S ⊢ B : VType
────────────────────────────────
Γ ⊢ exists (X as D). B : VType
```

This judgment applies uniformly when `D` is a kind classified by `Set` and
when `D` is a type classified by an ordinary kind.
The explicit form `exists (X as D : S). B` remains available and checks the stated classifier.

The canonical Builtin signature uses the inferred form for the leading CBPV universe components:

```zydeco
exists
  (VType as @(intrinsic(vtype)))
  (CType as @(intrinsic(ctype)))
.
  ...
```

The Builtin surface manifestly re-exports these kinds together with the intrinsic `Thk`, `Ret`, and `Unit` types.
Fixed representations such as `Int64` are likewise canonical intrinsics exposed as manifest fields on that surface.
These fields are transparent and erased; they neither create fresh identities nor contribute witnesses
to a package-dependent arrow.
Only provider-owned capabilities such as `Reader`, `Writer`, and `OS` retain ordinary abstract existential semantics.

## Manifest Types

A manifest type in a package signature has the following formation rule:

```text
Γ ⊢ A : K
Γ, X : K, X ≡ A ⊢ B : VType
────────────────────────────────────────
Γ ⊢ exists (X as A : K). B : VType
```

Introduction checks that the supplied witness agrees with the disclosed type:

```text
Γ ⊢ W : K
Γ ⊢ W ≡ A : K
Γ ⊢ v : B[W/X]
──────────────────────────────────────────────────
Γ ⊢ (W, v) : exists (X as A : K). B
```

Eliminating the package binds `X` as a transparent alias of `A` and checks the payload at `B[A/X]`.
It does not create a fresh skolem, so the result may mention `X` wherever it may mention `A`.
The type component is erased after checking.

Ordinary existential elimination remains unchanged.
Opening `exists (X : K). B` creates a fresh abstract identity and applies the usual non-escape check.
When a computation parameter pattern opens that identity, `PackPi` records the witness telescope
and scopes it over the result classifier; `ValPi` records the corresponding evidence for a value function.
At a package-dependent call, the result may mention only witness identities available in the caller's static scope.
A dynamically selected thunk of the package-dependent arrow is permitted: checking uses its signature,
without evaluating or statically identifying its implementation.
Manifest types do not add `PackPi` witnesses.

The two forms may occur in one telescope:

```text
exists
  (Key : VType)
  (Map as Tree Key : VType)
.
  API Key Map
```

Opening this package creates an abstract `Key` and then binds `Map ≡ Tree Key`.
Only `Key` belongs to the `PackPi` witness telescope. If no abstract witnesses remain, a computation arrow
over the manifest package is accepted whenever erasing its static prefix leaves a representable payload.
For now, `PackPi` opens only a leading existential prefix.
Supporting abstract existential components nested beneath preceding value products is deferred;
this implementation limit does not impose a normal form on package signatures.

## Package Signatures as Expected Types

A package is an ordinary term checked against an expected signature built from existential and product types.
A transparent type component may inhabit a manifest existential, whereas a nominal
or explicitly sealed component inhabits an abstract existential.

An explicitly annotated parameter checks its annotation against the provider package signature.
An unannotated parameter is accepted when that package signature is available as its expected type.
If neither an annotation nor an expected provider package signature is available, the parameter type cannot be inferred.
This is expected-type propagation from the dependency's package signature, rather than inference from uses in the body.

For now, the design assumes one global compilation context in which the provider signature is available
before the consumer is checked.
Separate compilation and persistent interface artifacts are deferred.

Just to be clear, package checking is independent of block elaboration.
The `begin ... end` block only arranges the bindings contributed by `that`;
it neither determines external visibility nor constructs a package interface.

## Sealing and Erasure

Sealing explicitly forgets a manifest equation:

```text
exists (X as A : K). B
    ⟶
exists (X : K). B
```

Opening the sealed package creates a fresh abstract identity.
The reverse conversion is unavailable because an abstract package signature carries no representation equation.
Sealing has no runtime behavior.

Manifest types are also erased:

```text
| (W, v) | = | v |
```

The compiled package signature retains `X ≡ A`, while the executable package retains only its value fields.
Static-only value-function fields are also eliminated under the revised composition rule;
ordinary runtime fields and explicit runtime contracts keep their residual representation.
Before publishing the package signature, normalization substitutes provider-local transparent names and ensures
that every disclosed right-hand side is closed over the public static context.
Normalization may simplify an equation the provider exposes, but it must not recover an equation hidden
by ordinary existential sealing.

## Arena-Wide Finalization

Type checking constructs types as an arena-backed directed acyclic graph.
Many arena IDs therefore share the same tails, especially the nested products
and existentials used for package signatures.
Once local inference closes, hole solutions are fixed for the remainder of the check,
and the resolved or normalized form of a type ID is a stable function of that ID and the frozen solution table.

Finalization exploits that stability with one pass-wide context.
Hole resolution maintains a shared map from each visited `TypeId` to its resolved `TypeId`
and a set of unresolved fills.
It walks every arena root, rebuilds only paths whose children changed,
and reuses the result whenever another root reaches an already visited node.
After resolution completes, filled normalization uses one shared kind map
and one shared type map for the complete arena.
New nodes produced by beta reduction, projection, or structural rebuilding join the same memoized graph.

The pass preserves three invariants:

- every original arena ID remains a valid lookup key, even when its stored structure is replaced by a resolved form;
- `kinds_normalized` and `types_normalized` contain deltas for finalized IDs whose pre-normalization form changed,
  while unchanged IDs expose their existing arena node directly; and
- missing solutions and sort mismatches remain checker diagnostics rather than partial normalized values exposed
  to later compiler phases.

For a graph with `V` type and kind IDs and `E` child edges,
structural finalization takes `O(V + E)` work plus the intrinsic cost of reductions that create new nodes.
A separate memo per arena root instead performs the sum of all reachable subgraphs,
which approaches quadratic work for long package-signature spines.
Sharing the context is therefore the algorithmic correction.
Internal arena maps and finalization working sets use FxHash because their keys are compiler-owned identities
or structural queries and do not require a denial-of-service-resistant hasher.
This improves the constant factor without weakening the semantic identity boundary;
denser tables remain a possible representation change where key-space-aware sparse storage is unnecessary.

This eager arena-wide pass is an intermediate architecture.
A future query-driven semantic model may normalize only closed types requested by a consumer,
as described in the [query-owned statics](query-owned-statics.md).
The shared finalizer establishes the same essential boundary now: inference mutates local facts,
then finalization reads a stable solution graph and publishes reusable semantic forms.
