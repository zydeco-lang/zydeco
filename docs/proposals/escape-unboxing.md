# Escape Analysis and Unboxing for Zydeco

## Motivation and boundary

Local product and closure construction can often be eliminated when the value is immediately projected or forced.
[C10](../references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices)
owns the implemented pack/unpack fusion and variable-level projection analysis.
This proposal extends that boundary to constraint-based representation choices over SPSLow:
unboxed fields, a frame-resident cell, or a managed heap cell.
Stack-product allocation and interprocedural escape constraints are not implemented.

SPSLow has explicit closure captures and a single lexical occurrence per value node; sharing uses named variables.
That makes producer/consumer structure available before ZASM decomposes it into stack operations. The runtime's
[tagged-word and collection contract](../references/compiler.md#c12-shared-native-model-allocation-and-collection)
constrains boxed fields.
Native [activation lifetime](../references/compiler.md#c11-native-preparation-activation-frames-and-amd64-emission)
constrains frame pointers.
Neither contract by itself proves that a proposed stack cell cannot escape.

The constraint analysis below is a design for extending local selection.
The following experiment evaluates the implemented local boundary before extending it.

## Configurable local representation experiment

Different layout preferences should reuse the same validity evidence.
[C10's policy boundary](../references/compiler.md#policy-selection) now lets a Rust policy select
among locally justified opportunities, using either a policy type or a per-compilation enum.
This makes small experiments independently reviewable while preserving one lowerer and one tracing convention.
It configures compiler choices for runtime values; it does not parameterize the type checker's Rust arenas.

The accepted experimental extension is local closure splitting: retain the environment
and code as separate field words when the closure is only opened locally.
This reuses variable field slots and closure opening; it needs no new calling convention.
The environment transported to the code entry remains an ordinary value.
The existing default is retained because the benefit depends on what survives earlier SPS normalization.

The [comparison program](../../cli/examples/representations.rs) lowers each source once to SPSLow,
then measures the resulting portable assembly for all policies.
It also composes `Shared` with a const-generic field limit.
On the checked-in fixtures, product/closure allocation **sites** are:

| Source fixture | Boxed | Direct | Local | Shared | Shared, maximum 1 field | Shared, maximum 4 fields |
| --- | --- | --- | --- | --- | --- | --- |
| `core/representation-policies.zy` | 14 | 14 | 14 | 13 | 14 | 13 |
| `core/local-normalization.zy` | 2 | 2 | 2 | 2 | 2 | 2 |
| `core/gc-stress.zy` | 28 | 28 | 28 | 28 | 28 | 28 |
| `builtin/argument-contract.zy` | 59 | 59 | 59 | 59 | 59 | 59 |
| `std/storage-access.zy` | 85 | 85 | 85 | 85 | 85 | 85 |

These are code counts from this implementation, not runtime allocation or speed measurements.
The first fixture loses one two-word closure cell in its recursive walker;
the maximum-one-field policy declines that expansion.
It retains a runtime-derived wide integer and a second captured scalar across repeated traversal,
with enough environment allocations to exceed the native semispace capacity.
Native and AM Wasm tests check all four enum policies against the interpreter's result.
Local IR tests separately cover cases that earlier source normalization usually removes,
and block expansion when a value is passed, aliased, captured, or retained by a continuation.

This evidence supports an opt-in closure experiment and a reusable comparison boundary.
It does not establish a general speedup or justify a new default.
Frame-resident products, mixed raw/reference fields, and representation-aware calls remain deferred:
they need lifetime or entry evidence that the current policy cannot supply.
The [workflow](../../CONTRIBUTING.md#representation-experiments) gives reproduction and validation commands.

## Representation contracts at call boundaries

The [explicit storage library](bytes.md#explicit-storage-contracts) raises a related question:
can a caller use its chosen representation directly as a function argument or result?
That would make representation choice part of an interface rather than merely an allocation optimization.
The current implementation supplies useful storage evidence but does not yet supply a calling convention.

There are four different descriptions in the implementation:

| Description | Evidence available today | What it does not establish |
| --- | --- | --- |
| Source `Representation A` | An abstract stored type, runtime size/alignment, and checked codecs | A statically selected argument width or register class |
| Source `Plan A` | Validated byte placement computed by value functions; inspectable widths and offsets | Type-level identity of a particular placement, reference maps, or register classes |
| SPSLow `ProductLayout` | Logical product arity and explicit producer/consumer structure | Byte offsets, padding, or scalar register classes |
| Native frame and root plans | Live tagged-word slots, entry roles, and suspension/resumption ownership | A mixed layout containing raw scalars alongside managed references |

For example, the ordinary logical type `UInt8 * UInt32` can have natural storage of eight bytes,
or a 64-byte aligned representation with tail padding.
Choosing the latter storage contract does not change the logical function type,
nor does it make a stored byte handle occupy 64 bytes in a Zydeco call frame.
The caller and callee currently agree on the existing word convention in either case.
Local product unboxing can remove a cell while still passing tagged field words;
it does not interpret the memory library's padding and alignment operations.

The [local analysis](../../lang/assembly/src/unbox.rs) deliberately treats argument-stack uses of a variable
as escapes, and the [lowerer](../../lang/assembly/src/lower.rs) consumes its local pack/unpack decisions.
There is no shared representation-indexed entry contract for an indirect call or a returned product.
Changing only the caller's packing would therefore change the stack shape expected by existing callees.

Automatic layout-directed calls are deferred.
A modular extension needs the following boundaries in order:

1. **Representation identity from source composition.** The implemented
   [static layout plans](bytes.md#static-layout-plans) calculate and validate scalar/product byte placement
   with ordinary value functions, expose its shape, and use the same codecs as runtime layout construction.
   They establish the source-calculation part of this prerequisite.
   The remaining witness must distinguish particular representations in an interface
   and describe reference-bearing components.
   `Plan A` alone does neither: different placements for `A` share its type,
   and a transported plan may have runtime metadata.
   An entry contract needs explicit evidence at the call boundary; a compiler policy cannot infer permission
   to change an ABI from an arbitrary `Int64` field.
2. **One entry contract for both ends.** Extend the checked SPSLow boundary with ordered argument/result components
   and representation evidence shared by direct calls, indirect calls, closures, and return continuations.
   Unknown representations require a uniform transport or a checked adapter;
   separate callers cannot independently infer a different number of stack slots.
3. **Target placement and tracing.** Derive register/stack placement and exact live-reference maps together.
   Raw integer bits cannot enter scanned tagged-word slots merely because they have the same machine width.
   Native stack scanning and frame maps, and both Wasm backends, must consume the same component contract.
4. **Explicit conversion boundaries.** Specify when ordinary values are encoded,
   decoded, boxed, or copied at generic calls and escaped closures.
   A direct specialized call can then remove conversion only when both ends retain the same evidence.
   C aggregate classification remains the separate target ABI question
   in [the foreign-interface design](c-ffi.md#additional-abi-shapes).

The first implementation should cover one fixed scalar product passed to and returned from a known function,
then the same interface through a dynamically selected thunk.
Tests must pair accepted matching representations with rejected width/alignment mismatches,
retain managed fields across collection, and verify that padding or raw scalar bits never become roots.
Polymorphic callers, recursive calls, and ordinary boxed adapters must continue to agree on the entry contract.
Until these prerequisites are implemented, explicit buffers and typed codecs remain the accepted interface;
the local optimization described below can progress independently.

The next bounded experiment is a representation-indexed interface for one fixed scalar product,
initially transported through the existing stored-buffer handle.
That isolates identity agreement from target placement: matching interfaces should compose,
and a different alignment or field width should fail before lowering.
After that boundary is usable, the shared component contract can enable direct/indirect calls
to choose another transport under the existing Rust policy configuration.
Raw scalar slots and mixed reference layouts remain deferred until entry and tracing evidence agree.

## Design

### Analysis site

The analysis runs on SPSLow, before assembly lowering.
SPSLow is the right boundary because:

- it retains explicit product layouts and producer/consumer structure;
- closure conversion is complete, so closure environments are explicit;
- the single-occurrence invariant gives a precise producer/consumer relation;
- a later rewrite can change value representation without fighting a control-flow graph.

The output of the analysis is a side table mapping `ValueId` and `DefId` to a representation.
The assembly lowerer consults that table and emits unboxed values without `PackProduct` / `UnpackProduct`.

### Abstract domain

Each value or variable is assigned an allowed set of representations:

```text
Allowed(v) ⊆ {U, S, R}
```

where:

- `U` means unboxed: the value is represented by its fields directly, with no pointer.
- `S` means stack: the value is non-escaping but needs a stable single-word pointer into the current stack frame.
- `R` means region: the value may escape and must remain a region pointer.

The initial value for a locally constructed value is `{U, S, R}`.
The final choice is the best remaining representation:

```text
rep(v) = U  if U ∈ Allowed(v)
       else S if S ∈ Allowed(v)
       else R
```

### Field representation constraints

A boxed value stores each field as one tagged runtime word.
An unboxed value has no cell, so its fields are independent values.
Define:

```text
fields(U) = {U, S, R}
fields(S) = {S, R}
fields(R) = {R}
```

If a value `w` has a field `v`, propagate the remaining parent representation possibilities to that field:

```text
Allowed(v) := Allowed(v) ∩ ⋃_{ℓ ∈ Allowed(w)} fields(ℓ)
```

This single rule covers products, constructor payloads, and closure environments.

### Projection constraints

When a pattern projects a value, the same field rule applies from the pattern variables back to the scrutinee.
For example, `let (x, y) = v in M` adds:

```text
Allowed(x) := Allowed(x) ∩ ⋃_{ℓ ∈ Allowed(v)} fields(ℓ)
Allowed(y) := Allowed(y) ∩ ⋃_{ℓ ∈ Allowed(v)} fields(ℓ)
```

If a pattern binds the whole value through an alias, the alias needs a stable pointer:

```text
Allowed(alias) := Allowed(alias) ∩ Allowed(v)
Allowed(v)     := Allowed(v) ∩ Allowed(alias)
```

A whole-value use removes `U`:

```text
Allowed(v) := Allowed(v) \ {U}
```

### Escaping sinks

The following uses force a region representation:

- passing a value as an argument to an external call;
- storing a value as a field of a value whose representation is `R`;
- returning a value to a continuation that may store it or pass it to an unknown caller.

For external calls, the rule is:

```text
Allowed(v) := Allowed(v) ∩ {R}
```

For a field of a region value, the `fields(R)` rule already forces `R`.

### Interprocedural blocks

SPSLow blocks are entered by `Jump` with an explicit stack.
Each block entry is a sequence of `LetArg` patterns.
The analysis needs a worklist over blocks:

1. Compute the constraints inside each block from its entry patterns.
2. For every `Jump` to a block, intersect the allowed sets of the supplied values
   with the block's current entry requirements.
3. Repeat until no allowed set changes.

Because the domain has only three elements, the analysis terminates.

### Rewriting rules

If `rep(v) = U` for a `VCons` value, assembly lowering emits the fields directly and omits `PackProduct`.
If a `VPat::VCons` is unboxed, lowering emits the sub-patterns and omits `UnpackProduct`.
The same rule applies to `ClosurePackage`.

If a variable is unboxed, its binding expands into one field slot per logical element.
Uses of the variable push those slots back in the same order as an unboxed `VCons`.

The proposed `rep(v) = S` representation keeps a single pointer but allocates the cell
in the current stack frame instead of calling `zydeco_alloc_scanned`.
This representation requires the explicit lifetime and reclamation discipline described
in [native activation frames](native-frames.md#frame-lifetime-and-entry-invariants),
including proof that references cannot survive the owning frame.
Native caller environments already survive return continuations; choosing storage
for an individual product remains a decision here.
It is not implemented; current lowering uses unboxed fields or a heap cell.

## Worked Example

Consider `let x = (10, 0) in let (y, z) = x in M`.

- The bindee is `VCons(10, 0)` with `Allowed = {U, S, R}`.
- The variable `x` is used only as the scrutinee of a `VCons` projection.
- No external call or region store observes the pair.

The analysis keeps `U` for the bindee and expands `x` into two field slots.
Assembly lowering changes:

```text
push 0
push 10
pack <product:2/2>
...
unpack <product:2/2>
pop y
pop z
```

into:

```text
push 0
push 10
pop x#0
pop x#1
push x#1
push x#0
pop y
pop z
```

An immediately forced closure changes in the same way:

```text
push env
push code
pack <product:2/2>
unpack <product:2/2>
pop env
pop code
jump code
```

becomes:

```text
push env
push code
pop env
pop code
jump code
```

## Validation boundary

Use [C10's implementation map](../references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices)
for current local unboxing.
A stack-cell implementation additionally needs paired checks for operand order, frame cleanup, call alignment,
escaping references, and collection while a retained frame holds managed pointers.
Inspection must show the removed allocation, while unknown consumers retain boxing.
Interprocedural constraints need recursive and indirect-call counterexamples
before their results can select stack storage.

## Alternatives Considered

**Performing the analysis on ZASM instead of SPSLow.** This is possible, but ZASM has already lost the direct value tree
and requires reconstructing producer/consumer relations over a control-flow graph.
SPSLow's single-occurrence invariant makes the analysis simpler and more precise.

**Using a single three-point lattice instead of allowed sets.** A single `{U, S, R}` lattice is simpler
but conflates two independent questions: whether a value escapes and whether it needs a stable pointer.
Allowed sets keep both dimensions explicit.

**Reboxing at escape points.** A more aggressive design would allow a value to be unboxed
on non-escaping paths and boxed again when it reaches an escaping sink.
The proposed conservative choice boxes from the start on such paths, avoiding duplicated representations.

## Open Questions

- How should unboxed values interact with host calls that expect pointer arguments?
  The proposed conservative constraint forces external arguments to `R`.
- How much of the analysis must cross recursive `Fix` blocks before the results are useful?
- How much fixed environment space should one function activation be allowed to use?
  The current [retained-frame model](native-frames.md#collection-and-space-behavior) must account
  for active and suspended activations, packed capacity, and their live data.
  Retention does not authorize a raw frame pointer to escape its activation.

## Related Documents

- The Stack IR phase boundaries in [`DESIGN.md`](../../DESIGN.md) record the SPSLow invariants this analysis builds on.
- [C11](../references/compiler.md#c11-native-preparation-activation-frames-and-amd64-emission)
  and [C12](../references/compiler.md#environment-actions-and-roots) own current native lifetime and root contracts;
  [native frame design](native-frames.md) keeps the alternative storage requirements.
- [C10](../references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices) owns current
  product layouts; the older product exploration remains historical.
