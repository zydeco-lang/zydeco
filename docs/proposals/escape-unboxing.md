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
Frame-resident products, mixed raw/reference fields, and layout-directed machine calls remain deferred:
they need lifetime or entry evidence that the current policy cannot supply.
The [workflow](../../CONTRIBUTING.md#representation-experiments) gives reproduction and validation commands.

## Representation contracts at call boundaries

The [explicit storage library](bytes.md#explicit-storage-contracts) raises a related question:
can a caller use its chosen representation directly as a function argument or result?
That would make representation choice part of an interface rather than merely an allocation optimization.
The implemented source interface now shares storage evidence across calls using the existing word transport.
Selecting a different physical calling convention remains a separate compiler extension.

The implementation carries several complementary descriptions:

| Description | Evidence available today | What it does not establish |
| --- | --- | --- |
| Source `Representation A` and `Storage A Stored` | An abstract carrier shared by codecs and call signatures, runtime size/alignment | A statically selected argument width or register class |
| Source `Plan A` | Validated byte placement computed by value functions; inspectable widths and offsets | Type-level identity of a particular placement, reference maps, or register classes |
| SPSLow `ProductLayout` | Logical product arity and explicit producer/consumer structure | Byte offsets, padding, or scalar register classes |
| SPSLow word entries | Ordered environment/result words and checked code/package provenance | Complete source stack protocols or a different component transport |
| SPSLow partial source protocols | Known value components, argument/continuation protocols, and cyclic codata observations retained across normalization | Nominal storage identity, unresolved type-family instantiation, or physical stack extent |
| Native frame and root plans | Live tagged-word slots, entry roles, and suspension/resumption ownership | A mixed layout containing raw scalars alongside managed references |

For example, the ordinary logical type `UInt8 * UInt32` can have natural storage of eight bytes,
or a 64-byte aligned representation with tail padding.
Choosing the latter storage contract does not change the logical function type,
nor does it make a stored byte handle occupy 64 bytes in a Zydeco call frame.
The caller and callee currently agree on the existing word convention in either case.
Local product unboxing can remove a cell while still passing tagged field words;
it does not interpret the memory library's padding and alignment operations.

### Stored call interfaces

A consumer needs to name the chosen carrier without creating another abstract opening.
The storage library therefore factors its existing package
into `Representation A = exists (= Stored : VType) . Storage A Stored`.
The [storage design](bytes.md#descriptions-computations-and-abstract-storage) owns that dictionary and its laws.
A composition root opens a representation once and passes `Stored` and the dictionary to its workers.
Those workers can live in separately checked sources and export `Thk` computations over that same carrier.

The ordinary [call library](../../lib/std/memory/call.zy) defines the computation protocol:

```zydeco
Function A B R = A -> Thk R -> Thk (B -> R) -> R
```

It receives an argument, a failure continuation, and a continuation accepting its result.
`R : CType` describes the required residual stack; using `OS` or `Ret Int64` does not require another adapter design.
The protocol itself places no purity, termination, or single-invocation requirement on user-authored workers.
The library's adapters perform the following sequences, forwarding the same failure continuation at every step:

| Operation | Resulting interface | Sequence when each preceding stage succeeds |
| --- | --- | --- |
| `between A B Input Output input output`, then `encode R logical` | `Thk (Function Input Output R)` | Load input; invoke logical worker; store output; invoke result continuation |
| The same boundary, then `decode R encoded` | `Thk (Function A B R)` | Store input; invoke stored worker; load output; invoke result continuation |
| `compose A B C R first second` | `Thk (Function A C R)` | Invoke first; forward its result directly to second |
| `convert A From To source target R` | `Thk (Function From To R)` | Load with source; store with target |

Construction is by value functions; execution occurs only when the resulting thunk is forced.
Runtime dictionaries and dynamically selected worker thunks may be captured by these adapters.
No metadata arithmetic is needed to forward a stored value, and composition inserts no codec conversion itself.
`convert` preserves the logical value through decoding and encoding; it does not reinterpret bytes or equate carriers.
For a worker whose interface already uses the desired carriers, an ordinary call passes them directly.

For example, after opening a representation of `Record = UInt8 * UInt32`:

```zydeco
let boundary = calls/between Record Record Stored Stored repr repr in
let increment = boundary/encode OS {
  fn (tag, payload) no yes =>
    do next <- ! numeric/uint32/add payload 1;
    ! yes (tag, next)
} in
! increment stored failure { fn result => ... }
```

Here the argument and result have the same abstract `Stored` type.
The [checked example](../../lib/tests/std/represented-call/main.zy) imports this kind of worker,
passes its result to recursive polymorphic code, and selects an alternative thunk at runtime.
The alternative explicitly converts a 16-byte aligned record into a 64-byte aligned record and back.
Both thunks expose the original carrier, so selection and continuation calls agree on their interface.
The example also dynamically chooses a provider package of the ordinary form:

```zydeco
exists (Stored : VType) . Storage Record Stored * Thk (Function Stored Stored OS)
```

Opening that package gives its consumer a coherent codec and worker even when the provider's byte layout is unknown.
Packaging preserves agreement by carrying both together; it does not recover an unknown witness from metadata.

Identity is deliberately nominal at this boundary.
Tests reject arguments, result continuations, and composed workers from independently opened representations,
including different alignment, different field width, and identical placement opened twice.
Matching callers share the opening; numerical equality of sizes, alignments, or shapes never introduces type equality.
Caller-authored storage dictionaries still carry the law obligations documented in the storage design.
The checker does not prove that two implementations at one carrier use identical codecs.

The tests run direct calls and dynamic selection on the interpreter, AMD64, and both Wasm backends,
including every configurable word policy on AMD64 and AM Wasm.
They check canonical bytes after conversion and failure propagation through each adapter stage in `Ret Int64`.
This establishes a usable source interface with conservative identity checking.
It does not establish a new machine ABI: each stored value remains the existing immutable-buffer handle.
An encoded logical worker still performs a load and a store, and its decoded wrapper adds a store and a load.
Those explicit conversions may allocate and are not removed merely because the carriers match.

### Checked word entry experiment

The compiler now represents the administrative part of a call as explicit SPSLow entry parameters and transfers.
[C9's word entry contract](../references/compiler.md#word-entry-contracts) owns their order,
provenance checks, and lowering rules.
This makes the existing convention inspectable with `zydeco build --target zir` and checked
before either assembly lowering or direct SPS Wasm emission.

The useful distinction is between the environment/result words introduced by closure conversion
and the source computation's remaining stack protocol.
The former have fixed roles even when the source worker or its representation provider is selected at runtime.
The partial source protocol extension below supplies known parts of the latter.
The administrative contract checks entry roles and package agreement independently of that source evidence.
It leaves ordinary argument consumption in the block body and retains the current word transport.

Direct entries, recursive labels, and package openings now supply code evidence to the verifier.
Regression tests pair accepted transfers with mismatched entry kinds, environment arities,
crossed closure environments, and replaced or partially consumed continuation stacks.
Aliases preserve the association when their producer is known.
The existing execution fixtures exercise recursive and dynamic calls on all backends;
policy tests keep wide captured values live across native collection.
These checks improve the compiler boundary without making a new runtime representation or performance claim.

### Partial source protocol experiment

Source protocol evidence survives lowering, normalization, and closure conversion.
[C9's partial source protocols](../references/compiler.md#partial-source-protocols) own the descriptors,
propagation, and validation rules.
They require no new source annotations.
This gives known scalar, product, thunk, and recursive codata components a checkable interface at direct
and indirect transfers, while keeping unknown remainders explicit.

The boundary deliberately preserves the distinction between a computation protocol and its physical stack extent.
[Ret and stack extent](../references/language.md#ret-and-stack-extent) explains why a
return continuation cannot establish a frame boundary.
The implemented descriptor records what that continuation accepts; its hidden saved stack contributes no size
or allocation claim.

The [source regression](../../lib/tests/core/stack-protocols.zy) defines a recursive codata protocol
with `.item : Int64 -> Stream` and `.done : Ret Int64` observations.
Its producer pushes a runtime-selected number of item arguments and tags,
and its consumer drains them before delivering a result to the installed continuation.
The retained graph connects the item observation back to the same stream interface.
The consumer is selected at runtime, returned as a thunk, and passed into the recursive producer;
the graph reference survives each of those boundaries.
The same program returns a dynamically selected worker thunk and calls it
through a recursive forwarder polymorphic in its residual computation protocol.
Tests inspect retained entry evidence and execute different stack depths on the interpreter,
AMD64, and both Wasm backends.
Mutated low IR is rejected for known argument/result conflicts, invalid observation names or indices,
malformed recursive tails, incomplete cases, missing graph definitions, and inconsistent entry metadata.
Graph comparisons exercise distinct but equivalent recursive descriptions, including cycles through returned thunks,
and reject conflicts reached after a recursive back edge.

The [declaration-order regression](../../lib/tests/core/codata-order.zy) exposed a related lowering defect:
two structurally equal codata interfaces could assign different runtime indices to the same destructor.
Canonical observation numbering now keeps those calls coherent across all backends.
This is evidence for sharing one descriptor between producers and consumers, including the tag identity itself.

This establishes partial source agreement through the existing word ABI.
It does not select byte layouts or raw slots, equate abstract carriers, or infer physical stack size.

### Remaining machine-call boundary

The [local analysis](../../lang/assembly/src/unbox.rs) deliberately treats call arguments and argument-stack uses
of a variable as escapes, and the [lowerer](../../lang/assembly/src/lower.rs) consumes its local pack/unpack decisions.
SPSLow does not retain a source carrier's layout as a shared component contract for indirect calls or returns.
Changing only the caller's packing would therefore change the stack shape expected by existing callees.

Automatic layout-directed calls are deferred.
A modular extension needs the following boundaries in order:

1. **Representation identity from source composition.** The implemented
   [static layout plans](bytes.md#static-layout-plans) calculate and validate scalar/product byte placement
   with ordinary value functions, expose its shape, and use the same codecs as runtime layout construction.
   They establish the source-calculation part of this prerequisite.
   The stored-call interface supplies a shared nominal carrier for particular source contracts.
   What remains is compiler-consumable placement evidence, including reference-bearing components.
   `Plan A` alone supplies neither: different placements for `A` share its type,
   and a transported plan may have runtime metadata.
   Abstract source carriers currently erase.
   An entry contract needs explicit evidence at the call boundary; a compiler policy cannot infer permission
   to change an ABI from an arbitrary `Int64` field.
2. **One entry contract for both ends.** The checked word entry experiment establishes administrative roles
   and package agreement; partial source protocols now preserve known argument/result
   and recursive observation structure.
   Extend that boundary with representation evidence shared by direct calls,
   indirect calls, closures, and return continuations.
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

The stored-buffer experiment covers the first source agreement tests, including direct and dynamic calls,
polymorphic callers, recursive calls, and logical adapters.
Before accepting another machine transport, its tests must also retain managed fields
across collection and verify that padding or raw scalar bits never become roots.
The current experiment inherits the existing handle/root contract; it does not exercise mixed raw/reference slots.

The accepted recursive protocol experiment retains observation transitions
and rejects incompatible known transfers while accepting dynamic depth.
The remaining protocol gap is instantiation: unresolved computation/type witnesses
and applied recursive families can still leave opaque components.
Any extension should preserve their binding relationships without unbounded specialization of recursive applications.
Representation identity and mixed reference layouts still need their own evidence
before a Rust policy can choose another component transport;
partial compatibility involving unknowns cannot justify specialization.
Static layout identity by structural equality, dependent size proofs, raw scalar slots,
mixed reference layouts, and C aggregate classification remain separate open work.
The accepted source library needs none of those mechanisms to express stored interfaces today.

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
Use the [word entry contract](../references/compiler.md#word-entry-contracts) for administrative parameters
and [partial source protocols](../references/compiler.md#partial-source-protocols) for known argument components.
Argument consumers in the body contribute local constraints; counting them cannot reconstruct an unknown interface.
The analysis needs a worklist over blocks:

1. Compute the constraints inside each block from its entry patterns and argument consumers.
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
