# Escape Analysis and Unboxing for Zydeco

Status: draft; core unboxing is implemented.

## Summary

Zydeco's native backend allocates every escaping product, constructor, and closure package in a fixed two-space
copying heap.
The call-by-push-value pipeline already places control and continuation structure on the machine stack,
but values that are created and consumed locally still consume region space.
This proposal adds a constraint-based escape analysis over SPSLow and an unboxing rewrite
that lets non-escaping values live as multiple stack or environment words instead of as region pointers.

The analysis classifies each value node and each value variable with a representation choice
from `{unboxed, stack, region}`.
SPSLow's single-occurrence invariant makes the analysis precise: every value node is consumed exactly once,
and sharing is explicit through variables.
The implementation covers direct pack/unpack fusion and variable-level unboxing for variables
whose uses are all projections.

## Motivation

The current AMD64 backend lowers `PackProduct` to a call to `zydeco_alloc_scanned`.
That is correct, but it is also unnecessary for values that never escape their creating frame.
Two common patterns pay this cost today:

```zydeco
let x = (10, 0) in
let (y, z) = x in
...
```

and

```zydeco
do x <- ! { fn (x : Int64) => ret x } 0;
! (process/exit) x
```

In both cases the value is constructed and immediately projected or forced.
A stack machine already has the fields available; the heap cell only adds allocation
and eventual copying work.
Unboxing removes the cell and keeps the fields in the machine's natural stack-shaped representation.

## Background

Zydeco's compiler lowers checked CBPV through branch-join SPS into first-order SPSLow and then into ZASM.
Relevant facts:

- SPSLow is first-order.
  Closures are explicit `ClosurePackage` values and continuations are explicit `ContinuationPackage` stacks.
- `SpsLowValidator` enforces that every `ValueId`, `CompuId`, `StackId`,
  and `VPatId` occurs exactly once in the lexical IR.
  Sharing is represented by `LetValue` binders and `DefId` variables.
- Assembly lowering turns every `VCons`, `Ctor`, and `ClosurePackage` into a `PackProduct` instruction.
- The AMD64 emitter calls `zydeco_alloc_scanned` for every region-allocated `PackProduct`.
- The runtime provides two fixed semispaces and uses Cheney copying collection when the active space fills.
  SPSLow product layouts carry only their physical arity. Odd runtime words are tagged immediates;
  aligned even words can be managed pointers, so the collector needs no compiler-generated pointer maps.

The CBPV value/computation distinction is what makes this design attractive: computation is already stack-shaped,
while values are inert and can be flattened when they do not need a stable pointer identity.

## Design

### Analysis site

The analysis runs on SPSLow, before assembly lowering.
SPSLow is the right boundary because:

- it still has static types and product layouts;
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

If a value `w` has a field `v`, then the representation of `v` must be compatible
with every possible representation of `w`:

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

If `rep(v) = S`, lowering keeps a single pointer but allocates the cell
in the current stack frame instead of calling `zydeco_alloc_scanned`.
The AMD64 emitter supports this via a `stack_alloc` flag on `ProductLayout`.

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

## Implementation Status

The following is implemented:

- Local `VCons` pack/unpack fusion for directly projected values.
- `ClosurePackage` fusion for directly forced closures.
- Variable-level expansion for `LetValue`-bound variables whose uses are all projections.
- A `stack_alloc` flag on assembly `ProductLayout` and AMD64 emission support for stack-frame product allocation.

The analysis lives in `lang/assembly/src/unbox.rs` and is consumed by `lang/assembly/src/lower.rs`.
See `docs/logs/escape-unboxing.md` for the worklog.

## File Touchpoints

| Area | Change |
| --- | --- |
| `lang/assembly/src/unbox.rs` | representation analysis and variable expansion metadata |
| `lang/assembly/src/lower.rs` | skip `PackProduct` / `UnpackProduct` for unboxed values; expand unboxed variables |
| `lang/assembly/src/analyze.rs` | recognize stack-allocated product slots |
| `lang/assembly/src/syntax.rs` | `ProductLayout.stack_alloc` flag |
| `lang/amd64/src/emit.rs` | stack-frame product allocation path |
| `lang/tests/tests/compile.rs` | regression fixtures for direct tuples and closures |

## Validation

- Existing interpreter and native tests must remain green.
- Unit tests in `lang/assembly/src/unbox.rs` check the local analysis.
- End-to-end tests cover direct tuple projection, direct closure forcing, and the existing variable-bound tuple fixture.
- Manual ZASM inspection confirms that unboxed values no longer emit the corresponding `pack` / `unpack` instructions.

## Alternatives Considered

**Performing the analysis on ZASM instead of SPSLow.** This is possible, but ZASM has already lost the direct value tree
and requires reconstructing producer/consumer relations over a control-flow graph.
SPSLow's single-occurrence invariant makes the analysis simpler and more precise.

**Using a single three-point lattice instead of allowed sets.** A single `{U, S, R}` lattice is simpler
but conflates two independent questions: whether a value escapes and whether it needs a stable pointer.
Allowed sets keep both dimensions explicit.

**Reboxing at escape points.** A more aggressive design would allow a value to be unboxed
on non-escaping paths and boxed again when it reaches an escaping sink.
This proposal keeps the current implementation conservative and boxes from the start,
avoiding duplicated representations.

## Open Questions

- How should unboxed values interact with host calls that expect pointer arguments?
  The current implementation marks all extern arguments as `R`.
- How much of the analysis must cross recursive `Fix` blocks before the results are useful?
- How much fixed environment space should one function activation be allowed to use?
  Tail calls reuse the environment buffer, so only the largest live frame determines this pressure.

## Related Documents

- `docs/logs/paper-aligned-stackir.md` records the SPSLow boundary this analysis builds on.
- `docs/legacy/ideas/products.md` explains the canonical product layouts that unboxing must respect.
