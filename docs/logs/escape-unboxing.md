# Escape Analysis and Unboxing Worklog

Date: 2026-08-16

## Goal

Implement the escape-analysis and unboxing design in
`docs/proposals/escape-unboxing.md`: values that do not escape can avoid GC heap
cells, and variables bound to such values can be expanded into field slots when
every use is a projection.

## What changed

- Added `lang/assembly/src/unbox.rs`, a local SPSLow analysis that marks:
  - `VCons` values directly consumed by a matching `VCons` pattern;
  - `ClosurePackage` values directly consumed by `OpenClosure`;
  - the environment value/pattern pair inside such an `OpenClosure` when both are
    `VCons`;
  - variables bound by `LetValue` to a `VCons` when every use is a projection.
- Wired the analysis into `lang/assembly/src/lower.rs`.
- Assembly lowering now skips `PackProduct` for unboxed values and skips
  `UnpackProduct` for unboxed patterns.
- `OpenClosure` skips the two-word closure unpack when the package is unboxed.
- `ProductLayout` carries a `stack_alloc` flag, and the AMD64 emitter can allocate a
  product in the current stack frame instead of calling `zydeco_gc_alloc`.
- Added end-to-end fixtures:
  - `lib/tests/compile/direct-tuple.zy`;
  - `lib/tests/compile/direct-closure.zy`.
- Registered both fixtures in `lang/tests/tests/compile.rs`.

## Observations

SPSLow's single-occurrence invariant makes the local analysis simple: a value node
that is directly consumed by a matching pattern cannot be observed through another
path, so the representation change is local.

The `tuple.zy` fixture exercises variable-level unboxing: `let x = (10, 0)` followed
by `let (y, z) = x` now expands `x` into two field slots and avoids the heap cell.

## Validation

- `cargo test -p zydeco-assembly` passes.
- `cargo test -p zydeco-tests --test compile` passes.
- Manual ZASM inspection confirms direct tuples, direct closures, and variable-bound
  tuples no longer emit `pack <product:2/2>` / `unpack <product:2/2>` for the
  unboxed values.

## Next steps

- Extend the analysis across block boundaries for values passed through known
  continuations.
- Use the `stack_alloc` path for non-escaping values that need a stable pointer.
