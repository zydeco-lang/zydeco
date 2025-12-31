# Dynamics (Runtime Semantics)

This crate implements the dynamic semantics for Zydeco: it links the
type-checked program into a runtime-friendly representation and evaluates it
with a small-step interpreter. The pipeline produces a `DynamicsArena` from
the statics pass, wires in builtin primitives, and then steps computations to
produce program continuations or exit codes.

## Role in the pipeline

```
textual -> bitter -> scoped -> tyck -> dynamics
```

The dynamics arena is built from the statics output, with builtin primitives
resolved by name.

## Key components

The crate is intentionally small and tightly scoped.

- The `syntax` module defines the runtime AST, semantic values, and `Runtime`.
- `link` lowers typed syntax into dynamic declarations and injects builtins.
- `eval` implements the stepper that executes dynamic computations.
- `builtin` and `impls` provide primitive operations (arithmetic, strings, IO).
- `fmt` offers a debug formatter for dynamic terms.
