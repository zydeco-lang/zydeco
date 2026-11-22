# Dynamics (Runtime Semantics)

This crate implements the reference dynamic semantics for Zydeco:
it borrows a self-contained residual `zydeco_ir::CoreProgram`, materializes host Builtin packages
from validated `BuiltinPackagePlan` values, and evaluates computations with a small-step interpreter
to produce program continuations or exit codes.

## Role in the pipeline

```markdown
textual -> bitter -> scoped -> tyck -> ir -> dynamics
```

`Runtime` steps a borrowed `&zydeco_ir::CoreProgram` directly without copying front-end or static arenas.
A package-dependent executable receives its concrete host Builtin package list via `Runtime::with_packages`
before evaluation.

## Key components

The crate is intentionally small and tightly scoped:

- Public runtime modules: `env` (persistent radix-trie environments keyed by `zydeco_ir::VarId`),
  `syntax` (semantic values `SemValue`, `ThunkBody`, host continuations `ProgKont`,
  and `Runtime`), `host` (host-owned values and capabilities used by runtime primitives),
  `builtin` (registry for runtime builtin primitives), `foreign` (checked foreign call plans
  and Unix `libffi` dynamic-library execution), `eval` (the small-step evaluator over `&CoreProgram`),
  and `fmt` (debug formatting for runtime semantic values).
- Internal support modules: `impls` (builtin primitive implementations), `memory` (host heap/address primitives),
  and `representation` (boxed and structured value helpers).
