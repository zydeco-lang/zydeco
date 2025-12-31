# Design

Zydeco is a proof-of-concept programming language based on call-by-push-value
(CBPV). It is used to study stack-manipulating computation and relative monads,
with a focus on a small, executable core and a clear compilation pipeline.

## Language Model

Zydeco separates values from computations.

- Values are inert and include variables, thunks, units, products, data
  constructors, and literals.
- Computations are effectful and include forcing thunks, function application,
  do-bindings, and returning values.

The core types include:

- `Thk <B>` to suspend computations as values.
- `Ret <A>` to return values as computations.
- `OS` to represent computations that run against the operating system stack.

The main program is required to have type `OS`.

## Relative Monads and Monadic Blocks

Relative monads are defined as codata in the standard library (see
`lib/oopsla/core.zydeco`). Zydeco also implements *monadic blocks*, a
generalized do-notation. A monadic block is translated during type checking
via the algebra translation implemented in `lang/statics/src/monadic.rs` and
invoked in `lang/statics/src/tyck.rs`.

Monadic blocks are designed to be closed in the paper, but the artifact allows
use of global types and terms. These global definitions are reinterpreted under
the block's ambient monad during translation.

## Implementation Architecture

Zydeco is implemented as a pipeline of phases:

1. parsing (`lang/surface/src/textual`)
2. desugaring (`lang/surface/src/bitter`)
3. name resolution (`lang/surface/src/scoped`)
4. type checking (`lang/statics/src`)
5. linking (`lang/dynamics/src`)
6. evaluation (`lang/dynamics/src`)

The phases are spread across three core crates:

- `zydeco-surface` (surface syntax, parsing, desugaring, name resolution)
- `zydeco-statics` (static semantics and algebra translation)
- `zydeco-dynamics` (operational semantics and interpreter)

Common patterns in each phase include `syntax`, `arena`, `err`, `fmt`, and
`span` modules.

## Repository Layout

- `lang/`: language implementation and tests.
- `lib/`: Zydeco standard library and examples (including `lib/oopsla`).
- `cli/`: command-line interface for running and checking programs.
- `docs/`: literate Zydeco tutorial material (see `docs/spell`).
- `editor/`: editor integrations (TextMate grammar and VSCode extension).
- `web/`: web interface.

## Current Limitations

The artifact documents a few important limitations:

- Zydeco is not compiled to low-level code yet; it is interpreted.
- The package manager supports only local dependencies.
- `module`, `pub`, and `use` are reserved but not implemented.
- Debug builds use a larger stack to avoid overflow on large tests.
- Monadic blocks pass monad instances at runtime; inlining is not implemented,
  and only global definitions can be referenced inside blocks.
