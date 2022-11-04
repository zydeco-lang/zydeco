# Zydeco

A proof-of-concept language adopting call-by-push-value as its evaluation order.

## Repository Structure

```plain
.
├── Cargo.lock
├── Cargo.toml
├── compiler
│  ├── cases
│  ├── src
│  └── tests
└── README.md
```

`/compiler/`: compiler for Zydeco; uses lalrpop as its parser generator.

- `src/`:
  - `lib.rs`: the top-level module for all compiler utilities
  - `main.rs`: a basic cli
- `cases/`: test cases and example code
  - `acc.zydeco`: accumulative test cases
- `tests/`: (WIP) test utils

## Running REPL

```bash
rlwrap cargo run -- repl
```

To conclude the program, type `@@@ <optional name><CR>`.

## Testing

An accumulative test file `compiler/cases/acc.zydeco` is kept for adding small, incremental while convenient test cases under heavy development. To add a test case,

1. Type a complete Zydeco term
2. Type `@@@` at the start of the line, followed by descriptions for the test

To run in terminal,

```bash
cargo run -- < compiler/cases/acc.zydeco
```
## Features

1. Environment-based Evaluator
2. Inductive and Co-inductive Types

## Roadmap

### Features

1. Arithmetics
2. String and IO
3. Modules
4. Continuation (w/ letcc?)
5. Polymorphism (System F)
6. Product and Sum

### Syntax

1. Formatter (for better debug experience)
2. Encode currying and multiple binding in AST
3. Automatic Insertion of `ret` & `Ret()` and `{}` & `Comp()`

## Pointers to the Literature

Call-by-push-value by Paul Blain Levy: https://dl.acm.org/doi/10.1145/3537668.3537670

## Related Work

- Fiddle: <https://github.com/maxsnew/modal-scheme>
- Riddle: <https://github.com/UMjoeypeng/riddle_compiler>
