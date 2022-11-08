# Zydeco

A proof-of-concept language adopting call-by-push-value as its evaluation order.

## Repository Structure

```plain
.
├── Cargo.lock
├── Cargo.toml
├── zydeco
├── zydeco-lang
│  ├── src
│  └── tests
└── README.md
```

`/zydeco/`: interesting test cases and standard libraries in zydeco.

`/zydeco-lang/`: an implementation for Zydeco language; uses lalrpop as its parser generator.

- `src/`:
  - `lib.rs`: the top-level module for all compiler utilities
  - `main.rs`: a basic cli
- `tests/`: test cases and example code

## Running Zydeco

```bash
cargo run -- run zydeco/interpreter.zydeco
```

This runs an "interpreter" of zydeco, written in zydeco.

## Running REPL

```bash
rlwrap cargo run -- repl
```

To conclude the program, type `@@@ <optional name><CR>`.

## Testing

An accumulative test file `zydeco-lang/tests/basics.zydeco` is kept for adding small, incremental while convenient test cases under heavy development. To add a test case,

1. Type a complete Zydeco term
2. Type `@@@` at the start of the line, followed by descriptions for the test

To run in terminal,

```bash
cargo run -- test < zydeco-lang/tests/basics.zydeco
```

## Features

1. Environment-based Evaluator
2. Inductive and Co-inductive Types
3. String and IO

## Roadmap

### Features

1. Arithmetics
2. Modules
3. Continuation (w/ letcc?)
4. Polymorphism (System F)
5. Product and Sum (as builtin)

### Syntax

1. Formatter (for better debug experience)
2. Encode currying and multiple binding in AST
3. Automatic Insertion of `ret` & `Ret()`

## Pointers to the Literature

Call-by-push-value by Paul Blain Levy: https://dl.acm.org/doi/10.1145/3537668.3537670

## Related Work

- Fiddle: <https://github.com/maxsnew/modal-scheme>
- Riddle: <https://github.com/UMjoeypeng/riddle_compiler>
