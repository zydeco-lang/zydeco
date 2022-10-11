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
  - `acc.vit`: accumulative test cases
- `tests/`: (WIP) test utils

## Running REPL

```bash
rlwrap cargo run -- repl
```

## Testing

An accumulative test file `compiler/cases/acc.vit` is kept for adding small, incremental while convenient test cases under heavy development. To add a test case,

1. Type a complete Zydeco term
2. Type `@@@` at the start of the line, followed by descriptions to the test
3. (TBD) Type `w/...` where `...` is a sequence of single letters representing phases to run

To run in terminal,

```bash
cargo run -- < compiler/cases/acc.vit
```

## Worklist

### Feature List

1. Continuation (w/ letcc?)
2. Inductive and Co-inductive Types
3. Polymorphism (System F)
4. Product and Sum
5. Arithmetics

### Syntax

1. Formatter (for better debug experience)
2. Encode currying and multiple binding in AST
3. Automatic Insertion of `ret` & `Ret()` and `{}` & `Comp()`

## Related Work

- Fiddle: <https://github.com/maxsnew/modal-scheme>
- Riddle: <https://github.com/UMjoeypeng/riddle_compiler>
