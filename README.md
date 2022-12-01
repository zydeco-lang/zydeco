# Zydeco 🪗

Zydeco is a proof-of-concept programming language based on Call-by-push-value.

## Running Zydeco

Create a file `hello-world.zydeco`:
```plain
! write_line "hello, world!" { ! exit 0 }
```

Then run
```bash
$ cargo run -- run hello-world.zydeco
hello, world!
```

Alternatively, run
```bash
cargo build --release
```
To build the executable which will be stored at `target/release/zydeco`.
Then run 
```bash
$ ./target/release/zydeco run hello-world.zydeco
hello, world!
```

Run `zydeco --help` for further usage information.

## Intro to Zydeco

We will develop more introductory material on zydeco when we have
implemented more features. For now, browse `zydeco-lang/tests/` for
some example programs.

## Repository Structure

```plain
.
├── Cargo.toml
├── zydeco-lang
│  ├── src
│  └── tests
├── cli
├── web
└── ...
```

- `zydeco-lang/`: the library implementing the parser, typechecker and
interpreter for the Zydeco language.
- `zydeco-lang/tests/`: test cases and example code
- `cli/` Command-line interface
- `web/` Web interface

## Related Literature

Zydeco is based on the Call-by-push-value calculus introduced by Paul
Blain Levy: https://dl.acm.org/doi/10.1145/3537668.3537670

## Related Language Implementations

- Fiddle : <https://github.com/maxsnew/modal-scheme>
- Riddle: <https://github.com/UMjoeypeng/riddle_compiler>
