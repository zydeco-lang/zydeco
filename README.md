# Zydeco 🪗

Zydeco is a proof-of-concept programming language based on Call-by-push-value.

## Running Zydeco

Create a file `hello-world.zydeco`:
```plain
main
  do~ ! write_line "hello, world!";
  ! exit 0
end
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
to build the executable which will be stored at `target/release/zydeco`.

Then run
```bash
$ ./target/release/zydeco run hello-world.zydeco
hello, world!
```

Run `zydeco --help` for further usage information.

## Intro to Zydeco

We now have a toy "literate zydeco" written in zydeco! Try it out by running
```bash
cd docs/spell && ./build.sh
```
and the product will show up right in the folder - which is also a series of guide to programming in zydeco.

A legacy version of the tutorial lies [here](docs/tutorial/intro_to_zydeco.md). With system-F_ω, type parameter and type alias shipped in we'll update the tutorial shortly.

We will develop more introductory material on zydeco when we have
implemented more features. For now, you can also choose to browse `zydeco-lang/tests/` for
some example programs.

To run all tests
```bash
cargo test --all
```

## Repository Structure

```plain
.
├── Cargo.toml
├── lang
│  ├── derive
│  ├── driver
│  ├── dynamics
│  ├── lib
│  ├── src
│  ├── statics
│  ├── surface
│  ├── syntax
│  ├── tests
│  └── utils
├── cli
├── web
└── ...
```

- `lang/`: the library implementing the parser, type checker and
interpreter for the Zydeco language.
- `lang/tests/`: test cases and example code
- `cli/` Command-line interface
- `web/` Web interface

## Related Literature

Zydeco is based on the Call-by-push-value calculus introduced by Paul
Blain Levy: https://dl.acm.org/doi/10.1145/3537668.3537670

## Related Language Implementations

- Fiddle : <https://github.com/maxsnew/modal-scheme>
- Riddle: <https://github.com/UMjoeypeng/riddle_compiler>
