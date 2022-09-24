# Vituloid

A proof-of-concept language adopting call-by-push-value as its evaluation order.

## Repository Structure

```
.
├── Cargo.lock
├── Cargo.toml
├── compiler
│  ├── build.rs
│  ├── Cargo.toml
│  └── src
│     └── ...
└── README.md
```

`/compiler`: compiler for Vituloid; uses lalrpop as its parser generator.
- `lib.rs`: the top-level module for all compiler utilities
- `main.rs`: a basic cli

## Related Work

- Fiddle: https://github.com/maxsnew/modal-scheme
- Riddle: https://github.com/UMjoeypeng/riddle_compiler
