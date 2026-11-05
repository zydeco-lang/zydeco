# Tooling boundary discrepancies

These bounded CLI and session/tooling issues were retained from the retired 2026-10-21 cleanup audit.
The code paths were inspected at `e2dd8979` on 2026-11-05 and are unchanged at `c6c90588`.
Each entry names its governing reference, the observed code path, and a bounded repair;
the [reference ownership map](../references/README.md#rule-ownership) locates the owning canonical account.

## Duplicate runtime digest and platform computation

[C11 compilation-unit preparation and artifacts](../references/compiler.md#compilation-unit-preparation-and-artifacts)
and the [CLI native executable workflow](../../cli/README.md#native-executables) govern the native build boundary.
On every `build -t exe`, the CLI build path
at [cli/src/main.rs:499-514](../../cli/src/main.rs) computes `LibraryPlatform::for_target`
and `LibraryDigest::runtime` twice: once as arguments to `validate_imports` and once as arguments to `validate_units`.
Both calls describe the same target and the same runtime directory, so each artifact resolves the platform
and rehashes the runtime source directory a second time.

Repair: bind the platform and digest once before the two validations, pass the same values to both,
and keep the current imports-then-units ordering.
Both constructors are fallible ([library.rs:56](../../cli/src/library.rs)
for the platform and [library.rs:226](../../cli/src/library.rs) for the digest),
so hoisting preserves their errors before either validation runs.
The change has no other behavior effect.

## Installed binaries and the build checkout

[C3 source loading](../references/compiler.md#source-loading-and-package-selection) owns the default prelude loader, and
[C15 interactive tooling](../references/compiler.md#c15-diagnostics-formatting-documentation-and-interactive-tooling)
owns the REPL surfaces.
The [prelude loader](../../lang/session/src/source/prelude.rs) computes the standard-library root
from `env!("CARGO_MANIFEST_DIR")` at `prelude.rs:13`, so a CLI installed
with `cargo install --path cli` resolves `lib/std` against the checkout that compiled it.
The [TUI engine](../../tui/src/engine.rs) pins its provider builtin the same way at `engine.rs:44`;
its test fixtures repeat the pattern at `engine.rs:410` and `engine.rs:583`.
The [bundled fallback](../../lang/session/src/source/prelude.rs)
at `prelude.rs:58-70` covers only the opted-out closed bootstrap files and deliberately excludes `builtin.zy`,
as the assertion at `prelude.rs:84` records, so a missing checkout leaves provider packages unresolved.

Repair options: resolve the standard library through an installed data directory or configuration,
or embed the complete standard library, including provider packages, in the binary.
Keep checkout-relative resolution working for development.

- [ ] Choose the installed-layout source: a data directory or configuration, or a complete embedded standard library.
- [ ] Route the prelude root, the TUI builtin path, and the test fixtures through the chosen resolution,
  retaining checkout behavior.
