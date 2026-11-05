# Reference drift

These outstanding documentation repairs follow the 2026-09-08 and 2026-09-10 audits,
with a source review against `3b4dd665` on 2026-09-14.
The backend, native-boundary, and component-guide entries below were rechecked
at `e2dd8979` on 2026-11-05 and are unchanged at `c6c90588`.
Completed repairs and migration histories have been removed from this work list.
The [reference ownership map](../references/README.md#rule-ownership) locates each canonical account;
[CONTRIBUTING](../../CONTRIBUTING.md#maintain-documentation) defines the consolidation workflow.

## Formal calculus

- [ ] **Recursive field search.** The [calculus](../../lang/statics/type-system.typ) still gives
  `field` an immediate-component search and explicitly excludes deeper traversal.
  [L9](../references/language.md#9-polymorphism-and-packed-values)
  and [field lookup](../../lang/statics/src/check/projection/field.rs) recursively search named wrappers,
  products, and witness telescopes.
  Reconcile the mathematical rule with those supported routes and ambiguity checks.
- [ ] **Computation witnesses.** Compare the calculus's witness-dependent computation rules
  with the leading-prefix boundary in [L9](../references/language.md#9-polymorphism-and-packed-values)
  and [the current application checker](../../lang/statics/src/check/functions/application.rs).
  The [nested-witness probe](compiler-boundaries.md#nested-witness-diagnostic) reaches an application failure.
  Keep value-function witness routes separate;
  their implemented structural routes do not extend computation application.
- [ ] **Value matches and integer operations.** Add the value-producing match
  and integer value forms supported by [L8](../references/language.md#8-value-functions-and-views)
  and [static elimination](../references/compiler.md#static-elimination).
  The current calculus's value grammar and rules omit them.
  The earlier n-ary product repair does not establish conformance of these other judgments.

Validate revised rules against accepted and rejected source examples, then build the mathematical companion:

```sh
typst compile --root . lang/statics/type-system.typ /tmp/zydeco-type-system.pdf
```

Document construction alone does not establish conformance to the compiler.
Stronger recursive admissibility is an [unimplemented design question](../ideas/README.md#recursive-admissibility),
not a missing positivity guarantee to add to the reference.

## Duplicate accounts and scratch records

- [ ] **DESIGN and component overlap.** [DESIGN](../../DESIGN.md) still repeats detailed language,
  runtime, and tooling accounts alongside the references.
  Audit each section against its [reference owner](../references/README.md#rule-ownership),
  preserve implemented rationale there, and shorten the secondary account to orientation and links.
  Keep library recipes and component module maps in their local guides.

## Execution backend and representation scope

- [ ] **Stale four-backend sentences.** Two reference sentences
  still say the corresponding tests run "on all four backends": the
  [C14 argument-lookup account](../references/compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls)
  at `compiler.md:3076` and [L14 foreign decoding](../references/language.md#foreign-decoding) at `language.md:2407`.
  Commit `5e4f4e46` removed the abstract-machine Wasm backend,
  and [L15 backend selection](../references/language.md#selecting-an-execution-backend) now names `interpreter`,
  `exe` (AMD64), and `wasm-sps`.
  The tests register exactly those three ([builtin.rs:82-103](../../lang/tests/tests/builtin.rs)
  and [ffi_examples.rs:165](../../lang/tests/tests/ffi_examples.rs)).
  Repair both sentences to say three backends and keep the existing test and specimen lists.
- [ ] **`--representation` scope.** The [C10 policy selection](../references/compiler.md#policy-selection) account says
  at `compiler.md:2493` that the CLI's `build --representation` applies
  to "ZASM, AMD64 assembly/executables, and the Wasm backend".
  The [CLI compiler](../../cli/src/compile.rs) also applies the selected policy to library and unit lowering:
  `lower_export` at `compile.rs:142` and `lower_unit` at `compile.rs:163` call `with_representation`,
  and the CLI's own rejection message includes object, staticlib, and sharedlib at `main.rs:583`.
  Decide whether the reference widens its scope sentence or the implementation narrows its selection,
  then add an end-to-end C-library or native-unit test that observes the selected policy in the emitted artifact.

## Native and host boundary accounts

- [ ] **Bundled numeric decimal spelling.** [wasm-numeric.mjs:1](../../cli/wasm/wasm-numeric.mjs) calls
  the decimal spelling "shared by the interpreter and native numeric builtins".
  The file is a JavaScript re-implementation
  of the [L13 Rust Display spelling](../references/language.md#13-primitive-values-and-capabilities)
  (`language.md:1824`), imported by the [Node host](../../cli/wasm/wasm-host.mjs) at `wasm-host.mjs:3`
  and checked against Rust by the [Wasm float-text parity test](../../lang/tests/tests/wasm_float_text.rs)
  at `wasm_float_text.rs:62`; the [execution runner](../../cli/src/execution.rs) bundles it
  for Wasm execution at `execution.rs:162`.
  [C13 module and host ABI](../references/compiler.md#module-and-host-abi) already treats the Node host
  as a separate implementation.
  Repair the comment to describe a cross-language copy checked by the parity test,
  or share one table across the implementations; do not imply shared code.
- [ ] **Exported host-symbol prefix.**
  [C11 native preparation](../references/compiler.md#c11-native-preparation-activation-frames-and-amd64-emission)
  and [C12 native runtime](../references/compiler.md#c12-shared-native-model-allocation-and-collection) own the
  native model, but neither explains the leading `\x01` in exported host symbol names.
  [runtime/stub.rs:548](../../runtime/stub.rs) exports `\x01zydeco_frame_step`,
  and [lang/machine/build.rs:51-65](../../lang/machine/build.rs) generates the `\x01`-prefixed link and export names.
  Mach-O normally underscores C symbols;
  the prefix yields the bare names the [AMD64 emitter](../../lang/amd64/src/emit.rs) declares for host externs
  at `emit.rs:648` and `emit.rs:677`, and `nm` on a built runtime archive reports bare `zydeco_frame_step`
  and `zydeco_entry_frames_<fingerprint>`.
  Document the convention where C11 or C12 owns the native model, or remove the need for the prefix.

## Component and client accounts

- [ ] **Dynamics module map.**
  [C7 linking and the reference interpreter](../references/compiler.md#c7-linking-and-the-reference-interpreter)
  makes the [dynamics guide](../../lang/dynamics/src/README.md) the runtime module map at `compiler.md:1468`,
  but the guide's key-components list omits `host` and `foreign` and the `memory`
  and `representation` support modules declared in [lib.rs:8-28](../../lang/dynamics/src/lib.rs).
  Add the omitted modules to the guide and mark which are public; no behavior changes.
- [ ] **Cajun package relationships.** The retired 2026-10-21 cleanup audit claimed
  that Cajun completions surface package relationships.
  In this checkout only document links consume the resolved `relations`:
  [document_links.rs:76-114](../../editor/cajun/src/document_links.rs) builds relationship links,
  while [completion.rs:153-180](../../editor/cajun/src/completion.rs) reads roles
  and package options from the metadata catalog.
  [C15 completion](../references/compiler.md#completion) owns candidate families
  but does not assign relationship targets to a surface.
  Confirm the intended client, then implement relationship completion or document
  that document links own relationship presentation.
