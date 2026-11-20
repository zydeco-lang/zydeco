# References

These references assume basic programming-languages background.
The language reference explains source rules and observable behavior; the compiler reference follows the representations
and algorithms implementing them.

- [Language reference](language/src/introduction.md): syntax, typing, evaluation, static composition,
  packages, host interfaces, and execution profiles.
- [Compiler implementation reference](compiler/src/introduction.md): phase contracts, pass composition,
  representations, checking, lowering, runtimes, tooling, and validation.

Both references are maintained as `mdBook` books under `docs/references/language/`
and `docs/references/compiler/`, published to [GitHub Pages](https://zydeco-lang.github.io/zydeco/)
by the [documentation workflow](../../.github/workflows/docs.yml),
and validated or built locally with `cargo xtask docs check`
and `cargo xtask docs build` (see [CONTRIBUTING](../../CONTRIBUTING.md#generate-documentation)).

The references own user-approved design that is fully implemented within its stated scope,
including rationale and explicit limits.
[Proposals](../proposals/README.md) contain concrete unfinished designs;
[ideas](../ideas/README.md) contain exploratory questions;
[evaluations](../evaluations/README.md) retain dated experimental reports and their evidence;
[todos](../todos/README.md) record observed discrepancies.
[CONTRIBUTING](../../CONTRIBUTING.md#maintain-documentation) describes how to review and consolidate material.

## Rule Ownership

Each chapter owns its semantic boundary.
The table links source rules to their implementation accounts; it does not assign a second home to the same rule.
Chapter contents provide the detailed topic indexes.

| Topic | Source contract | Implementation contract |
| --- | --- | --- |
| Bindings and scope | [L3](language/src/bindings-and-scope.md) | [C4: resolution](compiler/src/parsing-and-resolution.md#name-resolution) |
| Classification and inference | [L4](language/src/classification-and-inference.md) | [C5: checking and reuse](compiler/src/checking-and-inference.md#inference-and-reuse) |
| Values and products | [L5](language/src/values-products-and-data.md) | [C9: closure conversion](compiler/src/closure-conversion.md), [C10: local representation](compiler/src/zasm.md) |
| Computations and stack protocols | [L6](language/src/computations-and-control.md) | [C7: evaluator](compiler/src/reference-interpreter.md) |
| Patterns and coverage | [L7](language/src/patterns-and-coverage.md) | [C6: pattern checking](compiler/src/sps/operations-and-patterns.md#pattern-decisions-and-validation) |
| Value functions and views | [L8](language/src/value-functions-and-views.md) | [C6: static elaboration](compiler/src/elaboration-and-validation.md#static-elimination) |
| Packed values, witnesses, and fields | [L9](language/src/polymorphism-and-packed-values.md) | [C5: witness evidence](compiler/src/checking-and-inference.md#witness-evidence-and-field-lookup) |
| Required static elimination | [L10](language/src/static-elimination.md) | [C6: elaboration](compiler/src/elaboration-and-validation.md#static-elimination) |
| Relative monads | [L11](language/src/translations/relative-monads.md#relative-monads) | [C6: translation](compiler/src/elaboration-and-validation.md#monadic-and-copattern-elaboration) |
| Core prelude | [L12: default vocabulary and exceptions](language/src/sources/core-prelude.md) | [C3: source prefix](compiler/src/sessions-and-queries.md#source-loading-and-package-selection), [C4: lookup](compiler/src/parsing-and-resolution.md#name-resolution) |
| Sources and source packages | [L12: abstraction levels](language/src/sources.md#abstraction-levels) and [source rules](language/src/sources.md) | [C3: implementation representations](compiler/src/sessions-and-queries.md#implementation-representations), [loading and sessions](compiler/src/sessions-and-queries.md), [C4: assembly](compiler/src/parsing-and-resolution.md) |
| Memory layouts and views | [L13: memory](language/src/primitives-and-capabilities/manual-memory.md#manual-memory) | [C6: static elimination](compiler/src/elaboration-and-validation.md#static-elimination), [C14: host contracts](compiler/src/builtins-and-foreign-calls.md) |
| Primitive values and capabilities | [L13](language/src/primitives-and-capabilities.md) | [C14: host contracts](compiler/src/builtins-and-foreign-calls.md) |
| Foreign interfaces and compiled libraries | [L14](language/src/foreign-interfaces.md) | [C14: C calls](compiler/src/builtins-and-foreign-calls.md#foreign-calls), [C11: artifacts](compiler/src/native-emission.md#compilation-unit-preparation-and-artifacts) |
| Execution profiles and costs | [L15](language/src/execution-profiles.md) | [C11: activation lifetime](compiler/src/native-emission.md#activation-lifetime), [C12: native runtime](compiler/src/native-runtime.md), [C13: Wasm](compiler/src/webassembly.md) |
| Identity and provenance | Construct-specific scope rules above | [C2: compiler identities](compiler/src/compiler-data.md), [C3: retained facts](compiler/src/sessions-and-queries.md#analysis-facts-and-materialization) |
| Optional residual optimization | [L10: phase boundary](language/src/static-elimination.md) | [C8: normalization and demand](compiler/src/sps.md) |
| Formatting and documentation | [Meta annotation syntax](language/src/meta-annotations.md), [source documentation](language/src/meta-annotations.md#source-documentation) | [C4: source analysis](compiler/src/sessions-and-queries.md#shared-source-analysis), [C15: tooling](compiler/src/tooling.md) |

Package paths, source instantiation, import identity,
and shared semantic selection follow the [package namespace rules](language/src/sources/packages.md#package-hierarchy).

The [formal calculus](../../lang/statics/type-system.typ) is a mathematical companion;
its [remaining disagreements](../todos/reference-drift.md#formal-calculus) are tracked explicitly.
Use the [tutorial](../tutorial/zydeco-guide.md) for an introduction and the [library guide](../../lib/std/README.md)
for API inventories and composition recipes.
