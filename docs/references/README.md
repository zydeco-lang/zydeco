# References

These references assume basic programming-languages background.
The language reference specifies source rules and observable behavior;
the standard library reference specifies canonical library contracts for effects and manual memory;
the compiler reference follows the representations and algorithms implementing them.

- [Language reference](language/src/introduction.md): syntax, typing, evaluation, static composition,
  surface translations, packages, host interfaces, and execution profiles.
- [Standard library reference](library/src/introduction.md): library organization, relative monads,
  effect transformers, nondeterminism, manual memory, layouts, views, and byte buffers.
- [Compiler implementation reference](compiler/src/introduction.md): phase contracts, pass composition,
  representations, checking, lowering, runtimes, tooling, and validation.

All three references are maintained as `mdBook` books under `docs/references/language/`, `docs/references/library/`,
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

Each chapter owns its semantic boundary. The table links source and library contracts
to their implementation accounts; it does not assign a second home to the same rule.
Chapter contents provide the detailed topic indexes.

| Topic | Source or library contract | Implementation contract |
| --- | --- | --- |
| Bindings and scope | [Bindings and Scope](language/src/bindings-and-scope.md) | [C4: resolution](compiler/src/parsing-and-resolution.md#name-resolution) |
| Classification and inference | [Classification and Inference](language/src/classification-and-inference.md) | [C5: checking and reuse](compiler/src/checking-and-inference.md#inference-and-reuse) |
| Values and products | [Values, Products, and Data](language/src/values-products-and-data.md) | [C9: closure conversion](compiler/src/closure-conversion.md), [C10: local representation](compiler/src/representation-and-scalar-regions.md) |
| Computations and stack protocols | [Computations and Control](language/src/computations-and-control.md) | [C7: evaluator](compiler/src/reference-interpreter.md) |
| Patterns and coverage | [Patterns and Coverage](language/src/patterns-and-coverage.md) | [C6: pattern checking](compiler/src/sps/operations-and-patterns.md#pattern-decisions-and-validation) |
| Value functions and views | [Value Functions and Views](language/src/value-functions-and-views.md) | [C6: static elaboration](compiler/src/elaboration-and-validation.md#static-elimination) |
| Packed values, witnesses, and fields | [Polymorphism and Packed Values](language/src/polymorphism-and-packed-values.md) | [C5: witness evidence](compiler/src/checking-and-inference.md#witness-evidence-and-field-lookup) |
| Required static elimination | [Static Elimination](language/src/static-elimination.md) | [C6: elaboration](compiler/src/elaboration-and-validation.md#static-elimination) |
| Monadic translation and relative monads | [Monadic Blocks](language/src/translations/monadic-blocks.md), [Relative Monads](library/src/effects/relative-monads.md) | [C6: translation](compiler/src/elaboration-and-validation.md#monadic-and-copattern-elaboration) |
| Call-by-value and call-by-name blocks | [Call-by-Value and Call-by-Name Blocks](language/src/translations/call-by-value-and-call-by-name.md) | [C5: checking](compiler/src/checking-and-inference.md) |
| Core prelude | [Core Prelude](language/src/sources/core-prelude.md) | [C3: source prefix](compiler/src/sessions-and-queries.md#source-loading-and-package-selection), [C4: lookup](compiler/src/parsing-and-resolution.md#name-resolution) |
| Sources and source packages | [Sources, Imports, and Entry](language/src/sources.md), [Source Packages](language/src/sources/packages.md) | [C3: implementation representations](compiler/src/sessions-and-queries.md#implementation-representations), [loading and sessions](compiler/src/sessions-and-queries.md), [C4: assembly](compiler/src/parsing-and-resolution.md) |
| Standard library contracts | [Standard Library Organization](library/src/organization.md), [Effects](library/src/effects/transformers-and-instances.md) | [Library guide](../../lib/std/README.md) |
| Manual memory, layouts, and views | [Manual Memory](library/src/memory/manual-memory.md), [Layouts](library/src/memory/layouts-and-aggregates.md), [Views and Buffers](library/src/memory/views-and-buffers.md) | [C6: static elimination](compiler/src/elaboration-and-validation.md#static-elimination), [C14: host contracts](compiler/src/builtins-and-foreign-calls.md) |
| Primitive values and capabilities | [Primitive Values and Capabilities](language/src/primitives-and-capabilities.md) | [C14: host contracts](compiler/src/builtins-and-foreign-calls.md) |
| Foreign interfaces and compiled libraries | [Foreign Interfaces](language/src/foreign-interfaces.md) | [C14: C calls](compiler/src/builtins-and-foreign-calls.md#foreign-calls), [C11: artifacts](compiler/src/native-emission.md#compilation-unit-preparation-and-artifacts) |
| Execution profiles and costs | [Execution Profiles](language/src/execution-profiles.md) | [C11: activation lifetime](compiler/src/native-emission.md#activation-lifetime), [C12: native runtime](compiler/src/native-runtime.md), [C13: Wasm](compiler/src/webassembly.md) |
| Identity and provenance | Construct-specific scope rules above | [C2: compiler identities](compiler/src/compiler-data.md), [C3: retained facts](compiler/src/sessions-and-queries.md#analysis-facts-and-materialization) |
| Optional residual optimization | [Static Elimination](language/src/static-elimination.md) | [C8: normalization and demand](compiler/src/sps.md) |
| Formatting and documentation | [Meta Annotations](language/src/meta-annotations.md), [Source Documentation](language/src/meta-annotations.md#source-documentation) | [C4: source analysis](compiler/src/sessions-and-queries.md#shared-source-analysis), [C15: tooling](compiler/src/tooling.md) |

Package paths, source instantiation, import identity,
and shared semantic selection follow the [package namespace rules](language/src/sources/packages.md#package-hierarchy).

The [formal calculus](../../lang/statics/type-system.typ) is a mathematical companion;
its [remaining disagreements](../todos/reference-drift.md#formal-calculus) are tracked explicitly.
Use the [tutorial](../tutorial/zydeco-guide.md) for an introduction and the [library guide](../../lib/std/README.md)
for API inventories and composition recipes.
