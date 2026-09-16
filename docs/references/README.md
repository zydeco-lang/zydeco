# References

These references assume basic programming-languages background.
The language reference explains source rules and observable behavior; the compiler reference follows the representations
and algorithms implementing them.

- [Language reference](language.md): syntax, typing, evaluation, static composition, packages, host interfaces,
  and execution profiles.
- [Compiler implementation reference](compiler.md): phase contracts, pass composition, representations,
  checking, lowering, runtimes, tooling, and validation.

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
| Bindings and scope | [L3](language.md#3-bindings-and-scope) | [C4: resolution](compiler.md#name-resolution) |
| Classification and inference | [L4](language.md#4-classification-and-inference) | [C5: checking and reuse](compiler.md#inference-and-reuse) |
| Values and products | [L5](language.md#5-values-products-and-data) | [C9: closure conversion](compiler.md#c9-closure-conversion-and-first-order-spslow), [C10: local representation](compiler.md#c10-zasm-stack-analysis-and-local-representation-choices) |
| Computations and stack protocols | [L6](language.md#6-computations-and-control) | [C7: evaluator](compiler.md#c7-linking-and-the-reference-interpreter) |
| Patterns and coverage | [L7](language.md#7-patterns-and-coverage) | [C6: pattern checking](compiler.md#pattern-decisions-and-validation) |
| Value functions and views | [L8](language.md#8-value-functions-and-views) | [C6: static elaboration](compiler.md#static-elimination) |
| Packed values, witnesses, and fields | [L9](language.md#9-polymorphism-and-packed-values) | [C5: witness evidence](compiler.md#witness-evidence-and-field-lookup) |
| Required static elimination | [L10](language.md#10-static-elimination) | [C6: elaboration](compiler.md#static-elimination) |
| Relative monads | [L11](language.md#11-relative-monads) | [C6: translation](compiler.md#monadic-and-copattern-elaboration) |
| Sources and source packages | [L12: abstraction levels](language.md#abstraction-levels) and [source rules](language.md#12-sources-imports-and-entry) | [C3: implementation representations](compiler.md#implementation-representations), [loading and sessions](compiler.md#c3-source-loading-sessions-queries-and-memory-retention), [C4: assembly](compiler.md#c4-parsing-desugaring-and-name-resolution) |
| Memory layouts and views | [L13: memory](language.md#manual-memory) | [C6: static elimination](compiler.md#static-elimination), [C14: host contracts](compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls) |
| Primitive values and capabilities | [L13](language.md#13-primitive-values-and-capabilities) | [C14: host contracts](compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls) |
| Foreign interfaces and compiled libraries | [L14](language.md#14-foreign-interfaces) | [C14: C calls](compiler.md#foreign-calls), [C11: artifacts](compiler.md#compilation-unit-preparation-and-artifacts) |
| Execution profiles and costs | [L15](language.md#15-execution-profiles) | [C11: activation lifetime](compiler.md#activation-lifetime), [C12: native runtime](compiler.md#c12-shared-native-model-allocation-and-collection), [C13: Wasm](compiler.md#c13-webassembly-backends-and-embedding) |
| Identity and provenance | Construct-specific scope rules above | [C2: compiler identities](compiler.md#c2-compiler-data-identities-arenas-and-source-provenance), [C3: retained facts](compiler.md#analysis-facts-and-materialization) |
| Optional residual optimization | [L10: phase boundary](language.md#10-static-elimination) | [C8: normalization and demand](compiler.md#c8-high-sps-lowering-normalization-and-demand) |
| Formatting and documentation | [Meta annotation syntax](language.md#meta-annotations-compile-time-metadata), [source documentation](language.md#source-documentation) | [C4: source analysis](compiler.md#shared-source-analysis), [C15: tooling](compiler.md#c15-diagnostics-formatting-documentation-and-interactive-tooling) |

Package paths, source instantiation, import identity,
and shared semantic selection follow the [namespace and resolution proposal](../proposals/package-resolution.md).

The [formal calculus](../../lang/statics/type-system.typ) is a mathematical companion;
its [remaining disagreements](../todos/reference-drift.md#formal-calculus) are tracked explicitly.
Use the [tutorial](../tutorial/zydeco-guide.md) for an introduction and the [library guide](../../lib/std/README.md)
for API inventories and composition recipes.
