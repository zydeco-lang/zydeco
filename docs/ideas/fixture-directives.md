# Source fixture directive extensions

The directive parser accepts `backends`, `input`, `output`, `args`, and `setup`
for the [standard-library directive runner](../../lang/tests/tests/std_directives.rs).
Those directives require `stage: run`; the inline case runner rejects them.
An inapplicable expectation must continue to fail the trial.

Expected exit codes and stable desugaring-rejection spellings remain deferred;
add them only when a concrete case needs those contracts.
The [fixture guide](../../lang/tests/cases/README.md) owns current directives;
[C16](../references/compiler.md#source-fixtures-and-runtime-oracles) owns discovery and runtime oracles.
