# Source fixture directive extensions

The directive parser accepts `backends`, `input`, `output`, `args`,
and `setup` for the file-based standard-library runner.
Those directives require `stage: run`, and the inline case runner rejects them until that runner lands.
An inapplicable expectation must continue to fail the trial.

Expected exit codes and stable desugaring-rejection spellings remain deferred;
add them only when a concrete case needs those contracts.
The [fixture guide](../../lang/tests/cases/README.md) owns current directives;
[C16](../references/compiler.md#source-fixtures-and-runtime-oracles) owns discovery and runtime oracles.
