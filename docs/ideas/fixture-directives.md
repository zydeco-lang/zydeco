# Source fixture directive extensions

Add program arguments, expected exit codes, or stable desugaring-rejection spellings
to source fixtures only when a concrete case needs those contracts.
An inapplicable expectation must continue to fail the trial.
The [fixture guide](../../lang/tests/cases/README.md) owns current directives;
[C16](../references/compiler.md#source-fixtures-and-runtime-oracles) owns discovery and runtime oracles.
