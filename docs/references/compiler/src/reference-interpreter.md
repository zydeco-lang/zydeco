# C7. Linking and the Reference Interpreter

[Linking](../../../../lang/dynamics/src/link.rs) selects residual syntax,
erases types and witnesses, and constructs `DynamicsProgram`.
Builtin root linking materializes the host-provided packed value from its validated typed signature;
foreign declarations remain checked call plans until their runtime loader is used.
Named routes become structural tuple access, and aliases preserve a shared bindee.

[Runtime syntax](../../../../lang/dynamics/src/syntax.rs) separates dynamic terms from semantic values,
environments, suspended computations, and continuation state.
The [stepper](../../../../lang/dynamics/src/eval.rs) evaluates values, forces thunks,
consumes arguments and destructor observations, matches data, and resumes return continuations.
A thunk captures its lexical environment; `fix` supplies explicit computation recursion.
No backend-specific demand or frame optimization changes this reference evaluation path.

The interpreter uses Rust representations for values and host resources rather than the native tagged heap.
Its allocation behavior therefore does not predict native or Wasm costs.
The runtime borrows its input, output, error, and argument interfaces from the caller;
CLI runs use process streams, while tests and the REPL supply captured streams.
`ProgKont` distinguishes continuation outcomes, exit status, and runtime errors.
An executable failure is reported at this boundary rather than silently retried on another target.

The [dynamics guide](../../../../lang/dynamics/src/README.md) maps runtime modules.
[C14](builtins-and-foreign-calls.md) owns shared host roles and call plans;
[L6](../../language/src/computations-and-control.md) owns source dynamics.
Source regressions and runtime parity tests use the interpreter as one observable execution surface.
