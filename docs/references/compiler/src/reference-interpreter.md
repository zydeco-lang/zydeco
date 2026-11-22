# C7. Linking and the Reference Interpreter

Static elaboration in `zydeco-statics` erases types and witnesses, resolves structural projections
and tags, and produces a self-contained [`CoreProgram`](../../../../lang/ir/src/syntax.rs)
alongside any validated `BuiltinPackagePlan`.
Builtins materialize lazily from each `BuiltinPackagePlan` supplied via `Runtime::with_packages`,
while foreign declarations remain checked call plans until their runtime loader is used.
The interpreter depends directly on `zydeco-ir` and `zydeco-syntax` rather than surface or statics arenas.

[Runtime syntax](../../../../lang/dynamics/src/syntax.rs) defines semantic values (`SemValue`),
persistent lexical environments (`env::EvalEnv` keyed by `zydeco_ir::VarId`), suspended thunks, and continuation state.
The [stepper](../../../../lang/dynamics/src/eval.rs) (`Runtime`) borrows `&CoreProgram` directly to evaluate values,
force thunks, consume arguments and destructor observations, match data, and resume return continuations.
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
