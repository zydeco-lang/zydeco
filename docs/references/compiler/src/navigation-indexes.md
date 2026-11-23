# Navigation Indexes

| Source mechanism | Typed boundary | Execution route |
| --- | --- | --- |
| A named field | Resolved structural route and typed payload | Tuple access/patterns; names erase |
| An existential witness | Scoped abstract identity or manifest equation | Static evidence erases; payload remains |
| `val` or a view | `ValPi` and static application/pattern | C6 removes it before executable lowering |
| `@[typeof]` | Reused classifier identity | Operand has no runtime query node |
| A thunk | Checked computation capture | Interpreter closure or SPS closure conversion |
| `ret` and `do` | Computation and its consuming continuation | SPS return/binding, then target control representation |
| A literal pattern | Typed integer and opaque coverage observation | Equality branch during SPS lowering |
| A monadic block | Retained checked payload and translated algebra structure | Ordinary residualization and backend pipeline |
| A foreign thunk | Validated `ForeignSignature` | libffi or native returning-call bridge |

| Invariant | Establishing/checking boundary | Principal consumers |
| --- | --- | --- |
| Hygienic source and binder identities | C2–C4 | Checking and editor provenance |
| Closed inference and stable normalized types | C5 | C6, linking, and all lowering |
| Representable executable residual | C6 and executable selection | C7–C14 |
| Lexical single occurrence and branch joins | SPS and SPSLow validators, C8–C9 | Normalization, capture analysis, local unboxing |
| Local unboxing and bounded scalar plans | C10 analyses | Cranelift and WebAssembly emitters |
| Cranelift liveness, tail ABI, and units | C11 lowering and linking | Native `.o` emission, FFI adapters, collector |
| Tagged words, `ClifVmCtxHeader`, and shared ABI | C12 model and C13 Wasm | Host adapters, generated code, runtime |
| Complete mutable roots at safepoints | C11 `CaseLiveness` and C12 `CheneyHeap` | Native moving collector |
| Revision-correct semantic identity | C3 and C15 | Editor results and interactive actions |

The phase table in C1 is the entry-point index; chapter links lead to the owning modules and focused regressions.
