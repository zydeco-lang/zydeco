# 03 — Glossary

> Scratch study note for re-onboarding. Definitions here are working paraphrases for navigation;
> the linked reference section is authoritative in every case.

## Core Language

| Term | Working definition | Owner |
| --- | --- | --- |
| CBPV | Call-by-push-value: a calculus that separates values from computations instead of choosing call-by-value or call-by-name globally. | [L1](../../references/language/src/conventions.md) |
| Value | A piece of data or suspended code; evaluating it does not run effects. | [L1](../../references/language/src/conventions.md) |
| Computation | A term that performs control transfers: forcing, returning, invoking an operation. | [L6](../../references/language/src/computations-and-control.md) |
| Classifier | `VType` (classifies value types) or `CType` (classifies computation types); "does this term have kind or type" depends on it. | [L4](../../references/language/src/classification-and-inference.md) |
| `Thk B` / thunk | Value type of a suspended computation with protocol `B`; introduced by `{ M }` and consumed by force. | [L6](../../references/language/src/computations-and-control.md) |
| Force (`!`) | Run a thunk. `! m` is a computation; forcing the same thunk twice runs it twice. | [L6](../../references/language/src/computations-and-control.md) |
| `Ret A` | Computation protocol that returns an `A` to the installed continuation. Not a fixed frame. | [L6](../../references/language/src/computations-and-control.md) |
| `ret` / `do` | `ret v` returns; `do p <- m; n` runs `m : Ret A`, binds `v`, continues with `n`. | [L6](../../references/language/src/computations-and-control.md) |
| Arrow (`A -> B`) | Computation protocol that accepts an `A` before continuing at `B`; `fn p => M` introduces it. | [L6](../../references/language/src/computations-and-control.md) |
| Copattern | A binding header describes how its name is eliminated at use sites, not what it constructs. Example: `def ! id (x : Int) : Ret Int = ret x`. | [L8](../../references/language/src/value-functions-and-views.md) |
| Data / codata | Positive types constructed by constructors versus negative types observed by destructors/patterns. | [L5](../../references/language/src/values-products-and-data.md) |
| Value function | A total function over values, usable at type level; subject to static elimination. | [L8](../../references/language/src/value-functions-and-views.md) |
| View (`f ~> p`) | Pattern-like elimination of a value function; a rare surface form and a cleanup candidate. | [L8](../../references/language/src/value-functions-and-views.md) |
| Static elimination | Elaboration that runs type-level computation and erases static forms from the residual program. | [L10](../../references/language/src/static-elimination.md) |
| Packed value / witness | Existential introduction: a value hiding a type with evidence; opened by a pattern. | [L9](../../references/language/src/polymorphism-and-packed-values.md) |
| Package / source package | A named group of source definitions, discovered from `package.zy`/`workspace.zy`; distinct from packed values. | [L12](../../references/language/src/sources/packages.md#source-packages) |
| Core prelude | Default kind and type names injected into every source; provider choice stays explicit. | [L12](../../references/language/src/sources/core-prelude.md) |
| Relative monad | A monad-like structure whose values and computations may have different carriers; models stack-manipulating computation. | [L11](../../references/language/src/translations/relative-monads.md#relative-monads) |
| Monadic block | `@[monadic]` annotation that reinterprets CBPV code under a relative monad, including transformer derivation. | [L11](../../references/language/src/translations/relative-monads.md#monadic-blocks) |
| Effect module / capability dictionary | Library pattern: an effect interface plus a dictionary (record of operations) passed explicitly. | [tutorial §§9–10](../../tutorial/zydeco-guide.md#9-effect-modules) |
| Meta annotation | Compile-time metadata written `@[name(args)]`; runtime metadata is different (carried with a runtime value). | [Meta annotations](../../references/language/src/meta-annotations.md) |
| Execution profile | Selected backend and host contract for running a program: interpreter, native exe, Wasm. | [L15](../../references/language/src/execution-profiles.md) |

## Compiler Pipeline

| Term | Working definition | Owner |
| --- | --- | --- |
| Product | A phase's published, typed program representation (`ScopedProgram`, `CheckedProgram`, `FragmentProgram`, `SpsLowProgram`, ...). Products are the compiler's seams. | [C1](../../references/compiler/src/architecture.md) |
| Textual / bitter / scoped | The three front-end representations: tokens-and-layout syntax, desugared syntax, and resolved scoped syntax. | [C4](../../references/compiler/src/parsing-and-resolution.md) |
| Static / checked syntax | The typed arena produced by checking (`lang/statics/src/{syntax,arena}.rs`). | [C5](../../references/compiler/src/checking-and-inference.md) |
| Residual | The computation root left after static elimination; the interpreter and SPS lowering both start here. | [C6](../../references/compiler/src/elaboration-and-validation.md) |
| SPS | Stack-passing style: the consumer stack becomes an explicit, second-class value; each program consumes one ambient stack exactly once. High SPS is `FragmentProgram`. | [C8](../../references/compiler/src/sps.md) |
| SPS scheme | Where lowering names the current stack: `br` at branch joins (default), `dup` copies it per arm, `full` names every pushed frame. Exposed as `--sps-scheme`. | [C8](../../references/compiler/src/sps.md) |
| Normalization | Sharing-preserving local β/η reductions plus demand analysis; resolves known calls and prunes unused bindings/fields. | [C8](../../references/compiler/src/sps.md) |
| Localization | Turns a let-bound closure whose every use is a call into a second-class procedure before closure conversion. | [C8](../../references/compiler/src/sps.md) |
| SPSLow (`SPS_l`) | First-order target of closure conversion: blocks, jumps, value-existential closure packages, stack-existential continuation packages. `SpsLowProgram`. | [C9](../../references/compiler/src/closure-conversion.md) |
| Closure package / continuation package | The existential encodings closure conversion produces for captured code and its environment, and for a continuation and its stack. | [C9](../../references/compiler/src/closure-conversion.md) |
| Protocol descriptor | The partial source protocol (value and stack shapes) carried through SPS/SPSLow in place of full types; extraction feeds the SPSLow validators. | [C8](../../references/compiler/src/closure-conversion/word-contracts-and-protocols.md#partial-source-protocols) |
| Word entry contract | Environment/result word roles that a block entry declares and a jump must supply; validated by `low::contracts`. | [C9](../../references/compiler/src/closure-conversion/word-contracts-and-protocols.md#word-entry-contracts) |
| ZASM | Zydeco Assembly Stack Machine: low-level IR with basic blocks and stack instructions, the input to frame analysis and AMD64 emission. | [C10](../../references/compiler/src/zasm.md) |
| Representation policy | `boxed/direct/local/shared` choice for local representation; selected by `--representation`, kept separate from the evidence that permits a change. | [C10](../../references/compiler/src/zasm.md#policy-selection) |
| Frame / root metadata | Activation frame descriptions and GC root maps emitted for the native runtime. | [C11](../../references/compiler/src/native-emission.md) |
| Runtime model | The `no_std` Rust contracts in `lang/machine` shared by compiler and runtime: words, frames, closure and GC metadata. | [C12](../../references/compiler/src/native-runtime.md) |
| Wasm host contract | Tagged words, one-word boxes, bump allocator, and the host-call ABI shared by `lang/wasm-common` and the Node host. | [C13](../../references/compiler/src/webassembly.md) |
| Session / revision / overlay | The revisioned source-input database shared by CLI, TUI, and Cajun; overlays represent unsaved editor or REPL input. | [C3](../../references/compiler/src/sessions-and-queries.md) |
| Query / memo / Salsa | Incremental database infrastructure: source and statics queries, keyed analysis facts, and revision invalidation. | [C3](../../references/compiler/src/sessions-and-queries.md#analysis-facts-and-materialization) |
| Derived identifier / key space | IDs derived from an entity's identity and checking occurrence instead of a global counter; stable under replay. | [C2](../../references/compiler/src/compiler-data.md) |
| Arena schema / allocation scope | Trait-level declarations of which IDs a scope may allocate and which items an arena owns. | [C2](../../references/compiler/src/compiler-data.md) |
| Executable selection | Choosing the program root and establishing completeness before interpretation or lowering. | [C1](../../references/compiler/src/architecture.md) |
| Executable root | A computation accepting the host's Builtin value and ending in its `OS` protocol; required by `run`/`test`. | [L12](../../references/language/src/sources.md) |
| Pass framework | `CompilerPass`, `pipeline!`, runtime sequences, observers: typed composition of transformations with explicit error and observation boundaries. | [C1](../../references/compiler/src/architecture.md#compiler-pass-composition) |

## Naming Traps

Three pairs cause most confusion when reading this repository:

- **Package** is overloaded. A *source package* is a named unit of files; a *packed value* is an
  existential; a *closure package* is an SPSLow runtime record. The qualifier is always present in
  careful prose, but not always in code identifiers.
- **Static** is the checker's world (types, witnesses, elaboration), not "compile-time constant"
  in the C sense and not `static` in Rust. "Static values" are type-level values.
- **ZIR** in code and older prose covers the Stack IR family (high SPS and SPSLow). The current
  compiler reference names the products instead: `FragmentProgram`, `SpsLowProgram`. When a file
  says `zir`, check whether it means the rendering target (`-t zir` prints SPSLow) or the crate.
