# C8. SPS Lowering, Normalization, and Demand

[SPS lowering](../../../../lang/stackir/src/high/lower.rs) is indexed by the stack consuming a residual computation.
It builds complete user and Builtin structures into a `FragmentProgram`,
which records the [compilation scheme](../../../../lang/stackir/src/scheme.rs) whose fragment the program lies in.
SPS retains lexical values, closures, continuations, arguments, and explicit ambient stacks.
Under the default branch-join scheme, stack lets guard value-coproduct matches
and scalar comparisons so every branch shares one supplied stack; under the duplicating scheme,
no stack let occurs and every arm consumes its own copy of the stack; under the full scheme,
a stack let names every pushed frame and every consumer receives `•`.
The [SPS verifier](../../../../lang/stackir/src/high/check.rs) checks closed roots, lexical ownership,
and the placement rule of the program's scheme.

The [pass composition contract](architecture.md#built-in-plans-and-phase-boundaries) owns the configurable
stage between SPS checks and closure conversion, including its default normalization and built-in plans.
The [selection and inspection guide](architecture.md#selecting-and-inspecting-passes)
gives the corresponding CLI commands.
These optimizations are optional consequences of known runtime structure;
they do not relax L10's source elimination boundary.
The normalizer preserves definition identities while allocating fresh syntax for the surviving lexical tree.

## Correspondence with SPS

SPS separates values, stacks, and programs.
Continuations, argument pushes, and tag pushes build stacks; each program consumes one ambient stack `•` exactly once
on every path, and no value captures it, because closures receive their stack at each call.
SPS lowering follows the paper's stack-passing compilation in the scheme that `--sps-scheme` selects.
Indexed by the consuming stack, it pushes an argument for each application and a tag
for each destructor and installs a continuation for each `do`.
The branch-join and duplicating schemes extend the consuming stack with each frame and differ at a value case split;
the full scheme names each frame with a stack let as it is pushed, so every consumer receives `•`:

| Scheme | Paper fragment | Pushing a frame | At a value case split or scalar comparison |
| --- | --- | --- | --- |
| `br` (default) | `SPS_br`, whose stack lets occur exactly around value-coproduct matches | extends the stack | one stack let names the stack that the arms share |
| `dup` | `SPS_dup`, which has no stack let | extends the stack | each arm consumes its own [copy](../../../../lang/stackir/src/high/copy.rs) of the stack |
| `full` | `SPS_full`, whose stack lets each bind one frame and whose consumers receive `•` | `let • = frame :: • in …` | the arms consume `•`, which the lets above the branch name |

One [placement rule](../../../../lang/stackir/src/scheme.rs) per scheme checks its fragment in SPS and SPSLow;
the same module classifies each computation's stack use for both phases, asking a phase's own forms
which stack they consume and which frame shape their stacks have.

SPS and SPSLow share one [syntax](../../../../lang/stackir/src/syntax.rs): values, stacks, and computations are generic
over a `Phase` that supplies the node identifiers and the forms it adds, which the shared enums carry under `Ext`.
The table names a form by the [lexical SPS extension](../../../../lang/stackir/src/high/syntax.rs)
where SPS adds it and by the shared enum otherwise.

| SPS form | Syntax node | Printed |
| --- | --- | --- |
| Variable `x`, literal `n` | `Value::Var`, `Value::Literal` | `x`, `n` |
| Closure `λ_c •. M` | `ValueExt::Closure` | `{ • -> M }` |
| Tuple `(V_i)_i` | `Value::Triv`, `Value::VCons` | `()`, `(V, W)` |
| Injection `σ_c(V)` | `Value::Ctor` | `c(V)` |
| Stack variable `•` | `Stack::Var` | `•` |
| Continuation `λ_k x. M` | `StackExt::Kont` | `(kont p -> M)` |
| Argument push `V :: S` | `Stack::Arg` | `arg(V) :: S` |
| Tag push `π_d :: S` | `Stack::Tag` | `tag(d) :: S` |
| Call `V ! S` | `CompuExt::Force` | `V ! S` |
| Return `S @ V` | `CompuExt::Ret` | `S @ V` |
| Argument match `match_⊘ S {x :: • => M}` | `Computation::LetArg` | `let arg(p) :: • = S in M` |
| Tag match `match_⊕ S {π_d :: • => M_d}` | `Computation::CoCase` | `cocase S \| d -> M end` |
| Value let `let x = V in M` | `Computation::LetValue` | `let p = V in M` |
| Stack let `let • = S in M` | `Computation::LetStack` | `let • = S in M` |
| Product match `match_Π V {(x_i)_i => M}` | `Computation::ProductMatch` | `match-product V as p in M` |
| Coproduct match `match_Σ V {σ_c(x_c) => M_c}` | `Computation::CoprodMatch` | `case V \| c(p) -> M end` |
| Fixed point `fix (g : Closure B). M` | `CompuExt::Fix` | `fix g ! S -> M` |
| Procedure `letproc p • = N in M` | `CompuExt::LetProc` | `letproc p • = N in M` |
| Procedure call `p ! S` | `CompuExt::CallProc` | `p ! S` |

The implementation departs from SPS where Zydeco's source language or its backends need more:

- Types are erased;
  [partial source protocols](closure-conversion/word-contracts-and-protocols.md#partial-source-protocols) retain part
  of the SPS types that the paper's type translation assigns.
- Binders are irrefutable value patterns rather than variables, which SPS expresses with product matches;
  refutable source patterns lower to [ordered pattern plans](#residual-lowering-folder) built from one-level matches,
  scalar comparisons, and fallback closures.
- A fixed point carries the stack it runs on and binds a fresh `•` for its body,
  so stack substitution stops at it as it does at closures.
- Primitives, address offsets, scalar comparisons, external calls, memory steps, and holes extend the calculus.
  A scalar comparison is a second branching form that each scheme treats as it treats a value case split.

The duplicating scheme trades sharing for copies.
`n` nested splits copy a consumer `2ⁿ` times, and structured Wasm declares every local of the program in every function,
so heavily duplicated programs can exceed a WebAssembly engine's limit on locals.

Normalization orients part of the paper's equational theory as rewrites.
The [local reductions](sps/normalization-and-demand.md#local-reductions) orient its β- and η-equations:
closure β is inlining, continuation β is call elimination, continuation η removes a forwarding continuation
as tail-call optimization, closure η forwards a closure that only calls another,
and argument, tag, product, and coproduct β select known components.
[Sharing and discardability](sps/normalization-and-demand.md#sharing-and-discardability) orient the two let equations,
the paper's join points: a value let is substituted only under the occurrence and discardability conditions,
and a stack let disappears only when its branch is selected.
Rewrites apply only where sharing, effects, and stack movement permit; recursion is never unfolded,
and normalization does not compute βη normal forms.
Normalization preserves its input's scheme: at an unknown branch, branch-join rebuilds the guard's stack once,
while the duplicating scheme rebuilds a delayed consumer in every arm.
Under the full scheme, normalization delays a frame let whose frame is movable, so reductions see through it,
and keeps a let whose argument may trap; a consumer or an unknown branch then names the frames it receives above itself,
one let per frame, deepest first.

## Residual Lowering Folder

Static composition can produce thousands of nested bindings from a small authored program.
The [lowering folder](../../../../lang/stackir/src/high/lower/fold.rs) uses the shared
[resumable folder driver](compiler-data.md#resumable-folder-execution) for patterns, values, computations,
and ordered match decisions together; thunk bodies and branch tails resume through the same driver.
Builtin packed value materialization uses the same driver,
so residual reconstruction depth does not consume the Rust call stack.
`Lowerer` retains allocation, provenance, product layout, protocol extraction, and diagnostic state.

Domain frames retain the next reconstruction operation, while typed result stacks hold completed patterns,
value-binding plans, and computations.
Product cursors and branch iterators request one child at a time.
This retains structural order without constructing a separate list of pending child calls.
Tail calls cover erased wrappers and intermediate transitions that need no additional unfinished parent.

Computation visits inherit the SPS stack that consumes their result.
Reconstruction preserves the semantic child schedule: application arguments precede their function body;
a `do` continuation is lowered before its bindee; value-binding plans wrap their completed continuation
in evaluation order.
Closures and coproduct branches receive fresh ambient-stack nodes.
Definitions retain their identity, and generated patterns and terms retain their typed source sites
and applicable protocols.
Erased witnesses and type applications produce no runtime nodes.
Irrefutable constructor bindings left by static value elimination become explicit coproduct matches here,
including when optional normalization is disabled.

Input sharing always expands into fresh output syntax for each occurrence.
Memoizing lowered nodes by typed ID would violate lexical ownership and ignore inherited continuation stacks.
Ordered pattern plans may share flat plan IDs, including fallback decisions,
but each execution of a plan constructs its own syntax.
Constructor fallthrough binds the remaining rows once as a closure and resumes it
from rejected payloads or unmatched tags.
Flat plan storage also avoids recursive cloning and destruction of long decision chains.

Residual static-value failures accumulate in occurrence order; any failure prevents publication of a program.
Successful reconstruction validates lexical ownership and branch joins before returning a `FragmentProgram`.
[Depth regressions](../../../../lang/stackir/src/high/lower/tests.rs) construct and drop residual fixtures
on a 512 KiB worker stack, covering mixed computation and thunk nesting, value lets,
structural aliases, literal fallthrough, and Builtin products.
A mixed reconstruction with ordered literal fallthrough lowers to a valid fragment.
Separate assertions cover fresh ownership and protocols, and independent rejected values accumulate
before any program is published.
This guarantee concerns residual reconstruction; classifier protocol extraction, normalization analyses,
and subsequent conversion have their own traversal contracts.

## SPS Analysis Traversal

Lexical ownership, branch-join placement, and free variables all depend on the same SPS structure.
The [shared traversal](../../../../lang/stackir/src/traverse.rs) enumerates the children of the forms
that SPS and SPSLow share once; each phase enumerates the children of its own forms through `Children`,
so the [lexical SPS instance](../../../../lang/stackir/src/high/traverse.rs) adds only closures,
continuations, fixed points, and procedures.
It exposes borrowed pattern, value, stack, and computation nodes; definitions remain leaves.
An entry event identifies the immediate incoming edge and whether the occurrence is first, shared, or cyclic.
A stack-let tail carries the ID of its owning stack let so placement checks do not need their own recursive descent.

Every incoming edge is observed, including repeated ownership.
Children and exit events are scheduled once per node; repeated edges do not expand the same subtree again,
and active back edges terminate traversal of that edge.
`FragmentValidator` reports repeated owners and misplaced stack lets without stopping independent checks;
it applies the placement rule of the program's scheme.
`FragmentProgram` is constructed only when its complete `FragmentErrors` collection is empty.
This preserves lexical-tree ownership even though the structural driver can safely inspect a malformed graph.

The shared [`Variables`](../../../../lang/stackir/src/variables.rs) analysis computes bound and free variable summaries
at exit, reading completed child facts; a phase's own forms contribute their rules through `FreeVariables`.
Pattern binders remove names only from their lexical bodies; recursive binders,
continuations, and match arms retain their existing scope rules.
A cyclic graph has no free-variable result. The public `Vars` and `FreeVars` operations use this analyzer,
and closure conversion computes one set of facts for its immutable input, reusing them for every capture list.
Those transient facts are released with conversion; keeping summaries trades temporary memory
for avoiding repeated walks through nested closure bodies.

`Together` delivers the same events to independent analyzers.
SPS verification composes ownership and variable analysis in one traversal
and retains both invariant failures when the graph is acyclic.
The normalizer still owns forward producer propagation and backward consumer demands.
[Traversal regressions](../../../../lang/stackir/src/high/traverse/tests.rs) compare separate and composed results,
count entries and exits on shared syntax, and cover binders and cycles with independent ownership failures.
