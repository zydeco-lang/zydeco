# C9. Closure Conversion and First-Order SPSLow

[SpsLowConverter](../../../../lang/stackir/src/low/convert.rs) consumes lexical SPS and creates fresh low syntax.
Free-variable analysis determines ordered captures; renamed capture bindings close each generated block.
A closure becomes a record with an explicit environment and code pointer.
Its entry unpacks the captured environment before consuming ordinary arguments.
A continuation record retains its code and residual stack, including the bindings needed when it resumes.
Force and return open the corresponding closure or continuation record and jump to its code.

[Low syntax](../../../../lang/stackir/src/low/syntax.rs) separates `Block`, `Jump`,
`ClosurePackage`, `ContinuationPackage`, `OpenClosure`, and `OpenContinuation`.
Products, primitives, patterns, and external-call forms share their structural vocabulary with SPS.
Source captures remain value identities; code labels and lexical syntax nodes belong to the new low arena.
Administrative definition allocation continues from the consumed SPS program as described in C2.

`SpsLowProgram` has one root and first-order closed blocks.
Its [verifier](../../../../lang/stackir/src/low/check.rs) checks reference closure, lexical node ownership,
the stack placement of its scheme, label scope, and the word entry contracts below.
Conversion is homomorphic on stack lets, so SPSLow records and keeps the scheme of its SPS input;
under the full scheme, a continuation package, whose residual is the saved environment over `•`, binds as one frame.
A single-occurrence syntax tree means runtime sharing is explicit in variables;
it is not a proof that a continuation is dynamically used once.
Native preparation must establish the stronger lifetime facts in C11.
Structured Wasm can consume this representation directly because block boundaries and residual stacks are explicit.

## Correspondence with `SPS_l`

`SPS_l`, the paper's first-order target of closure conversion, keeps the three sorts
of SPS and replaces closures and continuations with labeled blocks, jumps, and existential packages.
A block `λ[l](• : B). M` is closed except for its own label `l`, and a jump `V !! S` enters a block with stack `S`.
A closure becomes a value package of type `Cl_B = ∃X. X × Label(X ⊘ B)`, which hides its environment type,
and a continuation becomes a stack package of type `Ko_A = ∃•Y. Label(A ⊘ Y) ⊘ Y`, which hides its residual stack.

| `SPS_l` form | SPSLow node | Printed |
| --- | --- | --- |
| Block `λ[l](• : B). M` | `ValueExt::Block` | `block [l](environment: p) • -> M` |
| Jump `V !! S` | `CompuExt::Jump` | `jump V(environment: E) ! S` |
| Closure package of type `Cl_B` | `ValueExt::ClosurePackage` | `pack-closure(E, V)` |
| Opening a closure package | `CompuExt::OpenClosure` | `open-closure V as (p, q) in M` |
| Continuation package of type `Ko_A` | `StackExt::ContinuationPackage` | `pack-continuation(V, S)` |
| Opening a continuation package | `CompuExt::OpenContinuation` | `open-continuation S as q :: • in M` |
| Label of a procedure | `CompuExt::LetLabel` | `label p • -> N in M` or `closed label p • -> N in M` |
| Jump to a label | `CompuExt::JumpLabel` | `jump p ! S` |

Continuation blocks list `result: p` before their environment, and returns jump with `result: V`.
Label jumps carry no word: they supply only a stack.
The remaining forms are those of SPS without closures, continuations, calls, returns, and fixed points.
Conversion follows the paper's translation: a call opens the closure package and jumps
to its code with the environment above the caller's stack; a return opens the continuation package and jumps
to its code with the result above the restored residual stack, which begins with the saved environment;
and the structural forms translate homomorphically.

[Procedures](sps/normalization-and-demand.md#second-class-procedures) lower to the labels
that the paper adds to `SPS_l` for them.
A label runs its body over the frame of the block that binds it, and a call becomes a direct jump
that supplies only a stack, with no package.
The [placement analysis](../../../../lang/stackir/src/low/convert/procedures.rs) keeps this local form
when every call lies in the binding block.
The paper's continuation packages save every variable in scope,
so a procedure stays reachable from a continuation's body.
Here a continuation becomes a block of its own that captures only what its body reads,
so a procedure also called from inside a continuation, or from inside a closed label's body, becomes a closed label.
Its body pops the procedure's free variables, ordered by definition identity,
and each jump pushes them over its stack; under the full scheme a stack let names each pushed variable.
A continuation that calls a closed label captures those variables rather than a closure package.
The verifier keeps each jump in its label's scope: a local label is reached only from its own block,
no label is reached from a closure block, and a closed label's body reads no variable.
A procedure that no call reaches is dropped.

The implementation departs from `SPS_l` in these respects:

- Only the two package shapes that conversion builds exist.
  Opening a package also binds its code, and a closure's environment, which the paper's eliminator reaches
  through a further product or argument match.
- Entry words are explicit: the environment and result that the paper's block patterns consume
  become [entry parameters](closure-conversion/word-contracts-and-protocols.md#word-entry-contracts),
  and each jump supplies the corresponding word.
- An environment captures the free variables of the converted body, ordered by definition identity,
  rather than every variable in scope.
  For the same reason, a procedure called from inside a continuation receives its free variables
  at each jump instead of finding them in a restored frame.
- A fixed point becomes a closure block whose body first rebuilds its own package from its label and environment,
  the paper's self-referential block.
- Types are erased; entry kinds,
  [entry contracts](closure-conversion/word-contracts-and-protocols.md#word-entry-contracts),
  and [partial source protocols](closure-conversion/word-contracts-and-protocols.md#partial-source-protocols) check part
  of the paper's typing.
- No pass rewrites SPSLow, so the `SPS_l` equations, including the package laws
  that relate packages hiding different environments, justify conversion without being oriented as passes.

## Closure Conversion Folders

[Closure conversion](../../../../lang/stackir/src/low/convert/fold.rs) uses the shared
[resumable execution interface](compiler-data.md#resumable-folder-execution)
for mutually dependent value, stack, and computation translation.
Requests carry source IDs and indices into the conversion's renaming environments.
Frames retain ordered captures, translated binders, and branch positions
while typed result vectors hold completed children until their parent can be allocated.
Capture bindings, recursive labels, block entries, and record metadata retain their established allocation order.
Each source occurrence creates fresh output syntax with its corresponding origins and protocols.

[Pattern translation](../../../../lang/stackir/src/low/convert/pattern.rs) is a separate folder using the same driver.
Each completed pattern returns both first-order syntax and its ordered source-to-fresh-binder assignments.
Constructor frames retain one payload; alias and product cursors accumulate children
and assignments in structural order.
Pattern syntax has no edge back to values, stacks, or computations, so this subwalk does not retain a recursive cycle
through the conversion folder.

Bounded regressions check syntax, binder order, origins, layouts, entry protocols, and continuation capture metadata.
Pattern regressions also cover empty field vectors and rejected layouts that cannot publish their parent.
An 8,192-level alias fixture constructs, translates, and drops on a 512 KiB stack.
Direct conversion fixtures additionally reconstruct and drop 4,096 nested bindings
in a closure and a 16,384-frame argument stack on that stack size.
These fixtures isolate reconstruction from downstream validation.
Semantic protocol cloning and destruction, low verification, and later phases retain their own depth requirements;
explicit conversion does not establish an end-to-end depth guarantee.

## SPSLow Traversal and Analyzers

The [SPSLow instance](../../../../lang/stackir/src/low/traverse.rs)
of the [shared traversal](sps.md#sps-analysis-traversal) describes the executable children of blocks,
packages, jumps, labels, and openings once.
Its resumable folder retains a borrowed node and the next child position.
Each child selection takes constant time, including wide products and branch vectors; frames do not copy those vectors.
`Traversal::run` executes through the shared driver.

Every incoming edge produces an entry event marked first, shared, or cyclic.
Only the first occurrence expands children and receives an exit event.
Block entry patterns precede their body in stack-consumption order;
the tail of a stack let carries its immediate branch-join owner.
Continuation metadata and provenance refer to existing syntax and add no executable edges.
`Together` sends the same events to independent observers.

The [SPSLow variable rules](../../../../lang/stackir/src/low/variables.rs) extend the shared summaries,
which compute bound patterns and free terms at exit.
Block labels and entry words bind only their body; record openings and local bindings likewise remove definitions
from their dependent body rather than from the record or bindee.
A cyclic traversal exposes no usable variable summaries.
The [structural validator](../../../../lang/stackir/src/low/check.rs) observes ownership
and branch-join edges alongside these summaries, then checks block captures,
continuation contexts, and root closure from the completed facts.
This avoids rescanning each block body and continuation context.
Malformed structure is rejected before dependent capture checks consume summaries.
The temporary summaries are dropped before entry-contract and protocol validation.
Validation still returns one error; broader diagnostic recovery remains
[unfinished work](../../../proposals/traversals.md#diagnostic-collection-and-recovery).

Traversal regressions compare composed and separate analyses and cover event order, sharing, cycles,
entry words, lexical binders, empty and wide fields, and rejection without publishing an invalid program.
Direct fixtures analyze and drop 16,384 nested bindings on a 512 KiB stack,
including a block whose capture check reuses those summaries.
Entry-contract and protocol validation remain separate semantic traversals with their own depth requirements.
Summary storage is proportional to retained facts, which can exceed syntax size;
these tests establish traversal counts and depth behavior rather than memory or compilation-time gains.
