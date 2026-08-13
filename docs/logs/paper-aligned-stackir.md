# Paper-Aligned Stack IR

Stack IR implements the stack-passing calculus developed in the adjacent
`stack-passing-style` paper. The compiler should expose that correspondence in its phase boundaries and invariants,
rather than approximate it through an implementation-specific substitution normal form.

This worklog records the architecture being implemented. The paper remains authoritative for the formal syntax,
typing judgments, equational theories, branch-join compilation, reconstruction, and closure conversion.

## High-Level Pipeline

The native pipeline has the following semantic phases:

```text
checked CBPV root
  -> branch-join SPS
  -> sharing-preserving local SPS simplification
  -> closure conversion into first-order SPS_l
  -> assembly
```

Each phase consumes one complete program and produces one complete program. A pass may use mutable storage while
constructing its result, but it does not rewrite its input arena in place. High-level SPS represents lexical syntax;
first-order `SPS_l` and assembly introduce explicit labels and are the first graph-shaped representations.

## Branch-Join SPS

Checked computations compile relative to the stack that will consume their result. At the root, that stack is the
ambient stack `bullet`. Application, sequencing, and destruction extend the supplied stack directly. Functions and
comatches inspect the supplied stack and compile their bodies relative to a freshly rebound `bullet`.

A value coproduct match is the only construct that names its supplied consumer during branch-join compilation:

```text
compile(match value { constructor pattern => branch }, stack) =
  let bullet = stack in
    coproduct-match value {
      constructor pattern => compile(branch, bullet)
    }
```

The resulting fragment has a syntactic invariant:

- every stack let-binding immediately guards a value coproduct match;
- every value coproduct match is immediately guarded by a stack let-binding;
- the same condition holds recursively in closure and continuation bodies; and
- every reachable arena node has one lexical occurrence, so sharing is represented by an explicit binding.

Product elimination is distinct from coproduct case analysis because it selects one continuation and therefore
threads the supplied stack without introducing a join point. Stack choice elimination likewise rebinds the residual
stack in each branch and does not name an outer shared consumer.

`BranchJoinProgram` is the checked boundary for this fragment. Its constructor validates the invariant, while the
checked-term compiler constructs it directly. A general SPS program may exist transiently after a transformation,
but a branch-join program is never inferred from naming conventions or iteration history.

## Presentation Versus Optimization

Branch-join placement and local simplification answer different questions. Branch-join compilation selects one
representative of the SPS join-point theory. Local simplification orients selected equations to remove concrete
overhead.

The simplifier names every implemented rule. Its initial rule set consists of the closure, continuation,
argument-push, and tag-push beta rules together with forwarding eta rules. Value and stack lets remain explicit when
substitution would duplicate their right-hand side across control-flow branches. An expanding rewrite requires an
explicit cost policy; it cannot be enabled accidentally by a generic substitution operation.

There is no fixed count of whole-program normalization rounds. If a transformation needs to restore branch-join
placement, it uses the paper's reconstruction followed by branch-join recompilation, which is an idempotent
projection. A local rewrite schedule may revisit the parent of a contracted redex, but termination follows from the
selected rules and their stated measure rather than from a magic fuel value.

## Zydeco Extensions

The implementation extends the paper calculus with constructs required by the language and runtime. Each extension
must have an explicit syntax form, static contract, SPS behavior, simplification policy, and downstream lowering.
The current extensions are:

- computation recursion (`Fix`);
- external calls and builtin operators;
- holes used for diagnostics or aborted lowering;
- runtime literals;
- product-layout information; and
- rich, irrefutable value patterns.

These extensions do not weaken the branch-join invariant. In particular, an irrefutable value pattern does not turn
product elimination into coproduct branching, and no extension may introduce an unclassified stack let-binding.

## First-Order SPS_l

High-level SPS closures and continuations capture lexical context. Closure conversion consumes high-level SPS and
produces a distinct `SpsLowProgram` with blocks, jumps, and explicit closure and continuation packages. Closure
packages contain an environment and code label. Continuation packages contain a code label and residual stack.

The translation is structural and produces fresh output. It does not encode continuations as ordinary thunk values,
scan all nodes in an arena, replace nodes in place, or represent the two package forms as undocumented generic
products. Runtime type erasure may remove existential witnesses after this boundary, but the IR keeps closure and
continuation packages as distinct typed data.

## Validation

The implementation should be checked against structural examples derived from the paper:

- a tail call exposes a continuation eta-redex;
- a known primitive call exposes closure, argument-push, and continuation beta-redexes;
- dispatch introduces exactly one stack join immediately above its coproduct match;
- nested dispatch grows linearly rather than duplicating its consumer;
- branch-join validation covers every reachable closure and continuation body; and
- reconstruction followed by branch-join recompilation is structurally idempotent.

Repository end-to-end tests additionally cover Zydeco extensions and require interpreted behavior alongside
successful native lowering.

## Implementation Status

The first transition establishes the high-level boundary: checked terms compile directly to a validated
`BranchJoinProgram`; product and coproduct elimination are separate syntax; and the substitution-normal form,
fixed-round schedule, general in-place inliner, and their mutation helpers have been removed. The existing closure
conversion is now a reachable structural translation. It alpha-renames duplicated lexical binders and constructs a
compact output arena after consuming its source arena; no source node is retained or rewritten.

That structural converter is an implementation bridge, not the final `SPS_l` boundary. The current optional CPS
translation still represents continuations through thunks, and explicit closure packages still use ordinary product
syntax. The CPS bridge nevertheless follows the same ownership rule, consuming its source and constructing a compact
output arena. The next representation change is therefore a distinct `SpsLowProgram` implementing blocks, jumps,
closure packages, and continuation packages directly. A named, cost-aware local simplifier should be added only
after those source and target syntaxes make its equations unambiguous.
