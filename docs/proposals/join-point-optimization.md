# Sharing-preserving join-point optimization

## Motivation and scope

A shared continuation prevents code duplication, but can also hide a useful consumer from a known producer.
In [short-circuit.zy](../../lib/tests/demos/short-circuit.zy), the first false arm returns `False`
to a shared outer match instead of transferring directly to its false alternative.
The intended result has one copy of each result body and one shared procedure `p5` reached from both false paths.

[short-circuit-commuted.zy](../../lib/tests/demos/short-circuit-commuted.zy) writes the
small dispatch movement manually.
The current compiler localizes its shared `f5` and produces two direct jumps to one label.
This establishes the existing mechanism for an already factored program;
it does not establish automatic conversion of the original program.
Merely finding any procedure in its output is insufficient because `read_bool` is also shared.

The [current normalization and procedure rules](../references/compiler.md#second-class-procedures) preserve
unknown branch joins and inline only singly called general closures or procedures.
This proposal supplies sharing-aware control-flow transformations before closure conversion.
It does not add source procedure syntax, change the source evaluation order, or require recursive procedure groups.
[Constructor-aware normalization](constructor-normalization.md) owns contracting the newly exposed known cases;
[procedure environment sharing](procedure-environments.md) owns the later cost of passing captured values.

## First milestone: factor alternatives, then commute dispatch

Case-of-case conversion moves an outer case into the branches of an inner computation.
Copying the outer case verbatim may duplicate large alternatives,
so the proposed transformation should first name any body that would otherwise be copied as a procedure.
It then copies only the dispatch and procedure calls.
For the short-circuit program, the desired control flow is:

```text
p5 = the one shared M5 body
evaluate M1
  False -> call p5
  True  -> evaluate M2
             False -> call p5
             True  -> execute M4
```

The first rewrite should recognize a shared continuation whose body immediately matches its returned value.
Use lexical free-variable and use analyses to determine which alternatives can be factored at a common binding site.
An alternative that uses its returned payload must receive that payload as an explicit argument;
it cannot capture the continuation's binder from outside its scope.
Every extracted body binds its own ambient stack, and its calls supply the residual stack with the established protocol.

Retain an existing shared thunk when it provides the required boundary,
or extract a new procedure when the body has already been exposed.
In the original example `f5` has only one syntactic occurrence before commuting,
so ordinary normalization can inline it too early for a policy that relies only on preserving that binding.
The implementation must either factor before that inlining or recognize and refactor the exposed alternative.
Reordering `localize` and `normalize` alone does neither.

Separate rewrite legality from profitability.
Legality includes lexical availability, procedure non-escape, stack substitution, and unchanged effect order.
Profitability should favor eliminating a known dispatch while bounding copied dispatch nodes and new procedure entries.
Initially decline cases that require moving effects across a join, duplicating arbitrary consumers,
or resolving unknown stack behavior.
A declined optimization must leave the valid input unchanged.

## Generalization: stack abstraction and selective procedure expansion

The paper's stack-abstraction equation provides another way to expose a shared continuation:
name its body as a procedure accepting the returned value, then replace the shared stack
with a forwarding frame that passes the value to that procedure.
Direct returns can contract to direct calls to this dispatcher.
Unknown producers still receive a continuation; the transformation does not remove their return protocol.

Naming the dispatcher alone does not remove its case analysis.
Selective procedure expansion should permit expanding a call supplied with a useful known value
while other calls continue to share the original body.
Reuse the constructor proposal to simplify the expanded dispatcher, and factor large alternatives
before expansion so only the small decision code is copied.
This extends the current single-call inlining rule rather than replacing sharing with unrestricted inlining.

Begin with nonrecursive procedures and a bounded per-call expansion policy.
Copied bindings and syntax need valid occurrence-local identities, scopes, source sites, and protocol evidence;
the [existing stack-copy implementation](../../lang/stackir/src/high/copy.rs) is a starting point,
not a blanket license to reuse definition identities when copies cease to occupy disjoint lexical scopes.
Common-stack folding and mutually recursive procedure groups remain later work:
neither is required to make the short-circuit example reach its shared false target.

## Pass integration

Use independent analyses for candidate discovery, free-variable availability, call uses, and cost.
Share their structural event stream when their scope and scheduling contracts agree.
Reconstruct approved candidates with an `Explicit` folder following the
[traversal selection](../references/compiler.md#choosing-and-composing-traversals)
and [resumable execution](../references/compiler.md#resumable-folder-execution) rules.
Do not make an analyzer mutate the program while another analyzer still depends on its original facts.

Expose the experiment
through the existing [SPS pass selection](../references/compiler.md#selecting-and-inspecting-passes)
before changing the default plan.
Compare these two integration choices:

| Placement | Benefit | Obligation |
| --- | --- | --- |
| Factor before general normalization | Existing source sharing remains visible | Avoid extracting code that normalization would discard or trivially reduce |
| Normalize, then factor exposed alternatives | Candidate code is smaller | Recover sharing from bodies rather than relying on surviving source thunk names |

Follow a successful rewrite with constructor simplification and ordinary dead-binding cleanup.
Do not substitute an unbounded repeat-until-stable loop for a stage contract.
If repeated rounds are needed, specify a progress measure or explicit growth/round budget,
and invalidate use counts and demands when the program changes.

The first milestone targets `br`, whose join is the obstruction in the motivating example.
Keep any accepted input within its declared fragment: `dup` must remain free of stack lets,
and `full` must keep its frame-naming discipline.
Until a scheme is supported, the opt-in pass should decline that scheme explicitly rather
than return a malformed fragment or silently switch schemes.

## Acceptance and failure cases

Extend [the existing short-circuit structural check](../../lang/tests/tests/short_circuit.rs)
from the manually commuted control to the original source:

- Exactly one copy of `M4` and `M5`; one procedure for `M5`, with both false arms jumping directly to it.
- Only the two necessary Boolean dispatches remain, with no return of a known `False` into another dispatch.
- Runtime `0 invalid` and missing-second-argument cases skip M2,
  while their true-first counterparts fail before either result body executes.
  The truth table agrees on interpreter, AMD64, and Wasm.
- Large alternatives remain shared, and nested splits do not reproduce the duplicating scheme's uncontrolled growth.

Pair each generalized rewrite with a case where it must decline or preserve an operation:
live payloads outside the proposed binding scope, calls beneath escaping closures,
different residual stacks, effectful or trapping producers, and unknown constructor tags.
Mutation tests must reject out-of-scope procedure calls, malformed stack substitution, and invalid protocol evidence.
Selective expansion tests should keep one original body for unexpanded calls
and preserve effects exactly once on every executed path.
Deeply nested candidates must not exhaust the Rust stack.

Report dispatches removed, procedure bodies retained, emitted code growth, and compile work separately.
A smaller IR is not by itself a runtime speedup claim.
After the transformation and its supported schemes are approved and implemented, move its settled legality,
pass order, and limits into the compiler reference; retain only further generalizations here.
