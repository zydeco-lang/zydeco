# Constructor-aware normalization through aliases

## Motivation and scope

A compiler can know which constructor a value contains without having the constructor expression at its use site.
Administrative bindings introduced by returning a value and matching it commonly create this situation.
The original [short-circuit example](../../lib/tests/demos/short-circuit.zy), compiled with the duplicating SPS scheme,
retains the equivalent of this dispatch in its first false arm:

```text
let a = False() in
let b = a in
case b | True() -> p4 ! • | False() -> p5 ! • end
```

The tag and unit payload suffice to choose the false arm, but the current normalizer retains the match.
The [local-reduction contract](../references/compiler.md#local-reductions) deliberately distinguishes knowing a tag
from extracting a stored payload.
That distinction protects evaluation and sharing; the proposed extension should remove redundant tests
without discarding that protection.

This proposal extends optional SPS normalization, not source coverage or required static elimination.
It owns the evidence needed to simplify a constructor match through bindings.
[Join-point optimization](join-point-optimization.md) owns moving a consumer to a site
where this simplification applies.
Neither proposal changes the default SPS scheme by itself.

## Proposed evidence and reduction

Treat branch selection and payload recovery as separate questions.
The existing `KnownValue::Constructor` fact can establish the tag;
an additional recovery decision must establish how the selected arm receives any payload it observes.
Use structured facts over definition identities and lexical scopes, not variable names
or a recipe to reevaluate the original producer.
A source node ID alone does not prove that its value has already been evaluated.

Begin with two conservative cases:

1. The selected pattern observes only a known trivial payload, such as the unit carried by `Bool`.
   No payload computation needs to be copied or extracted.
2. The selected arm does not observe the payload, and the pattern's remaining tests are established by producer facts.
   Remove the dispatch while retaining any nondiscardable producer at its original evaluation point.

A later extension may recover an already evaluated payload through an in-scope binding.
When no such binding exists, splitting the constructor's original binding can name its payload once,
then construct the shared value from that name.
Do this at the producer, preserving field evaluation order;
do not reconstruct the payload expression independently at each consumer.
Keep the match when recovery would require unavailable bindings, repeated evaluation, or an unsupported pattern test.

The selected successor must remain the first matching arm under the existing decision rules.
Known tags do not authorize skipping earlier uncertain tests, altering refutable payload matching,
or moving a trapping stack argument.
Removing a case does not make its producer discardable: the normalizer's existing effect
and demand rules still decide which bindings survive.

## Implementation boundary

The main implementation sites are `binding_fact`, `components`, and `pattern_facts`
in [`normalize.rs`](../../lang/stackir/src/high/normalize.rs), the branch-selection guard
in [`normalize/fold.rs`](../../lang/stackir/src/high/normalize/fold.rs),
and the [pattern decision folder](../../lang/stackir/src/high/normalize/decision.rs).
Extend their semantic facts rather than teaching each caller a separate alias-chasing simplifier.
Payload availability and consumer demand should remain distinguishable inputs to a reduction decision.

Reuse the [normalization reconstruction schedule](../references/compiler.md#normalization-reconstruction):
producer facts flow forward, and completed consumers determine demands.
If an unused-payload decision requires the rebuilt arm, represent the deferred decision in an explicit folder frame;
do not run a nested normalization traversal or guess demand before rebuilding the consumer.
New structural analysis should use the shared SPS traversal, and any reconstruction should follow the
[resumable folder interface](../references/compiler.md#resumable-folder-execution).

Facts must respect definition identity, shadowing, closure boundaries, and their producing environment.
Preserve source provenance and protocol evidence on rebuilt nodes, and validate lexical ownership
and scheme placement at the existing `FragmentProgram` boundary.
The first implementation should prove the unit-payload case without requiring a general product-projection system.

## Acceptance and failure cases

| Enabled reduction | Required counterpart or invariant |
| --- | --- |
| A known Boolean through several aliases selects its arm | An unknown tag retains dispatch; shadowed names do not inherit another definition's facts |
| A known trivial payload needs no extraction | A live opaque payload retains its match until a valid recovery path exists |
| An unobserved payload permits removing a redundant test | A trapping producer still executes once, at the same point relative to effects |
| Several consumers use a named evaluated payload | Construction and payload effects are not repeated for each consumer |
| A match under a movable continuation simplifies | A trapping stack argument is not delayed past effects |
| A selected arm uses bindings available in its lexical scope | Corrupted recovery evidence with an unavailable definition fails validation |

Retain and extend `known_constructor_aliases_keep_the_match_that_extracts_the_payload`
in [`normalize.rs`](../../lang/stackir/src/high/normalize.rs).
Pair it with unit-payload, unused-payload, and alias-chain cases, including nested patterns and multiple consumers.
Validate observable effects, failure order, and payload sharing rather than only counting removed cases.

As an integration target, compile the original short-circuit program with `--sps-scheme dup`
and require its first false arm to jump directly to the shared false target, with no intervening known-Boolean dispatch.
Keep its runtime truth table and skipped-invalid-input checks
in [`short_circuit.rs`](../../lang/tests/tests/short_circuit.rs).
The branch-join scheme additionally needs the separate join-point transformation.

## Decisions before implementation

Choose whether payload recovery initially recognizes only trivial payloads or also unobserved payloads.
Document the scope and evaluation evidence carried by any reusable payload reference before generalizing it.
Measure facts retained and work performed on long alias chains and deep patterns;
a reduction that needs repeated whole-subtree scans should be redesigned before enabling it by default.

Once implemented and approved, move the settled reduction and failure invariants
into the compiler reference's local-reduction and sharing sections, leaving only unfinished payload-recovery work here.
