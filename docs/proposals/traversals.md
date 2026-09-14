# Reusable folders and traversal composition

This proposal retains unfinished extensions to compiler traversal and diagnostic recovery.
The implemented surface, classifier, runtime, and high SPS interfaces are indexed
by [choosing and composing traversals](../references/compiler.md#choosing-and-composing-traversals).
That reference owns their shared design rules and links to the detailed phase contracts.
The [diagnostic collection contract](../references/compiler.md#diagnostic-collection) records current recovery
and reporting behavior.

## Diagnostic collection and recovery

Continue auditing producers for early exits across independent work.
Two concrete candidates remain:

- Package catalog construction visits selected roots and discovered files through separate fallible requests.
  Collect independent provider and discovery failures without publishing an incomplete catalog.
- [SPSLow validation](../../lang/stackir/src/low/check.rs) returns the first structural,
  entry-contract, or protocol failure.
  Establish recoverable structural regions before batching dependent contract checks;
  malformed ownership and cyclic input cannot supply the evidence those checks expect.

More checker recovery should begin with a demonstrated diagnostic or tooling gap.
The implemented [checker recovery](../references/compiler.md#checker-recovery) covers independent components,
arms, closed imports, and finalization roots.
Recovery through failed sequential bindings, telescope witnesses,
or ambiguous clause prefixes would require an explicit representation of unavailable evidence
before their dependent bodies could be checked.
Do not turn those prerequisites into fabricated annotations merely to continue traversal.

Error presentation customization remains deferred.
A future renderer can group or sort the retained diagnostics without changing producer order
or the phase's acceptance boundary.

## Additional graph views and adapters

A borrowed adapter for both bitter and scoped syntax may become useful if another analysis needs it.
Their [debug formatter](../../lang/surface/src/debug.rs) already adapts the shared syntax family,
while the implemented owned folder serves reconstruction.
Wait for another concrete borrowed consumer before expanding the interface.
Generating borrowed traversal and owned rebuilding from one structural declaration is likewise optional;
first establish that two real clients share a stable child description.

Further typed migrations must identify the graph they observe.
[Type support collection](../../lang/statics/src/normalize/scope.rs) depends on occurrence-specific scope constraints;
a replacement needs a context-sensitive traversal or a justified memo key under the reference's cache rules.
The raw classifier folder and residual runtime iterator provide distinct starting points. Further SPS
rebuilding must preserve the established [consumer-demand schedule](../references/compiler.md#consumer-demands).

## Pruning and synthesized results

The existing `Together` visitors compose independent observations under one traversal policy.
A future generic interface for synthesized child results could pair those results at each node,
letting each analyzer consume only its own child's component.
Adopt it only when it removes concrete repetition beyond the current postorder summaries.

Independent pruning needs a separate design: one analyzer cannot discard a subtree required by another.
Candidate approaches are per-analyzer activity masks or separate traversal groups.
Specify callback balancing, partial-result status, and diagnostic collection before choosing an approach.
Neither facility is needed by the current composed analyzers.

## Migration acceptance

For each extension, identify the client and its repeated structural rule,
then apply the [reference's traversal selection criteria](../references/compiler.md#choosing-and-composing-traversals).
Remove the superseded recursion and update callers in the same change.
Compare accepted behavior and rejected inputs, including error multiplicity and locations,
arena identities, provenance, and the absence of invalid normal products.
Measure traversal work, allocations, and temporary storage before claiming compilation-time or memory improvements.
