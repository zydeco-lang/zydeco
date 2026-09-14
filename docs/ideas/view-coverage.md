# Refutable view coverage

The checker currently treats a view with a refutable nested pattern as opaque for exhaustiveness,
even though it may occur in matches and partial computation bindings.
Investigate grouping arms that apply the same resolved function with the same static arguments,
then checking their result patterns together.
Exhaustiveness over the whole codomain is sufficient; exhaustiveness only over the image needs additional evidence.
Define equivalence and conservative fallback before accepting new programs.
Pair collectively exhaustive result arms with a missing-result case and with distinct views that must not be grouped.

Current rules remain in [L7–L8](../references/language.md#8-value-functions-and-views);
[coverage conversion](../../lang/statics/src/validate/coverage.rs) maps refutable view results to `Opaque`.
