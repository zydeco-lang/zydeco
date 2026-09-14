# Completion extensions and revision resilience

[Recovering parsing](../references/compiler.md#recovering-parsing) owns the implemented grammar,
strict/recovering boundary, typed expectations, and exact cursor identity.
[Completion queries](../references/compiler.md#completion-and-documentation) own current scope,
type compatibility, ranking, and source-path behavior.
This proposal extends those facts with structural candidates, richer snippets, and optional last-successful state.

## Candidate families

Visible definitions, metadata, and source paths are implemented.
Contextual snippets, structural members, and missing arms are extensions that build on the same request:

| Family | Site | Candidate source | Inserted form |
| --- | --- | --- | --- |
| Contextual forms | parser expectation | typed syntax-form catalog | keyword or snippet |
| Constructors | `+` term or match pattern | expected or scrutinee data type | constructor or arm snippet |
| Destructors | `.` postfix or comatch arm | receiver or expected codata type | destructor or arm snippet |
| Named fields | `/` projection or `#` field position | product or existential structure | field name or pattern |

Contextual forms include `let`, `do`, `fn`, `match`, `comatch`, `data`, `codata`,
`forall`, `exists`, `pack`, and `begin`.
The grammar decides whether the form is admissible.
A `SyntaxForm` catalog may add an editor-friendly description and placeholder structure
because useful snippet phrasing is presentation knowledge that cannot be derived mechanically from an LR production.
Each snippet must have a representative parsing test so its syntax cannot drift from the grammar.

Constructor, destructor, field, and missing-arm completion are type-checker queries rather than global name lists.
They should follow visible-definition completion because they require ownership and expected-type information
that ordinary lexical scope does not provide.

Package-aware import targets can extend the existing source-candidate interface.
Keep source identification in the typed metadata schema rather than testing the spelling `import` in Cajun.

## Cajun state after a failed revision

Cajun currently removes a cached `ProjectState` when analysis of the corresponding document revision fails.
Merely retaining that value in place would be unsafe: definition, rename, semantic-token,
and scope ranges would refer to old text, and completion could suggest a shadowed definition
after the edit introduced a new binder.

If last-successful memory is added, it should be represented separately from current-revision state:

```text
open document revision
  current analysis: successful | failed | pending
  last successful analysis: optional, with its own revision and source
```

Feature policy is explicit. Source edits and semantic ranges require current-revision facts.
A last-successful project may provide a conservative project vocabulary
or cached classifier rendering only when the current recovery pipeline cannot obtain those facts,
and stale results must not claim exact scope or expected-type compatibility.
A future source-diff map could prove that an enclosing site is unchanged,
but position coincidence alone is not such proof.

Consequently, last-successful memory is a resilience layer after recovering parsing,
not the semantic foundation of completion.
It can also improve temporary hover or highlighting behavior later, under feature-specific stale-range rules.

## Verification and extension criteria

Every recovery point needs a concrete edit sequence showing the context it preserves.
Compare valid strict and recovering parses by reachable structure and spans;
pair malformed inputs with explicit repairs and require strict mode to reject recovery-only acceptance.
Token deletion, replacement, UTF-8-safe prefixes, equal-span holes, abandoned allocations,
lexical failures, and fatal metadata conversion establish the integration boundary.
These checks do not independently prove the generated parser correct.

Resolver tests compare candidates with actual lookup under shadowing, mobile bindings, and source boundaries.
Checker probes must leave solutions and scope constraints unchanged and produce the same evidence
in any candidate order.
Session and Cajun checks exercise dependency edits, Unicode replacement ranges,
capability negotiation, and cancellation during incomplete source edits.
Generated snippets must parse after substituting representative placeholder terms. Current test entry points are indexed
by [C16](../references/compiler.md#c16-validation-debugging-and-extending-the-implementation).

## Remaining uncertainty

New edit sequences may justify additional list or arm recovery points. Start with a lost-context regression
and preserve the [existing recovery contract](../references/compiler.md#recovering-parsing).

Expected-token diagnostics are also lower-level than useful syntax forms.
The parser may initially expose only term, pattern, delimiter, and fixed-keyword expectations,
then introduce richer `SyntaxForm` identities as snippet support needs them.
The invariant is that raw LALRPOP diagnostic strings never cross into Cajun's completion logic.

Finally, definitive type compatibility may be more expensive than rendering candidate annotations.
Measurements on standard-library-sized scopes should decide whether compatibility is computed eagerly,
cached per expected classifier, or deferred until the client asks for more results.
The semantic result and filtering rules do not depend on that performance choice.
