# Semantic completion and recovery

Completion needs useful scope and classifier facts while the current source is incomplete.
[C15](../references/compiler.md#completion-and-documentation) owns the implemented query flow,
ranking, and frontend boundary.
The [textual component guide](../../lang/surface/src/textual/README.md) locates the parser and token APIs.
This design owns the recovery contract and the conditions for adding candidate families or stale-state fallback.

## Design boundary

Each phase exports facts it already establishes: parser expectations, resolver scope,
checker annotations, and session source paths.
Frontends translate those facts into edits and client-specific presentation.
Labels remain presentation text, never lookup keys.
A failed inference must not hide candidates unless the checker has definite incompatibility evidence.
This keeps completion deterministic and reusable beyond one editor without introducing a second scope or type checker.

## Recovering the current source

Completion is normally requested while a token or surrounding construct is incomplete.
A successful strict parse therefore cannot be a prerequisite.
Reusing an older successful parse alone is insufficient: the edit may introduce a new binder, change shadowing,
move the cursor into another scope, or establish a new expected type.

The grammar uses LALRPOP's special `!` symbol for recovery.
At a parser error, LALRPOP can inject that symbol, execute a recovery action,
discard input until the grammar can continue, and report both the parse error and discarded tokens.
Recovery points must be selected by the grammar author, and lexer failures are not recovered automatically.

That mechanism is suitable for retaining surrounding syntax, but it is not itself a completion model.
Zydeco needs two additional distinctions:

- A **completion hole** deliberately marks the cursor.
  It is not a user error and must remain identifiable through textual syntax, desugaring, resolution, and checking.
- A **recovery node** replaces malformed or missing syntax around the cursor.
  It carries a typed recovery identity and diagnostic range rather than becoming indistinguishable
  from an authored `_` hole.

The normal lexer never emits a completion token.
A tooling iterator wraps the ordinary token stream, removes the active token when appropriate,
and inserts one zero-width completion marker at the cursor.
The marker is deliberately not an authored `_` token and is not accepted as an ordinary grammar terminal.
A term or pattern recovery point turns it into a typed hole when that category is admissible;
otherwise the parser records fixed expectations such as `in`, `=>`, or `end` at the marker.
A marker-only issue is then removed from ordinary syntax diagnostics.
If the same recovery discards source tokens too, its diagnostic is retained at the first discarded source token.
The synthetic marker never appears among the discarded source tokens reported to callers.

LALRPOP's `expected` lists contain diagnostic terminal names.
They may be useful evidence while prototyping, but Cajun must not parse those strings.
The surface parser converts them into typed `TokenKind` values before exposing them.
These payload-free kinds are derived from the lexer's `Tok` declaration,
so an expected identifier needs no invented source text and no separately maintained terminal inventory.

`TokenMetadata` reads fixed spellings directly from Logos's `#[token(...)]` attributes.
A token with several aliases selects one registered spelling explicitly: both `def`
and `define` lex as `Define`, whose canonical spelling remains `define`.
Variable lexical categories have no fixed source spelling.
Their grammar names default to the variant name, with explicit overrides for existing labels such as `LowerId`.
Trivia and malformed lexical tokens are marked as excluded from parser expectations;
the synthetic `Completion` and `Invalid` terminals stay private to the parser.

The same generated metadata supplies fixed-token formatting and the parser's terminal-name conversion.
The LALRPOP external-token table remains explicit grammar integration:
conformance tests compare both terminal names and their mapped lexer variants.
This preserves the grammar as the syntax specification while avoiding an additional runtime catalog.
Unknown settings, ambiguous canonical spellings, and conflicting terminal names fail macro expansion.

### Strict and recovering modes

The parser exposes two explicit outcomes over the same grammar:

- **Strict parsing** is used by compilation, formatting, and normal source loading.
  Any recovery issue makes the source invalid, preserving the current language acceptance boundary.
- **Recovering parsing** returns the partial textual arena, its root when one was recovered,
  and an ordered collection of typed recovery issues.
  Editor queries may continue through recovered holes.

The generated LALRPOP parser sits behind this surface API so strict callers cannot accept a recovered source
by ignoring its issues.

### Trust boundary and recovery contracts

The implementation keeps `parser/grammar.lalrpop` as the single syntax specification
and its generated parser as the reference implementation.
Both modes run that parser; strict mode accepts only a returned root with no issues.
Recovery policy lives in the two `!` productions and LALRPOP's runtime.
There is no handwritten parser, synchronization algorithm, repair-search engine, or separate proof system to maintain.

The surrounding Rust code establishes smaller integration contracts.
A grammar semantic value carries either ordinary syntax or an opaque recovery handle.
The enclosing allocation rule records the exact `PatId` or `TermId` against that handle.
An authored hole at the same span, a previous parse's allocation, and a second zero-width hole
therefore cannot be mistaken for the same recovery event.
An abandoned semantic value can have no allocated hole; an allocated node can also outlive its parser stack entry.
Completion exposes an allocated hole only when it remains reachable from the returned root.
Ordinary recovery issues can retain links to abandoned allocations for diagnostics.

`RecoveringParser::new(source)` borrows a source snapshot,
and `RecoveringParser::at(source, offset)` additionally validates and binds a completion cursor.
Parsing then takes only `&mut Parser`, never a second source argument.
The cursor's bounds, UTF-8 boundary, and replacement range consequently belong to the actual input being parsed.
One lexical stream supplies comment and quoted-literal boundaries to the parser and Cajun,
including the EOF cursor of an unfinished token.
Byte ranges are retained as byte ranges; layout lookup uses character boundaries.

Lexical failures become a grammar-known `Invalid` terminal with a typed `LexicalError` payload.
Like `Completion`, it has no successful production and is excluded from public expectations.
This lets LALRPOP recover using its existing points instead of treating a lexer error
as an early return or an apparent EOF.
Diagnostics retain typed invalid tokens even when they are discarded alongside the completion marker.

Numeric conversions use LALRPOP's fallible actions.
Metadata integers remain signed 64-bit values, while ordinary integer literals remain arbitrary precision.
A failed conversion returns a source-located `LiteralError`, keeps any earlier recovery issues,
and returns no syntax root.
Fallible-action errors are fatal in LALRPOP; they do not run `!`.
Supporting a recoverable invalid metadata value would require an explicit representation and grammar decision,
so the current implementation reports the error rather than inventing a successful value.

These contracts are regression-tested against the reference parser.
They do not claim formally verified parsing, minimal edits, or maximal context retention.
LALRPOP may pop consumed stack entries as well as discard unread tokens; its `dropped_tokens` list describes the latter,
not every source fragment replaced during recovery.

### Recovery-point policy

Recovery begins at term and pattern atoms.
Those points already allow a malformed item inside a delimited list to recover
without swallowing a following complete binding, so list and arm boundaries should gain their own points only
when a concrete edit sequence shows that the atomic points are insufficient.
Adding `!` to many productions would make skipped-token behavior difficult to predict.
Tests justify each recovery point by pinning the retained syntax and reported range.

Recovered textual terms lower to ordinary semantic holes while retaining an origin
that identifies the completion or recovery site.
This allows the existing resolver and bidirectional checker to supply scope and expected-type facts
without making recovery nodes executable language constructs.
Strict parsing rejects them before ordinary compilation reaches desugaring.

## Candidate families

Visible definitions, metadata, and source paths are implemented.
Contextual snippets, structural members, and missing arms are extensions that build on the same request:

| Family | Site | Candidate source | Inserted form |
| --- | --- | --- | --- |
| Visible definitions | term and type positions | resolver scope plus definition annotations | source name |
| Contextual forms | parser expectation | typed syntax-form catalog | keyword or snippet |
| Metadata | metadata path | metadata catalog | name, call, or closed identifier |
| Import targets | `MetadataValue::Source` string | session source index and filesystem | escaped relative path |
| Constructors | `+` term or match pattern | expected or scrutinee data type | constructor or arm snippet |
| Destructors | `.` postfix or comatch arm | receiver or expected codata type | destructor or arm snippet |
| Named fields | `/` projection or `#` field position | product or existential structure | field name or pattern |
| Hole values | distinguished `_` or completion hole | visible definitions ranked by expectation | source name |

Contextual forms include `let`, `do`, `fn`, `match`, `comatch`, `data`, `codata`,
`forall`, `exists`, `pack`, and `begin`.
The grammar decides whether the form is admissible.
A `SyntaxForm` catalog may add an editor-friendly description and placeholder structure
because useful snippet phrasing is presentation knowledge that cannot be derived mechanically from an LR production.
Each snippet must have a representative parsing test so its syntax cannot drift from the grammar.

Constructor, destructor, field, and missing-arm completion are type-checker queries rather than global name lists.
They should follow visible-definition completion because they require ownership and expected-type information
that ordinary lexical scope does not provide.

Source-path completion extends the existing metadata schema: `MetadataValue::Source` identifies the argument
as a source without Cajun testing whether the callee string happens to be `import`.
The session resolves the typed source request relative to the importing file and returns directories,
supported source files, and overlays.
Package-aware targets can later implement the same source-candidate interface.

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

LALRPOP recovery chooses how many tokens to discard based on available recovery points.
The parser tests show that the term and pattern atom points preserve a following complete binding
when an inner list item is malformed.
New edit sequences may still justify a list or arm boundary.
Each change should start with a retained-context regression and remain in the grammar;
the current design does not add a separate recovery or repair-search implementation.

Expected-token diagnostics are also lower-level than useful syntax forms.
The parser may initially expose only term, pattern, delimiter, and fixed-keyword expectations,
then introduce richer `SyntaxForm` identities as snippet support needs them.
The invariant is that raw LALRPOP diagnostic strings never cross into Cajun's completion logic.

Finally, definitive type compatibility may be more expensive than rendering candidate annotations.
Measurements on standard-library-sized scopes should decide whether compatibility is computed eagerly,
cached per expected classifier, or deferred until the client asks for more results.
The semantic result and filtering rules do not depend on that performance choice.
