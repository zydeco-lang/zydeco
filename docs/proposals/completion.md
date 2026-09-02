# Semantic completion

Zydeco completion should answer a source-language query rather than expose an editor-owned vocabulary.
The parser knows which forms can occur at a position, name resolution knows which definitions are visible,
the type checker knows what classifiers those definitions have, and the source session knows which files can be
imported. Cajun should combine those facts, rank them, and translate them into Language Server Protocol items.

Metadata completion is the first instance of this model. The surface metadata catalog owns annotation names,
argument shapes, descriptions, and closed identifier domains; Cajun owns cursor recovery, replacement ranges,
snippets, and LSP conversion. General completion extends the same ownership rule to syntax, lexical scope,
types, and source paths.

This proposal answers the following review questions:

- Which compiler phase owns each kind of completion fact?
- How can completion use the current document while its syntax is incomplete?
- How are visible names recovered without rebuilding scope in Cajun?
- When may types order or remove candidates, and how are those types presented?
- What information may Cajun retain after current-revision analysis fails?
- Which implementation slice should follow metadata completion?

## Goals and boundaries

Completion should:

- remain available during ordinary incomplete edits;
- offer only names visible at the current lexical site when current scope is known;
- derive language spellings and semantic facts from compiler-owned representations;
- show a candidate's kind or type beside its name when the client supports that presentation;
- use an expected kind or type to rank candidates and to remove only candidates proven incompatible;
- support contextual syntax forms, source paths, members, and branch generation incrementally;
- return deterministic results independently of hash-map iteration and analysis scheduling; and
- keep protocol types and client capabilities out of the compiler crates.

Completion does not initially attempt arbitrary proof search or program synthesis. Filling a hole with a complete
expression may later build on the same expected-type query, but the first semantic candidates are existing visible
definitions and grammar-defined forms. Package-registry search is also outside the initial source-path completion;
the session first completes files and overlays it can already identify locally.

## Ownership of completion facts

The recurring rule is that each phase exports the facts it already establishes. No downstream consumer recreates
those facts by scanning a later arena.

| Fact | Canonical owner | Examples |
| --- | --- | --- |
| Cursor token and replacement range | textual tooling | prefix, lexical class, string/comment exclusion |
| Syntactically admissible forms | parser | term, pattern, `in`, `that`, `end`, arm position |
| Metadata schema | surface metadata catalog | `format`, `builtin`, nested options, intrinsic roles |
| Visible definitions | resolver | local binders, block bindings, shadowing, source boundaries |
| Candidate classifier | type checker | kind, value type, computation type |
| Expected classifier | type checker at the completion hole | expected kind or type and CBPV sort |
| Structural members | type checker | constructors, destructors, named fields, package projections |
| Importable sources | compiler session | relative paths, overlays, source kinds |
| Presentation and client capabilities | Cajun | LSP kind, label details, snippet syntax, sort text |

This separation permits non-LSP frontends to reuse completion. It also prevents Cajun from accumulating lists that
must be updated whenever the grammar, resolver, metadata decoder, or checker changes.

The compiler-facing result should use semantic identities rather than rendered labels as keys. A representative
shape is:

```rust
enum CompletionCandidateId {
    Definition(DefId),
    Syntax(SyntaxForm),
    Metadata(MetadataCandidate),
    Source(SourcePath),
    Member(MemberCandidate),
}

struct CompletionCandidate {
    id: CompletionCandidateId,
    label: String,
    class: CompletionClass,
    annotation: Option<CompletionAnnotation>,
    compatibility: TypeCompatibility,
}
```

The exact types may differ, but identity, semantic class, optional annotation, and compatibility remain separate.
The label is presentation data and is never parsed to recover the candidate's meaning.

## Completion request flow

One request proceeds from the current document rather than from an editor cache assembled independently:

1. Textual tooling converts the UTF-16 cursor to a byte offset, finds the complete token replacement range,
   and rejects positions inside comments or unrelated string contents.
2. Metadata context takes precedence inside `@[...]` or `@(...)` and delegates to the metadata catalog.
3. Otherwise a completion token stream marks the cursor and runs the recovering textual parser.
4. The recovered syntax identifies the grammatical site and a source entity representing the completion hole.
5. Resolution associates that entity with the definitions visible at the site.
6. Checking associates it with an expected classifier when the surrounding program provides one, and obtains
   the classifier of each visible definition from the ordinary definition-annotation table.
7. Syntax, semantic, metadata, and source providers contribute typed candidates applicable to the site.
8. A protocol-independent ranker filters proven mismatches, orders the remainder, and returns semantic results.
9. Cajun renders labels, types, documentation, snippets, and edits according to client capabilities.

Every result is tied to the document revision used by the query. If the overlay changes before the query completes,
Cajun discards the result just as it discards superseded analysis.

## Recovering the current source

Completion is normally requested while a token or surrounding construct is incomplete. A successful strict parse
therefore cannot be a prerequisite. Reusing an older successful parse alone is insufficient: the edit may introduce
a new binder, change shadowing, move the cursor into another scope, or establish a new expected type.

The workspace already uses LALRPOP 0.23. Its
[error-recovery mechanism](https://lalrpop.github.io/lalrpop/tutorial/008_error_recovery.html) provides a special
`!` grammar symbol. At a parser error, LALRPOP can inject that symbol, execute a recovery action, discard input until
the grammar can continue, and report both the parse error and discarded tokens. Recovery points must be selected by
the grammar author, and lexer failures are not recovered automatically.

That mechanism is suitable for retaining surrounding syntax, but it is not itself a completion model. Zydeco needs
two additional distinctions:

- A **completion hole** deliberately marks the cursor. It is not a user error and must remain identifiable through
  textual syntax, desugaring, resolution, and checking.
- A **recovery node** replaces malformed or missing syntax around the cursor. It carries a typed recovery identity
  and diagnostic range rather than becoming indistinguishable from an authored `_` hole.

The normal lexer never emits a completion token. A tooling iterator wraps the ordinary token stream, removes the
active token when appropriate, and inserts one zero-width completion marker at the cursor. The marker is deliberately
not an authored `_` token and is not accepted as an ordinary grammar terminal. A term or pattern recovery point turns
it into a typed hole when that category is admissible; otherwise the parser records fixed expectations such as `in`,
`=>`, or `end` at the marker. The deliberate marker issue is then removed from ordinary syntax diagnostics.

LALRPOP's `expected` lists contain diagnostic terminal names. They may be useful evidence while prototyping, but
Cajun must not parse those strings. The surface parser converts them into a typed `SyntaxExpectation` before exposing
them. A token or syntax-form catalog owns spellings and descriptions used by completion. The LALRPOP external-token
table remains an integration boundary and should have conformance tests rather than becoming a third runtime catalog.

### Strict and recovering modes

The parser should expose two explicit outcomes over the same grammar:

- **Strict parsing** is used by compilation, formatting, and normal source loading. Any recovery issue makes the
  source invalid, preserving the current language acceptance boundary.
- **Recovering parsing** returns the partial textual arena, its root when one was recovered, and an ordered collection
  of typed recovery issues. Editor queries may continue through recovered holes.

The generated LALRPOP parser should sit behind this surface API. Callers should not be able to accidentally ignore
recovery issues and treat a recovered source as a valid program.

Recovery begins at term and pattern atoms. Those points already allow a malformed item inside a delimited list to
recover without swallowing a following complete binding, so list and arm boundaries should gain their own points
only when a concrete edit sequence shows that the atomic points are insufficient. Adding `!` to many productions
would make skipped-token behavior difficult to predict. Tests justify each recovery point by pinning the retained
syntax and reported range.

Recovered textual terms lower to ordinary semantic holes while retaining an origin that identifies the completion
or recovery site. This allows the existing resolver and bidirectional checker to supply scope and expected-type facts
without making recovery nodes executable language constructs. Strict parsing rejects them before ordinary
compilation reaches desugaring.

## Exact lexical scope

`Resolver` already carries the authoritative `Local` and `Global` environments while visiting each pattern and term.
The completed `ScopedArena` currently retains definitions and resolved uses but not the environment at an arbitrary
source position. Reconstructing visibility from all definitions would mishandle binding order, shadowing, mobile
block bindings, branch-local patterns, and the fresh environments at source boundaries.

Resolution should therefore record a compact scope identity at textual sites relevant to tooling. A scope contains
the visible mapping from `VarName` to `DefId`; its persistent representation may share structure between neighboring
sites. The resolver records the environment on entry to a term and before or after a pattern according to the
pattern's binding semantics. A completion hole then points directly to the correct scope.

The visible-name query applies lexical shadowing before returning candidates. Two definitions with the same source
name do not become duplicate options: only the definition selected by name resolution at that site is visible.
Presentation may still rank definitions introduced in the nearest scope above equally compatible outer definitions.

Scope snapshots contain identities, not types or rendered signatures. The checker remains the canonical source of
each definition's annotation.

## Type-directed candidates

Types contribute in three distinct ways: presentation, ordering, and filtering. Keeping these roles separate avoids
turning a failed or incomplete inference into missing completion results.

### Showing types beside names

For every `DefId` candidate, the checker already retains an `AnnId` in `annotations_var`. Cajun's hover path already
renders these annotations with the statics formatter. Completion should reuse the same rendering policy and show a
compact classifier beside the label:

```text
map          : forall (A : VType) (B : VType) . (A -> B) -> List A -> List B
read_text    : Path -> OS
Path         : VType
```

The classifier is presentation metadata, not part of the inserted text or filter text. Cajun uses LSP label details
when the client supports them and falls back to the completion item's detail field. Longer documentation and expanded
type definitions may be deferred to `completionItem/resolve`; the compact classifier needed for ordering should be
available in the initial result.

### Obtaining the expected classifier

The completion token becomes a distinguished hole in recovered syntax. Bidirectional checking already encounters
holes under synthesis or analysis and represents unresolved hole information with typed identities. Completion adds
one compact editor-facing fact for the distinguished hole:

```rust
enum CompletionExpectation {
    Set,
    Type { kind: KindId },
    Value { ty: TypeId },
    Computation { ty: TypeId },
    Unknown,
}
```

This fact records the expected sort and classifier after checking the recovered source as far as possible. It does
not retain every checker `TyEnv`; those environments are intentionally stripped after checking because of their size.
Visible `DefId`s come from the resolver scope, candidate annotations come from retained statics indexes, and only the
completion site's expected classifier needs new retained storage.

When current checking cannot reach the hole or its expectation still contains unresolved inference variables, the
expectation is `Unknown`. Unknown information never removes a candidate.

### Compatibility and ranking

Compatibility is a typed result, not a Boolean inferred from rendered types:

```rust
enum TypeCompatibility {
    Exact,
    Compatible,
    Unknown,
    Incompatible,
}
```

`Exact` means the candidate and expectation have the same normalized classifier. `Compatible` means the checker can
reconcile them without committing inference changes to the shared analysis. `Unknown` covers absent expectations,
unresolved holes, or a comparison the checker cannot decide safely. `Incompatible` requires a definitive sort or
classifier mismatch.

The compatibility query must be side-effect free. It may compare completed normalized annotations or run against a
disposable checker snapshot; it must not fill metavariables in the cached project merely because an editor requested
completion.

Candidates are ordered by:

1. prefix match quality;
2. type compatibility: exact, compatible, unknown, then incompatible;
3. lexical proximity;
4. semantic class appropriate to the syntax site; and
5. deterministic source order and label.

Once both sides are complete and incompatibility is proven, completion omits the candidate. Automatic and manual
invocation use the same semantic candidate set; their trigger only affects when the client asks for it. No candidate
is filtered because inference failed, because a type was not materialized, or because its rendered type string
differs textually.

The initial semantic milestone may implement presentation and the `Unknown` rank for every candidate before adding
the compatibility query. This keeps the name-completion API stable while type-directed ordering arrives
incrementally.

## Candidate families

The following families build on the same request without requiring one monolithic implementation:

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

Contextual forms include `let`, `do`, `fn`, `match`, `comatch`, `data`, `codata`, `forall`, `exists`, `pack`,
and `begin`. The grammar decides whether the form is admissible. A `SyntaxForm` catalog may add an editor-friendly
description and placeholder structure because useful snippet phrasing is presentation knowledge that cannot be
derived mechanically from an LR production. Each snippet must have a representative parsing test so its syntax
cannot drift from the grammar.

Constructor, destructor, field, and missing-arm completion are type-checker queries rather than global name lists.
They should follow visible-definition completion because they require ownership and expected-type information that
ordinary lexical scope does not provide.

Source-path completion extends the existing metadata schema: `MetadataValue::Source` identifies the argument as a
source without Cajun testing whether the callee string happens to be `import`. The session resolves the typed source
request relative to the importing file and returns directories, supported source files, and overlays. Package-aware
targets can later implement the same source-candidate interface.

## Cajun state after a failed revision

Cajun currently removes a cached `ProjectState` when analysis of the corresponding document revision fails. Merely
retaining that value in place would be unsafe: definition, rename, semantic-token, and scope ranges would refer to old
text, and completion could suggest a shadowed definition after the edit introduced a new binder.

If last-successful memory is added, it should be represented separately from current-revision state:

```text
open document revision
  current analysis: successful | failed | pending
  last successful analysis: optional, with its own revision and source
```

Feature policy is explicit. Source edits and semantic ranges require current-revision facts. A last-successful
project may provide a conservative project vocabulary or cached classifier rendering only when the current recovery
pipeline cannot obtain those facts, and stale results must not claim exact scope or expected-type compatibility.
A future source-diff map could prove that an enclosing site is unchanged, but position coincidence alone is not such
proof.

Consequently, last-successful memory is a resilience layer after recovering parsing, not the semantic foundation of
completion. It can also improve temporary hover or highlighting behavior later, under feature-specific stale-range
rules.

## Phased implementation

### Phase 0: completion-oriented parser recovery

Add a surface parser API with explicit strict and recovering outcomes. Insert a distinguished completion marker,
introduce typed recovery identities, and add the smallest justified `!` recovery points for term and pattern atoms.
Strict callers continue to reject every recovered source.

This phase is successful when tests demonstrate all of the following:

- `let value = <cursor> in value` produces a term completion hole at the cursor;
- `fn argument => <cursor>` preserves the binder and body scope;
- an incomplete nested construct recovers its surrounding term without swallowing a following complete binding;
- a cursor where `in`, `that`, `=>`, or `end` is required yields typed syntax expectations;
- authored `_` and parser-created recovery holes remain distinguishable;
- strict parsing rejects every input accepted only through recovery; and
- existing valid-source parse trees and spans remain unchanged.

This phase establishes the parser behavior and public API used by the later semantic phases. It does not require a
LALRPOP version upgrade because the pinned 0.23 release already provides the needed recovery primitive.

### Phase 1: scope-backed definition completion

Record resolver scope identities at completion-relevant sites. Query visible definitions for the distinguished hole,
deduplicate shadowed names, and expose source annotations. Cajun adds general completion items with exact replacement
ranges and compact type details.

### Phase 2: syntax, paths, and failed-revision resilience

Project typed parser expectations through the syntax-form catalog, implement `MetadataValue::Source` path candidates,
and add a separate last-successful Cajun state with conservative fallback policy. This phase should be split if parser
recovery exposes enough independent review surface.

### Phase 3: type-directed ranking

Retain the completion expectation, implement a side-effect-free compatibility query, rank exact and compatible
definitions first, and filter only proven mismatches. Add client-capability tests for label details and detail-field
fallback.

### Phase 4: structural and branch completion

Use checked data, codata, product, and existential structure to offer constructors, destructors, fields, and missing
match or comatch arms. Generated arms reuse syntax-form snippets and the ordinary formatter.

## Verification

Each layer needs tests at the phase that owns its facts:

- textual tests cover UTF-8 and UTF-16 cursor conversion, mid-token replacement, comments, strings, completion-token
  insertion, recovery spans, and strict-versus-recovering outcomes;
- resolver tests cover sequential binders, shadowing, branch-local patterns, mobile block bindings, recursion,
  and source-boundary isolation;
- statics tests cover classifier display, exact and compatible ordering, unknown expectations, definitive filtering,
  and comparisons that leave inference state unchanged;
- session tests cover relative sources, directories, signatures, overlays, and import-cycle exclusions;
- Cajun unit tests cover LSP kinds, label details, snippets, deterministic sort text, and revision cancellation; and
- stdio tests exercise completion during realistic edit sequences rather than only complete source snapshots.

The broad regression property is that applying a non-resource completion edit at its advertised range produces the
candidate's canonical spelling at that site. Snippet tests additionally strip placeholders to representative terms
or patterns and parse them with the strict parser.

## Remaining uncertainty

LALRPOP recovery chooses how many tokens to discard based on available recovery points. The Phase 0 tests show that
the term and pattern atom points preserve a following complete binding when an inner list item is malformed. New edit
sequences may still justify a list or arm boundary. If recovery cannot remain predictable, the fallback is not an
editor-side parser: the surface crate should instead implement a completion probe that replaces a bounded source
region with a typed hole and strictly parses the repaired source. Both approaches preserve the same downstream scope
and type APIs described here.

Expected-token diagnostics are also lower-level than useful syntax forms. The parser may initially expose only term,
pattern, delimiter, and fixed-keyword expectations, then introduce richer `SyntaxForm` identities as snippet support
needs them. The invariant is that raw LALRPOP diagnostic strings never cross into Cajun's completion logic.

Finally, definitive type compatibility may be more expensive than rendering candidate annotations. Measurements on
standard-library-sized scopes should decide whether compatibility is computed eagerly, cached per expected
classifier, or deferred until the client asks for more results. The semantic result and filtering rules do not depend
on that performance choice.
