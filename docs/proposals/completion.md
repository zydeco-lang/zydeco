# Semantic completion

Zydeco completion should answer a source-language query rather than expose an editor-owned vocabulary.
The parser knows which forms can occur at a position, name resolution knows which definitions are visible,
the type checker knows what classifiers those definitions have, and the source session knows which files can be
imported. The compiler session combines and ranks those facts; Cajun translates the results into Language Server
Protocol items.

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
    compatibility: AnnotationCompatibility,
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
`=>`, or `end` at the marker. A marker-only issue is then removed from ordinary syntax diagnostics.
If the same recovery discards source tokens too, its diagnostic is retained at the first discarded source token.
The synthetic marker never appears among the discarded source tokens reported to callers.

LALRPOP's `expected` lists contain diagnostic terminal names. They may be useful evidence while prototyping, but
Cajun must not parse those strings. The surface parser converts them into typed `TokenKind` values before exposing
them. These payload-free kinds are derived from the lexer's `Tok` declaration, so an expected identifier needs no
invented source text and no separately maintained terminal inventory.

`TokenMetadata` reads fixed spellings directly from Logos's `#[token(...)]` attributes. A token with several aliases
selects one registered spelling explicitly: both `def` and `define` lex as `Define`, whose canonical spelling remains
`define`. Variable lexical categories have no fixed source spelling. Their grammar names default to the variant name,
with explicit overrides for existing labels such as `LowerId`. Trivia and malformed lexical tokens are marked as
excluded from parser expectations; the synthetic `Completion` and `Invalid` terminals stay private to the parser.

The same generated metadata supplies fixed-token formatting and the parser's terminal-name conversion.
The LALRPOP external-token table remains explicit grammar integration: conformance tests compare both terminal names
and their mapped lexer variants. This preserves the grammar as the syntax specification while avoiding an additional
runtime catalog. Unknown settings, ambiguous canonical spellings, and conflicting terminal names fail macro expansion.

### Strict and recovering modes

The parser should expose two explicit outcomes over the same grammar:

- **Strict parsing** is used by compilation, formatting, and normal source loading. Any recovery issue makes the
  source invalid, preserving the current language acceptance boundary.
- **Recovering parsing** returns the partial textual arena, its root when one was recovered, and an ordered collection
  of typed recovery issues. Editor queries may continue through recovered holes.

The generated LALRPOP parser should sit behind this surface API. Callers should not be able to accidentally ignore
recovery issues and treat a recovered source as a valid program.

### Trust boundary and recovery contracts

The implementation keeps `parser/grammar.lalrpop` as the single syntax specification and its generated parser as the
reference implementation. Both modes run that parser; strict mode accepts only a returned root with no issues.
Recovery policy lives in the two `!` productions and LALRPOP's runtime. There is no handwritten parser,
synchronization algorithm, repair-search engine, or separate proof system to maintain.

The surrounding Rust code establishes smaller integration contracts. A grammar semantic value carries either
ordinary syntax or an opaque recovery handle. The enclosing allocation rule records the exact `PatId` or `TermId`
against that handle. An authored hole at the same span, a previous parse's allocation, and a second zero-width hole
therefore cannot be mistaken for the same recovery event. An abandoned semantic value can have no allocated hole;
an allocated node can also outlive its parser stack entry. Completion exposes an allocated hole only when it remains
reachable from the returned root. Ordinary recovery issues can retain links to abandoned allocations for diagnostics.

`RecoveringParser::new(source)` borrows a source snapshot, and `RecoveringParser::at(source, offset)` additionally
validates and binds a completion cursor. Parsing then takes only `&mut Parser`, never a second source argument.
The cursor's bounds, UTF-8 boundary, and replacement range consequently belong to the actual input being parsed.
One lexical stream supplies comment and quoted-literal boundaries to the parser and Cajun, including the EOF cursor
of an unfinished token. Byte ranges are retained as byte ranges; layout lookup uses character boundaries.

Lexical failures become a grammar-known `Invalid` terminal with a typed `LexicalError` payload. Like `Completion`,
it has no successful production and is excluded from public expectations. This lets LALRPOP recover using its
existing points instead of treating a lexer error as an early return or an apparent EOF. Diagnostics retain typed
invalid tokens even when they are discarded alongside the completion marker.

Numeric conversions use LALRPOP's fallible actions. Metadata integers remain signed 64-bit values, while ordinary
integer literals remain arbitrary precision. A failed conversion returns a source-located `LiteralError`, keeps any
earlier recovery issues, and returns no syntax root. Fallible-action errors are fatal in LALRPOP; they do not run `!`.
Supporting a recoverable invalid metadata value would require an explicit representation and grammar decision,
so the current implementation reports the error rather than inventing a successful value.

These contracts are regression-tested against the reference parser. They do not claim formally verified parsing,
minimal edits, or maximal context retention. LALRPOP may pop consumed stack entries as well as discard unread tokens;
its `dropped_tokens` list describes the latter, not every source fragment replaced during recovery.

### Recovery-point policy

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

The implemented resolver captures a `ScopeSnapshot` when it visits the exact completion hole. Its ordinary `Local`
environment is a persistent map, while the snapshot contains only the visible definitions needed by this request.
Normal compilation does not retain environments at every syntax node. Pattern traversal determines when each
binder enters that environment, including the sequential visibility of dependent parameter annotations.

Name lookup and enumeration share one `NameScope` view over `Local` and `Global`. Enumeration looks up each distinct
name through that view, so only the definition selected by reference resolution is offered.
Each binder group records its introduction depth, allowing completion to prefer nearer bindings without using
source offsets as a proxy for lexical scope. All names contributed by one block share an introduction depth.

Scope snapshots contain identities, not types or rendered signatures. The checker remains the canonical source of
each definition's annotation.

### Current-source query and failure boundaries

`CompilerSession::complete(root, byte_offset)` uses the same effective source text as `source_text(root)`.
It recovers the root, assembles imports through the ordinary source provider, and remaps the root's completion
`TermId` into the merged arena while copying syntax. Desugaring's textual origins then connect that exact identity
to the resolver visit. Spans never substitute for node identity; an old parse's equal-position hole cannot match.
Imported files and companion signatures retain strict parsing, their own source identities, and fresh name environments.

The cursor identifies a whole word even when its current spelling is a keyword, such as `val` while typing `value`.
An authored `_` is also replaced as a whole token and contributes no name prefix. A term-hole recovery alone does
not prove that the original position admitted terms: recovery may have popped a restricted name position such as
`record/field`. Ordinary-name completion also requires the parser's original typed expectations to admit `Hole`.
Pattern holes, fields, constructors, destructors, and metadata arguments therefore do not receive general names.

Completion-oriented resolution records unbound-reference errors and substitutes semantic holes for those references,
allowing traversal to reach the cursor. Strict resolution remains fail-fast. A structural resolution error can still
stop traversal; a scope already captured remains usable without types, while an unvisited cursor yields no result.
Unrecoverable parsing, invalid source directives, failed dependency loading, and desugaring errors likewise provide
no invented scope. No last-successful source or position-based fallback is used.

The recovered program has its own type-checking identity. Available definition annotations come from that program's
retained statics arena, including partial facts after type errors. Unsatisfied annotations remain optional information,
and unresolved top classifiers are omitted from the normalized editor-fact index without changing checker diagnostics.
The completion query retains only its most recent result, and neither installs a repaired overlay nor replaces a strict
analysis. Cajun runs it on a disposable session snapshot and checks the document revision before returning edits.

## Type-directed candidates

Types contribute in three distinct ways: presentation, ordering, and filtering. Keeping these roles separate avoids
turning a failed or incomplete inference into missing completion results.

### Showing types beside names

When checking reaches a definition, its classifier is retained as an `AnnId` in `annotations_var`.
Cajun's hover path renders these annotations with the statics formatter. Completion reuses that formatter to show a
compact classifier beside the label when the annotation is available:

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

The completion token becomes a distinguished hole in recovered syntax. The useful fact is the incoming checking
judgment at that exact node, after the checker has prepared its annotation for the node's lexical environment.
An unsolved incoming annotation still preserves its known classifier category when checking falls back to synthesis.
Neither case uses the stand-in type later synthesized for the hole: treating that stand-in as an expectation would
manufacture a constraint at a synthesis site and could rank whichever candidate happened to solve it first.

The checker therefore retains the distinct analytic annotations it observes at the completion node:

```rust
struct CompletionTyping {
    expectations: Vec<AnnId>,
    compatibility: ArenaAssoc<DefId, AnnotationCompatibility>,
}
```

`AnnId` is already the canonical description of a checking constraint: `Set`, `Kind(KindId)`, or `Type(TypeId)`.
Introducing another enum for kind, type, value, and computation expectations would duplicate this information. The
kind of a `TypeId` supplies the CBPV value/computation distinction when the checker needs it.

Most source nodes are checked once, but a resolved term can be revisited during recursive checking. Retaining every
distinct analytic annotation makes completion satisfy all judgments that insertion at that shared node would face,
rather than depending on which visit happened last. A synthesis visit contributes no expected annotation. An empty
collection consequently means only that no usable analytic constraint was observed; it may reflect synthesis or an
earlier failure, and both cases must leave candidates unfiltered.

This fact does not retain every checker `TyEnv`; those environments are intentionally stripped after checking because
of their size. Visible `DefId`s come from the resolver scope, candidate annotations come from the ordinary
`annotations_var` table, and the incoming annotation has already been substituted and normalized by the term-checking
entry point. An expectation of `Set` is captured before the hole reports that it cannot synthesize a kind, so that
otherwise valid kind names are not lost with the rejected stand-in node.

### Compatibility and ranking

Expected-type ranking predicts the literal edit represented by a candidate. An ordinary definition candidate inserts
a bare variable reference, so its relevant judgment is the same one used by the variable rule:

```text
candidate annotation  Lub  expected annotation
```

Zydeco currently has no separate subtyping or implicit-coercion relation for this judgment. A successful rigid `Lub`
is definitional equality as implemented by the checker, including normalization and alpha-equivalence. Splitting that
one relation into `Exact` and `Compatible` would either make arena-ID equality semantically significant or duplicate
the checker's rules. Completion instead records three evidence levels:

```rust
enum AnnotationCompatibility {
    Equal,
    Unknown,
    Mismatch,
}
```

`Equal` means every observed analytic annotation reconciles without inference. `Unknown` means there is no expected
annotation, the definition has no annotation, reconciliation reaches an unsolved metavariable, or the probe encounters a
failure that is not a rigid equality rejection. `Mismatch` means at least one required judgment produces a definite
sort, kind, type, or label mismatch without relying on inference. Such a mismatch remains definitive when another
part of the same classifier contains an unresolved metavariable; the metavariable cannot repair the rigid part.

The probe reuses `Lub`; it does not implement a second structural type comparator. A probe mode follows already-solved
fills but defers an unsolved fill, marks the result unknown, and performs no solution or scope writes. It runs on the
request-local completion checker before checker environments are stripped. Deferring inference makes the evidence
independent of candidate enumeration order. Nodes allocated while exposing definitional equality belong to that
disposable check; source facts, diagnostics, and inference state remain unchanged. This is observationally side-effect
free for both strict project analysis and the semantic facts returned to the editor.

Ordinary name candidates are ordered by:

1. exact prefix matches, then other prefix matches;
2. annotation evidence: equal, then unknown;
3. lexical proximity; and
4. label, for deterministic tie-breaking.

`Mismatch` candidates are omitted. `Unknown` candidates remain visible, including every candidate when checking does
not reach an analytic context. Automatic and manual invocation use the same semantic candidate set; their trigger only
affects when the client asks for it. No candidate is filtered because inference was needed, because a classifier was
not materialized, or because two rendered type strings differ textually.

This judgment deliberately does not search for adapted expressions. A value that would fit after `!`, `ret`, a thunk,
or a type application is a future structured candidate with its own inserted form; the bare name is ranked according
to its own classifier. `Equal` is classifier-fit evidence, not proof that replacing the hole makes the entire program
valid. A surrounding construct may impose a further term-level condition, such as equality with a manifest package
witness, and unrelated source errors can remain. Such conditions require their own typed completion facts before
they can refine ranking.

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

## Implementation roadmap

### Completion-oriented parser recovery — implemented

The surface parser API exposes explicit strict and recovering outcomes. A distinguished completion marker and typed
recovery identities connect the cursor to `!` recovery points at term and pattern atoms.
Strict callers continue to reject every recovered source.

The parser tests demonstrate all of the following:

- `let value = <cursor> in value` produces a term completion hole at the cursor;
- `fn argument => <cursor>` preserves the binder and body scope;
- an incomplete nested construct recovers its surrounding term without swallowing a following complete binding;
- a cursor where `in`, `that`, `=>`, or `end` is required yields typed syntax expectations;
- authored `_` and parser-created recovery holes remain distinguishable;
- strict parsing rejects every input accepted only through recovery; and
- existing valid-source parse trees and spans remain unchanged.

This establishes the parser behavior and public API used by semantic completion. It does not require a
LALRPOP version upgrade because the pinned 0.23 release already provides the needed recovery primitive.

### Scope-backed definition completion — implemented

The resolver captures visible definitions for the distinguished hole and deduplicates shadowed names through ordinary
lookup. The session adds prefix filtering and deterministic ordering by exact match, binder proximity, and spelling.
Cajun supplies whole-token UTF-16 edits, compact type details, negotiated label details, and a plain detail-field fallback.
Names remain available without type information, and metadata dispatch remains separate from ordinary-name completion.

### Type-directed ranking — implemented

The resolver returns the cursor identity and its scope as one optional `CompletionSite`. The checker retains incoming
analytic annotations for that exact node and probes definition annotations through `Lub` without solving inference
variables. The session ranks equal definitions first and filters only rigid mismatches. Existing classifier
presentation remains unchanged, and Cajun projects the compiler's order directly into LSP sort text.

### Source-path completion — implemented

Cajun recognizes quoted arguments through `MetadataValue::Source`, including strings without a closing quote.
`CompilerSession::complete_source_paths` resolves the directory from the importer's canonical parent and merges
fresh disk entries with active overlays. It offers directories and conventional `.zy`, `.zyi`, and `.zydeco` files,
including directories implied by unsaved sources. Results are deduplicated and ordered with directories first,
then by spelling. Direct self-imports and their symlink aliases are excluded; transitive cycles remain the source
loader's responsibility.

Replacement covers the current path component, preserving quotes, the written directory prefix, and any following
components. Completed names use the source language's string escapes. Quotes and separators trigger LSP completion,
and incomplete lists request fresh candidates as the path changes. Comments, unrelated strings, numbered imports,
and cursors in unfinished escape sequences receive no path suggestions.

### Syntax and failed-revision resilience

Project typed parser expectations through the syntax-form catalog and add a separate last-successful Cajun state
with conservative fallback policy. These tasks should be split if parser recovery exposes enough independent
review surface.

### Structural and branch completion

Use checked data, codata, product, and existential structure to offer constructors, destructors, fields, and missing
match or comatch arms. Generated arms reuse syntax-form snippets and the ordinary formatter.

## Verification

Parser integration checks share the formatter's repository corpus and an arena-independent syntax projection.
For valid sources, strict and recovering modes must agree on reachable node tags, edges, rendered values, and entity
spans, with no recovery issues. Rejected sources are paired with valid repairs; representative recovery and completion
fixtures must match the strict parse of the explicit repair, ignoring the positions changed by that repair.

Deterministic tests delete or replace each token in representative programs and parse every UTF-8-safe typing prefix.
All three entry points check rejection consistency, repeatable recovery shapes, valid ranges, typed hole identities,
and root reachability for completion. Dedicated regressions cover equal-span holes, abandoned allocations, lexical
errors before and after valid input, unfinished comments and literals, metadata integer overflow, and Unicode layout
capture. These checks exercise the integration around LALRPOP; acceptance agreement is not an independent proof of
the generated parser's correctness. They run with `cargo test -p zydeco-surface textual::parser --lib`.

Each layer needs tests at the phase that owns its facts:

- textual tests cover UTF-8 and UTF-16 cursor conversion, mid-token replacement, comments, strings, completion-token
  insertion, recovery spans, and strict-versus-recovering outcomes;
- resolver tests cover sequential binders, shadowing, branch-local patterns, mobile block bindings, recursion,
  and source-boundary isolation;
- statics tests cover equality, unknown expectations, rigid mismatches, multiple incoming constraints, and
  comparisons that leave inference state unchanged;
- session tests cover ordering, filtering, relative sources, directories, signatures, overlays, and import-cycle exclusions;
- Cajun unit tests cover LSP kinds, label details, snippets, deterministic sort text, and revision cancellation; and
- stdio tests exercise completion during realistic edit sequences rather than only complete source snapshots.

Scope-backed completion has resolver tests comparing enumeration with actual resolved references, session tests for
scope and dependency isolation, and Cajun tests for whole-token edits, Unicode, metadata dispatch, and optional types.
Stdio tests replace a previously successful document with unbound or syntactically incomplete source and require
current names with both label-detail capabilities. Ranking tests pair inferred candidates with unknown ones, check
rigid rejection against actually inserting the candidate, and cover aliases, sealed identities, polymorphism, CBPV
sorts, current-revision expectation changes, and companion signatures. Checker tests verify prepared annotations,
multiple incoming constraints, unchanged solutions and scope constraints, and ordinary inference after a probe.
These checks run with:

```sh
cargo test -p zydeco-surface scoped::completion --lib
cargo test -p zydeco-statics check::tests --lib
cargo test -p zydeco-session source::query::completion --lib
cargo test -p zydeco-session source::query::paths --lib
cargo test -p cajun completion:: --lib
cargo test -p cajun --test stdio stdio_server_completes
```

The broad regression property is that applying a non-resource completion edit at its advertised range produces the
candidate's canonical spelling at that site. Snippet tests additionally strip placeholders to representative terms
or patterns and parse them with the strict parser.

## Remaining uncertainty

LALRPOP recovery chooses how many tokens to discard based on available recovery points. The parser tests show that
the term and pattern atom points preserve a following complete binding when an inner list item is malformed. New edit
sequences may still justify a list or arm boundary. Each change should start with a retained-context regression and
remain in the grammar; the current design does not add a separate recovery or repair-search implementation.

Expected-token diagnostics are also lower-level than useful syntax forms. The parser may initially expose only term,
pattern, delimiter, and fixed-keyword expectations, then introduce richer `SyntaxForm` identities as snippet support
needs them. The invariant is that raw LALRPOP diagnostic strings never cross into Cajun's completion logic.

Finally, definitive type compatibility may be more expensive than rendering candidate annotations. Measurements on
standard-library-sized scopes should decide whether compatibility is computed eagerly, cached per expected
classifier, or deferred until the client asks for more results. The semantic result and filtering rules do not depend
on that performance choice.
