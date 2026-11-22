# C4. Parsing, Desugaring, and Name Resolution

[Textual syntax](../../../../lang/surface/src/textual/README.md) owns Logos tokens,
literal decoding, LALRPOP parsing, metadata syntax, spans,
and contiguous `--|` [`TextBlock`](../../../../lang/surface/src/textual/text_block.rs) capture attached
from the token stream in `O(N log N)`.
Strict parsing supplies compilation input; recovering parsing supplies structured holes
and recovery evidence for tooling.
A recovered edit is not silently accepted as an executable program.
Formatting trivia and retained layout intentions live separately in [`zydeco-fmt`](../../../../lang/fmt/src/lib.rs)
so compilation never captures formatter tables.

## Recovering Parsing

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
Any token with multiple spellings selects one registered canonical spelling explicitly
via `#[token_metadata(canonical = "...")]`.
Variable lexical categories have no fixed source spelling.
Their grammar names default to the variant name, with explicit overrides for existing labels such as `LowerId`.
Trivia and malformed lexical tokens are marked as excluded from parser expectations;
the synthetic `Completion` and `Invalid` terminals stay private to the parser.

The same generated metadata supplies fixed-token formatting and the parser's terminal-name conversion.
The LALRPOP external-token table remains explicit grammar integration:
conformance tests compare both terminal names and their mapped lexer variants.
This preserves the grammar as the syntax specification while avoiding an additional runtime catalog.
Unknown settings, ambiguous canonical spellings, and conflicting terminal names fail macro expansion.

### Strict and Recovering Modes

The parser exposes two explicit outcomes over the same grammar:

- **Strict parsing** is used by compilation, formatting, and normal source loading.
  Any recovery issue makes the source invalid, preserving the current language acceptance boundary.
- **Recovering parsing** returns the partial textual arena, its root when one was recovered,
  and a collection of typed recovery issues.
  Editor queries may continue through recovered holes.

The generated LALRPOP parser sits behind this surface API so strict callers cannot accept a recovered source
by ignoring its issues.

### Trust Boundary and Recovery Contracts

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
Recoverable invalid metadata values remain unsupported; conversion failure returns diagnostics without a syntax root.

These contracts are regression-tested against the reference parser.
They do not claim formally verified parsing, minimal edits, or maximal context retention.
LALRPOP may pop consumed stack entries as well as discard unread tokens; its `dropped_tokens` list describes the latter,
not every source fragment replaced during recovery.

The grammar currently recovers at term and pattern atoms.
Malformed lists can retain later complete bindings through those points;
additional list or arm productions need a concrete lost-context regression.
A second handwritten repair parser would create a competing syntax contract.
Recovered nodes remain semantic holes with recorded origins,
while strict parsing rejects the recovery before elaboration.
The [recovery tests](../../../../lang/surface/src/textual/parser/tests/recovery.rs) pair retained contexts
with strict rejection, including authored holes, Unicode cursors, invalid literals, and abandoned recovery events.
Further grammar and candidate extensions remain an
[open idea](../../../ideas/README.md#structural-completion-and-last-successful-state).

## Fused Lowering and Resolution

Surface lowering and lexical resolution execute in one fused recursive pass
in [`ResolveFolder`](../../../../lang/surface/src/scoped/resolver.rs) using
[`telescopes`](../../../../lang/surface/src/scoped/resolver/telescopes.rs),
[`bindings`](../../../../lang/surface/src/scoped/resolver/bindings.rs),
and [`meta`](../../../../lang/surface/src/scoped/resolver/meta.rs), guarded
by [`zydeco_foundation::stack::maybe_grow`](../../../../lang/foundation/src/stack.rs) and building
into [`ScopedBuilder`](../../../../lang/surface/src/scoped/alloc.rs) without an intermediate lowered tree.
It expands binding headers and telescopes, makes nominal sealing and CBPV introductions explicit,
keeps consecutive parameters of `fn`, `pi`, and `forall` and juxtaposition arguments
as single nodes, replaces bound names with `DefId`s, and retains explicit boundaries such
as classifier queries, `@[monadic]` blocks, and `@[cbv]` blocks.
Source loading supplies per-unit file slices and resolved literal attachments;
[`ResolveFolder::run_unit`](../../../../lang/surface/src/scoped/resolver.rs) lowers one closed unit
at a time and emits `SourceBoundary` placeholders for imports and the default prelude,
which assembly patches once all units are resolved.
Every generated scoped node records its textual origin in `TextualOrigins`.

A rejected unit collects independent diagnostics across children and publishes no valid scoped output;
reusing an already-failed child does not repeat diagnostics.
The [telescope lowering rules](../../../../lang/surface/src/scoped/resolver/telescopes.rs) handle parameter sequences,
flatten consecutive existential layers, keep `pi` telescopes as single ordered binder groups
(while `@[cbv]` blocks translate each group as one function), and turn `val pi`
and `sigma` telescopes into one layer per binder.
The [binding lowering rules](../../../../lang/surface/src/scoped/resolver/bindings.rs) handle binding flavors together
with classifiers across the same parameter fold, distinguishing absence, whole-binding ascriptions,
and classifiers extended across parameters (freshening classifier binders only in the last case).
Destructor parameters discard propagated abstraction classifiers, replace binding classifiers with holes,
or reject value abstractions per the enclosing rule.

[`MetaRules`](../../../../lang/surface/src/scoped/resolver/meta.rs) decodes textual metadata
in one pass into typed `MetaAction` directives before visiting payloads
when the rule inspects authored syntax — lowering surviving term annotations directly
into span-free [`ResolvedMeta`](../../../../lang/surface/src/scoped/syntax.rs) values:
strip `@[doc]`/`@[format]`, handle prelude opt-out/override, record partial binders,
construct typed intrinsics, build `@[typeof]`, build `@[monadic]` or `@[cbv]` blocks,
validate `@[builtin]` roles and `@[ffi]` signatures, or retain opaque/debug metadata.
Intrinsic construction skips its hole payload, while `@[typeof]`, `@[monadic]`, and `@[cbv]` lower their payloads once.

`PreludeMeta` validates `none` or a `from` identifier without rebuilding intermediate ASTs:
`from` inserts the projection group and lowers its payload once,
while `none` suppresses `PreludeScope` fallback in its payload.
The [default source prefix](sessions-and-queries.md#source-loading-and-package-selection) provides default names
when opted in.
Override-generated definitions and projections retain the metadata annotation origin
(with bindees keeping the identifier origin),
whereas default-generated definitions retain synthetic origins (`insert_synthetic`)
so they never become editable declaration tokens.
Hover and completion identify prelude origins; rename refuses generated definitions
while renaming a supplied view includes its `from` reference, and go-to-definition resolves overrides
to the annotation while omitting fabricated default locations.
Contextual `OS` retains its authored term origin without inventing a `DefId`,
and the formatter prints authored annotations rather than inserted default prefixes.
[Resolver tests](../../../../lang/surface/src/scoped/resolver/tests.rs),
[composition regressions](../../../../lang/tests/tests/suite/prelude.rs), and CLI, REPL,
and editor tests cover opt-out and replacement, witness sharing, source independence,
and rejected downstream publication.

## Name Resolution

[`ResolveFolder`](../../../../lang/surface/src/scoped/resolver.rs) replaces names
with `DefId`s while preserving source identities.
A scoped builder materializes resolved nodes and issues new identities only for context elaboration.
The folder owns the diagnostic collector, required dependency analyzer, and a statically composed observer product.
Its standard profile always includes reference indexing; `with_observer` adds passive consumers.
The [scope module](../../../../lang/surface/src/scoped/scope.rs) supplies the same shadowing rules
for lookup and enumeration.

The inherited `ResolveEnv` is explicit.
Pattern resolution returns its identity and extended environment.
Classifiers precede their annotated binders; dependent pattern sequences
and copattern spines thread environments left to right.
Match arms receive independent environments. Monadic blocks resolve their basis before their body.
An exhaustive constructor classification delegates ordinary reconstruction to the shared structural folder;
constructors with binding, boundary, or scheduling effects have explicit semantic handlers.

The generated default projection group installs a separate, lowest-priority `PreludeScope`.
Ordinary lexical and block-wide definitions win before fallback lookup.
A retained `none` annotation disables fallback in its payload; mobile candidates retain that policy when moved.
Source boundaries start independent environments, and the source's own generated prefix establishes its defaults.
An otherwise unresolved enabled `OS` becomes `Internal::OS`, using the existing checked `BuiltinTypeResolution`
against the environment's visible witnesses.
No provider package is opened or imported by that reference, and no successful definition-reference event is fabricated.

Ordinary lexical bindings follow the [source-boundary rules](../../language/src/sources.md#source-boundaries).
Reuse between imported instances follows the
[package-resolution model](../../language/src/sources/packages.md#copy-resolve-merge-analyze).
Successful lookup emits a borrowed `ResolvedReference` containing the occurrence,
selected definition, optional owning `BindingSite`, and active enclosing bindings.
Failed lookup records its diagnostic and emits no successful-reference event.
It becomes a hole only inside the resolving computation so other names can be checked.

[`ResolutionObserver`](../../../../lang/surface/src/scoped/observers.rs) receives references
and scope events without access to mutation, pruning, or rejection.
Pairs forward events and pair their final outputs.
Reference indexing constructs the definition-to-use relation.
Hole events retain their exact textual origins; completion captures only the requested origin
and keeps its last actual visit.
Holes synthesized from failed name lookup do not emit further scope events.
The scope view is borrowed; snapshots are allocated only by consumers that retain them.
Context elaboration emits no second event stream for generated wrappers.

Blocks require a preparatory binder scan so forward references can resolve before their definitions are visited.
The [block collector](../../../../lang/surface/src/scoped/blocks.rs) respects nested block and provider boundaries,
projects candidate binders once, and installs an unambiguous block-wide environment.
[`DependencyAnalyzer`](../../../../lang/surface/src/scoped/dependencies.rs) receives the same reference events
but is a required collaborator: each block needs its result before it can be elaborated.
`begin_block` seeds every candidate, including candidates with no edges.
An event adds dependencies to every active binding whose owner matches the dependency's owner;
nested block graphs remain separate.
Contextual `OS` uses are recorded separately until ordinary edges are complete.
At `finish_block`, the analyzer computes the reverse closure
of those users and orders them after independent bindings, implementing the
[provider dependency rule](../../language/src/sources/core-prelude.md#explicit-providers-and-shared-openings).
Existing edges remain intact, and dependent bindings cannot become new implicit back-edges.
`finish_block` removes and returns the completed graph, while `abort_block` discards an incomplete graph
before recovery continues outside that block.
All graphs must be closed when resolution finishes.

Strongly connected components form a condensation DAG; source order breaks ties and orders recursive members.
All recursive parameter components are diagnosed before elaboration.
Parameters become abstractions, acyclic definitions become lets, and recursive definition components become `RecGroup`;
the checker subsequently verifies those groups' admissibility.
`Residual` indirections preserve ownership at the original mobile sites after binders move.
The [context collector](compiler-data.md#scoped-structural-traversal) summarizes free definitions
in the elaborated term for checking.

Strict publication rejects the entire scoped program when any diagnostic was recorded.
Independent children and block candidates continue after a failure where their inputs remain available.
Duplicate binders are all diagnosed, then resolution skips the ambiguous block scope instead of choosing a winner.
Completion can retain a request-local program with unbound-reference recovery;
other failures retain captured observations but prevent program publication.
No rejected strict result reaches type checking or execution as a normal program.
Parser and completion tests, observer visit counts, dependency lifecycle tests,
and the [uniform-term fixtures](../../../../lang/tests/cases/uniform-term) exercise this boundary.
The [binding rules](../../language/src/bindings-and-scope.md) explain placement and identity;
[deferred recursion work](../../../ideas/README.md#recursive-admissibility) retains stronger admissibility questions.
