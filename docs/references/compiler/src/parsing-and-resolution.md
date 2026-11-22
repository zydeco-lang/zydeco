# C4. Parsing, Desugaring, and Name Resolution

[Textual syntax](../../../../lang/surface/src/textual/README.md) owns Logos tokens, literal decoding,
LALRPOP parsing, metadata syntax, spans, trivia, and retained layout intentions.
Strict parsing supplies compilation input; recovering parsing supplies structured holes
and recovery evidence for tooling.
A recovered edit is not silently accepted as an executable program.
Formatting consumes the same textual model, so parser changes must preserve the information required by C15.

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

## Desugaring Boundary

[Bitter desugaring](../../../../lang/surface/src/bitter/README.md) removes surface sugar while keeping unresolved names.
It expands binding headers and telescopes, makes nominal sealing and CBPV introductions explicit,
keeps the consecutive parameters of `fn`, `pi` and `forall` and the arguments of one juxtaposition as one node,
and retains special boundaries such as classifier queries and monadic payloads.
Source assembly has already resolved import and literal splices.
Primitive terms have compiler-owned identities, and every generated node records its textual origin.
A special metadata node must survive whenever ordinary forwarding would change its checking environment or expectation.

## Desugaring Folders

[`DesugarFolder`](../../../../lang/surface/src/bitter/desugar/mod.rs) owns textual lookup,
recursive lowering, the `BitterBuilder`, and a source-term memo table.
Successful and rejected terms enter that table through one path;
reusing a rejected source node does not repeat its diagnostic.
Independent child checks collect their errors before rejecting the enclosing construction.
A rejected source returns all collected errors with their source context and publishes no complete bitter arena.

The [telescope rules](../../../../lang/surface/src/bitter/desugar/telescopes.rs) lower parameter sequences,
flatten consecutive existential layers, and construct quantifier and packed value layers.
A `pi` telescope stays one node with its binders in order, as an abstraction's parameters
and an application's arguments do, and checking walks it one binder at a time,
each at an allocation site of its own, so in the core a group and its nested spelling elaborate alike;
a call-by-value block translates the group as one function.
`val pi` and `sigma` telescopes become one layer per binder.
The [binding rules](../../../../lang/surface/src/bitter/desugar/bindings.rs) interpret binding flavors
and build terms together with classifiers over the same parameter fold.
Classifier state distinguishes absence, an annotation on the complete binding,
and an annotation extended across its parameters.
Only the last case freshens classifier binders.
Destructor parameters discard a propagated abstraction classifier, replace a binding classifier with a hole,
or reject a value abstraction, according to the enclosing rule.

Already-lowered construction receives the builder;
[CBPV introductions](../../../../lang/surface/src/bitter/desugar/cbpv.rs) share this allocation and provenance boundary.
Rules retain their semantic child schedules: a binding lowers its binder, bindee, classifier, and parameters;
a packed value lowers its body before processing parameters in reverse, with evidence before the parameter form.
Meta annotation inspection precedes payload lowering when its rule depends on authored syntax.

[`MetaRules`](../../../../lang/surface/src/bitter/desugar/meta.rs) inspects read-only textual syntax
and selects one typed action: preserve an annotation, construct an intrinsic, query a classifier,
build a monadic block, expand a prelude opening, or record partial binders and preserve the annotation.
The catalog identifies the recognized kind once; arbitrary annotations retain their structure.
Intrinsic and FFI payload checks inspect authored syntax before any lowering can erase grouping.
Intrinsic construction skips its hole payload; classifier queries and monadic actions lower their payload once.
Partial actions carry the header's textual binder identities and record them before visiting the payload,
including when that payload has already been memoized.
Existential parameter annotations use their own allowed-role validation and retain their precise annotation sites.

`PreludeMeta` validates either `none` or a lexical `from` argument independently of reconstruction.
The override action builds the ordinary fixed projection group and lowers the payload once;
`none` survives to resolution because it changes fallback lookup.
The [default source prefix](sessions-and-queries.md#source-loading-and-package-selection) is already present
when this folder runs.
These bounded actions need no separate `Explicit` traversal or resumable scheduler.

Override-generated definitions and projections retain the annotation's metadata origin;
the bindee retains the exact identifier argument's origin.
Default-generated definitions instead retain the source prefix's pattern origin and an explicit generated-binder mark.
Neither creates an editable declaration token.
Hover and completion identify the prelude origin; rename refuses generated definitions,
while renaming a supplied view includes its `from` reference.
Go-to-definition for an override points to its annotation; a default binding has no fabricated declaration location.
Contextual `OS` retains its authored term origin without inventing a `DefId`.
The editor classifies that term from checked evidence and filters its completion using its `CType` classifier.
The textual formatter preserves authored annotations rather than printing the inserted default prefix.
[Surface tests](../../../../lang/surface/src/bitter/desugar/tests.rs),
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
