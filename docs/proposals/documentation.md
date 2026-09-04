# Project documentation

Zydeco documentation should help a reader move from discovering an operation to understanding and using it.
A source comment can explain intent, but using an API also requires knowing its current type, its dependencies,
and how its examples relate to the reader's program. Those facts already have owners in the compiler.
The documentation system should combine authored explanations with compiler information and make the result
available through editor features, a searchable project reference, and terminal queries.

The first three implementation stages now provide shared semantic documentation queries, standard editor
documentation, a searchable offline reference, explicit example checking, and a persistent VS Code panel.
The [authoring guide](../documentation.md) describes their concrete syntax and commands.
This proposal also records the intended extensions: parameter and constructor-arm subjects, richer type
navigation, contextual discovery, and execution under an explicit runner policy.

The review questions are:

- What does `@[doc]` describe when its payload is an arbitrary term?
- How does documentation follow bindings, imports, aliases, and structural field projections?
- How should public interface documentation relate to implementation and use-site explanations?
- Which interactions work through ordinary LSP, and which require a richer editor client?
- What makes an example reproducible across the editor, generated reference, and CI?
- Which compiler component owns each fact, and what remains available during an incomplete edit?

## Starting point

The [source style](style.md) defines `--|` as Markdown documentation prose.
An uninterrupted block immediately above `@[doc]` becomes part of that annotation; a blank line or ordinary
comment breaks the attachment. Source analysis warns about unattached text blocks.

[`DocumentationSite`](../../lang/surface/src/textual/source.rs) retains the annotation, its payload term,
the attached text, and source spans. [`DocMeta`](../../lang/surface/src/metadata.rs) currently preserves arbitrary
metadata arguments without assigning them a documentation-specific schema.
[`SourceGraph::documentation`](../../lang/session/src/source/documentation.rs) collects sites across dependencies
in deterministic provider-before-consumer and source order. This is an inventory of authored attachments;
the semantic `DocumentationIndex` adds associations to public fields and resolved definitions.

Cajun presents inferred kinds and types, source links, selected type-definition information, and the first
documentation paragraph in [hover](../../editor/cajun/src/analysis.rs). Ordinary name completion presents names,
optional type details, and full documentation from the same resolved analysis.

The design must also respect [uniform term composition](term.md): a file contains one complete term,
and libraries use ordinary functions and packages. A documentation generator therefore needs an account of
term interfaces and named fields rather than assuming a language of top-level module declarations.

## Reading and using documentation

One documentation model should support different reading depths. A hover answers an immediate question;
a persistent view supports exploration; a project reference supports discovery without an open source file.
The content and semantic relationships remain shared even when the amount displayed differs.

| Reader's question | Surface | Primary content |
| --- | --- | --- |
| What is this? | Hover | Signature, summary, related types, link to full documentation |
| Which operation should I choose? | Completion | Summary and relevant details for the selected candidate |
| How do I call it? | Signature help | Active argument, parameter explanation, remaining arguments |
| How does this API work? | Persistent IDE panel | Full prose, interactive types, examples, source navigation |
| What does this project provide? | Project reference | Overview, guides, public API, search, dependencies |
| How can I inspect it in a terminal? | CLI | Text documentation, reference generation, example checking |

Hover should begin with the classifier and the first prose paragraph. Long explanations and example collections
belong in the full view, which must be reachable by an editor command as well as by supported hover links.
Completion can load fuller documentation when a candidate is selected, keeping the initial candidate list cheap.
Signature help must associate parameter prose with resolved parameter subjects; it must not infer parameter
identity from a rendered signature's spelling or count every arrow as a source parameter.

### Persistent IDE panel

The panel follows the cursor by default and can be pinned while the user edits elsewhere.
Pinning retains the selected subject, not an indefinitely valid compiler snapshot. An edit refreshes that subject
when its identity can be recovered; deletion or ambiguous recovery makes the unavailable subject explicit.
Back navigation preserves the reader's path through related documentation.

A type has two useful perspectives: the declared interface and its instantiation at this use.
The existing [integer interface](../../lib/std/numeric/integer.type.zy) provides a representative example:

```text
Declared interface:  Thk (Integer -> Ret Integer)
At int64/increment:  Thk (Int64 -> Ret Int64)
```

The panel shares the authored explanation between these views and obtains the substitution from checking.
Selecting `Thk` explains a suspended computation and its forcing operation; selecting `Ret` explains the returned
value and how a caller can bind it. Explanations of these language constructs can be maintained documentation
linked to compiler-recognized concepts, while the particular type and substitution come from the current analysis.

Examples extend this interaction. A reader can open an example in a scratch document with its setup,
edit it, inspect the types of subexpressions, and request checking or execution. The original example remains
available for comparison. Diagnostics and result information appear beside the example that produced them.
Selecting an API field also offers navigation to its contract and, when known, its implementation.

The implemented panel follows source occurrences and invalidates a pin when an edit overlaps its selected range.
It renders complete declared and use-site signatures; clickable type subterms and separate implementation views
remain extensions. Its built-in example actions are **Check** and **Open scratch**. Runtime execution is disabled.

Expected-type discovery is a later extension. At a hole, the panel could explain the expected classifier and
show relevant visible operations and examples. It should reuse the semantic completion and compatibility queries
described in [completion](completion.md); documentation does not introduce a second type-search algorithm.

### Standard editor support

The implemented baseline uses standard LSP hover, completion documentation, document links, and navigation.
Signature help remains dependent on parameter-subject support.
The protocol explicitly supports [markup in hover][lsp-hover] and
[loading completion documentation on selection][lsp-completion]. These provide useful documentation even when
the client cannot host a custom panel. Parameter explanations can use [signature help][lsp-signature].

A persistent interactive panel requires additional client support and capability negotiation.
Its requests should use typed documentation and example identities tied to a source revision.
Ordinary text and links remain available for each substantive explanation; an interactive control adds an action
or a different view of the same information. Editor commands provide an alternative to client-specific links
or buttons embedded in markup. Support in VS Code, Zed, and other clients must be checked separately rather than
assuming that a shared language server provides the same panel facilities everywhere.

Lean's [interactive widgets][lean-widgets] demonstrate how source positions, compiler information, and a persistent
view can work together. Zydeco should initially provide a fixed set of useful interactions: navigation,
type inspection, and examples. Author-defined executable widgets would require a separate extension design.

## Authoring and attachment

The ordinary authoring form remains Markdown beside the relevant source term.
For example, within the existing integer interface's scope:

```zydeco
--| Increments an integer by one.
@[doc]
(#increment :: Thk (Integer -> Ret Integer))
```

The parser establishes the physical attachment, and subsequent analysis establishes its documented subject.
The original site and prose remain available even if the latter step cannot complete.
This separates a reliable source fact from the semantic relationships that make it useful at other locations.

### Subject selection

The initial rules cover documented expressions, simple bindings, and named fields:

| Attachment position | Documented subject and exposure |
| --- | --- |
| On an ordinary expression | That expression; available at its source position |
| On `begin ... end` | The block as a whole; a root block can supply the source overview |
| On a `let` or `def` with one simple binder | The binding introduced by that form |
| On the immediate right-hand side of one simple binder | The expression, also exposed through that binding |
| On a named introduction or named classifier | The field within its owning value or interface |
| On an import expression | The use of the imported source at that site |

For example, `@[doc] def increment = value in body` documents `increment`.
An annotation on an expression nested inside `value` remains local to that expression.
Likewise, documentation on an enclosing block does not become every member's summary.
The rules follow syntax ownership and resolved identities, including when `that` moves a binding within a block.
Source proximity after elaboration is not an attachment rule.

For a destructuring binding with several introduced names, a block of prose describes the binding operation as a
whole; it is not copied onto all the names. Individual members can be documented at named introductions or
interface fields. Parameter, constructor, and destructor attachments need explicit subject rules before being
exposed through parameter help or member lookup. The current collector enumerates term sites, so support for
additional syntax categories must include collection, spans, and tests rather than relying on comment proximity.

A site's primary subject follows the table even when the site is also the source root.
A file containing a documented binding does not gain a second, independently authored overview by implication.
The source reference may link to that subject, while a documented root block supplies an explicit overview.

### Shared prose conventions

The first nonempty prose paragraph supplies the summary. Headings, lists, code fences, and ordinary links retain
their Markdown meanings. Further attachments to the same subject and role retain source order, with the first
nonempty paragraph supplying the summary; multiple origins and roles remain distinguishable.

The current arbitrary `DocMeta` arguments are a syntax facility, not separate semantic authorities for renderers.
As options are standardized, documentation analysis should decode them into shared types with focused diagnostics.
Titles, grouping, and example options should be introduced only with a defined consumer and behavior.
The existing surface metadata catalog can then provide their completion vocabulary.
The spelling and schema of those options remain open; this proposal does not assign meaning to the arbitrary
arguments used in collector tests or introduce a compatibility parser for them.

## Origin, contract, and use context

A documented subject has an authored origin and may have many uses.
The origin identifies the explanation, its source location, and the scope in which its semantic links were written.
A use context identifies the selected occurrence, its accessible public interface, its current classifier,
and any established instantiation. These identities answer different questions and must remain separate.

For example, the integer interface's explanation of `increment` may appear for both `int32/increment` and
`int64/increment`. They share an explanation but display different classifiers.
Two unrelated interfaces may each expose a field named `read`; spelling does not make their documentation related.
Two openings of an existential package may share authored documentation while retaining distinct type witnesses,
as required by [package modularization](package-modularization.md).

Documentation identity must therefore remain outside type equality and runtime representation.
Normalization may erase a wrapper without erasing its tooling provenance. The resolver and checker must preserve
the origin relationships needed by documentation as explicit facts, rather than forcing Cajun to reconstruct
them by comparing printed types.

### Following bindings and fields

Resolved variable occurrences can lead directly to their binding's documentation.
Simple aliases and imports retain an origin edge when their target is established by analysis.
Field projections and projection patterns need both the owning interface and the resolved field identity;
a field label by itself is insufficient. The association must survive substitution and package opening.

These relationships are deliberately bounded. A function that constructs a new package does not automatically
inherit arbitrary implementation documentation from every value it computes with.
When checking establishes a public interface field, that contract can supply its documentation.
Where no contract or origin relationship is available, the view displays the known type and direct prose,
without claiming documentation from a similarly shaped or similarly named API.

### Interface and implementation

An explicit public interface supplies the contract documentation for its exposed subjects.
This includes ordinary imported type terms and `.zyi` companion signatures; their documented fields have the same
status regardless of which source form introduced the interface. A paired filename alone does not establish a
correspondence between every nested definition and field.

At a public use, presentation selects content in this order:

1. Show any explicit use-site explanation as context, with its own origin.
2. Use the documented exposed interface as the main contract when one is established.
3. If that contract has no prose, use documentation from a known originating binding or field, labeled by origin.
4. Offer implementation documentation separately when an implementation relationship is available.

An alias can introduce its own explanation, which is shown as context while retaining a link to the underlying
contract. Documentation at an import site does not rewrite the provider's explanation.
Implementations can document algorithms and local invariants without putting those details into the public
contract. Public signatures and automatically expanded type details respect the interface's abstraction boundary.
Deliberate navigation to available implementation source remains a separate view.

## Links, project references, and search

Semantic links should resolve through compiler-owned scope and member information.
Each prose fragment keeps its authoring scope even when rendered beside an alias or imported use.
This follows the useful precedent of [scope-aware rustdoc links][rustdoc-links].
Locally added prose is resolved where that prose was written.

Lexical names and interface members require distinct reference targets: `#increment` declares a field label,
not an ordinary lexical binding named `increment`.
A link to a lexical name uses the language's scope and shadowing rules; a link to a member identifies its owning
contract and field path. The resolver must not silently reinterpret an unresolved name as a global field search.
The chosen inline Markdown destinations are `zydeco:name:Integer` for a lexical name and
`zydeco:member:Counter/value` for a public member of an explicitly named owner.
An annotation's scope is the scope at its metadata wrapper, before its payload's bindings are introduced.
Reference-style semantic links are rejected in the first implementation so every destination has an exact
authored range for diagnostics and navigation.

Resolved links retain their source ranges so diagnostics, go-to-definition, and eventual rename support can act
on the original prose. Unresolved or ambiguous semantic links receive focused documentation diagnostics.
Ordinary web and relative page links remain ordinary links, without compiler name-resolution guesses.
Each renderer turns a resolved target into an appropriate editor location, local page, or published URL.

### Public reference structure

A documentation build starts from one explicitly selected source root per output file.
The public API follows its exposed classifier, named package fields, and named result interfaces.
Private implementation bindings remain available to local inspection; an internal reference view would require
a separate explicit publication mode. Listing every file or collecting every `@[doc]` does not define the public API.

Package-producing functions have generic reference pages describing their parameters and result interfaces.
Generation may use normal type analysis and normalization under ordinary resource limits; it must not evaluate
an arbitrary runtime term to discover the module contents. Named fields appear as interface members even when
they have no prose. Recursive references link back to established subjects instead of expanding indefinitely.

The reference should include an authored overview and guide pages alongside API entries.
These pages join the same link and search index. A guide uses an explicit entry-root context for public member
links; lexical references require a declared source context. There is no implicit project-wide namespace.
Guide pages are explicit, repeatable `--guide` inputs. Their semantic links use `zydeco:member:./value`,
where `.` denotes the selected public root; lexical guide links are rejected until source-context declarations
are supported. Guide filenames must have distinct stems.

Public routes should be deterministic and based on a selected entry root and canonical exposed member path.
Formatting-only edits must not change named public routes. In-memory arena IDs, allocation order, and raw byte
offsets are not published identifiers. Anonymous sections need stable authored anchors when durable external
links are required. The implemented selectors use slash-separated field names, `()` for each
function/computation result, and `.` for the root. HTML anchors begin with `api`, encode field-name UTF-8 bytes
as hexadecimal `-f-...` segments, and encode results as `-result`. Duplicate public paths fail explicitly.
Guide anchors derive from their filename stems. An output file records the compiler version and SHA3-256 hashes
of the exact source and guide inputs; it does not yet assign registry or release-version URLs.

Search initially covers exposed names, summaries, prose, and explicitly documented relationships.
An entry records its documentation origin and the dependency/source revision used for the build.
Local dependency documentation should correspond to the dependencies actually analyzed, and local reference
browsing should work offline. Type-directed search can later extend this index through compiler compatibility
queries; it is not a prerequisite for useful name and prose search.

## Examples as reproducible source inputs

Examples connect an explanation to behavior that readers can verify.
A displayed code fence may be a fragment or schematic notation, so a language tag alone must not promise
successful checking. Authors explicitly opt examples into verification. An opted-in example defaults to checking;
execution and expected rejection are distinct modes.

| Mode | Acceptance condition |
| --- | --- |
| Check | The complete example checks in its declared context |
| Run (future) | It checks, executes, and satisfies a declared output or result expectation |
| Expected rejection | It produces the intended diagnostic, at the relevant example location |

The typed example description includes its source origin, setup, mode, expectations, and execution policy.
Its setup can be a complete source file or an explicitly composed context that produces one.
Any omitted setup must be inspectable and included when opening or copying a complete runnable example.
Public examples exercise the public entry interface rather than accidentally depending on private lexical names.

This is particularly important for parameterized packages. Documenting a generic integer interface does not
provide a runnable integer module. An executable example must supply a particular implementation and the
capabilities required to construct it. Source and dependency identities become inputs to verification and caching.

The IDE, CLI, and CI use one example runner over ordinary source inputs and overlays.
Source maps connect generated setup and example text to their respective origins; an error in setup is identified
as such, while an error in the example points back into its code block. Relative imports resolve against the
declared original source context rather than an incidental temporary directory.

An expected-rejection example must match the intended diagnostic code and relevant location or semantic detail.
An unrelated import failure, a timeout, or a compiler crash does not count as success.
Run expectations should use typed values where comparison is defined, or explicit output expectations,
without attempting to parse presentation text back into compiler values.
The implemented fence options are `zydeco check` and `zydeco reject=tyck.code at=line:column`.
The rejection location is a one-based UTF-16 position within the complete example, and every produced type
diagnostic must match both the expected code and that location. Parser, import, and resolution failures cannot
satisfy a type rejection. Plain `zydeco` fences are display-only, and `run` is explicitly rejected.

The initial setup is complete source in a top-level, unindented code fence, with explicit imports and no hidden
context. Diagnostic ranges map through Markdown and comment prefixes back to the authored file, including
Unicode and CRLF. Scratch generation parses the source and rewrites recognized file import paths to absolute
paths before validating the copy. Numbered REPL imports do not define a portable scratch context.

### Checking and execution

Checking is suitable for automatic background work with cancellation and bounded resource use.
Execution is an explicit reader action or a configured test operation.
A `Thk` classifier establishes suspension, not purity or termination, and even a computation without host I/O
can diverge. Execution needs limits and explicit capability configuration.

Current checks are requested explicitly by the CLI or panel. Both frontends use one subprocess worker with a
30-second timeout, a 64 KiB example limit, a 16 MiB input request limit, and a 1 MiB response limit.
The worker receives known source overlays, runs an isolated compiler session, and records analyzed input hashes.
It never interprets the program or modifies the reader's project. These limits bound time and transferred data;
an operating-system memory limit and runtime capability policy remain separate work.

The runner must isolate example execution from editing the reader's project and supply declared test resources.
Filesystem examples should operate against supplied temporary fixtures; examples requiring unavailable capabilities
report that requirement. Static reference generation does not execute examples or silently acquire dependencies.
A generated page can show verification from the matching build and offer a local runner or supported playground;
an arbitrary example is not assumed executable inside every browser.

Opening an example creates a scratch source with a reproducible setup. Subsequent edits belong to that scratch
source, not the authored documentation. Verification records distinguish the original build from the current
edited example and are invalidated when the example or its inputs change.

## Compiler and frontend responsibilities

The shared documentation model belongs alongside the compiler session's source and semantic queries.
The existing attachment inventory becomes its source input; each compiler phase supplies relationships it
already establishes or explicitly adds for tooling. Documentation analysis never asks a renderer to recover
semantic identity from text.

| Fact | Owner |
| --- | --- |
| Text attachment, syntax subjects, source ranges | Textual source tooling |
| Binder identity, lexical scope, import provenance | Source assembly and resolver |
| Classifiers, member resolution, instantiation, abstraction boundaries | Type checker |
| Prose structure, semantic links, examples, publication selection | Shared documentation analysis |
| Revisions, dependency inputs, queries, cancellation | Compiler session |
| Markdown, terminal text, HTML, LSP fields, panel controls | Respective presentation frontend |
| Example process execution and supplied capabilities | Runner, using the existing frontend execution boundary |

Representative shared types are a documentation identity, a typed subject, an authored origin, and a use context.
Subjects distinguish expressions, definitions, owned members, and authored pages.
Bodies contain structured prose, links, and examples; classifiers remain typed query results until rendering.
Optional facts and typed diagnostics record what is available instead of collapsing the whole document into one
success flag. These are modeling requirements, not commitments to particular Rust type names or crate boundaries.

Queries should support documentation at a position, documentation for a resolved subject, public members of an
entry interface, project search, and example verification. Results retain their source revision and dependent
inputs. Transient IDs passed to a client must be checked against that revision before later resolution or execution.

The first implementation should build on the current immutable `ProgramAnalysis` and source graph.
It does not require rewriting type checking into a new incremental engine.
Prose parsing can be cached independently of type enrichment, and expensive details can be requested on demand.
Types use the policies in [elaborated type rendering](typed-type-rendering.md); interactive type fragments need
semantic anchors from the renderer rather than reparsing its formatted output.
Inserting editable source must use original source or a separately validated source-generation path.

### Incomplete edits and failures

Documentation lookup must not require the entire project to typecheck successfully.
Current-source attachments and recoverable subject information remain useful when richer facts are unavailable.
Recovering syntax may retain an attachment only when its actual annotation and payload are present in the returned
source tree; recovery must not move a detached comment onto a guessed subject.
This extends the current strict source-graph path rather than assuming that path already handles incomplete files.

Available semantic facts must be justified by the current analysis, including when checking has rejected another
part of the program. If resolution cannot establish a subject, the view may show direct source prose without
inventing an origin edge or inferred classifier. A source revision change invalidates in-flight contextual results.
An older published reference can still be browsed as a labeled build, but its information must not masquerade as
the current editor state. These rules align with the recovery contracts in [completion](completion.md).

## Alternatives and scope

A static Markdown extractor would deliver readable pages quickly and remains a useful presentation backend.
On its own, however, it cannot associate a projected field with a contract, instantiate a signature,
or distinguish same-spelled subjects. It would leave the central editor questions unanswered.

An editor-owned documentation index would make the first hover change local to Cajun.
It would also duplicate scope, provenance, and subject rules when adding CLI output or another client.
The shared query boundary lets the first visible feature remain small while keeping those rules in one place.

Renderer-specific meanings for documentation arguments would allow presentation experiments, but could make an
example mean one thing in HTML and another in an editor. Shared typed semantics permit presentation differences
without different accounts of the documented program.

Fully programmable documentation widgets could support specialized visualizations.
Their execution, portability, and extension interfaces are substantial independent design questions.
The initial system can support the proposed learning interactions with built-in controls and ordinary examples.

The initial scope therefore includes subject association, prose, links, public reference discovery, and reproducible
examples. It leaves package hosting services, registry-wide search, arbitrary widget code, and program synthesis
to later proposals driven by concrete needs.

## Implementation sequence

1. **Subjects, hover, and completion.** Preserve attachment-to-subject provenance for expressions, simple bindings,
   and named fields; expose current-revision documentation queries; add prose to hover and completion.
   Include imported and projected uses so the slice tests the package model rather than only local identifiers.
2. **Links and project reference.** Resolve semantic links, diagnose broken targets, establish public exposure
   and route rules, and generate a searchable offline reference with explicitly selected guide pages.
   Add terminal inspection through the same queries.
3. **Verified examples and persistent panel.** Establish reproducible example inputs and diagnostic source maps,
   share checking with CI, and add a capability-negotiated panel with pinning, type views, and scratch examples.
   Enable execution only with the runner's explicit capability and resource policy.
4. **Contextual discovery.** Add richer parameter and member explanations, expression-level exploration,
   and expected-type search as the corresponding compiler queries and subject rules become available.

Stages 1–3 are implemented with the current scope described above. The commands are
`zydeco doc show <root> [subject]`, `zydeco doc search <root> <query>`,
`zydeco doc build <root> --output <html> [--guide <markdown>]`, and
`zydeco doc check <root> [--guide <markdown>]`.
`doc build` validates links and produces a reference without verification badges or example execution;
`doc check` separately verifies links and the opted-in examples. The generated file is self-contained and works
offline. Cajun exposes capability-negotiated `zydeco/documentation` and `zydeco/checkDocumentationExample`
requests; source-revision changes, including imported edits, invalidate their contextual results.

Each stage should update its callers directly and remove any superseded local extraction path.
There is no requirement to retain multiple semantic interpretations or editor-specific documentation stores.

## Acceptance and rejection cases

Implementation should pair successful cases with the failures that establish the intended boundary.
The following are focused regression requirements, to be implemented with each corresponding stage:

| Accepted behavior | Rejection or invariant to retain |
| --- | --- |
| An adjacent `--\|` block reaches its documented subject | Blank lines and ordinary comments still break attachment |
| A documented simple binding appears at resolved uses | Shadowing does not pick documentation by spelling |
| A documented right-hand side exposes prose through its immediate binder | Nested expression prose is not promoted |
| A block overview remains readable | Its prose is not copied onto all nested definitions |
| A field's documentation follows a resolved projection or renamed projection pattern | Same labels in unrelated owners remain distinct |
| Imported and aliased uses retain an established origin | Use-site prose does not replace provider prose globally |
| A public interface supplies its field's contract | Structural equality alone does not inherit documentation |
| One generic explanation appears with different concrete signatures | Distinct existential openings keep their type identities |
| A semantic link resolves in its declared scope | Ambiguous, missing, and out-of-scope targets receive precise diagnostics |
| A public field has a deterministic reference route | Formatting does not change the route or expose arena IDs |
| A valid example checks in its explicit setup | Missing imports and private names cannot be supplied accidentally |
| An expected-rejection example matches its intended diagnostic | An unrelated error, crash, or timeout cannot satisfy it |
| A run example uses declared fixtures and capabilities | Unavailable capabilities prevent the run without changing project files |
| Current prose survives an unrelated type error | Previous-revision semantic results are discarded |
| A supported panel offers interactive inspection | Ordinary clients retain equivalent readable documentation and links |

Example tests cover Unicode offsets and dependency changes invalidating verification; hidden-setup tests become
necessary if that feature is introduced. Editor verification should exercise keyboard navigation, pinning across edits, cancellation,
and example results associated with the correct revision. Shared-model tests should compare semantic targets
across HTML and LSP rendering rather than require identical presentation strings.

## Remaining decisions

The implemented slice retains plain annotation arguments and the subject rules above.
The following extensions require additional compiler relationships, source examples, or client investigation:

- The syntax and typed schema for metadata options, parameter and arm attachments, and composed example setup.
- Implementation fallback beneath a docless explicit field contract. Current projection provenance identifies
  the contract; following the particular implementation requires additional bounded value-origin relationships.
  Binding and alias fallback works when its origin is already known, and inferred named fields retain their own prose.
- Separate contract/implementation navigation, clickable type subterms, and expected-type discovery.
- Release-version URLs, stable authored anchors for anonymous sections, and an explicit internal publication mode.
- Guide discovery configuration and declared lexical source contexts for guide links.
- Panel clients beyond VS Code, signature help for Zydeco's application forms, and editable source generation
  where original source alone is insufficient.
- Runner capabilities, limits, and value-comparison support for deterministic execution examples.

These decisions refine one shared rule: authored documentation belongs to a precise subject,
and compiler-established relationships determine how it can be presented in another context.

[lsp-hover]: https://github.com/microsoft/language-server-protocol/blob/gh-pages/_specifications/lsp/3.17/language/hover.md
[lsp-completion]: https://github.com/microsoft/language-server-protocol/blob/gh-pages/_specifications/lsp/3.17/language/completion.md
[lsp-signature]: https://github.com/microsoft/language-server-protocol/blob/gh-pages/_specifications/lsp/3.17/language/signatureHelp.md
[lean-widgets]: https://lean-lang.org/examples/1900-1-1-widgets/
[rustdoc-links]: https://doc.rust-lang.org/rustdoc/write-documentation/linking-to-items-by-name.html
