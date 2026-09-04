# Writing and reading project documentation

Zydeco documentation combines Markdown attached to source terms with the compiler's information about
bindings, imports, and named fields. The same explanation appears in hover, completion, the VS Code
documentation panel, and a generated project reference.

## Start with an explanation

Write an uninterrupted `--|` block immediately above `@[doc]`:

```zydeco
--| The current counter value.
--|
--| Read this field to inspect progress.
@[doc] let counter = 42 in counter
```

The first prose paragraph becomes the hover summary. The panel and reference show the complete Markdown.
Use `--|` for blank lines within the block; a genuinely blank source line or an ordinary `--` comment breaks
attachment. Unattached text blocks produce warnings.

On a simple `let` or `def`, the explanation describes its binding. Documentation immediately on a binding's
right-hand side also follows its resolved uses. An annotation on `#field = value` or `#field :: Type`
describes that member. An annotation on a block describes the block itself; nested definitions acquire
documentation from their own annotations. Arbitrary expressions can have their own explanations too.

Documentation follows compiler-established origins, including imports, aliases, and resolved projections.
An explicit interface supplies the public contract; `.zyi` signatures participate in the same rules.
Prose written at an alias or import adds local context. Equal field names in unrelated packages do not share
documentation. At a concrete use of a generic interface, the explanation stays attached to the declaration
while the displayed type reflects the current use.

## Read it in the editor

Cajun supplies summaries in hover, full prose in name completion, clickable semantic links, and warnings
for invalid links. These features use standard LSP and do not require the VS Code panel.

In VS Code, place the cursor on a name or expression and run **Zydeco: Show Documentation** from the command
palette or editor context menu. The panel offers:

- **Follow and Pin:** follow the cursor, or retain the selected source occurrence while editing elsewhere.
  Edits before a pin move its position; overlapping edits invalidate it and prompt you to select it again.
- **Type views:** switch between the type at the cursor and the documented declaration when both are available.
- **Source and Back:** open the explanation's source and follow links through related documentation.
  Navigation entries in edited files are discarded so old positions are not reused.
- **Check and Open scratch:** verify an opted-in example or open a complete editable copy in a temporary file.
  Use ordinary hover, completion, and diagnostics while experimenting with that copy; Save As keeps it.

The panel refreshes after source changes and discards results from older revisions, including example checks.
It requires Cajun's version 1 documentation capability. During incomplete edits, available semantic facts still
provide documentation; where resolution fails, surviving comments can be read directly without an inferred type.

## Link to names and members

Ordinary Markdown links work alongside two explicit semantic destinations:

```markdown
[integer type](zydeco:name:Integer)
[current value](zydeco:member:Counter/value)
```

`zydeco:name:Integer` resolves a lexical name in the scope where that annotation was written.
`zydeco:member:Counter/value` names an owner and a public field path. The owner must already be in scope;
an annotation before a nonrecursive binding cannot link to the binding it introduces.
Imported prose keeps its original scope even when a consumer shadows a name.

Use inline Markdown links for semantic destinations. Reference-style semantic links and unresolved destinations
are diagnosed. Editors navigate to source; HTML uses a local API anchor when a unique exposed page is known,
and otherwise a source link. Source links in a copied reference require access to the original source paths.

## Build a reference

Build the CLI with `cargo build --bin zydeco` or install it with `cargo install --path cli`.
The checked-in [counter example](examples/documentation/counter.zy) has a companion interface and a guide.
From the repository root:

```sh
zydeco doc show docs/examples/documentation/counter.zy value
zydeco doc search docs/examples/documentation/counter.zy counter
zydeco doc build docs/examples/documentation/counter.zy \
  --guide docs/examples/documentation/guide.md --output /tmp/counter-docs.html
zydeco doc check docs/examples/documentation/counter.zy \
  --guide docs/examples/documentation/guide.md
```

Use `target/debug/zydeco` in these commands if you built without installing.
`doc show` defaults to the entry subject, `.`. Field paths use `/`; `()` selects a function or computation's
result interface, so a selector such as `'()/value'` describes a field of a generic result.
These are documentation paths, not executable Zydeco expressions.

The reference includes only the selected root's exposed interface, plus explicitly supplied `--guide` pages.
It expands named fields and generic result interfaces without executing package-producing functions or
publishing private local bindings. Duplicate exposed paths fail the build. Search covers public names and prose;
the HTML search also includes the selected guides.

Guides use `[value](zydeco:member:./value)` to refer to the selected public root. They have no implicit lexical
source scope. Repeat `--guide` to include more pages; guide filenames must have distinct stems.

The output is one self-contained HTML file with local search, source links, stable public anchors, compiler
version, and SHA3-256 fingerprints of the analyzed source and guide inputs. Formatting changes preserve named
public anchors. Raw author HTML is rendered as text and remote images are represented by their alt text.
`doc build` validates semantic links but does not check or execute examples, and the page claims no verification
result. Use `doc check` separately in CI.

## Verify examples explicitly

A plain `zydeco` fence displays code. Add `check` to require a complete source term that checks successfully:

````markdown
```zydeco check
let counter = @(import("counter.zy")) in counter/value
```
````

An expected-rejection example declares both a compiler diagnostic code and a position:

````markdown
```zydeco reject=tyck.missing-named-field at=1:15
(#value = 42)/missing
```
````

Positions are one-based line and UTF-16 column within the displayed example. Every reported type diagnostic
must match the expected code and contain that position. A successful program, a different error, a missing
import, a compiler crash, or a timeout cannot satisfy an expected rejection.

Verified fences must be top-level, unindented Markdown blocks, containing complete source with explicit imports.
There is no implicit surrounding lexical context or hidden setup. Relative imports use the directory containing
the comment or guide. **Open scratch** rewrites compiler-recognized file imports to absolute paths so an example
keeps that context in its temporary location. Numbered REPL imports cannot be copied this way.

`doc check` validates links and opted-in examples from the analyzed dependency graph and selected guides.
It exits unsuccessfully for invalid options or failed checks and reports diagnostic locations in the authored
comments. The panel checks a selected example with the editor's current overlays. Both use the same isolated
compiler worker with a 30-second timeout, 64 KiB example limit, 16 MiB request limit, and 1 MiB response limit.
Those are time and data limits, not an operating-system memory sandbox.

Execution mode (`run`) is currently rejected. Examples are checked without interpretation; execution requires
a separately designed capability and resource policy.

## Current scope

The first implementation covers source attachments, resolved origins, exposed members, semantic links,
offline reference generation, explicit example checking, and the VS Code panel. Parameter and constructor-arm
documentation, signature help, clickable subterms of panel types, expected-type search, and runtime examples
remain future work. Implementation documentation beneath a docless explicit field contract is not yet
recovered through value flow; such projections still show their exposed type. The panel currently links to the
selected documentation origin rather than presenting separate contract and implementation tabs.

The [design proposal](proposals/documentation.md) records the invariants and remaining extensions.
