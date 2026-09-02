# Elaborated Type Rendering

## Problem

Editor surfaces must show types that exist only as elaborated, typed-arena data: hover
annotations, the projected types of field projections, and the expected/found types in
diagnostics. Elaboration is the translation from source syntax into the checker's internal
types, and its output spells things the user never wrote — generated applications, generative
witnesses, field labels that track package components. When hover printed those spellings
verbatim, `process/exit` reported its type as `(Thk Int64 -> [14008893674656265450#8])`: an
IR-shaped application and a hash-named witness — the fresh abstract type that opening a
package had allocated. The question this proposal records is which component should own
turning elaborated data back into readable text, and when that answer should change.

## Rendering Surfaces

Three printers coexist, with different owners and obligations:

- The **textual pretty printer** (`lang/surface/src/textual/pretty.rs`) is the canonical
  formatter. It is a layout engine over textual AST nodes carrying spans and trivia, and it
  owns every source-shape decision: precedence, telescopes, punning, comment placement.
  Besides `render_unit` for whole documents it exposes `render_term` and `render_pattern`
  for bare nodes.
- The **scoped formatter** (`lang/surface/src/scoped/fmt.rs`) is explicitly a debug printer;
  it appends concise identifiers to names and is not user-facing.
- The **statics formatter** (`lang/statics/src/fmt.rs`) prints typed-arena entities. Its
  consumers are cajun hover, the stackir printers, the statics test suite, and the
  diagnostics layer's nested type rendering.

## Constraints

- Hover's types are frequently *synthesized*: the result of a projection, or a forall body
  after instantiation, has no source node that provenance could recover, so their text
  cannot be sliced from the document.
- Typed-only constructs have no source spelling and need an explicit policy: fillable holes,
  nameless abstract types, intrinsics (`Int64` versus `@(intrinsic(i64))`), label kinds, and
  the manifest `=` marker.
- Nothing constructs textual AST programmatically today; every consumer of the textual
  arena reads parser-produced nodes. A typed-to-textual bridge would be the first writer
  besides the parser.
- The consumers disagree on purpose. Diagnostics benefit from elaborated detail — naming the
  instantiated witness in a mismatch error is information, not noise — while hover is read
  next to the source and wants source-shaped output. One canonical renderer serving both
  would be wrong for one of them.

## Chosen Invariants

The statics formatter is the renderer for typed data, and it renders in source form wherever
a source spelling exists. Its layout follows the meta-rules that `formatting.md` states for every
printer of Zydeco syntax, instantiated at `layout(ignore)` because elaborated entities carry no
retained intentions or trivia; width alone decides where a permitted boundary breaks.
The source-form decisions are:

- Operands are parenthesized by precedence class: atoms stand bare, applications bind
  tighter than arrows and products, and only compound operands are wrapped, so
  `Thk (Int64 -> OS)` no longer prints as `(Thk Int64 -> OS)`.
- Generative witnesses are named at creation. A skolem — the fresh abstract witness
  allocated when a package pattern opens an existential — inherits its declaration's name as
  an `abst_hints` entry, so `SystemOS` prints by name and links to its declaration instead
  of surfacing as `[hash#index]`.
- Manifest existentials print their source shape `(= P as D : K)`, dropping the witness
  labels that projection tracking wraps around binders and kinds.

These landed together with the term hover feature (`515acfd7`, `6e367b62`, `78b0b604`).

## Alternatives

1. **Polish the statics formatter** (current choice). The policy helpers are a few dozen
   tested lines; the cost is drift — source-shape knowledge now lives in two places, and
   grammar changes must update both.
2. **Reify typed data into the textual arena and call `render_term`.** This buys canonical
   layout, output that re-parses, and a single spelling authority. It costs a translation
   layer of roughly three hundred lines that must map every typed construct, fabricate
   spans for synthesized nodes, and track the textual grammar thereafter; and it does not
   remove the statics formatter, because diagnostics and the stackir printers still render
   typed data directly.
3. **Share precedence classes.** Extract a syntactic-precedence vocabulary into
   `zydeco-syntax` and have both printers consult it. This narrows the duplication to the
   actual policy, but the two ASTs classify nodes differently, so the shared abstraction
   becomes a third structure to maintain for a modest gain.

## Reassessment Trigger

The reifier becomes worth its cost when a second *source-faithful* consumer of typed data
appears — for example a code action that inserts an elaborated type as a source annotation,
or golden tests that re-parse rendered output. Until then the statics formatter's small,
deliberate deviations, such as printing intrinsics by their primitive names, are cheaper
than the bridge.

## Remaining Uncertainty

- Intrinsic spellings print as canonical primitive names (`Int64`) where source writes the
  attribute form (`@(intrinsic(i64))`). Harmless in hovers, but a reparse round-trip would
  not be faithful.
- Quantifier heads print as one unbreakable run: `forall (A : VType) (B : VType) .` offers the
  renderer no boundary, and nested quantifiers print as separate `forall ... . forall ... .`
  scopes rather than one folded telescope. The parameter-telescope family — one binder per
  row, with adjacent same-form scopes folded — is unimplemented, so a long binder list
  exceeds any width budget; cajun's column-budget fixtures are calibrated around this
  (32 columns for the doubly quantified `zip`).
- Hover labels for terms still print elaborated structure — applications keep their internal
  parentheses — so only type operands are source-shaped so far.
- Whether some diagnostics would communicate better with source-shaped types has not been
  surveyed; the diagnostics layer shares the statics formatter and inherits its improvements
  automatically, so the default is elaborated detail everywhere.
