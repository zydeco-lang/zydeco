# Surface Formatting Design

The Zydeco formatter is a semantics-preserving normalizer over the parsed textual arena. Canonical syntax decides
how a construct is spelled. Retained source information may constrain its presentation only at an explicit
grammatical boundary.

The *pretty printer* performs the arena-to-document transformation in `textual`. The *formatter* is the complete
parse, print, and replace workflow exposed by `zydeco fmt` and Cajun. Both frontends use the ordinary parser and
the same printer.

## Retained Source Information

A parsed source has three kinds of printable information:

| Kind | Examples | Preservation contract |
| --- | --- | --- |
| Canonical syntax | binders, applications, precedence, field payloads | Preserve meaning and choose one spelling. |
| Trivia | documentation, line, and block comments | Preserve content and effective attachment. |
| Intentions | a joined line, a break, one empty line, a multiline group | Use only at declared layout boundaries. |

Spans provide evidence for trivia and intentions; they are not a second printable syntax tree. A leading comment
extends the layout start of its anchor, so the boundary compositor sees the separation before the comment. The
comment text itself remains stored once in `SurfaceTrivia`.

```text
source -> lexer and parser -> textual arenas and spans
       -> presentation capture -> trivia and intentions
       -> grammar-aware document construction
       -> width selection -> formatted source
```

Punning belongs to canonical syntax rather than intention. If a named term or pattern contains the same-named
variable, the printer always chooses its concise form. Raw whitespace is not retained.

## Formatter Laws

### Semantic identity

Formatted output must parse and desugar to the same structure. Parentheses are removed only when the exact parser
position accepts the enclosed term or pattern. Annotation payloads are checked separately because moving an
annotation across a named or projected pattern changes the tree even when the printed tokens look similar.

### Content retention

Every comment survives. Documentation starts on a fresh line at the indentation of its anchor, and only an
adjacent documentation block attaches to `@[doc]`. An unattached block remains visible and produces a warning.

### Canonical convergence

Equivalent spellings converge on one form. Horizontal spacing and puns are canonical, empty regions contain at
most one empty line, and a complete source ends with one newline. Formatting twice with the same options must have
no further effect.

### Layout as a lower bound

Vertical separation has the following order:

```text
joined < broken < one empty line
```

When intention preservation is enabled, an observed break is not collapsed and a larger empty region becomes one
empty line. A joined boundary can still break when its compact form does not fit. Local groups remain compact when
they fit, even inside an expanded parent.

### Boundary composition

A syntax case combines child documents through a named boundary policy. It must not inspect rendered text to
discover whether a child fits or spans lines. Punctuation placement and continuation indentation belong to the
relationship between children rather than to either child in isolation.

## Boundary Algebra

`LayoutFragment` carries a document and the first and last syntax anchors represented by that document.
`LayoutBoundary` names the source gap to consult:

- `Between` lies between consecutive entities.
- `AfterStart` lies between an enclosing construct and its first child.
- `AfterArmPrefix` lies between an arm header and its payload.
- `BeforeExistentialParameter` lies before a grammar-owned parameter delimiter that is not part of its binder.
- `BeforeEnd` lies between the final child and its closing delimiter.

The printer then chooses how much of that source information applies. A boundary can be canonical, preserve the
full break intention, or preserve only an empty line after moving a marker onto its own line.

`BoundaryLayout` supplies the compact gap, expanded gap, marker placement, and continuation nesting. Its common
forms are named by their effect: `aligned`, `hanging`, and `nested`. `StagedBoundary` distinguishes an ordinary
annotation from the `:` and `=` stages of a binding, because their expanded forms carry different indentation.

Document alternatives use the `pretty` algebra directly. A flexible boundary exposes its compact projection to
an enclosing group while retaining a complete expanded alternative. A candidate that is valid only on one line
contains a flat-mode guard. The final renderer therefore performs the only width selection; the printer does not
render temporary strings or maintain a syntax-specific boundary mode.

## Canonical Layout Families

Most constructs use one of these families:

| Family | Compact form | Expanded form |
| --- | --- | --- |
| Delimited region | Contents stay between delimiters; a thunk is `{ body }`. | Contents nest once and the closer returns to the opener. |
| Juxtaposition or list | Items use their canonical separator on one line. | Continuations nest once while fitting subgroups remain intact. |
| Parameter telescope | A fitting telescope follows its head. | The head stands alone; width expansion gives each parameter a row, while preserved source breaks retain fitting row groups. |
| Infix chain | Operators have one space on each side. | `*` and `->` lead continuation lines without recursive indentation. |
| Headed scope | A short head keeps `.`, `=>`, and its body together. | A multiline head ends with an aligned marker, then the body nests once. |
| Staged binding | Header, type, bindee, and placement remain together when they fit. | `:`, `=`, and then `in` or `that` close the stages at the binding indentation. |
| Sequence or block | A short stage may remain compact. | Tails of `do`, `let`, `def`, and `param` return to the enclosing indentation. |
| Arm block | A short arm header and payload share a line. | Arms begin with aligned `\|`; a broken payload nests once, while comments before `|` remain at the arm boundary. |

Each grammatical group makes one width decision for the boundaries it owns. If a delimited row overflows, the
delimiters and item separators enter their expanded layout together; boundaries inside each item remain
independent. Preserved source breaks partition fitting rows, but an overflowing row expands the complete outer
layer rather than whichever nested boundary happens to encounter the width limit first.

For layout purposes, `.`, `=>`, `in`, and `that` are scope-boundary markers. This is a presentation role shared by
several grammar categories. A constituent is “short” exactly when its complete compact alternative fits in the
remaining configured width; there is no second length threshold.

Canonical printing folds adjacent scopes of the same form into one parameter telescope. In intention-preserving
mode, a source line break before the nested introducer stops the fold. This rule applies to `fn`, `pi`, `forall`,
`sigma`, and `exists`. Consecutive existential nodes also normalize to one telescope during desugaring, so the
compact and repeated spellings have the same elaboration.

Minimal parenthesis formatting retains grammar-required groups. It also retains a multiline singleton group when
its delimiters provide an intentional boundary. Applications are the one self-grouping family: their own
compact-or-hanging boundary subsumes a singleton wrapper. `Parentheses::Preserve` is available when every parsed
singleton group must remain.

## Components and Policy

`PrettyFormatter` coordinates three reusable components over one arena. `GrammarContext` classifies rendered
terms and patterns against parser requirements. `Punning` recognizes concise field payloads. The boundary
compositor combines anchored documents with retained layout.

Semantic preservation, comment retention, punning, and convergence are laws rather than options. Printer policy
controls the positive `IndentWidth`, target line width, use of layout intentions, and treatment of transparent
parentheses. `zydeco fmt` and Cajun are adapters and must not introduce independent formatting rules.

## Verification and Extension

The focused regression matrix covers every layout family in compact, source-broken, and width-broken forms. A
repository corpus test formats the maintained `.zy` sources under `lib/` and `docs/spell/`. It then reparses them,
compares desugared structure, checks comment content, and verifies idempotence. Legacy examples and CLI fixtures
are excluded because they contain earlier syntax or test-harness directives rather than current parser input.

When syntax is added, first identify the parser requirement for each child. Then choose the canonical spelling and
an existing layout family for each boundary. A new primitive is warranted only when those choices introduce a new
invariant.

`NamedTermPunningAudit` is intentionally temporary. It records explicit term fields that canonical formatting can
shorten and should disappear after the standard library is migrated. Comments use entity anchors, typed arm
boundaries, and exclusion ranges. If future syntax permits truly floating comments, the model should add another
typed trivia boundary instead of retaining raw whitespace or a second token tree.
