# Zydeco Code Style

This guide is for contributors familiar with typed functional programming who may be new to Zydeco.
Zydeco uses one term language for kinds, types, values, and computations,
so a clear program gives the reader enough surface evidence to recognize each term's role.
In particular, the source should reveal where a name is available, whether its definition remains transparent,
and where computation is sequenced.

The conventions below favor Zydeco's direct forms and use annotations where they clarify a type-system boundary.
The [language reference](references/language.md#2-lexical-structure-and-syntax) specifies accepted syntax and grouping.

## Reading the Surface Syntax

Juxtaposition makes the classifier's structure visible without assigning an argument list to each head.
In `+Some +Pair(left, right)`, the outer constructor has one payload headed by another constructor.
In a copattern such as `.route +First(value) .left`, `.route` exposes an argument type,
`+First(value)` matches that argument, and `.left` observes the remaining codata computation.
Spaces separate those steps; the [computation rules](references/language.md#6-computations-and-control)
explain how each step follows the residual classifier.

Keep that path visible when laying out longer arms.
Align related observations and indent their bodies.
Parentheses mark actual grouping, such as a product payload or an annotated parameter;
newlines and alignment help the reader follow the path without changing its parse.

The distinction between `->` and `=>` helps readers find the boundary between a classifier and a body.
For example, in `fn (f : Thk (A -> B)) x => ! f x`, the annotation's arrow stays inside the header,
while the fat arrow introduces the body.
The same visual boundary applies to `fix`, match arms, and comatch arms.
The leading `|` already separates a match scrutinee from its arms, so an additional `with` would repeat that boundary.
`end` closes the delimited region, while braces identify suspended computations.

### Pattern alias syntax

Use `(p; q; ...)` to show several observations of one input, such as `((left, right); whole)` or `(/x; /y; whole)`.
The [pattern rules](references/language.md#7-patterns-and-coverage) define its scope and admissibility.
Commas expose product components; semicolons keep every observation attached to the same bindee.
Parentheses make the complete group a composable pattern terminal, including inside constructor payloads.
The matching relation is symmetric, while source order makes the availability of earlier bindings visible.

A directional binary as-pattern would emphasize one whole-value alias over one refining pattern.
The alternatives `(let aliased) = pattern` and `(let aliased = pattern)` borrow binding syntax:
the first collides with the enclosing binding delimiter, and the second makes multiple aliases nest.
`aliased => pattern` reuses the header/body separator and suggests control flow.
The semicolon form accommodates several field selections uniformly and preserves source order
if later pattern designs introduce sequential observations.
The [coverage proposal](proposals/exhaustiveness.md#refutable-conjunctions) retains the unresolved
refutable-conjunction questions.

## The Shape of a Source File

A source file denotes one complete term.
Most programs use an outer `begin ... end` block to gather their dependencies and definitions
before producing a computation or package:

```zydeco
begin
  let make_std = @(import("../std/std.zy")) that
  param (/VType; /Thk; /String; /OS; builtin) : @(import("../std/builtin.zy")) that

  ...
end
```

An import is metadata on a hole. Every occurrence refers to the same independently checked source term;
binding the result remains useful when the program should evaluate the imported term once and reuse its value.
Place imports and parameters near the beginning of the file, followed by foundational types, operations,
and finally the term that the file provides.

The forms ending in `that` contribute binders to their nearest block.
Dependency analysis places each binder where its dependencies are available,
and its name may consequently be used throughout that block.
Source order still matters to the reader and breaks ties between independent bindings,
so arrange definitions in a natural explanatory order.

The corresponding `in` form introduces a binder directly over its written tail.
It is a good fit for a short local name whose scope should remain visible in the source:

```zydeco
let pair = (left, right) in
ret pair
```

When the file itself is a parameterized term, write its parameters as leading `param` or `param val` binders,
according to the result judgment, instead of naming a wrapper function and returning it.
For a type family:

```zydeco
param Bool : VType in
exists (= Int as @(intrinsic(int)) : VType) .
  Numeric Bool Int
```

The importing file binds the result under whatever name it chooses, so a wrapper name
that is only returned once adds no information.
Keep an ordinary `let` when the body references the binding more than once.

When a value function's implementation is itself a block, make the block the outer term
and contribute its parameters with `param val`:

```zydeco
begin
  param val (builtin : Builtin) that
  let Api = ... that
  pack (Api : VType) where ... end
end
```

This form keeps imports, parameters, definitions, and the resulting package in one context-forming block.
Prefer it to wrapping the whole block in `val (builtin : Builtin) => begin ... end`.
Use direct `val P => V` when the body is a compact value expression and does not need a block.

The final term of a block determines what leaves the block.
A library usually ends in a package; an executable ends in its main computation.
Braces have a separate role: `{ computation }` is a thunk value.

## General Bindings

The general binding form, or genbind, records a function telescope once.
Its parameters belong in the header, and the annotation after them is the residual classifier:

```zydeco
def ! apply
  (A : VType)
  (B : VType)
  (function : Thk (A -> Ret B))
  (value : A)
  : Ret B =
  ! function value
that
```

The `!` is part of the binding pattern, so the header mirrors the use site:
where a caller writes `! apply A B function value`, the definition writes `def ! apply`.
A plain binding pattern corresponds to a use site that consumes the name without forcing it,
such as passing a package value directly.
The thunk-pattern spelling expands to `def apply ... : Thk (Ret B) = { ... }`;
the parameters wrap the body in the appropriate type-level abstraction or computation abstraction over values.
Genbind keeps the source at the level at which the operation is normally read.

The binding keyword controls transparency.
`def` seals its right-hand side and, for a type, establishes nominal identity.
It is the usual choice for an abstract `data` or `codata` definition.
`let` retains the defining equation, which suits type aliases, package signatures, and local abbreviations.
Transparency is independent of placement: either keyword may end in `in` or `that`.

Recursive computations use `fix` in the same header:

```zydeco
def fix loop (state : State) : Result =
  ...
that
```

This form binds a thunked fixed point and makes its parameters explicit.
Recursive types continue to use ordinary `def` bindings;
the block scheduler collects mutually recursive type definitions into a recursive component.

A genbind absorbs the parameter prefix visible in its written classifier.
Some aliases deliberately hide a further telescope.
In that case the remaining `fn` communicates the introduction form of the alias:

```zydeco
def ! algebra (R : CType) : Algebra Ret R =
  fn A computation continuation =>
    ...
that
```

Give exported components, type parameters, and informative patterns explicit annotations.
Expected-type inference keeps small local bindings concise.
The useful annotation is the one that states a boundary a reader would otherwise have to reconstruct.
Keep the annotation on its pattern's final line.
When the pattern expands across lines, attach `: classifier` to its closing delimiter;
a source break alone does not justify a singleton wrapper or force the classifier onto another line.

## Showing the CBPV Boundary

Zydeco's value/computation distinction should remain visible in ordinary code:

| Form | Reading |
| --- | --- |
| `Thk B` | the value type of suspended computations of type `B` |
| `{ M }` | a value that suspends computation `M` |
| `! thunk arguments` | a computation that forces and applies a thunk |
| `Ret A` | the computation type that returns a value of type `A` |
| `ret value` | a computation that returns `value` |
| `do pattern <- M; N` | run `M`, bind its result, then continue with `N` |
| `M { N }` | pass the suspended continuation `N` directly to `M` |
| `fn pattern => M` | the abstraction required by the residual computation type |

These forms also reveal evaluation order.
Mobile block entries contribute types and values to a context; application, `do`, matching,
and relative-monad operations order computations within that context.

Use `@[monadic] term` when a term is intentionally interpreted through a relative monad and its algebra.
Prefer `@[monadic] begin ... end` when explicit delimiters make a multiline translation easier to scan.
Metadata extends across its following term, so parenthesize the annotated term
before applying its result: `(@[monadic] term) argument`.
Ordinary CBPV sequencing reads most directly as `do`.
Use `match` to eliminate data values and `comatch` to construct a codata computation,
with one arm per visible constructor or destructor case.

A named projection uses `/`, while a computation destructor uses `.`.
Projection binds tighter than the undelimited prefixes `!`, `ret`, and constructor introduction,
so a projected operation needs no parentheses at a forcing site:

```zydeco
! int/eq left right
! monad .bind A B computation function
```

Parenthesize the prefix expression when projecting from its result, as in `(! thunk)/field`.

New code uses `Thk` and `Ret`, the names supplied by the builtin package's surface.

## Names

Names should help the reader recognize the static role of an occurrence before consulting its definition.

| Entity | Style | Examples |
| --- | --- | --- |
| Kinds, types, and type constructors | `UpperCamel` | `VType`, `Bool`, `Option`, `Monad` |
| Data constructors | `+UpperCamel` | `+False`, `+Some`, `+Cons` |
| Values, computations, destructors, and value fields | `lower_snake_case` | `read_line`, `fold`, `.bind` |
| Type fields in packages | `UpperCamel` | `= Bool`, `= Option` |
| Files and import binders | `lower_snake_case` | `monad.zy`, `monadic_basis` |

The conventional metavariables carry additional information.
`A`, `A'`, and related names denote value types; `B`, `B'`, and `R` denote computation types.
`M` commonly has kind `(VType) -> (CType)`.
A descriptive name is better when a parameter has a specific semantic role.

A trailing `~` marks a lazy or thunked variant.
A prime distinguishes a closely related formal variant, such as two encodings used in the same argument.
Longer-lived APIs benefit from descriptive words instead of chains of primes.

## Named Components and Packages

Named components use one spelling for the classifier and another for the payload.
`#field :: A` classifies a named payload, while `#field = value` introduces or patterns against it.
The `#` marker appears exactly where a field name stands on the left of `=` or `::`,
the positions where a bare identifier would read as a variable or binder; announced positions carry no marker,
so `term/field` projects, `/field = pattern` searches, and `= field` is the concise field pun when field
and variable share a name.

Parentheses make the extent of a named component explicit:

```zydeco
(#name = value) : (#name :: A)
(#left :: A) * (#right :: B)
```

Package fields follow the naming convention of their payload: type components use `UpperCamel`,
and value components use `lower_snake_case`.
Place an abstract type component before the values whose classifiers mention it.
A manifest existential records a representation equation as part of the interface;
an ordinary existential publishes the abstract identity alone.
Write signature binders with the field pun, as in `exists (= Bool : VType)`
or `exists (= Int as @(intrinsic(int)) : VType)`, so the public field name and the payload binder coincide
and the body refers to the public name directly.
The explicit `exists (#Int = Hidden : VType)` form is reserved for a provider
whose local binder genuinely needs a different name.
The order of these components forms a telescope and remains significant.

Library packages are easiest to use when their components are named.
Small local products may remain positional when their order is evident at the construction and elimination sites.

Use a field projection pattern when a consumer needs selected named fields without restating their product layout.
The punned form `/field` binds the selected payload as `field`; combine several projections
with a semicolon alias group because every projection observes the same bindee:

```zydeco
match tree
| +Leaf() => ret +Leaf()
| +Node(/left; /value; /right) =>
  ...
end
```

Keep a short projection group on one line. When the group exceeds the line width,
use a few lines organized by role, such as foundational classifiers, shared types, and module values.
Avoid giving every projection its own line when several short fields form one readable topic.

Write `/field = local_name` when the local role deserves a different name,
or chain projections to disambiguate a nested field.
Keep an ordinary comma product pattern when the positional structure is itself meaningful and every component is used.
Ordinary term projections traverse transparent named products and stop at an unopened existential package.
At a package boundary, use one projection-pattern group to open the existential telescope once
and bind only the public types and module values the consumer needs:

```zydeco
param (/VType; /Thk; /String; builtin) : @(import("../std/builtin.zy")) in
let (/int; /process) = builtin |> make_std in
...
```

One selection group shares the same Builtin value, and its field search reaches every public name:
the surface's kinds and types, the nested operation groups, and the system capabilities alike.
Place manifest type fields before value modules, and retain the provider's order
when it makes the list easier to compare with the contract.
The surface discloses each fixed representation under its public name,
so the ordinary pun binds it directly, as in `/Int` inside the group.
Use an explicit rename only when the consumer has a clearer role name.
Select Builtin operations as module values and keep individual calls qualified, such as `int/add` and `process/exit`.
A final ordinary pattern such as `builtin` retains the complete package for forwarding,
while the preceding projections introduce only the requested local names.
Omit that alias when the consumer does not forward the package.
This projection-pattern idiom serves the role of package `use` without adding a separate binding form.

The canonical builtin package is the single source of `@[builtin(...)]` host-capability metadata.
Compiler intrinsics, in contrast, are canonical importable terms in their own right:
a source splices `@(intrinsic(int))` directly where the term is needed, while host operations are acquired
by importing the builtin package and projecting only the required dependencies,
which keeps the names subject to ordinary language-level resolution without repeating the complete host interface.

Within that signature, fixed representations use canonical primitive intrinsics such as `@(intrinsic(int))`
and are re-exported through manifest packages whose fields carry the public type names.
Host-type roles are reserved for abstract capability patterns, as in `exists @[builtin(reader)] (Reader : VType) . ...`.
This keeps generative resource identities beside the provider boundary that owns them.
Host-operation roles are accepted only on term classifiers,
where they should annotate the corresponding labeled classifier.

## Layout and Comments

Indent by two spaces and aim for at most 100 characters per line.
A short binding may occupy one line.
For a longer binding, put one parameter on each line and place the residual classifier at the same indentation.
Align `in` or `that` with the keyword that opened the binding.

```zydeco
def fix fold
  (A : VType)
  (B : CType)
  (list : List A)
  (empty : Thk B)
  (step : Thk (A -> Thk B -> B))
: B =
  match list
  | +Nil(_) => ! empty
  | +Cons(head, tail) =>
    ! step head { ! fold A B tail empty step }
  end
that
```

Use spaces around `:`, `=`, `->`, `=>`, and `*`, and after commas.
Constructor and destructor sigils stay attached to their names.
A short thunk such as `{ ret value }` fits naturally on one line;
a multiline thunk receives one additional indentation level.
Align the arms of `match`, `comatch`, `data`, and `codata`, then indent an arm body once.
Multiline tuples and packages align their closing delimiter with the opener and keep separators between items.

`--|` writes Markdown documentation prose.
A contiguous block becomes part of the repository documentation when it appears immediately above a `@[doc]` annotation:

```zydeco
--| Maps a function over every element of a list.
--|
--| The result preserves the input order.
@[doc]
def map = _ in map
```

The [documentation reference](references/language.md#attaching-documentation) explains attachment,
semantic links, and checked examples.
Use the supported `@[doc]` form; section options have no implemented presentation contract.

`--` introduces a local implementation note.
The most useful comments explain purpose, invariants, or a typing choice that remains surprising
after the surrounding code is read.
