# Zydeco Code Style

This guide is for contributors familiar with typed functional programming who may be new to Zydeco.
Zydeco uses one term language for kinds, types, values, and computations,
so a clear program gives the reader enough surface evidence to recognize each term's role.
In particular, the source should reveal where a name is available,
whether its definition remains transparent, and where computation is sequenced.

The conventions below favor Zydeco's direct forms and use annotations where they clarify a type-system boundary.
The [surface syntax principles](syntax.md) explain the language-design choices behind the notation itself.

## The Shape of a Source File

A source file denotes one complete term.
Most programs use an outer `begin ... end` block to gather their dependencies and definitions
before producing a computation or package:

```zydeco
begin
  let std = @[import("../std/std.zy")] _ that
  param (
    (VType, CType, Thk, Ret, Unit, Int, Char, String, OS, api) :
    @[import("../std/builtin.zy")] _
  ) that

  ...
end
```

An import is metadata on a hole.
Each occurrence receives a fresh copy of the imported term,
so binding the result once gives every later use the same lexical identity.
Place imports and parameters near the beginning of the file,
followed by foundational types, operations, and finally the term that the file provides.

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

Here `!` says that `apply` is bound as a thunked computation.
The parameters wrap the body in the appropriate type-level, polymorphic, or value abstractions.
The expanded spelling would repeat those same parameters in a `Thk` classifier and a `fn`;
genbind keeps the source at the level at which the operation is normally read.

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
| `do~ M; N` | run `M`, discard its result, then continue with `N` |
| `fn pattern => M` | the abstraction required by the residual computation type |

These forms also reveal evaluation order. Mobile block entries contribute types and values to a context;
application, `do`, matching, and relative-monad operations order computations within that context.

Use `monadic ... end` when a block is intentionally interpreted through a relative monad and its algebra.
Ordinary CBPV sequencing reads most directly as `do`.
Use `match` to eliminate data values and `comatch` to construct a codata computation,
with one arm per visible constructor or destructor case.

A named projection uses `/`, while a computation destructor uses `.`.
Parentheses make a projected operation pleasant to scan at a forcing site:

```zydeco
! (std/int/eq) left right
! monad .bind A B computation function
```

New code uses `Thk` and `Ret`, the names supplied by the builtin package.
`Thunk`, `U`, and `F` remain available through the standard prelude for sources that use the earlier vocabulary.

## Names

Names should help the reader recognize the static role of an occurrence before consulting its definition.

| Entity | Style | Examples |
| --- | --- | --- |
| Kinds, types, and type constructors | `UpperCamel` | `VType`, `Bool`, `Option`, `Monad` |
| Data constructors | `+UpperCamel` | `+False`, `+Some`, `+Cons` |
| Values, computations, destructors, and value fields | `lower_snake_case` | `read_line`, `fold`, `.bind` |
| Type fields in packages | `UpperCamel` | `Bool = Bool`, `Option = Option` |
| Files and import binders | `lower_snake_case` | `monad.zy`, `monadic_basis` |

The conventional metavariables carry additional information.
`A`, `A'`, and related names denote value types; `B`, `B'`, and `R` denote computation types.
`M` commonly has kind `(VType) -> (CType)`.
A descriptive name is better when a parameter has a specific semantic role.

A trailing `~` marks a lazy or thunked variant. A prime distinguishes a closely related formal variant, such as
two encodings used in the same argument. Longer-lived APIs benefit from descriptive words instead of chains of
primes.

## Named Components and Packages

Named components use one spelling for the classifier and another for the payload.
`field :: A` classifies a named payload, while `field = value` introduces or patterns against it.
When the field and variable have the same name, `= field` is the concise field pun.

Parentheses make the extent of a named component explicit:

```zydeco
(name = value) : (name :: A)
(left :: A) * (right :: B)
```

Package fields follow the naming convention of their payload: type components use `UpperCamel`,
and value components use `lower_snake_case`.
Place an abstract type component before the values whose classifiers mention it.
A manifest existential records a representation equation as part of the interface;
an ordinary existential publishes the abstract identity alone.
The order of these components forms a telescope and remains significant.

Library packages are easiest to use when their components are named.
Small local products may remain positional when their order is evident at the construction and elimination sites.

The canonical builtin package is the single source of `@[intrinsic]` and `@[builtin(...)]` metadata.
Other sources acquire intrinsic kinds, types, and host operations by importing and unpacking that package,
which keeps their names subject to ordinary language-level resolution.

## Layout and Comments

Indent by two spaces and aim for at most 110 characters per line. A short binding may occupy one line.
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
Multiline tuples and packages use trailing commas.

`--|` writes Markdown documentation prose. A contiguous block becomes part of
the repository documentation when it appears immediately above a `@[doc]`
annotation:

```zydeco
--| Maps a function over every element of a list.
--|
--| The result preserves the input order.
@[doc]
def map = _ in map
```

The annotation may carry renderer-specific metadata, such as
`@[doc(section, "collections")]`. Its payload may be any term; documentation
renderers decide how to present the attached term from its syntax and checked
classifier. A blank line or an ordinary comment between the documentation
block and `@[doc]` leaves the prose unattached.

`--` introduces a local implementation note.
The most useful comments explain purpose, invariants, or a typing choice
that remains surprising after the surrounding code is read.
