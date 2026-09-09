# Zydeco language reference

This draft describes the current source language and its execution boundaries.
It assumes basic familiarity with typed lambda calculi, polymorphism, and operational semantics.
The [language guide](../tutorial/zydeco-guide.md) provides a longer introduction;
[CONTRIBUTING](../../CONTRIBUTING.md) covers command-line workflows.

1. [Conventions and the language model](#1-conventions-and-the-language-model)
2. [Lexical structure and syntax](#2-lexical-structure-and-syntax)
3. [Bindings and scope](#3-bindings-and-scope)
4. [Classification and inference](#4-classification-and-inference)
5. [Values, products, and data](#5-values-products-and-data)
6. [Computations and control](#6-computations-and-control)
7. [Patterns and coverage](#7-patterns-and-coverage)
8. [Value functions and views](#8-value-functions-and-views)
9. [Polymorphism and packages](#9-polymorphism-and-packages)
10. [Static elimination](#10-static-elimination)
11. [Relative monads](#11-relative-monads)
12. [Sources, imports, and entry](#12-sources-imports-and-entry)
13. [Primitive values and capabilities](#13-primitive-values-and-capabilities)
14. [Foreign interfaces](#14-foreign-interfaces)
15. [Execution profiles](#15-execution-profiles)

## 1. Conventions and the language model

Zydeco separates values from computations. Values are inert data, including suspended computations;
computations consume continuation stacks and may perform effects.
The surface grammar is shared by kinds, types, values, and computations.
Checking determines the sort.

| Notation | Meaning |
| --- | --- |
| `K` | A kind, classified by the meta-level `Set` |
| `A : VType` | A value type |
| `B : CType` | A computation type, describing a stack protocol |
| `v : A`, `M : B` | A value or computation with its classifier |
| `p` | A binding pattern; a copattern also permits destructor observations |
| `e : S` | A source annotation; `S` is a type or kind |

`Set` has no source term form.
`VType`, `CType`, `Thk`, `Ret`, and primitive type names are ordinary bindings supplied explicitly
by a program, commonly by selecting them from Builtin.
Literal syntax itself determines a primitive type when no expected type is available.

An arrow `A -> B` expects an `A` argument above a residual `B` stack.
`Ret A` expects a continuation receiving an `A`; `Thk B` is a value suspending a computation with protocol `B`.
These protocols do not prescribe physical stack layout or linear use.

Examples marked `zydeco check` are complete source terms.
Other code blocks use the displayed metavariables or an explicitly described context.
A checked term need not be a standalone executable.

## 2. Lexical structure and syntax

Source is UTF-8. Whitespace separates tokens but indentation and line breaks do not determine syntax.
`--` starts a line comment; `/- ... -/` is a nestable block comment.
An uninterrupted `--|` block attaches text to a following `@[doc]` or `@(literal)`;
an intervening blank line or ordinary comment breaks attachment.

Variable names start with an ASCII letter, or with `_` followed by at least one identifier character.
Subsequent characters are ASCII letters, digits, `_`, apostrophe, `?`, `+`, `*`, `-`, `=`, or `~`.
`_` alone is a hole.
A constructor starts with `+` and an uppercase letter; a destructor starts with `.` and a lowercase letter.
`#field` marks a field name wherever it could be confused with a binder.
Keywords are reserved; `def` and `define` are synonymous.

Integer tokens are optionally signed decimal digits.
Floats require a fractional part with digits on both sides of the decimal point,
or a complete exponent: `1.25`, `1e3`, and `-2.5e-2` are valid.
Prefixes and suffixes such as `0x10` and `42u8` are rejected.
Strings use double quotes; characters use single quotes and contain one Unicode scalar value.
Both support `\\`, `\"`, `\'`, `\n`, `\r`, `\t`, `\0`, and `\u{hex}`, with one
to six hexadecimal digits denoting a Unicode scalar.

The main precedence levels, from tightest to loosest, are:

| Forms | Grouping |
| --- | --- |
| Parentheses, braces, delimited terms | Explicit |
| `v/field` | Left |
| `!v`, `ret v`, `+C v` | Prefix |
| Application and `M .d` | Left |
| `A * B * C` | One flat product; explicit nesting is retained |
| `A -> B -> C` | Right |
| `v \|> f` | Left |
| `f <\| v` | Right |
| `pi`, `val pi`, `forall`, `sigma`, `exists` | Binder bodies extend rightward |
| Abstractions, bindings, metadata | Extend over their following term |

Thus `!p/action x` means `(! (p/action)) x`.
The thin arrow `->` constructs a classifier; `=>` separates a function or matching header from its body.
Annotations and named components have a separate grouping layer: `:` is non-associative
and binds more tightly than the right-associative `=` and `::` naming forms.
Expression annotations use parentheses, `(e : S)`, or the contents of a `begin ... end` block.
Write `((#x = 1) : (#x :: Int64))` to annotate the whole named value; `(#x = 1 : Int64)` annotates its payload.
Metadata in an application argument also needs parentheses.

## 3. Bindings and scope

`param p in e` introduces a type or computation abstraction; `param val p in v` introduces a value function.
`let p = e in body` binds a type or value transparently.
`def p = e in body` additionally seals a type definition with a nominal identity.
Binders scope over the tail; ordinary lexical bindings may shadow enclosing names.

A `that` binding contributes to the nearest `begin ... end` block.
Its names are visible throughout that block, and dependencies from bodies and annotations determine its placement.
Source order breaks ties. A dependency on a lexical binder must still be available at the block boundary;
a nested `begin` provides a nearer boundary.

```zydeco check
begin
  let answer = seed that
  param (seed : @(intrinsic(i64))) that
  ret answer
end
```

The example elaborates to a function taking `seed` before binding `answer`.
An unannotated `begin` adds no effect or package boundary.

Binding headers abbreviate abstractions:

| Header | Binding value or type |
| --- | --- |
| `let F (X : K) = T in e` | `let F = fn (X : K) => T in e` |
| `let val f p : A = v in e` | `let f = (val p => v : val pi p . A) in e` |
| `let ! f p : B = M in e` | `let f = ({ fn p => M } : Thk (pi p . B)) in e` |
| `let fix f p : B = M in e` | A thunk containing `fix f => fn p => M` |

Recursive block components must consist of sealed type definitions whose kinds are available before their bodies.
Parameters, values, and transparent aliases cannot form such cycles.
Runtime recursion is explicit through `fix`; refutable binding patterns follow §7.

## 4. Classification and inference

Kinds are `VType`, `CType`, kind arrows, and named kinds `#field :: K`.
Type functions use `fn (X : K) => T` and ordinary application.
Type application reduces by substitution; transparent aliases and manifest package equations participate in equality.
Products retain order, arity, and explicit nesting; labels retain their names.

A nominal `def` keeps its identity distinct from its implementation and other definitions.
Typing may expose a sealed data or codata shape to introduce or eliminate it without equating distinct seals.
Repeated uses of one definition share its identity; copying a term freshens its bound nominal definitions.
Import sharing is specified in §12.

Checking is bidirectional. Annotations supply expected classifiers; synthesis determines them from a term.
A bare unannotated variable in a synthesizing binder position defaults to a value binder with a flexible value type.
Body uses and compatible calls within the same inference region constrain that type.
Inference is order-independent within the region and closes at a block or source boundary,
where every flexible classifier introduced by that region must be solved.
There is no automatic let-polymorphic generalization.

```zydeco check
begin
  let identity = { fn x => ret x } that
  ! identity ()
end
```

Without a constraining use or annotation, the parameter is unconstrained:

```zydeco reject=tyck.unconstrained-inference at=1:4
fn x => ret x
```

Variables, unit, named patterns, and ordinary tuple patterns can synthesize; explicit annotations take precedence.
Refinement can expose product, value-to-computation arrow, thunk, and return shapes.
It does not guess constructor ownership, nominal definitions, or existential telescopes.
Solutions must satisfy occurs and witness-scope checks.
Constructors and package openings generally require an expected type; type and kind binders remain annotation-directed.
Sources close their inference independently before an import-site expectation is compared (§12).

`@[typeof] e` reuses the synthesized classifier of `e` as a static term:

| Operand | Query result |
| --- | --- |
| `1` | `Int64` |
| `ret 1` | `Ret Int64` |
| `{ ret 1 }` | `Thk (Ret Int64)` |
| `Int64` | `VType` |
| `Ret` | `VType -> CType` |

The annotation takes no arguments; `@[typeof()] e` is equivalent.
The operand receives no expected classifier from the query context, although an ascription inside it applies normally.
Thus `@[typeof] (1 : Int8)` yields `Int8`, and a bare constructor still needs an owner annotation.
`@(typeof)` has an unannotated hole as its operand and cannot synthesize.
A kind operand is rejected with `tyck.typeof-kind` because its classifier is `Set`.

The query preserves seals, witnesses, and local inference variables; it creates no inference boundary or generalization.
An extracted classifier must remain in the scope of its witnesses (§9),
including witnesses in later inference solutions.
The operand is checked, including name resolution and coverage, but never executes.
Its imports remain source dependencies (§12).

Value-let staging still applies: the complete value may be queried,
while a runtime value binding cannot have a static body.

```zydeco check
@[typeof] (let value = 1 in value)
```

```zydeco reject=tyck.sort-mismatch at=1:1
let value = 1 in @[typeof] value
```

A query inside an established runtime context can still introduce a static alias.
The [formal calculus](../../lang/statics/type-system.typ) provides the mathematical companion to these judgments.

## 5. Values, products, and data

Values include variables, literals, `()`, products, named values, constructor applications,
thunks, existential packages, and total value functions.
`() : Unit`; `(v)` is grouping; `(v1, ..., vn)` introduces a product or,
under an expected existential, a witness-prefixed package.
`A * B * C` has three components and differs from `A * (B * C)`.

`#field = v` introduces a named payload of type `#field :: A` when `v : A`.
`= x` puns `#x = x` in terms and patterns.
Named types work analogously: `(#item = T) : (#item :: K)`.
Named computations have no introduction form; store one as a thunk.

```zydeco check
let point = (#x = 3, #y = 4) in
ret (point/x, point/y)
```

A data type `data | +C1 : A1 ... | +Cn : An end` classifies constructor values `+Ci vi`,
where `vi : Ai`. Each constructor has one payload, which may itself be a product or constructor.
Names are interpreted against the expected data type.
Name and parameterize a data type with an ordinary definition.

```zydeco check
let Unit = @(intrinsic(unit)) in
def Flag = data | +On : Unit | +Off : Unit end in
match (+On() : Flag)
| +On() => ret 1
| +Off() => ret 0
end
```

## 6. Computations and control

The basic computation forms are:

| Form | Typing and behavior |
| --- | --- |
| `{ M }` | A value of `Thk B` when `M : B`; captures lexical bindings |
| `!v` | Runs `v : Thk B` against the current `B` stack |
| `ret v` | A computation of `Ret A`; delivers `v : A` to the return continuation |
| `do p <- M; N` | Runs `M : Ret A`, binds its return with `p`, then runs `N` |
| `fn p => M` | Consumes an argument, binds it with `p`, and continues with `M` |
| `M v` | Supplies an argument to a computation function |
| `fix f => M` | Binds `f` to a thunk of the recursive computation; `f` must have a thunk type |
| `match v \| p => M ... end` | Runs the body of the first matching arm |
| `M .d` | Observes destructor `.d` of a codata computation |

A computation type `codata | .d1 : B1 ... | .dn : Bn end` describes alternative observations.
`comatch | .d1 => M1 ... end` supplies the corresponding computations.
An observation can expose an arrow or another codata protocol, so copatterns may interleave argument patterns
and destructor names, as in `.route argument .result`.

```zydeco check
let Ret = @(intrinsic(ret)) in
let Int64 = @(intrinsic(i64)) in
def Probe = codata | .read : Ret Int64 end in
(comatch | .read => ret 42 end : Probe).read
```

For irrefutable `p`, the basic reductions are:

```text
!{M}                         → M
(fn p => M) v                → M with p bound to v
do p <- ret v; M             → M with p bound to v
(comatch | .d => M ... end).d → M
fix f => M                   → M with f bound to {fix f => M}
```

Value formation does not execute suspended bodies.
A thunk is not memoized: repeated forces execute its body again.
`do` determines effect order; duplicating a returned value does not repeat its producing computation.
Matching selects the first successful arm; partial binders may terminate execution as specified in §7.

`OS` is the host's root protocol. An `OS` computation transfers to another `OS` computation,
terminates, aborts, or diverges; it has no ordinary source return.
An explicit successor `Thk OS` is suspended code and need not be a captured machine continuation.

## 7. Patterns and coverage

Variables and holes match any input.
Unit, products, named wrappers, and existential openings are irrefutable when their components are.
A constructor pattern is irrefutable only for a single-constructor data type with an irrefutable payload.
Integer literals check against an expected primitive integer type and must fit its representation.
They are always refutable and select an arm by integer equality; matching remains a computation.
Float, string, and character literal patterns are rejected.

Ordinary parameters and `let`, `def`, `do`, and `param` binders require irrefutable patterns.
`@[partial]` permits failure in the annotated computation binding or function header:

```zydeco check
@[partial] let 0 = 0 in ret ()
```

```zydeco reject=tyck.refutable-binding at=1:5
let 0 = 0 in ret ()
```

The annotation covers that construct's own binders, including the parameters in one function header.
It does not propagate into bodies, binding tails, or nested functions, and cannot annotate an entire block.
Value functions and value-producing bindings remain total even under `@[partial]`.
A failed partial binder terminates execution unsuccessfully.

`(p; q; ...)` applies each member to the same original input, extending scope left to right.
The group requires at least two members and can nest wherever a pattern is accepted.
It neither constructs a product nor passes one member's result to the next.
General aliases require an expected value type and irrefutable members, even under `@[partial]`.
Type and kind pattern aliases are rejected.
Direct field-projection groups additionally support the shared package opening in §9.

```zydeco check
let ((left, right); whole) = (1, 2) in ret whole
```

```zydeco reject=tyck.refutable-pattern-alias at=1:16
@[partial] let (0; whole) = 0 in ret whole
```

Matches must be exhaustive over their typed input, including correlations between nested patterns.
Integer literal arms require a catch-all even for a finite-width integer type.
Views contribute coverage only through an irrefutable result pattern;
the checker does not prove properties of a view's image.
Generalized comatches must cover arguments and destructors along every observation path.
Duplicate integer arms are not currently rejected as redundant.

## 8. Value functions and views

`val (x : A) => v` introduces a total value transformation.
If `v : A'`, its classifier is `val pi (x : A) . A'`.
Type parameters express polymorphism within this value-function space.
Value binders may also open package witnesses used in the result type; arbitrary runtime values cannot index types.

```zydeco check
let val duplicate (x : @(intrinsic(i64))) = (x, x) in
ret (4 |> duplicate)
```

`f v`, `v |> f`, and `f <| v` are the same value application.
Functions can be partially applied, passed to other value functions, returned,
and stored in intermediate structures, subject to §10.
They may capture runtime values, but cannot execute computations to form their result.
A thunk in the result keeps its computation suspended.

A view pattern `f ~> p` applies the total value function `f` to its input, then matches `p` against the result.
Only `p` introduces bindings.
The source head is a variable, optionally with bracketed type arguments, as in `f[T] ~> p`.
Its result pattern may be refutable in a match or partial computation binding.

```zydeco check
let Int64 = @(intrinsic(i64)) in
let val first ((x, _) : Int64 * Int64) = x in
let (first ~> x; whole) = (3, 4) in
ret (x, whole)
```

Views associate to the right: `f ~> g ~> p` transforms with `f`, then `g`, before matching `p`.
For complete, well-typed total values, the usual capture-avoiding equations explain application and binding:

```text
V |> (val p => W)                 = let p = V in W
let x = V in x                   = V
let x = V in W                   = W                     (x not free in W)
let y = (let x = V in W) in U     = let x = V in let y = W in U
val x => f x                     = f                     (x not free in f)
let f ~> p = V in W               = let p = (V |> f) in W
```

The reassociation requires `x` fresh for `U`; binders may be renamed to satisfy it.
The binding equations use irrefutable patterns.
Type abstraction/application has the corresponding beta and eta equations.
These equations concern value transformations; execution still requires the bounded static elimination in §10.
An explicitly authored thunk is the runtime callable form when an implementation must remain dynamically selectable.

## 9. Polymorphism and packages

`forall (X : K) . B` classifies computation polymorphism, introduced by `fn (X : K) => M`
and eliminated by type application.
Type arguments erase.
At computation-type level, `pi` uses the binder's sort: a type binder yields a universal computation,
and a value binder yields a computation arrow, possibly dependent on opened package witnesses.
With a kind body, `pi (X : K) . L` forms the kind arrow `K -> L`.
`sigma` similarly yields an existential for a type binder or a product for a value binder.
These forms do not provide general dependence on runtime values or quantification over kinds.

Dependency is judged after elaboration: `pi (value : Int64) . (@[typeof] ret value)` forms `Int64 -> Ret Int64`.
The same erasure permits classifier queries in `val pi`, `sigma`, and kind-arrow bodies.

An existential `exists (X : K) . A` hides a type witness used by its payload.
The telescope may mix abstract witnesses, manifest equations `(X as T : K)`,
and manifest kind entries such as `(VType as @(intrinsic(vtype)))`.
Later entries may refer to earlier ones. Naming and punning compose with these binder forms.
Static and value entries may be interleaved through nested existentials and products;
package formation does not require all existentials to precede all products.
A manifest binder may omit its classifier when its definition synthesizes it, including a kind classified by `Set`.
An explicit classifier checks that same definition.

Against an expected existential, `(T, v)` supplies the witness and payload.
`pack` synthesizes a package type from explicit evidence:

```zydeco check
let Int64 = @(intrinsic(i64)) in
let package =
  pack (= Item as Int64 : @(intrinsic(vtype)))
  where #value = 42 end
in
let (/Item; /value) = package in
ret (value : Item)
```

A manifest entry discloses its witness with `as`; an abstract entry uses `(X : K) is T`.
The payload must synthesize its type. Sealing abstracts occurrences of a recoverable witness identity;
it does not infer an arbitrary abstract interface from concrete values.
For example, `pack (X : VType) is Int64 where 42 end` synthesizes `exists (X : VType) . Int64`.
Use an expected existential annotation when its abstract payload interface must be prescribed.
`pack` currently introduces type witnesses, not kind witnesses.

Opening an abstract package introduces fresh witnesses whose scope includes its payload bindings.
Those witnesses cannot escape an ordinary binding's result type.
A package-dependent function binder can retain them in its result classifier: `pi ((X, x) : exists (X : K) . A) . B`.
Application requires corresponding witness evidence available in the caller's static scope.
The runtime implementation may be an unknown thunk; its signature carries the dependency.
Opening a manifest entry substitutes the disclosed equation and creates no fresh witness.
Introduction checks the supplied witness against that equation.
Manifest entries erase and contribute no abstract witnesses to a package-dependent arrow.
Disclosed equations substitute transparent provider-local names;
normalization cannot recover an equation hidden by sealing.
At computation application, witness instantiation currently traverses a leading existential prefix;
it does not recover abstract components beneath preceding value products.
An ordinary value `sigma` introduces no witness telescope, so witnesses opened by its pattern must be absent
from the resulting component type, even when mentioned only through `typeof`.

Field selection `v/field` recursively searches named wrappers, product components,
and package telescopes for exactly one matching public name.
Functions, thunks, and data payloads are opacity boundaries.
Explicit named fields and public punned binder names, including manifest kind entries, share this search namespace.
Missing and ambiguous names are different errors, and an explicit path performs a new search at each slash.
Manifest packages are transparent; selecting through an abstract package requires a projection pattern.
Its hidden fields still count when checking ambiguity.

```zydeco reject=tyck.missing-named-field at=1:15
(#value = 42)/missing
```

`/field = p` selects and binds the payload; `/field` puns that binding.
Payload patterns are irrefutable and may carry annotations.
Projection patterns associate to the right: `/outer = /inner = p` selects `outer`, then `inner` from its payload.
A group such as `(/Item; /value; whole)` opens each selected package occurrence once,
so related fields share witnesses and `whole` can forward the same package.
Type projection selects the payload of a named kind.
Module factories and explicit dictionaries use these ordinary package and function forms.

## 10. Static elimination

Before execution, value-function applications, views, and static package structure must reduce
to a representable residual program.
Reduction follows lexical bindings, type and value application, known constructors,
projections, and package introduction/opening.
It preserves runtime value sharing and the order and multiplicity of effects.
It never runs a computation, force, or general recursion to discover static information.

A library may export an unapplied value function.
A residual runtime value cannot contain one:

```zydeco check
val (x : @(intrinsic(unit))) => x
```

```zydeco reject=tyck.static-elimination at=1:6
ret (val (x : @(intrinsic(unit))) => x)
```

Explicit computation thunks and representable package payloads may remain.
Types, witnesses, labels, and resolved static routes erase; erasure does not reveal an abstract representation.
Putting a value function inside a thunk does not exempt the thunk's residual body from the rule.

Typed value and computation holes can be inspected during checking.
Execution and lowering reject any hole reachable in the residual values, thunks, or computations before effects occur.
Holes confined to eliminated static definitions do not block execution; wildcard patterns are not runtime holes.

The current reducer bounds a request to 128 nested applications and 65,536 applications in total, including views.
Exceeding a bound during residualization reports `tyck.static-elimination`.
An unfinished witness inspection supplies no evidence to dependent checking.
These are implementation resource limits, not a termination theorem.
Optional backend simplification does not relax this source acceptance boundary.

## 11. Relative monads

A relative monad has carrier `M : VType -> CType`.
The library's `Monad M` is codata with operations `.return : forall (A : VType) . A -> M A`
and `.bind : forall (A : VType) (B : VType) . Thk (M A) -> Thk (A -> M B) -> M B`.
An `Algebra M R` is the computation type `forall (A : VType) . Thk (M A) -> Thk (A -> R) -> R`.
These are ordinary library interfaces; their types do not enforce algebraic laws.

`@[monadic] e` selects the lexically visible `Monad` and `Algebra` constructors and performs algebra translation.
It supplies an outer carrier parameter followed by a thunked monad implementation.
Source `Ret`, `ret`, and `do` are reinterpreted through that carrier and its operations.
Parameters outside the annotation remain outside; translated parameters inside follow the carrier and instance.
Polymorphic translation may require additional structure arguments describing how a type supports the translation.

```zydeco check
param (/Ret; /Thk; builtin) : @(import("../../lib/std/builtin.zy")) in
let basis = @(import("../../lib/std/control/monad.zy")) in
let (/Monad; /Algebra) = builtin |> basis in
let mo : Thk (Monad Ret) = {
  comatch
  | .return A value => ret value
  | .bind A B computation continuation =>
    do value <- ! computation;
    ! continuation value
  end
} in
(@[monadic] begin do x <- ret 1; ret x end) Ret mo
```

Lexical type bindings, including existential witnesses, remain available.
Bindings introduced inside the block are translated with it.
Free term references require definitions that translation can reinterpret;
arbitrary captured runtime values are rejected.
The [control library](../../lib/std/control) supplies State and Exception examples.
Library-encoded delimited control does not imply primitive capture or duplication of the native machine stack.

## 12. Sources, imports, and entry

A source file contains one complete term and contributes no surrounding context.
`@(import("path"))` imports a term, with relative paths resolved from the importing file.
After imports and any companion annotation are assembled, each source synthesizes its classifier in an empty context.
An importer's expected type is compared afterward and cannot solve inference variables inside the provider.

Repeated imports share the checked source root and its nominal identities.
Imported computations still execute at every dynamic occurrence.
Free names and mobile bindings cannot cross a source boundary.
Import cycles are rejected.

A `foo.zy` implementation may have an independently checked `foo.zyi` type companion.
The pair behaves as an annotation of the implementation by that type.
Companions can import other sources and be imported themselves; discovery does not apply to `.zydeco` roots.
There is no implicit prelude, authored project manifest, distinguished `main`, or separate-compilation interface.

`@[typeof] @(import("library.zy"))` extracts the provider's complete classifier.
For a builder, querying a particular result requires applying the builder in the operand.
A companion may query another acyclic source; querying its own implementation creates a rejected import cycle.
Classifier queries couple a signature to the inspected implementation; use an explicit public signature
when its contract should remain stable across implementation changes.

`check` accepts kinds, types, values, and computations.
`run` and `build` require a computation accepting the host Builtin package and ending in its `OS` protocol:

```zydeco check
param (/stdio; /process) : @(import("../../lib/std/builtin.zy")) in
! stdio/write_line "hello, world!" { ! process/exit 0 }
```

The REPL stores complete terms as numbered inputs; `@(import(1))` imports input 1,
while `@(import("1"))` addresses a file.
Its root commands are `@[type] e`, `@[run] e`, `@(help)`, and `@(quit)`.
By default it inspects kinds/types and evaluates values or directly returning computations.
Explicit execution can supply Builtin.
REPL evaluation captures output and uses empty stdin and arguments.

## 13. Primitive values and capabilities

[Builtin](../../lib/std/builtin.zy) exposes canonical kinds and fixed-representation types as manifest fields.
Repeating an intrinsic splice denotes the same canonical kind or type across independently checked sources.
`Reader`, `Writer`, and `OS` are abstract provider capabilities sharing one opening.
The `numeric`, `text`, and `system` groups contain host operations.
The [standard library](../../lib/std/README.md) assembles ordinary package functions and defines `Bool`,
`Option`, `Result`, and `List`; host operations select continuations instead of constructing those types.

| Family | Source behavior |
| --- | --- |
| `Int8/16/32/64`, `UInt8/16/32/64` | Exact signed/unsigned widths; arithmetic wraps at the chosen width |
| `Float32`, `Float64` | IEEE 754 arithmetic at the chosen width |
| `Char` | One Unicode scalar, excluding surrogates |
| `String` | Immutable valid UTF-8; indexed operations count Unicode scalars |
| `Bytes` | Immutable octets; lengths and positions count bytes |

Integer and float literals default to `Int64` and `Float64`.
An expected primitive type selects another width; integers must fit and floats round to that width.
Finite-range overflow is rejected, while float underflow may round to zero.
Existing numeric values have no implicit cross-width conversion.
Integer division or remainder by zero terminates unsuccessfully; signed minimum divided
by `-1` wraps, with remainder zero.
Float rendering uses the selected width's Rust Display spelling, including signed zero, `inf`, `-inf`, and `NaN`.

`String` indices are scalar positions, not byte offsets or grapheme clusters; `byte_length` observes UTF-8 bytes.
`Bytes` equality compares contents and ordering is lexicographic.
`bytes/slice buffer start length` uses a start and length; an empty window at the end is valid.
Negative or out-of-range positions, invalid Unicode scalars, and invalid UTF-8 select failure branches.
The public library reifies these as `Option` or `Result`.

Every scalar Builtin module provides `to_le_bytes : Thk (A -> Ret Bytes)`
and `from_le_bytes : Thk (forall (R : CType) . Bytes -> Thk R -> Thk (A -> R) -> R)`.
They preserve exact little-endian bits and reject a decoder input whose length differs from the scalar width;
float conversion preserves NaN payloads instead of performing arithmetic.
`bytes/aligned R buffer alignment no yes` requests equal contents with positive power-of-two address alignment.
Invalid requests and detected reservation failures select `no`; success supplies a buffer through `yes`.
On interpreter and native C borrowing paths, the visible buffer address satisfies the requested alignment.
A successful call may share the original allocation or copy; it does not mutate the input.
Other byte operations do not promise to preserve address alignment.
[C14](compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls) describes target allocation limits.

The ordinary [memory library](../../lib/std/memory/package.zy) composes these primitives
into explicit storage contracts with abstract stored types.
Its [layout laws](../proposals/bytes.md#layout-laws) own size, field placement, and padding;
ordinary value typing and calling conventions do not change.

I/O uses blocking byte streams and opaque reader/writer handles.
Copying a handle aliases the same resource; closing it invalidates all aliases, whose later operations report `Closed`.
Reserved standard streams are not closed by ordinary close operations.
Fallible effects use explicit `OS` continuations with structured errors.
A positive-length byte read at EOF returns empty; line reads distinguish EOF from an empty line.
Line reading removes `\n` and a preceding `\r`.
Filesystem helpers use UTF-8 paths, and write/create versus append behavior is explicit in the API.
The [library guide](../../lib/std/README.md) is the operation inventory.

## 14. Foreign interfaces

A foreign implementation is an annotated hole:

```zydeco check
param val (/Thk; /Ret; /Bytes; /UInt64) : @(import("../../lib/std/builtin.zy")) in
(@(ffi(c, library("xxhash"), symbol("XXH3_64bits"))) : Thk (Bytes -> Ret UInt64))
```

The supported classifier is `Thk (A1 -> ... -> An -> Ret UInt64)`, including zero arguments.
Each `UInt64` contributes one C `uint64_t`; each `Bytes` contributes a `const void *` and `size_t`, in source order.
At most six flattened C arguments are accepted.
The `uint64_t` result preserves all bits when re-encoded.

A byte argument lends its visible contiguous window for the duration of the call.
The callee must neither modify nor retain the pointer, and must not dereference it for a zero-length window.
The declaration author is responsible for the actual symbol's signature and these borrowing obligations.
A returning call must use the C return protocol; unwinding, nonlocal jumps, and reentry into Zydeco are unsupported.
`Ret` does not imply purity or termination.

Checking validates the declared classifier without loading a library or inspecting headers.
The Unix interpreter loads symbols lazily; native AMD64 links the named library.
Missing libraries or symbols fail at loading/linking.
Wasm and the ZASM interpreter reject native imports.
Callbacks, C-to-Zydeco exports, other scalar/result types, and larger signatures are outside this subset.

## 15. Execution profiles

The execution paths share source checking and static elimination, but have different allocation and host boundaries.
The native layout is an implementation ABI, not source-level control over addresses or object layout.

| Profile | Control and memory | Host boundary |
| --- | --- | --- |
| Interpreter | Explicit evaluator state; Rust-owned values and environments | CLI/REPL I/O; returning C imports on Unix |
| Native AMD64 | Machine control stack; growable retained environments; two fixed 1 MiB copying semispaces | Supplied runtime, Linux/macOS toolchain, returning C imports |
| `wasm-am` | Trampoline over ZASM; fixed 1 MiB operand/control stack; growing non-collecting heap | Imports from a `zydeco` embedding |
| `wasm-sps` | Block trampoline and persistent stack frames; growing non-collecting heap | The same host operation contract |

Native managed live values must fit in one semispace, including headers.
Host-owned strings and byte buffers have a separate lifetime from managed products and closures.
Environment growth and managed-heap capacity are distinct limits.
Native tail transfers reclaim dead activations, but live continuations and escaping values can retain storage;
Wasm trampolines avoid growth of the host call stack without guaranteeing constant heap use.

Static value-function application expands residual bodies and can increase code size.
Product and thunk allocation depends on optimization.
Interpreter byte slicing shares a window; native slicing currently copies.
Repeated concatenation can be quadratic.
Immutability guarantees observations, not identical costs on every backend.

The supplied Node host is a test embedding: randomness is deterministic,
and its process argument fold rejects two or more arguments.
Native FFI requires installed libraries, and the CLI does not execute Wasm modules.
There are no source raw pointers, layout annotations, manual allocation, or primitive concurrency interfaces.
Runtime-managed capabilities provide the current resource boundary.

## Metadata index

Metadata is `@[meta] e`; `@(meta)` abbreviates a hole payload.
A metadata expression is a name, string, integer, or a named application with comma-separated arguments.

| Metadata | Meaning and valid use |
| --- | --- |
| `import(path-or-number)` | Replace a hole with an independently checked source term (§12) |
| `intrinsic(role)` | Replace a hole with a canonical kind/type; roles include `vtype`, `ctype`, `thk`, `ret`, `unit`, `i8`…`i64`, `u8`…`u64`, `f32`, `f64`, `char`, `string`, `bytes` |
| `builtin(role)` | Mark a host capability or operation in a typed package contract (§13) |
| `ffi(c, library("name"), symbol("name"))` | Supply a foreign thunk implementation at a hole (§14) |
| `typeof` | Extract a synthesized classifier (§4); no arguments |
| `monadic` | Algebra translation under the lexical basis (§11); no arguments |
| `partial` | Permit the annotated computation header's binders to fail (§7); no arguments |
| `literal` | Replace a hole with its attached text block as a string; no arguments |
| `doc` | Attach Markdown to a term, binding, or member |
| `format(options...)` | Scope formatting options to a payload |
| `debug` | Record a checked term for compiler observation |

The REPL commands in §12 are frontend interpretations of root metadata.
Unrecognized metadata is structurally accepted and has no defined semantic effect here.
Documentation and debug metadata may carry additional metadata arguments.
Formatting options include `width`, `indent`, `layout`, `parentheses`, and `verbatim`;
their workflow and values are in [CONTRIBUTING](../../CONTRIBUTING.md#format-and-lint).

## Diagnostic index

| Diagnostic family | Relevant rule |
| --- | --- |
| Missing annotation/solution, unconstrained inference, occurs check | Classification and inference (§4) |
| Invalid binding cycle, missing seal | Bindings and nominal recursion (§3–§4) |
| Sort/kind/type mismatch, unknown constructor/destructor | Classification and introduction/elimination (§4–§6) |
| Refutable binding/alias/projection, coverage, overlapping copatterns | Patterns and coverage (§7) |
| Missing/ambiguous/sealed field, unavailable/escaping witnesses | Package scope and selection (§9) |
| Static elimination | Residual representation and reduction limits (§10) |
| Integer/float literal range errors | Primitive representation (§13) |

Rejection examples name stable `tyck.*` codes and a source position.
Diagnostic wording and rendering are implementation details.
