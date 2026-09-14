# Zydeco language reference

This reference describes the current source language and its execution boundaries.
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
| `! v`, `ret v`, `+C v` | Prefix |
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

A binding form determines transparency or nominal identity;
its `in` or `that` connective determines placement and scope.
These choices are independent: moving a definition before or after a use does not turn a transparent alias
into a nominal type.
`let` supports transparent package composition, while `def` establishes an identity that clients can share
without equating it with its implementation.

A `that` binding contributes to the nearest `begin ... end` block.
Its names are visible throughout that block, and dependencies from bodies and annotations determine its placement.
Source order breaks ties. Lexical binder identities are resolved before dependency scheduling;
reordering must preserve capture, shadowing, and nominal identity.
A dependency on a lexical binder must still be available at the block boundary;
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
| `! v` | Runs `v : Thk B` against the current `B` stack |
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
! {M}                         → M
(fn p => M) v                 → M with p bound to v
do p <- ret v; M              → M with p bound to v
(comatch | .d => M ... end).d → M
fix f => M                    → M with f bound to {fix f => M}
```

Value formation does not execute suspended bodies.
A thunk is not memoized: repeated forces execute its body again.
`do` determines effect order; duplicating a returned value does not repeat its producing computation.
Matching selects the first successful arm; partial binders may terminate execution as specified in §7.

`OS` is the host's root protocol. An `OS` computation transfers to another `OS` computation,
terminates, aborts, or diverges; it has no ordinary source return.
An explicit successor `Thk OS` is suspended code and need not be a captured machine continuation.

### Ret and explicit CPS

The public API convention is `Ret A` for observably pure calculations
and explicit continuation-passing style (CPS) for effectful operations.
The programmer is responsible for keeping public `Ret` operations free of hidden effects,
including effects from captured computations and foreign code.
This convention is not enforced by the checker; `Ret` alone is not purity evidence for compiler optimizations.

In an explicit CPS interface, the caller's `R : CType` describes the remaining computation protocol.
Use `Thk R` for completion without a result and `Thk (A -> R)` when delivering an `A`;
failure can have its own continuation.
Both styles coexist, and CPS supplies no single-use or cleanup guarantee.
The [C FFI](#14-foreign-interfaces) keeps the native return protocol beneath these public interfaces.

### Ret and stack extent

**`Ret A` describes an installed continuation accepting an `A`, not a stack-frame marker.**
It specifies the return-address protocol at the point of use.
`ret v` delivers `v` to that continuation, and `do p <- M; N` installs a continuation
that binds `p` and resumes `N` against its saved residual stack.
The continuation hides that residual stack's extent.

A CBPV computation can push a runtime-dependent, unbounded number of arguments before transferring control;
the number need not be predictable from its source syntax or a `Ret` occurrence.
Consequently, `Ret` establishes neither a fixed frame size nor a boundary for stack allocation,
scanning, or reclamation.
A compiler may record a known argument prefix and the continuation's accepted value protocol,
but must keep unknown or recursive stack structure explicit.
Physical frame layout and lifetime require separate backend evidence.

## 7. Patterns and coverage

Variables and holes match any input.
Unit, products, named wrappers, and existential openings are irrefutable when their components are.
A constructor pattern is irrefutable only for a single-constructor data type with an irrefutable payload.
Integer literals check against an expected primitive integer type and must fit its representation.
They are always refutable and select an arm by integer equality.
A match may produce a value or a computation under the rules in §8.
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

`match` also produces a value when every arm produces a value of the same type.
It uses the patterns and exhaustive coverage of §7; computation-producing arms keep their usual meaning.
The four integer value intrinsics below have classifier `val pi (left : Int64) (right : Int64) . Int64`,
where `Int64` denotes `@(intrinsic(i64))`:

| Intrinsic | Result |
| --- | --- |
| `i64_add` | Addition wrapping modulo 2⁶⁴ |
| `i64_sub` | Subtraction wrapping modulo 2⁶⁴ |
| `i64_and` | Bitwise conjunction |
| `i64_compare` | Signed comparison: −1, 0, or 1 |

```zydeco check
let Int64 = @(intrinsic(i64)) in
let add = @(intrinsic(i64_add)) in
let compare = @(intrinsic(i64_compare)) in
let val maximum (left : Int64) (right : Int64) : Int64 =
  match compare left right | -1 => right | _ => left end
in
ret (maximum 16 (add 5 11))
```

These total leaves allow checked arithmetic and layout construction in ordinary source functions.
They obey §10's static requirements; returning numeric computations remain available for runtime inputs.

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
V |> (val p => W)               = let p = V in W
let x = V in x                  = V
let x = V in W                  = W                     (x not free in W)
let y = (let x = V in W) in U   = let x = V in let y = W in U
val x => f x                    = f                     (x not free in f)
let f ~> p = V in W             = let p = (V |> f) in W
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

### Module interfaces and shared openings

A module interface exposes the names a client needs without requiring the client
to unpack the provider's entire product layout.
Recursive field search permits grouping operations for organization; explicit paths resolve ambiguity.
The opacity boundaries above prevent selection from forcing a thunk or inspecting an arbitrary data payload.
Changing grouping is compatible only where existing selections remain unique and preserve their witness evidence.

An abstract carrier and the operations consuming it must share the witness from one package opening.
Projection groups and whole-package aliases preserve that opening for forwarding through a composition root.
Independently opened abstract packages remain distinct even when their fields have identical names or layouts.
This relationship uses existential elimination and ordinary bindings; it introduces no separate module object.

An inferred `pack` interface is useful when its witnesses and payload already express the intended contract.
An explicit annotation, including a source companion (§12), prescribes the interface when details must be hidden.
`typeof` can reuse an inferred contract but couples it to the inspected implementation.
No companion is required merely because a term is used as a module.

Value functions compose modules statically (§8).
A provider selected at runtime instead uses an ordinary thunk or codata protocol;
a package-dependent computation arrow carries any witness dependency needed by its result.
Explicit adapters can expose curried universal interfaces when that is the desired public protocol.

A consumer can open a provider inside a scoped existential elimination and pass its carrier
and operations to a polymorphic callback.
The hidden identity stays within that elimination and cannot escape through its result.
The callback's classifier supplies neither purity nor single invocation nor a native continuation lifetime.
Remaining questions concern [kind-witness introduction](../ideas/kind-witness-introduction.md),
[computation witness routes](../ideas/computation-witness-routes.md),
and [companion generation](../ideas/companion-interface-generation.md).

## 10. Static elimination

Before execution, value-function applications, views, and static package structure must reduce
to a representable residual program.
Reduction follows lexical bindings, type and value application, known constructors, projections,
package introduction/opening, value matches, and the integer value operations in §8.
It preserves runtime value sharing and the order and multiplicity of effects.
It never runs a computation, force, or general recursion to discover static information.

Integer value operations require known integer operands.
Value matches select the first matching arm without reducing unselected arm bodies.
Every tag or literal needed to decide that choice must be known;
the reducer cannot skip an undecidable earlier row to choose a later catch-all.
Irrefutable patterns can forward unknown runtime payloads inside known structure.
An unresolved integer operand or value-match choice reports `tyck.static-elimination` at a source site.
Ordinary computation matches can still inspect runtime data.

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
Import cycles are rejected. A source is a complete term, rather than an implicit namespace around its contents.
Sharing one imported root preserves its declarations; distinct authored definitions keep distinct identities.
Closing provider inference before comparing import-site expectations prevents importer order
from choosing the meaning of a shared library.

A `foo.zy` implementation may have an independently checked `foo.zyi` type companion.
The pair behaves as an annotation of the implementation by that type.
Companions can import other sources and be imported themselves; discovery does not apply to `.zydeco` roots.
There is no implicit prelude, distinguished `main`, or separate-compilation interface.
A whole file is already a source package; annotations can also register named terms as described below.

`@[typeof] @(import("library.zy"))` extracts the provider's complete classifier.
For a builder, querying a particular result requires applying the builder in the operand.
A companion may query another acyclic source; querying its own implementation creates a rejected import cycle.
Classifier queries couple a signature to the inspected implementation; use an explicit public signature
when its contract should remain stable across implementation changes.

`check` accepts kinds, types, values, and computations.
`run` and executable builds require a computation accepting the host Builtin package and ending in its `OS` protocol:

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

### Source packages

Packages name source terms for reuse, execution, and testing.
Prefer complete files as library and binary entry points.
A file is already an unnamed library package; a root meta annotation can give it a name and role:

```zydeco
-- library.zy
@[package(library, name(example/math))]
(#answer = 42)
```

The first argument chooses a role:

| Role | Independently emitted boundary |
| --- | --- |
| `library` | Source interface only; its implementation is incorporated into consumers. |
| `library(c, export(...), ...)` | Explicit [C exports](#compiled-libraries-and-c-exports). |
| `binary` | Executable Builtin/OS boundary. |
| `test` or `test(of(...))` | The same executable boundary, with test associations. |

A compilation unit is an independently selected source term with a complete external entry contract.
Binary and test roles already establish that contract; a compiled library declares its own.
There is no additional `unit` annotation or package argument.
Names identify independently reusable artifacts, but naming alone supplies no machine entry point.
The [compiler reference](compiler.md#compilation-unit-preparation-and-artifacts) explains how the designs
of compilation units, FFI, and package management evolve together.

A test is itself a package: `test(of(example/math))` optionally identifies its subjects,
while plain `test` needs no subject.
Subsequent arguments are an optional `name(id)` and typed relationships, such as `test(example/smoke)`.
Source libraries may expose any source classifier.
`check` additionally validates C exports for compiled libraries and the executable Builtin/OS contract
for binaries and tests.

#### Names and project catalogs

Package names are unquoted identifiers, optionally qualified with `/`: `std`, `std/data`, `std/data/smoke`.
Each segment begins with an ASCII letter or underscore and continues with ASCII letters,
digits, underscores, or hyphens; a lone underscore is not a name.
Qualification organizes names without implying term projection, a directory, inheritance, or a code dependency.
Names are unique across all declarations in one selected catalog, regardless of role or nesting.

A catalog is the declarations in root files plus their declared discovery matches.
Before a source operation, the CLI detects `package.zy` and `workspace.zy` in the current working directory.
Both files contribute when present; neither overrides the other.
Prefer `package.zy` for a complete package entry point and `workspace.zy` for shared declarations and discovery.
Both are ordinary source files; `workspace.zy` introduces no additional package kind.
These are two exact filename checks, with no ancestor search, search beside a source argument,
or implicit traversal into subdirectories.
Each root's discovery patterns are relative to that root file.
All selected declarations are indexed before resolving names, so file order does not affect lookup.
Duplicate names are errors with both declaration locations; repeated selection of the same file is harmless.
If no roots are selected, the catalog is empty and file paths remain usable.
There is no built-in registry or filesystem fallback for unknown names.
An invalid root or discovery match fails preparation before checking, output, or execution.
Formatting, compiler-pass inspection, help, and documentation workers do not prepare catalogs;
workers use the bindings captured by their requesting operation.

Names and paths have distinct source spellings:

```zydeco
@(import(example/math))
@(import("library.zy"))
```

A bare identifier resolves in the selected catalog.
A quoted nonempty path selects the complete file, relative to the referencing file.
It never selects an inner field or annotation; `#` and NUL are rejected in paths.
The earlier `file#name` address spelling is removed.
A name declared on the file root and a direct path to that file identify the same source entry.
Code imports accept any package role, subject to ordinary typing, and never automatically apply a factory.

CLI source arguments use the same distinction without depending on which files exist:
absolute paths, spellings beginning with `.`, and paths ending in `.zy`, `.zyi`,
or `.zydeco` are files; other spellings are package names.
Use `./file` for an extensionless file. For example:

```sh
zydeco check -p example/math
zydeco run -p example/hello
zydeco check library.zy
```

The repeatable option `-p NAME`, also spelled `--pkg NAME` or `--package NAME`, selects declared packages by name.
Place it after `show`, `check`, `test`, `run`, or `build`.
It accepts exact package names, not file paths or globs, and never adds discovery roots.
The source commands require either one positional source or one or more package options; the forms cannot be mixed.
`check`, `test`, and `build` support multiple selected packages, with repeated entries deduplicated
by source identity in first-selection order.
`run` and `build --execute` require exactly one distinct entry.
All requested names resolve before an operation begins; an unknown name is an error, never a file fallback.
`show` without package options lists all declarations; with them, it lists only the selected entries.

#### Concluding files and exact term selection

Declarations can remain on their implementation files or be collected in one or a few concluding files.
For example, a file can register imported terms without changing the implementations:

```zydeco
(
  @[package(library, name(example/math))] @(import("math.zy")),
  { @[package(binary, name(example/hello))] @(import("main.zy")) },
  { @[package(test(of(example/math)), name(example/smoke))] @(import("smoke.zy")) }
)
```

The tuple and thunks have their ordinary meanings.
Package names come exclusively from meta annotations; renaming fields or local bindings leaves them unchanged.
A nested declaration requires a name; a file-root declaration does not.
The name option occurs at most once and is not a relationship.
Registering an import describes that annotated term, not the imported file:
the registration and the implementation remain distinct entries and test subjects.

A named entry selects exactly the annotated term, including its payload.
Required imports, parameters, types, and other meta annotations must be inside that term.
Selection parses the containing file but checks only the selected term and its code dependencies.
Unrelated terms need not check; syntax and malformed package annotations still reject the file.
A file-root annotation retains the complete file term, including transparent surrounding wrappers.

In ordinary compilation, package annotations preserve lexical scope, inference, and runtime structure.
They do not create source boundaries or defer execution.
Selecting an annotated term independently requires it to synthesize under an empty context, just like a file import.
A term using enclosing bindings can therefore be valid locally but invalid as an independent entry.
Prefer a complete file when it supplies that context.

Graph identity is the canonical path and selected root term.
Repeated imports of that root share checking and source inputs; imported computations still execute at each dynamic use.
A `.zyi` companion applies to the complete file only, never to an arbitrary nested entry.

#### Bounded discovery

Test-side associations let tests join a suite without editing the library.
A file-root `discover` annotation declares the candidate files explicitly:

```zydeco
-- workspace.zy
@[discover(include("library.zy", "main.zy", "tests/**/*.zy"), exclude("tests/fixtures/**"))]
()
```

A matching test can declare its subject entirely from its own file:

```zydeco
@[package(test(of(example/math)))]
param (/process) : @(import("builtin.zy")) in
let math = @(import(example/math)) in
! process/exit 0
```

The association selects a suite; the import supplies code.
Neither implies the other.
Plain `@[package(test)]` remains independently runnable and is not assigned to every library in the catalog.

Include and exclude calls each accept one or more quoted globs.
Rules run in source order, with the last matching rule winning; they never remove the root file's declarations.
Patterns are relative to their declaring file: `*` matches within a component,
`?` matches one character, and whole-component `**` matches zero or more components.
Literal filenames are valid. Leading `../` steps choose a fixed ancestor before matching.
Parent steps elsewhere, absolute patterns, backslashes, `#`, brackets, and braces are rejected.
Only `.zy`, `.zyi`, and `.zydeco` files are candidates; missing matches select nothing.

Catalog preparation expands only the roots' rules, never rules in matched files or code dependencies.
In the CLI, those roots are `package.zy` and `workspace.zy` in the working directory;
passing another file to `check`, `test`, `run`, `build`, or `doc` does not make it a discovery root.
Each include starts at its literal directory prefix; traversal depth is bounded unless it uses `**`.
Exact filenames require no enumeration, excludes never initiate scans,
and subtree excludes ending in `/**` prune directories before enumeration.
Symbolic links are not followed. Resolving leading parent steps does not enumerate ancestors.
Broad recursive patterns explicitly opt into broad traversal.

Each catalog preparation reads directory membership afresh, including matching editor overlays.
Source text follows the compiler-session snapshot and disk-refresh contract.
Unreadable directories or invalid matched sources reject preparation.
The prepared catalog is reused for name resolution, test planning, checking, and later materialization.
Compilation validates annotations but never expands discovery.

#### Relationships and operations

Relationships are typed associations, kept separate from the code graph:

| Kind | Meaning |
| --- | --- |
| `code` | Inferred from ordinary imports; needed to check or use the term |
| `test(target)` | Select a companion test when testing the declaring package |
| `of(subject, ...)` under `test(...)` | Select this test when testing an exact subject in the catalog |
| Other valid names | Preserve for inspection; no behavior is guessed |

Targets are package names or quoted whole-file paths.
Trailing relationship calls take exactly one target; `of` accepts one or more subjects.
`test()` also means plain `test`.
`of` is allowed only under the test role, and identical repeated relationships are errors.
Explicit `code(...)` is rejected because imports already determine code dependencies.
A library's test association and a test's import of the library do not form a code cycle:
only imports and companion signatures participate in cycle checking.

`test SOURCE` selects forward test targets and catalog tests whose `of` names that exact source entry.
A requested test also selects itself. Every selected target must have the test role.
Planning resolves all catalog test subjects and rejects unknown names;
unknown relationship kinds on the requested package also fail before execution.
It does not recursively activate associations on code dependencies or selected tests.
Within each selected package, targets are deduplicated by source identity and ordered by source/name.
With repeated `-p` selections, suites are combined in selection order and each test runs once per backend,
even when several selected packages share it.
All requested packages and selected executables are checked before any test runs.
Tests use empty stdin and arguments.
[Execution selection](#selecting-an-execution-backend) defines backend preparation, ordering, and results.

`show` lists the project catalog's declarations, locations, roles, direct imports,
and relationships without checking code or loading relationship targets.
A file without declarations appears as one library.
`check SOURCE` checks the selected package and its code dependencies, without following test associations;
compiled libraries, binaries, and tests also require their declared boundary, for names and file paths alike.
`show`, `check`, `test`, `run`, `build`, `doc`, and `repl` use the same automatically prepared project catalog.
Named `run` targets require the binary role.
`build` accepts binaries, tests, and compiled libraries, with a target appropriate to their boundary.
Direct file execution retains the executable-script convention;
a named source library alone is not an independent build target.
Named build artifacts replace namespace separators with dots, so `tools/hello` produces `tools.hello.sps.wasm`;
distinct valid names remain distinct filenames.
See the [package workflow](../../CONTRIBUTING.md#use-source-packages) for commands.

Local checkouts and copied sources provide distribution without an additional hosted service.
Remote fetching, source dependency lockfiles, package versions and source compatibility policies,
and programmable relationship handlers remain deferred.
Compiled artifacts already have [manifest compatibility checks](compiler.md#compilation-unit-preparation-and-artifacts).

## 13. Primitive values and capabilities

[Builtin](../../lib/std/builtin.zy) exposes canonical kinds and fixed-representation types as manifest fields.
Repeating an intrinsic splice denotes the same canonical kind or type across independently checked sources.
`Addr`, `Access`, `Buffer`, `Reader`, `Writer`, and `OS` are abstract provider capabilities sharing one opening.
`Addr` and `Access` support [checked memory and views](#checked-memory-capabilities);
their kinds remain `VType`, and the view constructors add no compiler type forms.
Pure libraries can name canonical carriers without requiring a numeric or resource provider merely to name a type.
Resource operations instead share their provider's abstract opening,
so a composition root must forward the capability and its operations together.
The `numeric`, `text`, and `system` groups contain host operations.
The [standard library](../../lib/std/README.md) assembles ordinary package functions and defines `Bool`, `Option`,
`Result`, `List`, and abstract `Bytes`; host operations select continuations instead of constructing those types.

The memory interfaces build on those capabilities using ordinary packages and computation protocols:

| Question | Owning section |
| --- | --- |
| Which allocation may an address access? | [Checked memory capabilities](#checked-memory-capabilities) |
| How is a handle stored and interpreted? | [Cells and views](#cells-describe-storage-views-interpret-handles) |
| What makes a byte sequence immutable? | [Immutable owners](#immutable-owners-and-source-bytes) |
| How are destinations allocated, written, and closed? | [Mutable buffers](#mutable-destination-capabilities) |
| How are logical values encoded with explicit placement? | [Storage contracts](#explicit-storage-contracts) |
| How do separately checked workers share a stored carrier? | [Stored calls](#stored-call-interfaces) |

| Family | Source behavior |
| --- | --- |
| `Int8/16/32/64`, `UInt8/16/32/64` | Exact signed/unsigned widths; arithmetic wraps at the chosen width |
| `Float32`, `Float64` | IEEE 754 arithmetic at the chosen width |
| `Char` | One Unicode scalar, excluding surrogates |
| `String` | Immutable valid UTF-8; indexed operations count Unicode scalars |

Integer and float literals default to `Int64` and `Float64`.
An expected primitive type selects another width; integers must fit and floats round to that width.
Finite-range overflow is rejected, while float underflow may round to zero.
Existing numeric values have no implicit cross-width conversion.
Integer division or remainder by zero terminates unsuccessfully; signed minimum divided
by `-1` wraps, with remainder zero.
Float rendering uses the selected width's Rust Display spelling, including signed zero, `inf`, `-inf`, and `NaN`.

### Text and byte sequences

`String` indices are scalar positions, not byte offsets or grapheme clusters; `byte_length` observes UTF-8 bytes.
The source-defined `Bytes` type contains immutable octets; equality compares contents and ordering is lexicographic.
Negative or out-of-range positions, invalid Unicode scalars, and invalid UTF-8 select failure branches.
The public library reifies these as `Option` or `Result`.
The [immutable byte contract](#immutable-owners-and-source-bytes) below explains ownership and library operations.

Every scalar Builtin module provides checked `store_le` and `load_le` operations over `Access` and `Addr`.
They preserve exact little-endian bits; float loads and stores preserve NaN payloads without arithmetic.
The [source codecs](../../lib/std/numeric/codecs.zy) provide `to_le_bytes` and `from_le_bytes`,
and std includes them in its numeric modules.
Decoders require the exact scalar width.

### Streams and process arguments

`args/at : Thk (forall (R : CType) . Int64 -> Thk R -> Thk (String -> R) -> R)` looks up a zero-based argument
in the invocation's stable sequence, excluding the executable name.
Negative and out-of-range indices select the first continuation; valid indices supply the string to the second.
Lookup neither advances nor consumes the sequence.
The [argument library](../../lib/std/system/arguments.zy) builds a lazy right fold in ordinary CBPV.
Its item computation receives a reusable `Thk R`: discarding it skips the suffix,
and forcing it repeatedly reruns that suffix computation, including its effects.
The standard library's `process/arg_list` uses this fold to build its own `List String`.

I/O uses blocking byte streams and opaque reader/writer handles.
Copying a handle aliases the same resource; closing it invalidates all aliases, whose later operations report `Closed`.
Reserved standard streams are not closed by ordinary close operations.
Fallible effects use explicit `OS` continuations with structured errors.
A positive-length byte read at EOF returns empty; line reads distinguish EOF from an empty line.
Line reading removes `\n` and a preceding `\r`.
Filesystem helpers use UTF-8 paths, and write/create versus append behavior is explicit in the API.
The [library guide](../../lib/std/README.md) is the operation inventory.

Immutable bytes are data; readers and writers grant access to resources whose observations can change.
Their `OS` protocols make that distinction explicit without requiring linear ownership of handles.
The host reports an error kind and display message, while the source library constructs its own algebraic result.
Whole-file helpers centralize open/operate/close sequencing and error precedence.
`io` owns shared streams, `fs` supplies file capabilities, and `stdio` supplies reserved process streams;
process control is separate.
A UTF-8 `Path` prevents text/path interchange but cannot represent every native path.
Growable writers, seeking, and asynchronous protocols remain in the [stream proposal](../proposals/filesystem.md).

### Checked memory capabilities

A pointer to a record, a pointer paired with a length, and a pointer whose length lives just
before its payload share memory operations.
Their differences are source-defined representation choices: which value crosses a boundary,
where its runtime metadata lives, and how that information is obtained.
Length and capacity are examples of runtime metadata; so are strides, tags, allocator handles, and vtable pointers.
There is no fixed compiler record of optional fields.

The [source library](../../lib/std/memory/views.zy) implements these types and constructors
with the existing `VType` and `CType` kinds.
The [native provider](../../lib/std/memory/native.zy) binds them to checked host memory;
[examples](../../lib/tests/std/memory-views.zy) run on the interpreter, AMD64, and both WebAssembly backends.
The [bounded model](../../lib/tests/ffi/views/model.zy) supplies a deterministic alternative provider
for [source composition and phase tests](../../lang/tests/tests/memory_views.rs).
The WebAssembly host models addresses in its own virtual address space and exposes no C pointer.
The source `Bytes` and `Storage` interfaces use this same memory provider and its retained immutable-owner transition.

#### The primitive boundary

The Builtin provider exposes two abstract value types:

| Type | Meaning | Runtime responsibility |
| --- | --- | --- |
| `Addr : VType` | An opaque data address. It carries no element type, length, capacity, ownership, or permission. | The implemented native address cell occupies one 8-byte pointer slot. Copying an address does not keep its allocation alive; the explicit readable-window transport supplies one C pointer. |
| `Access : VType` | Authority to access a live allocation or granted range with particular permissions. | The checked implementation identifies an owned allocation and a range grant, checks liveness and bounds, and rejects invalid operations. |

`Access` is separate from `Addr`, so a thin external handle can remain one pointer.
A source wrapper can retain both when it should own or retain the resource.
Mutable grants come from `Buffer` owners; frozen grants retain immutable allocations.
The current provider grants access only to its owned storage; foreign-owned grants remain unsupported.
A length read from an arbitrary address cannot grant authority to read that address or its surrounding allocation.
Revocation invalidates every alias of a grant; copying the handle does not duplicate ownership or release rights.
The checked runtime record stores a shared live/revoked grant, an allocation identity
and range, and read/write permissions.
The buffer owner controls allocation lifetime.
Those fields belong to the provider's abstract grant representation, not to every raw pointer.

The provider takes an explicit `Access` on every memory operation.
It checks the addressed range, required alignment, initialization and leaf representation before exposing a value.
Offsetting checks signed displacement without overflow and stays within the granted allocation,
including its one-past address; a subsequent nonempty load rejects one-past access.
A negative offset is therefore valid when the grant includes the header before the payload.
Loading an address from a pointer slot checks that slot; accessing its target requires a suitable target grant.
Addresses remain pointer values through native loads and stores; generic integer casts
or byte codecs do not manufacture pointer validity or a foreign grant.

The source read interface is:

```zydeco
let Memory (R : CType) = codata
| .check : Access -> Addr -> Int64 -> Int64 -> Thk (Fault -> R) -> Thk R -> R
| .load_u8 : Access -> Addr -> Thk (Fault -> R) -> Thk (UInt8 -> R) -> R
| .offset : Access -> Addr -> Int64 -> Thk (Fault -> R) -> Thk (Addr -> R) -> R
| .load_i64 : Access -> Addr -> Thk (Fault -> R) -> Thk (Int64 -> R) -> R
| .load_addr : Access -> Addr -> Thk (Fault -> R) -> Thk (Addr -> R) -> R
end in
let Mem (A : VType) (R : CType) =
  Thk (Memory R) -> Thk (Fault -> R) -> Thk (A -> R) -> R in
...
```

`Mem A R` abbreviates a computation supplied with a memory provider and failure/success continuations.
Its answer protocol `R` belongs to the caller.
It does not promise purity, termination, or one invocation.
The provider must validate a primitive access before performing it and reporting success.
The faults are ordinary source constructors `Closed`, `Bounds`, `Permission`, `Overflow`,
`Alignment`, `Uninitialized`, `InvalidValue`, `Unavailable`, and `AllocationFailed`.
The integer model implements only a bounded read space and uses `Bounds` for displacements outside that space.
The native provider checks initialization for every loaded leaf.
Its `check` operation validates the complete footprint and alignment without reading padding
or requiring padding bytes to be initialized.

The native module also exports `allocate`, `close`, `freeze`, `immutable_length`, `grant`, `base`,
`revoke`, and the typed operations `store_i64`, `store_u8`, and `store_addr`.
Allocation returns a `Buffer` owner and starts uninitialized; the existing `buffer/allocate` remains zero-initializing.
`grant R owner offset length permission no yes` creates a range grant
with ordinary source permissions `Read`, `Write`, or `ReadWrite`.
A grant does not embed itself in an address. `base` obtains the grant's first address,
and `revoke` invalidates every copy of that grant without closing the allocation or independent grants.
Closing invalidates all mutable grants. Freezing transfers the allocation to an immutable owner,
invalidates the old grants, and returns a new retained grant as specified below.
Address identity is preserved across that transfer; using an address still requires a valid grant.
Failed writes leave bytes, initialization information, and pointer slots unchanged.
Freezing an incompletely initialized buffer reports `Uninitialized` and preserves the live owner.

Integer and byte stores initialize their footprint.
Pointer stores additionally record the target's allocation identity and offset
and write the native pointer into the slot.
Any overlapping byte or scalar store removes that pointer information, even when it writes identical bits.
Loading such a slot as an address reports `InvalidValue`;
loading a properly stored address preserves the target identity, including after that target has closed.
A later target access checks its own grant and reports `Closed`.
The shared [runtime model](../../lang/machine/src/memory.rs) owns these checks for the interpreter and AMD64.
Generic runtime address values are opaque handles into that model;
an explicit address cell has the separate 8-byte native storage representation.
No new kind or compiler rule recognizes thin, fat, or header views.

This is a checked capability design.
It makes no claim that current typing proves pointer lifetimes or that all checks erase.
Static region retirement and transitive support remain unimplemented;
their separate proposal is [reachability regions](../proposals/reachability-regions.typ).
`Ret A` remains an installed continuation accepting `A`; it supplies neither a memory lifetime nor cleanup scope.

### Cells describe storage; views interpret handles

`Cell A` describes a fixed memory representation of an `A` and a computation that reads it:

```zydeco
let Cell (A : VType) =
    (#size :: Int64)
  * (#alignment :: Int64)
  * (#read :: Thk (forall (R : CType) . Access -> Addr -> Mem A R)) in
let Fat (RuntimeMetadata : VType) =
  (#address :: Addr) * (#runtime_metadata :: RuntimeMetadata) in
let View (Handle : VType) (RuntimeMetadata : VType) =
  (#open :: Thk (forall (R : CType) . Access -> Handle -> Mem (Addr * RuntimeMetadata) R)) in
...
```

The three questions have different answers: `Handle` is the value being passed,
`Cell Handle` describes its explicit stored form, and `open` obtains a payload address
and runtime metadata from that handle.
A view descriptor is an ordinary reusable dictionary; it need not be stored inside each handle.
Opening a logical handle requires no physical `Cell Handle`; the caller supplies a cell separately when storing it.
`Fat M` permits any representable `M`.
Its fields are logical source fields until a `Cell (Fat M)` or call adapter supplies physical placement.
A source product alone does not promise adjacent native words.

Cell construction follows the [layout laws](#layout-laws): nonnegative size,
power-of-two alignment, checked rounding, and checked addition.
The complete cell size includes tail padding and is its array-element stride.
Product construction places the second cell at `round_up(left.size, right.alignment)`
and rounds the complete size to the larger alignment.
The library reuses the existing [size value functions](../../lib/std/memory/size.zy) for those calculations.
Before reading any field, a product validates its complete size and alignment through `Memory.check`.
It then reads fields through their cells, leaving padding uninterpreted.
`padding count` constructs a `Cell Unit` with size `count` and alignment one;
its read checks the footprint and returns unit without loading bytes.
`align A boundary cell` raises alignment to the larger of `boundary` and the cell's current alignment,
rounds its size accordingly, and preserves every field offset.
It validates that new footprint before delegating to the original cell.
Thus a small field cannot make an out-of-bounds padded or misaligned enclosing cell succeed.
All three constructors return `Result (Cell A) LayoutError` using source value calculations.
The read interface does not grant mutation permission; native stores require a writable access grant.

The public `Cell A` dictionary type does not prove its size, alignment, and decoder agree.
Caller-authored cells must satisfy those laws; checked builders can hide successful layouts behind package abstraction.
All cells for `A` share `Cell A`; this is not a type index distinguishing their placements.
The supplied native leaves are `UInt8` (size/alignment 1), `Int64` (8),
and an address cell (8) for the current 64-bit native target.
The integer model uses that same sample format.
Additional primitive formats can extend the provider without changing the view constructors.
Native address cells cannot be obtained by serializing an integer through `Bytes`:
the existing portable byte contract has a different carrier and validity boundary.

### Concrete view forms

The source constructors use the following representations.
`p` is the supplied handle address, and offsets are byte displacements checked by the provider.

| Form | `Handle` | `RuntimeMetadata` | `open` behavior |
| --- | --- | --- | --- |
| Thin | `Addr` | `Unit` | Return `(p, ())`; no memory operation. |
| Fat length | `Fat Int64` | `Int64` | Project `(handle/address, handle/runtime_metadata)`; no memory operation. |
| Fat length and capacity | `Fat ((#length :: Int64) * (#capacity :: Int64))` | The named pair | Project the carried record; no memory operation. |
| Prefix header | `Addr` | Any `M` with a `Cell M` | Read `M` at `p - header_delta`, return payload `p`. |
| Inline header | `Addr` | Any `M` with a `Cell M` | Read `M` at `p`, return payload `p + payload_offset`. |
| Object header | `Addr` | `Addr`, for a vtable slot | Read the slot at `p`, return the original object address and the vtable address. |

The last three are applications of one source constructor:

```zydeco
indirect M runtime_cell runtime_offset data_offset
```

It offsets to the runtime metadata, reads it through `runtime_cell`, offsets to the payload,
and invokes success only after these operations succeed.
A runtime metadata cell can itself be a product; following more
than one indirection is another ordinary `open` computation.
The caller supplies the original allocation grant, so prefix recovery does not attempt
to validate itself using the header it is about to read.

On the model's chosen 64-bit format, a thin handle occupies 8 bytes,
a fat length handle 16, and a fat length/capacity handle 24.
A prefix or inline-header handle still occupies 8 bytes; the runtime metadata resides in the referenced allocation.
Header size, payload alignment, and the position of an embedded pointer are choices of the source cell plan.
The object view loads a data address from a checked slot; it does not load or invoke a callable code value.
[Code-pointer and external-ABI bindings](../proposals/c-ffi.md#external-handle-conventions) remain proposed.

### Typed pointers, slices, and immutable bytes

The typed operation layer receives a cell for the element it accesses:

```text
read_at : forall A R. Cell A -> Access -> Addr -> Mem A R
index  : forall H A R. View H Int64 -> Cell A -> Access -> H -> Int64 -> Mem A R
```

`read_at` validates the selected cell's complete footprint and then delegates to its reader.
`index` opens the handle, checks `0 <= index < length`, checks multiplication by the element stride for overflow,
offsets within the grant, and reads through the element cell.
Thus a slice length counts elements, and its stride comes from `Cell A`.
For byte slices the element is `UInt8` with stride one.
Runtime checks and numeric arithmetic are computations; the length does not become a dependent integer index such
as `Slice A n`.

The source factories `pointer A carrier element` and `slice H A carrier view element` export an abstract `Ptr`
or `Slice` together with operations specialized to the chosen element cell.
The pointer factory provides `from_address`, `address_of`, `pointer_cell`, and `get`;
the slice factory provides `from_handle`, `handle_of`, `slice_cell`, and indexed `get`.
Their expected existential signatures hide the selected handle representation
while sharing its witness with the returned operations.
For example, `let (= Slice, slices) = views/slice H A carrier view element in ...` opens the slice factory once,
and `slices/get` receives that opening's `Slice`.
Constructing a wrapper only preserves the handle; access still validates the grant when `get` runs.
This binds the chosen representation to an API without a compiler builtin for `Slice`.
For a concrete instance, `Handle` may be `Addr`, `Fat Int64`, or a retained pair containing an owner.
Clients that need to select different handle types dynamically package the handle
with its matching operations: `exists (= H : VType) . H * View H Int64 * Cell A`.
Clients sharing one `H` can select a view at runtime directly.

Capacity has a separate meaning from length. A growable container's source API validates `0 <= length <= capacity`,
manages initialized elements, and supplies a writable grant for mutations.
Copying its runtime metadata proves none of those facts and does not authorize a write.
The same separation supports runtime strides, allocator records, and application-specific tags.

#### Compile-time and runtime behavior

The existing [value-function](#8-value-functions-and-views)
and [static-elimination](#10-static-elimination) rules remain authoritative.
The following table applies those rules to memory views:

| Expression or information | During checking | At runtime |
| --- | --- | --- |
| `Addr`, `Access`, `M`, and `H` | Check ordinary kinds, types, and package witnesses. | Types, witnesses, and field labels erase; their values remain as needed. |
| Fixed cell size, alignment, and product offsets | Value arithmetic requires known operands and checks its ordinary error result. | A retained descriptor may carry those calculated integers and read thunks. |
| Fat-handle construction and runtime metadata projection | A value function may forward unknown runtime fields inside known structure. | The residual program constructs or projects ordinary values; it contains no value-function closure. |
| Thin/fat `open` | Check the suspended computation; never force it to discover static information. | Invoke success with carried fields, without accessing memory. |
| Header recovery and element indexing | Check types and operation protocols. Runtime lengths cannot drive static arithmetic. | Execute checked offsets, loads, and numeric computations through the supplied provider. |
| Runtime-selected view or cell | Check that the selected values have a common type, or open an existential package. | Keep required dictionaries, offsets, and captured values. Selection does not make their integers statically known. |
| Explicit native ABI layout | Require a known target leaf layout and argument/result transport plan. | Apply the validated marshalling plan to runtime payloads. |

Runtime metadata names the information's role in a representation, not a requirement that it be unknown during checking.
A literal length may fold away while still describing that representation's runtime metadata.
Conversely, ordinary layout descriptions may be constructed and selected at runtime.
Neither is a [meta annotation](#meta-annotations-compile-time-metadata).

An `open` result is a snapshot of the observations its computation made.
It neither freezes the referenced allocation nor promises an atomic snapshot of a mutable multifield header.
Shared mutable runtime metadata needs its own synchronization protocol, and later accesses recheck their grants.

### Immutable owners and source Bytes

A byte sequence has immutable octet contents.
Sharing storage cannot create a mutation channel. Foreign borrowing can also observe an address;
equality of contents promises neither pointer identity nor the same allocation behavior.
This permits sharing or copying a slice while preserving source content observations.
Start-and-length windows match explicit pointer-and-length borrowing, and `UInt8` makes singleton construction total.
Library-defined options and booleans stay outside the host ABI.

[`text/bytes.zy`](../../lib/std/text/bytes.zy) defines `Bytes` as an ordinary abstract std type.
Its private representation is either an allocation-free empty value
or a retained immutable `Access` paired with `Fat Int64`.
The fat handle carries the visible address and byte count.
Length, indexing, slicing, comparisons, singleton construction, concatenation, and copying are source algorithms.
The compiler has no `Bytes` intrinsic, byte-sequence operation roles, or special byte foreign classifier.

`freeze R owner no yes` checks that the entire allocation is initialized before changing ownership.
Success transfers the same allocation to retained immutable storage and returns a new read grant.
Every old `Buffer` alias and mutable-owner grant becomes closed.
The new grant cannot be revoked, used for writes, or used to obtain a mutable owner.
Its address identity and physical alignment are preserved.
Failed freeze leaves the original owner, grants, contents, and initialization state unchanged.
The runtime arena retains frozen allocations for its lifetime; per-value reclamation remains open.

A read-only grant over mutable storage is insufficient for `Bytes`: other grants may still write or close it.
`from_immutable R access no yes` checks the immutable-owner state before constructing a byte value.
`build R count alignment fill no yes` allocates a private owner, gives `fill` a writable range,
and freezes after `fill` invokes its completion.
Failure closes the private mutable owner.
A retained writable alias therefore fails after a successful build.
Completion is reusable at the type level; repeated completion encounters the checked closed state
and does not recreate mutation authority.

`slice` takes a start and length, checks its bounds, and shares the immutable owner without copying its contents.
An empty window at the end is valid; negative or out-of-range windows select failure.
`copy_to` validates the full destination extent and write permission before its first write.
`aligned` allocates with the requested alignment and copies the visible bytes before freezing.
It requires positive power-of-two alignment; invalid requests and detected reservation failures select failure.
`with_window R value no yes` exposes retained access, visible address, and count to a source adapter;
an allocation-free empty value obtains a valid empty immutable allocation when a window is requested.
A zero-length window grants no readable octet and need not have a null address.

The [byte package signature](../../lib/std/text/bytes.type.zy) shares one abstract `Bytes` witness
with all its operations.
A composition root passes that package to text, system, buffer, layout, and codec builders;
independently opened byte packages cannot exchange their abstract values without an explicit conversion.
The assembled std package exports that same type with its convenient `Option` and `Bool` operations.

Scalar leaves exchange checked memory through `store_le` and `load_le`, preserving exact bits;
source codecs provide `to_le_bytes` and `from_le_bytes` with exact-width validation.
UTF-8 conversion and primitive I/O exchange immutable grants or explicit readable windows.
These boundary operations need no knowledge of the source byte representation.
The [C adapter](#storage-and-foreign-transport) consumes an explicit readable window as one pointer;
a separate integer argument supplies a C length when the binding requires it.

Contiguous storage gives constant-time indexing and avoids flattening before each foreign borrow.
Shared windows also make decomposition cheap, but a small retained window can keep a large parent alive.
Slices share retained immutable allocations on every backend.
The runtime arena retains these allocations for its lifetime;
dropping a source window does not currently reclaim its owner.
The [cost table](../../lib/std/README.md#byte-operation-costs) makes these target differences explicit.

### Mutable destination capabilities

Fixed-capacity destination storage extends the representation boundary with an explicit resource protocol.
The host-owned `Buffer` capability supplies allocation identity.
The [source buffer interface](../../lib/std/memory/buffer.zy) composes the general memory provider
and one shared byte package.
Its convenience operations run in `OS`;
the underlying memory operations accept the caller's answer protocol `R : CType`.
A source integer or immutable `Bytes` cannot stand in for a buffer handle.

`allocate size alignment error success` creates zero-initialized storage
with the requested nonnegative size and positive power-of-two alignment.
`write handle offset bytes error done` replaces a checked range without resizing.
`read handle offset length error success` returns a detached immutable snapshot of that range.
An empty range at the end is valid.
Negative or overflowing ranges are rejected before any byte is changed.

`freeze handle error success` produces aligned immutable bytes and closes the handle on success.
`close handle error done` frees mutable storage without producing bytes.
Both transitions invalidate every alias.
Closed handles are never reused, and read, write, freeze, or close through an old alias report `Closed`.
A failed freeze leaves the handle open.
Snapshots and frozen bytes remain immutable after later writes or close.
These are resource-state guarantees, not a static uniqueness or lexical-lifetime claim.

The stable error codes are `InvalidLayout = 0`, `Closed = 1`, `Bounds = 2`,
`AllocationFailed = 3`, and `Uninitialized = 4`.
The last applies to buffers created by the uninitialized memory allocator described above.
Operations on a closed handle report `Closed` before inspecting their range.
Detected allocation and layout failures create no returned handle; range and capability rejections precede copying.
General host allocator aborts remain outside this fallible protocol, as for immutable storage.
Native/interpreter buffers use real aligned allocations; the Wasm host retains its opaque-address limitation.
Reads copy a detached snapshot; freeze transfers the initialized allocation under the immutable-owner rule above.

#### Choosing an allocator on the computation stack

[allocation.zy](../../lib/std/memory/allocation.zy) defines an ordinary codata `Allocator`
with an `.allocate` observation.
`Allocate A` is a computation accepting that service, an error continuation, and a result continuation.
`allocate size alignment : Allocate Buffer` requests storage from the supplied service rather
than selecting an allocator inside the compiler.
The heap provider delegates to the source zeroing buffer allocator;
the `limited maximum parent` value function intercepts requests larger
than its per-allocation ceiling and delegates the rest.
This is a size policy, not a cumulative quota or a distinct physical allocator.
A negative ceiling rejects every nonnegative request.

Consumers may supply other source-defined services without changing the host ABI or the layout language.
This makes allocator choice explicit at participating call sites; it does not prevent a program
with Builtin access from calling the heap operation directly.
[The checked example](../../lib/tests/std/buffer.zy) exercises the service, a restrictive provider,
alias invalidation, detached snapshots, and failure without mutation on all backends.

Lexically scoped borrowing and automatic cleanup are deferred.
A thunk may be retained, invoked twice, or invoke its completion continuation zero or multiple times.
A scope-shaped helper cannot derive single invocation or cleanup from these types.
The current interface provides checked close and freeze.
Stronger lifetime protocols remain in the [memory proposal](../proposals/bytes.md#remaining-questions).

### Explicit storage contracts

A `Bytes` value alone does not say where fields live, how many bytes a scalar occupies,
or which alignment a borrowed address satisfies.
An ordinary Zydeco product also leaves its physical layout to the compiler.
The implemented memory libraries supply an explicit representation boundary:
a logical type `A` has a source-authored layout, which can be realized into a storage contract.
The concrete stored payload is one contiguous immutable buffer, without retaining the original logical product.
The surrounding runtime value is an ordinary source value retaining its immutable memory grant.

#### Descriptions, computations, and abstract storage

The [runtime builder](../../lib/std/memory/package.zy) has an abstract `Layout A`.
It constructs layouts with ordinary total value functions: `product A B left right`,
`padding count`, and `align A boundary layout`.
Their bodies construct thunks; applying these value functions does not execute numeric arithmetic during type checking.
Forcing a layout through `realize A R layout no yes` calculates
and validates its layout information using ordinary returning computations,
then selects one of the supplied `R` continuations.
The choice of `R : CType` belongs to the caller, so construction requires no `OS` stack.
The static builder performs the layout calculation within [value functions](#8-value-functions-and-views).

A successful realization supplies [Representation A](../../lib/std/memory/representation.type.zy),
an existential package `exists (= Stored : VType) . Storage A Stored`.
[Storage A Stored](../../lib/std/memory/storage.type.zy) names its dictionary independently of the opening,
so a consumer can receive the shared carrier and its operations as explicit parameters.
The dictionary contains `size`, `alignment`, and four operations:

| Operation | Contract |
| --- | --- |
| `store R value no yes` | Encode `A`, establish backing-buffer alignment, and deliver `Stored` on success. |
| `load R stored no yes` | Decode `Stored` into `A`. A valid stored value satisfies this decoder; its interface retains the explicit failure branch. |
| `bytes stored` | Return the immutable byte buffer for observation or foreign borrowing. |
| `from_bytes R buffer no yes` | Check exact size and canonical contents, establish alignment, and deliver `Stored` on success. |

Only `store` and `from_bytes` introduce `Stored` through this interface.
Arbitrary `Bytes` cannot be passed to `load`, and different existential openings cannot exchange stored values even
when their logical types coincide.
A caller can transfer storage between contracts by explicitly extracting bytes
and validating them at the second contract.
This scopes representation evidence with ordinary package abstraction;
layouts are not runtime indices in the type system.
The [call interface](#stored-call-interfaces) uses the same opening to share stored argument and result types
across separately checked workers, with explicit logical conversion between different carriers.
The [module signature](../../lib/std/memory/package.type.zy) uses an expected existential annotation to prescribe
that abstraction rather than attempting to infer it from the concrete byte implementation.

The current checker cannot derive a total byte decoder from size and layout evidence.
Consequently `load` retains a failure continuation even though these constructors establish its input invariant.
Likewise, two independently opened contracts have no type-level proof that their runtime layouts agree.
These are remaining expressiveness limits: the nominal storage boundary is enforced,
while its byte-level laws are implemented and tested by the library rather than represented as value-dependent proofs.
The public `Storage A Stored` type also permits caller-authored dictionaries.
Its classifier specifies the operations; implementing one carries the same layout-law obligations as a builder.
Reusing a carrier while silently changing its interpretation is not ruled out by a dependent proof.

#### Static layout plans

Foreign records and fixed buffer operations often need placement before execution.
The [static builder](../../lib/std/memory/static-layout.zy) answers that requirement using ordinary value functions.
Its [signature](../../lib/std/memory/static-layout.type.zy) hides `Plan A`
and discloses `Layout A = Result (Plan A) Error`.
Constructors have the same composition syntax as the runtime builder: scalar leaves,
`unit`, `padding`, `product`, and `align`.
Each successful plan contains validated placement and the codecs derived from that placement.
Callers cannot introduce a successful plan from a layout-information record.

With `builtin` and its `UInt8` and `UInt32` carriers in scope, the following fragment constructs an aligned plan.
Import paths are relative to this reference:

```zydeco
let make_memory = @(import("../../lib/std/memory/static-layout.zy")) in
let (/Bytes; byte_package) = builtin |> (@(import("../../lib/std/text/bytes.zy"))) in
let (= Plan, = Layout, memory) = (builtin |> make_memory) byte_package in
let record = memory/align (UInt8 * UInt32) 16
  (memory/product UInt8 UInt32 memory/uint8 memory/uint32) in
match record
| +Err(error) => ...
| +Ok(plan) =>
  let shape = memory/inspect (UInt8 * UInt32) plan in
  let (= Stored, repr) = memory/realize (UInt8 * UInt32) plan in
  ...
end
```

`inspect` and `realize` are value functions.
`inspect` returns a [Shape](../../lib/std/memory/shape.zy) with `size`, `alignment`, and `form`.
A scalar form retains its original byte width, padding is a leaf,
and a product form records the right `offset` and both child shapes.
The left offset is zero.
Raising alignment preserves that form, so an over-aligned scalar still exposes its original width
and an over-aligned product retains its field offsets.
For the example, the shape exposes size 16, alignment 16, and right offset 4.
`realize` constructs the usual `Representation A`; it needs no failure continuation
because placement has already been checked.
Its `store` and `from_bytes` operations still perform fallible backing allocation.

[Size calculations](../../lib/std/memory/size.zy) are source-defined value functions returning `Result Int64 Error`.
They check the nonnegative signed range, power-of-two alignment, and overflow,
using only [L8's total integer leaves](#8-value-functions-and-views).
`NegativeSize`, `InvalidAlignment`, and `SizeOverflow` are ordinary constructors
in [Error](../../lib/std/memory/layout-error.zy); callers can handle them with value matches.
Product construction propagates the left error before the right error;
alignment validates the requested boundary before inspecting its input layout.
No allocation or byte operation runs to calculate a plan, including a representable but impractically large plan.

Both builders use the same [descriptor](../../lib/std/memory/descriptor.type.zy)
and [codec implementation](../../lib/std/memory/codec.zy).
They pass completed offsets, gaps, and tails to codecs, so byte writes do not repeat placement arithmetic.
These internal codec constructors assume validated placement; only the public builders expose opaque successful plans.
The common descriptor remains an internal implementation interface, not an independently checked proof
of arbitrary user-supplied codecs.

The [static elimination contract](#10-static-elimination) determines when a value calculation must resolve.
A runtime size cannot supply a static `padding` calculation; the runtime builder supports that use.
Validated plans themselves can be transported or selected at runtime,
and `inspect` can forward their layout information as ordinary values.
A runtime-selected plan does not thereby supply known integers to a later static calculation.
Neither API executes `Ret` computations during checking.

This is source-level construction evidence, with practical limits.
All plans for one logical `A` have the same `Plan A` type; the type does not distinguish two different placements.
`Shape` is an inspection result, not a dependent proof or a compiler calling-convention descriptor.
It contains no managed-reference map or target register classification.
The [call-boundary proposal](../proposals/escape-unboxing.md#remaining-machine-call-boundary) owns the
additional evidence required before compiler policies may choose among source-constrained call layouts.
The current Rust representation policies continue to govern only locally justified word representations.

#### Layout laws

All sizes and offsets are nonnegative `Int64` values.
Alignment is a positive power of two.
Arithmetic checks the `Int64` bound before adding or rounding; invalid inputs and overflow select `no`
during runtime realization or return a static-construction `Err`, without allocating a payload.
A representable size does not guarantee that storage can be allocated.
Type checking enforces the logical type and abstract storage boundary; the builders implement the arithmetic laws.

The ten scalar leaves use explicit little-endian storage.
Integers occupy their exact declared width, with signed integers using two's complement.
`Float32` and `Float64` occupy their IEEE bit patterns, including signed zero and NaN payloads.
Scalar size and alignment are both the width in bytes.
Each scalar decoder accepts exactly that many bytes.
The [scalar primitives](../../lib/std/builtin/numeric) load and store exact-width bits through checked memory.
[Source codecs](../../lib/std/numeric/codecs.zy) construct immutable byte results and enforce exact decoder widths.

`unit` has size zero and alignment one.
`padding n : Layout Unit` has size `n` and alignment one, and stores exactly `n` zero octets.
It can occur as a field in an ordinary product layout.
For a product with field sizes and alignments `(left_size, left_alignment)` and `(right_size, right_alignment)`:

```text
right_offset = round_up(left_size, right_alignment)
alignment    = max(left_alignment, right_alignment)
size         = round_up(right_offset + right_size, alignment)
```

The left field begins at zero; the right field begins at `right_offset`.
Every gap and trailing byte is zero.
Nested products obey the same rule, so grouping is significant: `A * (B * C)` contains a nested aggregate.
A standalone padding layout may have any nonnegative size; product composition establishes its own aligned stride.

`align A boundary layout` preserves field offsets, raises alignment to the maximum of the requested
and existing alignment, and rounds size up to that alignment with zero tail padding.
It never weakens a field's requirement. For example:

```zydeco
let record = memory/align (UInt8 * UInt32) 16
  (memory/product UInt8 UInt32 memory/uint8 memory/uint32) in
...
```

This description has size 16 and alignment 16.
Its stored bytes are:

| Byte offsets | Contents |
| --- | --- |
| 0 | `UInt8` field |
| 1–3 | Zero gap |
| 4–7 | `UInt32` field, little endian |
| 8–15 | Zero tail padding |

`from_bytes` validates the complete canonical representation.
It rejects truncated or oversized buffers and nonzero padding.
The current implementation decodes and re-encodes to check canonical contents before aligning the supplied buffer.
This deliberately distinguishes an accepted storage contract from arbitrary C struct bytes:
C code must initialize padding to the required value before importing a complete object through this interface.
Alternatively, a [foreign input decoder](#foreign-decoding-and-canonical-storage) can read the meaningful fields
and use `store` to construct canonical storage.

#### Address realization and FFI

Alignment is implemented by source `bytes/aligned` using the general memory allocator and byte copying.
Numeric size calculation, power-of-two validation, and field placement remain library code,
using value functions or returning computations according to the builder.
Zero-padding construction remains a suspended computation.
No layout annotation or special compiler interpretation of `product`, `padding`, or `align` is involved.

Interpreter and native realizations preserve the buffer's contents at a borrowed address divisible
by the contract's alignment.
`bytes/with_window` supplies the retained access, address, and byte count;
the binding passes its explicit readable window as one C pointer and supplies a separate length if required.
Subsequent byte transformations produce ordinary buffers and carry no stored-type proof;
re-import them through the contract to reestablish its invariants.
The Wasm host models checked memory and alignment in a virtual address space, with no native C pointer export.

The [C example](../../lib/tests/ffi/representation.zy) constructs an over-aligned record
and passes it to a [C fixture](../../lib/tests/ffi/boundary.c) that checks address alignment,
`sizeof`, field offsets, contents, and zero padding.
This exercises byte borrowing through the existing FFI, not C aggregate argument classification.
The example's native scalar layout matches the supported little-endian targets;
this is not a portable derivation of every platform's C ABI.
C reads through `memcpy` to avoid assigning an effective C type to the byte allocation.
The [static-plan variant](../../lib/tests/ffi/static-layout.zy) exercises the same C checks
with placement calculated before execution.

#### Construction costs and limits

Layout realization constructs ordinary closure environments.
Storage construction currently creates intermediate buffers and concatenates them;
decoding shares slices, while `from_bytes` also re-encodes for canonical validation.
Deep composition can therefore copy a payload repeatedly.
Static plans remove placement arithmetic and its continuation structure,
without proving fewer executed allocations or a faster program.
The [representation comparison tool](../../cli/examples/representations.rs) reports generated allocation sites.

Retained immutable allocations live outside managed GC until runtime teardown.
Stored carriers still use ordinary words at calls, and an inspected offset remains an integer rather
than a typed field path.
[Memory extensions](../proposals/bytes.md#remaining-questions) retain direct destination codecs,
ownership-aware reuse, typed field paths, and reclamation questions;
[machine representation work](../proposals/escape-unboxing.md#remaining-machine-call-boundary) requires explicit call
and tracing evidence.
Neither the source storage dictionary nor its numerical shape changes an ABI.

### Access through existing representation contracts

[access.zy](../../lib/std/memory/access.zy) consumes the existential `Representation A` interface.
It introduces no compiler rule and does not couple the immutable layout builder to `OS` or `Buffer`.

`read_at A R representation source offset no yes` checks a window of the representation's exact size,
validates its canonical contents, and decodes an `A`.
It accepts dynamically supplied representation packages: unpacking occurs inside the body
because the result does not depend on their hidden stored type.
It returns through the caller's `R` stack and can inspect a field of a larger immutable buffer.
Invalid ranges or representations select `no`.

`write_to A representation destination offset value error done` encodes an `A`
and writes it into a caller-provided `Buffer`.
Destination bounds and closed-handle errors are the buffer protocol's errors;
detected temporary storage allocation failure uses `AllocationFailed`.
No destination byte changes unless encoding and bounds checking succeed.
The operation still constructs a temporary encoding, but repeated field writes reuse the destination allocation rather
than reconstructing the whole record.
The caller chooses its capacity and base alignment through an allocator.

A successful write proves that the bytes fit; it does not turn the destination
into `Stored` or prove that the chosen offset is aligned for a field.
Freeze and import through a complete representation when that proof boundary is needed.
Typed field paths relating parent and child layouts remain deferred:
the current types carry no value-dependent offset or layout-equality evidence.
These explicit checked offset operations remain useful without claiming those proofs.

[Access tests](../../lib/tests/std/storage-access.zy) cover field decoding, wrong ranges,
and failed writes preserving other fields on all backends.
[The C construction example](../../lib/tests/ffi/storage-access.zy) creates the existing 64-byte-aligned record
by writing fields into one destination and freezing it before foreign borrowing.
Reads share byte windows, and writes currently allocate temporary encodings;
a direct destination codec is a later optimization that must preserve these failure and canonical-padding contracts.

### Stored call interfaces

A consumer needs to name the chosen carrier without creating another abstract opening.
The storage library therefore factors its existing package
into `Representation A = exists (= Stored : VType) . Storage A Stored`.
The [storage contract](#descriptions-computations-and-abstract-storage) defines that dictionary and its laws.
A composition root opens a representation once and passes `Stored` and the dictionary to its workers.
Those workers can live in separately checked sources and export `Thk` computations over that same carrier.

The ordinary [call library](../../lib/std/memory/call.zy) defines the computation protocol:

```text
Function A B R = A -> Thk R -> Thk (B -> R) -> R
```

It receives an argument, a failure continuation, and a continuation accepting its result.
`R : CType` describes the required residual stack; using `OS` or `Ret Int64` does not require another adapter design.
The protocol itself places no purity, termination, or single-invocation requirement on user-authored workers.
The library's adapters perform the following sequences, forwarding the same failure continuation at every step:

| Operation | Resulting interface | Sequence when each preceding stage succeeds |
| --- | --- | --- |
| `between A B Input Output input output`, then `encode R logical` | `Thk (Function Input Output R)` | Load input; invoke logical worker; store output; invoke result continuation |
| The same boundary, then `decode R encoded` | `Thk (Function A B R)` | Store input; invoke stored worker; load output; invoke result continuation |
| `compose A B C R first second` | `Thk (Function A C R)` | Invoke first; forward its result directly to second |
| `convert A From To source target R` | `Thk (Function From To R)` | Load with source; store with target |

Construction is by value functions; execution occurs only when the resulting thunk is forced.
Runtime dictionaries and dynamically selected worker thunks may be captured by these adapters.
No metadata arithmetic is needed to forward a stored value, and composition inserts no codec conversion itself.
`convert` preserves the logical value through decoding and encoding; it does not reinterpret bytes or equate carriers.
For a worker whose interface already uses the desired carriers, an ordinary call passes them directly.

For example, after opening a representation of `Record = UInt8 * UInt32`:

```zydeco
let boundary = calls/between Record Record Stored Stored repr repr in
let increment = boundary/encode OS {
  fn (tag, payload) no yes =>
    do next <- ! numeric/uint32/add payload 1;
    ! yes (tag, next)
} in
! increment stored failure { fn result => ... }
```

Here the argument and result have the same abstract `Stored` type.
The [checked example](../../lib/tests/std/represented-call/main.zy) imports this kind of worker,
passes its result to recursive polymorphic code, and selects an alternative thunk at runtime.
The alternative explicitly converts a 16-byte aligned record into a 64-byte aligned record and back.
Both thunks expose the original carrier, so selection and continuation calls agree on their interface.
The example also dynamically chooses a provider package of the ordinary form:

```zydeco
exists (Stored : VType) . Storage Record Stored * Thk (Function Stored Stored OS)
```

Opening that package gives its consumer a coherent codec and worker even when the provider's byte layout is unknown.
Packaging preserves agreement by carrying both together; it does not recover an unknown witness from metadata.

Identity is deliberately nominal at this boundary.
Tests reject arguments, result continuations, and composed workers from independently opened representations,
including different alignment, different field width, and identical placement opened twice.
Matching callers share the opening; numerical equality of sizes, alignments, or shapes never introduces type equality.
Caller-authored dictionaries carry the [storage laws](#layout-laws).
The checker does not prove that two implementations at one carrier use identical codecs.

[Stored-call regressions](../../lang/tests/tests/represented_calls.rs) check direct and dynamic calls,
failure propagation, canonical conversion, and distinct-opening rejection across the execution backends.
This interface uses the existing word convention.
Encoding performs a load and store; decoding adds a store and load.
Matching carriers do not by themselves remove these potentially allocating conversions.

## 14. Foreign interfaces

A foreign implementation is an annotated hole:

```zydeco check
param val (/Thk; /Ret; /Access; /Addr; /Int64; /UInt64) : @(import("../../lib/std/builtin.zy")) in
(@(ffi(c, library("xxhash"), symbol("XXH3_64bits"))) : Thk ((Access * Addr * Int64) -> Int64 -> Ret UInt64))
```

The supported classifier is `Thk (A1 -> ... -> An -> Ret B)`, including zero arguments.
Each fixed-width integer (`Int8` through `Int64`, `UInt8` through `UInt64`) contributes the matching C `intN_t`
or `uintN_t`.
An explicit product `Access * Addr * Int64` contributes one `const void *`;
its count states the readable extent checked before C entry.
Any C length parameter is a separate integer argument.
The result `B` is a fixed-width integer or `Unit`; `Ret Unit` calls a C `void` function
and resumes the Zydeco continuation with `()`.
The declaration specifies exact widths: C `int`, `long`, enums, and typedefs require platform-specific agreement.
At most six flattened C arguments are accepted.
Integer results preserve the declared width and signedness when re-encoded.

A readable-window argument lends its contiguous memory for the duration of the call.
The adapter checks liveness, read permission, bounds, and initialization before C entry.
The binding validates any stronger alignment, element format, and relationship between extent and explicit length;
a mismatched C length is still an incorrect trusted declaration or adapter.
The callee must neither modify nor retain the pointer, and must not dereference it for a zero-length window.
The declaration author is responsible for the actual symbol's signature and these borrowing obligations.
A returning call must use the C return protocol; unwinding and nonlocal jumps across this boundary are unsupported.
An import may call another compiled Zydeco library under its [entry discipline](#compiled-libraries-and-c-exports);
callbacks into an active instance remain unsupported.
Here `Ret` records the C return protocol even when the C function has effects.
Apply the [public API convention](#ret-and-explicit-cps) in source wrappers: keep pure operations returning,
and expose effectful ones by binding the raw result and invoking an explicit successor.
The declaration establishes neither purity nor termination.

Checking validates the declared classifier without loading a library or inspecting headers.
The Unix interpreter loads symbols lazily; native AMD64 links the named library.
Missing libraries or symbols fail at loading/linking.
Wasm and the ZASM interpreter reject native imports.
Callbacks, floating-point or aggregate values, ungranted or mutable pointers,
and larger signatures are outside this subset.
[Concrete boundary examples](../proposals/c-ffi.md#examples-and-observed-gaps) motivate proposed extensions.

### Storage and foreign transport

A `Cell H` specifies the storage of a handle; its view interprets that handle.
The foreign signature separately specifies what the callee receives.
Opening a fat handle can supply an address and count for a pointer-plus-length call,
but its logical product does not become a C aggregate or expand into arguments automatically.
A prefix-header handle can pass its payload address without reading the header
when the binding already knows the extent.
The supported 64-bit targets use a 64-bit `size_t`; a checked nonnegative `Int64` byte count has the same bits.

A source `Storage A Stored` can validate and encode a record, then expose a readable window to an import.
The [record fixture](../../lib/tests/ffi/boundary.zy) exercises one pointer plus five integer parameters:
the validation extent consumes no additional C argument.
Using `Stored` itself, or a logical product of record fields, as the foreign classifier is rejected.
Declaring integers instead would describe a different C signature; checking cannot discover that mismatch
with the actual header.

The binding's record contract and physical alignment remain explicit obligations.
A generic window validates initialized readable bytes,
but infers neither element alignment nor a relationship between its extent and another integer parameter.
Source wrappers can expose recoverable preflight failure through `Memory.check`
and the required leaf reads before invoking the foreign thunk.
An empty window has no readable byte and need not have a null address;
nullability and sentinel conventions belong to the binding.
Raw pointer results require an ownership, extent, and release contract before they can yield a grant.

#### Foreign decoding and canonical storage

The [C specimen](../../lib/tests/ffi/contracts.c) writes a `UInt8` tag and `UInt32` payload
and deliberately fills padding with `0x58`.
The C inspector reads the expected logical record.
The test feeds these actual C-produced bytes into Zydeco: the existing whole-record `from_bytes` rejects them,
because that operation checks canonical encoding, including zero padding.
This rejection is correct for its [storage contract](#layout-laws).

The [record input example](../../lib/tests/ffi/record-input.zy) demonstrates a separate foreign decoder
in ordinary CBPV.
It obtains the size and field positions from the existing source layout plan, checks exact extent,
decodes the two fixed-width integer fields, and ignores only the layout's padding.
The caller then uses `store` to construct canonical `Stored`.
The same test checks that these canonical bytes pass `from_bytes`; short and oversized inputs select failure
before any logical fields are exposed to the success continuation.
The canonicalization program executes on all four backends with the bytes produced by the C specimen.

This example needs no new compiler feature.
It establishes the direction for C output adapters: decode the foreign representation to `A`,
then store through the selected `Storage A Stored` contract.
Keep strict `from_bytes` for callers that require canonical bytes.
Other foreign decoders must account for their own endianness, valid field encodings, active union alternative,
and length conventions; the integer record does not establish a generic decoder for arbitrary C objects.
The fixture checks its native layout and little-endian byte order explicitly.

### Compiled libraries and C exports

A [compiled library package](#source-packages) exposes selected functions through an FFI entry profile. Its evolution
follows the
[shared design of compilation units, FFI, and package management](compiler.md#compilation-unit-preparation-and-artifacts).
A compiled library declares a named source implementation and its complete public C interface:

```zydeco check
@[package(
  library(c,
    export(field(add), symbol("example_add")),
    export(field(identity), symbol("example_identity"))),
  name(example/arithmetic))]
param val (/Thk; /Ret; /Int64; /numeric) : @(import("../../lib/std/builtin.zy")) in
(
  #add = ({ fn x y => ! numeric/int64/add x y } : Thk (Int64 -> Int64 -> Ret Int64)),
  #identity = ({ fn x => ret x } : Thk (Int64 -> Ret Int64))
)
```

`library(c, ...)` marks source intended to produce a compiled C library.
It requires an explicit package name and at least one `export(selector, symbol("c_name"))`.
`root` selects the prepared value itself; `field(api/add)` follows ordinary named projections through that value.
Selectors and symbols must be distinct, and a root export cannot coexist with field exports.
Each symbol is an unmangled ASCII C identifier; the `zydeco_` prefix is reserved for runtime support.
Only the selected symbols are public.
Export a constant through a zero-argument returning thunk; global data exports have no contract in this profile.

Independent selection checks the whole source term under an empty lexical context, including its code imports.
A declaration cannot capture a binding outside its selected term.
Preparation accepts a closed value or exactly one leading `param val` over a validated Builtin value contract.
Aliases and source imports may supply that factory; preparation uses ordinary static application.
Builtin validation examines its typed roles, independently of the process-only `OS` codomain requirement.
The provider's runtime values are created at each external entry.
Supply other static arguments in source before the boundary; an unapplied generic factory is not an export.

Field selection and static specialization precede runtime readiness.
Unselected static helpers may remain without runtime representations, while each selected thunk,
its captures, and its reachable body must be complete and representable.
Missing or ambiguous projections, escaping static values, and reachable executable holes reject the unit.
Preparing the interface never executes the exported computation.

The export classifier is `Thk (I1 -> ... -> In -> Ret R)`, with zero through six fixed-width integer parameters
and a fixed-width integer or `Unit` result.
It shares the import signature's widths and scalar conversions, but reverses the transport direction.
An incoming pointer cannot establish an `Access` grant, owner, permission, or extent;
therefore the outgoing readable-window adapter cannot serve as an export parameter.
Abstract source values, closures, floating-point and aggregate values, `OS` results,
and extra arguments are rejected at this boundary.

Every call creates a fresh runtime instance and supplies a C return delimiter for `Ret R`.
Normal return converts the result while the instance is alive, releases its resources, and resumes the C caller.
Arguments exposed through Builtin are empty.
Standard streams borrow the process streams; newly opened resources belong to the call.
There is no cross-call source state or exported runtime handle.
Runtime faults terminate the process with a diagnostic; this profile neither unwinds
through C nor fabricates a return value.
A nonterminating body need not return.

One guard covers all exports of a unit in its link image.
Concurrent entry or reentry into an active unit is rejected before source initialization.
Sequential calls and ordinary internal recursion are supported.
A call from unit A to independent unit B preserves A's complete suspended instance and restores it on return,
even when the units share runtime support in one native image.
Calling A again while it is active is rejected. Entry from a signal handler is unsupported.
Retained callbacks, persistent instances, recoverable errors, and incoming memory ownership remain separate extensions.

`build --target object`, `staticlib`, and `sharedlib` emit AMD64 Linux or macOS libraries.
The [artifact workflow](../../CONTRIBUTING.md#compile-and-consume-c-libraries) produces a C header,
a generated Zydeco import interface, and a manifest alongside the native output.
Ordinary `@(import(...))` still imports source, including a compiled library's original factory;
it does not choose an artifact or implicitly apply Builtin.
A consumer imports the generated `.imports.zy` interface and explicitly supplies `--link-library MANIFEST`
to `build`, `run`, or `test`.
This supports separate compilation without the producer's implementation source.
The interpreter accepts only shared libraries matching its own host; Wasm has no adapter for these artifacts.

## 15. Execution profiles

The execution paths share source checking and static elimination, but have different allocation and host boundaries.
The native layout is an implementation ABI, not source-level control over addresses or object layout.

| Profile | Control and memory | Host boundary |
| --- | --- | --- |
| Interpreter | Explicit evaluator state; Rust-owned values and environments | CLI/REPL I/O; returning C imports on Unix |
| Native AMD64 | Machine control stack; growable retained environments; two fixed 1 MiB copying semispaces | Supplied runtime, Linux/macOS toolchain, returning C imports and scalar exports |
| `wasm-am` | Trampoline over ZASM; fixed 1 MiB operand/control stack; growing non-collecting heap | Imports from a `zydeco` embedding |
| `wasm-sps` | Block trampoline and persistent stack frames; growing non-collecting heap | The same host operation contract |

Native managed live values must fit in one semispace, including headers.
Host-owned strings and byte buffers remain allocated within their runtime instance, independently of managed collection.
Environment growth and managed-heap capacity are distinct limits.
Native tail transfers reclaim dead activations, but live continuations and escaping values can retain storage;
Wasm trampolines avoid growth of the host call stack without guaranteeing constant heap use.

Static value-function application expands residual bodies and can increase code size.
Product and thunk allocation depends on optimization.
Source byte slicing shares retained immutable storage on every backend.
Repeated concatenation can be quadratic.
Immutability guarantees observations, not identical costs on every backend.

The supplied Node host implements I/O, checked resources, and random integers for CLI execution and tests.
It reads stdin on demand and shares the supplied argument sequence with the program.
Native FFI requires installed libraries; Wasm does not support native C imports.
Source memory capabilities support checked allocation, addresses, and the explicit storage contracts in §13.
Ordinary compiler-managed values have no source-controlled ABI layout; primitive concurrency remains absent.
Runtime-managed capabilities provide the current resource boundary.

### Selecting an execution backend

Backend selection is command-line execution policy, independent of package meta annotations.
`run SOURCE -t TARGET` selects one of `interpreter`, `exe` (AMD64), `wasm-am`, or `wasm-sps`.
`--target` is the long spelling; omitting it selects `interpreter`.
`run` inherits terminal streams, forwards arguments after `--`, and returns the program's exit status.
`--dry` checks and selects the executable root without linking, emitting code, or requiring backend tools.

`test SOURCE` accepts the same targets and allows repeated `-t` options.
The test-only selection `-t all` expands in place to `interpreter`, `exe`, `wasm-am`, and `wasm-sps`, in that order.
Explicit choices replace the interpreter default; duplicates are removed, keeping first-occurrence order.
Discovery and source checking are shared across backends, with one lowering per test for compiled targets.
All selected tests and requested backend artifacts are prepared before any test executes.
Source errors, unsupported lowering, and missing toolchains fail preparation rather than skipping a backend.

Execution is sequential: each test in package-plan order runs on its backends in requested order.
Every test/backend pair reports a labeled result.
Zero exits pass; nonzero exits display captured stdout and stderr.
Runtime errors also fail their pair, and remaining pairs still run.
The summary counts pairs, and the command fails if any pair fails.
Wasm requires Node.js and uses the bundled host; native execution requires the AMD64 tools and runtime sources.
See the [workflow](../../CONTRIBUTING.md#check-and-run-source-terms) for tool and runtime configuration.

## Meta annotations (compile-time metadata)

Meta annotations are written `@[meta] e`; `@(meta)` abbreviates a hole payload.
These forms supply compile-time metadata to the compiler.
An annotation expression is a name, string, integer, or a named application with comma-separated arguments.
Runtime metadata, such as a buffer length or object vtable pointer,
is ordinary value data and follows the usual checking and erasure rules.
An ordinary layout descriptor is also a typed value; evaluating it during checking does not make it a meta annotation.

| Meta annotation | Meaning and valid use |
| --- | --- |
| `import(source)` | Replace a hole with an independently checked source term; select a catalog name, quoted file path, or positive input number (§12) |
| `package(role, ...)` | Register the annotated term with a role, metadata name, and typed relationships (§12); compiled libraries require a name |
| `discover(include("glob", ...), exclude("glob", ...), ...)` | Declare ordered file-root discovery rules for an explicit package catalog (§12) |
| `intrinsic(role)` | Supply a canonical kind/type (`vtype`, `ctype`, `thk`, `ret`, `unit`, `i8`…`i64`, `u8`…`u64`, `f32`, `f64`, `char`, `string`) or an integer value function (§8) |
| `builtin(role)` | Mark a host capability or operation in a typed package contract (§13) |
| `ffi(c, library("name"), symbol("name"))` | Supply a foreign thunk implementation at a hole (§14) |
| `typeof` | Extract a synthesized classifier (§4); no arguments |
| `monadic` | Algebra translation under the lexical basis (§11); no arguments |
| `partial` | Permit the annotated computation header's binders to fail (§7); no arguments |
| `literal` | Replace a hole with its attached text block as a string; no arguments |
| `doc` | Attach [documentation](#source-documentation) to a term, binding, or member |
| `format(options...)` | Scope formatting options to a payload |
| `debug` | Record a checked term for compiler observation |

The REPL commands in §12 are frontend interpretations of root meta annotations.
Unrecognized meta annotations are structurally accepted and have no defined semantic effect here.
Documentation and debug annotations may carry additional annotation arguments.
Formatting options include `width`, `indent`, `layout`, `parentheses`, and `verbatim`;
their [directive contract](compiler.md#formatter-directives) lives in the compiler reference,
and [CONTRIBUTING](../../CONTRIBUTING.md#format-and-lint) gives the workflow.

### Source documentation

Zydeco documentation combines Markdown attached to source terms with the compiler's information
about bindings, imports, and named fields.
The same explanation appears in hover, completion, the VS Code documentation panel, and generated project references.
The [documentation workflow](compiler.md#documentation-workflow) describes those tools and their commands.

#### Attaching documentation

Write an uninterrupted `--|` block immediately above `@[doc]`:

```zydeco check
--| The current counter value.
--|
--| Read this field to inspect progress.
@[doc] let counter = 42 in counter
```

Use `--|` for blank lines within the block; a genuinely blank source line or an ordinary `--` comment breaks attachment.
Unattached text blocks produce warnings.
Use ordinary comments for implementation notes.

On a simple `let` or `def`, the explanation describes its binding.
Documentation immediately on a binding's right-hand side also follows its resolved uses.
An annotation on `#field = value` or `#field :: Type` describes that member.
An annotation on a block describes the block itself; nested definitions acquire prose from their own annotations.
Arbitrary expressions can have explanations too.
The [provenance contract](compiler.md#documentation-subjects-and-provenance) determines
which explanations follow aliases, imports, explicit interfaces, and projected fields.

Additional `doc(...)` arguments are retained as meta annotation values;
section and grouping options have no implemented presentation contract.
Parameter and constructor-arm documentation remain unsupported extensions,
tracked in the [documentation proposal](../proposals/documentation.md).

#### Semantic documentation links

Ordinary Markdown links work alongside two explicit semantic destinations.
For names already in the annotation's lexical scope:

```markdown
[integer type](zydeco:name:Integer)
[current value](zydeco:member:Counter/value)
```

`zydeco:name:Integer` resolves a lexical name in the scope where the annotation was written.
`zydeco:member:Counter/value` names an owner and a public field path.
The owner must already be in scope; an annotation before a nonrecursive binding cannot link
to the binding it introduces.
Imported prose keeps its original scope even when a consumer shadows a name.

Use inline Markdown links for semantic destinations.
Reference-style semantic links and unresolved destinations are diagnosed.
Editors navigate to source; generated HTML uses a local API anchor when a unique exposed page is known,
and otherwise a source link.
Source links in a copied reference require access to the original source paths.
Standalone guide pages use the [selected public root](compiler.md#documentation-publication-and-verification) instead
of an implicit lexical source scope.

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
