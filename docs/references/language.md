# Zydeco Language Reference

This reference describes the current source language and its execution boundaries.
It assumes basic familiarity with typed lambda calculi, polymorphism, and operational semantics.
The [language guide](../tutorial/zydeco-guide.md) provides a longer introduction;
[CONTRIBUTING](../../CONTRIBUTING.md) covers command-line workflows.

1. [Conventions and the Language Model](#1-conventions-and-the-language-model)
2. [Lexical Structure and Syntax](#2-lexical-structure-and-syntax)
3. [Bindings and Scope](#3-bindings-and-scope)
4. [Classification and Inference](#4-classification-and-inference)
5. [Values, Products, and Data](#5-values-products-and-data)
6. [Computations and Control](#6-computations-and-control)
7. [Patterns and Coverage](#7-patterns-and-coverage)
8. [Value Functions and Views](#8-value-functions-and-views)
9. [Polymorphism and Packed Values](#9-polymorphism-and-packed-values)
10. [Static Elimination](#10-static-elimination)
11. [Relative Monads](#11-relative-monads)
12. [Sources, Imports, and Entry](#12-sources-imports-and-entry)
13. [Primitive Values and Capabilities](#13-primitive-values-and-capabilities)
14. [Foreign Interfaces](#14-foreign-interfaces)
15. [Execution Profiles](#15-execution-profiles)

## 1. Conventions and the Language Model

Zydeco separates values from computations. Values are inert data, including suspended computations;
computations consume continuation stacks and may perform effects.
Kinds, types, values, and computations share the surface term grammar; checking determines the sort of a term.

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

### Term-Oriented Composition

Zydeco's term-oriented design uses binding, abstraction, and application
to compose expressions at the language's different sorts.
Bindings and parameters are themselves term forms with scoped bodies (§3):
`let x = e in body` combines a definition with its use, while `param p in body` constructs an abstraction.
Both forms can occur within larger expressions.
Type functions build types, and value functions assemble values (§8).

The benefit is that library composition can use ordinary language mechanisms.
Products group operations, packed values associate them with abstract or manifest type witnesses (§9),
and functions take dependencies explicitly (§8).
These structures can be nested, passed, and composed wherever their classifiers permit them.

Composition follows the relevant typing and phase rules: computation sequencing is explicit (§6),
and execution requires static elimination of value-function applications and static witness structure (§10).
Source files, packages, and compilation units apply this composition model to reuse and distribution (§12).

## 2. Lexical Structure and Syntax

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
Write `((#x = 1) : (#x :: Int))` to annotate the whole named value; `(#x = 1 : Int)` annotates its payload.
Metadata in an application argument also needs parentheses.

## 3. Bindings and Scope

`param p in e` introduces a type or computation abstraction; `param val p in v` introduces a value function.
`let p = e in body` binds a type or value transparently.
`def p = e in body` additionally seals a type definition with a nominal identity.
Binders scope over the tail; ordinary lexical bindings may shadow enclosing names.

A binding form determines transparency or nominal identity;
its `in` or `that` connective determines placement and scope.
These choices are independent: moving a definition before or after a use does not turn a transparent alias
into a nominal type.
`let` supports composition of packed values with disclosed equations, while `def` establishes an identity
that clients can share without equating it with its implementation.

A `that` binding contributes to the nearest `begin ... end` block.
Its names are visible throughout that block, and dependencies from bodies and annotations determine its placement.
Source order breaks ties. Lexical binder identities are resolved before dependency scheduling;
reordering must preserve capture, shadowing, and nominal identity.
A dependency on a lexical binder must still be available at the block boundary;
a nested `begin` provides a nearer boundary.

```zydeco check
begin
  let answer = seed that
  param (seed : @(intrinsic(int))) that
  ret answer
end
```

The example elaborates to a function taking `seed` before binding `answer`.
An unannotated `begin` adds no effect or existential boundary.

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

## 4. Classification and Inference

Kinds are `VType`, `CType`, kind arrows, and named kinds `#field :: K`.
Type functions use `fn (X : K) => T` and ordinary application.
Type application reduces by substitution; transparent aliases and manifest witness equations participate in equality.
Products retain order, arity, and explicit nesting; labels retain their names.

A nominal `def` keeps its identity distinct from its implementation and other definitions.
Typing may expose a sealed data or codata shape to introduce or eliminate it without equating distinct seals.
Repeated uses of one definition share its identity; copying a term freshens its bound nominal definitions.
Identity across imported source copies follows the
[package-resolution model](../proposals/package-resolution.md#copy-resolve-merge-analyze).

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
Constructors and openings of packed values generally require an expected type;
type and kind binders remain annotation-directed.
Sources close their inference independently before an import-site expectation is compared (§12).

`@[typeof] e` reuses the synthesized classifier of `e` as a static term:

| Operand | Query result |
| --- | --- |
| `1` | `Int` |
| `ret 1` | `Ret Int` |
| `{ ret 1 }` | `Thk (Ret Int)` |
| `Int` | `VType` |
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

## 5. Values, Products, and Data

Values include variables, literals, `()`, products, named values, constructor applications,
thunks, existential values, and total value functions.
`() : Unit`; `(v)` is grouping; `(v1, ..., vn)` introduces a product or,
under an expected existential type, a witness-prefixed value.
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

## 6. Computations and Control

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
let Int = @(intrinsic(int)) in
def Probe = codata | .read : Ret Int end in
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

### Ret and Explicit CPS

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

### Ret and Stack Extent

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

## 7. Patterns and Coverage

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
Direct field-projection groups additionally support the shared opening of a packed value in §9.

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

## 8. Value Functions and Views

`val (x : A) => v` introduces a total value transformation.
If `v : A'`, its classifier is `val pi (x : A) . A'`.
Type parameters express polymorphism within this value-function space.
Value binders may also open type witnesses used in the result type; arbitrary runtime values cannot index types.

```zydeco check
let val duplicate (x : @(intrinsic(int))) = (x, x) in
ret (4 |> duplicate)
```

`f v`, `v |> f`, and `f <| v` are the same value application.
Functions can be partially applied, passed to other value functions, returned,
and stored in intermediate structures, subject to §10.
They may capture runtime values, but cannot execute computations to form their result.
A thunk in the result keeps its computation suspended.

`match` also produces a value when every arm produces a value of the same type.
It uses the patterns and exhaustive coverage of §7; computation-producing arms keep their usual meaning.
The four integer value intrinsics below have classifier `val pi (left : Int) (right : Int) . Int`,
where `Int` denotes `@(intrinsic(int))`:

| Intrinsic | Result |
| --- | --- |
| `int_add` | Addition wrapping within the `Int` payload |
| `int_sub` | Subtraction wrapping within the `Int` payload |
| `int_and` | Bitwise conjunction |
| `int_compare` | Signed comparison: −1, 0, or 1 |

```zydeco check
let Int = @(intrinsic(int)) in
let add = @(intrinsic(int_add)) in
let compare = @(intrinsic(int_compare)) in
let val maximum (left : Int) (right : Int) : Int =
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
let Int = @(intrinsic(int)) in
let val first ((x, _) : Int * Int) = x in
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

## 9. Polymorphism and Packed Values

Polymorphism lets code use a type supplied by its caller.
A packed value lets a provider supply types together with values whose classifiers may depend on them.
These arrangements determine who chooses a type and where its identity is available.

`forall (X : K) . B` classifies computation polymorphism, introduced by `fn (X : K) => M`
and eliminated by type application.
Type arguments erase.
At computation-type level, `pi` uses the binder's sort: a type binder yields a universal computation,
and a value binder yields a computation arrow, possibly dependent on opened type witnesses.
With a kind body, `pi (X : K) . L` forms the kind arrow `K -> L`.
`sigma` similarly yields an existential for a type binder or a product for a value binder.
These forms do not provide general dependence on runtime values or quantification over kinds.

Dependency is judged after elaboration: `pi (value : Int) . (@[typeof] ret value)` forms `Int -> Ret Int`.
The same erasure permits classifier queries in `val pi`, `sigma`, and kind-arrow bodies.

An **existential type** `exists (X : K) . A` hides a type witness used by its payload;
an **existential value** inhabits that type.
The broader term **packed value** also covers structures with manifest type components
or a mixture of abstract and manifest witnesses.
For example, a library may expose an element type together with operations on that type.
Its interface determines which type equations a consumer can use.

An **abstract witness** exposes its kind and identity while keeping its defining equation opaque.
A **manifest witness** is transparent: its interface exposes an equation, such as `Element = Int`.
An exposed equation `Element = T` can still refer to an abstract `T`,
so manifest does not imply that the underlying representation is known.
Visibility belongs to each exposed witness; a packed value can contain both abstract and manifest witnesses.
The [static elimination rules](#10-static-elimination) determine which components erase.

The telescope may mix abstract witnesses, manifest equations `(X as T : K)`,
and manifest kind entries such as `(VType as @(intrinsic(vtype)))`.
Later entries may refer to earlier ones. Naming and punning compose with these binder forms.
Static and value entries may be interleaved through nested existentials and products;
packed value formation does not require all existentials to precede all products.
A manifest binder may omit its classifier when its definition synthesizes it, including a kind classified by `Set`.
An explicit classifier checks that same definition.

Against an expected existential, `(T, v)` supplies the witness and payload.
`pack` synthesizes an existential type from explicit evidence:

```zydeco check
let Int = @(intrinsic(int)) in
let package =
  pack (= Item as Int : @(intrinsic(vtype)))
  where #value = 42 end
in
let (/Item; /value) = package in
ret (value : Item)
```

A manifest entry discloses its witness with `as`; an abstract entry uses `(X : K) is T`.
The payload must synthesize its type. Sealing abstracts occurrences of a recoverable witness identity;
it does not infer an arbitrary abstract interface from concrete values.
For example, `pack (X : VType) is Int where 42 end` synthesizes `exists (X : VType) . Int`.
Use an expected existential annotation when its abstract payload interface must be prescribed.
`pack` currently introduces type witnesses, not kind witnesses.

Opening a packed value introduces fresh identities for its abstract witnesses, with scope over its payload bindings.
Those witnesses cannot escape an ordinary binding's result type.
A **witness-dependent** function classifier binds the abstract witnesses opened from its argument,
allowing its result classifier to refer to them: `pi ((X, x) : exists (X : K) . A) . B`.
Application requires corresponding witness evidence available in the caller's static scope.
The runtime implementation may be an unknown thunk; its signature carries the dependency.
Opening a manifest entry substitutes the disclosed equation and creates no fresh witness.
Introduction checks the supplied witness against that equation.
Manifest entries erase and contribute no abstract witnesses to a witness-dependent arrow.
Disclosed equations substitute transparent provider-local names;
normalization cannot recover an equation hidden by sealing.
At computation application, witness instantiation currently traverses a leading existential prefix;
it does not recover abstract components beneath preceding value products.
An ordinary value `sigma` introduces no witness telescope, so witnesses opened by its pattern must be absent
from the resulting component type, even when mentioned only through `typeof`.

Field selection `v/field` recursively searches named wrappers, product components,
and witness telescopes for exactly one matching public name.
Functions, thunks, and data payloads are opacity boundaries.
Explicit named fields and public punned binder names, including manifest kind entries, share this search namespace.
Missing and ambiguous names are different errors, and an explicit path performs a new search at each slash.
Selection can traverse manifest entries directly; crossing an abstract witness requires a projection pattern.
Fields behind an abstract witness still count when checking ambiguity.

```zydeco reject=tyck.missing-named-field at=1:15
(#value = 42)/missing
```

`/field = p` selects and binds the payload; `/field` puns that binding.
Payload patterns are irrefutable and may carry annotations.
Projection patterns associate to the right: `/outer = /inner = p` selects `outer`, then `inner` from its payload.
A group such as `(/Item; /value; whole)` opens each selected packed value occurrence once,
so related fields share witnesses and `whole` can forward the same packed value.
Type projection selects the payload of a named kind.
Module factories and explicit dictionaries use these ordinary packed value and function forms.

### Module Interfaces and Shared Openings

A module interface exposes the names a client needs without requiring the client
to unpack the provider's entire product layout.
Recursive field search permits grouping operations for organization; explicit paths resolve ambiguity.
The opacity boundaries above prevent selection from forcing a thunk or inspecting an arbitrary data payload.
Changing grouping is compatible only where existing selections remain unique and preserve their witness evidence.

An abstract carrier and the operations consuming it must share the witness from one opening of a packed value.
Projection groups and whole-value aliases preserve that opening for forwarding through a composition root.
Independent openings introduce distinct abstract witnesses even
when the packed values have identical field names or layouts.
This relationship uses existential elimination and ordinary bindings; it introduces no separate module object.

An inferred `pack` interface is useful when its witnesses and payload already express the intended contract.
An explicit annotation, including a source companion (§12), prescribes the interface when details must be hidden.
`typeof` can reuse an inferred contract but couples it to the inspected implementation.
No companion is required merely because a term is used as a module.

Value functions compose modules statically (§8).
A provider selected at runtime instead uses an ordinary thunk or codata protocol;
a witness-dependent computation arrow carries any witness dependency needed by its result.
Explicit adapters can expose curried universal interfaces when that is the desired public protocol.

A consumer can open a provider inside a scoped existential elimination and pass its carrier
and operations to a polymorphic callback.
The hidden identity stays within that elimination and cannot escape through its result.
The callback's classifier supplies neither purity nor single invocation nor a native continuation lifetime.
Remaining questions concern [kind-witness introduction](../ideas/kind-witness-introduction.md),
[computation witness routes](../ideas/computation-witness-routes.md),
and [companion generation](../ideas/companion-interface-generation.md).

## 10. Static Elimination

Before execution, value-function applications, views, and static packed value structure must reduce
to a representable residual program.
Reduction follows lexical bindings, type and value application, known constructors, projections,
construction and opening of packed values, value matches, and the integer value operations in §8.
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

Explicit computation thunks and representable packed value payloads may remain.
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

## 11. Relative Monads

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

## 12. Sources, Imports, and Entry

### Abstraction Levels

Zydeco's [term-oriented composition](#term-oriented-composition) gives each term static semantics
in a context and composes terms through ordinary language constructs.
The following abstractions connect that semantic structure to source reuse and independent compilation.

| Abstraction | Responsibility |
| --- | --- |
| Project | Available packages, discovery roots, and package-name bindings |
| Package | A term selected for distribution and reuse, with identity, role, and relationships |
| Compilation unit — optional at package level | Establish a complete external contract for independent compilation |
| Semantic unit | Establish a term's meaning in context: resolved names, classifier, provenance, and semantic documentation |

A semantic unit is a term considered with its analysis context.
Processing source into its semantic form includes lexing, parsing, name resolution, and type checking.
Semantic units compose through the language's binding, abstraction, application, and import rules;
independent source selection retains the [source-boundary requirements](#source-boundaries).
One semantic unit can participate in several compositions and supported compilations.
Its meaning depends on its source inputs and resolution context; target and optimization choices belong
to the consuming compilation.

A compilation unit supplies the external contract needed to emit a selected package independently.
The [source-package roles](#source-packages) describe these optional contracts.

Semantic documentation belongs with the term's analysis and provenance.
Documentation operations query those facts, verify selected links and examples,
and assemble reference pages, explicit guides, anchors, and rendered output.
The [source-documentation rules](#source-documentation) describe authoring;
the compiler reference maps these abstractions
to their [implementation representations](compiler.md#implementation-representations).
Package context, source selection,
and import identity follow the [namespace and resolution proposal](../proposals/package-resolution.md).

### Source Boundaries

A source file contains one complete term. After imports and any companion annotation are assembled,
each source synthesizes its classifier from its own ordinary lexical bindings and explicit imports.
Provider inference closes before comparison with import-site expectations.
An independently selected term follows the same binding and inference boundary.

Imports and companion signatures form an acyclic graph.
Imported computations execute at every dynamic occurrence. Package context
and sharing follow the [resolution model](../proposals/package-resolution.md#copy-resolve-merge-analyze).

A `foo.zy` implementation may have an independently checked `foo.zyi` type companion.
The pair behaves as an annotation of the implementation by that type.
Companions can import other sources and be imported themselves.
Source terms supply their imports and parameters explicitly;
[package roles](#source-packages) describe optional compilation contracts.

`@[typeof] @(import("library.zy"))` extracts the provider's complete classifier.
For a builder, querying a particular result requires applying the builder in the operand.
A companion may query another acyclic source; querying its own implementation creates a rejected import cycle.
Classifier queries couple a signature to the inspected implementation; use an explicit public signature
when its contract should remain stable across implementation changes.

`check` accepts kinds, types, values, and computations.
`run` and executable builds require a computation accepting the host Builtin packed value
and ending in its `OS` protocol:

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

### Source Packages

A package is a term selected for distribution and reuse.
It may be supplied as source or, when its external contract supports independent emission, as compiled artifacts.
This distribution role is independent of the term's language-level structure:
a source package can supply a type term, a packed value (§9), or a function that constructs one.
A package-definition meta annotation selects a term and records its role:

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
| `library(zydeco)` | A [native initializer](#native-zydeco-units) returning the prepared library value. |
| `binary` | Executable Builtin/OS boundary. |
| `test` or `test(of(...))` | The same executable boundary, with test associations. |

A compilation unit is an independently selected source term with a complete external entry contract.
Binary and test roles already establish that contract; a compiled library declares its own.
The compiled-library contract exposes the term through the C or Zydeco ABI, within the supported entry profile.
There is no additional `unit` annotation or package argument.
Naming makes the term selectable by name; independent emission additionally requires the external contract.
The [compiler reference](compiler.md#compilation-unit-preparation-and-artifacts) explains how the designs
of compilation units, FFI, and package management evolve together.

A test is itself a package: `test(of(...))` records its subjects, and plain `test` describes an independent test.
Subsequent arguments supply an optional `name(...)` and typed relationships.
Source libraries may expose any source classifier.
Compilation-unit validation checks the declared compiled-library interface or the executable Builtin/OS contract.

#### Package Resolution

Package paths, source instantiation, shared identity,
and name conflicts follow the [namespace and resolution proposal](../proposals/package-resolution.md).
The [package implementation plan](../proposals/package-management.md#shared-project-context) covers project preparation,
discovery integration, and frontend selection.

#### Relationships and Operations

Relationships associate packages with tests and other subjects:

| Relationship | Meaning |
| --- | --- |
| `test(target)` | A companion test associated with the declaring package |
| `of(subject, ...)` under `test(...)` | The packages tested by this test |

Relationship targets follow the shared
[package-path rules](../proposals/package-resolution.md#paths-and-package-context).
The [operation plan](../proposals/package-management.md#validation-and-operation-planning) develops validation scope,
test selection, and frontend integration.
The [execution contract](#selecting-an-execution-backend) governs backend preparation and test results.

## 13. Primitive Values and Capabilities

[Builtin](../../lib/std/builtin.zy) exposes canonical kinds and fixed-representation types as manifest fields.
Repeating an intrinsic splice denotes the same canonical kind or type across independently checked sources.
`Addr`, `Reader`, `Writer`, and `OS` are abstract provider capabilities sharing one opening.
`Addr` supports [manual memory](#manual-memory); its kind remains `VType`.
Typed pointers and initialization states are ordinary library types.
Pure libraries can name canonical carriers without requiring a numeric or resource provider merely to name a type.
Resource operations instead share their provider's abstract opening,
so a composition root must forward the capability and its operations together.
The `numeric`, `text`, and `system` groups contain host operations.
The [standard library](../../lib/std/README.md) assembles ordinary module factories and defines `Bool`, `Option`,
`Result`, `List`, and abstract `Bytes`; host operations select continuations instead of constructing those types.

The memory interfaces build on those capabilities using ordinary packed values and computation protocols:

| Question | Owning section |
| --- | --- |
| How are addresses allocated and released? | [Manual memory](#manual-memory) |
| What does the type checker know about memory state? | [Typed pointers and slices](#typed-pointers-and-slices) |
| Which layout information is static? | [Static layout plans](#static-layout-plans) |
| What makes a byte sequence immutable? | [Immutable owners](#immutable-owners-and-source-bytes) |
| How are bytes constructed incrementally? | [Byte builders](#byte-builders) |
| How do source modules share pointers? | [Stored calls](#stored-call-interfaces) |

| Family | Source behavior |
| --- | --- |
| `Int`, `UInt` | Tagged machine integers; arithmetic wraps within the payload |
| `Int8/16/32/64`, `UInt8/16/32/64` | Exact signed/unsigned widths; arithmetic wraps at the chosen width |
| `Float32`, `Float64` | IEEE 754 arithmetic at the chosen width |
| `Char` | One Unicode scalar, excluding surrogates |
| `String` | Immutable valid UTF-8; indexed operations count Unicode scalars |

`Int` and `UInt` use every payload bit of a tagged runtime word.
The current interpreter, AMD64, and Wasm profiles have 64-bit words and 63-bit integer payloads:
`Int` ranges from `-2^62` through `2^62 - 1`, and `UInt` from zero through `2^63 - 1`.
Their arithmetic wraps modulo `2^63`; a signed result uses two's-complement interpretation.
`Int64` ranges from `-2^63` through `2^63 - 1`, and `UInt64` from zero through `2^64 - 1`.
Their arithmetic wraps modulo `2^64`.
The [scalar value boundary](compiler.md#scalar-value-boundaries) specifies their boxed runtime representation.

Integer and float literals default to `Int` and `Float64`.
An expected primitive type selects another width; integers must fit and floats round to that width.
Finite-range overflow is rejected, while float underflow may round to zero.
Existing numeric values have no implicit cross-width conversion.
`int64/from_int : Thk (Int -> Ret Int64)` and `uint64/from_uint : Thk (UInt -> Ret UInt64)` preserve the value.
The reverse operations `int64/to_int` and `uint64/to_uint` accept a result protocol,
a value, a failure thunk, and a success thunk receiving the machine integer.
They select success exactly when the mathematical value fits the destination range; they never truncate.
These operations are available in both the Builtin numeric modules and std.
Integer division or remainder by zero terminates unsuccessfully; signed minimum divided
by `-1` wraps, with remainder zero.
Integer parsing rejects values outside the `Int` range through its failure continuation;
random integers range over that same domain.
Float rendering uses the selected width's Rust Display spelling, including signed zero, `inf`, `-inf`, and `NaN`.

### Text and Byte Sequences

`String` indices are scalar positions, not byte offsets or grapheme clusters; `byte_length` observes UTF-8 bytes.
The source-defined `Bytes` type contains immutable octets; equality compares contents and ordering is lexicographic.
Negative or out-of-range positions, invalid Unicode scalars, and invalid UTF-8 select failure branches.
The public library reifies these as `Option` or `Result`.
The [immutable byte contract](#immutable-owners-and-source-bytes) below explains ownership and library operations.

Every scalar Builtin module provides raw `store_le` and `load_le` operations over `Addr`.
Callers establish writable or initialized readable extents; source byte codecs validate their own lengths.
They preserve exact little-endian bits; float loads and stores preserve NaN payloads without arithmetic.
The [source codecs](../../lib/std/numeric/codecs.zy) provide `to_le_bytes` and `from_le_bytes`,
and std includes them in its numeric modules.
Decoders require the exact scalar width.

### Streams and Process Arguments

`args/at : Thk (forall (R : CType) . Int -> Thk R -> Thk (String -> R) -> R)` looks up a zero-based argument
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

### Manual Memory

Systems code needs to choose storage, initialize it, and release it explicitly.
[std/memory](../../lib/std/memory/package.zy) provides that interface using ordinary packed values,
abstract type witnesses, and [CPS](#ret-and-explicit-cps).
It introduces no lifetimes, borrow checker, affine types, or special continuation kind.

The compiler supplies `Addr`, existing scalars, `Ret`, `Thk`, and the primitive memory operations.
`Addr` is an unmanaged data address; it carries no allocation identity, extent, permission, or initialization flag.
The library supplies the other types:

| Type | Purpose | Runtime payload |
| --- | --- | --- |
| `Uninit`, `Init`, `Fields S T` | Whole-object and partial-record initialization states | None |
| `Ptr L S` | An address interpreted with layout witness `L` and state `S` | One `Addr`; no wrapper or state tag |
| `Slice L S` | Contiguous elements with an explicit dynamic extent | Pointer and element count |
| `Field P C`, `DynamicField P C` | A parent/child layout path | Fixed recipe erases; dynamic path carries one offset |
| `views/View P H A`, `views/DynamicView P H A` | Handle interpretation | Fixed recipe erases; dynamic operation is an ordinary thunk |
| `fixed/Layout A` | A static `Result (Plan A) LayoutError` | Construction evidence eliminates before fixed realization |
| `Representation A` | An existential layout witness with separate storage and codec components | Fixed recipes erase; dynamic counterparts carry geometry and operation thunks |
| `dynamic/Layout A` | Runtime layout computation | Captured dynamic inputs and operations |
| `Storage L`, `DynamicStorage L` | Validated geometry independent of logical values | Fixed recipe erases; dynamic geometry carries size/alignment |
| `Codec L A`, `DynamicCodec L A` | Initialization and observation recipes | Fixed recipes erase; dynamic codecs carry ordinary thunks/context |
| `StaticAlloc Context`, `Alloc` | Fixed and runtime allocation services | Explicit context; runtime interface carries ordinary operations |
| `Buffer` | Incremental byte construction | Address, capacity, initialized-prefix length |

The table describes payload requirements. Products, closures, and runtime-selected dictionaries still use
the [ordinary representation policy](compiler.md#c10-zasm-stack-analysis-and-local-representation-choices).
CPS alone does not guarantee allocation-free callbacks or stack frames.
`Buffer` builds bytes; the [array factory](#array-storage-and-element-builders) also provides generic element builders.
No `Frozen` state is needed for this manual interface.

#### Unsafe Obligations

A library field named `unsafe` documents caller obligations; it is not a language keyword or a checked effect.
Raw addresses and typed pointers may be copied freely.
The caller establishes lifetime, allocation extent, initialized fields, valid scalar
or pointer representations, alias discipline, and synchronization before each access.
Use of a freed pointer or stale state alias is a contract violation, with no required recoverable runtime fault.
The native and interpreter accesses perform no grant lookup or initialized-byte tracking.

An ordinary `Thk R` may escape, be dropped, or be forced repeatedly.
CPS successors therefore establish neither unique ownership nor guaranteed cleanup.
After invoking a completion that releases or publishes storage, a callback must not use an old mutable alias.
A caller that abandons cleanup leaks its manually allocated storage.
There are no implicit destructors.
Raw storage must not contain movable managed references without a separate rooting protocol;
the supplied layouts cover scalars and unmanaged addresses.

### Independent Storage and Codecs

[Storage](../../lib/std/memory/storage.zy) validates geometry without selecting a logical value type or allocator.
`storage/constant/create size alignment` returns `Result (exists L. Storage L) LayoutError`;
its runtime counterpart returns `Ret (Result (exists L. DynamicStorage L) LayoutError)`.
Both reject negative sizes and nonpositive or non-power-of-two alignments.
The fixed constructor requires static operands. Both representations are sealed against unchecked construction;
`unsafe/for_layout L` validates the same geometry but lets the caller associate it with an existing witness.
`query L` exposes `SizeAlign = (#size :: Int) * (#alignment :: Int)`.
`constant/materialize L` explicitly obtains dynamic geometry from fixed storage.

A witness relates geometry, pointers, and codecs, not an allocation instance.
Independently opened witnesses remain distinct even with equal geometry.
For dynamic instances, the caller retains the matching geometry until release.
Size and indexing remain bounded by nonnegative `Int`; full pointer-width sizes remain proposed work.

[Codec](../../lib/std/memory/codec.type.zy) supplies value functions `init pointer value : Thk (Cps (Ptr L Init))`
and `read pointer : Thk (Cps A)`.
The [dynamic form](../../lib/std/memory/dynamic-codec.type.zy) carries ordinary thunks:
`init : Thk (Ptr L Uninit -> A -> Cps (Ptr L Init))` and `read : Thk (Ptr L Init -> Cps A)`.
`codecs/materialize L A` selects that form explicitly.
`codecs/unsafe/scalar L A write read` associates scalar accesses with a caller-established layout.
`codecs/unsafe/take` reads through a fixed codec before yielding an uninitialized pointer
and the value; `dynamic_take` does the same through a dynamic codec.
Neither clears bytes nor invalidates aliases. A codec carries no implicit allocator.
Its stored format is independent of the compiler's representation of `A`.

[StaticAlloc](../../lib/std/memory/static-allocator.type.zy) selects allocator code using value functions,
with explicit `Context`, byte size, and alignment; its `free` also takes the original base.
`allocation/static_heap` uses `Unit` context, and `materialize Context allocator context` produces `Alloc`.
`reserve L Context allocator context storage` produces a CPS allocation recipe yielding `Ptr L Uninit`;
`unsafe/release` requires matching geometry, provider/context, and an uninitialized pointer.
`dynamic_reserve` and `unsafe/dynamic_release` use `Alloc` and `DynamicStorage` at runtime.
The [executable example](../../lib/tests/std/storage-codecs.zy) covers both forms and state transitions.
Fixed and dynamic layout conveniences expose these components separately under the same layout witness.
Record and array factories compose storage; callers select logical conversion only when needed.
`codecs/product` combines child codecs through matching field paths.
Its lower-level `unsafe/product_at` and `unsafe/relabel` recipes require the caller
to establish valid nonoverlapping offsets and a compatible parent layout; they perform no geometry validation.

### Allocation and Release

The [allocator interface](../../lib/std/memory/allocator.type.zy) receives byte size and alignment explicitly.
`allocation/heap` uses the host allocator; `allocation/limited maximum parent` rejects oversized requests
before calling its parent.
Runtime selection of an allocator is ordinary source dispatch.
There is no hidden allocator field on a pointer.

The raw [memory adapter](../../lib/std/memory/native.zy) exposes allocation and an `unsafe` operation group.
Allocation validates nonnegative size, positive power-of-two alignment, and a representable rounded extent;
invalid requests report `InvalidLayout`, and reservation failures report `AllocationFailed`.
The resulting storage is uninitialized.
Zero-sized allocations require no backing bytes and cannot be dereferenced.
Release receives the original base, exact requested size and alignment, and matching allocator.
It must occur exactly once, after the caller has settled the initialized contents and all uses of aliases.

`null` and signed wrapping `offset` are pure address calculations returning through `Ret`.
Dereference validity is a separate obligation; an offset calculation does not prove bounds.
`load_addr` and `store_addr` access unmanaged pointer slots.
Scalar `load_le` and `store_le` access the exact little-endian width without requiring natural alignment.
`copy source destination count` permits overlapping ranges; `fill address count octet` writes a byte pattern.
Valid raw loads, stores, copies, and fills use only a success/completion continuation.
There is no runtime initialization check and no implicit initialization of padding.

### Typed Pointers and Slices

Selecting a fixed representation produces `exists L. (#storage :: Storage L) * (#codec :: Codec L A)`.
Its dynamic counterpart substitutes `DynamicStorage` and `DynamicCodec` in that same packed value shape.
The [representation alias](../../lib/std/memory/representation.type.zy) is parameterized by these component families.
[Ptr](../../lib/std/memory/types.zy) privately aliases `Addr` and has no runtime wrapper.
Allocate and release using the selected storage and provider; initialize, observe, or take using the selected codec.
The [independent interfaces](#independent-storage-and-codecs) give the exact call order and protocols.

`init` writes directly into the supplied destination.
`read` copies a logical value without changing its state.
`take` reads the value and supplies a pointer interpreted as uninitialized; it does not clear the storage.
These types reject reading an uninitialized pointer, initializing an initialized one,
freeing through the wrong state, and interchanging independently opened layouts.
They do not invalidate the old pointer or consume any aliases.
In particular, copying a pointer before a transition can leave a well-typed stale alias.

`pointer/unsafe/address L S` exposes the address; `from_address L S` asserts an interpretation.
The latter requires the caller to establish layout, initialization state, extent, and lifetime.
It supplies the explicit boundary for foreign memory and manually calculated field pointers.
No typed field-path or partial-record tracker is implied by an integer offset.

The [slice operations](../../lib/std/memory/slice.zy) share these pointer and state types.
`from_parts` checks a nonnegative count and wraps a caller-validated pointer and extent.
`slices/for_layout L storage` selects fixed geometry; `for_dynamic_layout L storage` selects dynamic geometry.
Each supplies `unsafe/at`, derives stride by rounding size up to alignment,
checks the index against the count, and rejects rounding or multiplication overflow before offsetting,
using the shared [counted-view access](#memory-views) implementation.
Zero-sized elements have zero stride. The resulting pointer keeps the same `L` and `S`.
The caller supplies an allocation covering the elements; bounds checks do not verify that assertion.
A raw slice does not retain its allocation.

### Static Layout Plans

Fixed placement belongs to source value calculation.
The [fixed builder](../../lib/std/memory/layout.zy) exposes `Layout A = Result (Plan A) LayoutError`,
with an abstract successful plan and [inspectable shape](../../lib/std/memory/shape.zy).
Its constructors support scalars, unmanaged addresses, unit, padding, products, and increased alignment.
Invalid alignment, negative size, and overflow produce typed `Err` results during static construction;
only an `Ok` plan can be realized.
The caller handles that static result explicitly.
Unknown runtime inputs to placement or fixed realization produce a `StaticElimination` diagnostic.

`realize A plan` introduces `L` with the [representation interface](../../lib/std/memory/representation.type.zy).
Size and alignment are known constants at this boundary.
The source recipes specialize field offsets and selected accesses; the pointer carries none of this evidence.
Explicit queries can materialize size or alignment as an `Int`, and allocation receives those constants.
Transporting a plan through an unknown runtime argument does not make its placement static.
Use `dynamic` when execution chooses sizes or alignment.

The [complete fixed-layout example](../../lib/tests/std/static-layout.zy) allocates a 16-byte aligned record,
initializes and reads its fields, then takes the value and releases the allocation.
[Type and phase tests](../../lang/tests/tests/static_layout.rs) pair accepted layouts with invalid arithmetic,
forged plans, mismatched logical types, and runtime placement operands.

#### Layout Laws

Supported scalar widths are 1, 2, 4, or 8 bytes, with matching natural alignment.
The current interpreter/native 64-bit profiles and Wasm's virtual-address profile use an 8-byte address slot.
This is an explicit storage format; it does not select a C aggregate ABI or the compiler's logical product layout.

For a product, the second field starts at the first field's size rounded up to the second field's alignment.
Aggregate alignment is the maximum field alignment; total size rounds the field end up to that alignment.
All size arithmetic is checked against nonnegative `Int` bounds before any wrapping primitive arithmetic.
Raising alignment preserves internal field offsets and rounds the final size to the greater boundary.
Unit has size zero and alignment one.
`padding count` reserves bytes and contributes only a logical `Unit`.

For example, `UInt8 * UInt32` has offsets 0 and 4, size 8, and alignment 4.
Raising alignment to 16 retains offsets 0 and 4 and gives size 16.
Typed initialization writes the fields directly; reading ignores the intervening and trailing padding.
No temporary `Bytes` encoding or canonical-zero-padding check participates in these operations.
Serialization and foreign functions that inspect every byte require the caller to initialize padding explicitly,
for example with `raw/unsafe/fill` before field initialization.

### Dynamic Layouts

The [dynamic builder](../../lib/std/memory/dynamic-layout.zy) supports the same layout combinators
when their inputs are runtime values.
Its `realize A R layout no yes` validates sizes, alignment, and overflow at execution,
then supplies a `Representation A` with a witness, `storage : DynamicStorage L`, and `codec : DynamicCodec L A`.
Invalid placement selects `no` before allocation or writes.
Dynamic size, alignment, and offsets remain captured where needed; they are not appended to each typed pointer.
The [runtime-layout example](../../lib/tests/std/representation.zy) exercises this distinction.

### Typed Records and Field Paths

The [record factories](../../lib/std/memory/record.zy) compose child storage geometry while preserving its witnesses.
`records/product Left Right left_storage right_storage` calculates ordinary product placement;
`records/at` additionally accepts the left and right byte offsets, total size, and alignment.
Both produce a static `Result (Record Left Right) LayoutError`; neither takes a logical type or codec.
Explicit placement checks nonnegative extents, alignment, field overlap, and the enclosing size before construction.
Zero-sized fields occupy no bytes; padding remains uninterpreted.

Opening the result yields a fresh parent `L`, its `storage : Storage L`,
`left` and `right` field groups, and state conversions.
Names come from ordinary named values: a record module may expose `#length = left` and `#payload = right`.
Nested records require neither reflection nor a compiler row system.

[Field](../../lib/std/memory/field.zy) is an abstract source family `Field Parent Child`.
Its implementation is a static value function carrying the constant displacement;
`fields/offset` and path composition must resolve before execution.
`fields/initialized` and `fields/uninitialized` specialize pure pointer projections for whole-object states.
A path relates layouts, without identifying a particular allocation.
Ordinary clients cannot exchange unrelated parent or child witnesses.
`fields/unsafe/from_offset` is the explicit assertion boundary for caller-authored layout relationships;
its caller establishes a nonnegative offset and a child footprint fitting and aligned within the parent.
A raw shape inspection supplies no such evidence automatically.

`fields/materialize` deliberately produces `DynamicField Parent Child`, carrying one runtime byte offset.
The `runtime_fields` operations query, compose, and project these paths through `Ret`.
`runtime_fields/unsafe/from_offset` accepts a runtime displacement under the same caller obligations.
Neither path form appends a descriptor to its resulting pointer.

Records track partial construction with the erased `Fields LeftState RightState` constructor.
A field's accessors select a logical type and matching codec when used.
After supplying `A`, `Codec Left A`, and sibling state `S` to a left accessor,
its remaining value-function arguments produce these suspended protocols:

```text
left/init pointer value : Thk (Cps (Ptr L (Fields Init S)))  -- pointer has Fields Uninit S
left/read pointer       : Thk (Cps A)                      -- pointer has Fields Init S
left/take pointer       : Thk (forall R. Thk (Ptr L (Fields Uninit S) -> A -> R) -> R)
```

These accessors live under `unsafe`, which slash projection can omit when unambiguous.
For example, `! (left/init A codec S pointer value) R yes` initializes the left field.
The right field preserves the left state analogously.
`states/empty` opens `Uninit` as `Fields Uninit Uninit`, and `states/full` opens `Init` as `Fields Init Init`.
`states/finish` and `states/vacate` perform the inverse conversions for completely initialized or vacant records.
These are erased value conversions, with no flag writes or padding initialization.

A field's `unsafe/project` selects its own state from a partially initialized parent.
For a nested transition, `unsafe/replace` accepts the original parent and the same projected child
after its update, and returns the parent with that child state changed.
It performs no writes or address-equality check: the caller must supply that exact child and retire stale aliases.
An arbitrary path cannot propagate a parent's entire `Fields` state onto one child.
The [record and view example](../../lib/tests/std/general-views.zy) exercises direct construction and untouched padding;
[type/state regressions](../../lang/tests/tests/general_memory.rs) cover nested paths and rejected transitions.

### Array Storage and Element Builders

The [array factory](../../lib/std/memory/array.zy) composes element storage without selecting a logical value type.
`arrays/make Element storage capacity boundary` takes `Storage Element` and static inputs,
returning `Result (Array Element) LayoutError`.
`arrays/realize Element R storage capacity boundary no yes` takes `DynamicStorage Element`,
validates runtime inputs, and supplies `DynamicArray Element`.
Both calculate `stride = round_up(element_size, element_alignment)`
and `size = round_up(stride * capacity, max(element_alignment, boundary))`, checking all arithmetic before allocation.
An eight-byte element aligned to 64 therefore has stride 64; a codec still touches only its meaningful bytes.
Fixed arithmetic uses [size.zy](../../lib/std/memory/size.zy),
dynamic arithmetic uses [dynamic-size.zy](../../lib/std/memory/dynamic-size.zy).
No integer-dependent type is required.

Opening either packed value introduces an array-layout witness `L`, construction handle `Build`, and family `Values A`.
The packed value supplies `storage`, `capacity`, `stride`, and element/builder operations.
`Ptr L S` is one address.
Fixed recipes specialize their geometry; dynamic packed values retain the geometry and ordinary operation thunks.
Dynamic packed values contain no static value-function fields.
`elements/unsafe/at` checks an index against capacity and produces `Ptr Element S`;
zero-sized elements have zero stride and require no backing bytes.
`elements/unsafe/base` is a pure address interpretation and supplies no dereference bounds.

`elements/unsafe/init_each` writes directly into an uninitialized destination through a caller-supplied callback.
A successful callback supplies that same element as `Init`; a failure callback must first settle its current contents
and supply that same element as `Uninit`.
Completed elements advance the initialized prefix.
The operation's failure successor receives the fault and a `Build` describing that prefix;
success receives whole-array `Init` only after every element completes.
Abandoning a continuation produces neither cleanup nor failure notification.

The `buffer/unsafe` group manages this prefix explicitly.
`start` begins with zero initialized elements; `resume` reopens a fully initialized array at capacity.
`length` and `prefix` expose the current count and a counted initialized element pointer.
`push` checks capacity before writing; `pop` reads the last element and decreases the count.
Fixed calls select `A` and `Codec Element A` as value arguments: `! (buffer/push A codec build value) R no yes`
and `! (buffer/pop A codec build) R no yes`.
Dynamic calls explicitly dispatch through `DynamicCodec Element A`:
`! buffer/push A R codec build value no yes` and `! buffer/pop A R codec build no yes`.
`finish` requires a complete prefix, while `free` requires an empty one and the original allocator.
A rejected operation preserves the supplied handle and storage.
Copying handles still permits stale aliases.

Whole-value conversion is optional.
For a fixed array, `array/codecs A element_codec` produces `Codec L (Values A)`.
A dynamic array uses `! array/codecs A dynamic_element_codec`,
returning a materialized `DynamicCodec L (Values A)` through `Ret`.
`values/generate A` builds exactly the capacity of logical elements through a pure callback;
`values/at A` observes them with a checked index.
`Values` is sealed, preserving that length contract.
Whole-value reads and takes construct a managed list behind `Values A`.
Direct access, prefix building, and `init_each` need no such list.
Embedding array storage in a record requires only the array's `storage` component.
[Array construction](../../lib/tests/std/array-memory.zy),
[storage-only padded stride](../../lib/tests/std/storage-arrays.zy),
and [runtime selection](../../lib/tests/std/runtime-memory.zy) exercise these choices.
A typed no-read discard operation remains
[proposed](../proposals/memory.md#independently-selectable-storage-operations);
manual address reinterpretation remains available under its existing obligations.

### Memory Views

Storage layout, handle representation, and handle interpretation are independent choices.
A handle `H` is the value passed by the caller; a view describes how it leads to a payload pointer and observations.
A logical fat handle does not itself prescribe native field placement or a C argument layout.
Its own storage representation is selected separately.

The [view library](../../lib/std/memory/view.zy) defines these ordinary source types:

```text
Cps A        = forall R. Thk (A -> R) -> R
Checked E A  = forall R. Thk (E -> R) -> Thk (A -> R) -> R
View P H A   = val pi (handle : H). Thk (P A)
DynamicView P H A = Thk (H -> P A)
```

`View` selects a recipe statically and produces a computation for its handle.
`! (view handle)` executes that operation; a CPS operation then receives `R` and its successor.
Typical results are `Ptr L S * M`, where `M` is runtime metadata, but a view can yield another handle for composition.
`Ret` serves pure calculations, while `Cps` and `Checked` sequence memory observations
under the [L6 convention](#ret-and-explicit-cps).
Applying a value function never executes a load.

`identity`, `precompose`, `map`, and `compose` adapt and combine pure views.
`as_cps` lifts a pure view; `map_cps` and `compose_cps` preserve the CPS protocol.
`as_checked`, `map_checked`, and `compose_checked` propagate explicit failure without running a later step.
No adapter asserts that arbitrary CPS code is pure.
The fixed recipe itself must disappear under static elimination; runtime handle fields and captured context can remain.

For runtime selection, `materialize` explicitly produces `DynamicView` with its ordinary thunk and captured data.
Different handle types can be paired with their matching operations in an existential value.
Neither form inserts a view dictionary into each handle.

```zydeco check
param (/VType; /CType; /Thk; /Ret; /Int; builtin) : @(import("../../lib/std/builtin.zy")) in
let (/views) = builtin |> (@(import("../../lib/std/memory/package.zy"))) in
let (/Cps; /View; /identity; /as_cps) = views in
let run : Thk (Int -> Ret Int) = {
  fn value => ! ((as_cps Int Int (identity Int)) value) (Ret Int) { fn result => ret result }
} in
! run 7
```

The supplied constructors support these handle forms:

| Constructor | Handle | Observation |
| --- | --- | --- |
| `thin` | `Ptr L S` | Same pointer and `Unit`, through `Ret` |
| `fat` | `Ptr L S * M` | Carried pointer and arbitrary runtime metadata, through `Ret` |
| `unsafe/header` | Abstract `H S`, backed by `Addr` | Read through a metadata codec at a fixed signed displacement; calculate payload address through CPS |
| `unsafe/inline` | Header address in `H S` | Metadata at the handle; payload at a fixed displacement |
| `unsafe/prefix` | Payload address in `H S` | Metadata at a signed displacement; preserve payload address |
| `unsafe/indirect` | Address of an unmanaged pointer slot | Load its pointer, then run another CPS view |

A `Header M Payload` packed value supplies the handle family `H S` and its unsafe wrapping,
address, and opening operations.
The caller establishes initialized metadata fields and payload state `S` independently;
finding an uninitialized destination does not require a fully initialized enclosing object.
For a counted initialized element interpretation, the obligation covers the stated extent, not spare capacity.
The fixed header constructors receive `Codec Metadata M` without requiring its allocation geometry.
`unsafe/dynamic_header` receives `DynamicCodec Metadata M`, runtime displacements,
and a raw address under these same obligations.
A header containing a vtable address is supported as data; calling code pointers has its separate FFI boundary.

[headers/from_fields](../../lib/std/memory/header.zy) derives inline and prefix recipes
from metadata and payload paths sharing one parent layout.
Prefix recovery includes the padding chosen by that layout.
For example, an `Int` length followed by four `UInt32` elements with payload alignment 16 has offsets 0
and 16, total size 32, and alignment 16.
The inline handle is the allocation base; the prefix handle is `base + 16`.
The [complete example](../../lib/tests/std/header-array.zy) constructs that layout and uses both interpretations.

`bounded` wraps a counted CPS view with checks for nonnegative capacity and `0 <= length <= capacity`.
`counted` and `counted_checked` receive `Storage L`, derive rounded element stride,
validate index and multiplication bounds, and produce an element pointer.
`dynamic_counted` and `dynamic_counted_checked` use `DynamicStorage L` and compute checked stride at runtime.
These checks cannot establish allocation validity or initialized contents from an arbitrary header.
A fixed array can instead use a thin interpretation, keeping its bound in the recipe without a carried length.

An opening is a snapshot: later metadata changes do not update earlier observations or copied handles.
Reopening performs fresh loads.
Synchronization, alias validity, and exact allocation release remain the caller's task;
release uses the original base and allocation layout, not a mutable length header.
Fixed recipes and state witnesses erase, while ordinary products, thunks,
and frames retain the compiler's representation policy.
Staging and CPS alone do not guarantee that all runtime allocations disappear.

### Immutable Owners and Source Bytes

[Bytes](../../lib/std/text/bytes.zy) is a source abstraction with a private address and byte count.
Its backing allocations are explicitly retained until the runtime instance is destroyed.
Copying and slicing share storage without per-copy reference counting; a small slice can retain a large allocation.
Text conversion and I/O reads import their results into that retained storage.
No mutable operation is available through the immutable content interface.

`bytes/unsafe/build R count alignment fill no yes` allocates a private destination.
The fill callback receives its address, a failure successor, and completion.
It must initialize every published byte and invoke exactly one successor.
Failure frees the allocation before forwarding the error; completion transfers ownership
to runtime retention before publishing `Bytes`.
A retention reservation failure also frees the allocation.
After success, no alias may mutate or free the storage.
Escaped aliases and repeated completions violate this caller contract; the callback type does not prevent them.

`bytes/unsafe/from_retained` wraps an address and nonnegative length whose retention,
immutability, and initialized extent the caller establishes.
`with_window` exposes the address and byte count for a trusted read-only consumer.
`copy_to` copies initialized contents into a caller-provided destination.
Retaining an allocation with unused spare capacity is permitted when only its initialized prefix is published.
Retention transfers ownership, without scanning bytes or tracking initialization.

`empty`, `length`, content comparison, slicing, concatenation, scalar codecs,
and UTF-8 conversion preserve immutable-content semantics.
Bounds and UTF-8 failures remain explicit branches.
Concatenation allocates once and copies each input; alignment conversion allocates a suitably aligned copy.
These pure-content wrappers may use internal allocation while following the [Ret convention](#ret-and-explicit-cps).
The [byte regressions](../../lib/tests/std/source-bytes.zy) exercise slicing, bounds, UTF-8, copying, and retention.

### Byte Builders

The source [Buffer](../../lib/std/memory/buffer.zy) is a manually managed byte builder.
`allocate` takes an explicit allocator and capacity and starts with an empty initialized prefix.
`unsafe/push` and `unsafe/extend` check capacity before writes and deliver an updated handle through CPS.
A capacity failure leaves storage and the supplied handle's prefix unchanged.
Copying a handle does not keep its prefix metadata synchronized with later updates; callers retire stale handles.

`unsafe/snapshot` copies the initialized prefix into retained `Bytes`.
`unsafe/finish_heap` transfers a builtin-heap allocation to retention and publishes its initialized prefix
without copying its payload; the caller must not reuse any old handle.
It requires builtin-heap allocation, since retention eventually frees through that allocator.
`unsafe/close` instead releases storage through the original allocator.
Neither repeated finish nor access after close is a checked operation.
The [builder example](../../lib/tests/std/buffer.zy) covers capacity preflight,
independent snapshots, spare capacity at publication, and explicit cleanup.
Growth and integration
with `Writer` remain [proposed](../proposals/filesystem.md#memory-backed-writer-and-byte-builder).

### Stored-Call Interfaces

Source modules share pointer types and CPS operations from the same layout opening.
A worker can accept `Ptr L Init`, take its fields, initialize the same destination,
and deliver `Ptr L Init` without allocating another payload.
The [cross-module example](../../lib/tests/std/represented-call/main.zy) uses this sequence with ordinary thunks.
There is no separate storage-conversion or call-adapter package.
A different layout needs an explicit allocation and field conversion, with both allocations' lifetimes settled.
Equal numerical placement does not equate independent witnesses.
These calls use the existing source word convention; managed logical values and continuations may still allocate.

## 14. Foreign Interfaces

A foreign implementation is an annotated hole:

```zydeco check
param val (/Thk; /Ret; /Addr; /Int; /UInt32) : @(import("../../lib/std/builtin.zy")) in
(@(ffi(c, library("xxhash"), symbol("XXH64"))) : Thk (Addr -> Int -> UInt64 -> Ret UInt64))
```

For `ffi(c, ...)`, the supported classifier is `Thk (A1 -> ... -> An -> Ret B)`, including zero arguments.
`Int8/16/32/64` and `UInt8/16/32/64` contribute their matching C `intN_t` or `uintN_t`.
`Int` and `UInt` use `int64_t` and `uint64_t` carriers in the current profiles;
their source ranges remain those in [L13](#13-primitive-values-and-capabilities).
Values entering Zydeco, whether a C import result or an export argument, are range-checked.
An out-of-range value terminates execution before source code resumes.
`Addr` contributes one raw data pointer.
Any C length or capacity parameter is a separate integer argument.
The result is a supported integer or `Unit`; `Ret Unit` corresponds to C `void`.
At most six C arguments are accepted. The declaration chooses carriers and signedness; C `int`, `long`,
enums, and typedefs require platform-specific agreement.

The declaration author supplies the real symbol's ABI and pointer contract.
For every pointer argument, the binding establishes lifetime, extent, alignment, initialized readable fields,
writable ranges, alias compatibility, and any foreign retention or release requirements.
There is no automatic bounds, liveness, or permission check before C entry.
An empty range has no readable byte and need not have a null address; nullability belongs to the binding.
A function receiving immutable `Bytes` storage must not modify or free it.
Manual mutable allocations can be passed directly when their foreign contract permits it.

Here `Ret` records the C return protocol even when the function has effects.
The [public Ret/CPS convention](#ret-and-explicit-cps) applies to source wrappers: keep pure observations returning;
bind an effectful raw result, interpret its status, then invoke an explicit successor.
The classifier establishes neither purity nor termination.
C may leave partial writes on a reported failure; only the particular binding can promise failure before mutation.
A normal return uses the C return protocol.
Unwinding, nonlocal jumps, and callbacks into an active instance are unsupported.
An import may enter a separate compiled library under its [entry discipline](#compiled-libraries-and-c-exports).

Checking validates the declared classifier without loading libraries or inspecting headers.
The Unix interpreter loads symbols lazily; AMD64 links the named library.
Missing libraries or symbols fail at loading or linking.
Wasm and the ZASM interpreter reject native imports.
Callbacks, pointer results, floating-point and aggregate ABI values,
and larger signatures remain [extensions](../proposals/c-ffi.md).
C exports currently admit only scalar arguments and scalar or unit results.
The `zydeco` convention instead names a [native unit initializer](#native-zydeco-units).

### Storage and Foreign Transport

`Int` and `UInt` use eight-byte little-endian storage in the current profiles.
Signed storage sign-extends the 63-bit value; unsigned storage leaves its top bit zero.
`Int64` and `UInt64` use eight bytes with every bit available to the payload; every eight-byte pattern is valid.
Their C carriers are `int64_t` and `uint64_t`, respectively, regardless of the ordinary value representation.
Raw `Int`/`UInt` loads reject out-of-range carrier bits.
The safe numeric `from_le_bytes` codecs check both length and payload range
and select their failure continuation on invalid input.
The raw memory operations still require a valid address and initialized readable extent.

A storage layout determines byte placement; the foreign signature independently determines argument transport.
`Ptr L S` requires explicit address exposure before a C import; its abstract library type is not a foreign classifier.
A logical product or `Slice L S` does not become a C aggregate or expand into arguments automatically.
A binding passes the address and any required count separately.
The current 64-bit profiles use a 64-bit `size_t`; a validated nonnegative `Int` count has the same bits.

The [aligned record fixture](../../lib/tests/ffi/static-layout.zy) allocates storage, explicitly initializes padding
because its C inspector reads every byte, writes the typed fields, calls C with the address, and releases storage.
The [mutable output fixture](../../lib/tests/ffi/mutable-output.zy) passes an uninitialized destination to C,
interprets the C status through a CPS wrapper, and asserts `Init` only after the promised fields have been written.
The raw adapter supplies no ownership inference for these transitions.

#### Foreign Decoding

The [C specimen](../../lib/tests/ffi/contracts.c) produces a `UInt8` tag and `UInt32` payload with `0x58` padding.
The [source decoder](../../lib/tests/ffi/record-input.zy) obtains size and offsets from a static plan,
checks exact byte extent, and decodes only those fields.
Padding is not part of the logical record and need not be canonicalized before typed use.
Short and oversized inputs fail before exposing fields to the consumer.
The [integration test](../../lang/tests/tests/ffi_examples.rs) runs the decoder
on all four backends using actual output from the C specimen.
Other bindings must establish their own endianness, valid field encodings, union alternatives, and length conventions.
Canonical serialization, when required, is a separate codec contract.

### Compiled Libraries and C Exports

A [compiled library package](#source-packages) exposes selected functions through an FFI entry profile. Its evolution
follows the
[shared design of compilation units, FFI, and package management](compiler.md#compilation-unit-preparation-and-artifacts).
A compiled library declares a source implementation and its complete public C interface:

```zydeco check
@[package(
  library(c,
    export(field(add), symbol("example_add")),
    export(field(identity), symbol("example_identity"))),
  name(example/arithmetic))]
param val (/Thk; /Ret; /Int; /numeric) : @(import("../../lib/std/builtin.zy")) in
(
  #add = ({ fn x y => ! numeric/int/add x y } : Thk (Int -> Int -> Ret Int)),
  #identity = ({ fn x => ret x } : Thk (Int -> Ret Int))
)
```

`library(c, ...)` marks source intended to produce a compiled C library.
It requires at least one `export(selector, symbol("c_name"))`.
`root` selects the prepared value itself; `field(api/add)` follows ordinary named projections through that value.
Selectors and symbols must be distinct, and a root export cannot coexist with field exports.
Each symbol is an unmangled ASCII C identifier; the `zydeco_` prefix is reserved for runtime support.
Only the selected symbols are public.
Export a constant through a zero-argument returning thunk; global data exports have no contract in this profile.

Independent selection follows the ordinary lexical binding and inference rules
at [source boundaries](#source-boundaries).
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

The export classifier is `Thk (I1 -> ... -> In -> Ret R)`, with zero through six supported integer parameters
and a supported integer or `Unit` result.
It shares the import signature's widths and scalar conversions, but reverses the transport direction.
Incoming pointer adapters and their binding-specific ownership contracts remain unimplemented;
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

### Native Zydeco Units

`@[package(library(zydeco), name(example/math))]` selects a native library
whose initializer returns the complete prepared source value.
Select the intended public value in source; all its fields participate in the interface.
Preparation accepts a closed value or one leading value function over a validated Builtin contract,
applies that provider statically, and checks the resulting value, captures, and reachable computations for readiness.
An initializer materializes the value without forcing its exported thunks.

The initial interface profile supports `Unit`, the supported integers, `Float32`, `Float64`, `Char`, `String`,
named value types, n-ary products, and `Thk B`, where `B` consists of arrows and `Ret`.
These forms can nest, so an exported function may accept a thunk or return a capturing thunk.
Product order, nesting, names, scalar widths, and all argument/result classifiers remain in the interface.
Nominal data, codata, abstract types and capabilities, universals, existentials, witness-dependent arrows,
and static functions are rejected in public classifiers in this profile.
Their use inside an implementation remains governed by ordinary typing and static elimination.
The interface has a maximum structural depth of 48 and 4,096 nodes.

A generated `.imports.zy` is a self-contained declaration
of type `Thk (Ret Exports)` using `ffi(zydeco, library("example.math"), symbol("..."))` and canonical type intrinsics.
Its symbol is generated by the compiler. Only that initializer classifier is accepted for a `zydeco` import;
ordinary exported functions are obtained from the returned value and invoked through their source types.
Consumers supply the matching manifest with `--link-library` before native linking or execution.

Initialization is explicit:

```text
let initialize = @(import("build/example.math.imports.zy")) in
do math <- ! initialize;
do add_seven <- ! math/make_adder 7;
! add_seven 35
```

Forcing `initialize` again materializes another export value.
A consumer wanting one shared initialization binds the result once and passes or captures it normally.
Importing the generated declaration alone does not initialize anything.
Source imports retain their ordinary execution behavior; there is no implicit module cache or initialization scheduler.

Units and their consumers execute within one native runtime instance.
Returned values and closures can survive initializer returns and subsequent collection through ordinary source roots.
The process owns the heap, resource provider, frame store, and code lifetime;
an initializer neither creates nor destroys an instance.
Runtime faults retain the native process's fatal behavior.
The initializer symbol is a Zydeco stack entry, not an ordinary C-callable function.

Publication currently supports AMD64 Linux and macOS objects through `build --target object`.
Consumers use the native `exe` backend; the interpreter and Wasm reject native unit execution.
Units require matching compiler, runtime model, runtime source, target, and entry profile.
Native unit dependencies are supported; dependencies between native units
and C library artifacts inside a library remain unsupported.
A process may separately link both kinds of library.
The [workflow](../../CONTRIBUTING.md#compile-and-consume-native-zydeco-units) shows the commands;
the [compiler contract](compiler.md#native-unit-artifacts) owns artifact and linkage details.
Broader type interfaces and host-language bindings remain in the [ABI proposal](../proposals/zydeco-abi.md).

## 15. Execution Profiles

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

### Selecting an Execution Backend

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

## Meta Annotations (Compile-Time Metadata)

Meta annotations are written `@[meta] e`; `@(meta)` abbreviates a hole payload.
These forms supply compile-time metadata to the compiler.
An annotation expression is a name, string, integer, or a named application with comma-separated arguments.
Runtime metadata, such as a buffer length or object vtable pointer,
is ordinary value data and follows the usual checking and erasure rules.
An ordinary layout descriptor is also a typed value; evaluating it during checking does not make it a meta annotation.

| Meta annotation | Meaning and valid use |
| --- | --- |
| `import(source)` | Import an independently checked term (§12); source selection follows [package resolution](../proposals/package-resolution.md) |
| `package(role, ...)` | Define a package's role and relationships (§12); names and package context follow [package resolution](../proposals/package-resolution.md#package-hierarchy) |
| `discover(include("glob", ...), exclude("glob", ...), ...)` | Declare candidate source files; project integration follows the [package plan](../proposals/package-management.md#shared-project-context) |
| `intrinsic(role)` | Supply a canonical kind/type (`vtype`, `ctype`, `thk`, `ret`, `unit`, `int`, `uint`, `i8`/`i16`/`i32`, `u8`/`u16`/`u32`, `f32`, `f64`, `char`, `string`) or an integer value function (§8) |
| `builtin(role)` | Mark a host capability or operation in a typed Builtin interface (§13) |
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

### Source Documentation

Zydeco documentation combines Markdown attached to source terms with the compiler's information
about bindings, imports, and named fields.
Compiler provenance connects those explanations to hover, completion, and the VS Code documentation panel.
The [documentation workflow](compiler.md#documentation-workflow) describes editor use;
the [documentation proposal](../proposals/documentation.md) develops reference generation and lookup.

#### Attaching Documentation

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

#### Semantic Documentation Links

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
Editors navigate to the resolved source target. Package and subject selection follow the
[shared selection model](../proposals/package-resolution.md#shared-selection-and-documentation).
Guide contexts and published links follow the [documentation proposal](../proposals/documentation.md).

## Diagnostic Index

| Diagnostic family | Relevant rule |
| --- | --- |
| Missing annotation/solution, unconstrained inference, occurs check | Classification and inference (§4) |
| Invalid binding cycle, missing seal | Bindings and nominal recursion (§3–§4) |
| Sort/kind/type mismatch, unknown constructor/destructor | Classification and introduction/elimination (§4–§6) |
| Refutable binding/alias/projection, coverage, overlapping copatterns | Patterns and coverage (§7) |
| Missing/ambiguous/sealed field, unavailable/escaping witnesses | Witness scope and field selection (§9) |
| Static elimination | Residual representation and reduction limits (§10) |
| Integer/float literal range errors | Primitive representation (§13) |

Rejection examples name stable `tyck.*` codes and a source position.
Diagnostic wording and rendering are implementation details.
