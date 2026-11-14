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
11. [Translations into the Core](#11-translations-into-the-core)
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

Examples use complete source terms or the displayed metavariables and an explicitly described context.
The surrounding prose identifies intentional rejection cases.
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
| `f <\| v` | Right; the right operand is a full term |
| `pi`, `val pi`, `forall`, `sigma`, `exists` | Binder bodies extend rightward |
| Abstractions, bindings, metadata | Extend over their following term |

Thus `!p/action x` means `(! (p/action)) x`.
The thin arrow `->` constructs a classifier; `=>` separates a function or matching header from its body.
Backward application, also called the backward pipe, accepts an abstraction, quantifier,
or binding directly after `<|`: `f <| fn x => body` groups as `f (fn x => body)`.
The abstraction's body extends rightward as usual.
Parenthesize the completed application when supplying another argument, as in `(f <| fn x => body) next`.

Annotations and named components have a separate grouping layer: `:` is non-associative
and binds more tightly than the right-associative `=` and `::` naming forms.
Expression annotations use parentheses, `(e : S)`, or the contents of a `begin ... end` block.
Write `((#x = 1) : (#x :: Int))` to annotate the whole named value; `(#x = 1 : Int)` annotates its payload.
Metadata in a juxtaposed application argument also needs parentheses; backward application admits it
as a full right operand.

The `#` marker keeps a field name from reading as a binder.
`=` already marks binding boundaries and both introduces and eliminates named components,
while field names draw on the same lexical space as variables.
Without a marker, a name on the left of `=` or `::` could be told apart as a field only by sort information.
Marking exactly those positions leaves announced positions bare: projections and puns carry `/`
or a leading `=`, so their field name is already identified.
The marker also completes the role-sigil family with `+Ctor` and `.dtor`.
Marking every field occurrence, including projection and pun positions, was rejected as redundant;
a distinct field connective would split the connective family; `.field` would collide with destructor lexing;
and restricting field names by case would break field puns, whose public name follows the payload's naming convention.

## 3. Bindings and Scope

`param p in e` introduces a type or computation abstraction; `param val p in v` introduces a value function.
`let p = e in body` binds a type or value transparently.
`def p = e in body` additionally seals a type definition with a nominal identity.
Binders scope over the tail; ordinary lexical bindings may shadow enclosing names.

A value binding normally has a value or computation continuation.
A static-only package opening may also return a kind or type: its checked pattern selects static components
and discards the runtime payload, introducing no runtime value binder or view pattern.
For example, `let (/Int) = @(import("../../lib/std/primitive.zy")) in Int` returns the manifest type.
The ordinary witness-escape check still rejects a result containing an abstract witness introduced by that opening.
An unrelated value binding such as `let value = 1 in Int` does not become a type-level binding.

When every selected witness is manifest, the opening introduces no runtime binding or payload evaluation.
An opening that introduces abstract witnesses retains the evidence needed for static specialization.

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

```zydeco
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
Identity across imported source copies follows the [package-resolution model](#copy-resolve-merge-analyze).

Checking is bidirectional. Annotations supply expected classifiers; synthesis determines them from a term.
A bare unannotated variable in a synthesizing binder position defaults to a value binder with a flexible value type.
Body uses and compatible calls within the same inference region constrain that type.
Inference is order-independent within the region and closes at a block or source boundary,
where every flexible classifier introduced by that region must be solved.
There is no automatic let-polymorphic generalization.

```zydeco
begin
  let identity = { fn x => ret x } that
  ! identity ()
end
```

Without a constraining use or annotation, the parameter is unconstrained, so this term is rejected
with `tyck.unconstrained-inference`:

```zydeco
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

```zydeco
@[typeof] (let value = 1 in value)
```

This term is rejected with `tyck.sort-mismatch`:

```zydeco
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

```zydeco
let point = (#x = 3, #y = 4) in
ret (point/x, point/y)
```

A data type `data | +C1 : A1 ... | +Cn : An end` classifies constructor values `+Ci vi`,
where `vi : Ai`. Each constructor has one payload, which may itself be a product or constructor.
Names are interpreted against the expected data type.
Name and parameterize a data type with an ordinary definition.

```zydeco
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

```zydeco
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

```zydeco
@[partial] let 0 = 0 in ret ()
```

This term is rejected with `tyck.refutable-binding`:

```zydeco
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

```zydeco
let ((left, right); whole) = (1, 2) in ret whole
```

This term is rejected with `tyck.refutable-pattern-alias`:

```zydeco
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

```zydeco
let val duplicate (x : @(intrinsic(int))) = (x, x) in
ret (4 |> duplicate)
```

`f v`, `v |> f`, and `f <| v` are the same application; the directional spellings also apply to type functions.
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

```zydeco
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

```zydeco
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

```zydeco
let Int = @(intrinsic(int)) in
let package =
  pack (= Item as Int)
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

This term is rejected with `tyck.missing-named-field`:

```zydeco
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
Remaining questions concern [kind-witness introduction](../ideas/README.md#kind-witness-introduction),
[computation witness routes](../ideas/README.md#computation-witness-routes),
and [companion generation](../ideas/README.md#companion-interface-generation).

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

```zydeco
val (x : @(intrinsic(unit))) => x
```

This term is rejected with `tyck.static-elimination`:

```zydeco
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

## 11. Translations into the Core

Two meta annotations give their payload a reading other than the core's own and translate it
into the core; a third is reserved for another.
`@[monadic] e` reinterprets the returners in `e` through a relative monad that the block takes as a parameter.
`@[cbv] e` reads `e` as a call-by-value expression and elaborates it, writing its evaluation order into the result.
`@[cbn] e` is reserved for the call-by-name counterpart and is refused until that exists.
Each translation runs in one direction and produces ordinary core syntax, which is checked,
linked and run like any other; that is also why they compose.
A call-by-value block nested in a monadic block is translated by both, and that composition is
where a call-by-value block's evaluation order meets effects.

### Relative Monads

A relative monad has carrier `M : VType -> CType`.
The library's `Monad M` is codata with operations `.return : forall (A : VType) . A -> M A`
and `.bind : forall (A : VType) (B : VType) . Thk (M A) -> Thk (A -> M B) -> M B`.
An `Algebra M R` is the computation type `forall (A : VType) . Thk (M A) -> Thk (A -> R) -> R`.
These are ordinary library interfaces; their types do not enforce algebraic laws.

### Relative Monad Laws

A lawful dictionary preserves both answers and observable effects when sequencing is reassociated.
Write `return_M a` for its return operation and `bind_M m f` for its bind,
omitting type arguments and writing braces for thunks.
The two unit equations and associativity are:

```text
bind_M { return_M a } f                         ≃ f a
bind_M m { x => return_M x }                    ≃ force m
bind_M { bind_M m f } g                         ≃ bind_M m { x => bind_M { f x } g }
```

Linearity additionally preserves the effects of producing the first computation thunk:

```text
tt : Thk (Ret (Thk (M A)))

do t <- force tt; bind_M t f
  ≃ bind_M { do t <- force tt; force t } f
```

This equation concerns bind's first computation argument, not a global linear-use restriction on thunks.
The equalities are observational, including ambient effect order rather than only returned values.
The [relative-monad paper, §4.2](https://arxiv.org/html/2502.15031v2#S4.SS2), gives the semantic account.
Checking a `Monad` dictionary verifies its classifier, not these equations.

### Monadic Blocks

`@[monadic] e` selects the lexically visible `Monad` and `Algebra` constructors
and performs sort-preserving algebra translation.
Write `L_M` for translation under carrier `M` and its monad implementation.
The annotation synthesizes its operand once, then supplies an outer interface appropriate to that operand's sort:

| Operand | Generated interface | Result classifier |
| --- | --- | --- |
| Type `T : K` | `fn (M : VType -> CType) => L_M(T)` | `(VType -> CType) -> K` |
| Value `v : A` | `val (M : VType -> CType) (mo : Thk (Monad M)) => L_M(v)` | `val pi (M : VType -> CType) (mo : Thk (Monad M)) . L_M(A)` |
| Computation `e : B` | `fn (M : VType -> CType) (mo : Thk (Monad M)) => L_M(e)` | `forall (M : VType -> CType) . Thk (Monad M) -> L_M(B)` |

Type operands include value types, computation types, and higher-kinded type constructors.
Their translation needs the carrier but no runtime dictionary argument.
Value operands include thunks, value functions, and ordinary value packages used as modules;
translation preserves them as values rather than inserting a `ret` or forcing suspended computations.
Kinds have no operand translation and are rejected with `tyck.sort-mismatch`.
The operand receives no expected classifier from outside the annotation; an ascription inside it applies normally.

Source `Ret`, `ret`, and `do` are reinterpreted through the supplied carrier and its operations.
Parameters outside the annotation remain outside; translated parameters inside follow the generated interface.
Polymorphic translation may require additional structure arguments describing how a type supports the translation.
For example, `let MonadicMonad = @[monadic] (Monad Ret)` derives the dictionary classifier:
`MonadicMonad M` has the translated operations, including a `Thk Top` argument after each value-type quantifier.
Likewise, `(@[monadic] { ret 1 }) M mo` is a value of type `Thk (M Int)`.

```zydeco
param (/Ret; /Thk; builtin) : @(import("../../lib/std/builtin.zy")) in
let basis = @(import("../../lib/std/effect/monad.zy")) in
let (/Monad; /Algebra) = basis in
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

### Packed Effect Instances

An effect-polymorphic client needs a carrier, its monad dictionary, and operations at that same carrier.
The [effect library](../../lib/std/effect) separates carrier-indexed operations
from existential packaging so several effects can share one carrier.
`IState S M` contains `state : Thk (MonadState S M)`; `IException E M` contains `exception : Thk (MonadThrow E M)`
and a `catch` operation at `M`.
Neither operation bundle introduces a carrier or a monad dictionary of its own.

The carrier and its required dictionary form one packed argument.
The shared `MonadInstance` interface carries the dictionary as its unnamed payload,
without additional effect operations:

```zydeco
exists (M : VType -> CType) . Thk (Monad M)
```

`WithMonad I` packages one leading positional witness `M : VType -> CType`,
a named `monad : Thk (Monad M)` dictionary, and an operation payload of type `I M`.
Standalone clients use `WithMonad (IState S)` or `WithMonad (IException E)`.
The combined signature `IStateExn S E` composes the operation bundles before packaging them:

```zydeco
WithMonad <| fn (M : VType -> CType) =>
  IState S M * IException E M
```

Both bundles therefore use the same `M` and the surrounding package's single monad dictionary.
A product of independently packaged interfaces would instead introduce independent witnesses.
These signatures constrain carrier agreement, not the algebraic laws of the supplied operations.

A client whose result uses that carrier has a [witness-dependent signature](#9-polymorphism-and-packed-values),
such as `pi ((M, _) : WithMonad (IState Int)) . M Int`.
Its parameter pattern opens the carrier and operation payload once;
repacking `(M, ops)` forwards the same witness to other clients.
The packed boundary is an ordinary library interface.
An adapter inside the client supplies the carrier, monad dictionary, and operations
to the existing curried interface generated by `@[monadic]`.

Each concrete `instance` value function exposes a manifest carrier equation.
Open that result and explicitly reconstruct the argument expected by an abstract instance signature:
`let (M, ops) = instance Int in ! client (M, ops)`.
This keeps the concrete equation available to the caller's runner while the client sees only its declared operations.
The generic helpers `modify`, `raise`, and `catch` use these packed boundaries;
the runners observe the concrete carriers and remain outside the effect interfaces.
The [library walkthrough](../tutorial/zydeco-guide.md#10-capability-dictionaries) shows a complete adapter.

The operation interfaces leave effect interaction to the chosen carrier and handlers.
The [State and Exception composition contract](#state-and-exception-composition) specifies the standard
combined instance and contrasts it with a state-preserving failure stack.

### Derived Relative Monad Transformers

A transformer should preserve the source effect's sequencing rather than introduce a second implementation of it.
The standard library derives transformers from closed ordinary relative-monad definitions with `@[monadic]`,
following the [relative-monad paper, §5.4, Figures 13–14](https://arxiv.org/html/2502.15031v2#S5.SS4).
The source dictionary and effect operations use ordinary `Ret`, `ret`, and `do`.
Algebra translation supplies their interpretation over a base carrier `M` and its `Monad M` dictionary.
`Ret` itself remains the bootstrap instance.

The [shared transformer interface](../../lib/std/effect/transformer.zy) separates a transformer family
from its result at one base:

```text
Transformer M T = codata
  .monad : Monad T
  .lift  : forall A. Thk (M A) -> T A

MonadTrans F = pi ((M, _) : MonadInstance) . Transformer M (F M)
```

Here `F : (VType -> CType) -> VType -> CType` maps a base carrier to a transformed carrier.
Each effect exports `transformer`, with effect configuration
before one [`MonadInstance`](#packed-effect-instances) argument `(M, base)`.
For example, `{ ! reader/transformer R }` has type `Thk (MonadTrans (ReaderT R))`,
and `{ ! writer/transformer W output }` has type `Thk (MonadTrans (WriterT W))`.
The witness-dependent arrow opens the caller's chosen base once and retains that identity in both `M`
and `F M` in the result; the implementation may remain an unknown runtime thunk.
The interface constrains the carriers of the dictionary and lift; lawfulness remains a semantic obligation.
For lawful source and base monads, lift preserves return and bind:

```text
lift (return_M a)       ≃ return_T a
lift (bind_M m f)       ≃ bind_T { lift (force m) } { x => lift (f x) }
```

The common [source helper](../../lib/std/effect/transformer-source.zy) pairs a source dictionary
with `lift A computation = do a <- ! computation; ! source_monad .return A a`.
Import that helper inside the closed translated payload together with the source implementation.
Translation acts on both, generating the transformed dictionary and lift at once.
It does not inspect an opaque dictionary passed from outside the annotation.
Dependencies must be bound as parameters inside the payload, as required
by the [closed translation boundary](#closed-translation-boundary).

The adapter's input classifier is itself derived by translating `fn (T : VType -> CType) => Transformer Ret T`
as a type root.
This keeps the translated monad and lift signatures synchronized with their ordinary definitions.
Each standard effect similarly shares one closed, module-local source carrier, such as `ReaderSource`
in [Reader](../../lib/std/effect/reader.zy) or `NonDetSource` in [NonDet](../../lib/std/effect/nondet.zy),
between its public type derivation and its implementation translation.
Configured effects reorder the generated type arguments to keep configuration before the base carrier.

`from_monadic` supplies the trivial `Top` arguments introduced for value-type quantifiers
on the generated monad and lift.
Each effect similarly adapts its generated operation interface.
These adapters retain meaningful structure evidence: in particular, a computation-type quantifier
in the source introduces an `Algebra M` argument that cannot be discarded.
The NonDet source quantifies over a computation answer `R` without an algebra parameter;
translation produces the algebra-bearing [public carrier](#ordered-depth-first-nondeterminism).
The public concrete instance specializes the derived carrier to `Ret`, which need not be definitionally identical
to the untransformed source carrier.

A `transformer_instance` value function exposes the manifest transformed carrier,
one `monad` dictionary, its `lift`, and the effect's own operations.
`Lift M T` abbreviates the lift classifier above; `ITransformer M T` contains only `lift : Thk (Lift M T)`.
It can combine with effect bundles under `WithMonad`, without another carrier or monad dictionary.
An ordinary effect client receives an explicit projection of the fields its interface requests;
for example, pass `(T, #monad = t/monad, #state = t/state)` to `WithMonad (IState S)`.
A lift-aware client instead uses `WithMonad (ITransformer M)` or a composition of that bundle with effect operations.
Concrete `instance` builders specialize to `Ret` and project the ordinary effect interface.
The computation-level `transformer` is a view of the same derived instance, not a separate dictionary implementation.
It opens its packed base and forwards the curried `M base` arguments to `transformer_instance`.
The generated `@[monadic]` interface also remains curried; packaging belongs to the library boundary,
with no definitional equality between the packed and curried classifiers (§9).

External dependencies remain explicit at the translation boundary.
Writer supplies its source with an empty value and an append operation;
the module adapts the existing `Ret`-returning append by running it and returning its answer through the base monad.
This retains the [Writer output contract](#writer-output-contract),
rather than accepting arbitrary effectful base-monad append operations.
NonDet keeps its shared data package and list observations outside the translated search core,
preserving the caller's existing `List` witness.

The [uniform-interface fixture](../../lib/tests/effects/transformer-operations.zy) exercises all
six transformer families through `MonadTrans` and a packed lift-aware client.
The [classifier tests](../../lang/tests/tests/transformers.rs) pair closed source derivation
and matching carrier witnesses with rejection of captured dictionaries, mismatched lifts,
and missing answer-type algebras.

### Elementary Effect Instances

The [effect library](../../lib/std/effect) exports elementary effects independently of OS capabilities.
Except for the bootstrap `Ret` instance, each concrete instance specializes its derived transformer
to the reusable `Monad Ret` dictionary.
`Ret` is the ambient computation carrier, not a separate pure identity effect.
These modules are imported explicitly rather than aggregated by `std.zy`.

| Module | Concrete carrier | Instance entry point | Carrier-indexed operation bundle |
| --- | --- | --- | --- |
| [`ret.zy`](../../lib/std/effect/ret.zy) | `Ret` | `instance` | Only `monad` |
| [`reader.zy`](../../lib/std/effect/reader.zy) | `Reader R A = R -> Ret A` | `instance R` | `IReader R M`: `reader : Thk (MonadReader R M)` |
| [`writer.zy`](../../lib/std/effect/writer.zy) | `Writer W A = Ret (A * W)` | `instance W output` | `IWriter W M`: `writer : Thk (MonadWriter W M)` |
| [`option.zy`](../../lib/std/effect/option.zy) | `Optional A = Ret (Option A)` | `instance` | `IOption M`: `option : Thk (MonadOption M)` |
| [`state.zy`](../../lib/std/effect/state.zy) | `State S A = S -> Ret (A * S)` | `instance S` | `IState S M`: `state : Thk (MonadState S M)` |
| [`exception.zy`](../../lib/std/effect/exception.zy) | `Exception E A = Ret (Either E A)` | `instance E` | `IException E M`: throw and catch at `M` |

The value `Option` is the existing [data-library type](../../lib/std/data/option.zy).
`Optional` names its computation carrier, so a client does not confuse a value datatype with a relative monad.
Concrete builders disclose their carriers.
As with State and Exception, open an instance and repack `(M, operations)` for a client taking `WithMonad`
of the corresponding bundle.

Reader's `.ask` returns the current environment.
Its `.local A replacement computation` runs only the protected computation with a replacement environment.
The surrounding environment is restored after that computation.
The replacement is a value, not an environment-mapping function.
`run_reader R A environment computation` supplies the initial environment and returns `Ret A`.

Writer's `.tell output` emits an output with a Unit answer.
Its `.listen A computation` returns the protected computation's answer and output together,
while retaining that output in the surrounding Writer computation.
Bind combines the first computation's output before the continuation's output.
`run_writer W A computation` returns the answer and accumulated output as `Ret (A * W)`.

Optional's `.none A` produces an absent answer.
Bind runs its continuation on `Some` and propagates `None` without running it.
Its `.or_else A computation fallback` executes the fallback only after `None`.
`run_option A computation` returns the existing `Option A` observation.
Short-circuiting and fallback do not undo effects already performed by the protected computation.
These operations make no fairness or distributivity promise.

#### Writer Output Contract

[`Monoid W`](../../lib/std/algebra/monoid.zy) contains `empty : W` and `append : Thk (W -> W -> Ret W)`.
For Writer, append must be total and observationally pure, associative, and have `empty` as a two-sided identity.
Commutativity is unnecessary: output order is significant. The computational operation supports runtime-selectable
and recursive implementations beyond the current total value-function fragment,
but its classifier cannot establish the purity or algebraic obligations.

Purity matters because reassociating Writer bind changes when intermediate outputs are combined.
An append that prints can move those prints relative to user computations even
if its returned values obey the ordinary monoid equations.
Such a dictionary is well-typed but outside the Writer contract.
String concatenation with the empty string is a useful noncommutative instance.
No complexity guarantee is imposed on a caller's append implementation.

#### Elementary Transformers

The transformer carriers retain the underlying base computation:

```text
ReaderT R M A    = R -> M A
WriterT W M A    = M (A * W)
OptionT M A      = M (Option A)
StateT S M A     = S -> M (A * S)
ExceptionT E M A = M (Either E A)
```

Exception uses the data library's shared [`Either E A`](#either-values),
with `Left E` for errors and `Right A` for successful values.
The builders share the [derived transformer interface](#derived-relative-monad-transformers):

| Module | Packed builder | Base-aware observation |
| --- | --- | --- |
| Reader | `transformer_instance R M base` | `run_reader_t R M A environment computation : M A` |
| Writer | `transformer_instance W output M base` | `run_writer_t W M A computation : M (A * W)` |
| Option | `transformer_instance M base` | `run_option_t M A computation : M (Option A)` |
| State | `transformer_instance S M base` | `run_state_t S M A initial computation : M (A * S)` |
| Exception | `transformer_instance E M base` | `run_exception_t E M A computation : M (Either E A)` |

Here `base : Thk (Monad M)` and `output : Monoid W`.
Each builder supplies its own effect operations; it does not automatically lift other interfaces from the base.
The instance's `lift A computation` ignores Reader's environment, attaches Writer's empty output,
wraps Optional's answer in `Some`, preserves State's state, or wraps Exception's answer in `Right`, respectively.
The runners expose the carrier equations without introducing additional sequencing policies.

Base effects preceding `None` remain observable in OptionT.
For example, `OptionT (Writer W) A` can retain output with an absent answer,
whereas `WriterT W Optional A` has no returned output when its base computation produces `None`.
This difference follows from the carrier equations, not from a common rollback policy.

The [operation fixture](../../lib/tests/effects/effect-operations.zy) exercises scoped environments,
noncommutative output, lazy fallback, and transformer order.
The [law fixture](../../lib/tests/effects/effect-laws.zy) checks the four monad equations
and lift laws using an ambient mutable trace.
Its [counterparts](../../lib/tests/effects/effect-law-counterexamples.zy) detect repeated execution
and effectful append despite successful typechecking.
The [translated clients](../../lib/tests/effects/effect-monadic.zy) demonstrate closed adapters,
including the structure arguments required for polymorphic operations.
These finite tests are regression evidence, not proofs for arbitrary dictionaries.

#### State and Exception Composition

StateT's `.get` and `.put` use the current state, and bind passes the state returned
by the first computation to its continuation.
ExceptionT's `.throw` returns `Left`; bind skips its continuation on that case.
Its `catch` runs a handler only for `Left` and preserves a successful `Right` without invoking the handler.
Ambient and base effects already performed remain observable through recovery.
The [derived-law fixture](../../lib/tests/effects/transformer-laws.zy) checks both ordinary instances,
their transformers over Writer, and their lifts with exact ambient traces.

The standard combined carrier is `StateExn S E A = StateT S (Exception E) A`, or `S -> Ret (Either E (A * S))`.
It composes the derived State transformer with Exception rather than maintaining another bind implementation.
Failure discards the updated state, and its catch handler restarts from the state at entry to the protected computation.
The [combined example](../../lib/tests/effects/state-exception-stack.zy) tests this policy.

Reversing the stack gives `ExceptionT E (State S) A = S -> Ret (Either E A * S)`.
The state remains observable on failure, and a catch handler continues with that updated state.
The [translated stack client](../../lib/tests/effects/transformer-monadic.zy) distinguishes these policies
through the same packed operations;
the [short-circuit fixture](../../lib/tests/monadic/nominal-short-circuit.zy) also admits the state-preserving carrier.
Neither policy follows from the combined operation interface alone.

### Ordered Depth-First Nondeterminism

A search can produce an answer before exploring its remaining alternatives.
The [NonDet library](../../lib/std/effect/nondet.zy) represents that remaining work as a continuation,
so bind can explore the descendants of one answer before requesting another.
The standard instances preserve branch order and duplicates and use depth-first traversal.
They provide neither fair interleaving nor implicit pruning or deduplication.

The module exports a `make` value-function factory taking Builtin
and then the caller's shared [`DataPackage`](../../lib/std/data/package.zy).
Its list inputs and observations use that package's `List` witness and operations;
the factory does not create a separate list type.
The carriers are:

```text
NonDetT M A = forall (R : CType) .
  Thk (Algebra M R) -> Thk (A -> Thk R -> R) -> Thk R -> R
NonDet A = NonDetT Ret A
```

The observer chooses an answer protocol `R` and supplies its base-monad algebra.
The success continuation receives one answer and a thunk for the remaining search at `R`;
the failure continuation describes exhaustion.
Bind replaces success with a continuation that explores the selected answer's descendants,
passing the remaining alternatives as their failure continuation.
The rest is an ordinary reusable thunk, not a linear or memoized cursor.
Repeated forcing repeats its computation and effects; the standard observers force each visited rest once.

`MonadNonDet M` has `.empty : forall (A : VType) . M A`
and `.choice : forall (A : VType) . Thk (M A) -> Thk (M A) -> M A`.
`INonDet M` contains `nondet : Thk (MonadNonDet M)` and combines with the shared monad dictionary
under `WithMonad INonDet`.
`instance` exposes `NonDet`; `transformer_instance M base` exposes `NonDetT M`, its dictionary,
lift, and search operations through the [common derivation](#derived-relative-monad-transformers).
The shared constructor takes `base : Thk (Monad M)` even though search sequencing and choice compose continuations.
The observation supplies the algebra used to interpret lifted base computations.

The standard `.choice A left right` starts with `left` and suspends `right`
until the left search and its descendants are exhausted.
`.empty A` invokes failure without producing an answer.
`choose instance A values` visits the supplied list in order, including duplicates;
`guard instance condition` produces one Unit answer for true and no answer for false.
Choice is associative with empty as a two-sided identity.
Bind distributes over choice in its first argument and skips its continuation after empty.
These laws do not imply commutative or idempotent choice, or that discarding all answers undoes effects.

The concrete observers are:

| Operation | Observation | Exploration boundary |
| --- | --- | --- |
| `observe_first A computation` | `Ret (Option A)` | Stops at the first answer; returns `None` on exhaustion |
| `observe_many A limit computation` | `Ret (List A)` | Stops after at most `limit` answers; a nonpositive limit does not force the computation |
| `observe_all A computation` | `Ret (List A)` | Explores to exhaustion, retaining order and duplicates |

The bound in `observe_many` limits answers, not execution steps.
An infinite or diverging left branch can starve later branches; even `observe_first` may therefore diverge.
`observe_all` requires the explored search to terminate to return its list.
Stopping after an answer does not force the rest merely to determine whether another answer exists.

Each `_t` observer takes `M base` before the concrete observer's arguments
and returns the corresponding `M (Option A)` or `M (List A)`.
Here `base : Thk (Monad M)` constructs the algebra for that observer's protocol.
The instance's `lift A computation` uses the algebra passed to the search
to execute the base computation and yield its answer.
With lawful base dictionaries and algebras it preserves return, bind, and base-effect order.
Custom observers that call the carrier directly must supply a lawful `Algebra M R`.

Explored ambient and base effects remain observable when a branch later produces no answers.
For example, `NonDetT (Writer W)` accumulates the output of visited branches, including failed branches,
whereas `WriterT W NonDet` attaches output to each surviving answer.
A failure in the base carrier is distinct from search exhaustion: lifting `None`
from Optional propagates that failure rather than trying the next search alternative.
No operation automatically rolls back state or other effects.

#### Lists and Search Observations

Collecting answers is an observation, not the representation used by search bind.
The eager candidate `Ret (List A)` executes a continuation on every element before yielding the combined list.
For input `[1, 2]`, let `f i` emit `F_i` and return one result, and let `g` emit `G_i` on that result.
Left-associated bind emits `F_1, F_2, G_1, G_2`; right-associated bind emits `F_1, G_1, F_2, G_2`.
Equal lists therefore do not establish associativity in the presence of ambient effects.
The [relative-monad paper, §5.4](https://arxiv.org/html/2502.15031v2#S5.SS4),
identifies the commutativity restriction on this returned-list representation.
NonDet imposes no such commutativity restriction on its base,
and collecting its answers is not generally a monad-preserving conversion to returned lists.

The [operation fixture](../../lib/tests/effects/nondet-operations.zy) checks ordering, duplicate preservation,
repeated observation, lazy branches, and bounded observation of a productive infinite search.
The [law fixture](../../lib/tests/effects/nondet-laws.zy) checks branching monad and choice laws,
linearity, and lifts over Reader and Writer.
The [returned-list counterexample](../../lib/tests/effects/nondet-law-counterexamples.zy) is well-typed
but distinguishes the associativity traces above.
[Transformer interactions](../../lib/tests/effects/nondet-transformers.zy)
and [translated clients](../../lib/tests/effects/nondet-monadic.zy) exercise the other boundaries described here.

### Closed Translation Boundary

Algebra translation is defined on closed payloads.
Supply external runtime values and operations through function parameters bound inside the annotation;
supply external types and type constructors through type abstractions inside it.
Instantiating the translated function supplies those arguments after the carrier and, for value or computation roots,
the monad implementation, together with any structure arguments introduced by translation.
This parameter-passing boundary is intentional.
Implicit runtime capture and importing algebra evidence from the enclosing lexical environment are outside the design.

Closure is measured after resolving the fixed language intrinsics and expanding available closed definitions.
The lexically selected `Monad` and `Algebra` constructors configure the translation itself.
A source name for a closed, inlinable definition can therefore be reinterpreted with the payload;
an enclosing runtime variable or abstract type witness is not such a definition.
In particular, knowing an external type's name does not supply its algebra.
Pass that type through a type parameter of the translated function.

The implementation rejects non-inlinable free term references, but currently retains lexical type bindings,
including existential witnesses.
Some direct uses of those types still pass checking;
[closure validation](../todos/compiler-boundaries.md#monadic-closure-validation) tracks this discrepancy
with the approved boundary.
It is not a planned mechanism for importing external structure evidence.

### Polymorphic Value Functions

A translated value function may assemble suspended code that sequences into an abstract computation type.
Translation supplies that type's algebra at the value-function binder,
using the same structure-passing convention as computation polymorphism.
The value function still disappears through [static elimination](#10-static-elimination).

Write `Sig_M,K(T)` for the computation type of the structure required by a target type `T : K`.
The existing kind-directed signature rules are:

```text
Sig_M,VType(T)    = Top
Sig_M,CType(T)    = Algebra M T
Sig_M,K1->K2(F)  = forall (X : K1) . Thk (Sig_M,K1(X)) -> Sig_M,K2(F X)
```

`Top` is empty codata; named kinds use their payload projection.
Let `Str_M(T)` construct evidence of type `Sig_M,K(L_M(T))` from the source type `T`.
Each type parameter introduces exactly one following structure parameter, including the trivial structure at `VType`:

```text
L_M(val pi (X : K) . A)
  = val pi (X' : K) (s_X : Thk (Sig_M,K(X'))) . L_M(A)
L_M(val (X : K) => v)
  = val (X' : K) (s_X : Thk (Sig_M,K(X'))) => L_M(v)
L_M(f T)
  = (L_M(f) L_M(T)) { Str_M(T) }
```

The body uses the renamed witness `X'` and the structure bound by `s_X`.
Named type patterns retain their shape. Ordinary value parameters still receive one translated argument;
parameters that open packages use the structures already carried by those packages.
This uniform interface supports annotated, partially applied, and higher-order value functions
without requiring source-level structure arguments at calls inside the block.

For example, this local factory can sequence into `B` even before the caller instantiates it:

```zydeco
@[monadic] begin
  let val make (B : CType) = {
    fn (k : Thk B) => do _ <- ret (); ! k
  } in
  ! (make (Ret Int)) { ret 1 }
end
```

Its translated classifier is schematically `val pi (B : CType) (alg_B : Thk (Algebra M B)) . Thk (Thk B -> B)`.
The application supplies `M Int` and the structure for the source `Ret Int`.
The selected monad implementation may remain runtime data:
static specialization forwards the structure thunk without executing it.
Unknown value-function implementations still fail at a runtime boundary.
An [inference discrepancy](../todos/compiler-boundaries.md#inferred-higher-order-value-function-witnesses)
currently requires an explicit signature for some higher-order factories even without monadic translation.

### Applicative Nominal Translation

A closed monadic payload can define its own nominal data and codata types,
including parameterized and mutually recursive definitions.
Translation preserves their constructors, patterns, and observations through a nominal type family
for each source declaration.
Transparent aliases retain their existing behavior.

The family takes the carrier, the selected `Algebra` type constructor,
and the declaration's free type dependencies as explicit type arguments.
The `Algebra` parameter accounts for structure fields introduced into existential packages
and polymorphic value functions.
Equal arguments select the same nominal identity; distinct source declarations remain distinct.
Choosing `Ret` does not identify a translated declaration with its untranslated source.

Write `L` for translation with carrier `M` and algebra constructor `G`.
For a local nominal definition `N` with body `T` and free type dependencies `D1 ... Dn`:

```text
L(N)          = N_alg M G L(D1) ... L(Dn)
body(N_alg)   = fn M G D1 ... Dn => L(T)
kind(N_alg)   = (VType -> CType) -> kind(G) -> kind(D1) -> ... -> kind(Dn) -> kind(N)
L(N U)        = L(N) L(U)
```

The parameters on the right bind the translated dependencies in `L(T)`.
Recursive references reuse the same family, including at different type arguments.
A shared closed definition retains its translated identity across blocks, and nested translation composes families.
Runtime application never allocates a fresh nominal identity.

The monad implementation is an ordinary argument to the generated algebra code.
It can be evaluated dynamically or inlined without changing nominal type identity.
Nominal value types have the trivial structure; nominal codata algebras act through their observations.
Recursive algebras share a suspended dictionary whose methods receive the required type and structure arguments.
Constructing or passing that dictionary performs no source effects; observations demand its methods.
This finite construction does not impose a new positivity or productivity rule on recursive source definitions.

For example, the type parameter and recursive constructor below both belong to the annotated payload:

```zydeco
param (/VType; /Unit; builtin) : @(import("../../lib/std/builtin.zy")) in
let basis = @(import("../../lib/std/effect/monad.zy")) in
let (/Monad; /Algebra) = basis in
@[monadic] fn (A : VType) (item : A) =>
  begin
    def List (X : VType) : VType =
      data
      | +Nil : Unit
      | +Cons : X * List X
      end
    that
    ret (+Cons(item, +Nil()) : List A)
  end
```

External nominal types still enter through type parameters inside the annotation.
Knowing their seal bodies does not authorize opening them or constructing an enclosing algebra implicitly.
The [compiler reference](compiler.md#nominal-algebra-construction) describes ownership and recursive construction.

### Translation Coverage

Type, value, and computation roots use the same structural translation rules.
Pure modules are translated according to their ordinary type or value sort, with no separate module mechanism.
Type roots construct applicative nominal families without generating runtime algebra dictionaries.
Value and computation roots bind the shared lazy dictionary inside their carrier and monad parameters.
Polymorphic value functions and local nominal data/codata follow the rules above.
Supported nominal definitions have a data or codata body after their type parameters.
Other sealed heads, including nominal aliases of arrows, `Ret`, or abstract constructors,
remain unsupported: they need their own typed introduction and algebra construction rules.

Library-encoded delimited control does not imply primitive capture or duplication of the native machine stack.

### Call-by-Value Blocks

A **call-by-value block**, `@[cbv] e`, reads `e` as a call-by-value expression and elaborates it into the core,
following Levy's call-by-value translation.
Evaluation order is written into the result rather than chosen by a separate strategy,
which is what the core's separation of values from computations buys.
That order is observed once a call-by-value block sits inside a monadic block that supplies effects,
as set out below.
In the examples, `Int`, `Unit` and `VType` stand for the built-in types, which a block binds for itself,
as set out below.

A block's classifier is `Ret A'`, where `A'` translates the source type `A`.
That holds for a value body too: `@[cbv] (fn (x : Int) => x)` has type `Ret (Thk (Int -> Ret Int))`,
not `Thk (Int -> Ret Int)`.
A source value does have its own translation, but selecting between the two by whether a body happens
to be a value would make a block's type depend on a local edit inside it.

Types translate by sending each source arrow to a suspended computation arrow that returns.
A product passes through unchanged, because the core's product is already a value type.

**Two halves of one rule govern every type row.** A source type translates to a **value** type,
so a former that is already positive passes through and a **negative** one is wrapped in `Thk`.
That much decides the outside. What decides the inside is the core's own kinding:
each position the core former requires to be a **computation** type receives `Ret A'`,
since a source term there translates at that type, and each position it requires to be a value type receives `A'`.
So an arrow's codomain, a quantifier's body and a codata field carry a returner, and an arrow's **domain** does not.
Taking only the first half gives `Thk (A' -> B')` for an arrow, which is ill-kinded,
because an arrow's codomain must be a computation type.

A block writes a source type with an arrow, a product, a quantifier, an existential, abstract or manifest,
a type operator, type application, a `data` or `codata` type, a field type, a named type,
a type projection, an ascription, or a **name**; those are the only forms it translates.
A name is one the block bound, or an intrinsic for a primitive source type, such as `@(intrinsic(int))`
or `@(intrinsic(unit))`, which translates to itself; a block is closed over types, as set out below.
A source arrow's sides and a product's components are value-type positions, so a type operator in one is a kind error.

A **source kind** is `VType`, an arrow between source kinds, or a named kind over one;
the kind translation is the identity.
`CType` is not one, so a quantifier over a computation kind is refused: no source type is a computation.
A block writes `VType` with its intrinsic, `@(intrinsic(vtype))`, or by a kind name it bound,
since the host's `VType` is a name bound outside the block.
A type binding's declared kind, a type binder's annotation and an ascription's classifier are all read
by one kind judgment, so each place a block writes a kind admits the same kinds.

| Source type | Core |
| --- | --- |
| `A -> B` | `Thk (A' -> Ret B')` |
| `pi p1 ... pn . B`, `forall p1 ... pn . B` | `Thk (p1' ... pn' . Ret B')`, one function of all `n` parameters, where a binder that declares a kind becomes `forall (X : K) .` and one that declares a type `A' ->`; `A -> B` and `forall (X : K) . A` are the telescopes of one binder |
| `A1 * ... * An` | `A1' * ... * An'` |
| `forall (X : K) . A` | `Thk (forall (X : K) . Ret A')`, with `K` a source kind |
| `exists (X : K) . A` | `exists (X : K) . A'`, with `K` a source kind |
| `exists (X as S : K) . A` | `exists (X as S' : K) . A'`, with `K` a source kind |
| `fn (X : K) => A` | `fn (X : K) => A'`, with `K` a source kind, which may be left out where an arrow kind is expected; it has an arrow kind, so it stands where one is accepted: a type binding at one, a type argument, or the head of an application |
| `F A` | `F' A'`, the operator and the argument translated; then normalized, so `Pair Int` lands as `Int * Int` |
| `data \| +C1 : A1 \| ... end` | `data \| +C1 : A1' \| ... end`, transparent |
| `codata \| .d1 : A1 \| ... end` | `Thk (codata \| .d1 : Ret A1' \| ... end)`, transparent |
| `#f :: A` | `#f :: A'` |
| `(l = S)` | `(l = S')`, at the named kind `l :: K` of `S'`'s kind `K` |
| `S/l` | `S'/l` |
| `(A : K)` | `A'`, with `K` checked against its kind |
| a name the block bound | what the name stands for |
| an intrinsic for a primitive `P` or `unit` | `P`, or `Unit` |

Terms translate by producing the source value, so an application evaluates both operands first,
the function before the argument, and a binding evaluates its bindee before its body.
A value has its own translation:

| Source value | Core |
| --- | --- |
| `x`, a literal, `()` | itself |
| `_` | `_` at the expected type, a typed hole, which needs the expected type |
| `(v1, ..., vn)` | `(v1', ..., vn')` |
| `#f = v` | `#f = v'` |
| `+C v` | `+C v'`, which needs the expected type |
| `(T, v)` | `(T', v')`, a package, which needs the expected type |
| `fn (x : A) => e` | `{ fn (x : A') => e' }` |
| `fn p1 ... pn => e` | `{ fn p1' ... pn' => e' }`, one function of all `n` parameters |
| `fn (X : K) => e` | `{ fn (X : K) => e' }`, a type abstraction; synthesizes when the kind is written, and takes it from an expected quantifier when it is not; an expected quantifier's body is pushed into `e` |
| `comatch \| .di => ei end` | `{ comatch \| .di => ei' end }`, which needs the expected type |

| Source term | Core |
| --- | --- |
| a value `v` | `ret v'` |
| `e1 e2` | `do f <- e1'; do a <- e2'; ! f a` |
| `e e1 ... en` | `do f <- e'; do a1 <- e1'; ...; ! f a1 ... an`, one call taking as many arguments as `f`'s telescope has parameters; a type argument is not sequenced, arguments left over apply to the result, and too few are refused |
| `e A` | `do f <- e'; ! f A'`, an instantiation |
| `e.d` | `do o <- e'; ! o .d` |
| `e/f` | `do v <- e'; ret (v/f)` |
| `(e1, ..., en)` | `do a1 <- e1'; ...; ret (a1, ..., an)` |
| `#f = e` | `do a <- e'; ret (#f = a)` |
| `+C e` | `do a <- e'; ret (+C a)` |
| `(T, e)` | `do a <- e'; ret (T', a)` |
| `let p = e1 in e2` | `do p <- e1'; e2'` |
| `let (X, x) = e1 in e2` | `do (X, x) <- e1'; e2'`, an unpack; `X` may not appear in the type of `e2'` |
| `match e \| +Ci pi => ei end` | `do s <- e'; match s \| +Ci pi => ei' end` |
| `let fix f p1 ... pn : B = e1 in e2` | `do f <- ret { fix g => fn p1' ... pn' => e1' }; e2'`, with `f` resolving to `g` |
| `(e : A)` | `e'`, with `A'` pushed inward |
| `@[doc] e`, `@[debug] e`, `@[format] e` | `e'`; `@[debug]` prints it |
| `let X : K = A in e` | `e'`, with `X` denoting `A'` |
| `let K = κ in e` | `e'`, with `K` denoting the kind `κ` |
| `def X : K = D in e` | `e'`, with `X` denoting a seal over `D'`, or the thunk of one for `codata`; `X` may not appear in the type of `e'` |
| `def X1 : K1 = D1 that … def Xn : Kn = Dn that e` | `e'`, with every `Xi` bound as in the row above before any `Di` is translated; no `Xi` may appear in the type of `e'` |
| `let p = e1 that e2` | the `let` row above; name resolution lifts the binding and leaves a residual |
| `param p : A that e` | `ret { fn (p : A') => e' }` |

**Four source forms have both a value row and a term row** — a tuple, a named field, a constructor and a package.
Which applies is decided by whether the form is a source *value*, which holds for a variable, a literal,
`()`, a hole `_`, either abstraction, and for the other four exactly when their components or payloads are.
A `comatch` has a value row but is not counted as a source value here, so a tuple containing one is sequenced.
The two rows agree up to `do x <- ret v; e` being `e` with `v` substituted for `x`,
so the choice shows in the elaborated term rather than in what it computes.

A block is **closed**, over terms, types and kinds alike: a term that refers
to a variable bound outside it is rejected, so is a type or kind name the block did not bind,
and so is an import boundary anywhere within it, including in a parameter's classifier.
What a block uses from outside arrives as a parameter it abstracts over: an operation or an effect as a value,
and a type by type abstraction, which the host instantiates, as set out below.
The built-in types are no exception, since the host's `Int` and `VType` are names bound outside the block.
A block writes a primitive with its intrinsic, usually behind a binding of its own:

```zydeco
param (/Ret; /Thk; /process) : @(import("../../lib/std/builtin.zy")) in
do answer <- @[cbv] (
  let VType = @(intrinsic(vtype)) that
  let Int : VType = @(intrinsic(int)) that
  (fn (x : Int) => x) 7);
! process/exit answer
```

An intrinsic names nothing outside the block, but it is admitted only for a primitive source type, `unit`,
`vtype` or `ctype`: `@(intrinsic(thk))` is a shift, which no source type translates to.
The other examples in this section leave these bindings out.

The smallest blocks use primitive source types and `Unit`, arrows and products over them, and source terms built
from variables, literals, `()`, abstraction, application, tuples, and `let`; the paragraphs below add the rest.
A binder may be a variable, a wildcard, `()`, a named field `(#f = p)` or `(= f)`,
or a product of those, since the core's abstraction rule requires an irrefutable binder.
A `let`'s or a parameter's binder may also be a constructor tag over one, as in `let +Box x = b in M`,
when the tag cannot fail: its declaration has no other constructor.
The host's own check decides that, so a tag that can fail is refused with `tyck.refutable-binding`,
as it is outside a block.
Any part of a binder may carry an annotation, `(p : A)`, as in `let (f : Int -> Int, n) = M in N`.
`A` is a source type, which ordinary pattern checking cannot read, so the block translates it
before the binder is checked, and it is then compared with the type its position expects.
Since that happens before the binder binds anything,
an annotation may not mention a variable the same binder introduces, as `X` in `(X, (x : X))`.
A parameter needs its type: from an annotation around it, from an expected type as below,
or from the annotations inside it when they give the type of every part,
so `fn ((x : Int), (y : Int)) => x` takes `Int * Int`.
A part with no annotation would need inference, which a block does not do.
A function type's binder may name its parameter, but no source type mentions a term: `A -> B`, `pi (_ : A) . B`
and `pi (x : A) . B` denote one type, and a type that mentions `x`, as `pi (x : A) . x` would, is refused by sort.
So a function may be bound with the host's header, `let f (x : A) : B = e`,
whose desugared signature names the parameter.

Consecutive parameters are one function.
`fn (x : A) (y : B) => e` takes both arguments in one call, at `pi (_ : A) (_ : B) . C`,
while `fn (x : A) => fn (y : B) => e` takes one and returns a function, at `A -> B -> C`, and the two types differ.
A call passes all of a telescope's arguments together: `f a b` is one call, and `(f a) b` applies what `f a` returns,
which for a function of two parameters is refused as too few arguments.
Likewise, a function checked against an expected type groups its parameters as the type does:
the grouped function at `A -> B -> C` is refused for taking more parameters at once than its type,
and the nested one at `pi (_ : A) (_ : B) . C` for taking fewer.
Every argument is evaluated, left to right, before the body runs,
whereas a curried function runs what lies between its parameters as soon as the first arrives.
The grouped form is the shape a call-by-push-value library has, so such a function crosses the block's boundary
in either direction without an adapter.

An authored classifier is **pushed inward** rather than compared with what the body produced.
That is what a source ascription is for: `(5 : Int64)` has to reach the literal,
because a literal's representation is resolved when it is checked and cannot be corrected afterwards.
The same expectation supplies an abstraction's domain, so `((fn x => x) : Int -> Int)` needs no binder annotation,
while `fn x => x` alone still does — there is nothing to take the domain from.
A type binder's kind is supplied the same way: an expected quantifier gives a type abstraction its kind,
and a kind known first — a binding's declared kind, a quantifier's binder at an instantiation,
an existential's binder at a pack, a manifest existential's binder at its definition, an operator's domain —
gives a type operator its own, so `let F : VType -> VType = fn X => X * X` is read as the host reads it.
An expectation that no clause could use is checked against the whole term instead,
so `((fn (x : Int) => x) : Int)` is a mismatch rather than an unchecked abstraction.
An application's expectation describes its *result*: recovering a function's type from its argument
and its result is inference, not analysis, so `(((fn x => x) 5) : Int)` is refused.

A term hole `_` is a typed hole, as in the host: checking reports the type it stands at, and running refuses it.
That type is the translation of the source type expected, so a hole
where `Int -> Int` is expected reports `Thk (Int -> Ret Int)`; a hole needs an expectation,
since with none its report would say nothing.
`@[doc]`, `@[debug]` and `@[format]` are transparent inside a block,
and `@[debug]` prints the core term the part it annotates became, rather than the whole block's;
every other annotation is refused.

**Analysis stops at the block's boundary.** Every expectation comes from inside,
and the host's own expectation is reconciled with what the block synthesized:
`(@[cbv] (fn x => x) : Ret (Thk (Int -> Ret Int)))` does not reach the binder,
although the same ascription written inside the block does.
A block synthesizes independently and the host compares afterwards, which is one direction in types
as the translation is one direction in terms, and it is what makes a block's classifier a consequence
of its body rather than of its position.

Each form a source type is written with is translated compositionally:
an ascription checks its declared kind against the translation, and `F A` translates to `F' A'`,
so an operator and its argument are both source types rather than host ones.
What a name stands for needs no test of its own.
A type binding's name stands for its translated bindee, a declaration's name for its seal, a type variable for itself,
and an intrinsic for its primitive, so every type a block writes is the translation of a source type by construction.
A shift is refused where it would be written: `Thk (Int -> Ret Int)` names `Thk` and `Ret`,
which the block did not bind, and written with intrinsics it is refused because they name no source primitive.

Which *expressions* a classifier may be written from is decided separately.
A classifier given by an expression outside the forms above, such as a `@[typeof]` query,
is rejected rather than resolved, because resolving one would reach past the closure restriction.

An outside type reaches a block the way an outside value does: the block abstracts
over it, and the host instantiates it.
Below, the block takes a type `O` and the two operations it needs, and the host instantiates `O`
with a host `data` type and supplies the operations, curried as the translations of the source arrows are:

```zydeco
begin
  param (/VType; /Thk; /Ret; /Int; /Unit; /process) : @(import("../../lib/std/builtin.zy")) that
  def Opt : VType = data | +None : Unit | +Some : Int end that

  do f <- @[cbv] (
    let VType = @(intrinsic(vtype)) that
    let Int : VType = @(intrinsic(int)) that
    let Unit : VType = @(intrinsic(unit)) that
    fn (O : VType) =>
    fn (some : Int -> O) =>
    fn (caseO : O -> (Unit -> Int) -> (Int -> Int) -> Int) =>
      caseO (some 4) (fn (u : Unit) => 0) (fn (n : Int) => n));

  do g <- ! f Opt;
  do h <- ! g { fn (n : Int) => ret +Some n };
  do r <- ! h {
    fn (o : Opt) => ret {
      fn (none : Thk (Unit -> Ret Int)) => ret {
        fn (some : Thk (Int -> Ret Int)) =>
          match o
          | +None u => ! none u
          | +Some n => ! some n
          end
      }
    }
  };
  ! process/exit r
end
```

Inside the block `O` is abstract, so nothing the block does can depend on how it is represented,
and the host may instantiate it at any value type, `Thk (Ret Int)` included, though no source type translates to that.
The block's meaning depends on no host declaration.
The cost is the host's: the block above is a few lines, and the instantiation and adapted operations are more.
No sugar for instantiating a block and adapting its operations is provided.

A call-by-value block is also a block in the host's sense, so a [`that` binding](#3-bindings-and-scope)
written inside one contributes to it and not to the block around it, and the names it introduces end at its boundary.
That is what lets a call-by-value block be read as one closed source term.
Inside it the host's rule still holds: a `that` binding belongs to the nearest enclosing block,
the call-by-value block itself unless a `begin … end` is nearer, and name resolution lifts it there.
One written in a lambda's body is therefore evaluated once, at that block's level, rather than at each call,
and cannot see the lambda's parameter; a `begin … end` around the body keeps it inside.
A `param p : A that e` is lifted the same way and read as the call-by-value abstraction `fn (p : A) => e`
over the rest of its block, so a call-by-value block holding one still returns a value.
`@[cbv] (param (x : Int) that x)` has type `Ret (Thk (Int -> Ret Int))`, not `Int -> Ret Int`.

A block may also **name a source type**, written `let X : K = A`, and the declared kind is required.
The host decides whether a binding binds a type or a term by synthesizing its bindee and reading off the sort,
which is not available here: a source type is not a host type, so synthesizing one can fail rather than answer,
and the kind is what settles it before either judgment runs.
A type binding emits nothing — the name is inlined, so the block's computation is its tail's.

This is also how a block names a source arrow in its own spelling.
`Int -> Int` is ill-kinded in the host, so no host binding can stand for it as written,
while `let Fn : VType = Int -> Int` makes `Fn` denote `Thk (Int -> Ret Int)`.
A classifier may then use `Fn`; `Thk` written by hand stays refused, as above.

A block names a **kind** as the host does, `let K = VType -> VType`,
and binds `VType` itself the same way, `let VType = @(intrinsic(vtype))`.
A kind has no classifier to declare, and none is needed: no kind is a source term, so the bindee decides,
when it is an intrinsic for `VType` or `CType`, a kind arrow, a named kind, or a kind name the block bound.
A kind binding emits nothing either.

A block may use a **data type**, and construct and match on its values.
It may write one in place and bind it with a kind, `let Opt : VType = data | +None : Unit | +Some : Int end`,
and its arms are then source types, translated like any other.
The result is transparent, and the host compares transparent data by structure,
so a host value of the same declaration is a value of the block's type.
A `let` is not recursive, so a declaration that mentions itself is an unbound name;
a recursive data type is a sealed declaration, `def`, as below.
A constructor cannot synthesize — nothing about `+On` says which declaration it belongs to — so it takes its type
from the expectation, and an application pushes its domain into its argument for exactly that reason.
A match evaluates its scrutinee before examining it, which is what every positive elimination does
in the core: `do s <- e'; match s …`.
An arm's binder is a constructor tag over a plain binder, which is the one refutable shape the source language has,
or a plain binder alone, which catches what the earlier arms did not; exhaustiveness and the rest of §7 apply unchanged.

A block may also use a **codata type**, build one with `comatch`, and observe it.
It may write one in place, `let P : VType = codata | .fst : Int | .snd : Int end`, or declare a sealed and possibly
recursive one with `def`, as below. A codata type is negative, so its translation keeps the shift outside
and gives every field a returner: `codata {.d : A}` becomes `Thk (codata {.d : Ret A'})`.
A source codata type is therefore a value type, and an ascription `: CType` written around one is refused.
Two consequences follow, and both are visible to an author.

What a declaration may seal is the **codata type**, not the thunk of it, because the translation
of a codata value is a thunk and a thunk cannot be given a seal's identity.
Where `data` works behind `def` — a constructor's own type *is* the seal —
a codata needs the seal one layer in, and a block's own `def` puts it there, as below.
One observation per clause is admitted, and an interleaved copattern such as `.at n` is refused rather
than un-interleaved.

The other consequence is semantic.
A field is a suspended computation, so it runs when observed and runs again on the next observation:
admitting codata puts a **by-name** corner inside a call-by-value source language.
A field that diverges and is never observed never runs, which is what makes the choice observable without effects.

A block may define a **recursive function**, with the host's existing `let fix` header:
`let fix f (x : A) : B = M in N`, where `A` and `B` are source types and a recursive call is written `f x`.
The `Thk`, the `Ret` and the force are the translation's, and no coercion is needed anywhere:
`A -> B` already translates to a thunk type and the core's `fix` binds a thunk,
so the source's recursive name maps straight onto the `fix` binder.
The signature is settled before the body is read, and the body is checked against the declared return type rather
than against the expectation around the binding.
Several parameters are one function: `let fix f (x : A) (y : B) : C = M` binds `f`
at `pi (_ : A) (_ : B) . C`, and a recursive call passes both.
Any parameter may be a type, read as a quantifier in the same telescope, and since the recursive name stands
at the whole quantified signature, a recursive call may instantiate it at another type:
`let fix pick (X : VType) (n : Nest X) : X = … pick (X * X) m …` recurses through a nested data type.
This is polymorphic recursion, and it needs no inference, because the signature is written.
`fix` written directly is refused, with one exception the recognition cannot currently close:
`let ! f : pi (x : A) . B = fix g => fn (x : A) => M` desugars to the shape a `let fix` header produces,
so it is read as one.
Its signature is checked against its parameter, so the spelling cannot declare a type it does not have;
distinguishing the two entries would mean carrying the binding's flavor past desugaring, which erases it.

Data and recursion together are what let a block compute: a declaration
with a decreasing structure makes a terminating recursive function writable with nothing else.
Arithmetic, like any host operation, is not reachable from inside, so a block takes it as a parameter:
`param (add : pi (_ : Int) (_ : Int) . Int) that add 2 3` returns a function, which the host applies
to `int/add` directly, since the parameter's type translates to `Thk (Int -> Int -> Ret Int)`.
A curried `add : Int -> Int -> Int` would instead need `int/add` curried to `Thk (Int -> Ret (Thk (Int -> Ret Int)))`.

Effects arrive the same way, as parameters, and a monadic block is what supplies them.
A returner is observably pure by the core's [convention](#ret-and-explicit-cps),
so a call-by-value block alone has no effect but divergence, which cannot tell the function-before-argument
and left-to-right orders of the term rows from any other.
Nested in [`@[monadic]`](#relative-monads), a call-by-value block is translated twice:
the monadic block reinterprets every `Ret` and `do` the call-by-value block emitted through its carrier,
so an arrow's `Thk (A' -> Ret B')` becomes a thunk of a function returning through the carrier,
with no row of the call-by-value translation changed.
The call-by-value block abstracts over the operations it uses, and the monadic block supplies them:

```zydeco
@[monadic] begin
  fn (put : Thk (Int -> Ret Unit)) =>
    do h <- @[cbv] (fn (a : Unit -> Int) => fn (b : Unit -> Int) => (a (), b ()));
    do h <- ! h { fn (u : Unit) => do _ <- ! put 1; ret 0 };
    do _ <- ! h { fn (u : Unit) => do _ <- ! put 2; ret 0 };
    ret 0
end
```

Under `State Int` this ends in state `2`, because the tuple runs `a` before `b`;
with the tuple written `(b (), a ())` it ends in `1`, and an application likewise runs its function before its argument.
Quantifiers, packages, type operators, records, codata, transparent data and `let fix` compose the same way.
So does a sealed type the call-by-value block declares,
which the monadic block's [applicative nominal translation](#applicative-nominal-translation) carries along,
so a call-by-value block nested in a monadic block can recurse over a data type it declares.
[`lib/tests/cbv/effects.zy`](../../lib/tests/cbv/effects.zy) runs each of these with the carrier supplied,
and each order beside its counterfactual, the same two calls written the other way round;
[`lib/tests/cbv/effects-on-data.zy`](../../lib/tests/cbv/effects-on-data.zy) does the same for recursive declarations.

A block declares a **sealed** type with the host's `def X : K = D`, for `D` a `data` or `codata` declaration,
possibly under type parameters: `def List (X : VType) : VType = data | +Nil : Unit | +Cons : X * List X end`.
A seal is nominal, so the only way into or out of one is the `data`
or `codata` shape [its typing exposes](#4-classification-and-inference),
and a `def` whose body declares neither is refused.
Two sealed declarations of one shape are two types, where two transparent `let` declarations are one.

The seal sits where the core can meet it.
For `data` it is on the value type: `σ : K`, a seal over `D'`, and `X` denotes `σ`.
For `codata` it is on the computation type inside the thunk the translation adds, `σ : K1 -> ... -> CType`,
and `X` denotes `fn (Y1 : K1) ... => Thk (σ Y1 ...)`, so `Stream Int` translates to `Thk (σ Int)`.
A seal on the thunk would never be met, because a codata value is introduced only by a thunk,
whose type is synthesized as `Thk B` with the codata inside it.
A source codata type is a value type, so its declared kind ends in `VType`, and `def S : CType = …` is refused.
A `def` that writes no kind takes its parameters' kinds over `VType`,
so `def D (X : VType) = data … end` has kind `VType -> VType`; each parameter must then declare its kind.
A `_` in a written kind stands for the part left to inference and for nothing else:
`def D (X : VType) : _ = …` infers the result, and what is written before a `_` is read
and guarded as any kind is, so `def D : CType -> _ = …` is refused.
Each binder there is checked as a type binder is, every annotation in it included,
so the inner annotation of `pi ((X : @[typeof] outside) : VType) . _` reaches outside the block and is refused.
A kind mentions no type, so a parameter's kind that names an earlier parameter,
as in `(X : VType) (Y : X)`, is refused by sort.
An ascription inside the definition, such as `(codata … end : VType)`, is the ordinary one,
checked where the parameters outside it are bound, against the kind of what it ascribes,
so `: CType` there is refused, and so is `: X` naming a parameter.

`X` is bound before `D` is translated, so `D` may mention it, and the declaration is then a recursive group of one.
Declarations that mention one another form a single group, and the same order extends to it,
as the host's fixpoint takes it: every member's name is bound before any member's declaration is translated,
so each may mention the others.

```zydeco
def Tree : VType = data | +Node : Int * Forest end that
def Forest : VType = data | +Nil : Unit | +Cons : Tree * Forest end that
```

Binding a name needs only its kind and whether it declares `data` or `codata`,
never what the declaration says, which is why a whole group can be bound first.
Each member is its own seal, so two members of one shape are two types, and none may appear in the type of the tail.
[`lib/tests/cbv/interpreter.zy`](../../lib/tests/cbv/interpreter.zy) runs an interpreter
whose environment holds values and whose closures hold environments.

The host admits a binding cycle only among seals with known kinds,
and for these cases a block gives the host's diagnostics, in the host's order:
a value cycle directs the author to `let fix`; a kind that is absent, as in a recursive `def`
with parameters and no written kind, or that is not a kind, as in `def N : Int = …`, is not known;
and a transparent `let` that mentions itself or another member is missing its seal.
These checks keep that order among themselves, member by member through the group, and a block makes them
for every member before applying what it alone requires of a member, such as a binder that is a variable or a wildcard.
The host checks each member's binder before it reaches the next member, so a group at fault
in both ways can report a different error first in a block than in the host.

Functions that call one another are not a group of `let fix` bindings, which would be a value cycle,
but a single `let fix` returning a record of them, as the host takes a fixed point of codata.
A call between them goes through the record, and the header's parameter is the one every `let fix` has:
`let fix parity (u : Unit) : Parity = comatch | .even => fn (n : Nat) => … (parity ()).odd m … end`.

A declaration's scope is the rest of its block, so the block's type may not mention it,
as an unpack's witness may not escape; the refusal is `tyck.escaping-declaration`.
An abstract package exports it, `((Opt, +Some 7) : exists (X : VType) . X)`, and a manifest one,
which would disclose it, is refused.

A block may use an **existential package**. The abstract form `exists (X : K) . A`
and the manifest form `exists (X as T : K) . A` may each be written where they are used or bound to a name,
and the binder's kind must be a source kind, so `exists (R : CType) . A` is refused.
An existential is a positive former, so its translation takes no shift and the body translates in place:
`exists X . A` becomes `exists X . A'`.
A manifest equation's disclosed witness is itself a source type and is translated,
which is what keeps the equation true on the translated side: opening the package shows `X` as `T'`.

A **pack** is written `(T, v)` against an expected existential, with the witness first.
It is the same syntax as a tuple, and the expected type is what decides which it is — so a pack cannot synthesize,
and `(Int, 7)` with no expectation is read as a tuple and refused for using a type as a term.
A telescope of several witnesses consumes that many leading components
and the rest form the body's product, as in the host.
A payload that is not a source value is sequenced first, so `(Int, f 7)` evaluates `f 7` and packs the result.

An **unpack** is `let (X, x) = M in N`. Nothing about it is special to packages: it is the ordinary binding clause,
because the bindee's type is what makes the binder a package pattern rather than a product one.
The witness it opens behaves inside `N` as a source type variable, so a source function may be written at it;
and the witness may not escape, so `N`'s own type may not mention it.
That is what makes the package's own operations the only way to consume its payload,
which is what an existential is for.

**Named fields** travel through the translation untouched.
A field name is not a type former: it carries no polarity, so none of the three type rows takes a shift,
and `#f :: A` becomes `#f :: A'`, a named type `(l = S)` becomes `(l = S')`, and a type projection `S/l` becomes `S'/l`.
The kind translation is the identity, which is why a *named* kind such as `#Counter :: VType` is a source kind exactly
when its payload is, and so a package whose telescope binds at one is admitted.
A named kind is written wherever a block writes a kind, as in `fn (X : (#C :: VType)) => fn (x : X/C) => x`.
`#C :: A` with `A` a type is the same node read as a field type, so where a kind belongs it is refused by sort.

A package's binder may name its witness's field, as in `exists (#Counter = ((Rep as Int) : VType)) . (#zero :: Rep)`:
the witness has the named kind `#Counter :: VType`, and `Rep` binds its payload.
No other type binder may: `fn (#C = (X : VType)) => M` and `forall (#C = (X : VType)) . A` are refused.
For the term-level abstraction this is forced, because the core term a block builds
for it makes a plain binder from its witness, and the field name would vanish from the type it builds.
A quantifier and a type operator keep their binder's pattern, so for them it is a restriction of the source language:
what instantiating or applying one at a named argument should do has not been specified.

A block builds a record with `(#f = v, #g = w)` against the record's declared type,
and a field whose payload is a computation is sequenced before the record is built, so the field holds the result.
It reads one either by pattern — `let (= f, = g) = r in …`, which binds each field under its own name —
or by projection, `r/f`, which synthesizes and leaves the head to be sequenced, so the head need not be a value.
The field itself is resolved by the host's resolver rather than by the block,
which is what makes a field behind a product prefix reachable and gives a missing
or ambiguous field the host's own diagnostic.

A record type is written in place as a product of field types, `(#fst :: Int, #snd :: Int)`,
and is structural: a host record and the same type written in a block are one type.
A pack's witness is written in place too, so `(((#Counter = Int), #zero = 0) : Pkg)` needs no outside name
for `(#Counter = Int)`.

### Call-by-Name Blocks

`@[cbn] e` is reserved for reading `e` as a call-by-name expression and elaborating it into the core,
following Levy's call-by-name translation as `@[cbv]` follows his call-by-value one.
It is not implemented, and it is refused during desugaring rather than ignored:
an [unrecognized meta annotation](#meta-annotations-compile-time-metadata) is accepted with no effect,
so without the reservation a body written for call-by-name would run as plain call-by-push-value, with no diagnostic.

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
| Semantic unit | Establish a term's meaning in context: resolved names, classifier, and provenance |

A semantic unit is a term considered with its analysis context.
Processing source into its semantic form includes lexing, parsing, name resolution, and type checking.
Semantic units compose through the language's binding, abstraction, application, and import rules;
independent source selection retains the [source-boundary requirements](#source-boundaries).
One semantic unit can participate in several compositions and supported compilations.
Its meaning depends on its source inputs and resolution context; target and optimization choices belong
to the consuming compilation.

A compilation unit supplies the external contract needed to emit a selected package independently.
The [source-package roles](#source-packages) describe these optional contracts.

The compiler reference maps these abstractions
to their [implementation representations](compiler.md#implementation-representations).
Package context, source selection, and import identity follow the [package namespace rules](#package-hierarchy).
Source text can retain [documentation annotations](#source-documentation).

### Source Boundaries

A source file contains one complete term.
After imports and any companion annotation are assembled, each source synthesizes its classifier
from its own lexical bindings, explicit imports, and [default vocabulary](#core-prelude).
Provider bindings are always explicit.
Provider inference closes before comparison with import-site expectations.
An independently selected term follows the same binding and inference boundary.

Imports, companion signatures, and the closed default prelude dependency form an acyclic graph.
Imported computations execute at every dynamic occurrence.
Package context and sharing follow the [resolution model](#copy-resolve-merge-analyze).

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

```zydeco
param (/stdio; /process) : @(import("../../lib/std/builtin.zy")) in
! stdio/write_line "hello, world!" { ! process/exit 0 }
```

The REPL stores complete terms as numbered inputs; `@(import(1))` imports input 1,
while `@(import("1"))` addresses a file.
Its root commands are `@[type] e`, `@[run] e`, `@(help)`, and `@(quit)`.
By default it inspects kinds/types and evaluates values or directly returning computations.
Explicit execution can supply Builtin.
REPL evaluation captures output and uses empty stdin and arguments.

### Core Prelude

Ordinary source terms can use the core kind and type vocabulary without importing or opening a prelude.
The governing rule is **implicit vocabulary, explicit providers, preserved witness identity**.
Closed names such as `Int`, `Thk`, and `Option` come from the standard view;
`OS` refers to an explicitly opened provider's witness, never to a global host type.
For example:

```zydeco
param (/process; builtin) : @(import(/std/builtin)) in
let stop : Thk (Int -> OS) = process/exit in
! stop 0
```

The parameter selects the provider and opens its package once.
That opening supplies both `process` and the witness used by `OS`.
The whole-value alias `builtin` retains it for further composition, but that spelling has no special status.
A provider-independent source needs no such parameter: `Option Int`, or `fn (value : Int) => (ret value : Ret Int)`,
is already a complete source term.

#### Vocabulary and Lookup

The default vocabulary is fixed:

| Names | Default meaning |
| --- | --- |
| `VType`, `CType` | Manifest value and computation kinds |
| `Thk`, `Ret` | Manifest intrinsic constructors of kinds `CType -> VType` and `VType -> CType` |
| `OS` | The unique visible witness carrying the `builtin(os)` role, of kind `CType` |
| `Unit` | Manifest intrinsic unit value type |
| `Int8`, `Int16`, `Int32`, `Int64`, `Int` | Manifest signed primitive value types |
| `UInt8`, `UInt16`, `UInt32`, `UInt64`, `UInt` | Manifest unsigned primitive value types |
| `Float32`, `Float64`, `Char`, `String` | Manifest floating-point and text primitive value types |
| `Bool`, `Option`, `Either` | Manifest re-exports of the transparent data definitions, with kinds `VType`, `VType -> VType`, and `VType -> VType -> VType` |

Authored lexical and block-wide bindings take precedence over the default names.
This includes forward mobile definitions: `let Int = Unit that` selects that local meaning throughout its block.
The prelude does not export fields from the source's result or import operations.
`Addr`, `Reader`, `Writer`, `builtin`, stream and memory operations, numeric dictionaries and arithmetic intrinsics,
`List`, `Bytes`, `Monad`, `Algebra`, and effect instances remain explicitly selected.
Data constructors follow ordinary data typing.
Changing the core set changes lexical scope and requires review as a language change.

Naming a computation type supplies neither an action nor an effect handler.
`OS` permits signatures such as `Thk OS`; it does not introduce `exit` or acquire a provider.
`Ret` retains its [ordinary protocol meaning](#ret-and-explicit-cps), rather than asserting purity.
The default concerns common names, not the purity of the computations they describe.

#### Explicit Providers and Shared Openings

An unresolved `OS` in a default-enabled scope selects the unique visible abstract witness
with the `@[builtin(os)]` role.
The role, rather than a signature path or binder name, identifies it.
A signature alias or another provider contract carrying that role works by the same rule.
A package parameter that has not been opened supplies no visible witness.
No visible witness produces `tyck.missing-builtin-type-role`;
multiple witnesses produce `tyck.ambiguous-builtin-type-role` instead of silently choosing the nearest
or first provider.

Select an ordinary lexical `OS` explicitly when several providers are open:

```zydeco
param (/OS; /process = left; first) : @(import(/std/builtin)) in
param (/process = right; second) : @(import(/std/builtin)) in
let stop : Thk (Int -> OS) = left/exit in
! stop 0
```

The explicit field binding chooses the first opening's witness.
Replacing `left/exit` with `right/exit` is rejected: opening the same contract twice does not establish equality.
Reusing an already retained opening preserves identity without a second opening or a fresh witness.
Forward the provider and dependent operations together across composition boundaries,
following the [shared-opening rules](#module-interfaces-and-shared-openings).
Builtin's abstract `SystemOS` witness, role, and manifest `OS` export remain unchanged;
there is no canonical `intrinsic(os)`.

A mobile binding using contextual `OS` must see independent provider openings in its block,
including openings written later.
Its ordinary dependencies are collected first.
Bindings that neither use contextual `OS` nor depend transitively on a binding that does are checked first;
the remaining bindings retain their ordinary dependency order.
This adds no back-edge through an `OS` user's dependents and does not remove authored cycles.
Use an explicit `OS` field binding when provider selection itself needs ordinary lexical dependencies.

#### Opt-Out and Replacement

Meta annotations describe exceptions rather than activating the normal vocabulary.
`@[prelude(none)] e` disables default lookup within `e`, including contextual `OS`.
Authored bindings remain available, including names explicitly bound outside the annotation;
sibling terms retain their own policy.
A leading file-level opt-out also applies to declarations independently selected from that file.

```zydeco
@[prelude(none)]
let Int = @(intrinsic(int)) in
let value : Int = 42 in
value
```

`@[prelude(from(p))] e` replaces the vocabulary with fields from an ordinary lexical packed view `p`.
It abbreviates exactly this opening:

```zydeco
let (
  /VType; /CType; /Thk; /Ret; /OS; /Unit;
  /Int8; /Int16; /Int32; /Int64; /Int;
  /UInt8; /UInt16; /UInt32; /UInt64; /UInt;
  /Float32; /Float64; /Char; /String;
  /Bool; /Option; /Either
) = p in
e
```

Every field comes from `p`, including the kinds; spelling does not assign a compiler identity.
The bindee resolves before the new bindings, so `from(Int)` can refer to an outer value named `Int`.
Missing or ambiguous fields receive ordinary projection diagnostics.
Bind imports, provider selections, and custom adapters explicitly before the annotation.
An override can be used inside an opted-out region because its bindings are ordinary lexical definitions.
It must preserve the witness identity expected by dependent operations; it cannot canonicalize
or reseal a provider's `OS`.

Exactly one argument is accepted: `none` or `from(identifier)`.
Missing, duplicate, combined, unknown, or malformed arguments are rejected.
`from` accepts a lexical variable, not a string, keyword, package path, or arbitrary expression.

#### Standard View and Bootstrap

[`prelude.zy`](../../lib/std/prelude.zy), package `/std/prelude`, exports the closed vocabulary directly
as a packed value, excluding provider-relative `OS`.
It imports [`primitive.zy`](../../lib/std/primitive.zy), package `/std/primitive`,
and the three transparent data definitions.
Those data sources import only the primitive leaf, which uses existing intrinsic roles and has no imports,
abstract witnesses, host capabilities, operations, or parameters.
The view exports each closed core name once with a unit payload and no hidden factory parameter.
Manifest kind witnesses use annotated introductions; manifest type witnesses use `pack` (§9).

The loader supplies that closed view as an ordinary tracked dependency for each default-enabled source.
The primitive leaf, three bootstrap data definitions, prelude itself, and raw Builtin contracts opt out explicitly
with `@[prelude(none)]`, keeping the dependency graph acyclic.
Other provider-independent libraries use the implicit vocabulary rather than repeating primitive imports.
The default requires neither a project declaration nor a runtime provider.
Ordinary dependency refresh and source sharing apply; the compiler bundles the closed bootstrap for installations
without the source checkout.

#### Source Boundaries and Results

Imports, companions, independently selected packages, and numbered REPL inputs receive their own defaults.
They do not inherit a caller's overrides, provider bindings, or opt-out; the file-level opt-out
for selected declarations is the explicit exception described above.
Each source still synthesizes independently before comparison with import-site expectations.
Annotations remain at their authored positions and do not move a monadic basis across a binder
or change monadic capture.

Default vocabulary is available in kind and type roots as well as value and computation roots.
This follows the ordinary [static-only opening rule](#3-bindings-and-scope), not a special retry or an inserted `pack`.
An abstract provider witness still cannot escape its opening in a raw type result;
package the result with an appropriate dependent interface when necessary.
A `.zyi` source must still denote a type.
Checking eliminates the wholly manifest closed opening, leaving no prelude work for interpreter or backend lowering.
No initialization computation, runtime dictionary, extra provider parameter, or runtime metadata is introduced.

Compiler-canonical primitives retain their identities, and transparent data definitions share identity according
to their resolved definitions and the [source sharing rules](#copy-resolve-merge-analyze).
Neither fact makes a provider-specific witness canonical.

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

#### Relationships and Operations

Relationships associate packages with tests and other subjects:

| Relationship | Meaning |
| --- | --- |
| `test(target)` | A companion test associated with the declaring package |
| `of(subject, ...)` under `test(...)` | The packages tested by this test |

Relationship targets follow the shared [package-path rules](#paths-and-package-context).
Relationship-driven test selection remains an [open idea](../ideas/README.md#project-context-inspection).
The [execution contract](#selecting-an-execution-backend) governs backend preparation and test results.

### Package Hierarchy

A package definition is a meta annotation on a term, so it establishes a package context
for the lexical extent of that term.
Package resolution instantiates source in that context, merges identical resolved copies,
and gives each resulting term semantics once.

The package hierarchy is a tree of naming contexts.
A point in the tree can serve as a namespace prefix, a package definition site, or both.
The resolution run supplies each source instance's entry package context;
enclosing package-definition annotations establish their annotated terms' package contexts.
For example:

```text
<root>
└── std
    ├── data
    │   └── smoke
    └── text
```

Defining `data` in the lexical scope of `std` associates its term with `/std/data`.
The prefix records the naming context at the definition site.

#### Paths and Package Context

Package namespace paths are inspired by filesystem paths.
The package context is the namespace from which relative package paths are resolved.
An ordinary path starts from the package context, `.` denotes that context,
`..` denotes its parent, and a leading `/` starts from the root.
All package paths follow this same rule, including paths in definitions, imports, and relationships.

| Package Context | Package Path | Target |
| --- | --- | --- |
| `/std` | `data` | `/std/data` |
| `/std/data` | `.` | `/std/data` |
| `/std/data/smoke` | `..` | `/std/data` |
| `/app` | `../std/data` | `/std/data` |
| Any package context under the same root | `/std/data` | `/std/data` |

Relative and absolute spellings identify the same declaration.
Normalizing redundant steps such as `./data` and `data/../data` gives one canonical relative path
from a given package context to a target, and one canonical absolute path from the root.

Package paths use the lexical package context; quoted file paths use the referencing file's location.
Thus `@(import(/std/data))` uses a package path, while `@(import("data.zy"))` uses a file path.
Both select source for the same instantiation process.

#### Lexical Scope of Package Context

A package-definition meta annotation resolves its name in the enclosing package context.
That name establishes the package context for its relationship arguments and annotated body.
Sibling terms retain the enclosing package context.

```zydeco
@[package(library, name(std))]
(
  @[package(library, name(data))] (),
  @[package(library, name(text))] ()
)
```

At a project's source root, `std` resolves to `/std`.
Both `data` and `text` are defined in the lexical scope of that annotation,
so their names resolve to `/std/data` and `/std/text`.
Their annotated bodies have package contexts `/std/data` and `/std/text`, respectively.

Relationship arguments use the same package-path rules in their lexical package context.
For example, a test named `/std/data/smoke` can use `of(..)` to identify `/std/data`.

An imported copy starts in the package context supplied by its route through the resolution run.
Package definitions inside that copy establish lexical scope for their annotated terms in the same way.
The importer's surrounding terms retain their enclosing package context.

#### The Opaque Root

The root is an opaque name: `/std/data` denotes `<root>/std/data`.
The leading slash supplies that root.

Two components describe package-path resolution:

| Component | Meaning |
| --- | --- |
| Root `ρ` | The opaque namespace denoted by leading `/` |
| Package Context `ν` | The namespace used by relative paths, including `.` |

At a project's initial source root, `ν = ρ`.
Package definitions establish an inner `ν`; resolution supplies the context for each imported instance.

Giving the root a name makes registering one project inside another a namespace substitution.
Registering project B under A's `/vendor/b` substitutes:

```text
ρB ↦ ρA/vendor/b
```

B's authored `/std/data` then denotes `ρA/vendor/b/std/data`.
This substitution supplies the semantics of project registration and renaming.
Its user-facing configuration syntax remains an [open idea](../ideas/README.md#project-context-inspection).

#### Unnamed Packages Have Opaque Names

An unnamed package receives an opaque name for its declaration within an instantiation.
For example, `@[package(test)]` in package context `/std` establishes the package context `/std/α`,
where `α` is explanatory notation for that name.

Inside the package, `.` refers to `/std/α`.
Declaring `data` produces `/std/α/data`, addressed as `data` from that package context.

Every namespace point has an absolute semantic path.
Package paths obtain opaque components from their resolution context: absolute paths use the root,
and relative paths use the package context.
Parent steps follow the namespace tree from that context.

### Resolution Runs and Source Copies

A package context depends on how the package was reached in this resolution run.
Each import route produces a copy of the file or package on the resolution graph, with its entry package context.
Within the copy, package annotations determine lexical scope through the term structure.
The rule applies uniformly to source files, named and unnamed packages, libraries, tests, and programs.
Several importers can supply the same context, and different contexts can lead to identical resolved contents.

Each copy participates in a composition as a semantic unit. Package resolution supplies its package context;
the [source-boundary rules](#source-boundaries) govern ordinary lexical bindings and inference.

### Copy, Resolve, Merge, Analyze

Compiler sharing follows resolved file and package imports:

**Copy → resolve imports for this run → merge identical resolved packages → analyze once per merged node.**

1. **Instantiate.** Each route to a file or package produces a separate candidate instance.
   Its import route supplies the entry package context.
2. **Resolve.** Resolve each instance's package definitions and imports in their lexical package context.
3. **Merge.** Once dependencies are resolved and themselves merged, merge candidates with identical contents
   and dependency targets.
4. **Analyze.** Give each resulting node semantics once.

For example, two copies of a file containing `@(import(data))` can merge
if their respective `data` targets ultimately become the same resolved package and their remaining contents agree.
Different resolved dependencies produce distinct package nodes.

The comparison is structural equality of resolved inputs.
Source loading reads the syntax needed to identify definitions and imports.
The acyclic import graph orders merging from providers toward consumers: compare dependency targets
through their merged nodes, then compare the remaining contents.
Semantic-unit analysis processes the resulting graph.

Local semantic declaration identities are created after merging.
Merged copies share those identities; distinct resolved instances receive their own.
Namespace bindings record which merged packages their paths select.
Merged nodes retain import routes and source locations as provenance.

Resolved contents include the term's companion signature and relevant meta annotations.
Consumers use the retained roles, relationships, attached text, names, and origins.
Copies that merge can share a binding; conflicting resolved definitions produce a diagnostic with both origins.

An imported computation executes at each dynamic occurrence.
A semantic unit supports several compilations through the package's declared external contract.

### Standard Library Organization

The standard library separates value representations, reusable algebraic dictionaries, and effect protocols.
This keeps a data type such as `Option` independent of a computation carrier that uses it,
and lets a dictionary such as `Monoid` serve more than one effect.
The directories under `lib/std/` have these responsibilities:

| Directory | Responsibility |
| --- | --- |
| `data/` | Value representations and their operations, including `Bool`, `Option`, `Either`, and `List` |
| `algebra/` | General-purpose algebraic dictionaries, currently `Monoid` |
| `effect/` | Effect interfaces, carriers, transformers, handlers, and shared relative-monad machinery |
| `numeric/` | Numeric operations and their domain-specific dictionaries |
| `text/` | Text and byte-sequence operations |
| `system/`, `memory/` | Concrete runtime capabilities and their library interfaces |

`Algebra M R` belongs to `effect/`: it describes an algebra for a particular relative monad,
whereas `Monoid W` describes an operation and identity on a value type independently of a monad.
Standard-library modules define related type constructors and operations together.
A reusable structural contract is a manifest export, alongside the factory or operations that use it;
a type-only package carries its exports with a `()` payload.
Consumers open these packages with projection patterns (§9), including before defining local type aliases.
A consumer that exports only a derived type packages that result as a manifest field as well.
The Builtin ABI declarations remain type terms because they prescribe the host-supplied interface.

Explicit signatures remain at representation boundaries: colocating a signature
and its implementation must preserve the abstract witnesses and disclosed equations of the public contract.
A separate interface-only package is appropriate when it has an independent dependency boundary,
as with the numeric dictionaries shared by both numeric builders.
Each effect's operation interface, concrete carrier, and transformer stay in the same module.
The Writer effect is distinct from the host stream `Writer` capability.

The root `builtin.zy` remains the host contract, and `std.zy` remains the assembled public value.
The `algebra/` and `effect/` modules are imported explicitly rather than added to that aggregate.
Directory ownership does not assign [source-package names](#source-packages).
The [library guide](../../lib/std/README.md#source-layout) inventories the entry points and imports.

#### Either Values

The data library's shared [`Either L R`](../../lib/std/data/either.zy) represents a choice
between `Left` carrying an `L` and `Right` carrying an `R`.
It is a transparent type, so data operations, exception carriers, I/O, and memory interfaces share its representation.
The constructors do not intrinsically mean failure or success; fallible library APIs use `Either E A`,
with errors on the left and successful values on the right.
The public std value and the data package both export `Either` and its `either` operation bundle.

All operations take the input branch types `L` and `R` first.
Transformations then take their new payload type `S`; `fold` instead takes an arbitrary computation result `B`.
The operations select only the callback for the branch present in the input:

| Operation | Contract |
| --- | --- |
| `left L R value`, `right L R value` | Construct the corresponding branch under `Ret` |
| `fold L R B value on_left on_right` | Apply the handler for the selected branch, returning `B` |
| `map L R S value transform` | Transform a right payload through `R -> Ret S`, returning `Ret (Either L S)` |
| `map_left L R S value transform` | Transform a left payload through `L -> Ret S`, returning `Ret (Either S R)` |
| `and_then L R S value next` | Sequence a right payload through `R -> Ret (Either L S)`; preserve a left payload |
| `unwrap_or L R value fallback` | Return the right payload, or the fallback for a left value |
| `is_left L R value`, `is_right L R value` | Return the corresponding branch test as `Ret Bool` |

`map` and `and_then` are right-biased; their callbacks are not run for a left value.
`map_left` leaves right values untouched, and `fold` takes its left handler before its right handler.
Computation callbacks are passed as thunks.

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
`Either`, `List`, and abstract `Bytes`; host operations select continuations instead of constructing those types.

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
The public library reifies these as `Option` or `Either`.
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
A UTF-8 `Path` prevents text/path interchange but cannot represent every native path. Growable writers, seeking,
and asynchronous protocols remain [open ideas](../ideas/README.md#growable-byte-builders-and-incremental-streams).

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
| `fixed/Layout A` | A static `Either LayoutError (Plan A)` | Construction evidence eliminates before fixed realization |
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
`storage/constant/create size alignment` returns `Either LayoutError (exists L. Storage L)`;
its runtime counterpart returns `Ret (Either LayoutError (exists L. DynamicStorage L))`.
Both reject negative sizes and nonpositive or non-power-of-two alignments.
The fixed constructor requires static operands. Both representations are sealed against unchecked construction;
`unsafe/for_layout L` validates the same geometry but lets the caller associate it with an existing witness.
`query L` exposes `SizeAlign = (#size :: Int) * (#alignment :: Int)`.
`constant/materialize L` explicitly obtains dynamic geometry from fixed storage.

A witness relates geometry, pointers, and codecs, not an allocation instance.
Independently opened witnesses remain distinct even with equal geometry.
For dynamic instances, the caller retains the matching geometry until release.
Size and indexing remain bounded by nonnegative `Int`; full pointer-width sizes remain proposed work.

[Codec](../../lib/std/memory/codecs.zy) supplies value functions `init pointer value : Thk (Cps (Ptr L Init))`
and `read pointer : Thk (Cps A)`.
The [dynamic form](../../lib/std/memory/codecs.zy) carries ordinary thunks:
`init : Thk (Ptr L Uninit -> A -> Cps (Ptr L Init))` and `read : Thk (Ptr L Init -> Cps A)`.
`codecs/materialize L A` selects that form explicitly.
`codecs/unsafe/scalar L A write read` associates scalar accesses with a caller-established layout.
`codecs/unsafe/take` reads through a fixed codec before yielding an uninitialized pointer
and the value; `dynamic_take` does the same through a dynamic codec.
Neither clears bytes nor invalidates aliases. A codec carries no implicit allocator.
Its stored format is independent of the compiler's representation of `A`.

[StaticAlloc](../../lib/std/memory/allocation.zy) selects allocator code using value functions,
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

The [allocator interface](../../lib/std/memory/allocation.zy) receives byte size and alignment explicitly.
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
The [representation alias](../../lib/std/memory/codecs.zy) is parameterized by these component families.
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
The [fixed builder](../../lib/std/memory/layout.zy) exposes `Layout A = Either LayoutError (Plan A)`,
with an abstract successful plan and [inspectable shape](../../lib/std/memory/shape.zy).
Its constructors support scalars, unmanaged addresses, unit, padding, products, and increased alignment.
Invalid alignment, negative size, and overflow produce typed `Left` results during static construction;
only a `Right` plan can be realized.
The caller handles that static result explicitly.
Unknown runtime inputs to placement or fixed realization produce a `StaticElimination` diagnostic.

`realize A plan` introduces `L` with the [representation interface](../../lib/std/memory/codecs.zy).
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
Both produce a static `Either LayoutError (Record Left Right)`; neither takes a logical type or codec.
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
returning `Either LayoutError (Array Element)`.
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
A typed no-read discard operation remains an [open idea](../ideas/README.md#further-storage-boundaries);
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

```zydeco
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
with `Writer` remain an [open idea](../ideas/README.md#growable-byte-builders-and-incremental-streams).

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

```zydeco
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
Wasm rejects native imports. Callbacks, pointer results, floating-point and aggregate ABI values,
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

```zydeco
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
The [artifact workflow](../../cli/README.md#c-libraries) produces a C header,
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
The [workflow](../../cli/README.md#native-zydeco-units) shows the commands;
the [compiler contract](compiler.md#native-unit-artifacts) owns artifact and linkage details.
Broader type interfaces and host-language bindings remain in the [ABI proposal](../proposals/zydeco-abi.md).

## 15. Execution Profiles

The execution paths share source checking and static elimination, but have different allocation and host boundaries.
The native layout is an implementation ABI, not source-level control over addresses or object layout.

| Profile | Control and memory | Host boundary |
| --- | --- | --- |
| Interpreter | Explicit evaluator state; Rust-owned values and environments | CLI/REPL I/O; returning C imports on Unix |
| Native AMD64 | Machine control stack; growable retained environments; two fixed 1 MiB copying semispaces | Supplied runtime, Linux/macOS toolchain, returning C imports and scalar exports |
| `wasm-sps` | Block trampoline and persistent stack frames; growing non-collecting heap | Imports from a `zydeco` embedding |

Native managed live values must fit in one semispace, including headers.
Host-owned strings and byte buffers remain allocated within their runtime instance, independently of managed collection.
Environment growth and managed-heap capacity are distinct limits.
Native tail transfers reclaim dead activations, but live continuations and escaping values can retain storage;
The Wasm trampoline avoids growth of the host call stack without guaranteeing constant heap use.

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
`run SOURCE -t TARGET` selects one of `interpreter`, `exe` (AMD64), or `wasm-sps`.
`--target` is the long spelling; omitting it selects `interpreter`.
`run` inherits terminal streams, forwards arguments after `--`, and returns the program's exit status.
`--dry` checks and selects the executable root without linking, emitting code, or requiring backend tools.

`test SOURCE` accepts the same targets and allows repeated `-t` options.
The test-only selection `-t all` expands in place to `interpreter`, `exe`, and `wasm-sps`, in that order.
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
See the [workflow](../../cli/README.md#run-and-test-programs) for tool and runtime configuration.

## Meta Annotations (Compile-Time Metadata)

Meta annotations are written `@[meta] e`; `@(meta)` abbreviates a hole payload.
These forms supply compile-time metadata to the compiler.
An annotation expression is a name, string, integer, or a named application with comma-separated arguments.
Runtime metadata, such as a buffer length or object vtable pointer,
is ordinary value data and follows the usual checking and erasure rules.
An ordinary layout descriptor is also a typed value; evaluating it during checking does not make it a meta annotation.

| Meta annotation | Meaning and valid use |
| --- | --- |
| `import(source)` | Import an independently checked term (§12); source selection follows [package resolution](#package-hierarchy) |
| `package(role, ...)` | Define a package's role and relationships (§12); names and package context follow [package resolution](#package-hierarchy) |
| `discover(include("glob", ...), exclude("glob", ...), ...)` | Declare candidate source files; discovery follows [package resolution](#package-hierarchy) |
| `intrinsic(role)` | Supply a canonical kind/type (`vtype`, `ctype`, `thk`, `ret`, `unit`, `int`, `uint`, `i8`/`i16`/`i32`, `u8`/`u16`/`u32`, `f32`, `f64`, `char`, `string`) or an integer value function (§8) |
| `builtin(role)` | Mark a host capability or operation in a typed Builtin interface (§13) |
| `ffi(c, library("name"), symbol("name"))` | Supply a foreign thunk implementation at a hole (§14) |
| `prelude(none)` / `prelude(from(p))` | Disable the default [core vocabulary](#core-prelude) or replace it from a lexical packed view |
| `typeof` | Extract a synthesized classifier (§4); no arguments |
| `monadic` | Algebra translation under the lexical basis (§11); no arguments |
| `cbv` | Elaborate a call-by-value expression into the core (§11); no arguments |
| `cbn` | Reserved for call-by-name blocks (§11); refused until implemented |
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

`@[doc]` retains an optional adjacent text block on its payload term.
It leaves the payload's typing and execution unchanged.
The retained syntax supports authored explanations; no higher-level documentation tooling is implemented.

#### Attaching Documentation

Write an uninterrupted `--|` block immediately above `@[doc]`:

```zydeco
--| The current counter value.
--|
--| Read this field to inspect progress.
@[doc] (#value = 42)
```

Use `--|` for blank lines within the block; a genuinely blank source line or an ordinary `--` comment breaks attachment.
Unattached text blocks produce warnings. Use ordinary comments for implementation notes.
The parser retains the annotation, its arguments, the attached text, and the payload's source location.
The formatter preserves the text and its effective attachment.

The payload can be any term, including a binding, named field, or block.
Additional `doc(...)` arguments remain ordinary meta annotation values without a defined option schema.
Attached Markdown is retained as text; its links and code fences have no compiler-defined interpretation.
Documentation inheritance, semantic links, lookup, editor presentation, reference generation,
and example verification are deferred.

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

Rejection examples identify the intended `tyck.*` diagnostic where relevant.
Diagnostic wording and rendering are implementation details.
