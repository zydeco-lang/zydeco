# Design

Zydeco is a proof-of-concept language for studying call-by-push-value (CBPV),
stack-manipulating computation, and relative monads.
This document gives the project design and repository map.
The [language reference](docs/references/language.md) specifies source behavior,
and the [compiler reference](docs/references/compiler.md) follows phase contracts and maintenance entry points.
Linked proposals retain independently reviewable rationale and open decisions;
[CONTRIBUTING.md](CONTRIBUTING.md) covers tooling,
and the [language guide](docs/tutorial/zydeco-guide.md) provides a longer source-level walkthrough.

## Language Model

Zydeco separates values from computations. Values include variables, thunks, units,
products, constructors, literals, total value functions, and existential packages.
Value functions and packages support higher-order static composition
under the shared [static elimination contract](docs/references/language.md#10-static-elimination).
Runtime callable contracts remain explicit computations behind `Thk`.
Computations may perform effects and include forcing thunks, computation-function application,
do-bindings, and returning values.

`VType` classifies value types and `CType` classifies computation types.
The boundary constructors are `Thk B`, which suspends a computation as a value,
and `Ret A`, which classifies a computation returning an `A`.
Kinds are classified by the meta-level `Set`, which has no source term form.

A checked source can be a kind, type, value, or computation.
The CLI's `run` and `build` commands impose an additional entry boundary:
the root must accept the Builtin package and end in that package's `OS` computation protocol.
For example, `ret 1` is a valid checked term and can be evaluated in the REPL, but is not a standalone CLI executable.
[README.md](README.md#quick-start) shows the complete executable form.

### Computation Types as Stack Protocols

A value type classifies inert data; a computation type classifies a continuation stack.
Read `M : B` as saying that `M` can consume a stack with protocol `B`, not that it produces a value of type `B`.
`Ret A` expects a return continuation accepting an `A`; `A -> B` expects an `A` argument above a residual `B` stack;
codata describes alternatives of observable frames.

> **`Ret` is an installed continuation, not a stack-frame marker.** Stack extent can depend on runtime control flow,
> including an unbounded number of argument pushes. The [source rule](docs/references/language.md#ret-and-stack-extent)
> forbids inferring a fixed frame size or an allocation/reclamation boundary from `Ret` alone.

Stack shape is a typed control protocol, not a physical layout: native code may use the machine stack,
while WebAssembly may use explicit frames and a trampoline.
`Thk B` is suspended code compatible with a `B` stack, not the stack itself.

`OS` is the root protocol. An `OS`-typed computation consumes the process stack prepared
by the operating system and adapted by the launcher and runtime.
It has no ordinary source-level return: it transfers control to another `OS` computation,
terminates, aborts, or diverges.
Here, *consume* describes control flow, not linearity or destructive stack mutation.

The host interface illustrates the resulting control convention:

```text
write : Thk (String -> Thk OS -> OS)
exit  : Thk (Int64 -> OS)
```

`write` receives an explicit `Thk OS` successor, whereas `exit` terminates.
The successor is suspended code, not automatically a captured machine stack.
An FFI must therefore return an `A` for `Ret A`, but select a successor or terminate for `OS`.

The [returning C import proposal](docs/proposals/c-ffi.md) specifies the initial classifier-to-ABI mapping,
its borrowed-buffer contract, and the call plan shared by the interpreter and native backend.

## Source Terms and Imports

Every Zydeco source file contains exactly one complete term.
A file contributes no surrounding context: all names are bound by forms in the term itself.
In particular, `param` and `param val` are ordinary term forms that construct abstractions;
they do not declare file parameters.

After its own imports and optional companion annotation have been assembled,
a source root is resolved and type checked under an empty context and must synthesize its classifier.
An expected classifier at an import site may be compared with that result, but it does not participate
in elaborating the imported source.

Imports are typed metadata on holes, such as `@(import("library.zy"))`.
Parenthesized metadata `@(meta)` abbreviates the bracket form whose payload is a hole,
so `@(import("library.zy"))` names the same import.
A compiler session discovers the file dependency graph, orders providers before their consumers,
and materializes each provider as one shared term node.
The provider is resolved and checked once under its own empty context;
every import occurrence is an edge to that checked root.
A source boundary prevents free names and mobile block bindings from crossing between the two terms.
Sharing is static: an imported computation is still evaluated at every dynamic occurrence.

An implementation source `foo.zy` may have an adjacent companion `foo.zyi`.
The companion contains one ordinary type term and must itself synthesize a type.
Source assembly treats the pair as the annotated term `(contents-of-foo.zy : contents-of-foo.zyi)`.
Companion files participate in the same dependency graph, may use imports, and may themselves be imported as type terms.
Companion discovery applies to reusable `.zy` sources only; `.zydeco` program roots remain unpaired.

Text blocks attached to holes supply multi-line string values:
`--| text` immediately above `@(literal)` replaces the hole with the recovered text as a string literal,
so embedded prose shares the attachment discipline of repository documentation.

Typed value and computation holes remain available during checking and type inspection.
Execution and lowering require a complete residual runtime program: a hole reachable through its values,
thunks, or computations is rejected before any runtime effects occur.
This check follows static elimination, so holes confined to eliminated static definitions do not block execution.
Type annotations and wildcard patterns are outside this runtime completeness check.

### Lexical and Block Bindings

`param P in body` introduces an abstraction, and `let P = value in body` introduces a lexical binding.
For type definitions, `let` is transparent and `def` introduces a nominal seal.
A `begin ... end` block also accepts bindings connected with `that`.
Their scope is the nearest block, and the resolver schedules them by dependency rather than textual order.
For example, a block can refer to a later definition when the dependency graph admits an elaboration.

Resolution installs the block's binders before resolving their uses and records dependencies
from right-hand sides and pattern annotations.
Dependencies propagate through nested blocks to active enclosing bindings.
The resulting condensation graph orders acyclic parameters and definitions;
recursive type components retain an explicit `RecGroup` so the checker introduces identities before equations.
The scoped elaboration uses `Abs`, `ValAbs`, `Let`, and `Sealed` for the corresponding ordinary judgments.
Lexical `in` forms elaborate directly, while runtime recursion remains explicit through `fix`.
The [term design](docs/proposals/term.md) specifies the binding and scheduling rules.
The [language reference](docs/references/language.md#2-lexical-structure-and-syntax) specifies surface syntax;
the [style guide](docs/style.md#reading-the-surface-syntax) explains how classifier arrows, term bodies,
and constructor and destructor spines guide the reader.

## Products and Existential Packages

Products group values. Existential packages also carry type witnesses on which later components may depend,
allowing a package to expose operations while hiding their representation types.
A telescope is an ordered sequence of binders in which later classifiers may refer to earlier bindings.

Packages may be introduced by `pack`, opened by patterns, and composed through products,
named components, constructors, other packages, and function arguments and results.
Their package-witness bindings and structural projection routes are resolved statically
under the [shared phase contract](docs/references/language.md#10-static-elimination).
Representable package payloads may also flow through computation parameters and returns.
[Package modularization](lib/std/README.md#explicit-runtime-contracts) distinguishes this static composition
from explicit runtime contracts, which may use products of thunks or thunked arrows, `forall`, codata,
and package-dependent computation `pi`.

Parenthesized comma sequences are preserved by the surface `Cons` variant over a flat component vector.
The type checker interprets them as value products or existential packages from the expected type,
and applies the same rules to patterns.
`()` is the explicit `Triv` term or pattern and checks at `Unit`; a nonempty `Cons` stores its components
in one vector with no distinguished final element, so nesting survives only through explicit parentheses.

The infix product operator builds one flat n-ary node: `A * B * C` parses as a single three-component product,
and an explicitly parenthesized component stays a distinct nested product.
Products are therefore neither left- nor right-associative; `A * (B * C)`
and `(A * B) * C` are different types from `A * B * C`.
Stack IR derives its physical arity from the component count, so a flat product lays out its components contiguously
while an explicitly nested component stores its own sub-product by pointer.
Product layouts are always nonempty; `Triv` is carried separately through the backends.

### Named Components

Names are an orthogonal wrapper rather than a separate record calculus.
Two surface constructors distinguish classification from introduction:

- `#field :: classifier` says that the classifier expects a payload carrying `field`.
- `#field = term` introduces a payload carrying `field`; the same syntax in a pattern eliminates the wrapper.

The `#` marker distinguishes a field name from a variable or binder wherever the two could be confused:
a field standing on the left of `=` or `::` is always marked, so an unmarked `=` is always a binding separator
and a bare identifier is always a variable or binder.
Positions that already announce a field carry no marker: `term/field` projects, `/field = pattern` searches,
and `= field` puns.

This distinction matters because a payload type does not itself contain its field name.
In particular, `(#field = value) : (#field :: A)` relates the term-level name to a classifier
that records the same name, rather than reusing `=` structurally at both levels.

The value-level rules are:

```text
Γ ⊢ A : VType                   Γ ⊢ value : A
─────────────────── LABEL-V     ─────────────────────────── NAME-V
Γ ⊢ (#field :: A) : VType       Γ ⊢ (#field = value) : (#field :: A)
```

Named values remain limited to value types.
Zydeco does not yet have a corresponding introduction form for computations.
A computation can occupy a named value component through `Thk`.

The same distinction lifts one level to named types and named kinds:

```text
Γ ⊢ K : Set                   Γ ⊢ A : K
────────────────── LABEL-K     ─────────────────────── NAME-T
Γ ⊢ (#field :: K) : Set        Γ ⊢ (#field = A) : (#field :: K)
```

For example, `(#item = Int64) : (#item :: VType)` is a type-level judgment,
while `#item :: Int64` is the value type classifying values such as `(#item = 1) : (#item :: Int64)`.
A type constructor can be named at its higher kind in the same way.
This complete example, saved in the repository root, checks `42` against a type obtained by projecting
and applying the named constructor:

```zydeco
param (/Ret; /VType; /Int64) : @(import("lib/std/builtin.zy")) in
let Identity : VType -> VType = fn (X : VType) => X in
let NamedIdentity : (#constructor :: (VType -> VType)) = (#constructor = Identity) in
let IntAgain = NamedIdentity/constructor Int64 in
ret (42 : IntAgain)
```

`Set` remains the meta-level classifier of kinds.
There is no named-kind introduction `#field = K`, because that would require a first-class `#field :: Set`;
the hierarchy therefore stops cleanly at named kinds.
Labels preserve the existing level instead of adding subkinding or coercions,
and two labeled classifiers unify only when both their labels and payload classifiers agree.

Named product types use the existing product operator:

```zydeco
(#x :: A) * (#y :: B)
```

Their term and pattern forms use the existing comma tuple syntax:

```zydeco
(#x = a, #y = b)
(#x = p, #y = q)
```

When a field and a variable or pattern binder have the same name, prefix `=` provides field-punning syntax:

```zydeco
(= x, = y)                 -- equivalent to (#x = x, #y = y)
(= x : Int64, middle, = y)  -- the annotation describes the payload x
```

The set of valid field names is exactly the set of valid variable names; the `#` marker,
consumed by the lexer like the constructor `+` and destructor `.` prefixes, carries the role distinction.
The parser expands the shorthand directly into `Named` syntax.
In a term it creates an ordinary same-spelled variable reference;
in a pattern it creates an ordinary same-spelled binder.
Because parsing remains sort-agnostic, the same syntax may refer to a type variable in a type position.
Non-variable payloads must continue to use the explicit `#field = term` form.

In particular, `(#x = A, #y = B)` is not alternate product-type syntax.
Depending on its expected sort, it can be a tuple containing named values or the witness prefix
of an existential package containing named types.
Only `*` forms a product type, and its named components use `::`.
Product order and explicit grouping remain significant, and named and unnamed components may be mixed.

The parser preserves `#field = ...` as `Named` and `#field :: ...` as `Label`,
while continuing to defer their precise sorts to type checking.
Named projection uses postfix slash syntax: `term/field`.
Selection associates to the left, making `term/outer/inner` a path through nested named terms.
Its receiver undergoes ordinary lexical or global name resolution,
while field labels are checked statically rather than resolved as variables.
Slash is reserved exclusively for named projection; dot remains exclusively the elimination syntax
for computation destructors, preserving the value/computation distinction.
Slash binds tighter than the undelimited prefix forms `!`, `ret`, and constructor introduction,
which in turn bind tighter than application and computation destruction.
Consequently, `! package/action argument` means `(! (package/action)) argument`,
`ret package/value` returns the selected value, and `+Some package/value` constructs `+Some(package/value)`.
Parentheses express the converse grouping, as in `(! thunk)/field`.
Application arguments also retain tight projections, so `f value/field` means `f (value/field)`.

At the annotation layer, `:` binds more tightly than named-component `=` and `::`.
The two named-component operators share one precedence level and associate to the right:

```text
#field = value : A           ≡  #field = (value : A)
#field :: A : K              ≡  #field :: (A : K)
#outer = #inner :: A         ≡  #outer = (#inner :: A)
#outer :: #inner :: A        ≡  #outer :: (#inner :: A)
```

The annotation operator is non-associative.
Parentheses therefore state whether an annotation describes a payload or the complete named component.
The canonical judgment spelling is `(#field = value) : (#field :: classifier)`;
leaving off the first pair would annotate only `value`, and a named classifier used
to the right of `:` must itself be parenthesized.
The same parentheses also keep named components from capturing the right side of ordinary operators:
`(#field :: A) * B` labels only `A`, whereas `#field :: A * B` means `#field :: (A * B)`.

A parenthesized semicolon pattern applies every member to the same bindee.
For example, `((left, right); whole; copy)` destructures a pair and binds the complete pair twice.
Semicolon is same-bindee composition, whereas comma assigns successive product components.
Members retain source order and extend the pattern environment from left to right.
Same-bindee aliases require [irrefutable value members](docs/references/language.md#7-patterns-and-coverage).
A group of direct field projections may additionally select static and dynamic fields
while opening one package telescope.
Irrefutable whole-value members retain that package for forwarding; general constructor aliases
and arbitrary static aliases remain future extensions.

Named projection recursively searches transparent named classifiers, product components,
and the telescopes of nested existential packages.
Kind, manifest, and abstract binders contribute their public field names on the same terms
as `#field = value` components, so one field-name universe spans the package system and named values.
It requires exactly one matching field across the complete structure and exposes the payload beneath `Named`; missing
and ambiguous matches are distinct static errors, and matches at different depths or of different sorts still collide.
Other type constructors are opacity boundaries for term projection.
Manifest packages are transparent to it, while a package with abstract witnesses is sealed:
its fields count for uniqueness, but opening it changes identity and scope,
so only a projection pattern, which is an elimination form, selects through it.
An explicit chain performs a fresh search at each slash, so `term/outer/inner` can state or disambiguate a path.
Type projection is the static counterpart over nested named kinds: if `T : (#field :: K)`, then `T/field : K`.
A concrete projection `(#field = A)/field` reduces to `A`.
Projection from an abstract named type remains explicit in the typed syntax and reduces
when the abstract type is later instantiated.

The pattern `/field = pattern` uses the same search against its bindee,
then checks `pattern` against the unique payload.
It associates to the right, allowing `/outer = /inner = payload` to express a staged path.
Type checking elaborates the result into ordinary named and product patterns with typed holes outside the selected path.
The pun `/field` expands to `/field = field`, while `/field : Type` annotates that generated payload binder.
Projection payloads must be [irrefutable](docs/references/language.md#7-patterns-and-coverage);
refutable nested constructor matching through this pattern form is not implemented.

When a same-bindee group of direct projection patterns is checked against a package,
it is also the package's selective elimination form.
The checker opens the leading static telescope once, including manifest-kind and existential entries.
Unselected abstract fields receive anonymous witnesses,
while checking a package-dependent abstraction reuses the canonical witnesses of its arrow.
Selected static payloads bind those same witnesses or manifest definitions; the checker substitutes the opening
through the package body and resolves selected dynamic fields structurally.
The same search continues into nested packages: a selection may name a field
of a package sitting inside a product component, each distinct nested package occurrence receives one shared opening,
and selections through the same occurrence agree on its witnesses.
Thus `let (/Item; /value; /consume) = package in ...` gives all three selections one package identity
without naming every intervening field.
A whole-value member in the group retains the opened witness prefix, allowing the package
to be forwarded without reconstructing its positional telescope.
Plain existential binders contribute their binder name as a punned field;
explicitly named binders contribute their public label.
Missing and ambiguous package fields use the ordinary projection errors.

Manifest existential binders compose from the same pattern constructors.
The fully grouped form `exists (#field = ((X as A) : K)) . B` places the transparent binder `X as A`
inside its payload annotation, then wraps that payload with the ordinary named pattern `#field = ...`.
The compact punned spelling `exists (= X as A : K) . B` expands to `exists (#X = ((X as A) : K)) . B`;
`exists` itself adds no field-punning rule.

### Package Introduction

The comma form introduces a package only in check mode, because an expected type must say
which leading components are witnesses.
Recovering the abstract body of an existential from its concrete payload alone would be abduction,
so synthesis instead requires the witness bindings to appear in the term.
`pack` is that introduction form:

```zydeco
pack (X as A : K) (Y : K) is B where c_1, c_2, ..., c_n end
```

For type witnesses, the telescope reuses existential parameter shapes:
abstract or manifest, named or plain, punned or not.
Each binder additionally carries its witness in the term.
A manifest parameter keeps the type-level spelling `X as A` and discloses the witness in the synthesized type.
An abstract parameter states its witness as sealed evidence after `is`;
the synthesized type keeps the binder abstract, and the witness rides only in the package value.
A parameter with neither form is rejected: an introduction must name its evidence.
The evidence itself is one atomic term, so a compound witness parenthesizes
and a following parameter's parenthesis is never absorbed as an application argument.
The `where` body is one nonempty comma sequence at the tuple-element level, so annotations,
named components, and a trailing comma are all available; a single component is the payload itself,
and `where () end` packs the explicit `Unit`.

Synthesis assigns the package its type directly:

```text
Γ ⊢ A : K                     Γ, X ↦ A : K ⊢ v ⇑ B
─────────────────────────────────────────────────────  PACK-SYN
Γ ⊢ pack (X as A : K) . v ⇑ exists (X as A : K) . B

Γ ⊢ W : K                     Γ, X ↦ W : K ⊢ v ⇑ B
─────────────────────────────────────────────────────  PACK-SEAL
Γ ⊢ pack (X : K) is W . v ⇑ exists (X : K) . B[W ↦ X]
```

The payload is always checked against the disclosed witness, and its type `B` becomes the existential body.
Sealing then rewrites the witness's occurrences in `B` into the binder, so the body stays dependent
on the seal exactly where the payload speaks about the witness; what the payload leaves concrete —
such as a literal's primitive type or an intrinsic the witness already normalizes to —
stays concrete, and the emitted body is then simply witness-independent, which is sound.
The rewrite catches a witness that elaborates to an abstraction of its own: opaque definitions such as data types,
and the abstract witnesses a package opening introduces.
A witness defined as a transparent type function has no such abstraction by the time the payload is elaborated,
so sealing such a witness leaves the body concrete; the comma form, whose inversion checks the payload
against the expected body under a skolem, remains the spelling for that case.
An unannotated payload such as `pack (X : VType) is Int64 where 42 end`
therefore synthesizes the degenerate but sound `exists (X : VType) . Int64`.
The manifest form stays in the synthesized type, so a disclosed value joins a manifest expected existential
by the ordinary least-upper-bound operation, and a sealed value joins an abstract one once their bodies agree
under the respective binders.
Checking against a type first synthesizes and then joins.

Both spellings elaborate to the same witness-prefixed value, so elimination,
dynamics, and the backends cannot distinguish them.
The comma form remains preferable when the package type is already known, while `pack` removes the annotation
from the enclosing binding.

Type patterns make one additional distinction visible.
A named pattern `(#field = X) : (#field :: K)` binds `X : K` to the payload,
whereas a plain pattern `Whole : (#field :: K)` binds the complete named type.
Typed `forall`, `exists`, and type-function binders retain this pattern shape.
Consequently:

```text
(fn (#field = X) => B) (#field = A)  ↦  B[A/X]
(fn Whole => B) (#field = A)         ↦  B[(#field = A)/Whole]
```

The same payload extraction is used when existential witnesses instantiate a package-dependent result.
Retaining the pattern is necessary for sound substitution; reducing every type pattern
to one abstract identifier would confuse the payload kind `K` with the whole named kind `#field :: K`.

Named structure does not enter StackIR.
Type checking resolves each projection to the sequence of physical product positions on its unique path;
a path may be empty when only named wrappers are traversed.
Lowering erases named steps and translates each product step to an ordinary full-arity tuple pattern and `let`.
Subsequent backends therefore see only the existing tuple representation and layout.
Named types, named kinds, and static projections are also compile-time-only and have no runtime representation.
Selective package patterns use the existing existential `SCons` plus value-pattern aliases,
so they likewise add no runtime module representation.

## Value Functions

`val P => V` introduces a total value function and `val pi P . A` classifies it.
Type parameters erase; value parameters must be irrefutable, and shared static elaboration reduces an application
by resolving its head to the abstraction and binding the argument in its lexical environment,
so no closure or environment is built for a value function.
`param val P in V` is the lexical block-form introduction,
while its `that` variant contributes the same value parameter to the nearest `begin` context.
Plain `param` continues to introduce type functions and computations.
`let val` is ordinary non-recursive binding sugar.
Juxtaposition, `value |> function`, `function <| value`, and the view pattern `function ~> pattern` are one operation;
application can rearrange values, select known alternatives, and calculate integers without entering computations.
Only the nested pattern of a view contributes bindings and refutability.
Static composition admits partial applications, products, constructors, packages, and higher-order parameters
and results under the [shared residual contract](docs/references/language.md#10-static-elimination).
Runtime callable values remain explicit suspended computations behind `Thk`;
failed static elimination does not implicitly construct one.
[L8](docs/references/language.md#8-value-functions-and-views) specifies value functions and view patterns;
[C6](docs/references/compiler.md#static-elimination) owns their implementation boundary.

Package parameters may open existential witnesses used by the result classifier.
Both computation package-dependent arrows (`PackPi`) and value-function classifiers (`ValPi`) retain those witnesses;
`ValPi` also records the structural route through the parameter pattern by which application recovers them.
A binder that opens abstract witnesses elaborates to a package-dependent arrow on its own.
A thunk of that arrow may be passed or selected at runtime: its signature supplies the static witness dependency,
so the callee implementation need not be statically known.
A package may also be passed through a plain computation arrow when its residual payload is representable.
These are static dependencies on type identities, rather than dependencies on arbitrary runtime values.
Libraries compose through these functions and packages without an additional module or namespace sort.

For a package `exists (X : K). A X`, the interfaces `pi ((X, x) : Package). C X`
and `forall (X : K). A X -> C X` admit explicit packaging and unpackaging adapters.
Their thunked implementations may remain runtime values, and `C X` may itself be a codata protocol.
This correspondence preserves witness scope; it does not make the two classifiers definitionally equal.
The [runtime contract account](lib/std/README.md#explicit-runtime-contracts) includes the adapters
and the distinction between polymorphism and a provider's hidden representation type.

## Classifier Extraction

`@[typeof] e` makes an existing synthesized classifier available to source annotations and static definitions.
It preserves the operand's semantic identities while erasing its execution.
The [language reference](docs/references/language.md#4-classification-and-inference) owns classifier extraction,
staging, and inference;
its [source-boundary account](docs/references/language.md#12-sources-imports-and-entry) covers imports and signatures.

Classifier queries share the checked-term repository with source providers and monadic elaboration.
The [compiler reference](docs/references/compiler.md#inference-and-reuse) owns the synthesize-once protocol,
context extension, and separation from ordinary metadata forwarding.

## Standard Library and Host Boundary

Compiler-canonical kinds and fixed-representation types are manifest fields on the surface
of `lib/std/builtin.zy`, the launcher-supplied package contract, so selecting `Int64`,
`Thk`, or `Ret` is one field search and every selection shares one intrinsic identity.
Its operation groups are `numeric`, `text`, and `system`, and the field search descends through them,
so a source selects `(/stdio; /process)` without naming the enclosing group.
Fixed-width numbers, `Char`, `String`, and `Bytes` are compiler-canonical types.
`Reader`, `Writer`, and `OS` are abstract provider capabilities whose uses share one generative opening.
This separates stable data representations from runtime ownership.
The [language reference](docs/references/language.md#13-primitive-values-and-capabilities) defines
that identity boundary;
the [package design](docs/proposals/package-modularization.md#primitive-identity-and-package-boundaries)
explains the resulting organization.

`lib/std/std.zy` is a value function that assembles the public library from a Builtin argument.
The `data`, `numeric`, `text`, and `system` topic packages are also value functions;
topics that depend on the shared algebraic base accept that package alongside Builtin.
The aggregate carries the library-defined types and modules; host types stay on the Builtin contract.
Package functions are values, while effectful operations inside the resulting packages retain computation types.
See the [standard library guide](lib/std/README.md) for current entry points and package shapes.

## Relative Monads and Monadic Blocks

Relative monads are defined as codata in the standard library (see `lib/std/control/monad.zy`).
The module exports a value function from Builtin to the `Monad` and `Algebra` type package,
so importing and opening it requires neither a thunk nor a returned computation.
Zydeco also implements *monadic blocks*, a generalized do-notation selected by the `@[monadic]` metadata annotation.
The annotation may attach to any term.
During type checking, its payload undergoes the algebra translation implemented
in `lang/statics/src/elaborate/monadic/mod.rs` and invoked from `lang/statics/src/check/monadic.rs`.

Each annotated term resolves `Monad` and `Algebra` as ordinary types at its lexical site.
The checker verifies their expected higher kinds and records the selected constructors in the translation environment.
Translation checks each selected operation's codata membership and application type;
an incompatible lexical basis produces a type error at the monadic block.
It synthesizes the payload into the checker-wide checked-term repository,
then algebra translation consumes that immutable handle.
Each resolved monadic block retains one payload and one translated root;
a use-site expectation is compared with the canonical synthesized classifier afterward.
The translation retains lexical type bindings, including existential witnesses and transparent aliases.
Bindings introduced inside the block are translated with it.
A free term reference must have an inlinable definition that algebra translation can reinterpret;
an arbitrary captured runtime value is rejected.
Monad operations are supplied at runtime, and there is no general specialization pass eliminating their dispatch.

## Implementation Architecture

The
[compiler reference's phase map](docs/references/compiler.md#c1-architecture-and-a-programs-path-through-the-compiler)
connects source loading and checking to interpretation, SPS lowering, and native or WebAssembly emission.
SPS is stack-passing style: calls and continuations become explicit in the intermediate representation.
High SPS retains lexical branch joins; normalization simplifies known producers
and consumers before closure conversion constructs first-order SPSLow.
SPSLow checks explicit environment/result entry roles and package provenance
under the [word entry contract](docs/references/compiler.md#word-entry-contracts).
ZASM makes the control-flow graph explicit for assembly-derived targets.

| Responsibility | Owning reference | Implementation |
| --- | --- | --- |
| Source graph, overlays, and shared analysis | [C3](docs/references/compiler.md#c3-source-loading-sessions-queries-and-memory-retention) | `lang/session/src/source` |
| Parsing, desugaring, and resolution | [C4](docs/references/compiler.md#c4-parsing-desugaring-and-name-resolution) | `lang/surface/src/{textual,bitter,scoped}` |
| Typing, finalization, elaboration, and validation | [C5–C6](docs/references/compiler.md#c5-typed-representation-judgments-and-inference) | `lang/statics/src` |
| Linking and interpretation | [C7](docs/references/compiler.md#c7-linking-and-the-reference-interpreter) | `lang/dynamics/src` |
| High SPS, demand, and closure conversion | [C8–C9](docs/references/compiler.md#c8-high-sps-lowering-normalization-and-demand) | `lang/stackir/src/{high,low}` |
| ZASM and representation analysis | [C10](docs/references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices) | `lang/assembly/src` |
| Native preparation, emission, and runtime | [C11–C12](docs/references/compiler.md#c11-native-preparation-activation-frames-and-amd64-emission) | `lang/{assembly,amd64,machine}`, `runtime` |
| WebAssembly emission and embedding | [C13](docs/references/compiler.md#c13-webassembly-backends-and-embedding) | `lang/{wasm-am,wasm-sps,wasm-common}` |

Each completed representation has one selected program root and the storage needed by its syntax.
Phase-owned builders publish immutable products; later metadata lives in phase-local deltas.
[C2](docs/references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance) defines allocation
and provenance, and C8–C9 define lexical ownership before control-flow lowering.
Local representation selection accepts a Rust policy type or a per-compilation strategy enum;
[C10's policy boundary](docs/references/compiler.md#policy-selection) keeps preferences separate
from the evidence that permits removing a cell.
Policies feed assembly lowering before stack and frame validation.

### Query-Based Analysis

Source and statics queries share one Salsa database and revision system.
Allocation-producing judgments return fragments for the checker to materialize;
stateful inference and elaboration retain a mutable algorithmic core.
[Query ownership](docs/references/compiler.md#query-and-checker-ownership) defines this boundary,
and [analysis retention](docs/references/compiler.md#analysis-facts-and-materialization) distinguishes
keyed tooling facts from full typed-tree consumers and fine-grained memos.

The [checker module guide](lang/statics/src/check/README.md) locates local judgments.
[Finalization](docs/references/compiler.md#finalization) resolves and normalizes the shared type graph;
[coverage](docs/references/compiler.md#coverage) validates typed matches;
[static elaboration](docs/references/compiler.md#static-elimination) records the residual root used
by both interpreter linking and SPS lowering.
The optional [typed-arena lint](docs/references/compiler.md#typed-arena-lint) checks the published artifact's integrity.
The [memory](docs/proposals/arena-gc.md), [pattern](docs/proposals/exhaustiveness.md),
and [lint](docs/proposals/tyck-lint.md) records retain the remaining design questions.

### Source and Editor Analysis

The session owns revisioned source inputs and immutable analysis results shared by the CLI, TUI, and Cajun.
Lowering schedules live with Stack IR and assembly.
The CLI owns native tool invocation, runtime packaging, and process policy; diagnostic frontends own presentation.

Interactive tooling also needs answers for unfinished programs.
Strict and recovering parser entry points share one LALRPOP grammar and the Logos token definitions.
Recovery retains partial syntax and typed diagnostics; strict compilation rejects any parse issues.
Strict parse failures retain their structured issues and the rejected source snapshot through loading and formatting,
so diagnostics can render byte-accurate snippets even after an editor buffer changes.
Completion only uses a recovered cursor hole when it is reachable from the returned root
and a hole was legal at the original cursor position.

`CompilerSession::complete` uses recovering parsing for the edited root and ordinary source loading
for dependencies, including overlays and companions.
Source assembly and desugaring preserve the exact cursor-hole identity.
The resolver snapshots the ordinary lexical environment at that hole, so completion follows the same shadowing,
block, branch, and source-boundary rules as compilation.

The session compares visible names with the hole's expected classifier when available.
It removes definite mismatches, retains candidates with unknown compatibility,
and orders candidates by exact spelling match, classifier compatibility, lexical proximity, and spelling.
Compatibility checks use a disposable recovered analysis; they do not solve holes in a strict analysis.
Cajun renders optional kind or type details and inserts only the selected name.
Missing type information does not itself hide a candidate.
The [completion design](docs/proposals/completion.md) records recovery, filtering, and edit-range contracts.

Compiler annotations have one typed catalog in `lang/surface/src/metadata.rs`.
Metadata decoding and editor suggestions share argument shapes and enum spellings, including nested options.
Unknown metadata stays structurally valid without compiler-defined suggestions.
Import-path completion uses the importer's canonical parent, merges filesystem entries with active overlays,
and offers directories and supported source files while excluding the importing file and its symlink aliases.

The session's `DocumentationIndex` connects `@[doc]` attachments to resolved bindings, expressions, and named members
through exact source origins; field provenance recorded during type checking survives normalization and substitution.
Hover, completion, reference generation, and the VS Code documentation panel consume this shared model.
The [project documentation proposal](docs/proposals/documentation.md) records the invariants,
and the [authoring guide](docs/documentation.md) describes the workflow.

`zydeco doc show`, `search`, and `build` present a selected root's exposed classifier rather
than private source bindings: public paths distinguish named fields from function and computation results,
reject ambiguous exposures, and never contain arena identifiers.
`doc check` verifies explicitly opted-in examples in a bounded subprocess worker
and maps diagnostics back to their code fences without interpreting them.
Cajun versions documentation and example requests against a shared source revision,
so editing an imported file also invalidates a consumer's cached documentation.

Each parsed entity, including nested metadata, has its own source span.
The assembled program uses a shared `SourceMap` to associate byte offsets with their files;
compiler diagnostics retain a primary location, stable code, and optional semantic relationships or help.
Unsolved classifiers render as `_`.
A primary error suppresses follow-on missing-solution messages for its failed expression
and enclosing inferred classifiers; unrelated holes remain visible.
Nominal mismatches distinguish the two abstract identities within the diagnostic and label their source introductions,
so repeated names still identify the relevant seals, package openings, or type parameters.
The checker task stack remains an internal trace. CLI and TUI render the diagnostics with Ariadne,
while Cajun converts byte spans to the client's UTF-16 positions at the LSP boundary.
Document revisions invalidate stale completion responses.

Cajun retains negotiated client capabilities and reads runtime preferences from a revisioned configuration snapshot.
Valid updates replace the snapshot, omitted options return to defaults,
and invalid updates retain the previous settings.
Hover and completion read one snapshot per request, so presentation changes need no compiler cache invalidation.
See the [editor configuration guide](editor/README.md#runtime-configuration) for settings and client setup.

### Interactive Inputs

The REPL stores submitted terms as numbered session overlays, reusing the file-source model.
`@(import(1))` refers to source input `[1]`; a quoted target such as `@(import("1"))` is a filesystem path.
Both imports retain the same hygienic boundary and static sharing rules.
A type checking rejection preserves the editor and reserves the input number for a corrected retry.

Root metadata supplies frontend commands: `@[type]` requests static inspection,
`@[run]` explicitly requests evaluation, and `@(help)` and `@(quit)` control the REPL.
Default evaluation supports values and directly returning computations;
explicit execution may supply a Builtin host contract.
The frontend captures output and uses empty stdin and arguments.
The [interactive engine](docs/references/compiler.md#interactive-engine) explains its lifecycle,
and [CONTRIBUTING.md](CONTRIBUTING.md#use-the-interactive-repl) lists the commands and editing keys.

### Arena and ID invariants

Compiler-owned IDs contain an opaque `KeySpaceId` and a raw arena index; the Rust ID type identifies the node category.
There are two allocation strategies. `IdAllocator<Scope>` is a non-cloneable sequential issuer:
construction claims one process-unique key space, and allocation advances its local cursor.
The statics checker instead uses `DerivedAllocator`, while judgment queries construct derived IDs directly.
Both derive identity from an entity's full ID, its checking occurrence, a derivation-family tag, and a local slot.
Replaying the same site reproduces its IDs without a shared sequential cursor;
repeated checks of an entity use distinct occurrences.

Two separate type-level relations constrain IDs:

- `Scope: Allocates<Id>` declares which ID categories an allocator may issue.
  The scope belongs to the operation or pipeline lifetime that creates nodes;
  it is not stored on the ID and does not prevent independent allocators with the same scope.
- `Scope: ArenaSchema<Id, Item = T>` declares the contents owned by an arena representation.
  Since `Id` is a trait parameter, one scope can own several ID categories
  and the same ID can inhabit several representation scopes.
  This is used, for example, by the several node categories in Stack IR.

Storage and access have separate contracts:

- Dense storage wraps `la_arena::Arena`.
  The `la-arena` allocation itself supplies the raw index, so a dense arena retains only its identity tag
  and rejects IDs from another dense arena even when raw indices happen to match.
  Dense-only IDs have no external `Allocates` implementation.
- Externally issued IDs use sparse, paged, or indexed owning storage, depending on their density and access pattern.
  These stores retain the IDs supplied by their producer; changing storage does not change node identity.
  All owning stores are constrained by `ArenaSchema`.
- Associative side tables are deliberately not constrained by `ArenaSchema`: annotations,
  provenance, environments, caches, and relations legitimately associate one ID with many property types.
  They require callers to choose explicit `insert_new`, `replace_existing`, `upsert`, or set-like `ensure` semantics.
- `ArenaAccess` is the read capability shared by construction and consumption.
  `ArenaAccessMut` adds indexed mutation only for builders, while `FrozenArena<A>` carries an owned `A`
  across a phase boundary without exposing that capability.
  Consuming a frozen value can recover its storage for a structural rebuild,
  after which the new phase establishes its own frozen output boundary.
- Sequential issuers live on the operation that creates nodes, such as `Parser`,
  `Desugarer`, assembly `Lowerer`, and stack analysis.
  Their output arenas do not retain the cursor.
  The checker uses the derived allocation strategy described above.
  Stack IR is the deliberate exception: high SPS retains its definition issuer until the consuming SPSLow conversion,
  which moves that issuer into the low administrative arena for globally unique synthetic definitions.
  SPSLow nodes use a separate low-syntax issuer and never reuse high node IDs.
- Provenance tables encode their actual cardinality.
  In particular, repeated type checking and transparent syntax make surface-to-typed provenance many-to-many,
  while one typed node can lower to many stack-IR nodes.
- Parsed entities use a tagged `EntityId` enum, so definitions, patterns, copatterns,
  and terms cannot be confused through raw-ID casts.

## Runtime Representations

### Numeric Representations

Zydeco exposes fixed-width numeric types whose runtime domains match Rust's primitive representations:
`Int8`, `Int16`, `Int32`, and `Int64` use `i8`, `i16`, `i32`, and `i64`; `UInt8`, `UInt16`, `UInt32`,
and `UInt64` use the corresponding unsigned Rust types; `Float32` and `Float64` use `f32` and `f64`.
Integer arithmetic wraps within the selected representation, comparisons retain signedness,
and floating-point operations follow IEEE 754 at the selected width.
Integer division and remainder by zero stop execution with a clear runtime error and nonzero exit status.
Signed minimum divided by `-1` wraps to the signed minimum; the corresponding remainder is zero.
Float `to_string` uses Rust's `Display` spelling at the selected width: the shortest round-tripping decimal
without exponent notation, with `-0`, `inf`, `-inf`, and `NaN` for the corresponding special values.

An expected numeric type selects a literal's representation.
Numeric literals use decimal digits, with an optional decimal fraction or complete exponent for floats.
Unsupported prefixes and suffixes such as `0x1F` and `42u8`, and incomplete exponents such as `1e`,
are lexical errors rather than separate number and identifier tokens.
Integer literals must fit that representation; floating-point literals are rounded
to the selected width, including subnormal rounding and underflow to zero.
Literals that overflow the finite `Float64` range are rejected during parsing,
and narrowing a finite literal to `Float32` also rejects overflow.
When no expected type selects a representation, integer literals synthesize `Int64`
and decimal literals synthesize `Float64`.
There are no implicit conversions between numeric types for existing values.

At the AMD64 runtime boundary, a value occupies one machine word.
The low bit is a runtime tag:

- Odd words are immediate values. They represent `Unit`, constructor indices, `Char`, all integers through 32 bits,
  `Float32`, `Int64` values from `-2^62` through `2^62 - 1`, and `UInt64` values through `2^63 - 1`.
- Even words are pointer-shaped values.
  Region-allocated products and closures refer to scanned blocks in the fixed two-space heap.
  An `Int64` or `UInt64` outside the immediate range and every `Float64` instead point
  to an opaque one-word block containing all 64 payload bits.

This encoding preserves the full source-level numeric domains while letting the copying collector distinguish immediates
from movable pointers exactly.
Opaque scalar blocks are copied but their payload bits are never traced.
Aligned Rust-owned pointers, such as host strings, are outside both semispaces and remain unchanged.

### Shared Rust Runtime Model

[`zydeco-machine`](lang/machine/src/lib.rs) is a dependency-free `no_std` crate consumed
by compiler phases and the standalone native runtime.
It owns the tagged-word operations described above, the closure field order used by ZASM,
the AMD64 host-transfer representation, and nested environment transitions.
The frame model uses Rust's `alloc` for control metadata and growable environment storage.
Compiler-specific literal resolution stays in `zydeco-syntax`.

Closure records are parameterized by their word carrier.
Code generation derives byte offsets from `Closure<u64>` and `HostTransfer<u64>`,
so an ARM or 32-bit compiler host cannot change the AMD64 layout accidentally.
The native stub uses the same records with `usize` words; target-side assertions check their sizes,
alignment, and offsets against the 64-bit layouts.
The closure declaration also generates the field traversal used by ZASM packing and opening.
Likewise, the frame action declaration generates both the runtime record and its emitter traversal.

The host-control protocol is represented by `HostArguments` and a shared resumption catalog.
A host operation returns a pointer to a `HostTransfer` containing a bridge address, a closure, and up to two arguments.
The stub selects that address from the arguments' arity; the emitter generates each bridge
from the same catalog and record offsets.
The bridge loads the closure, pushes arguments in reverse consumption order,
pushes the closure environment, and jumps to its code.
The stub has one reusable transfer slot, consumed before another host call occurs.
This record is separate from a source-language `Ret` continuation on the control stack.

The compiler embeds a self-contained source bundle of the model and supplies it to each native build.
Both compilations derive the native entry symbol from a deterministic fingerprint
of the model sources, including its manifest and build script.
Different source identities therefore fail to link.
This is a conservative compatibility check: even a comment change requires rebuilding both sides.
It checks artifact pairing, while the shared definitions remove duplicated layout and protocol decisions;
it does not prove the correctness of handwritten instruction selection or host implementations.
See [native build packaging](CONTRIBUTING.md#compile-programs) for the build-directory contract.

The frame model implements entry, suspension, resumption, and sparse root discovery.
Native preparation checks activation ownership, initialized bindings, and continuation slot aliases before emission;
the emitter supplies static action descriptors consumed by the same model in the stub.
The [native preparation](docs/references/compiler.md#c11-native-preparation-activation-frames-and-amd64-emission)
and [environment actions](docs/references/compiler.md#environment-actions-and-roots) own those phase
and transition contracts.
Register assignment, stack alignment, control-stack instruction selection,
and individual builtin signatures remain outside the model.
The collector owns its private headers and forwarding algorithm,
while the model supplies allocation kinds and the immediate tag.
A future scheme can introduce its own state and transition types in this crate; there is no universal runtime trait.
Retained frames and experimental compact suspension fragments share the narrower `frames::Environment` capability:
the same noncollecting actions, nested tokens, declared captures, and mutable roots.
The [compact environment contract](docs/proposals/native-frames.md#experimental-compact-environments)
explains how the latter reuses active storage without changing emitted accesses.
The fixed and growable environment stores now share the narrow `frames::storage::Storage` contract.
The experimental moving-heap root adapter has its own contract: managed collection can relocate those frames,
which is outside the entry-only relocation capability of `Storage`.

### Native Environments and Control Stack

The AMD64 backend uses the machine stack for arguments, destructor tags, continuations, and temporary values.
The variable environment uses activation frames in a separate, geometrically growing Rust allocation.
Generated closure entries establish an active frame and load its base into `rbp`;
local branches preserve it, and return entries restore the retained caller's base.
Bindings use statically packed offsets within their activation, including bindings introduced by its continuations.
Packing accounts for both current execution and every possibly pending continuation.
Entry can relocate the backing allocation; saved frame offsets remain valid and the returned base replaces `rbp`.
Managed collection does not move this allocation.

Portable SPSLow and ZASM retain explicit capture products.
Native preparation uses checked continuation provenance to replace a return continuation's capture product
with a frame token.
Resumption reads the retained slots directly and binds the returned value.
Ordinary closures continue to own explicit capture environments, which can outlive their creating activation.
The [native activation frame proposal](docs/proposals/native-frames.md) owns the implemented lifetime,
entry, reclamation, root, and bounded tail-space invariants, together with the remaining optimization questions.

### Native Garbage Collection

The native runtime uses Cheney copying collection with two fixed 1 MiB semispaces.
The live graph must fit in one semispace, including block headers; allocation reports out
of memory when collection cannot make enough room.
Collection updates the control stack, live slots in active and suspended frames, and registered host roots,
preserving sharing, cycles, and word-aligned interior pointers into payloads.
Frame slot maps exclude reserved, uninitialized, and dead slots; word tags then identify immediate values.

Allocation receives a deferred root source.
If a cell fits, it advances the allocation cursor without enumerating frame or host roots.
If collection is needed, the source publishes the complete root set once,
and the collector updates those locations before allocation continues.
An oversized request rejected before collection does not enumerate roots;
a request rejected after collection still preserves the relocated live graph.
The native frame model continues to own which slots are live, independently of when they are enumerated.

Each semispace has a block-start index that locates the owning header with one index-region read and one header read.
For each 512-byte region, a bitmap records header starts at word granularity,
and a predecessor offset identifies a block crossing into that region.
The collector validates payload bounds before forwarding, so addresses into headers
or outside allocated space remain unchanged.
This avoids searching through preceding live or dead blocks for every pointer.

Allocation and copying share the index's block-recording operation.
It initializes regions as the allocation cursor enters them, replacing stale metadata
when a semispace is reused; entries beyond the current cursor are never consulted.
Collection therefore needs neither an index rebuild over dead objects nor a sweep to clear the whole index.
Lookup work scales with inspected pointer references, while copying, scanning,
and destination-index maintenance scale with the live graph.
Each index occupies 32 KiB per 1 MiB semispace, adding 3.125% to the space reservation.
The implementation and boundary, reuse, and lookup-work regressions live in [runtime/gc.rs](runtime/gc.rs);
the workspace runs those tests through the `native_gc` target in `zydeco-tests`.

### Text and Bytes

`String` is immutable UTF-8 text.
String indices and lengths count Unicode scalar values; `byte_length` measures its UTF-8 encoding.
These are distinct from grapheme clusters and from compiler source spans, which use byte offsets.
`Char` is one Unicode scalar value.
`Bytes` is an immutable octet sequence with no implicit encoding, and its indices and lengths count bytes.

String and character literals share the escapes `\\`, `\"`, `\'`, `\n`, `\r`, `\t`, and `\0`.
Unicode escapes use `\u{...}` with one to six hexadecimal digits denoting a Unicode scalar value;
surrogates and values above `U+10FFFF` are rejected.
Unknown or incomplete escapes are source errors.

Builtin operations report invalid observations through computation-polymorphic branches.
The library reifies those branches as `Option` for operations such as indexing,
splitting, parsing, and codepoint conversion, or as `Result` for fallible I/O.
Filesystem contents are bytes; text conveniences explicitly validate or produce UTF-8.
EOF, an empty line, and an I/O error have distinct results.
The [text and library contracts](lib/std/README.md#text-model)
and [filesystem design](docs/proposals/filesystem.md) describe these boundaries independently of runtime storage.

Explicit storage is available through the ordinary [memory library](lib/std/memory/package.zy).
The [static builder](lib/std/memory/static-layout.zy) computes checked plans with value functions,
exposes their placement information, and shares codecs with the runtime builder.
Its [owning design](docs/proposals/bytes.md#explicit-storage-contracts) specifies typed storage,
source-composed alignment and padding, and the boundary between logical values,
concrete buffers, and existing foreign borrowing.
The [stored-call interface](docs/proposals/escape-unboxing.md#stored-call-interfaces) shares abstract storage carriers
across source modules and supplies ordinary CBPV call and conversion adapters.
These calls use the existing runtime word transport.

### Returning C Imports

A foreign annotation supplies an implementation for a thunk.
The supported classifier has the form `Thk (A1 -> ... -> An -> Ret B)`, with each argument either a fixed-width integer
or `Bytes`, and result `B` a fixed-width integer or `Unit` (C `void`).
A byte buffer expands into a borrowed pointer and length, and the flattened C call admits at most six arguments.
The checker records one typed call plan used by the Unix interpreter's libffi path and the AMD64 emitter.
Checking a declaration does not load its library or validate the real C symbol's signature.
Both WebAssembly backends and the ZASM interpreter reject native foreign imports.
The [returning C import design](docs/proposals/c-ffi.md) specifies the supported ABI, borrowing obligations,
loader behavior, and acceptance and rejection tests.

## WebAssembly backend

WebAssembly emission forks at first-order SPSLow so the repository can compare two implementation strategies.
The `wasm-sps` target consumes SPSLow directly.
The `wasm-am` target first lowers SPSLow to ZASM, then embeds that abstract machine in WebAssembly.
Their explicit names keep the architectural choice visible while neither implementation is
yet the preferred unqualified WebAssembly target.
The CLI caches assembly lowering on demand, so selecting `wasm-sps` does not construct an unused ZASM program.
The [WebAssembly backend strategies proposal](docs/proposals/wasm-backends.md) records the alternatives,
prototype evidence, open runtime questions, and criteria for choosing a future `wasm` default.

### Structured SPS backend

SPSLow has already made closures and continuations first order: code is represented by explicit blocks,
closure packages pair an environment with code, and continuation packages pair code with a residual stack.
The structured backend maps the root and each SPSLow block to one WebAssembly function.
Lexical computations inside a block become structured instructions in that function,
and value bindings become WebAssembly locals instead of entries in a global environment array.

Dynamic jumps still require indirection because core WebAssembly does not expose raw function addresses.
The backend assigns tagged table-index handles to blocks and uses a trampoline between blocks,
so recursive Zydeco calls do not consume the host call stack.
Products, closure packages, boxed scalars, and persistent stack frames live in linear memory.
This retains SPSLow's block granularity without reconstructing the instruction-level ZASM machine.

### Abstract-machine backend

The abstract-machine backend consumes ZASM, the same first-order stack machine used by the native emitters.
It assigns every ZASM program point a private table index and emits each point as a `() -> ()` WebAssembly function.
The exported `entry` function repeatedly dispatches the current index through that table.
Direct jumps, dynamic continuation jumps, and branches all update the machine's program counter,
so higher-order control does not require tail-call or function-reference proposal features.

The reusable variable environment, one-megabyte operand/control stack, products,
closure packages, and boxed 64-bit scalars live in linear memory.
Exhausting or underflowing this stack reports a runtime error through the shared host ABI.
Products currently use a growing bump heap rather than a collector.

### Shared runtime ABI

Both backends use `i64` runtime data words and retain the native low-bit convention:
odd words are immediate values and aligned even words are pointer-shaped.
The SPS backend encodes its block handles as tagged immediates;
the abstract-machine backend keeps ZASM code addresses as backend-private table indices.
Generated modules import builtins from the `zydeco` namespace through these typed forms:

- A returning builtin accepts its Zydeco arguments as `i64` parameters and returns one `i64` runtime word.
- A control builtin accepts its Zydeco arguments and returns four `i64` values:
  an untagged argument count from zero through two, a module-created closure pointer, and up to two arguments.
  The backend supplies the arguments and closure environment before resuming the selected code block.
- An operation that may produce a boxed full-width scalar receives a trailing `i32` address for a one-word spare box.
  Narrow operations receive zero in this position when their shared ABI includes the parameter.

The additional `string_literal(i32, i32) -> i64` import receives an offset and UTF-8 byte length
in exported memory and returns the host's opaque string value.
The mandatory `runtime_error(i32) -> ()` import reports fatal language errors.
Its codes are defined by `RuntimeFailure` in `lang/wasm-common/src/host.rs`:
`1` means pattern-match failure, `2` operand/control stack overflow, `3` stack underflow,
`4` integer division by zero, and `5` integer remainder by zero.
The host must report the error and stop execution unsuccessfully.
Generated code traps if the host returns. Each module exports `entry`, the conventional `_start` alias,
and `memory`, but the embedding must supply the imports before invoking either function.

## Repository Layout

| Path | Role |
| --- | --- |
| `lang/` | Compiler phases, interpreter, emitters, utilities, test harnesses, and data-driven case fixtures under `lang/tests/cases/`. |
| `lang/machine/` | Rust representations and host-resumption descriptions shared by compiler phases and native stubs. |
| `lib/` | Standard library, reusable examples, and regression projects under `lib/tests/`. |
| `cli/` | Source checking, interpreter launch, formatting, and compilation commands. |
| `runtime/` | Runtime sources copied into native executable builds. |
| `tui/` | Ratatui REPL using the shared compiler session. |
| `editor/cajun/` | Language server, included in the Rust workspace. |
| `editor/tree-sitter-zydeco/` | Editor grammar and its conformance checks. |
| `editor/vscode/`, `editor/zed/` | Client integrations with their own build workflows. |
| `docs/` | Tutorials, executable literate chapters, proposals, and exploratory notes. |
| `web/` | Older browser frontend, excluded from the active workspace. |

## Current Limitations

- Value-function application inlines each body at its use, so emitted code and compiler recursion depth grow
  with the unfolded program, dominated by multiply-instantiated library functors.
  The workspace raises its test-stack minimum accordingly; factoring repeated residual code remains future work recorded
  in [residual-code sharing todos](docs/todos/deferred-designs.md#residual-code-sharing).
- Static reduction has the documented [resource bounds](docs/references/language.md#10-static-elimination)
  and does not enter computations to discover static functions or witnesses.
  Demand does not yet flow through runtime package-dependent computation applications,
  so their arguments materialize whole ([Package modularization](docs/proposals/package-modularization.md)).
- The standard native test path is AMD64 on Linux or macOS.
  The CLI defaults to the host architecture, so an ARM host needs explicit AMD64 target selection
  and appropriate tools for native execution.
- WebAssembly requires a `zydeco` host embedding.
  Both variants have growing, non-collecting heaps.
  The abstract-machine variant has a fixed one-megabyte operand/control stack;
  the SPS variant allocates persistent stack frames and boxes products without ZASM's local-unboxing analysis.
  The Node.js test host uses deterministic randomness.
- Native foreign imports support only the returning subset described above;
  callbacks and C-to-Zydeco exports are not implemented.
  Checking a source is not evidence that a foreign library can be loaded or linked.
- Imports address filesystem paths or numbered interactive inputs.
  Separate compilation and external package resolution are not implemented,
  and Zydeco source dependencies have no package lockfile.
  Absolute source imports are location-dependent and receive no portability warning.
- `pack` cannot introduce kind witnesses.
  Field-projection payload patterns and general same-bindee aliases are restricted to irrefutable forms,
  apart from the supported selective package opening.
- Monadic translation requires inlinable free term references.
  Lexical type bindings and terms introduced inside a block are supported; arbitrary captured runtime values
  and automatic removal of monad dispatch remain outside it.
