# Design

Zydeco is a proof-of-concept programming language based on call-by-push-value
(CBPV). It is used to study stack-manipulating computation and relative monads,
with a focus on a small, executable core and a clear compilation pipeline.

## Language Model

Zydeco separates values from computations.

- Values are inert and include variables, thunks, units, products, data
  constructors, and literals.
- Computations are effectful and include forcing thunks, function application,
  do-bindings, and returning values.

The core types include:

- `Thk <B>` to suspend computations as values.
- `Ret <A>` to return values as computations.
- `OS` to represent computations that run against the operating system stack.

The main program is required to have type `OS`.

Surface notation distinguishes classifier arrows from term bodies while leaving constructor and destructor
spines whitespace-guided. The [surface syntax principles](docs/ideas/syntax.md) record the rationale and the
intended use of juxtaposition, grouping, and block delimiters.

## Products and Existential Packages

Parenthesized comma sequences are preserved by the surface `Cons` variant using
the shared n-ary `ConsN` syntax. The type checker interprets them as value
products or existential packages from the expected type, and applies the same
rules to patterns. `()` is the explicit `Triv` term or pattern and checks at
`Unit`; a nonempty `ConsN<S, T>(Vec<S>, T)` stores an initial sequence and a
distinguished final element. The ordinary binary `Cons<S, T>` remains available
for compiler structures that are intrinsically pairs.

Product types remain binary. Stack IR derives a canonical physical arity from
their right-associated `Prod` spine, so `A * (B * C)` is laid out as three
contiguous fields while `(A * B) * C` stores the left product by pointer.
Assembly pack and unpack instructions carry both physical arity and logical
element count, allowing explicit nested grouping to use suffix pointers
without changing the canonical layout. Product layouts are always nonempty;
`Triv` is carried separately through the backends.

### Named Components

Names are an orthogonal wrapper rather than a separate record calculus. Two surface constructors distinguish
classification from introduction:

- `field :: classifier` says that the classifier expects a payload carrying `field`.
- `field = term` introduces a payload carrying `field`; the same syntax in a pattern eliminates the wrapper.

This distinction matters because a payload type does not itself contain its field name. In particular,
`(field = value) : (field :: A)` relates the term-level name to a classifier that records the same name, rather
than reusing `=` structurally at both levels.

The value-level rules are:

```text
Γ ⊢ A : VType                   Γ ⊢ value : A
─────────────────── LABEL-V     ─────────────────────────── NAME-V
Γ ⊢ (field :: A) : VType        Γ ⊢ (field = value) : (field :: A)
```

Named values remain limited to value types. Zydeco does not yet have a corresponding introduction form for
computations. A computation can still occupy a named value component through `Thk`, as before.

The same distinction lifts one level to named types and named kinds:

```text
Γ ⊢ K : Set                   Γ ⊢ A : K
────────────────── LABEL-K     ─────────────────────── NAME-T
Γ ⊢ (field :: K) : Set         Γ ⊢ (field = A) : (field :: K)
```

For example, `(item = Int) : (item :: VType)` is a type-level judgment, while `item :: Int` is the value type
classifying values such as `(item = 1) : (item :: Int)`. A type constructor can be named at its higher kind in
the same way:

```zydeco
alias NamedIdentity : (constructor :: (VType -> VType)) =
  (constructor = Identity)
end

alias IntAgain : VType = NamedIdentity/constructor Int end
```

`Set` remains the meta-level classifier of kinds. There is no named-kind introduction `field = K`, because that
would require a first-class `field :: Set`; the hierarchy therefore stops cleanly at named kinds. Labels preserve
the existing level instead of adding subkinding or coercions, and two labeled classifiers unify only when both
their labels and payload classifiers agree.

Named product types use the existing product operator:

```zydeco
(x :: A) * (y :: B)
```

Their term and pattern forms use the existing comma tuple syntax:

```zydeco
(x = a, y = b)
(x = p, y = q)
```

When a field and a variable or pattern binder have the same name, prefix `=`
provides field-punning syntax:

```zydeco
(= x, = y)                 -- equivalent to (x = x, y = y)
(= x : Int, middle, = y)  -- the annotation describes the payload x
```

The set of valid field names is exactly the set of valid variable names. The
parser expands the shorthand directly into `Named` syntax. In a term it creates
an ordinary same-spelled variable reference; in a pattern it creates an
ordinary same-spelled binder. Because parsing remains sort-agnostic, the same
syntax may refer to a type variable in a type position. Non-variable payloads
must continue to use the explicit `field = term` form.

In particular, `(x = A, y = B)` is not alternate product-type syntax. Depending on its expected sort, it can be a
tuple containing named values or the witness prefix of an existential package containing named types. Only `*`
forms a product type, and its named components use `::`. Product order and explicit grouping remain significant,
and named and unnamed components may be mixed.

The parser preserves `field = ...` as `Named` and `field :: ...` as `Label`, while continuing to defer their
precise sorts to type checking. Named projection uses postfix slash syntax: `term/field`. Selection associates to
the left, making `term/outer/inner` a path through nested named terms. Its receiver undergoes ordinary lexical or
global name resolution, while field labels are checked statically rather than resolved as variables. Slash is
reserved exclusively for named projection; dot remains exclusively the elimination syntax for computation
destructors, preserving the value/computation distinction. Slash binds tighter than application, so
`f value/field` means `f (value/field)`.

At the annotation layer, `:` binds more tightly than named-component `=` and `::`. The two named-component
operators share one precedence level and associate to the right:

```text
field = value : A           ≡  field = (value : A)
field :: A : K              ≡  field :: (A : K)
outer = inner :: A          ≡  outer = (inner :: A)
outer :: inner :: A         ≡  outer :: (inner :: A)
```

The annotation operator is non-associative. Parentheses therefore state whether an annotation describes a
payload or the complete named component. The canonical judgment spelling is
`(field = value) : (field :: classifier)`; leaving off the first pair would annotate only `value`, and a named
classifier used to the right of `:` must itself be parenthesized. The same parentheses also keep named components
from capturing the right side of ordinary operators: `(field :: A) * B` labels only `A`, whereas
`field :: A * B` means `field :: (A * B)`.

Named projection accepts a directly named value or searches only the immediate
product spine. It requires exactly one matching field and exposes the payload
beneath `Named`; missing and duplicate matches are distinct type errors. Type
projection is the static counterpart: if `T : (field :: K)`, then `T/field : K`.
A concrete projection `(field = A)/field` reduces to `A`. Projection from an
abstract named type remains explicit in the typed syntax and reduces when the
abstract type is later instantiated.

Type patterns make one additional distinction visible. A named pattern
`(field = X) : (field :: K)` binds `X : K` to the payload, whereas a plain pattern
`Whole : (field :: K)` binds the complete named type. Typed `forall`, `exists`,
and type-function binders retain this pattern shape. Consequently:

```text
(fn (field = X) => B) (field = A)  ↦  B[A/X]
(fn Whole => B) (field = A)        ↦  B[(field = A)/Whole]
```

The same payload extraction is used when existential witnesses instantiate a
package-dependent result. Retaining the pattern is necessary for sound
substitution; reducing every type pattern to one abstract identifier would
confuse the payload kind `K` with the whole named kind `field :: K`.

Named structure does not enter StackIR. Type checking resolves each projection
either to the payload of a directly named value or to a physical product
position. Lowering erases the former as an identity and the latter as an
ordinary full-arity tuple pattern and `let`; subsequent backends therefore see
only the existing tuple representation and layout. Named types, named kinds,
and static projections are also compile-time-only and have no runtime
representation.

### Source Organization and Modules

Every Zydeco source file contains exactly one complete term. Imports are typed
metadata on holes, such as `@[import("library.zy")] _`, rather than namespace
operations. A compiler session discovers a file dependency graph, orders providers
before their consumers, and substitutes a freshly cloned provider term at each
import occurrence. Parsed templates are memoized by source input, while each assembled
occurrence remains fresh. A source boundary around each clone prevents free names and
mobile block bindings from crossing the file boundary.

The session owns revisioned source inputs and immutable frontend analysis results shared by
the CLI and language server. Lowering schedules live with Stack IR and assembly, while the
CLI owns diagnostic rendering, native tool invocation, runtime packaging, and process policy.
This boundary keeps editor analysis independent of executable-building concerns.

Libraries use ordinary term abstractions and package types. Transparent
definitions travel through products and manifest package signatures; abstract
types travel through existential packages and package-dependent arrows. The
language therefore needs no module, namespace, visibility, or qualified-name
sort to compose the current whole-program sources. Separate compilation and
external package discovery remain future work and should elaborate to the same
term-level interfaces.

The same representation supports nested context-forming terms. A
`begin ... end` term collects `param`, `let`, and `def` forms connected by
`that` up to the nearest block boundary. Name resolution installs all of their
pattern binders before resolving the block, records dependencies from right-hand
sides and pattern annotations, and retains the resulting condensation DAG in
`ScopedArena`. Dependencies also propagate through nested blocks to an active
binding in the enclosing block.

The scoped block also carries a dependency-ordered elaboration for the existing
static judgments. Acyclic parameters become `Abs` terms, and acyclic transparent
or nominal definitions become `Let` terms; nominal right-hand sides retain a
`Sealed` marker. Recursive components remain explicit `RecGroup` terms so the
checker can introduce all type identities before checking their equations.
`in` forms elaborate directly to the corresponding lexical `Abs` or `Let`.
This division keeps dependency analysis in the scoped language while reusing
the established CBPV rules for type functions, polymorphic computations, value
functions, and local definitions.

The body sort also determines the classifier synthesized for an abstraction.
A type-pattern abstraction with a value body has a pure universal type; a value-pattern abstraction with a value
body has an ordinary pure arrow, or a pure package-dependent arrow when the boundary pattern opens existential
witnesses used by the result. The corresponding type arguments and package witnesses are retained by statics and
erased before evaluation. Consequently, `param`, `let`, `def`, and `begin ... end` can assemble a type or value
package directly whenever their residual term is pure. Computation-producing packages continue to use the CBPV
forms required by their effects.

The standard-library components and their aggregate package use this pure boundary. Importing `bool.zy`,
`option.zy`, `list.zy`, or `std.zy` yields a value-level package function; clients apply it and open its result with
`let`. The operations exported inside those packages retain their computation types.

## Relative Monads and Monadic Blocks

Relative monads are defined as codata in the standard library (see
`lib/std/monad.zy`). The module is a pure package-dependent function from Builtin to the `Monad` and `Algebra`
type package, so importing and opening it requires neither a thunk nor a returned computation. Zydeco also
implements *monadic blocks*, a
generalized do-notation. A monadic block is translated during type checking
via the algebra translation implemented in
`lang/statics/src/elaborate/monadic/mod.rs` and invoked from
`lang/statics/src/check/mod.rs`.

Each monadic block resolves `Monad` and `Algebra` as ordinary types at its lexical site.
The checker verifies their expected higher kinds and records the selected constructors in the block's
translation environment. Global types and terms used by the block are then reinterpreted under this
lexically selected monad during translation. The block's preliminary typing environment retains lexical
type bindings, including existential witnesses and transparent aliases, while term bindings still require
the global, inlinable status needed by algebra translation.

## Implementation Architecture

Zydeco is implemented as a pipeline with an interpreter and native-code branch:

1. parsing (`lang/surface/src/textual`)
2. desugaring (`lang/surface/src/bitter`)
3. name resolution (`lang/surface/src/scoped`)
4. type checking and post-check validation (`lang/statics/src`)
5. linking and evaluation (`lang/dynamics/src`), or
6. Stack IR and substitution normalization (`lang/stackir/src`)
7. assembly lowering (`lang/assembly/src`)
8. AMD64 or LLVM emission (`lang/amd64/src`, `lang/llvm/src`)

The phases are spread across several core crates:

- `zydeco-surface` (surface syntax, parsing, desugaring, name resolution)
- `zydeco-statics` (typed representation, static semantics, normalization, and type-directed elaboration)
- `zydeco-dynamics` (operational semantics and interpreter)
- `zydeco-stackir` and `zydeco-assembly` (lowered, stack-oriented IRs)
- `zydeco-amd64` and `zydeco-llvm` (native-code backends)

Within `zydeco-statics`, `syntax`, `environment`, and `arena` define the durable typed representation.
`check` owns local kinding and typing rules, `normalize` owns substitution and definitional normalization,
`elaborate` owns type-directed source translations, and `validate` owns post-check whole-program properties.
Its coverage pass checks data matches with a typed pattern matrix. Generalized comatch clauses are first elaborated
type-directly into shared argument matches and unique codata arms, after which the same pass checks argument coverage
and missing destructors along every observation path. The
[exhaustiveness design note](docs/ideas/exhaustiveness.md) explains matrix specialization, copattern elaboration,
counterexample construction, and the invariants supplied by typed syntax. This separation lets validation consume
completed typed syntax without becoming more type-checking branches.

### Arena and ID invariants

Compiler-owned IDs contain an opaque `KeySpaceId` and a `la-arena` raw index.
Externally issued IDs come from a non-cloneable `IdAllocator<Scope>`. Creating
an allocator claims a process-unique identity once; subsequent allocation only
mutates its local cursor. Independent allocators can therefore run in parallel,
and their identity tags keep merged IDs distinct.

Two separate type-level relations constrain IDs:

- `Scope: Allocates<Id>` declares which ID categories an allocator may issue.
  The scope belongs to the operation or pipeline lifetime that creates nodes;
  it is not stored on the ID and does not prevent independent allocators with
  the same scope.
- `Scope: ArenaSchema<Id, Item = T>` declares the contents owned by an arena
  representation. Since `Id` is a trait parameter, one scope can own several
  ID categories and the same ID can inhabit several representation scopes.
  This is used, for example, by the stack-passing and substitution-normal forms
  of Stack IR.

- Dense storage wraps `la_arena::Arena`. The `la-arena` allocation itself
  supplies the raw index, so a dense arena retains only its identity tag and
  rejects IDs from another dense arena even when raw indices happen to match.
  Dense-only IDs have no external `Allocates` implementation.
- Sparse storage is used where passes merge fragments or rewrite nodes while
  retaining IDs. It is storage-only: the phase-level owner issues an ID and
  inserts it explicitly. Both dense and sparse owning stores are constrained by
  `ArenaSchema`.
- Associative side tables are deliberately not constrained by `ArenaSchema`:
  annotations, provenance, environments, caches, and relations legitimately
  associate one ID with many property types. They require callers to choose
  explicit `insert_new`, `replace_existing`, `upsert`, or set-like `ensure`
  semantics.
- Issuers live on the operation that creates nodes: `Parser`, `Desugarer`,
  `Tycker`, assembly `Lowerer`, and stack analysis. Their output arenas do not
  retain the cursor. Stack IR deliberately keeps one issuer in `AdminArena`
  because normalization, substitution, CPS, and closure-conversion passes all
  continue creating nodes in the same IR.
- Provenance tables encode their actual cardinality. In particular, repeated
  type checking and transparent syntax make surface-to-typed provenance
  many-to-many, while one typed node can lower to many stack-IR nodes.
- Parsed entities use a tagged `EntityId` enum, so definitions, patterns,
  copatterns, and terms cannot be confused through raw-ID casts.

## Repository Layout

- `lang/`: language implementation and tests.
- `lib/`: Zydeco standard library, examples, and projects under `lib/tests`.
- `cli/`: command-line interface for running and checking programs.
- `docs/`: literate Zydeco tutorial material (see `docs/spell`).
- `editor/`: editor integrations (TextMate grammar and VSCode extension).
- `web/`: web interface.

## Current Limitations

The artifact documents a few important limitations:

- The LLVM emitter is experimental; the tested native backend targets AMD64.
- Imports currently address relative or absolute source paths; there is no
  external package resolver or lock file.
- Absolute imports are location-dependent and receive no portability warning.
- Monadic blocks pass monad instances at runtime; inlining is not implemented,
  and only global definitions can be referenced inside blocks.
