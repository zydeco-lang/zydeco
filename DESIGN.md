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

Names are an orthogonal wrapper rather than a separate record calculus.
`Named(field, A)` labels a type, while `Named(field, term)` and
`Named(field, pattern)` introduce or eliminate a payload with the same label.
Surface syntax uses `field = inner` for all three forms. For example, if
`value : A`, then `(field = value) : (field = A)`, and a pattern
`(field = pattern)` checks its payload against `A`.

Named product types use the existing product operator:

```zydeco
(x = A) * (y = B)
```

Their term and pattern forms use the existing comma tuple syntax:

```zydeco
(x = a, y = b)
(x = p, y = q)
```

In particular, `(x = A, y = B)` is not alternate product-type syntax. It is
parsed as the same comma tuple used in every other term position, and its sort
and validity are left to type checking. Only `*` forms a product type. This
keeps `Named` modular: it labels one type, term, or pattern, while `Prod` and
`ConsN` continue to supply product formation and tuple introduction or
elimination.

For the MVP, named types are value types. A named term checks against a named
type only when its field label agrees and its payload checks against the
underlying value type; named patterns follow the same rule. Computations can be
stored in named products through their thunks. Product order and explicit
grouping remain significant, and named and unnamed components may be mixed.
Labels affect static equality and checking, but not runtime layout, so they may
be erased after type checking.

The parser preserves `field = ...` as `Named` syntax and continues to defer its
sort to type checking. Named projection uses postfix slash syntax:
`term/field`. Selection associates to the left, making
`term/outer/inner` a path through nested named terms. Its receiver undergoes
ordinary lexical or global name resolution, while field labels are checked
statically rather than resolved as variables. Slash is reserved exclusively
for named projection; dot remains exclusively the elimination syntax for
computation destructors, preserving the value/computation distinction. Slash
binds tighter than application, so `f value/field` means
`f (value/field)`.

Named projection accepts a directly named value or searches only the immediate
product spine. It requires exactly one matching field and exposes the payload
beneath `Named`; missing and duplicate matches are distinct type errors.
Reusing slash is intentional: named terms internalize namespace-like name
management in the expression language instead of introducing a parallel
projection mechanism for each sort or abstraction level.

Named structure does not enter StackIR. Type checking resolves each projection
either to the payload of a directly named value or to a physical product
position. Lowering erases the former as an identity and the latter as an
ordinary full-arity tuple pattern and `let`; subsequent backends therefore see
only the existing tuple representation and layout.

### Source Organization and Modules

Zydeco temporarily has no source-language module, namespace, import, layer, or
qualified-name machinery. Files that previously used module blocks are
flattened into one global declaration scope, with comments retaining any useful
organizational boundaries. Name resolution therefore maps ordinary local and
global `VarName`s directly to their definitions. Filesystem paths in project
dependency configuration remain a driver concern and do not introduce paths
into the language. A future module system will be designed afresh rather than
constrained by the removed scaffolding.

## Relative Monads and Monadic Blocks

Relative monads are defined as codata in the standard library (see
`lib/oopsla/core.zydeco`). Zydeco also implements *monadic blocks*, a
generalized do-notation. A monadic block is translated during type checking
via the algebra translation implemented in `lang/statics/src/monadic.rs` and
invoked in `lang/statics/src/tyck.rs`.

Monadic blocks are designed to be closed in the paper, but the artifact allows
use of global types and terms. These global definitions are reinterpreted under
the block's ambient monad during translation.

## Implementation Architecture

Zydeco is implemented as a pipeline with an interpreter and native-code branch:

1. parsing (`lang/surface/src/textual`)
2. desugaring (`lang/surface/src/bitter`)
3. name resolution (`lang/surface/src/scoped`)
4. type checking (`lang/statics/src`)
5. linking and evaluation (`lang/dynamics/src`), or
6. Stack IR and substitution normalization (`lang/stackir/src`)
7. assembly lowering (`lang/assembly/src`)
8. AMD64 or LLVM emission (`lang/amd64/src`, `lang/llvm/src`)

The phases are spread across several core crates:

- `zydeco-surface` (surface syntax, parsing, desugaring, name resolution)
- `zydeco-statics` (static semantics and algebra translation)
- `zydeco-dynamics` (operational semantics and interpreter)
- `zydeco-stackir` and `zydeco-assembly` (lowered, stack-oriented IRs)
- `zydeco-amd64` and `zydeco-llvm` (native-code backends)

Common patterns in each phase include `syntax`, `arena`, `err`, `fmt`, and
`span` modules.

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
  copatterns, terms, and declarations cannot be confused through raw-ID casts.

## Repository Layout

- `lang/`: language implementation and tests.
- `lib/`: Zydeco standard library and examples (including `lib/oopsla`).
- `cli/`: command-line interface for running and checking programs.
- `docs/`: literate Zydeco tutorial material (see `docs/spell`).
- `editor/`: editor integrations (TextMate grammar and VSCode extension).
- `web/`: web interface.

## Current Limitations

The artifact documents a few important limitations:

- The LLVM emitter is experimental; the tested native backend targets AMD64.
- The package manager supports only local dependencies.
- `pub` is parsed, but declaration visibility is not enforced.
- Debug builds use a larger stack to avoid overflow on large tests.
- Monadic blocks pass monad instances at runtime; inlining is not implemented,
  and only global definitions can be referenced inside blocks.
