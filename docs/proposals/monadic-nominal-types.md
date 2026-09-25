# Nominal and recursive types in monadic blocks

Status: implementation draft, 2026-09-25.
The user approved applicative translation; construction is in progress.
The approved scope is the [closed translation boundary](../references/language.md#closed-translation-boundary);
[non-computation roots remain deferred](../references/language.md#translation-coverage-and-deferred-roots).
[Polymorphic value functions](../references/language.md#polymorphic-value-functions) already use structure passing.

## Motivation and current failure

A closed monadic program should be able to define the data and stack protocols it uses.
Today a transparent, nonrecursive data definition can be translated,
but using a nominal `def` fails with `tyck.not-inlinable-seal`.
Recursive type definitions require nominal identities
under [the binding rules](../references/language.md#3-bindings-and-scope), so replacing `def`
with `let` cannot recover recursive datatypes.

The following complete source is an acceptance target.
Its type parameter is inside the annotation, and its recursive constructor is defined inside the payload:

```zydeco
param (/VType; /Unit; builtin) : @(import("../../lib/std/builtin.zy")) in
let basis = @(import("../../lib/std/control/monad.zy")) in
let (/Monad; /Algebra) = builtin |> basis in
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

At the reviewed implementation, `type_translation` rejects any `Type::Abst` present in the seal table.
That test does not distinguish a definition owned by the payload from an external nominal type.
The relevant paths are [type and structure translation](../../lang/statics/src/elaborate/monadic/mod.rs),
[seal construction](../../lang/statics/src/check/binding.rs),
and [the checked payload handoff](../../lang/statics/src/check/monadic.rs).

## Applicative translation

The approved identity rule lives in [L11](../references/language.md#applicative-nominal-translation).
Implement it with a nominal family for each source declaration, parameterized
by the carrier and any free type dependencies.
Transparent aliases retain their existing behavior.

Write `L_M(T)` for type translation and `N` for a local nominal definition with body `T`.
The proposed equations describe a graph construction rather than unrestricted unfolding:

```text
L_M(N)          = N_alg M L_M(D1) ... L_M(Dn)
body(N_alg)     = fn M D1 ... Dn => L_M(T)
kind(N_alg)     = (VType -> CType) -> kind(D1) -> ... -> kind(Dn) -> kind(N)
L_M(N U)        = (L_M(N)) L_M(U)
```

Allocate `N_alg` before translating `T`; `D1 ... Dn` are its free type dependencies.
A recursive reference therefore returns the allocated identity instead of recursively translating the body again.
For a mutually recursive component, allocate every target identity before translating any member body.
Parameterized recursive constructors remain constructors; translation does not specialize a new declaration
for every encountered type application.

The initial implementation target is local data and codata definitions,
including parameterized and mutually recursive groups.
A sealed definition with another head needs an explicit account of its introduction, elimination,
and algebra construction before inclusion; simply treating all seals as transparent is insufficient.
This draft does not add a positivity or guardedness rule to source typing.
The [recursive-admissibility question](../ideas/recursive-admissibility.md) remains separate.

## Ownership, scope, and sharing

The translator needs to know which nominal declarations belong to the closed payload.
Membership in the checker-wide seal table establishes that a body exists, not that translation owns it.
Retain declaration ownership and lexical scope with the checked payload or in a checked side table;
include declarations introduced by an admitted closed source expansion under the existing copy-identity policy.
An external type supplied through a payload type parameter stays abstract, even if the compiler knows its seal body.

A family's explicit parameters close its definition over the carrier and free type dependencies.
A local type inside `fn (A : VType) => ...` supplies the translated `A` when applying the family;
the family's body binds its own parameter instead of capturing that source witness.
This permits caching the closed family by source declaration identity.
Nested translation composes families, and genuinely copied source declarations retain their copy identities.
Keep declaration identity separate from ordinary type application:
neither `List (List A)` nor a different runtime monad dictionary creates another translated declaration.

Use domain states such as `Reserved`, `Defining`, and `Complete` for target declarations.
A recursive back-edge may reference a reserved identity; operations requiring its body must wait for completion.
Publish a successful translated root only when its reachable target definitions are complete.
On failure, partial facts may remain for diagnostics under the existing unpublished-arena discipline,
but no incomplete translation may reach residualization or execution.

## Constructing structures for nominal types

Type reconstruction alone does not implement `do` with a nominal computation result.
For each translated type, structure translation must construct evidence with the same signature already used
by the algebra translation: value types have the trivial structure, computation types have `Algebra M B`,
and type constructors have a polymorphic operation mapping argument structures to result structures.

For a nominal value type, the structure is trivial even when its data graph is recursive.
For a nominal codata type, construct the algebra by the codata's observations,
using the translated destructor signatures and the structures for their result types.
Keep the nominal target as the classifier of the generated computation; expose its body only
through the same shape-directed operations that ordinary checking admits.
The existing `CoDataStructureTranslation` is a starting point, but its structural `comatch` builder must be extended
to retain a nominal expected classifier.
A structural result cannot be relabeled without checking.

Recursive computation protocols can make their structures mutually recursive too.
Reserve structure references before building their bodies, independently of the type-definition map.
A candidate encoding uses existing computation `fix` and a private codata dictionary
with one destructor per required structure.
Each destructor returns the corresponding structure computation; recursive requests refer to the shared dictionary.
Wrap references in thunks when the structure interface requires a value.
This avoids introducing cyclic Rust-owned terms or a new runtime recursive-value construct.

Generate that dictionary at the lexical scope containing its monad implementation and translated type parameters.
Higher-kinded structures keep their type and structure parameters, rather than recursively specializing at each use.
Forcing a structure must occur only where the translated computation demands it;
reserving or passing its thunk must neither run source computations nor force recursive evidence eagerly.
Finite graph construction establishes compiler termination for the reconstruction step;
it does not establish productivity of every recursive source protocol or laws for an arbitrary monad implementation.

## Implementation outline

1. Retain local declaration ownership and dependency facts when checking the payload.
   Collect dependencies over the checked classifier graph, including seal bodies only when owned by the payload.
   Keep this analysis separate from target reconstruction and structure generation.
2. Schedule recursive components and reserve their target identities in the translated lexical scope.
   Rebuild bodies, binder substitutions, data/codata arms, and provenance using the reserved map.
3. Extend nominal introductions, eliminations, patterns, and structure construction together.
   Constructors, destructors, coverage hints, and expected classifiers must all refer to the same translated definition.
4. Materialize demanded recursive structures and validate their signatures against the translated nominal types.
   Preserve the checked source root and its original seal graph for diagnostics and editor facts.
5. Finalize, run typed-arena validation, then use the existing static elimination and backend pipeline.
   Remove the unconditional rejection for supported local definitions in the same change as their consumers.

Follow [traversal selection](../references/compiler.md#choosing-and-composing-traversals)
and [resumable execution](../references/compiler.md#resumable-folder-execution).
The raw classifier folder supplies ordinary child reconstruction; seal edges,
ownership, and scope remain explicit semantic operations.
The residual runtime visitor excludes the classifier evidence needed here.
If owned declaration facts require a new checked-graph view, put its child enumeration beside the representation
and reuse it across independent analyzers.
Use `Explicit` frames for recursive reconstruction and structure work; do not add another unbounded recursive walker
or use mutable global tables to imply binder scope.

## Validation and acceptance

Retain complete source reproducers in the existing fixture or Rust harness when implementing this draft.
The following pairs define the first acceptance boundary:

| Accepted case | Rejected counterpart or required invariant |
| --- | --- |
| Local nominal data and its constructors/matches | Distinct local seals with identical bodies still reject cross-use with the ordinary nominal type mismatch |
| Recursive `List` and mutually recursive data | Existing illegal transparent or value dependency cycles retain their source diagnostics |
| Local codata with `do` before observation | An unknown or mismatched destructor retains the ordinary checking error |
| Recursive codata and mutually dependent structures | Construction terminates; unused structure thunks perform no effects |
| A higher-kinded recursive constructor used at several arguments | Target declaration counts follow source declarations and lexical instances, not the number of unfolding paths |
| Local declarations depending on translated type parameters | A target definition cannot contain an out-of-scope original witness |
| Abstract external types supplied through internal type parameters | Direct external witness capture follows the closure rejection; known seals are not opened through an abstract parameter |
| Packages carrying translated nominal witnesses and operations | Distinct witnesses and forbidden existential escape retain their identity/scope failures |

Run representative terms under both `Ret` and an existing nontrivial relative monad,
checking observations and effect order.
Use an observable recursive codata whose next method returns a further protocol through a thunk;
observe a finite prefix to detect eager structure forcing.
Check interpreter execution and focused residual lowering, then the existing backend smoke paths affected
by generated `fix` and codata; no new backend ABI is intended.
Deep recursive graphs and shared tails must exercise the explicit execution path and bounded declaration counts.
Rejected inputs must return diagnostics without panics or a published executable root.

## Questions to settle before adoption

- Decide the retained ownership representation and how it composes with checked-source copying and nested translation.
  Demonstrate stable identity for a reused canonical root and freshness for genuinely copied local declarations.
- Work through a mutually recursive codata example to establish
  that the proposed structure dictionary inhabits the nominal signatures using ordinary typed operations,
  including higher-kinded members.
- Specify the supported treatment of sealed aliases whose heads are arrows, `Ret`, or an abstract constructor.
  Preserve source nominal distinctions; do not add a general coercion or erase seals to make an algebra typecheck.
- Determine which existing classifier operations must wait for completed recursive definitions,
  and make failures before completion explicit.
  Do not claim broader recursive admissibility from memoization alone.

After implementation and approval of the settled choices, move the semantic contract
to [L11](../references/language.md#11-relative-monads) and the construction invariants
to [C6](../references/compiler.md#monadic-and-copattern-elaboration).
