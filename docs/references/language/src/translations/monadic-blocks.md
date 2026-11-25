# Monadic Blocks

Surface translations use meta annotations to give their payload a reading other than the core's own
and elaborate it into ordinary core syntax, which is then checked, linked, and run like any other term.
`@[monadic] e` performs sort-preserving algebra translation under a lexically visible relative-monad basis;
[`@[cbv] e`](call-by-value-and-call-by-name.md#call-by-value-blocks) elaborates a call-by-value expression,
and [`@[cbn] e`](call-by-value-and-call-by-name.md#call-by-name-blocks) is reserved for call-by-name.
Because each translation produces core syntax, they compose: a call-by-value block nested
in a monadic block is translated by both.

## Outer Interface and Basis

`@[monadic] e` selects the lexically visible `Monad` and `Algebra` constructors defined
by the [relative-monad contract](../../../library/src/effects/relative-monads.md#relative-monads)
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
Value operands include thunks, value functions, and ordinary packed values used as modules;
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

## Closed Translation Boundary

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
[closure validation](../../../../todos/compiler-boundaries.md#monadic-closure-validation) tracks this discrepancy
with the approved boundary.
It is not a planned mechanism for importing external structure evidence.

## Polymorphic Value Functions

A translated value function may assemble suspended code that sequences into an abstract computation type.
Translation supplies that type's algebra at the value-function binder,
using the same structure-passing convention as computation polymorphism.
The value function still disappears through [static elimination](../static-elimination.md).

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
parameters that open packed values use the structures those values already carry.
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
An [inference discrepancy](../../../../todos/compiler-boundaries.md#inferred-higher-order-value-function-witnesses)
currently requires an explicit signature for some higher-order factories even without monadic translation.

## Applicative Nominal Translation

A closed monadic payload can define its own nominal data and codata types,
including parameterized and mutually recursive definitions.
Translation preserves their constructors, patterns, and observations through a nominal type family
for each source declaration.
Transparent aliases retain their existing behavior.

The family takes the carrier, the selected `Algebra` type constructor,
and the declaration's free type dependencies as explicit type arguments.
The `Algebra` parameter accounts for structure fields introduced into existential values
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
The [compiler reference](../../../compiler/src/elaboration-and-validation.md#nominal-algebra-construction)
describes ownership and recursive construction.

## Translation Coverage

Type, value, and computation roots use the same structural translation rules.
Pure modules are translated according to their ordinary type or value sort, with no separate module mechanism.
Type roots construct applicative nominal families without generating runtime algebra dictionaries.
Value and computation roots bind the shared lazy dictionary inside their carrier and monad parameters.
Polymorphic value functions and local nominal data/codata follow the rules above.
Supported nominal definitions have a data or codata body after their type parameters.
Other sealed heads, including nominal aliases of arrows, `Ret`, or abstract constructors,
remain unsupported: they need their own typed introduction and algebra construction rules.

Library-encoded delimited control does not imply primitive capture or duplication of the native machine stack.
Standard library relative monads, effect instances, and derived transformers are specified
in the [standard library reference](../../../library/src/effects/relative-monads.md).
