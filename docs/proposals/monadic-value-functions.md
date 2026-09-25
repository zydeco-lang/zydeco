# Polymorphic value functions in monadic blocks

Status: design draft, 2026-09-25. The translation below is proposed, not implemented or adopted. It follows the approved
[closed translation boundary](../references/language.md#closed-translation-boundary) and addresses value functions
inside computation payloads.
[Translating a value or type as the annotation's root remains deferred](../references/language.md#translation-coverage-and-deferred-roots).
The separate [nominal-type draft](monadic-nominal-types.md) owns local seals and recursive definitions.

## Motivation and current failure

Value functions assemble libraries and specialize code without executing their suspended computations.
A polymorphic value function can nevertheless build a thunk that sequences into an abstract computation type.
Algebra translation needs the structure for that type while translating the thunk body,
even though the value function itself will later disappear through static elimination.

This complete source passes ordinary checking when the annotation is removed,
but currently fails with `tyck.missing-structure`:

```zydeco
param (/CType; /Thk; /Ret; /Int; builtin) : @(import("../../lib/std/builtin.zy")) in
let basis = @(import("../../lib/std/control/monad.zy")) in
let (/Monad; /Algebra) = builtin |> basis in
@[monadic] begin
  let val make (B : CType) = {
    fn (k : Thk B) =>
      do _ <- ret ();
      ! k
  } in
  ! (make (Ret Int)) { ret 1 }
end
```

The [current translator](../../lang/statics/src/elaborate/monadic/mod.rs) introduces a structure parameter
for computation `TAbs` and supplies one at `TApp`.
Its `ValPi`, `ValAbs`, and `ValApp` cases rebuild type parameters and arguments without that evidence.
The missing case is therefore a local polymorphic binder, not a reason to capture an algebra from outside the block.

## Proposed translation

Extend the existing structure-passing convention to type-polymorphic value functions.
Write `L_M` for translation under carrier `M`, and `Sig_M,K(T)` for the computation type
of the structure required by a target type `T : K`.
These names are notation for this draft, not proposed source operators.
The existing signature construction supplies:

```text
Sig_M,VType(T)       = Top
Sig_M,CType(T)       = Algebra M T
Sig_M,K1 -> K2(F)    = forall (X : K1) .
                        Thk (Sig_M,K1(X)) -> Sig_M,K2(F X)
```

Named kinds use their existing payload projection rule.
Let `Str_M(T)` denote the computation constructing structure evidence of type `Sig_M,K(L_M(T))`
from a source type `T : K` and the current translated binder environment.
This distinction prevents applying type translation twice to a structure signature.

For a type parameter, introduce one additional value-function parameter immediately after it:

```text
L_M(val pi (X : K) . A)
  = val pi (X' : K) .
      val pi (s_X : Thk (Sig_M,K(X'))) . L_M(A)

L_M(val (X : K) => v)
  = val (X' : K) =>
      val (s_X : Thk (Sig_M,K(X'))) => L_M(v)

L_M(f T)
  = (L_M(f) L_M(T)) { Str_M(T) }
```

The application rule applies when the checked function expects a type argument.
Translate the body under both the renamed witness `X -> X'` and its structure association `X' -> s_X`.
Record the association at the same binder boundary that introduces `s_X`; do not use a compiler-global lookup.
Keep the source type-pattern shape, including named patterns, when rebinding its payload witness.
The extra structure parameter follows that complete source parameter.

For an ordinary value parameter, retain a single translated value parameter and translate its argument once.
Its domain, codomain, and pattern must share one witness-renaming operation.
When the parameter opens a packed value, use the structures already carried by the translated package;
do not introduce a second copy merely because the opening occurs in a value function.
Recompute dependent witness routes against the translated package shape, including the inserted structure fields.

Use the uniform convention at every type kind, including the trivial structure at `VType`.
This matches computation polymorphism and gives a predictable rule for higher-order and partially applied functions.
Removing provably unused trivial arguments is a later simplification, not an alternative translation interface.

## Worked interpretation and phase behavior

For the example, the translated factory has the following schematic classifier:

```text
val pi (B : CType) .
  val pi (alg_B : Thk (Algebra M B)) .
    Thk (Thk B -> B)
```

Its returned thunk sequences using `alg_B`.
At the source application `make (Ret Int)`, translation passes `M Int` and the structure generated
from the source type `Ret Int`, which uses the selected monad's bind.
It then applies the resulting thunk to the translated `{ ret 1 }`.
The caller supplies no extra source annotation; the checker has already identified the type application.

The phase order remains: check the closed payload once, translate its checked terms and classifiers,
then apply [required static elimination](../references/compiler.md#static-elimination).
Structure arguments are thunk values. A value function may forward one into the thunk it builds
without forcing it or using it to discover a static type witness.
The monad implementation may remain a runtime parameter; the static evaluator only needs
to know the value-function closure being applied.

Generated `ValAbs`, `ValApp`, and `ValPi` forms obey the ordinary elimination boundary.
Value functions may remain in an allowed static library result,
but surviving runtime values and computations must have representable classifiers
and bodies under [L10](../references/language.md#10-static-elimination).
This draft creates no runtime interpreter for value functions and permits no computation execution
during static specialization.
Unknown runtime choices still cannot determine static functions or type witnesses.

### Why not eliminate value functions before translation?

Early specialization can make the motivating application work by substituting `Ret Int` before its body is lifted.
It does not itself specify how to translate an unapplied polymorphic helper,
a partial application, or a higher-order value-function classifier.
Performing whole-payload residualization first can also expose representation checks at the wrong phase,
before the translated interface and its structures exist.

The proposed structure-passing rules give these forms a compositional translation
and leave the existing static evaluator responsible for reduction.
A narrower early-reduction optimization could be considered later, but would need evidence
that it preserves classifier queries, witness scope, source provenance, and nominal identity.
It is not the acceptance mechanism proposed here.

## Implementation outline

1. Extend `ValPiBinder::Type` translation to build the translated type binder followed by a structure-value binder.
   Preserve the source kind and named pattern shape; translate the codomain under the renamed witness.
2. Extend `ValBinder::Type` abstraction translation to introduce the matching value parameter
   and register its structure before translating the body.
   Use a child scope so the association cannot leak to siblings.
3. Extend `ValArgument::Type` application translation to supply the translated type and a thunk of its structure.
   Use the checked source classifier to distinguish a type application from an ordinary value application.
4. Share binder and package-opening logic across type and term reconstruction.
   Verify higher-kinded and named parameters, ordered telescopes, witness-dependent value parameters,
   and projections from translated packages.
   Rebuild witness routes instead of copying stale indices.
5. Update inferred classifiers, annotated helper signatures, and every affected application in the same change.
   Replace the previous type-parameter cases rather than retaining a second calling convention.
   Run normal finalization, static elimination, and typed-arena validation on the result.

The extension consumes the existing immutable `CheckedTerm`; it must not recheck the source body
under a different expectation to obtain evidence.
Keep type renaming and structure lookup as distinct domain maps.
Structure values capture the currently selected monad implementation through ordinary lexical binding;
there is no outside-algebra registration path.

Follow [traversal selection](../references/compiler.md#choosing-and-composing-traversals)
and [resumable execution](../references/compiler.md#resumable-folder-execution).
Reuse classifier reconstruction for ordinary children and explicit scope transitions for added binders.
A substantial rewrite of the translation should use an `Explicit` folder
whose owned frames retain the pending classifier or term construction.
The residual runtime visitor is unsuitable because it omits static functions and their evidence.
Any shared checked-term child view belongs beside that representation, with independent validation kept separate
from reconstruction.

## Validation and acceptance

Retain the motivating source and its ordinary counterpart in the existing fixture or Rust harness.
Translate the complete program rather than testing only a helper that constructs a structure argument.

| Accepted case | Rejected counterpart or required invariant |
| --- | --- |
| The local `CType` factory above, under `Ret` and a nontrivial monad | A mismatched translated structure fails type checking; no unchecked relabeling is used |
| A `VType` factory returning an identity thunk | Its trivial argument follows the same convention and erases when unused |
| A higher-kinded parameter used to build suspended code | The supplied constructor structure has the correct kind-directed signature and argument order |
| Named type parameters and multiple alternating type/value parameters | Mismatched labels, argument sorts, and order retain ordinary diagnostics |
| Known partial applications and higher-order value-function composition | Unknown residual value functions still fail with `tyck.static-elimination` |
| Value parameters opening abstract and manifest package witnesses | Invalid witness routes or witness escape retain ordinary checking failures |
| A static helper forwarding runtime data and an algebra thunk | Static evaluation never forces that thunk to compute a witness or select a value function |
| Sibling helpers and nested monadic blocks | Structure associations cannot leak across their binder or translation scopes |
| External inputs passed through binders inside the annotation | Free outer values and type witnesses follow the approved closure rejection |

Inspect translated classifiers as well as execution results:
each translated type binder has exactly one structure argument, and each type application supplies it exactly once.
Use a custom monad whose return/bind operations record a visible order to detect eager evidence forcing,
duplicated computations, or effects performed during checking.
Preserve the ordinary source failure cases for static choices depending on runtime data.

Run focused checks and interpreter execution, then verify that surviving translated thunks reach residual lowering
without `ValAbs`, `ValApp`, or `ValPi` in runtime positions.
Exercise representative generated closures with the existing backend smoke paths;
the proposal adds no backend runtime construct.
Rejections must preserve source diagnostics and withhold a successful executable root.
Deep nested binders should validate explicit reconstruction without native-stack growth.

## Questions to settle before adoption

- Confirm the uniform structure argument for all kinds, especially the observable generated helper signature at `VType`,
  against existing higher-order value-function examples.
- Specify a shared binder result that keeps translated classifier witnesses, term patterns, structure associations,
  and package projection routes synchronized without coupling independent analyzers.
- Demonstrate static elimination of a known higher-order factory that forwards an opaque runtime algebra thunk.
  A successful monomorphic example alone does not establish the general phase contract.
- Coordinate with the [nominal-type draft](monadic-nominal-types.md) on factories defining local seals:
  specialization and structure insertion must preserve that draft's identity and ownership rules.
  Each feature must also have independent tests that do not require the other extension.

After implementation and approval of the settled choices, put the value-function translation contract
in [L11](../references/language.md#11-relative-monads) and its elaboration invariants
in [C6](../references/compiler.md#monadic-and-copattern-elaboration).
