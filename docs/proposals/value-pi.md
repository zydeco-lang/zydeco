# Value Functions with `ValPi`

## Abstract

This proposal adds a single dependent classifier for total value transformations:

```zydeco
val pi (A : VType) (value : A) . A
```

Its direct introduction form is `val`, its block-form introduction is `param val`,
its ordinary binding sugar is `let val`, and its elimination forms are application
and the pipeline operators `|>` and `<|`.

```zydeco
let val id (A : VType) (value : A) : A = value that
let answer = 42 |> id Int64 in
...
```

`ValPi` internalises a derivation of the CBPV value judgment as a definitional function:
one that is bound, applied, and unfolded, but never stored or passed as a runtime value.
It is distinct from the existing computation arrow: `A -> C` classifies a computation accepting `A`,
while `val pi (x : A) . B` classifies a total, effect-free transformation producing a value.
This distinction makes value-level cut available both dynamically and inside patterns
without introducing a separate class of named views.

## One Classifier

Earlier implementations divided value functions into non-dependent arrows,
polymorphic functions, and package-dependent functions.
Those are three instances of one telescope:

```text
ValPiBinder ::= TypeBinder | ValueBinder(domain, package-witnesses?)
ValPi       ::= val pi ValPiBinder . Type
```

A type binder replaces value-level universal quantification:

```zydeco
val pi (A : VType) . A
```

A value binder without package witnesses is the ordinary value function space:

```zydeco
val pi (_ : A) . B
```

A value binder over an existential package may disclose its static witnesses to the codomain:

```zydeco
let Box = exists (X : VType) . X that
let val unpack ((X, value) : Box) : X = value that
```

The inferred classifier of `unpack` is `val pi ((X, _) : Box) . X`.
The dependency is static: a codomain may depend on type arguments and on witnesses disclosed
by a package pattern, but not on an arbitrary runtime value.
`ValPi` therefore remains a value type even though its telescope contains both erased and runtime binders.

The checked value binder retains two pieces of static evidence: the canonical witness telescope
and a structural projection route through the parameter pattern.
The route matters for composite domains.
In a parameter such as `(builtin, (X, value))`, `builtin` may carry its own existential witnesses,
but only the witnesses opened by `(X, value)` instantiate the codomain.
Application follows the stored route instead of flattening every witness in the argument.
Thus the dependency is determined by the binder, while the argument supplies the corresponding memory-level evidence.

## Introduction and Binding

The explicit abstraction form mirrors the classifier:

```zydeco
val (A : VType) (value : A) => value
```

Parameters are curried from left to right.
Type parameters erase during lowering; value parameters construct lexical closures.
Runtime parameter patterns must be irrefutable because applying a value function is total.

The corresponding block-form introduction is `param val`:

```zydeco
param val (value : A) in value
```

The lexical form above elaborates to `val (value : A) => value`.
With `that`, the parameter contributes to the nearest enclosing `begin` context before
that context is reconstructed as a value abstraction:

```zydeco
begin
  param val (value : A) that
  value
end
```

This is the preferred spelling when the value-function body is a `begin ... end` block:
the block remains the visible context boundary, and its value parameters participate
in the same dependency ordering as its definitions.
Direct `val P => V` remains the concise spelling for a non-block value body.

The explicit `val` modifier is semantically significant.
Plain `param` retains its existing role for type functions and computations;
it does not infer a value-function abstraction from a value body.

The declaration form is sugar for an ordinary non-recursive value binding:

```zydeco
let val id (A : VType) (value : A) : A = value that
```

expands to

```zydeco
let id : val pi (A : VType) (value : A) . A =
  val (A : VType) (value : A) => value
that
```

The result annotation names the residual codomain after all parameters.
Since value formation is total, `let val` does not admit `fix`; recursion continues to belong to computations.
An unannotated result may be synthesized when the ordinary value checker can infer it.

## Elimination and Pipelines

Application is one thing: juxtaposition, both pipelines, and the view pattern are spellings of the same value-level cut.

```text
V |> f  ==  f V  ==  f <| V          let f ~> p = V in N  ==  let p = (V |> f) in N
```

Evaluating the cut rewrites complex values no further than their memory representation: projections,
constructions, and trivial fills — never a closure, an indirect call, or an effect.
A value function is a derivation `x : A |-v W : B`; its cut is performed entirely at the value level,
and the spellings differ only in reading direction.
`|>` associates to the left and `<|` to the right, so

```zydeco
input |> first |> second
second <| first <| input
```

both apply `first` before `second`. An application consumes type arguments and value arguments in telescope order,
so either side may be a partial instantiation:

```zydeco
let val keep (A : VType) (value : A) : A = value that
let keep_unit : val pi (value : Unit) . Unit = keep Unit that
let recovered : Unit = () |> keep_unit that
```

No spelling can make the function itself a value that flows onward:
a pipeline side is a function standing at its own application, never a computed function stored
in data (see [Second-Class Occurrences](#second-class-occurrences)).

## Equational Theory

The beta law exposes the underlying complex-value binding:

```text
V |> (val P => W)  ==  let P = V in W
```

For a variable pattern, the right-hand side reduces by substitution.
Together with the ordinary equations for complex values,

```text
let x = V in x                         == V
let x = V in W                         == W       when x is not free in W
let y = (let x = V in W) in U          == let x = V in let y = W in U
```

this gives identity, dead-cut elimination, associativity, and pipeline fusion.
The equations are sound because value terms are total and effect-free and allocation identity is not observable
at the source level.

Eta is admissible at `ValPi`:

```text
val x => f x  ==  f                    when x is not free in f
```

Type abstraction and application satisfy the analogous beta and eta laws modulo erasure.
Package-witness binders substitute the disclosed static identities into the codomain
while passing the package representation at runtime.

## CBPV Boundary

The proposal adds a positive function space; it does not reinterpret the computation arrow.
A body containing `force`, computation application, effects, or general recursion cannot check
in the value judgment and therefore cannot inhabit `ValPi`.
Such behaviour retains the CBPV shape `A -> C`, commonly `A -> F B`, and must be thunked
when a computation function itself is stored as a value.

Value functions are second-class. A body may close over ambient values — the capture is resolved lexically
when the function is applied — but the function itself may not be selected from data,
passed to another value function, returned as a value, or stored in any payload.
The first-class function space of the value category is instead `Thk B`:
suspending a computation keeps higher-order programming on the computation side,
where effects and its runtime representation already live.
The [value-view proposal](value-views.md) keeps its patterns: a view applies a named value function to its subject
and then matches the nested pattern, so it consumes the function at its application rather than storing it.

## Second-Class Occurrences

The checker confines `val pi` to definition and application sites with one occurrence rule:
a `ValPi`-typed value may occur only as the right-hand side of its own binding (including partial type instantiation,
which leaves a residual function bound to a name) or as the head of an application;
a `val pi` classifier may occur only as the classifier of such a binding.
Every other position would materialize a function at runtime and is rejected:

```zydeco
let val keep (A : VType) (value : A) : A = value that
let functions = (keep Unit, ()) that        -- rejected: stored in a product
```

The rejected positions are, on the term side, product components, constructor payloads, package payloads,
named components, computation arguments, returned values, and match scrutinees; on the type side, product components,
computation-arrow domains, `ValPi` runtime domains (higher-order value functions), existential package bodies,
named payload types, `Ret` payloads, package-dependent arrow domains, and data or codata declaration payloads.
The rule is enforced after checking by validating every occurrence against its recorded classifier,
so a function reaches storage under any path — through a variable, an import, or instantiation of an abstract domain —
the occurrence is still found at the point where it would be stored.

Passing a function *through* a polymorphic value function remains legal, because the function is never stored:
`(A : VType)`-abstract code that returns its argument unchanged elaborates to lexical rebinding when applied.
Storage inside such code is still rejected at the storing position.

Existential packages obey the same discipline through a sibling rule — a package never crosses a computation boundary —
specified with its own positions and error code in [Package modularization](package-modularization.md);
the two checkers run together after ordinary checking.

## Runtime and Compilation

Value functions have no runtime representation.
Lowering treats the beta law as the definition of application: it resolves the applied head —
through elided definition bindings, aliases, erased type arguments, and residual instantiations —
to its abstraction, and lowers each cut as a lexical pattern binding at the application site.
A definition binding never materializes; its body lowers once per application,
and ambient capture resolves lexically where the body is spliced, so no environment is built.

The reference interpreter still evaluates abstractions as closures.
Extending an environment is extensionally equal to unfolding, because value application is total
and allocation identity is unobservable.
The compiled program contains no closure, environment tuple, or indirect call for a value function:
when an application does not statically resolve or an abstraction is reached outside its definition,
lowering reports a source error — a diagnostic naming the offending application
or abstraction at its span — instead of emitting a closure.
`function ~> pattern` applies its named function to the subject by the same unfolding before the nested pattern matches.

A source file that exports a package transformation is consequently an ordinary value term:

```zydeco
val (builtin : Builtin) =>
  pack (Api : VType) where ... end
```

Importing it binds a definition, which can be named and applied normally:

```zydeco
let make_std = @(import("std.zy")) that
let std = builtin |> make_std in
...
```

Inlining duplicates a body at each application, so emitted size and compiler recursion depth grow
with the unfolded program; the workspace test configuration raises its minimum stack accordingly.
Static resolution also lets a caller's demand flow through an application into the callee's body,
so unused components of an instantiated package become ordinary dead bindings;
the demand analysis records this alongside its binding decisions.
One tier remains future work: compiling each definition once as a block reached by a direct jump —
an unboxed closure whose call site the occurrence rule proves static.

## Implementation Boundary

The implementation should represent the telescope with typed binder and argument variants rather
than three parallel type constructors.
Formation, introduction, elimination, substitution, formatting,
and package-witness recovery then follow the same structural recursion.
A runtime binder stores its optional package-witness telescope together with a typed projection route (`ignore`,
`package`, or component-wise `product`); this route is internal static evidence and erases before dynamics.

The direct transition removes `VArrow`, `VForall`, and `VPackPi` in favour of `ValPi`,
and removes every static-view artifact (`ViewId`, view signatures, view namespaces,
expansion plans, and view-specific source loading).
`val` and `let val` produce ordinary values. Pipelines elaborate to ordinary `ValPi` application.
Only `f ~> p` remains as new pattern machinery, specified independently by the value-view proposal.
