# Value Functions and Views with `ValPi`

## Abstract

This proposal adds a single dependent classifier for total value transformations:

```zydeco
val pi (A : VType) (value : A) . A
```

Its direct introduction form is `val`, its block-form introduction is `param val`,
its ordinary binding sugar is `let val`, and its elimination forms are application,
the pipeline operators `|>` and `<|`, and the value-view pattern `f ~> p`.

```zydeco
let val id (A : VType) (value : A) : A = value that
let answer = 42 |> id Int64 in
...
```

`ValPi` internalises a derivation of the CBPV value judgment as a definitional function.
Its requirement is static elimination: functions may be passed, returned, and collected in intermediate structures,
provided their composition normalizes before executable code is emitted.
It is distinct from the existing computation arrow: `A -> C` classifies a computation accepting `A`,
while `val pi (x : A) . B` classifies a total, effect-free transformation producing a value.
The resulting runtime data construction may depend on runtime inputs, but the value-function abstraction
and application themselves have no runtime representation.
The same transformation is available inside patterns without introducing a separate class of named views.

This proposal specifies the replacement for the implementation's second-class occurrence restrictions.
The remaining implementation work is recorded under [Implementation Boundary](#implementation-boundary).

## One Classifier

Earlier implementations divided value functions into non-dependent arrows,
polymorphic functions, and package-dependent functions.
Those are three instances of one telescope:

```text
ValPiBinder ::= TypeBinder | ValueBinder(domain, package-witnesses?)
ValPi       ::= val pi ValPiBinder . Type
```

A type binder expresses polymorphism within the statically eliminated value-function space:

```zydeco
val pi (A : VType) . A
```

Computation-level `forall` remains available independently:
`Thk (forall (A : VType) . A -> Ret A)` classifies a runtime polymorphic function whose type arguments erase
but whose callable code may remain.

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
Type parameters erase during lowering; value parameters extend the lexical environment used for static reduction.
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

Application has three term spellings: juxtaposition, `V |> f`, and `f <| V`.
The [value-view pattern](#value-views) applies the same operation before matching its result.

```text
V |> f  ==  f V  ==  f <| V
```

After static elimination, the remaining work consists of value projections and constructions,
including explicitly authored thunks.
The application itself needs no value-function closure or indirect call and performs no effect.
A value function is a derivation `x : A |-v W : B`; the cut happens entirely at the value level,
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

The function side may itself be computed by a value function or selected from a statically known structure.
Partial applications and function-valued results follow the same [static elimination requirement](#static-elimination),
independently of the application spelling.

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

## Value Views

A value view observes a value through a total value function before matching it.
If `f` has classifier `val pi (_ : A) . B` and `p` is a pattern for `B`, then `f ~> p` is a pattern for `A`:

```zydeco
let val first ((left, _) : A * B) : A = left that
let first ~> selected = pair in
...
```

This is an active pattern in a deliberately narrow sense: it precomposes an existing pattern
with a statically eliminated, effect-free map.
The function is an ordinary `ValPi` value, so views introduce no declaration class or namespace.

### Pattern Formation

The surface extension is

```text
p ::= ... | f ~> p
```

where `f` ranges semantically over checked value terms.
The current surface grammar accepts a value variable followed by optional bracketed type arguments;
this keeps erased application visibly separate from the nested pattern.
The operator associates to the right, so

```zydeco
let first_view ~> second_view ~> result = input in ...
```

applies `first_view`, then `second_view`, and finally matches `result`.
Naming a statically computed function before the pattern permits higher-order static selection
and lexical capture without making the pattern grammar ambiguous.
The current occurrence checker still rejects some such compositions, as described
under [Implementation Boundary](#implementation-boundary).

The typing rule is ordinary value-function elimination followed by pattern checking:

```text
Delta; Gamma |-v f : val pi (_ : A) . B
Delta; Gamma |-p p <= B -| Gamma'
------------------------------------------------ VIEW
Delta; Gamma |-p f ~> p <= A -| Gamma'
```

Only the nested pattern contributes binders.
The function expression is checked in the lexical environment at the pattern site,
so a view may use a statically selected function that closes over ambient runtime values.
Its head follows the same [static elimination requirement](#static-elimination) as a term application.

The initial rule requires a single runtime value binder.
A polymorphic function must be instantiated before it is used as a view:

```zydeco
let val first (A : VType) (B : VType) ((left, _) : A * B) : A = left that
let first[Int64, String] ~> selected = pair in
...
```

Square brackets are pattern syntax for erased type application.
They make the boundary between static arguments and the nested pattern explicit;
the term-level spelling remains ordinary value application.

### Meaning

A view pattern is defined by expansion through a fresh intermediate value:

```text
let f ~> p = V in N  ==  let p = (V |> f) in N
```

Here `==` is a source-language equation.
Equivalently, if a pattern denotes a partial binding map, then

```text
match_(f ~> p) = match_p o F_f
```

where `F_f : Value(A) -> Value(B)` is the total map denoted by `f`.
The view changes how a value is presented to a pattern while preserving the function's ordinary meaning.
For a fresh variable `result`, term and pattern uses agree:

```text
V |> f  ==  let f ~> result = V in result
```

This coherence condition ensures that a value function has one meaning whether its result is retained
as a term or immediately decomposed by a pattern.

### Refutability and Coverage

Refutability belongs to the result pattern:

```text
irrefutable(f ~> p) iff irrefutable(p)
```

Applying `f` cannot fail, diverge, or perform effects.
A partial observation must expose failure in its result type, for example by returning `Option B`,
and the nested pattern may then choose which result to accept.

Coverage is necessarily conservative for arbitrary functions.
Arms that use the same syntactic function and the same static arguments may be analysed as patterns over its codomain.
Exhaustiveness over that codomain implies exhaustiveness over the domain,
although the converse need not hold when the function is not surjective.
Arms with unrelated functions require an ordinary exhaustive fallback.

### Elaboration and Sharing

The elaborated pattern stores a checked value expression and its nested pattern.
Static elaboration eliminates the function application by the same rules as `|>`;
runtime matching observes its residual value construction and continues with the nested pattern.

An implementation may share a transformed result between adjacent arms after proving
that their function expressions are equivalent and pure.
Such sharing is an optimisation, not part of name resolution or the source semantics.
A later elaborator could expose a typed equivalence key for this purpose.
Whether to admit arbitrary value terms directly as view heads remains a pattern-syntax question;
the variable-plus-type-arguments restriction does not require a separate function namespace.

## CBPV Boundary

The proposal adds a positive function space; it does not reinterpret the computation arrow.
A value function cannot execute `force`, computation application, effects, or general recursion in the value judgment.
It may construct a thunk containing such computations, whose execution remains suspended.
Such behaviour retains the CBPV shape `A -> C`, commonly `A -> Ret B`, and must be thunked
when a computation function itself is stored as a value.

An explicit adapter can use a statically supplied value function inside a thunked computation.
The value application is eliminated when compiling the thunk body, while the authored thunk remains a runtime value.
The [runtime contract account](package-modularization.md#explicit-runtime-contracts) describes the computation
interfaces available at that boundary, including package-dependent `pi` and its `forall` adapters.

## Static Elimination

The shared [static elimination contract](normalization.md#static-elimination-and-residual-code) governs
higher-order `ValPi` composition, lexical capture, and residual runtime data.
For value functions, this admits transport through intermediate products, constructors,
named components, and packages, as well as function-valued parameters and results.
For example, the revised design accepts this composition, which the current occurrence checker rejects:

```zydeco
let Endomorphism = val pi (_ : Unit) . Unit that
let val keep (value : Unit) : Unit = value that
let val apply (function : Endomorphism) (value : Unit) : Unit = function value that
let functions = (keep, ()) that
let (stored, _) = functions that
let recovered : Unit = apply stored () that
```

The product projection and higher-order application reduce to the same value as `keep ()`.
Returning `keep` from another value function or carrying it in a static package is governed by the same rule.
There is no special exemption for polymorphic identity functions: direct and polymorphic transport use one reducer.

A residual runtime product containing a `ValPi` value fails that contract,
as does an application whose function remains unavailable to static reduction.
Term application and view patterns therefore share the same acceptance boundary.

## Runtime and Compilation

A source file may export an unapplied package transformation as an ordinary value term,
with its static parameters discharged by a consumer:

```zydeco
val (builtin : Builtin) =>
  pack (Api : VType) is ... where ... end
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
Factoring repeated residual code into direct blocks remains a backend optimization after static elimination.
It must preserve specialization by static arguments and cannot reintroduce runtime `ValPi` values.

## Implementation Boundary

The implemented `ValPi` representation uses typed binder and argument variants for one telescope.
Formation, introduction, elimination, substitution, formatting,
and package-witness recovery follow the same structural recursion.
A runtime binder stores its optional package-witness telescope together with a typed projection route (`ignore`,
`package`, or component-wise `product`); this route is internal static evidence and erases before dynamics.

This representation replaces `VArrow`, `VForall`, and `VPackPi` with `ValPi`.
View patterns use checked function values without a `ViewId`, view signature, namespace,
dependency graph, expansion plan, or view-specific source loading.
`val` and `let val` produce ordinary values. Pipelines elaborate to ordinary `ValPi` application.
The `f ~> p` pattern retains only the machinery described under [Value Views](#value-views).

The unified `ValPi` representation is implemented, but general static composition is not.
`ValueFunctionChecker` still rejects products and higher-order domains with `tyck.first-class-value-function`,
and SPS resolution follows a restricted set of definition bindings and application spines.
Replacing these occurrence bans must follow the [shared normalization boundary](normalization.md#implementation-status),
updating demand analysis and execution entry points together.

Regression cases must pair accepted static transport through products, packages, partial applications,
and higher-order parameters and results with rejected unresolved runtime transport.
They must cover lexical capture of runtime data, preserved sharing, and the same acceptance outcome
across checking, interpretation, and compiled lowering.
Existing tests that intentionally reject reducible products and higher-order domains describe the superseded requirement
and must change with that implementation.
