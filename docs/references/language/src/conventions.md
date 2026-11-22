# Conventions and the Language Model

## The Value-Computation Split

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
`VType`, `CType`, `Thk`, `Ret`, and primitive type names are ordinary bindings rather than reserved keywords;
ordinary source files receive them automatically from the [core prelude](sources/core-prelude.md),
while opted-out scopes and closed translation blocks bind them explicitly through intrinsics or provider openings.
Host capabilities such as `OS`, `Addr`, `Reader`, and `Writer` always come from an explicit provider opening.
Literal syntax itself determines a primitive type when no expected type is available.

An arrow `A -> B` expects an `A` argument above a residual `B` stack.
`Ret A` expects a continuation receiving an `A`; `Thk B` is a value suspending a computation with protocol `B`.
These protocols do not prescribe physical stack layout or linear use.

Examples use complete source terms or the displayed metavariables and an explicitly described context;
some examples bind intrinsics explicitly to stay self-contained under `@[prelude(none)]`.
The surrounding prose identifies intentional rejection cases.
A checked term need not be a standalone executable.

## Term-Oriented Composition

Zydeco's term-oriented design uses binding, abstraction, and application
to compose expressions at the language's different sorts.
Bindings and parameters are themselves term forms with scoped bodies ([Bindings and Scope](bindings-and-scope.md)):
`let x = e in body` combines a definition with its use, while `param p in body` constructs an abstraction.
Both forms can occur within larger expressions.
Type functions build types, and [value functions](value-functions-and-views.md) assemble values.

The benefit is that library composition can use ordinary language mechanisms.
Products group operations, [packed values](polymorphism-and-packed-values.md) associate them with abstract
or manifest type witnesses, and [value functions](value-functions-and-views.md) take dependencies explicitly.
These structures can be nested, passed, and composed wherever their classifiers permit them.

Composition follows the relevant typing and phase rules:
computation sequencing is explicit ([Computations and Control](computations-and-control.md)),
and execution requires [static elimination](static-elimination.md) of value-function applications
and static witness structure.
[Sources, packages, and compilation units](sources.md) apply this composition model to reuse and distribution.
