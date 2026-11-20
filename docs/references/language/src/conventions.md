# Conventions and the Language Model

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

## Term-Oriented Composition

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
