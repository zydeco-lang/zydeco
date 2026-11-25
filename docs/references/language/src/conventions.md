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

Examples use complete source terms, or the displayed metavariables with an explicitly described context,
and rely on the default vocabulary.
An intrinsic appears in an example only where that vocabulary is unavailable: under `@[prelude(none)]`,
inside a closed translation block, or for an operation outside the core set.
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

## Static and Runtime Phases

Term-oriented composition builds a library out of types, packed values, and value functions.
Much of that structure serves checking and reuse and is settled before the program runs.
Zydeco therefore gives every term a phase. The phase answers a question the value-computation split leaves open:
which parts of a checked program survive into execution.

| Phase | Terms | Before execution |
| --- | --- | --- |
| Static | Kinds, types, and type arguments; the type witnesses of packed values; field names; value functions, their applications, and views | Reduced or erased |
| Runtime | Values such as literals, products, constructor applications, and thunks; computations | Retained as the residual program |

[Static elimination](static-elimination.md) is the step between the two phases.
It reduces value-function applications and opens statically known packed values until only runtime values
and computations remain, erasing types, witnesses, and field names along the way.
The reduction never runs a computation.
A static term can therefore capture a runtime value and place it in its result, while any choice it makes,
such as a value match or an integer operation, needs inputs known during checking.
A program whose residual form would still contain a value function is rejected with `tyck.static-elimination`.

The phase also tells a reader what an abstraction costs.
Type arguments erase, and a `val` function is expanded at each application,
so neither adds a runtime call; the expansion can enlarge the residual code.
A thunk `{ M }` is the runtime form of abstraction.
It is an ordinary value that can be stored, passed, and selected dynamically, and forcing it runs `M`.

## A First Program

The program below prints one line and exits.
It is small enough to read whole, and it contains a term of each sort and phase.

```zydeco
param (/stdio; /process) : @(import("../../lib/std/builtin.zy")) in
let Greeting = String in
let val greet (text : Greeting) = (#message = text, #code = 0) in
let reply = greet "hello, world!" in
let finish : Thk OS = { ! process/exit reply/code } in
! stdio/write_line reply/message finish
```

| Line | What it introduces | Sort and phase |
| --- | --- | --- |
| `param ... in` | The host's Builtin [packed value](polymorphism-and-packed-values.md), from which a [projection pattern](polymorphism-and-packed-values.md#field-search-and-projection-patterns) selects two operation groups | A runtime value; the selections are static routes |
| `let Greeting = String` | A transparent alias | A type; static |
| `let val greet ...` | A [value function](value-functions-and-views.md) that builds a named product | A value function; static |
| `let reply = ...` | The application of `greet` | The application is static; its result is a runtime product |
| `let finish ...` | A thunk suspending the exit | A runtime value of type `Thk OS` |
| `! stdio/write_line ...` | The call that writes the line and continues with `finish` | A computation; runtime |

The whole term is a computation that accepts Builtin and ends in the host's `OS` protocol,
which is the shape [`run` requires](sources.md#source-boundaries).
`write_line` receives its successor as a thunk
because host effects use [explicit continuation-passing style](computations-and-control.md#ret-and-explicit-cps).
After static elimination, `Greeting`, `greet`, and the field names `message` and `code` are gone.
The residual program holds the string, the integer, the thunk, and the final call.

A quoted import path is relative to the importing file;
this one assumes a source two directories below the repository root, as the other examples in this reference do.
`zydeco run` executes the program,
and the [CLI guide](../../../../cli/README.md#run-and-test-programs) covers the remaining commands.
