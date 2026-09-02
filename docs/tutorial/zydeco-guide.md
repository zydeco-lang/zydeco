# Zydeco Guide

This guide collects the language knowledge a working Zydeco programmer needs in one place.
It is written for readers who have seen typed functional programming before but have not yet
internalized call-by-push-value (CBPV) or Zydeco's module and monadic-block idioms.
The guide is deliberately source-level: compiler architecture belongs to
[`DESIGN.md`](../../DESIGN.md), the OOPSLA artifact walkthrough remains
[`lib/tests/oopsla/README.md`](../../lib/tests/oopsla/README.md), and the paper is
[_Notions of Stack-Manipulating Computation and Relative Monads_](https://arxiv.org/abs/2502.15031).

All examples use the current root-term syntax. Some OOPSLA artifact snippets still show the older
`def x = ... end` spelling; current code writes `def x = ... that` inside a `begin ... end` block.

---

## 1. The core idea

Zydeco separates **values** from **computations**.

- A value is inert. Variables, literals, tuples, data constructors, and thunks are values.
  Evaluating a value performs no work and has no side effect.
- A computation runs. Forcing a thunk, returning a value, applying a function,
  sequencing with `do`, and running an `OS` action are computations.

The two kinds are spelled `VType` and `CType`. The builtin package supplies the four boundary forms:

| Form | Reading |
| --- | --- |
| `Thk B` | the value type of a suspended computation `B` |
| `{ M }` | a value that suspends the computation `M` |
| `Ret A` | the computation type that returns a value of type `A` |
| `ret value` | a computation that returns `value` |
| `! thunk` | force a thunk, producing the suspended computation |
| `do pattern <- M; N` | run `M : Ret A`, bind the returned value, then run `N` |
| `A -> B` | computation type of a function from value type `A` to computation type `B` |
| `fn pattern => M` | introduce a function computation |
| `M { continuation }` | pass a suspended continuation to a computation that expects one |

The executable root of a program must have type `OS`. An `OS` computation owns the host stack and
may perform I/O; the launcher runs the whole term and uses the value passed to `process/exit`
as the process exit code.

A tiny executable:

```zydeco
begin
  param (/core; /representations; /numeric; /system; builtin) : @(import("../std/builtin.zy")) that
  let (/VType; /CType; /Thk; /Ret; /Unit) = core that
  let (/String) = representations/string that
  let (= Int64, int64) = numeric/int64 that
  let (/OS; /stdio; /process) = system that

  ! stdio/write_line "hello" {
    ! process/exit 0
  }
end
```

Notes:

- `@(import("path"))` is sugar for `@[import("path")] _`. Both forms mean
  “splice a fresh copy of the imported source term here.”
- Relative import paths are resolved from the file that contains the import; the snippets
  below use paths appropriate for files under `lib/tests/`.
- `stdio/write_line` is a thunked `String -> Thk OS -> OS` function. The final argument of an
  `OS`-producing operation is the continuation: the computation to run next.
- `process/exit` is `Thk (Int64 -> OS)`.

### Running programs

```sh
zydeco run path/to/program.zy      # run through the interpreter
zydeco run --dry path/to/program.zy
zydeco check path/to/program.zy    # parse, resolve, and type check only
zydeco repl                        # interactive full-screen REPL
```

In the REPL, root metadata selects a command for the submitted term:

```zydeco
@[type] ret 1
@[run] ret 1
@[help] _
@[quit] _
```

An earlier REPL input can be spliced by number with `@[import(1)] _`. Editor support is provided
by the `cajun` language server under `editor/`.

---

## 2. One source file, one term

A `.zy` or `.zydeco` file contains exactly one term. There is no declaration pass and no
distinguished `main` name: the whole file is the program. Definitions, parameters, and imports
are themselves term forms.

The canonical outer shape is a `begin ... end` block. Inside it, three binding forms contribute
context:

- `param pattern in/that term` adds a parameter.
- `param val pattern in/that term` adds a total value-function parameter.
- `let pattern = term in/that term` adds a **transparent** binding.
- `def pattern = term in/that term` adds a **sealed** binding.

The connective controls placement:

- `in` keeps the binder exactly where it is written, scoped over its syntactic tail.
- `that` makes the binder **mobile**: it belongs to the nearest enclosing `begin` block and the
  block places it where its dependencies allow. The binder's name is visible throughout the
  entire block, including text written before the binding.

Mobile bindings are dependency-ordered. Source order only breaks ties. This is why the standard
skeleton can put an import, then a `param`, and then open its fields with `that`:

```zydeco
begin
  param (/core; /representations; /numeric; /system; builtin) : @(import("../std/builtin.zy")) that
  let (/VType; /Thk) = core that
  let (/OS; /process) = system that

  ...
end
```

A nested `begin` starts a new mobile-binding boundary. Use `in` for a local name whose scope
should remain visibly short.

### `let` versus `def`

- `let` keeps the right-hand side transparent. The checker may unfold it during equality and
  type-directed elaboration. Use `let` for type aliases and lightweight abbreviations.
- `def` seals the right-hand side and gives the binder a stable identity. Use `def` for
  `data` and `codata` definitions when the representation should be an abstraction boundary.

A function-carrier alias such as `State` must be transparent for the checker to see its arrow:

```zydeco
let State (S : VType) (A : VType) = S -> Ret (A * S) that
```

Markdown documentation blocks are written with `--|` and attach to a following `@[doc]`
annotation. An unattached `--|` block is a warning; use `--` for implementation notes.

### The `!` in a binding is part of the (co)pattern

A binding header is read as an elimination pattern for the name being bound. The general form

```text
def ! name params : B = body that
let ! name params : B = body that
```

therefore says: at a use site, this name is eliminated with `!`. The binding is the
thunk-pattern spelling of

```text
def name params : Thk B = { body } that
```

The two spellings have the same meaning. Which one to write is determined by how the name is
used, not by a separate “computation versus value” mode:

```text
use site:  ! f Int64 "argument"
binding:   def ! f (A : VType) ... : B = body that

use site:  capability
binding:   let capability : Capability = (...) in
```

In the first line the consumer forces `f` and then applies type and value arguments, so the
binding pattern carries `!`. In the second line the consumer uses `capability` directly as a
package value, so the binding pattern is plain.

The same duality applies at every elimination site. A thunk binding can be consumed either as
the value `f : Thk B` or as the forced computation `! f : B`; the binding pattern simply records
which shape the reader should expect at the primary use site. This is why `def !` is only
well-kinded when `B` is a computation type: `Thk B` is a value type exactly when `B` is a
computation type. A package or other VType value therefore uses plain `let` or `def`.

---

## 3. Kinds, types, and type-level terms

Kinds classify type-level terms. The base kinds are `VType` and `CType`; `Set` classifies kinds.
`VType -> VType -> CType` is the kind of a binary carrier such as `State` or `Exception`.

Type-level abstraction and application use ordinary term syntax:

```zydeco
fn (E : VType) (A : VType) =>
  data
  | +Left : E
  | +Right : A
  end

Either String Int64
```

The primitive value types include:

- `Unit`
- `Thk B`
- products `A * B`
- data types
- fixed-width numbers: `Int8`, `Int16`, `Int32`, `Int64`, `UInt8`, `UInt16`, `UInt32`,
  `UInt64`, `Float32`, `Float64`
- `Char`, `String`, `Bytes`

The primitive computation types include:

- `Ret A`
- arrows `A -> B`
- `forall (X : K) . B`
- package-dependent arrows `pi (pattern : A) . B`
- `OS`
- codata types

A literal with no expected type defaults to `Int64` (integer) or `Float64` (decimal). There are
no implicit numeric conversions; a literal must fit the expected fixed-width type. Integer
arithmetic wraps within the selected width, and floating-point arithmetic follows IEEE 754 at
the selected width.

### Quantifiers and package boundaries

`forall` abstracts a type over a kind:

```zydeco
forall (A : VType) . A -> Ret A
```

`exists` hides type-level witnesses inside a value package. A witness may be abstract:

```zydeco
exists (= Option : VType -> VType) .
  (#none :: Thk (forall (A : VType) . Ret (Option A)))
* (#some :: Thk (forall (A : VType) . A -> Ret (Option A)))
```

or manifest, which publishes its definition:

```zydeco
exists (= Int64 as @(intrinsic(i64)) : VType) .
  (#add :: Thk (Int64 -> Int64 -> Ret Int64))
```

The leading `=` puns the public field name with the binder, so the two fields above are
`#Option` and `#Int64`. In the manifest case, `as` discloses the equation
`Int64 ≡ @(intrinsic(i64))`. An explicit `exists (#Int64 = Hidden : VType) . B` form remains
available when the public name and the provider's local binder differ.

`pi` abstracts over a whole package pattern. It is how a library states its dependency on the
builtin provider while opening some of its fields:

```zydeco
pi (_ : @[import("../std/builtin.zy")] _) .
  exists (= State : VType -> VType -> CType) .
    (#get :: ...) * (#put :: ...) * ...
```

---

## 4. Products, named fields, and packages

Parenthesized comma sequences are **tuples**. `*` builds a binary product type, which associates
to the right, so `A * B * C` is `A * (B * C)`. Tuples are stored as n-ary spines but typed as
right-associated products.

Named fields have two spellings:

```zydeco
#field :: A          -- a product/payload classifier carrying `field`
#field = value       -- introduce or pattern-match that named payload
(#field = value) : (#field :: A)
```

`term/field` projects a named field; it binds tighter than application. Search is transparent
through named classifiers and product components. Missing and ambiguous fields are static errors.
To open a package without naming every field, use a projection-pattern group:

```zydeco
param (/core; /representations; /numeric; /system; builtin) : @(import("../std/builtin.zy")) in
let (/VType; /Thk; /Ret) = core in
...
```

`/field = local_name` renames; `/field` alone is the pun `/field = field`. A final ordinary
member such as `builtin` retains the whole package for forwarding.

Type-level named projection uses the same slash: if `T : (#field :: K)`, then `T/field : K`.
A manifest named type such as `(#field = A)/field` reduces to `A`; an abstract named type keeps
the projection explicit until it is instantiated.

### Projection and prefix precedence

Projection binds tighter than the undelimited prefixes `!` and `ret`, which bind tighter than
application. Therefore:

```zydeco
! cap/get argument  ≡ (! (cap/get)) argument
ret cap/initial     ≡ ret (cap/initial)
```

Parentheses express the converse grouping when the result of a prefix form is projected:

```zydeco
(! thunk)/field
```

Constructor introduction, described next, occupies the same prefix level.

---

## 5. Data and codata

A `data` term introduces a value type and its constructors:

```zydeco
def Bool =
  data
  | +False : Unit
  | +True : Unit
  end
that
```

A constructor takes one payload; a nullary constructor takes `Unit` and is used as `+True()`.
Projection binds into an unparenthesized constructor payload, so `+Some package/value` means
`+Some(package/value)`; use `(+Some(value))/field` to project from the constructed value.
`match` eliminates data:

```zydeco
match condition
| +False() => ret 0
| +True()  => ret 1
end
```

Constructor spines are whitespace-guided:

```zydeco
| +Some +Pair(left, right) => ret left
```

A `codata` term introduces a computation type and its destructors. A destructor is an observation
with its own residual signature:

```zydeco
def Counter =
  codata
  | .tick : Ret Counter
  | .value : Ret Int64
  end
that
```

`comatch` introduces a codata computation, one arm per destructor:

```zydeco
comatch
| .tick => ...
| .value => ret 0
end
```

`counter .value` selects a destructor; `.` binds like application and only appears on computations.
Recursive functions use `fix` in the binding header:

```zydeco
def fix countdown (n : Int64) : Ret Int64 =
  ...
that
```

Recursive function groups are not a separate feature; mutually recursive functions are encoded
through codata observations, as in the CBV machine example.

---

## 6. The Builtin package

`lib/std/builtin.zy` is the provider signature. It is generative: abstract capabilities such as
`Reader`, `Writer`, and `OS` receive fresh identities when the host supplies the package.

The commonly used fields are:

| Field | Contents |
| --- | --- |
| `core` | `VType`, `CType`, `Thk`, `Ret`, `Unit` |
| `representations` | one manifest scalar package per primitive type, e.g. `representations/string` |
| `numeric` | operation packages per numeric type, e.g. `numeric/int64` |
| `text` | `char`, `string`, `bytes` operation packages |
| `system` | `io`, `fs`, `stdio`, `args`, `random`, `process` |

Open a fixed representation with its conventional field name:

```zydeco
let (/String) = representations/string that
let (= Int64, int64) = numeric/int64 that
```

The second pattern binds both the type `Int64` and the operations package `int64`. Typical
operations are:

```zydeco
! int64/add left right              -- Ret Int64
! int64/eq (Ret Bool) left right
  { ret +True() } { ret +False() }  -- branch by result type
! stdio/write_line text { next }    -- String -> Thk OS -> OS
! process/exit 0                    -- OS
```

`lib/std/std.zy` is a larger facade over builtin plus the standard data modules (`Bool`,
`Option`, `Result`, `List`) and numeric capability packages. Prefer it for ordinary data types;
use `builtin.zy` directly when only primitive capabilities are needed.

---

## 7. Relative monads

A relative monad in Zydeco is a carrier

```zydeco
M : VType -> CType
```

with a codata dictionary, defined once in `lib/std/control/monad.zy`:

```zydeco
let Monad (M : VType -> CType) =
  codata
  | .return : forall (A : VType) . A -> M A
  | .bind :
      forall (A : VType) (B : VType) .
        Thk (M A) -> Thk (A -> M B) -> M B
  end
that

let Algebra (M : VType -> CType) (R : CType) =
  forall (A : VType) .
    Thk (M A) -> Thk (A -> R) -> R
that
```

`bind` takes thunks because computations passed as values must be suspended. The `Ret` monad is
the identity instance:

```zydeco
def ! mo_ret : Monad Ret =
  comatch
  | .return A value => ret value
  | .bind A B computation continuation =>
    do value <- ! computation;
    ! continuation value
  end
that
```

The binding pattern `! mo_ret` records that `mo_ret` is a thunk. A use site may take the
value `mo_ret`, or force it as `! mo_ret`; sites requiring a `Thk (Monad Ret)` therefore write
`{ ! mo_ret }` to force and immediately re-suspend.

---

## 8. Monadic blocks

`@[monadic]` attaches to any computation term. During type checking the payload is translated by
the algebra translation:

- `Ret A` becomes the ambient `M A`.
- `ret value` becomes `! mo .return A value`.
- `do pattern <- M; N` becomes `! mo .bind ...`.

The translation needs the names `Monad` and `Algebra` in lexical scope. The usual setup is:

```zydeco
let monadic_basis = @(import("../std/control/monad.zy")) that
...
let (= Monad, = Algebra, ()) = builtin |> monadic_basis in
```

A minimal block:

```zydeco
def ! #translated = @[monadic] begin
  do value <- ret 1;
  ret value
end that

do value <- ! translated Ret { ! mo_ret };
...
```

The translated term is a function computation. Its first two arguments are always the chosen
carrier `M` and the thunked instance `mo : Thk (Monad M)`. Function parameters written inside
the annotated term follow those two arguments, left to right.

### Effect operations inside a block

An operation usable by a generic monadic block is written with `Ret` in its source signature.
The translation replaces `Ret` with the ambient monad:

```text
source:  get   : Thk (Ret S)
lifted:  get   : Thk (M S)

source:  put   : Thk (S -> Ret Unit)
lifted:  put   : Thk (S -> M Unit)

source:  raise : Thk (forall (A : VType) . E -> Ret A)
lifted:  raise : Thk (forall (A : VType) . Thk Top -> E -> M A)
```

A `forall` inside the translated type gains a `Thk Top` **structure argument**. Concrete
operations must be built in that lifted shape. This is why `raise` in
`lib/std/control/exception.zy` has the extra parameter:

```zydeco
def ! raise (E : VType) (A : VType) (_ : Thk Top) (e : E) : Exception E A = ...
```

### Instantiation order

For

```zydeco
def ! program (S : VType) (E : VType) =
  @[monadic] begin
    fn (get : Thk (Ret S))
       (raise : Thk (forall (A : VType) . E -> Ret A))
       (msg : E)
    =>
      do n <- ! get;
      do _ <- ! raise S msg;
      ret n
  end
that
```

the application is:

```zydeco
(! program Int64 String)
  M
  { ! mo }
  { ! get_concrete }
  { ! raise_concrete }
  "message"
```

`S` and `E` are parameters of `program`, outside the annotation, so they come before the
translated M/mo arguments. Parameters written inside the annotated `fn` follow M/mo.

---

## 9. Effect modules

The effect-related modules under `lib/std/control` are:

| Module | Exports |
| --- | --- |
| `monad.zy` | `Monad`, `Algebra` |
| `state.zy` | `State`, `MonadState`, `mo_state`, `state`, `get`, `put`, `modify`, `run_state`, `eval_state` |
| `exception.zy` | `Exception`, `MonadThrow`, `mo_exception`, `throw_ops`, `raise`, `handle_exception`, `try_exception` |
| `state-exn.zy` | `StateExn`, `mo_state_exn`, `state_ops`, `throw_ops`, `get`, `put`, `raise`, `catch`, `run_state_exn` |

A module source is a first-class value function from Builtin to an `exists`-wrapped package. When its implementation
is a `begin ... end` block, put `param val pattern that` inside that block; use direct `val pattern => value` for a
compact non-block body. Its `ValPi` classifier is inferred from the annotated, irrefutable parameter and the final
`pack` introduction. Importing the source yields an ordinary value, so opening it requires neither a thunk nor a
returned computation.
Open a module with a projection group:

```zydeco
let exception_basis = @(import("../std/control/exception.zy")) that
...
let (
  = Exception,
  = MonadThrow,
  = mo_exception,
  = throw_ops,
  = raise,
  = handle_exception,
  = try_exception
) = builtin |> exception_basis in
```

Abstract carriers (`State`, `Exception`, `StateExn`) cannot be pattern-matched or directly
applied. Eliminate them through the module's runners:

```zydeco
! run_state S A B initial computation {
  fn final_state answer => ...
}
```

---

## 10. Capability packages

The most reusable way to write effect-polymorphic user code is to package the required
operations into a named product whose fields are written with `Ret`. The concrete module then
supplies one instance of that product for the chosen monad.

The source-side signature:

```zydeco
let StateExnCapability (S : VType) (E : VType) =
  (#get :: Thk (Ret S))
* (#put :: Thk (S -> Ret Unit))
* (#raise :: Thk (forall (A : VType) . E -> Ret A))
* (#catch :: Thk (
    forall (A : VType) .
      Thk (Ret A) -> Thk (E -> Ret A) -> Ret A
  ))
* (#add :: Thk (S -> S -> Ret S))
that
```

The user program depends on this package, not on any concrete module:

```zydeco
def ! user_program (S : VType) (E : VType) =
  @[monadic] begin
    fn (cap : StateExnCapability S E) (one : S) (msg : E) =>
      do n <- ! cap/get;
      do recovered <- ! cap/catch S
        {
          do _ <- ! cap/raise S msg;
          ret n
        }
        {
          fn _ => do m <- ! cap/get; ret m
        };
      do n' <- ! cap/add recovered one;
      do _ <- ! cap/put n';
      ret n'
  end
that
```

The algebra translation lifts the package fieldwise. At the application site, construct the
lifted package for the chosen `M`, bind it with plain `let`, and pass it directly:

```zydeco
let capability : (
     #get :: Thk (M S)
  ) * (
     #put :: Thk (S -> M Unit)
  ) * (
     #raise :: Thk (forall (A : VType) . Thk Top -> E -> M A)
  ) * (
     #catch :: Thk (
       forall (A : VType) .
         Thk Top -> Thk (M A) -> Thk (E -> M A) -> M A
     )
  ) * (
     #add :: Thk (S -> S -> M S)
  ) = (
    #get = { ! get_concrete },
    #put = { ! put_concrete },
    #raise = { ! raise_concrete },
    #catch = { ! catch_concrete },
    #add = add_concrete
  )
in

(! user_program S E) M { ! mo } capability one message
```

The complete working example is
[`lib/tests/effects/state-exception-stack.zy`](../../lib/tests/effects/state-exception-stack.zy).

The three traps here are the same three syntax rules from earlier sections:

1. Projection binds inside force: `! cap/get` forces the selected thunk, while `(! cap)/get`
   projects from the result of forcing `cap`.
2. Bind the package with plain `let`, because it is a value.
3. Pass the package directly; do not wrap it in `{ ! ... }` because it is not a thunk.

---

## 11. Pitfall checklist

1. **Root type** — a runnable program must end in `OS`.
2. **Prefix precedence** — projection binds inside `!`, `ret`, and constructor introduction,
   while application remains outside: `! record/field x` parses as `(! (record/field)) x`.
3. **`do` only eliminates `Ret`** — an `OS`, a function, or an arbitrary `M A` computation is
   not directly bindable. Sequence `OS` actions by passing continuations; sequence a relative
   monad with `bind` or a monadic block.
4. **`!` in a binding mirrors the use site** — if consumers write `! name`, bind with
   `def ! name` or `let ! name`; if consumers write `name`, bind without `!`. A package or
   other VType value therefore uses plain `let`.
5. **A thunk binding may still be used as a value** — `def ! f` gives `f : Thk B`; pass `f`
   directly where a thunk is expected, or force it as `! f` where a computation is expected.
6. **`let` versus `def`** — transparent aliases need `let`; nominal `data`/`codata` normally
   use `def`. A carrier alias such as `State` must be `let` if clients need its arrow.
7. **Monadic blocks need lexical `Monad` and `Algebra`** — open `monad.zy` in the scope where
   `@[monadic]` appears.
8. **Monadic operation signatures use `Ret`** — the translation only rewrites `Ret`. A
   concrete-M type inside a generic block defeats the abstraction.
9. **`forall` in lifted operations gains `Thk Top`** — concrete `raise`/`catch` instances take
   that structure parameter.
10. **Abstract carriers are opaque** — use the module's runner, not `match` or direct
    application.
11. **Thunk arguments are plain values** — `mo` is passed as `{ ! mo_instance }` because its
    classifier is `Thk (Monad M)`, but an already-thunked operation is passed directly.
12. **Continuation style** — `stdio` and `io` operations take the next `OS` computation as an
    explicit thunk argument.

---

## 12. Quick grammar cheat sheet

```text
-- comments                    -- line
/- block -/                    block

VType  CType  Set              kinds
Thk B   Ret A   A -> B         core CBPV type constructors
Unit    A * B   data            value type constructors
codata                          computation type constructor
forall (X : K) . B             computation-level polymorphism
pi (pattern : A) . B           package-dependent function
exists (X : K) . A             existential value package
exists (X = def as X : K) . A  manifest existential

{ M }                          thunk value
! V                            force
ret V                          return
do P <- M; N                   Ret sequencing
fn P => M                      computation abstraction
fix P => M                     computation fixed point
match V | P => M ... end       data elimination
comatch | .d => M ... end      codata introduction
M .d                           codata destructor
V / field                      named projection
+Constructor payload           data constructor
#field :: A                    named payload classifier
#field = term                  named payload introduction/pattern

begin term end                 mobile-binding block
param P in/that term           parameter
param val P in/that term       value-function parameter
let P = term in/that term      transparent binding
def P = term in/that term      sealed binding
def ! P ... : B = term         thunk-pattern binding; use site is !P
@[monadic] term                algebra translation
@(import("path"))              import sugar
```

---

## 13. Where to look next

- [`lib/tests/effects/`](../../lib/tests/effects/) — executable examples of the modules and
  capability packages described here.
- [`lib/tests/oopsla/README.md`](../../lib/tests/oopsla/README.md) — the artifact walkthrough,
  including the monad-law violation example and relative-monad transformers.
- [`docs/proposals/syntax.md`](../proposals/syntax.md) — why the concrete syntax looks the way it does.
- [`docs/proposals/style.md`](../proposals/style.md) — naming, layout, package-opening, and monadic
  style conventions.
- [`docs/spell/`](../spell/) — the self-hosting literate tutorial.
- [`lib/tests/delimcc/`](../../lib/tests/delimcc/) — `reset`/`shift` delimited continuations and
  `try`/`throw`, built from the continuation monad.
- [`DESIGN.md`](../../DESIGN.md) — language model, pipeline, and implementation architecture.
