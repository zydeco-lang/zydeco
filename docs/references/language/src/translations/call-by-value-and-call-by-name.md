# Call-by-Value and Call-by-Name Blocks

## Call-by-Value Blocks

A **call-by-value block**, `@[cbv] e`, reads `e` as a call-by-value expression
and elaborates it into the core, following Levy's call-by-value translation.
Evaluation order is written into the result rather than chosen by a separate strategy,
which is what the core's separation of values from computations buys.
That order is observed once a call-by-value block sits inside a monadic block that supplies effects,
as set out under [Composing with Monadic Blocks](#composing-with-monadic-blocks).
In the examples below, `Int`, `Unit`, and `VType` stand for the built-in types, which a block binds for itself.
Throughout this chapter an unqualified *block* is a call-by-value block; the core's own block is written `begin … end`,
and [Block Scoping](#block-scoping-type-aliases-and-kind-aliases) relates the two.

A block's classifier is `Ret A'`, where `A'` translates the source type `A`.
That holds for a value body too: `@[cbv] (fn (x : Int) => x)` has type `Ret (Thk (Int -> Ret Int))`,
not `Thk (Int -> Ret Int)`.
A source value does have its own translation, but selecting between the two by whether a body happens
to be a value would make a block's type depend on a local edit inside it.

### Source Kinds and Types

Types translate by sending each source arrow to a suspended computation arrow that returns.
A product passes through unchanged, because the core's product is already a value type.

**Two halves of one rule govern every type row.** A source type translates to a **value** type,
so a former that is already positive passes through and a **negative** one is wrapped in `Thk`.
That much decides the outside. What decides the inside is the core's own kinding:
each position the core former requires to be a **computation** type receives `Ret A'`,
since a source term there translates at that type, and each position it requires to be a value type receives `A'`.
So an arrow's codomain, a quantifier's body, and a codata field carry a returner, and an arrow's **domain** does not.
Taking only the first half gives `Thk (A' -> B')` for an arrow, which is ill-kinded,
because an arrow's codomain must be a computation type.

A block writes a source type with an arrow, a product, a quantifier, an existential (abstract or manifest),
a type operator, type application, a `data` or `codata` type, a field type, a named type,
a type projection, an ascription, or a **name**; those are the only forms it translates.
A name is one the block bound, or an intrinsic for a primitive source type, such as `@(intrinsic(int))`
or `@(intrinsic(unit))`, which translates to itself; a block is closed over types.
A source arrow's sides and a product's components are value-type positions, so a type operator in one is a kind error.

A **source kind** is `VType`, an arrow between source kinds, or a named kind over one;
the kind translation is the identity.
`CType` is not one, so a quantifier over a computation kind is refused: no source type is a computation.
A block writes `VType` with its intrinsic, `@(intrinsic(vtype))`, or by a kind name it bound,
since the enclosing scope's `VType` is a name bound outside the block.
A type binding's declared kind, a type binder's annotation, and an ascription's classifier are all read
by one kind judgment, so each place a block writes a kind admits the same kinds.

| Source type | Core |
| --- | --- |
| `A -> B` | `Thk (A' -> Ret B')` |
| `pi p1 ... pn . B`, `forall p1 ... pn . B` | `Thk (p1' ... pn' . Ret B')`, one function of all `n` parameters, where a binder that declares a kind becomes `forall (X : K) .` and one that declares a type `A' ->`; `A -> B` and `forall (X : K) . A` are the telescopes of one binder |
| `A1 * ... * An` | `A1' * ... * An'` |
| `forall (X : K) . A` | `Thk (forall (X : K) . Ret A')`, with `K` a source kind |
| `exists (X : K) . A` | `exists (X : K) . A'`, with `K` a source kind |
| `exists (X as S : K) . A` | `exists (X as S' : K) . A'`, with `K` a source kind |
| `fn (X : K) => A` | `fn (X : K) => A'`, with `K` a source kind, which may be left out where an arrow kind is expected; it has an arrow kind, so it stands where one is accepted: a type binding at one, a type argument, or the head of an application |
| `F A` | `F' A'`, the operator and the argument translated; then normalized, so `Pair Int` lands as `Int * Int` |
| `data \| +C1 : A1 \| ... end` | `data \| +C1 : A1' \| ... end`, transparent |
| `codata \| .d1 : A1 \| ... end` | `Thk (codata \| .d1 : Ret A1' \| ... end)`, transparent |
| `#f :: A` | `#f :: A'` |
| `(l = S)` | `(l = S')`, at the named kind `l :: K` of `S'`'s kind `K` |
| `S/l` | `S'/l` |
| `(A : K)` | `A'`, with `K` checked against its kind |
| a name the block bound | what the name stands for |
| an intrinsic for a primitive `P` or `unit` | `P`, or `Unit` |

Each form a source type is written with is translated compositionally:
an ascription checks its declared kind against the translation, and `F A` translates to `F' A'`,
so an operator and its argument are both source types rather than core ones.
What a name stands for needs no test of its own.
A type binding's name stands for its translated bindee, a declaration's name for its seal, a type variable for itself,
and an intrinsic for its primitive, so every type a block writes is the translation of a source type by construction.
A shift is refused where it would be written: `Thk (Int -> Ret Int)` names `Thk` and `Ret`,
which the block did not bind, and written with intrinsics it is refused because they name no source primitive.

Which *expressions* a classifier may be written from is decided separately.
A classifier given by an expression outside the forms above, such as a `@[typeof]` query,
is rejected rather than resolved, because resolving one would reach past the closure restriction.

### Source Values and Terms

Terms translate by producing the source value, so an application evaluates both operands first,
the function before the argument, and a binding evaluates its bindee before its body.
A value has its own translation:

| Source value | Core |
| --- | --- |
| `x`, a literal, `()` | itself |
| `_` | `_` at the expected type, a typed hole, which needs the expected type |
| `(v1, ..., vn)` | `(v1', ..., vn')` |
| `#f = v` | `#f = v'` |
| `+C v` | `+C v'`, which needs the expected type |
| `(T, v)` | `(T', v')`, a packed value, which needs the expected type |
| `fn (x : A) => e` | `{ fn (x : A') => e' }` |
| `fn p1 ... pn => e` | `{ fn p1' ... pn' => e' }`, one function of all `n` parameters |
| `fn (X : K) => e` | `{ fn (X : K) => e' }`, a type abstraction; synthesizes when the kind is written, and takes it from an expected quantifier when it is not; an expected quantifier's body is pushed into `e` |
| `comatch \| .di => ei end` | `{ comatch \| .di => ei' end }`, which needs the expected type |

| Source term | Core |
| --- | --- |
| a value `v` | `ret v'` |
| `e1 e2` | `do f <- e1'; do a <- e2'; ! f a` |
| `e e1 ... en` | `do f <- e'; do a1 <- e1'; ...; ! f a1 ... an`, one call taking as many arguments as `f`'s telescope has parameters; a type argument is not sequenced, arguments left over apply to the result, and too few are refused |
| `e A` | `do f <- e'; ! f A'`, an instantiation |
| `e.d` | `do o <- e'; ! o .d` |
| `e/f` | `do v <- e'; ret (v/f)` |
| `(e1, ..., en)` | `do a1 <- e1'; ...; ret (a1, ..., an)` |
| `#f = e` | `do a <- e'; ret (#f = a)` |
| `+C e` | `do a <- e'; ret (+C a)` |
| `(T, e)` | `do a <- e'; ret (T', a)` |
| `let p = e1 in e2` | `do p <- e1'; e2'` |
| `let (X, x) = e1 in e2` | `do (X, x) <- e1'; e2'`, an unpack; `X` may not appear in the type of `e2'` |
| `match e \| +Ci pi => ei end` | `do s <- e'; match s \| +Ci pi => ei' end` |
| `let fix f p1 ... pn : B = e1 in e2` | `do f <- ret { fix g => fn p1' ... pn' => e1' }; e2'`, with `f` resolving to `g` |
| `(e : A)` | `e'`, with `A'` pushed inward |
| `@[doc] e`, `@[debug] e`, `@[format] e` | `e'`; `@[debug]` prints it |
| `let X : K = A in e` | `e'`, with `X` denoting `A'` |
| `let K = κ in e` | `e'`, with `K` denoting the kind `κ` |
| `def X : K = D in e` | `e'`, with `X` denoting a seal over `D'`, or the thunk of one for `codata`; `X` may not appear in the type of `e'` |
| `def X1 : K1 = D1 that … def Xn : Kn = Dn that e` | `e'`, with every `Xi` bound as in the row above before any `Di` is translated; no `Xi` may appear in the type of `e'` |
| `let p = e1 that e2` | the `let` row above; name resolution lifts the binding and leaves a residual |
| `param p : A that e` | `ret { fn (p : A') => e' }` |

**Four source forms have both a value row and a term row** — a tuple, a named field, a constructor, and a packed value.
Which applies is decided by whether the form is a source *value*, which holds for a variable, a literal,
`()`, a hole `_`, either abstraction, and for the other four exactly when their components or payloads are.
A `comatch` has a value row but is not counted as a source value here, so a tuple containing one is sequenced.
The two rows agree up to `do x <- ret v; e` being `e` with `v` substituted for `x`,
so the choice shows in the elaborated term rather than in what it computes.

### Closure Boundary and Core Interoperability

A block is **closed**, over terms, types, and kinds alike: a term that refers
to a variable bound outside it is rejected, so is a type or kind name the block did not bind,
and so is an import boundary anywhere within it, including in a parameter's classifier.
What a block uses from outside arrives as a parameter it abstracts over: an operation or an effect as a value,
and a type by type abstraction, which the enclosing term instantiates.
The built-in types are no exception, since the enclosing scope's `Int` and `VType` are names bound outside the block.
A block writes a primitive with its intrinsic, usually behind a binding of its own:

```zydeco
param (/process) : @(import("../../lib/std/builtin.zy")) in
do answer <- @[cbv] (
  let VType = @(intrinsic(vtype)) that
  let Int : VType = @(intrinsic(int)) that
  (fn (x : Int) => x) 7);
! process/exit answer
```

An intrinsic names nothing outside the block, but it is admitted only for a primitive source type, `unit`,
`vtype`, or `ctype`: `@(intrinsic(thk))` is a shift, which no source type translates to.
The other examples in this chapter leave these bindings out.

An outside type reaches a block the way an outside value does: the block abstracts over it,
and the enclosing term instantiates it.
Below, the block takes a type `O` and the two operations it needs, and the enclosing term instantiates `O`
with a core `data` type and supplies the operations, curried as the translations of the source arrows are:

```zydeco
begin
  param (/process) : @(import("../../lib/std/builtin.zy")) that
  def Opt : VType = data | +None : Unit | +Some : Int end that

  do f <- @[cbv] (
    let VType = @(intrinsic(vtype)) that
    let Int : VType = @(intrinsic(int)) that
    let Unit : VType = @(intrinsic(unit)) that
    fn (O : VType) =>
    fn (some : Int -> O) =>
    fn (caseO : O -> (Unit -> Int) -> (Int -> Int) -> Int) =>
      caseO (some 4) (fn (u : Unit) => 0) (fn (n : Int) => n));

  do g <- ! f Opt;
  do h <- ! g { fn (n : Int) => ret +Some n };
  do r <- ! h {
    fn (o : Opt) => ret {
      fn (none : Thk (Unit -> Ret Int)) => ret {
        fn (some : Thk (Int -> Ret Int)) =>
          match o
          | +None u => ! none u
          | +Some n => ! some n
          end
      }
    }
  };
  ! process/exit r
end
```

Inside the block `O` is abstract, so nothing the block does can depend on how it is represented,
and the enclosing term may instantiate it at any value type, `Thk (Ret Int)` included,
though no source type translates to that.
The block's meaning depends on no outside declaration.
The cost falls on the enclosing term: the block above is a few lines,
and the instantiation and adapted operations are more.
No sugar for instantiating a block and adapting its operations is provided.

### Binders, Telescopes, and Bidirectional Analysis

The smallest blocks use primitive source types and `Unit`, arrows and products over them,
and source terms built from variables, literals, `()`, abstraction, application, tuples, and `let`.
A binder may be a variable, a wildcard, `()`, a named field `(#f = p)` or `(= f)`,
or a product of those, since the core's abstraction rule requires an irrefutable binder.
A `let`'s or a parameter's binder may also be a constructor tag over one, as in `let +Box x = b in M`,
when the tag cannot fail: its declaration has no other constructor.
The core's own check decides that, so a tag that can fail is refused with `tyck.refutable-binding`,
as it is outside a block.

Any part of a binder may carry an annotation, `(p : A)`, as in `let (f : Int -> Int, n) = M in N`.
`A` is a source type, which ordinary pattern checking cannot read, so the block translates it
before the binder is checked, and it is then compared with the type its position expects.
Since that happens before the binder binds anything,
an annotation may not mention a variable the same binder introduces, as `X` in `(X, (x : X))`.
A parameter needs its type: from an annotation around it, from an expected type, or from the annotations inside it
when they give the type of every part, so `fn ((x : Int), (y : Int)) => x` takes `Int * Int`.
A part with no annotation would need inference, which a block does not do.
A function type's binder may name its parameter, but no source type mentions a term: `A -> B`, `pi (_ : A) . B`,
and `pi (x : A) . B` denote one type, and a type that mentions `x`, as `pi (x : A) . x` would, is refused by sort.
So a function may be bound with the core's header, `let f (x : A) : B = e`,
whose desugared signature names the parameter.

Consecutive parameters are one function.
`fn (x : A) (y : B) => e` takes both arguments in one call, at `pi (_ : A) (_ : B) . C`,
while `fn (x : A) => fn (y : B) => e` takes one and returns a function, at `A -> B -> C`, and the two types differ.
A call passes all of a telescope's arguments together: `f a b` is one call, and `(f a) b` applies what `f a` returns,
which for a function of two parameters is refused as too few arguments.
Likewise, a function checked against an expected type groups its parameters as the type does:
the grouped function at `A -> B -> C` is refused for taking more parameters at once than its type,
and the nested one at `pi (_ : A) (_ : B) . C` for taking fewer.
Every argument is evaluated, left to right, before the body runs,
whereas a curried function runs what lies between its parameters as soon as the first arrives.
The grouped form is the shape a call-by-push-value library has, so such a function crosses the block's boundary
in either direction without an adapter.

An authored classifier is **pushed inward** rather than compared with what the body produced.
That is what a source ascription is for: `(5 : Int64)` has to reach the literal,
because a literal's representation is resolved when it is checked and cannot be corrected afterwards.
The same expectation supplies an abstraction's domain, so `((fn x => x) : Int -> Int)` needs no binder annotation,
while `fn x => x` alone still does — there is nothing to take the domain from.
A type binder's kind is supplied the same way: an expected quantifier gives a type abstraction its kind,
and a kind known first — a binding's declared kind, a quantifier's binder at an instantiation,
an existential's binder at a pack, a manifest existential's binder at its definition, an operator's domain —
gives a type operator its own, so `let F : VType -> VType = fn X => X * X` is read as the core reads it.
An expectation that no clause could use is checked against the whole term instead,
so `((fn (x : Int) => x) : Int)` is a mismatch rather than an unchecked abstraction.
An application's expectation describes its *result*: recovering a function's type from its argument
and its result is inference, not analysis, so `(((fn x => x) 5) : Int)` is refused.

A term hole `_` is a typed hole, as in the core: checking reports the type it stands at, and running refuses it.
That type is the translation of the source type expected, so a hole
where `Int -> Int` is expected reports `Thk (Int -> Ret Int)`; a hole needs an expectation,
since with none its report would say nothing.
`@[doc]`, `@[debug]`, and `@[format]` are transparent inside a block,
and `@[debug]` prints the core term the part it annotates became, rather than the whole block's;
every other annotation is refused.

**Analysis stops at the block's boundary.** Every expectation comes from inside,
and the enclosing term's expectation is reconciled with what the block synthesized:
`(@[cbv] (fn x => x) : Ret (Thk (Int -> Ret Int)))` does not reach the binder,
although the same ascription written inside the block does.
A block synthesizes independently and the enclosing term compares afterwards,
which is one direction in types as the translation is one direction in terms,
and it is what makes a block's classifier a consequence of its body rather than of its position.

### Block Scoping, Type Aliases, and Kind Aliases

A call-by-value block is also a block in the core's sense,
so a [`that` binding](../bindings-and-scope.md#mobile-bindings-and-blocks) written inside one contributes
to it and not to the block around it, and the names it introduces end at its boundary.
That is what lets a call-by-value block be read as one closed source term.
Inside it the core's rule still holds: a `that` binding belongs to the nearest enclosing block,
the call-by-value block itself unless a `begin … end` is nearer, and name resolution lifts it there.
One written in a lambda's body is therefore evaluated once, at that block's level, rather than at each call,
and cannot see the lambda's parameter; a `begin … end` around the body keeps it inside.
A `param p : A that e` is lifted the same way and read as the call-by-value abstraction `fn (p : A) => e`
over the rest of its block, so a call-by-value block holding one still returns a value.
`@[cbv] (param (x : Int) that x)` has type `Ret (Thk (Int -> Ret Int))`, not `Int -> Ret Int`.

A block may also **name a source type**, written `let X : K = A`, and the declared kind is required.
The core decides whether a binding binds a type or a term by synthesizing its bindee and reading off the sort,
which is not available here: a source type is not a core type, so synthesizing one can fail rather than answer,
and the kind is what settles it before either judgment runs.
A type binding emits nothing — the name is inlined, so the block's computation is its tail's.

This is also how a block names a source arrow in its own spelling.
`Int -> Int` is ill-kinded in the core, so no core binding can stand for it as written,
while `let Fn : VType = Int -> Int` makes `Fn` denote `Thk (Int -> Ret Int)`.
A classifier may then use `Fn`; `Thk` written by hand stays refused.

A block names a **kind** as the core does, `let K = VType -> VType`,
and binds `VType` itself the same way, `let VType = @(intrinsic(vtype))`.
A kind has no classifier to declare, and none is needed: no kind is a source term, so the bindee decides,
when it is an intrinsic for `VType` or `CType`, a kind arrow, a named kind, or a kind name the block bound.
A kind binding emits nothing either.

### Data, Codata, and Recursion

A block may use a **data type**, and construct and match on its values.
It may write one in place and bind it with a kind, `let Opt : VType = data | +None : Unit | +Some : Int end`,
and its arms are then source types, translated like any other.
The result is transparent, and the core compares transparent data by structure,
so a value of the same declaration built outside the block is a value of the block's type.
A `let` is not recursive, so a declaration that mentions itself is an unbound name;
a recursive data type is a sealed declaration, `def`, as described
in [Sealed Declarations and Mutual Recursion](#sealed-declarations-and-mutual-recursion).
A constructor cannot synthesize — nothing about `+On` says which declaration it belongs to — so it takes its type
from the expectation, and an application pushes its domain into its argument for exactly that reason.
A match evaluates its scrutinee before examining it, which is what every positive elimination does
in the core: `do s <- e'; match s …`.
An arm's binder is a constructor tag over a plain binder, which is the one refutable shape the source language has,
or a plain binder alone, which catches what the earlier arms did not; exhaustiveness and the other rules
in [Patterns and Coverage](../patterns-and-coverage.md) apply unchanged.

A block may also use a **codata type**, build one with `comatch`, and observe it.
It may write one in place, `let P : VType = codata | .fst : Int | .snd : Int end`, or declare a sealed and possibly
recursive one with `def`. A codata type is negative, so its translation keeps the shift outside
and gives every field a returner: `codata {.d : A}` becomes `Thk (codata {.d : Ret A'})`.
A source codata type is therefore a value type, and an ascription `: CType` written around one is refused.
Two consequences follow, and both are visible to an author.

First, what a declaration may seal is the **codata type**, not the thunk of it,
because the translation of a codata value is a thunk and a thunk cannot be given a seal's identity.
Where `data` works behind `def` — a constructor's own type *is* the seal —
a codata needs the seal one layer in, and a block's own `def` puts it there.
One observation per clause is admitted, and an interleaved copattern such as `.at n` is refused rather
than un-interleaved.

Second, a field is a suspended computation, so it runs when observed and runs again on the next observation:
admitting codata puts a **by-name** corner inside a call-by-value source language.
A field that diverges and is never observed never runs, which is what makes the choice observable without effects.

A block may define a **recursive function**, with the core's existing `let fix` header:
`let fix f (x : A) : B = M in N`, where `A` and `B` are source types and a recursive call is written `f x`.
The `Thk`, the `Ret`, and the force are the translation's, and no coercion is needed anywhere:
`A -> B` already translates to a thunk type and the core's `fix` binds a thunk,
so the source's recursive name maps straight onto the `fix` binder.
The signature is settled before the body is read, and the body is checked against the declared return type rather
than against the expectation around the binding.
Several parameters are one function: `let fix f (x : A) (y : B) : C = M` binds `f`
at `pi (_ : A) (_ : B) . C`, and a recursive call passes both.
Any parameter may be a type, read as a quantifier in the same telescope, and since the recursive name stands
at the whole quantified signature, a recursive call may instantiate it at another type:
`let fix pick (X : VType) (n : Nest X) : X = … pick (X * X) m …` recurses through a nested data type.
This is polymorphic recursion, and it needs no inference, because the signature is written.
`fix` written directly is refused, with one exception the recognition cannot currently close:
`let ! f : pi (x : A) . B = fix g => fn (x : A) => M` desugars to the shape a `let fix` header produces,
so it is read as one.
Its signature is checked against its parameter, so the spelling cannot declare a type it does not have;
distinguishing the two entries would mean carrying the binding's flavor past desugaring, which erases it.

Functions that call one another are not a group of `let fix` bindings, which would be a value cycle,
but a single `let fix` returning a record of them, as the core takes a fixed point of codata.
A call between them goes through the record, and the header's parameter is the one every `let fix` has:
`let fix parity (u : Unit) : Parity = comatch | .even => fn (n : Nat) => … (parity ()).odd m … end`.

Data and recursion together are what let a block compute: a declaration
with a decreasing structure makes a terminating recursive function writable with nothing else.
Arithmetic, like every operation defined outside a block, arrives as a parameter:
`param (add : pi (_ : Int) (_ : Int) . Int) that add 2 3` returns a function, which the enclosing term applies
to `int/add` directly, since the parameter's type translates to `Thk (Int -> Int -> Ret Int)`.
A curried `add : Int -> Int -> Int` would instead need `int/add` curried to `Thk (Int -> Ret (Thk (Int -> Ret Int)))`.

### Sealed Declarations and Mutual Recursion

A block declares a **sealed** type with the core's `def X : K = D`, for `D` a `data` or `codata` declaration,
possibly under type parameters: `def List (X : VType) : VType = data | +Nil : Unit | +Cons : X * List X end`.
A seal is nominal, so the only way into or out of one is the `data`
or `codata` shape [its typing exposes](../classification-and-inference.md#nominal-seals),
and a `def` whose body declares neither is refused.
Two sealed declarations of one shape are two types, where two transparent `let` declarations are one.

The seal sits where the core can meet it.
For `data` it is on the value type: `σ : K`, a seal over `D'`, and `X` denotes `σ`.
For `codata` it is on the computation type inside the thunk the translation adds, `σ : K1 -> ... -> CType`,
and `X` denotes `fn (Y1 : K1) ... => Thk (σ Y1 ...)`, so `Stream Int` translates to `Thk (σ Int)`.
A seal on the thunk would never be met, because a codata value is introduced only by a thunk,
whose type is synthesized as `Thk B` with the codata inside it.
A source codata type is a value type, so its declared kind ends in `VType`, and `def S : CType = …` is refused.
A `def` that writes no kind takes its parameters' kinds over `VType`,
so `def D (X : VType) = data … end` has kind `VType -> VType`; each parameter must then declare its kind.
A `_` in a written kind stands for the part left to inference and for nothing else:
`def D (X : VType) : _ = …` infers the result, and what is written before a `_` is read
and guarded as any kind is, so `def D : CType -> _ = …` is refused.
Each binder there is checked as a type binder is, every annotation in it included,
so the inner annotation of `pi ((X : @[typeof] outside) : VType) . _` reaches outside the block and is refused.
A kind mentions no type, so a parameter's kind that names an earlier parameter,
as in `(X : VType) (Y : X)`, is refused by sort.
An ascription inside the definition, such as `(codata … end : VType)`, is the ordinary one,
checked where the parameters outside it are bound, against the kind of what it ascribes,
so `: CType` there is refused, and so is `: X` naming a parameter.

`X` is bound before `D` is translated, so `D` may mention it, and the declaration is then a recursive group of one.
Declarations that mention one another form a single group, and the same order extends to it,
as the core's fixpoint takes it: every member's name is bound before any member's declaration is translated,
so each may mention the others.

```zydeco
def Tree : VType = data | +Node : Int * Forest end that
def Forest : VType = data | +Nil : Unit | +Cons : Tree * Forest end that
```

Binding a name needs only its kind and whether it declares `data` or `codata`,
never what the declaration says, which is why a whole group can be bound first.
Each member is its own seal, so two members of one shape are two types, and none may appear in the type of the tail.

The core admits a binding cycle only among seals with known kinds,
and for these cases a block gives the core's diagnostics, in the core's order:
a value cycle directs the author to `let fix`; a kind that is absent, as in a recursive `def`
with parameters and no written kind, or that is not a kind, as in `def N : Int = …`, is not known;
and a transparent `let` that mentions itself or another member is missing its seal.
These checks keep that order among themselves, member by member through the group, and a block makes them
for every member before applying what it alone requires of a member, such as a binder that is a variable or a wildcard.
The core checks each member's binder before it reaches the next member, so a group at fault
in both ways can report a different error first in a block than in the core.

A declaration's scope is the rest of its block, so the block's type may not mention it,
as an unpack's witness may not escape; the refusal is `tyck.escaping-declaration`.
An abstract existential value exports it, `((Opt, +Some 7) : exists (X : VType) . X)`, and a manifest one,
which would disclose it, is refused.

### Existentials, Records, and Named Fields

A block may use a **packed value**. The abstract form `exists (X : K) . A`
and the manifest form `exists (X as T : K) . A` may each be written where they are used or bound to a name,
and the binder's kind must be a source kind, so `exists (R : CType) . A` is refused.
An existential is a positive former, so its translation takes no shift and the body translates in place:
`exists X . A` becomes `exists X . A'`.
A manifest equation's disclosed witness is itself a source type and is translated,
which is what keeps the equation true on the translated side: opening the packed value shows `X` as `T'`.

A **pack** is written `(T, v)` against an expected existential, with the witness first.
It is the same syntax as a tuple, and the expected type is what decides which it is — so a pack cannot synthesize,
and `(Int, 7)` with no expectation is read as a tuple and refused for using a type as a term.
A telescope of several witnesses consumes that many leading components
and the rest form the body's product, as in the core.
A payload that is not a source value is sequenced first, so `(Int, f 7)` evaluates `f 7` and packs the result.

An **unpack** is `let (X, x) = M in N`.
Nothing about it is special to packed values: it is the ordinary binding clause,
because the bindee's type is what makes the binder open a packed value rather than match a product.
The witness it opens behaves inside `N` as a source type variable, so a source function may be written at it;
and the witness may not escape, so `N`'s own type may not mention it.
That is what makes the packed value's own operations the only way to consume its payload,
which is what an existential is for.

**Named fields** travel through the translation untouched.
A field name is not a type former: it carries no polarity, so none of the three type rows takes a shift,
and `#f :: A` becomes `#f :: A'`, a named type `(l = S)` becomes `(l = S')`, and a type projection `S/l` becomes `S'/l`.
The kind translation is the identity, which is why a *named* kind such as `#Counter :: VType` is a source kind exactly
when its payload is, and so a packed value whose telescope binds at one is admitted.
A named kind is written wherever a block writes a kind, as in `fn (X : (#C :: VType)) => fn (x : X/C) => x`.
`#C :: A` with `A` a type is the same node read as a field type, so where a kind belongs it is refused by sort.

An existential binder may name its witness's field, as in `exists (#Counter = ((Rep as Int) : VType)) . (#zero :: Rep)`:
the witness has the named kind `#Counter :: VType`, and `Rep` binds its payload.
No other type binder may: `fn (#C = (X : VType)) => M` and `forall (#C = (X : VType)) . A` are refused.
For the term-level abstraction this is forced, because the core term a block builds for it makes a plain binder
from its witness, and the field name would vanish from the type it builds.
A quantifier and a type operator keep their binder's pattern, so for them it is a restriction of the source language:
what instantiating or applying one at a named argument should do has not been specified.

A block builds a record with `(#f = v, #g = w)` against the record's declared type,
and a field whose payload is a computation is sequenced before the record is built, so the field holds the result.
It reads one either by pattern — `let (= f, = g) = r in …`, which binds each field under its own name —
or by projection, `r/f`, which synthesizes and leaves the head to be sequenced, so the head need not be a value.
The field itself is resolved by the core's resolver rather than by the block,
which is what makes a field behind a product prefix reachable and gives a missing
or ambiguous field the core's own diagnostic.

A record type is written in place as a product of field types, `(#fst :: Int, #snd :: Int)`,
and is structural: a core record and the same type written in a block are one type.
A pack's witness is written in place too, so `(((#Counter = Int), #zero = 0) : Pkg)` needs no outside name
for `(#Counter = Int)`.

### Composing with Monadic Blocks

Effects arrive the same way as outside operations, as parameters, and a monadic block is what supplies them.
A returner is observably pure by the core's [convention](../computations-and-control.md#ret-and-explicit-cps),
so a call-by-value block alone has no effect but divergence, which cannot tell the function-before-argument
and left-to-right orders of the term rows from any other.
Nested in [`@[monadic]`](monadic-blocks.md), a call-by-value block is translated twice:
the monadic block reinterprets every `Ret` and `do` the call-by-value block emitted through its carrier,
so an arrow's `Thk (A' -> Ret B')` becomes a thunk of a function returning through the carrier,
with no row of the call-by-value translation changed.
The call-by-value block abstracts over the operations it uses, and the monadic block supplies them:

```zydeco
@[monadic] begin
  fn (put : Thk (Int -> Ret Unit)) =>
    do h <- @[cbv] (fn (a : Unit -> Int) => fn (b : Unit -> Int) => (a (), b ()));
    do h <- ! h { fn (u : Unit) => do _ <- ! put 1; ret 0 };
    do _ <- ! h { fn (u : Unit) => do _ <- ! put 2; ret 0 };
    ret 0
end
```

Under `State Int` this ends in state `2`, because the tuple runs `a` before `b`;
with the tuple written `(b (), a ())` it ends in `1`, and an application likewise runs its function before its argument.
Quantifiers, packed values, type operators, records, codata, transparent data, and `let fix` compose the same way.
So does a sealed type the call-by-value block declares, which the monadic block's
[applicative nominal translation](monadic-blocks.md#applicative-nominal-translation) carries along,
so a call-by-value block nested in a monadic block can recurse over a data type it declares.

## Call-by-Name Blocks

`@[cbn] e` is reserved for reading `e` as a call-by-name expression and elaborating it into the core,
following Levy's call-by-name translation as `@[cbv]` follows his call-by-value one.
It is not implemented, and it is refused during desugaring rather than ignored:
an [unrecognized meta annotation](../meta-annotations.md) is accepted with no effect,
so without the reservation a body written for call-by-name would run as plain call-by-push-value, with no diagnostic.
