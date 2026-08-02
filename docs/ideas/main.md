# What should the type of "main" be?

We should decide what effects are allowed to be performed by the
"main" expression, and at what type?

The most liberal reasonable choice would be to have the main be of
type `Ret(Unit)`, i.e., an expression that performs side-effects and
possibly returns a trivial value. All of the observable behavior of
such an expression is in its side-effects. So this design would go
well with a very liberal approach to built-in effects, such as typing
`read_line` as `Thunk(Ret(String))`.

At the other extreme, we could have a built-in computation type,
perhaps called `OS` for operating system or `IO` to match Haskell that
allows only some built-in operating system/runtime-system supported
primitives. Then the main expression would be required to have type
`IO`, so it's impossible to do write a program without some
primitives. For instance, to simply terminate the process normally we
could have `halt : IO`, to terminate with an exit code `exit : Number
-> IO`. Then `read_line` would be given the very natural type
`Thunk(String -> IO) -> IO`, an instance of the relative kontinuation
monad. Analogous to Haskell, this would probably benefit greatly from
nice support for relative monads.

Should study some more recent proposals too, such as the following,
which is related to EEC/CBPV: https://arxiv.org/abs/1910.11629
