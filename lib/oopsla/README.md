# Overview

This document is a high-level overview of the artifact for our OOPSLA paper "Notions of Stack-Manipulating Computation and Relative Monads" by Yuchen Jiang, Runze Xue, and Max S. New.

1. [Introduction](#introduction)
2. [Hardware Dependencies](#hardware-dependencies)
3. [Getting Started Guide](#getting-started-guide)
4. [Step-by-Step Instructions](#step-by-step-instructions)
5. [Reusability Guide](#reusability-guide)



## Introduction

The artifact contains the implementation of Zydeco, a call-by-push-value (CBPV) calculus with executable examples from the paper. The artifact supports the following claims of the paper.
- We demonstrate that relative monads can model common stack-manipulating computations used in functional programming
- We described a generalized do-notation called "monadic blocks" that enables embedded CBPV programming by reinterpreting code to use any provided relative monad.
- We showed that the monadic blocks allow for the automatic extension from any user-defined relative monad to a monad transformer.
Further details are provided in the [Step-by-Step Instructions](#step-by-step-instructions) section.

## Hardware Dependencies

The artifact doesn't require any specific hardware dependencies. The artifact is tested on a Linux machine, a macOS machine, and a Windows machine. Broadly speaking, the artifact is expected to work on any machine that has tier 1 or tier 2 support by the Rust compiler.



## Getting Started Guide

This section prepares the reviewers to evaluate the artifact. Delivered as a docker image, the reviewers are encouraged to pull the image from Docker Hub and run it in a container.

1. Access the artifact by pulling the docker image and run it in a container
```sh
docker run -it lighghteeloo/zydeco:amd64
```
If you are on a machine with an ARM architecture, you can pull the image from Docker Hub by running the following command:
```sh
docker run -it lighghteeloo/zydeco:arm64
```
A shell will then be spawned in the container at `/usr/src/zydeco`, where a copy of this repository is also located.

2. Run the tests on Zydeco inside the docker container
```sh
cargo test --release
```
The tests perform basic sanity checks on the implementation of Zydeco, as well as all the core features mentioned in the paper.

## Step-by-Step Instructions

This section shows how our artifact supports the claims made in the paper, and how the reviewers can use the artifact to run the examples and write their own Zydeco programs to test the claims. In the following sections, we first describe Zydeco as a functional programming language in a high-level manner through examples, and then briefly describe its implementation. After that, we describe relative monads and their algebras in Zydeco and demonstrate how to implement them in Zydeco and apply them to effectful programming. Finally, we show how we can further extend it to support monadic blocks, and how to automatically derive relative monad transformers using monadic blocks in Zydeco.


### Running the examples

All examples are located under the `lib/oopsla` directory and listed in the `lib/oopsla/proj.toml` file.

We provide an interactive and integrated script to run all the examples. Under the root directory of the repository, run the following command
```sh
./run.sh
```

Alternatively, the reviewers can manually run the examples using the `run` sub-command.

```sh
zydeco run lib/oopsla/proj.toml --bin=<name>
```

For example, to run the first example `polynomial`, run the following command:

```sh
zydeco run lib/oopsla/proj.toml --bin=polynomial
```


### VSCode Extension

Zydeco is optionally syntax-highlighted in VSCode. To install the extension, open the VSCode Extensions tab and search for "zls". 



### Zydeco as a Functional Programming Language

In the following sections, we'll first introduce the grammar of Zydeco and then show example Zydeco programs in the paper.

In Section 3 of the paper, we illustrated the abstract syntax and semantics of Zydeco. Specifically, Figure 2 through 6 shows the syntax and semantics of Zydeco. The following paragraphs briefly show the corresponding concrete syntax of Zydeco.
The core standard library less than 200 lines of code is located in [`core.zydeco`](core.zydeco). After reading through this section, the reader is encouraged to browse through the file to understand all the core features of Zydeco.

#### Comments

Zydeco supports both line comments and block comments.

+ line comments: `--`
+ block comments: `/-` and `-/`

#### Kinds
`<K>` is used to denote kinds.

+ value kind: `VType`
+ computation kind: `CType`
+ arrow kind: `<K> -> <K>`
  + kind of `Either` type: `VType -> VType -> VType`
  + kind of relative monads: `VType -> CType`

#### Types
`<A>` is used to denote value types, and `<B>` is used to denote computation types; `<T>` is used to denote types.
Conventionally, PascalCase is used as type variables, but it's not enforced.

Type abstraction: `fn (X: <K>) -> <T>`
Type application: `<T> <T>`

The primitive value types are:
+ thunk type: `Thk <B>`
+ unit type: `Unit`
+ product type: `<A> * <A>`
+ existential type: `exists (X: <T>) . <A>`
+ integer type: `Int`
+ character type: `Char`
+ string type: `String`

User-defined data types are also value types. We'll see them in the abstract machine interpreter example below.
The following is an example of the `Either` type.

```zydeco
fn (E: VType) (A: VType) ->
  data
  | +Left : E
  | +Right : A
  end
```

The primitive computation types are:
+ return type: `Ret <A>`
+ function type: `<A> -> <B>`
+ forall type: `forall (X: <T>) . <B>`
+ OS type: `OS`

User-defined codata types are also computation types. We'll see them in the calling convention example below.

#### Values and Computations
`<V>` is used to denote value terms, and `<M>` is used to denote computation terms.

The value terms span the following forms:
+ all variables are values.
+ thunk: `{ <M> }`
+ unit: `()`
+ product: `(<V>, <V>, ...)`
+ pack: `(<T>, <V>)`
+ integers: `1`, `-1`, ...
+ characters: `'a'`, `'b'`, ...
+ strings: `"hello"`, `"world"`, ...
+ data constructors: `+Ctor <V>`

Their elimination forms are computations:
+ forcing a thunk `v`: `! v`
+ unit `v`: `let () = v in <M>`
+ product `v`: `let (v1, v2, ...) = v in <M>`
+ pack `v`: `match v | (t, x) -> <M> end`
+ data constructor `v`: `match v | +Ctor1 x -> <M> | +Ctor2 x -> <M> | ... end`

Other computation terms are:
+ return: `ret <V>`
+ do-binding: `do x <- <M>; <M>`
+ term function: `fn (X: <T>) -> <M>`
+ type function: `fn (X: <K>) -> <M>`
+ function application: `<M> <V>` and `<M> <T>`
+ comatch (terms of codata types): `comatch | .dtor1 -> <M> | .dtor2 -> <M> | ... end`
+ codata destructor: `<M> .dtor`

Primitive functions that deal with integers, characters, strings, and operating system features are located in `lib/oopsla/core.zydeco`.

The main program is required to have type `OS`. A few crucial primitive functions related to the `OS` type are:
+ `exit: Thk (Int -> OS)`: constructs a computation that exits the program with the given exit code.
+ `write_str: Thk (String -> Thk OS -> OS)`: takes a string and a continuation of type `OS`, prints the string and then runs the continuation.
What `OS` type actually means is a computation that can run on the operating system stack and consume it. All Zydeco features that interact with the operating system are implemented using this type.


### The Implementation of Zydeco

To demonstrate that Zydeco is usable as a functional programming language, we provide a Rust implemenation of Zydeco System-Fω-style type checker, a small-step interpreter, and a minimal standard library [`core.zydeco`](core.zydeco).

The implementation of Zydeco is located in the `lang` directory in a modularized manner. Among all components, the most crucial modules are:

+ `lang/syntax`: defines the general syntax of Zydeco that are used in all passes
+ `lang/surface`: defines the surface syntax of Zydeco as well as the lexer, the parser, the desugaring pass, and the name resolution pass
+ `lang/statics`: implements the static semantics of Zydeco with a type checker; the monadic blocks are implemented as a type-directed source-to-source translation during type checking
+ `lang/dynamics`: implements the operational semantics of Zydeco with a small-step interpreter
+ `lang/driver`: includes utilities for simple Zydeco package management and driver for the pipeline


### Programming with the Stack in Call-by-Push-Value

In Section 2 of the paper, we demonstrated how to use Zydeco to perform stack-manipulating computations. All examples are included in the `lib/oopsla` directory.

#### Implementing a Polynomial Function
The first example [`polynomial.zydeco`](polynomial.zydeco) is a simple function that computes the polynomial `f(x) = x^2 + x + 10`.
```zydeco
fn (x: Int) ->
  do s <- ! times x x;
  do y <- ! add x 10;
  ! add s y
```
The first feature we may notice is **type annotations**, denoted by `:`. We are going to use them throughout the examples to show the types and kinds of the all programs.

`fn` is the keyword for function. `do x <- ...; ...` is the monadic bind operator, syntactically similar to the OCaml-style monadic `let*` binding. All computations in Zydeco that returns an integer will have type `Ret Int`, and the result can only be accessed with the `do` operator. Therefore, both `! times` and `! add` computations have type `Int -> Int -> Ret Int` and the above program overall has type `Int -> Ret Int`. The force operator `! ...` runs the thunk. Thunks are typed as `Thk ...` and are written as `{ ... }`, so we have `times: Thk (Int -> Int -> Ret Int)` and `add: Thk (Int -> Int -> Ret Int)`, and forcing them will remove the thunks wrapping them.

Zydeco syntactically disambiguates between values and computations. Values don't run or have side effects, and computations do. To briefly recap from the paper:

+ Functions are computations because they push and pop values, interacting with the stack, thus they have side effects.
+ Thunks are values because they suspend computations, and they will not run until the force operator `!` is used.
+ Return values are computations because they pop a continuation from the stack and pass the result to it; the continuation is corespondingly created by the `do` operator.
+ Lastly, variables are themselves values, because references to variables don't have side effects.

The top-level in Zydeco are either declarations, definitions, or the program entry point `main`. To wrap the function above into a definition that can be reused, we'll make it a definition.

```zydeco
def poly = {
  fn (x: Int) ->
    do s <- ! times x x;
    do y <- ! add x 10;
    ! add s y
} end
```

Here, `def x = ... end` is the syntax for definition. As mentioned, since variable `poly` is a value, and the function is computation, we need to wrap the function in a thunk using `{ ... }` and then assign it to `poly`.

To make the example above into a runnable Zydeco test case, we'll wrap it in `main`, and use the exit code `0` to indicate whether the test passes.

```zydeco
main
  -- the exit code should be 0 for the test to pass
  do r <- ! poly 10;
  -- since f(10) = 120, we minus the result by 120 to get 0
  do e <- ! sub r 120;
  ! exit e
end
```

#### Complex Calling Conventions

We include three complex calling conventions in [`cc.zydeco`](cc.zydeco). Specifically, we define the following variant function types:

+ `FnOpt (A: VType) (B: CType)`: function type with optional argument (`A ->? B`)
+ `FnVar (A: VType) (B: CType)`: variadic function type (`A ->* B`)
+ `FnVarOnce (A: VType) (B: CType)`: variadic function type with at least one argument (`A ->+ B`)

The reader is encouraged to open the file and read the comments to understand the implementation.

The file also contains a simple use case of the complex calling conventions demonstrated in the paper named `sum_and_mult`. The reader is encouraged to open the file, run the example, and understand how it works though detailed comments.

#### Abstract Machine Interpreter

We provide a call-by-value abstract machine interpreter in [`cbv.zydeco`](cbv.zydeco), resembling the example from Figure 1 in the paper. A small difference is that we use a `codata` definition to encode the mutually recursive functions `descend` and `ascend`:

```zydeco
codata Interp where
| .descend : Expr * Env -> Machine
| .ascend  : Value -> Machine
end
```

Zydeco doesn't directly support mutually recursive function definitions, however, they can be encoded using codata types. If the reader is interested in such encoding, please refer to the definition of `interp` and how it's used in the source code of this example, which demonstrates a general pattern that resolves the issue.


### Relative Monads in Zydeco

In Section 4 of the paper, we demonstrated how to define relative monads in Zydeco.
Here, we demonstrate how the concept of relative monad can be defined in [`core.zydeco`](core.zydeco).
Three exception monads are defined in [`exn.zydeco`](exn.zydeco).
Furthermore, common data structures that are relative monads are defined in [`monads.zydeco`](monads.zydeco).
And finally, the free monad is defined in [`free.zydeco`](free.zydeco).

Detailed comments are provided in the source code in each example.

#### Relative Monads

The relative monad is defined as a codata type in Zydeco:

```zydeco
alias Monad (M: VType -> CType) : CType =
  codata
  | .return : forall (A: VType) .
    A -> M A
  | .bind : forall (A: VType) (A': VType) .
    Thk (M A) -> Thk (A -> M A') -> M A'
  end
end
```

#### Exception Monads

As shown in Figure 7, we implement three exception monads in [`exn.zydeco`](exn.zydeco), along with the proof of the violation of the monad laws (shown in Definition 4.1) of the defunctionalized exception monad because the exposure of the low-level details of the stack.

#### Continuation and State Monads

The monads shown in Figure 8 are implemented in [`monads.zydeco`](monads.zydeco). We also implement an I/O monad to show the generality of our design of relative monads.


### Monadic Blocks and the Algebra Translation

In Section 5 we introduced the monadic blocks as generalization of the do-notation. We implement a source-to-source translation called "algebra translation" that works during the type checking phase to traverse the program inside the monadic blocks and generate structures to allow the piece of code to work with the user-specified ambient monad. We also introduce algebras of relative monads in Zydeco and how to extend algebras on all type (constructors), which is crucial for the implementation of monadic blocks. To observe the code generated by monadic blocks, we provide tools for the reviewer to check the result of the algebra translation to witness the correctness of our implementation. As an application of monadic blocks, we show how relative monad transformers can be automatically derived from user-implemented relative monads in Zydeco.

#### Algebras of Relative Monads

The definition of algebras of relative monads is shown in Definition 5.1. In Zydeco, we define the interface for algebras of relative monads in [`core.zydeco`](core.zydeco) as follows:

```zydeco
alias Algebra (M: VType -> CType) (R: CType) : CType =
  forall (A: VType) . Thk (M A) -> Thk (A -> R) -> R
end
```

In the paper, we claimed that the algebras of relative monads extend to all types (constructors). To demonstrate, we manually implements crucial algebras listed in Section 5.1 of the paper in [`algebra.zydeco`](algebra.zydeco). Later on we'll demonstrate a systematic approach to derive algebras of relative monads in [Algebra Translation](#algebra-translation) section.


#### Monadic Blocks

The monadic blocks, defined in Section 5.2, allow the user to create a dialect of Zydeco that uses a user-specified ambient monad inside the monadic blocks. A monadic block accepts a Zydeco computation term and produces a computation term that accepts a monad instance as function argument. The syntax of monadic blocks looks like the following:

```zydeco
monadic
  ret ()
end
```

where `ret ()` is just an arbitrary computation. The whole block will then be translated into a computation that overloads the ambient monad inside the monadic block.

```zydeco
fn (M: VType -> CType) (mo: Thk (Monad M)) ->
  ! mo .return Unit ()
```

The reader may have noticed that the monad type and its implementation are not demanded on site; instead, they can be later passed in as function arguments, making the whole setup more flexible to the user. We can provide all the necessary interface as function arguments into the monadic blocks in a similar fashion. For example,

```zydeco
monadic
  fn (E: VType) (raise: Thk (forall (A: VType) . E -> Ret A)) ->
    ...
end
```

In such way, the user can require a monad instance that supports `raise`, and later pass in the `Exn` monad instance to the monadic block.

A caveat is that the monadic blocks don't naturally support any reference to variables defined outside the monadic blocks. Only primitive CBPV constructs like `VType`, `CType`, `Thk`, `Ret`, units and products, functions, and exists and forall types are allowed inside the monadic blocks, noticably excluding all abstract primitive types like the `String` and `OS` type. All other terms used in the monadic block must also be passed in. Below is an example of a monadic block that raises an exception:

```zydeco
monadic fn (Str: VType) (raise: Thk (forall (A: VType) . Str -> Ret A)) (msg: Str) ->
  do x <- ! raise Unit msg;
  ret x
end Exn mo-exn String triv { ! exn-raise String } "error"
```

Observe how the implementations are passed in as function arguments in the last line. `Exn` and `mo-exn` are the monad type and its implementation, respectively. `String` and `triv` instantiate type `Str` and its algebra (introduced in the next section). `{ ! exn-raise String }` is the implementation of the `raise` function, specialized to the `String` type. `"error"` is the message to be passed to the `raise` function.


#### Algebra Translation

To implement the monadic blocks, in the paper we introduced the algebra translation in Section 5.3. The corresponding implementation is located in [`lang/statics/src/monadic.rs`](../../lang/statics/src/monadic.rs). To briefly summarize,

+ The signature translation in Figure 20 is implemented in the function `signature_translation`
+ The carrier translation in Figure 21 is implemented in the function `type_pattern_translation` and `type_translation`
+ The structure translation in Figure 22 is implemented in the function `structure_translation`
+ The value term translation in Figure 23 is implemented in the function `value_translation`
+ The computation term translation in Figure 24 is implemented in the function `value_pattern_translation` and `computation_translation`
+ The monadic block translation in Figure 25 is implemented through direct invocation of the above functions during the type checking phase in [`lang/statics/src/tyck.rs`](../../lang/statics/src/tyck.rs)

#### Using *Global* Types and Terms in Monadic Blocks

In the paper we claimed that the monadic blocks are required to be closed, while in the artifact we slightly improve it. As a programming convenience to allow for more code reuse, Zydeco's monadic blocks allow for some limited use of definitions outside the block, whereas in the paper, code inside a monadic block must be closed. We define a type or a term to be "global" when it is well-kinded/typed only using other global types and terms. Closed types and kinds are global, as well as types and kinds that only reference other globally defined types and terms. In Zydeco, the code inside a monadic block doesn't need to be closed, but instead can make use of global definitions. This can be implemented (somewhat inefficiently) as inlining the used definitions into the block, so this feature does not increase the expressive power of monadic blocks, but makes them much more convenient to use.

When global types and terms are referenced inside the monadic block, they have a different meaning than when they are referenced outside the blocks, because the ambient monad of the global type or term is now overloaded by the monadic block. When the monadic block undergoes the algebra translation, the global type or term will be translated to use the user-specified ambient monad.

As an example of using global types and terms in monadic blocks, we can define an identity function:
```zydeco
monadic
  ! { fn (A: VType) (x: A) -> ret x }
end
```
and observe how we can move the definition out of the monadic block:
```zydeco
let id = { fn (A: VType) (x: A) -> ret x } in
monadic
  ! id
end
```

The reason why we can do this is because the definition of `id` is global, and therefore its ambient monad can be reinterpreted according to the surrounding monadic block.

For a more realistic use case, refer to [`exnt.zydeco`](exnt.zydeco) and observe that variables `Exn` and `mo-exn` are directly referenced in the monadic block. Given the definition of the `Exn` type
```zydeco
alias Exn (E: VType) (A: VType) : CType =
  Ret (Either E A)
end
```
In the definition of `Exn`, `Either` is a global type, therefore we can use `Exn` as a global type inside the monadic block, but keep in mind that the meaning of `Ret` type will be overloaded by the monadic block. Such overloading is the reason why we can derive relative monad transformers from a relative monad instance, which is itself defined as a global Zydeco program. Without this convenience, we would have to inline the definition of the monad inside the block.

#### Deriving Relative Monad Transformers

We generate the relative monad transformer implementations for the `Exn` and `ExnK` monads, and compare to our manual implementations ([`exnt.zydeco`](exnt.zydeco) and [`exnkt.zydeco`](exnkt.zydeco)) to check their correctness. The `@[debug(...)]` macros are used to print the generated code to the console during type checking. The reader may look into the source code and read the comments to understand the details.


## Reusability Guide

The artifact is designed to be reusable by other researchers and practitioners. Overall, it contains the following components:
+ The source code of `zydeco` can be compiled to interpret Zydeco programs and perform algebra translation.
+ The `core.zydeco` and `data.zydeco` files implement a minimal standard library for Zydeco.
+ Examples listed under [lib](../) can be used as references to understand the implementation of Zydeco. They are also a good starting point to write new Zydeco programs, either as a library or as a starting point for standalone executables.



-- In the Reusability Guide, explain which parts of your artifact constitute the core pieces which should be evaluated for reusability. Explain how to adapt the artifact to new inputs or new use cases. Provide instructions for how to find/generate/read documentation about the core artifact. Articulate any limitations to the artifact’s reusability.

### The `zydeco` binary

When using `zydeco` as binary, it's recommended to build it under the `release` profile.

```sh
cargo build --bin=zydeco --release
```

Once the build is complete, the `zydeco` binary will be located at `target/release/zydeco`. It supports the following sub-commands:

+ `run`: run a Zydeco program or a Zydeco project
+ `check`: check a Zydeco program or a Zydeco project
+ `help`: print help information








