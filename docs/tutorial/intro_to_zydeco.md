# Intro to Zydeco
## Types
Zydeco has two kinds of types: value type (`A`) and computation type (`B`). 

A value type classifies inert data and a computation type classifies programs that compute things.

Value type can be: 
- `X`: type variable defined using `data`.
- `Int | String | Boolean | Unit`: basic primitive type with built-in functions.
- `Thunk Y`: suspend a computation type `Y` and consider it as a value type. 

Computation type can be:
- `Y`: type variable defined using `codata`.
- `Ret X`: `Ret X` classifies computations that return X values.
- `X -> Y`: takes a value type as an argument and return a computation type.

For `Thunk B` type, we can represent its value as `{ some_computation_value }`.

For `Ret A` type, we can represent its value as `ret some_value_type`

## Playing with REPL
Using the REPL can help us get familiar with syntax and basic idea of zydeco fast. 

Key points: `Force`, `Binding`, `Function`
```
> 1
1 : Int
> "ann arbor"
"ann arbor" : String
> True()
True() : Bool
> Unit()
Unit() : Unit
```
Basic value types can be interpreted in the ways above.
```
> add
add : Thunk(Int -> Int -> Ret(Int))
```
Now `add` has a value type of `Thunk B`, where `B` is a computation type which takes two `Int` and returns a computation type `Ret Int`.

Notice that `add` is still suspended, so if we try to apply it into practice by `add 1 2` and get the result, there will be an error.
```
> add 1 2
Parse Error: Unrecognized token `NumLiteral(1)` found at 4:5
```
In fact, we need to `force` the `Thunk` type first and then apply it. The syntax is like:
```
> ! add 1 2
3
```
`3` is the returned `Int`.

Another example is the `exit` function:
```
> exit
exit : Thunk(Int -> OS)
```
`OS` is the other type of the computation result.
```
> ! exit 0
```
The REPL will exit directly without printing anything.

We have some ways of binding a certain value to variables which can be used later. There're two main ways: `let` and `do`.

We can bind something with `value` type to a variable using `let`. 
```
> let pre = "ann " in ! str_append pre "arbor"
"ann arbor"
```

We can also define a function using `let`.
```
> mod
mod : Thunk(Int -> Int -> Ret(Int))
> let mod10 = {fn (n : Int) -> ! mod n 10} in ! mod10 54
4
```
`mod` is a built-in function and we can define a function taking an `x : Int` and calculating `x mod 10`. The type of defined function `mod10` should also be `Thunk B`. Therefore, we add `{}` at each side of the definition part.

For each `let` statement, a semicolon `;` is needed, indicating that it's not the main expression. We can add more `let` to bind more variables, but there must be a main expression at the end of the program.

Binding the result of computation to a variable using `let` is not allowed. Instead, we use `do`.
```
> do x <- ! mod 10 4; ! add x 2    
4
```
The process a `do` statement is executed is similar to the process that we call another function and a new stack frame is created. The return value of the function is binded to the variable after `do`.


## OS (Operating System)
Besides `Ret A`, main expression can also have the built-in type of `OS` which classifies computations that can be run as a process that interacts with the `OS`. The idea of kontinuation requires programmers to specify what the `OS` looks like after the program reads or writes something. Here are some examples:
```
pub extern define write_line : Thunk(String -> Thunk(OS) -> OS);
pub extern define read_line : Thunk(Thunk(String -> OS) -> OS);

# outputs "hello world" and then exit with code 0
! write_line "hello world" {! exit 0}

# echo what users just input in an infinite loop
let rec loop : OS = ! read_line { fn (str : String) ->
  ! write_line str {! loop} 
} in
! loop

# echo what users just input in an infinite loop until the input string is "exit"
let loop : OS = {
  rec loop -> ! read_line {
    fn (str : String) -> (
      do b <- ! str_eq str "exit";
      match b
      | +True() -> ! write_line str { ! exit 0 }
      | +False() -> ! write_line str { ! loop }
      end
    )
  }
} in
! loop
```
## data (and match)
We can define `union type` or `recursive type` as follows:
```
# Non-recursive
data Weather where
  | +Sunny()
  | +Rainy()
  | +Windy()
end

# Recursive
data ListInt where
  | +NoInt()
  | +Cons(Int, ListInt)
end

# Here's a function print every element in the ListInt seperated by a ' '
# Notice that for each possible branch, the type after "->" must be the same
let printListInt = {
  rec (printReal : Thunk(ListInt -> OS)) ->
    fn myList ->
      match myList
      | +NoInt() -> ! exit 0
      | +Cons(x, xs) ->
        do wx_ <- ! int_to_str x;
        do wx <- ! str_append wx_ ' ';
        ! write_str wx { ! printReal xs }
      end
};
```

## codata (and comatch)
If we consider functions as computations, we can use `codata` to simulate the process of calling functions. We take a value type `A` and return a computation type `B`. The `codata` type itself is a computation type.

For example, when we try to calculate the sum of a list of number recursively, we can simulate the construction of stack model and execute the computation by destructing the stack. The existence of `codata` helps label different kind of stacks and indicate when the computation stops.
```
# When adding a list of numbers, the process should be either finishing or keeping adding numbers
codata Summer where
  | .done()    : Ret(Int) 
  | .addN(Int) : Summer
end

# Since Summer includes the return type Ret(Int), it can be used directly instead of using the original return type.
let rec retSummer : Int -> Summer =
  fn (n : Int) ->
    comatch
    | .done   -> ret n
    | .addN x ->
      do n' <- ! add n x;
      ! retSummer n'
    end;

let rec sumOk : NumList -> Summer =
  fn (xs : NumList) ->
    match xs
    | +Empty()     -> ! retSummer 0 # When none of the elements remains, add a zero(.done) at the end of stack
    | +Cons(x, xs) -> ! sumOk xs .addN x; # As we call sumOk recursively, the label of .addN x is appended at the end of stack until xs is empty
    end
```

