# Intro to Zydeco
## type (value and computation)
some simple example
## Thunk and Ret
When we give some computation types `B` a `Thunk` (`Thunk(B)` or `U(B)`), it means that we suspend it and use it as a value type. 
When we stop computing and want to return the result (in a value type `A`), we need to transform it into computation types using `Ret` (`Ret(A)` or `F(A)`).
Here's an example of seperating a simple built-in `add` function into two parts:
```
let add_in : U(Int -> Int -> F(Int)) = {
    fn (x : Int) ->
        fn (y : Int) ->
            ! add x y
};
let add_out : U(Int -> F(Int)) = {! add_in 10};
do res <- ! add_out 20;
! write_int_line res {! exit 0}
```
## OS (Operating System)
The main expression has the built-in type of `OS`, especially when we do some staff with `IO`. The idea of kontinuation requires programmers to specify what the `OS` looks like after the program reads or writes something. Here are some examples:
```
pub extern define write_line : Thunk(String -> Thunk(OS) -> OS);
pub extern define read_line : Thunk(Thunk(String -> OS) -> OS);

# outputs "hello world" and then exit with code 0
! write_line "hello world" {! exit 0}

# echo what users just input in an infinite loop
let rec loop : OS = ! read_line { fn (str : String) -> ( ! write_line str {! loop} 
  )
};
! loop

# echo what users just input in an infinite loop until the input string is "exit"
let rec loop : OS = ! read_line { fn (str : String) -> (
  do b <- ! str_eq str "exit";
  match b
  | True() -> ! write_line str {! exit 0}
  | False() -> ! write_line str {! loop}
  )
};
! loop
```
## data (and match)
We can define `union type` or `recursive type` as follows:
```
# Non-recursive
data Weather where
  | Sunny()
  | Rainy()
  | Windy()

# Recursive
data ListInt where
  | NoInt()
  | Cons(Int, ListInt)

# Here's a function print every element in the ListInt seperated by a ' '
# Notice that for each possible branch, the type after "->" must be the same
let printListInt = {
  rec (printReal : U(ListInt -> OS)) ->
    fn (myList : ListInt) ->
      match myList
      | NoInt() -> ! exit 0
      | Cons(x, xs) -> (
        do wx_ <- ! int_to_str x;
        do wx <- ! str_append wx_ ' ';
        ! write_str wx {! printReal xs}
      )
};
```

## codata (and comatch)
If we consider functions as computations, we can use `codata` to simulate the process of calling functions. We take a value type `A` and return a computation type `B`. The `codata` type itself is a computation type.
For example, when we try to calculate the sum of a list of number recursively, we can simulate the construction of stack model and execute the computation by destructing the stack. The existence of `codata` helps label different kind of stacks and indicate when the computation stops.
```
# When adding a list of numbers, the process should be either finishing or keeping adding numbers
codata Summer where
  .done()    : Ret(Int) 
  .addN(Int) : Summer

# Since Summer includes the return type Ret(Int), it can be used directly instead of using the original return type.
let rec retSummer : Int -> Summer =
  fn (n : Int) -> comatch
      | .done()  -> ret n
      | .addN(x) ->
      (do n' <- ! add n x;
          ! retSummer n');

let rec sumOk : NumList -> Summer =
  fn (xs : NumList) ->
    match xs
    | Empty()     -> ! retSummer 0 # When none of the elements remains, add a zero(.done()) at the end of stack
    | Cons(x, xs) -> ! sumOk xs .addN(x); # As we call sumOk recursively, the label of .addN(x) is appended at the end of stack until xs is empty
```

