# Values, Products, and Data

Values include variables, literals, `()`, products, named values, constructor applications,
thunks, existential values, and total value functions.
`() : Unit`; `(v)` is grouping; `(v1, ..., vn)` introduces a product or,
under an expected existential type, a witness-prefixed value.
`A * B * C` has three components and differs from `A * (B * C)`.

`#field = v` introduces a named payload of type `#field :: A` when `v : A`.
`= x` puns `#x = x` in terms and patterns.
Named types work analogously: `(#item = T) : (#item :: K)`.
Named computations have no introduction form; store one as a thunk.

```zydeco
let point = (#x = 3, #y = 4) in
ret (point/x, point/y)
```

A data type `data | +C1 : A1 ... | +Cn : An end` classifies constructor values `+Ci vi`,
where `vi : Ai`. Each constructor has one payload, which may itself be a product or constructor.
Names are interpreted against the expected data type.
Name and parameterize a data type with an ordinary definition.

```zydeco
let Unit = @(intrinsic(unit)) in
def Flag = data | +On : Unit | +Off : Unit end in
match (+On() : Flag)
| +On() => ret 1
| +Off() => ret 0
end
```
