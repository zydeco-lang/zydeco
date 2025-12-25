## Why Product Syntax should be Right-Associative

N-nray products can be desugared into binary products, but to which direction? I think right-associative works better, and I have two reasons.

First, being right-associative enables a local-style desugaring of projection. Consider a 3-tuple:

```
(0, 1, 2) => (0, (1, 2))
.0 => .0
.1 => .1.0
.2 => .1.1
```

A part from the last bit, it's a direct traslation into the number of `.1`s.
It will work even better if we drag nullary-product (unit) into the picture.

```
(0, 1, 2) => (0, (1, (2, ())))
```

Then the nth-projection will just be written as `(.1)`^n`.0`. Easy. In comparison, the left-associative approach will make the desugaring dependent on the length of the product, which is not ideal.

By the way, nullary products can also give rise to unary product syntax `(0,) => (0, ())`, where `,` is really what makes a product term looks like a product, so `(0, 1)` should be normalized into `(0, 1,)`. Maybe `(,0 ,1)` also works. Oops, off-topic.

Second, it works better with existential types. The left hand side is the witness/parameter, and the right hand side is the body/interface. The left-associative approach doesn't make sense anymore because the left hand side is a type term / type pattern, which is usually not a product itself. However, there should not be a nullary or unary form of existentials, since `(0, 1, 2) => (0, (1, (2, ())))` will completely change whether `2` from a body to a witness.

## Why Products should not be Desugared into its Binary Form

Because nullary and unary cases will always be special cases and will be very difficult to work with if we follow this desugaring design choice. Instead, we can parse product term syntax as `Vec`s and translate nullary case into `triv` (term of unit type), and unary case into a normal AST node, leaving products that are at least binary.

Unary product is similar to precedence parentheses in syntax, making it difficult to distinguish. And all mechanisms that unary product brings can (arguably) be replaced by other devices, like `box`ing (heap allocation), `align`ing (memory layout), or `data` type (type-dependent implementations; newtype pattern).
