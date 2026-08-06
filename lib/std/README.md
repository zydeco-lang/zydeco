# Zydeco Standard Library

The standard library has two boundaries. [`builtin.zy`](builtin.zy) is the typed contract between Zydeco programs
and the host runtime. Its operations expose representation-independent observations and effects, but never
construct library-defined `Bool`, `Option`, or `List` values. [`interface.zy`](interface.zy) defines the public
package independently from its implementation. [`std.zy`](std.zy) applies the ordinary Zydeco modules in this
directory, derives the higher-level operations, and assembles a value of that public package type.

This separation keeps algebraic data in the language. The interpreter and native runtime only need to agree on
the small Builtin ABI, while `bool.zy`, `option.zy`, `list.zy`, and the derived operations in `std.zy` remain ordinary
Zydeco code.

## Text model

`String` is immutable, valid UTF-8 text. Its indexed operations use zero-based Unicode scalar positions:

- `string/length` counts Unicode scalar values.
- `string/byte_length` counts bytes in the UTF-8 encoding.
- `string/get` returns `Option Char`; negative and out-of-range positions return `none`.
- `string/split_at` splits at a scalar boundary and returns `none` for an invalid position.
- `string/to_chars` and `string/from_chars` convert between text and `List Char`.

A `Char` is one Unicode scalar value. `char/codepoint` returns its integer value, and `char/from_codepoint` rejects
negative numbers, surrogate code points, and values above the Unicode range with `none`.

Unicode scalar values are deliberately different from user-perceived grapheme clusters. For example, a combining
mark occupies its own position. Grapheme segmentation and normalization should be added as a separate text layer
rather than changing the meaning of these foundational operations.

## Total operations

Operations whose inputs may be invalid report that fact in their types:

```zydeco
string/get          : String -> Int -> Ret (Option Char)
string/split_at     : String -> Int -> Ret (Option (String * String))
string/parse_int    : String -> Ret (Option Int)
char/from_codepoint : Int -> Ret (Option Char)
list/get            : forall (A : VType) . List A -> Int -> Ret (Option A)
```

The Builtin forms implement these results as computation-polymorphic branches. The public library reifies a
successful branch with `option/some` and a failed branch with `option/none`. Neither backend has a hidden sentinel,
and malformed input does not panic the host runtime.

Integer division and remainder still inherit the machine integer domain and are not yet wrapped in checked
operations. A future numeric module should expose checked arithmetic before adding more integer representations.

## Public modules

- `bool`: constants, logical connectives, equality, and conditional elimination.
- `option`: construction, elimination, mapping, chaining, defaults, and zipping.
- `list`: construction, right and left folds, append, map, reverse, length, safe indexing, head, and tail.
- `int`: arithmetic, complete comparisons, successor/predecessor, negation, extrema, and string rendering.
- `char`: UTF-8 text rendering and checked Unicode codepoint conversion.
- `string`: scalar-aware observation, safe decomposition, character-list conversion, concatenation, and parsing.
- `os`: text and integer output, input, process arguments, randomness, successful halt, failure, and explicit exit.

The component files are independently importable pure package functions. `std.zy` is the composition root used by
most programs and re-exports their abstract type witnesses in one package. Keeping the interface separate makes
the exposed contract reviewable without reading the implementation machinery.
