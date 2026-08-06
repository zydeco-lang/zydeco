# Pattern Aliasing

Pattern aliasing lets several patterns observe one bindee. The chosen surface form is a parenthesized,
semicolon-separated pattern:

```zydeco
let ((left, right); whole; copy) = pair in
...
```

Every member receives `pair` itself. The construct is therefore a same-bindee, symmetric conjunction: comma
patterns distribute different components of a product, while semicolon patterns repeat one observation. It is not
a pipeline in which `whole` receives the result of `(left, right)`, and it does not construct a product.

The syntax requires at least two members, so ordinary `(pattern)` and `(left, right)` retain their existing meanings.
The complete parenthesized form is one pattern terminal. It can consequently appear wherever an ordinary pattern
can appear and can nest inside another pattern. This terminal role is what allows field projection patterns to
compose without giving slash its own multi-field list syntax:

```zydeco
let (/x = x; /y = y; whole) = point in
...
```

The representation retains source order even though reordering irrefutable members does not change which value is
accepted. Name resolution and type checking visit members from left to right, consistently with product patterns,
so a later annotation can refer to a definition introduced earlier. Keeping order explicit also leaves room for a
future extension with guards or other genuinely sequential observations without changing the syntax tree.

## Initial Semantic Boundary

The implemented form currently aliases value patterns whose members are irrefutable. Variables, holes, named
wrappers, unit, products, existential packages, and compositions of those shapes are irrefutable after type
checking: every value of the expected type has that shape. Constructors remain refutable and are rejected within
an alias group for now. Field projection patterns with irrefutable payloads belong to this set. Type and kind
pattern aliases are also deferred.

This boundary covers whole-value aliases and the motivating multi-field projection patterns. General conjunction
of refutable patterns raises additional questions. Two constructor members may agree and refine the same payload,
or disagree and make the pattern impossible; several match arms additionally need well-defined fallthrough after
a partial conjunction fails. Those cases should be designed together with usefulness and exhaustiveness checking.

At runtime, matching saves the original bindee and applies every member to that saved value in source order.
Interpreter linking preserves the alias node directly. StackIR normalization expands aliases in ordinary value
assignments, while assembly lowering uses a temporary for aliases that remain at parameter or match boundaries.

## Alternatives Retained for Later

The earlier proposals remain useful reference points:

- `(let aliased) = pattern` makes the alias visually prominent, but the `=` lies outside the pattern and collides
  with the surrounding binding delimiter. It is difficult to use as a composable pattern terminal.
- `(let aliased = pattern)` resembles a conventional binary as-pattern. It clearly distinguishes the whole alias
  from the refining pattern, but embeds declaration syntax inside a pattern and makes the relation directional.
  Chaining several aliases also becomes nested and visually heavy.
- `aliased => pattern` is compact and directional, but `=>` already separates function or match inputs from their
  bodies. Reusing it inside patterns suggests control flow rather than same-bindee conjunction.
- `(p; q; r)` gives semicolon a local sequencing flavor while keeping the matching relation symmetric. Parentheses
  delimit that meaning, and source order is preserved if the language later gives the sequence more significance.

The semicolon design is the best fit for composing several field projections today. The binary `let` designs may
still be worth revisiting if Zydeco later needs a distinct, unrestricted as-pattern over refutable patterns.
