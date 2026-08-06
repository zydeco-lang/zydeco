# Structural Field Projection

Named components provide a useful interface only when clients can ask for a field without reproducing the
receiver's product layout. Slash projection therefore behaves as a built-in structural `Has` operation. Given
`value/field`, the type checker searches the receiver's transparent named and product structure for occurrences of
`field :: Payload` and returns the payload of the unique occurrence.

For example, neither the left-hand product nesting nor the outer named wrapper must be repeated at a use site:

```zydeco
let Inner = (x :: Int) * Int that
let Outer = Inner * (z :: Int) that
let Wrapped = (outer :: inner :: Int) that

nested/x
wrapped/inner
```

The search descends through named classifiers and through every component of a product. Other type constructors
are opacity boundaries: lookup does not inspect function arguments, thunk results, data constructor payloads, or
unopened existential packages. Thus structural lookup exposes declared record-like structure without turning `/`
into an unrestricted search through every type reachable from the receiver.

Lookup succeeds only when the complete search has exactly one match. No matches produce a missing-field static
error, while two or more matches produce an ambiguity static error even when the matches occur at different depths.
An explicit chain performs a new search at each slash, so `value/outer/inner` remains available both to document an
intended path and to disambiguate a larger receiver.

Static projection follows the same rule over nested named kinds. If
`T : (outer :: (inner :: VType))`, both `T/outer/inner` and the uniquely resolved `T/inner` have kind `VType`.
Concrete named introductions reduce during checking; projections from abstract types remain typed static terms
until substitution exposes their introductions.

## Resolved Paths

Named wrappers erase at runtime, whereas product traversal does not. Type checking records every product position
on the unique route to the selected field, together with the product type whose layout determines that position.
The interpreter turns this route into successive tuple projections. StackIR lowering turns the same route into
successive full-arity tuple-pattern bindings, leaving backends independent of field names.

This representation deliberately preserves the whole route rather than only the first product position. A field
inside the left component of a nested product may require several projections, and each layer can have a different
physical arity.

## Projection Patterns

The pattern counterpart uses `/field = pattern`:

```zydeco
let (/x = x) = nested in body
```

The slash means that `x` is located by the same recursive, exactly-one-match rule as `nested/x`; the pattern to the
right of `=` is then checked against the selected payload. This differs from `x = pattern`, which requires the
bindee itself to have `x` as its outer named wrapper.

When the payload binder has the same name as the selected field, `/field` is the punned form of
`/field = field`. An annotation still describes the payload, so `/field : Type` expands to
`/field = field : Type`. As with named-field punning, the parser performs this expansion directly and the later
language phases only see the ordinary projection pattern.

A projection pattern is intended to be one pattern terminal. Multiple observations of the same bindee are formed
by the semicolon construct described in [Pattern Aliasing](aliasing.md):

```zydeco
let (/x; /z; whole) = nested in body
```

Every member receives the original bindee. This separation lets slash retain one consistent `Has` meaning while
the aliasing operator supplies same-bindee composition and preserves source order for possible future sequencing
rules.

Projection patterns associate to the right, so `/outer = /inner = payload` performs two staged searches just like
`value/outer/inner`. This gives an explicit way to disambiguate a name that occurs more than once in the complete
receiver. A missing or ambiguous stage is rejected statically by the same resolver used for term projection.

After resolving the unique route, type checking elaborates a projection pattern into ordinary named and product
patterns. Product components away from the route become typed holes. No projection-pattern node enters typed
runtime syntax, which keeps matching, coverage checking, and backend layout on the existing pattern machinery.

The initial implementation requires the payload to be irrefutable. Variable and structural payloads cover the
field-binding use case; constructor payloads are deferred until backend matching supports general nested
fallthrough.
