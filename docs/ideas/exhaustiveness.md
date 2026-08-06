# Exhaustiveness Checking for Typed Pattern Matches

A data match is exhaustive when every runtime value of the scrutinee's type is accepted by at least one arm.
Checking only that every constructor name appears is insufficient because constructor payloads may contain
nested data and products. Several individually broad arms may also leave a correlated combination uncovered.

For example, these arms mention both Boolean constructors in both fields, but they do not cover
`+Pair(+False(_), +True(_))`:

```zydeco
match pair
| +Pair(+True(_), _) => ...
| +Pair(_, +False(_)) => ...
end
```

Zydeco detects such gaps with a pattern-matrix algorithm over typed syntax. The algorithm successively removes
one constructor layer from the patterns, keeps the constraints on the remaining fields together, and rebuilds
uncovered rows as concrete counterexamples. This is a post-check validation pass: it runs after local type
checking, hole resolution, and type normalization have succeeded.

## Why Coverage Uses Typed Syntax

Surface syntax alone does not contain all the information needed to interpret a pattern. Constructor names are
resolved relative to the expected data type, tuple syntax is interpreted using its expected product type, and
existential packages distinguish erased static witnesses from their dynamic payload. The type checker has already
made these decisions and rejected unknown constructors before coverage validation begins.

The typed arena records the following facts used by the validator:

- `data_hints` associates a match scrutinee with its `DataId`.
- `data_pat_hints` associates each constructor pattern with the data definition that owns it.
- `codata_hints` associates a comatch computation with its `CoDataId`.
- `copattern_matches` identifies matches generated from generalized comatch argument patterns.
- `copattern_pack_pi_binders` identifies package-dependent copatterns checked as direct abstractions.

The first hint supplies the complete constructor space even when a match has no arms. The second disambiguates
nested constructor patterns. Because coverage starts only after normalization and only when local checking has no
errors, it can treat every row as well typed and assume that all concrete heads in one column describe the same
value space.

## The Internal Pattern Language

Bindings do not affect which values a pattern accepts, so variables and holes both become wildcards. The checker
otherwise preserves the eliminable structure of a typed value pattern:

```text
p ::= _                    wildcard, variable, or hole
    | +K p                 data constructor and its payload
    | ()                   unit
    | (p1, ..., pn)        product
    | field = p            named value
    | (_, p)               existential package and its dynamic payload
```

Every non-wildcard pattern has a finite *head space*. A head space says which constructors can occur at the next
layer and how many fields appear when one of them is removed:

| Head space | Possible heads | Head arity |
| --- | --- | ---: |
| Data definition `D` | Every constructor declared by `D` | 1 |
| Unit | `()` | 0 |
| Product | The product constructor | Number of components |
| Named value | The expected field label | 1 |
| Existential package | The package constructor | 1 dynamic payload |

A Zydeco data constructor has one payload pattern for coverage purposes. A nullary-looking constructor normally
has `Unit` as that payload, while a constructor with several source arguments has a product payload. Keeping this
representation uniform lets the matrix algorithm handle constructor arguments with the same rules as every other
nested pattern.

Typed `VCons` patterns are converted to a right-associated binary product spine. Consequently, `(a, b, c)` and
`(a, (b, c))` have the same coverage shape even though earlier compiler phases may preserve their distinct source
grouping. Static fields of an existential package are erased from coverage; only its dynamic payload can distinguish
runtime cases. Named values and packages each have one possible head, but retaining those heads allows a missing
witness to be printed in source-like form.

## Matrices Preserve Correlations

The arms of a match form a matrix. Each arm is one row, and the initial matrix has one column for the scrutinee:

```text
[ p1 ]
[ p2 ]
[ .. ]
[ pn ]
```

Rows are alternatives: a value is covered when it matches any row. Columns are simultaneous constraints within
one alternative: every pattern in a row must match its corresponding component. Decomposing a constructor replaces
its column with columns for the constructor fields, so relationships between nested fields remain in the same row.

After removing `+Pair` and its product payload from the earlier example, the matrix is:

```text
         first       second
arm 1    +True(_)    _
arm 2    _           +False(_)
```

This two-column matrix makes the gap visible. A checker that independently collected constructors from each column
would lose the correlation and incorrectly accept the match.

## Computing Uncovered Rows

Let `U(P, n, E)` compute witnesses not covered by matrix `P`, where every row has `n` columns and `E` is an
optional expected head space for the first column. The initial call uses the arm matrix, one column, and the
scrutinee's data definition as `E`.

The implementation uses the following recursion:

```text
uncovered(P, n, expected):
  if n = 0:
    return [[]] if P is empty, otherwise []

  if P is empty and expected is absent:
    return [[_, ..., _]] with n wildcards

  space = expected, or the first non-wildcard head space in column 1

  if space is absent:
    return prepend(_, uncovered(default(P), n - 1, none))

  return concatenate, for every constructor c in space:
    rebuild(c, uncovered(specialize(c, P), n - 1 + arity(c), none))
```

The result is a matrix too, but its rows are missing value shapes rather than source arms. An empty result means
the original match is exhaustive. A nonempty result gives counterexamples for the diagnostic.

### The Base Case

When no columns remain, there is exactly one possible tuple of remaining values: the empty tuple. If at least one
row remains, that tuple is covered and the result is empty. If no row remains, the algorithm returns one empty
witness row so that surrounding recursive calls can rebuild the constructors that led to the gap.

This convention also handles empty data definitions. Their head space contains no constructors, so iterating over
the possible heads produces no missing witness. A zero-arm elimination of an empty data type is therefore
exhaustive.

### Specialization

For a chosen constructor `c`, specialization keeps exactly the rows that could match a value headed by `c`:

```text
specialize c (_ :: rest)       = [_1, ..., _arity(c)] ++ rest
specialize c (c(fields) :: rest) = fields ++ rest
specialize c (d(fields) :: rest) = discard the row, when c != d
```

A wildcard accepts `c`, so it expands to one wildcard for each field. A matching concrete head contributes its
field patterns. Any different head cannot match and is removed. The first column has been consumed, while the
constructor fields become new columns; this accounts for the recursive column count
`n - 1 + arity(c)`.

After the recursive call, `rebuild` takes the first `arity(c)` patterns from every missing row, wraps them back in
`c`, and leaves the other columns in place. Specialization descends from source patterns into fields, and rebuilding
turns the result back into a source-like missing pattern.

### The Default Matrix

If the first column contains only wildcards, it does not reveal or restrict a finite head space. The default matrix
keeps the wildcard-headed rows, removes their first column, and recursively checks the remainder. Every resulting
witness is prefixed with `_`.

When a concrete typed head is present, its head space is finite and known. The checker enumerates the entire space,
including constructors absent from the source arms. When the specialized matrix for one of those constructors is
empty, the recursive call produces wildcard fields, giving a concise witness without needing to enumerate every
possible payload.

## Walking the Correlated Example

The earlier `Pair` match proceeds as follows:

1. The expected data space contains the single `+Pair` constructor. Specialization removes `+Pair` and exposes its
   product payload.
2. A product has one possible head, so specialization splits the payload into `first` and `second` columns.
3. In the `+False` branch of the first Boolean column, the first arm is discarded because it requires `+True`.
   The second arm survives because its first field is `_`.
4. The remaining second-field constraint covers `+False` but has no row for `+True`.
5. The recursion produces a wildcard for the unit payloads and rebuilds the enclosing constructors and product.

The resulting counterexample is:

```text
+Pair(+False(_), +True(_))
```

In the `+True` branch of the first field, the first arm's wildcard second field covers both possibilities. That
branch contributes no missing row. The algorithm therefore reports only the genuinely uncovered correlation.

## Diagnostics and Search Bounds

Coverage witnesses use a separate `CoveragePattern` representation so diagnostics do not need to allocate new typed
syntax. It preserves constructors, products, units, named fields, and package wrappers, while using `_` wherever a
more specific payload is unnecessary.

Nested finite spaces can have exponentially many missing combinations. The validator retains at most eight patterns
for a diagnostic and computes one additional pattern to determine whether the list was truncated. This bound limits
diagnostic construction; it does not weaken the exhaustiveness decision. An exhaustive matrix still requires every
finite branch to close, while a non-exhaustive matrix needs only one witness to establish failure.

## Coverage for Generalized Comatches

A source comatch clause may describe an entire observation path, mixing value patterns, type patterns, and
destructors in the order required by the computation type. For example, these clauses first select `.route`, inspect
its input, and then select one of the destructors of the returned codata computation:

```zydeco
comatch
| .route +First(x)  .left  => ret x
| .route +First(x)  .right => ret x
| .route +Second(x) .left  => ret x
| .route +Second(x) .right => ret x
end
```

The next meaningful copattern item depends on the residual type at that point in the path. Desugaring therefore
retains each clause spine instead of prematurely turning every source clause into nested abstractions and singleton
comatches. The type checker then elaborates the clause matrix according to the expected computation type:

1. At a codata type, it requires a destructor, groups clauses with the same destructor, and recursively checks each
   group against that destructor's result type. It emits one typed comatch arm for the whole group.
2. At an arrow type, it requires a value pattern, introduces one shared argument, and checks every clause pattern
   against the arrow domain. The patterns remain pending while elaboration follows the rest of each observation path.
3. At a universal type, it consumes a type abstraction pattern and checks the remaining path under the introduced
   type argument.
4. At a package-dependent arrow, it consumes the existential package pattern, checks that its dynamic payload is
   exhaustive, and makes its witnesses available to the dependent result. The current elaborator admits one clause
   at this boundary.
5. When a group reaches its clause bodies, it checks the pending value patterns as one match. Several arrow arguments
   become a right-associated product scrutinee, so the ordinary pattern-matrix algorithm retains correlations between
   arguments.

Delaying the value match is important. Two clauses such as `.choose +False(_)` and `.choose +True(_)` are not duplicate
`.choose` definitions. Together they define one `.choose` observation whose argument match is exhaustive. Conversely,
the rows `.choose +True(_) _` and `.choose _ +False(_)` leave the correlated input
`(+False(_), +True(_))` uncovered, just as the corresponding product patterns do in an ordinary match.

Every generated typed comatch records its residual codata definition. Generated argument matches and direct
package-dependent binders are visited by the same post-check coverage pass as a source match. Exhaustiveness is
therefore compositional: each codata point must contain every declared destructor, and the argument matrix for every
complete observation path must cover all inputs. The generated-node hints let diagnostics describe the latter as
comatch argument gaps. A missing nested destructor and a missing constructor case beneath a destructor are both
reported at the original source comatch.

The typed core still has one arm per destructor. Its codata validator compares those unique arms with the declared
destructor set and also rejects duplicate typed arms defensively. Unknown destructor names and copattern items that
do not agree with the residual type remain local type errors and never reach post-check validation.

## Current Boundary

The validator answers whether a data match covers all inputs and whether every generalized comatch observation is
defined. It does not yet report redundant or unreachable pattern rows. Redundancy can be added with the dual
*usefulness* query over the same matrix operations: a row is redundant when it covers no value that preceding rows
leave uncovered.

Package-dependent arrows currently accept one copattern clause because their result type can depend on existential
witnesses opened by the argument pattern. Supporting several such clauses requires a dependent form of the generated
argument match; ordinary arrows, universal arguments, and arbitrarily nested destructor paths already share the
generalized coverage procedure above.

Type-directed clause elaboration lives in
[`lang/statics/src/check/copattern.rs`](../../lang/statics/src/check/copattern.rs), and the shared pattern-matrix pass
lives in [`lang/statics/src/validate/coverage.rs`](../../lang/statics/src/validate/coverage.rs). Focused examples are
in [`lang/tests/tests/coverage.rs`](../../lang/tests/tests/coverage.rs).
