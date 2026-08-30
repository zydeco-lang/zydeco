# Surface Syntax Principles

Zydeco's surface syntax should expose the structure that the type checker follows
without assigning extra meaning to punctuation.
Juxtaposition carries ordinary type-directed structure, while delimiters mark boundaries
that a reader cannot otherwise recover locally.
This keeps the notation light and lets value and computation types determine what may follow each construct.

## Follow the Residual Classifier

A constructor or destructor selects a residual classifier.
The syntax after that head is interpreted against the residual classifier, whose structure may be a constructor payload,
an arrow, a quantifier, another data or codata type, or a dependent package boundary.
The head does not declare a source-level argument list.

Patterns therefore use juxtaposition when one constructor payload is itself headed by another constructor:

```zydeco
match optional_pair
| +Some +Pair(left, right) => ret left
| +None _                  => ret 0
end
```

Copatterns extend the same idea along an elimination path.
Each item is checked against the computation type left by the preceding item:

```zydeco
comatch
| .route +First(value)  .left  => ret value
| .route +First(value)  .right => ret value
| .route +Second(value) .left  => ret value
| .route +Second(value) .right => ret value
end
```

Here `.route` selects its result type, `+First(value)` or `+Second(value)` matches the next value argument,
and `.left` or `.right` observes the codata computation that remains.
Writing `.route(+First(value)).left` would make the value pattern appear to be syntactically owned by `.route`,
even though its role comes from the residual arrow type.
Zydeco consequently prefers the whitespace-guided spine.

This syntax is whitespace-guided rather than layout-sensitive.
Newlines and alignment help readers see the spine, but changing them does not change the parse.
Parentheses remain meaningful for products, annotations, precedence, and other genuine grouping:

```zydeco
| .apply (function : Thk (A -> B)) argument => ! function argument
```

## Distinguish Classifiers from Term Bodies

The thin arrow `->` constructs an arrow type.
The fat arrow `=>` introduces an executable term body:

```zydeco
A -> B
fn value => ret value
fix recur => fn value => ! recur value

match value
| +Present(payload) => ret payload
| +Absent(_)        => ret fallback
end

comatch
| .read => ret current
| .write next => ! update next
end
```

This distinction applies uniformly to `fn` and `fix` bodies, match arms, comatch arms,
and the single-clause block abstraction form.
A type annotation inside a pattern may still contain `->`; the surrounding `=>` then marks the unambiguous transition
from the complete pattern or copattern to its body.

## Prefer Existing Boundaries

The leading `|` already separates a match scrutinee from its arms,
so `match` does not need an additional `with` keyword.
Both matching forms retain `end`, consistent with `data`, `codata`, and the other explicitly delimited blocks.
Braces remain available for thunks, and parentheses are not added merely to imitate call syntax.

These choices favor a small grammar in which each token records one durable distinction:
juxtaposition follows the residual classifier, `->` belongs to classifiers, `=>` enters term bodies,
and explicit delimiters close regions whose extent would otherwise be unclear.
