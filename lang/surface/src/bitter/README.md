# Bitter (Desugared Surface Syntax)

`bitter` is the desugaring phase that sits between the parsed surface syntax (`textual`)
and the name-resolved syntax (`scoped`).
It normalizes surface constructs into a smaller core, injects primitive terms for built-in kinds and types,
and preserves a mapping back to the original parsed entities so later phases can report accurate spans.

## Role in the pipeline

The surface pipeline looks like:

```markdown
textual (parser output) -> bitter (desugared surface) -> scoped (name resolution)
```

`bitter` is intentionally still "surface-shaped" (it keeps source-level names),
but removes syntactic sugar and inserts explicit nodes the later passes rely on.

## Data model

`bitter::syntax` defines the desugared AST and the identifiers used to store it:

- `DefId`, `PatId`, and `TermId`: arena-backed IDs.
- `BitterArena`: holds arenas for definitions, patterns, and terms plus a `textual` mapping (`ArenaForth`)
  that links bitter nodes back to the original textual entity IDs.
- `PrimTerms`: collects the inserted internal terms (like `VType`, `CType`, `Thk`, `Ret`,
  etc.) so name resolution can treat them as primitives and avoid capture.

This pass keeps names as `VarName` in `Term` until the `scoped` pass rewrites them to bound variables.

## Desugaring pass

`bitter::desugar` implements the `Desugarer` compiler pass.
It:

- Expands sugar into core forms (e.g., empty parens to `Triv`, comma-separated parens to `Cons(ConsN<_, _>)`,
  `Arrow` to `Pi`, `Prod` to `Sigma`, and `forall`/`exists` to annotated `Pi`/`Sigma`).
- Normalizes multi-parameter abstractions and applications into nested `Abs` and `App` nodes.
- Inserts type annotations for literals, `ret`, and `thunk` so later phases see explicit types.
- Consumes `@[monadic]` metadata into a monadic-translation node while preserving the annotated payload term.
- Wraps definitions in `Sealed` when they should not be expanded accidentally.
- Builds `PrimTerms` by inserting the internal kind/type terms and storing their `TermId`s in a `MultiCell`.

`Alloc` centralizes allocation into `BitterArena` while recording the textual-to-bitter mapping.
`DeepClone` is used to duplicate nodes while preserving their original source linkage.

## Errors and spans

- `bitter::err` defines errors for malformed intrinsic or monadic metadata and incompatible binding modifiers.
- `bitter::span` provides `SpanView` implementations that retrieve spans via the `textual` back-mapping.

## Formatting

`bitter::fmt` implements an "ugly" formatter over the bitter syntax.
It is primarily a debugging aid to print desugared terms in a safe surface syntax.
