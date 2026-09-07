# Type Checking

`check` implements Zydeco's bidirectional kind and type checking.
It consumes scoped surface syntax, elaborates typed kinds, types, values,
and computations, and records their annotations in a `StaticsArena`.
A source check also manages lexical scope, inference, source reuse, allocation sites, and diagnostics.
The module boundaries separate those responsibilities so a rule can be maintained with its semantic mechanism.

## Source checking flow

`Tycker` holds the mutable state for one source check.
`driver` initializes that state, checks the root, closes inference, normalizes and validates the arena,
and invokes static value elaboration before publishing a checked source.
`source` owns the resulting source outcomes, local inference regions, and the immutable handles used
to synthesize a resolved source term once.

Local rules run through the `Tyck` protocol with an `Action` or `PatternAction`.
Synthesis determines a classifier; analysis checks against an expected classifier.
The term dispatcher prepares expected annotations and forwards that preparation through transparent wrappers.
Term and pattern dispatchers retain the task stack, allocation-site entry and exit,
and source annotation recording around their syntax-specific rules.
These administrative boundaries preserve source identity and editor facts when a rule invokes another judgment.

After local inference closes, shared normalization contexts resolve holes and normalize the arena's kinds and types.
Coverage validates data matches and codata observations.
Static elaboration then supplies the residual root used by interpreter linking and SPS lowering.
The phase rules are specified in [Compile-Time Normalization](../../../../docs/proposals/normalization.md).

## Checking modules

| Module | Responsibility |
| --- | --- |
| `mod` | Checker state and the public checking API. |
| `driver` | Source lifecycle, allocation sites, finalization, and diagnostic guards. |
| `source` | Source outcomes, inference regions, and reuse of synthesized source terms. |
| `judgment` | Synthesis/analysis modes, expected-annotation provenance, and the `Tyck` protocol. |
| `binding` | Acyclic bindings, recursive groups, and typed pattern assignment. |
| `term` | Term dispatch and rules grouped into atomic, structural, function, package, computation, data, and source-boundary modules. |
| `pattern` | Pattern dispatch, opening scopes, and atomic, named, product, constructor, and alias rules. |
| `functions` | Function formation, canonical package witnesses, and dependent introduction and application. |
| `projection` | Field search, delayed substitution, telescope traversal, and selective package opening. |
| `intrinsics` | Primitive classifiers, intrinsic materialization, and Builtin/foreign metadata registration. |
| `monadic` | Monadic basis checking and algebra translation of a shared checked payload. |
| `copattern` | Type-directed elaboration of generalized comatch clauses. |
| `annotation`, `lub`, `syntactic` | Annotation conversions, type compatibility, and syntactic classification. |
| `completion` | Expected annotations and compatibility evidence for completion queries. |
| `error`, `dump` | Structured diagnostics and checker trace rendering. |

Keep a new syntax rule in its corresponding term or pattern family.
Shared witness, projection, inference, and representation operations belong in the module that owns that mechanism;
callers use its internal interface rather than repeating the rule.
Expose only the operations needed by the enclosing subsystem, and retain the dispatcher-owned administrative boundaries.
Regression tests for source reuse, projection, annotation compatibility, completion,
and term preparation live with their owning modules and share the checker fixture in `tests`.

## Neighboring statics modules

`syntax`, `environment`, and `arena` define the durable typed representation.
`alloc`, `construct`, and `destruct` supply typed allocation, construction, and inspection APIs.
`normalize` separates scope support, substitution, type reduction, inference refinement, hole resolution,
and filled normalization; the finalization caches remain shared across arena roots.

`query` separates shared database inputs from syntax-family judgment producers and source orchestration.
It exports those queries through one public module boundary.
`elaborate::monadic` implements the algebra translation, while `elaborate::static_values` shares evaluator state
across value reduction, pattern handling, computation traversal, and residual construction and validation.

`validate` consumes typed syntax for coverage and the optional type lint.
`fmt` and the crate-private `source_span` provide formatting and source-aware diagnostic locations.
The overall ownership and publication boundaries are described
in [DESIGN.md](../../../../DESIGN.md#query-based-analysis).
