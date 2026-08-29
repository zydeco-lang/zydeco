# Standard library topic consolidation worklog

This records the restructuring that collapsed the per-leaf companion-signature fan-out under `lib/std/`
into one implementation and one companion per topic. The motivating audit and the chosen target shape are
described with the proposal in [package modularization](../proposals/package-modularization.md);
this log keeps the implementation evidence: what the checker actually supports, what failed, and why.

## Baseline

Before the restructure, `lib/std` held 106 sources (about 5,900 lines).
Every data leaf split into `foo.zy`, `foo.zyi`, `foo.type.zy`, and for extendable leaves `foo-core.type.zy`;
`std.type.zy` restated all twenty-seven existential witnesses and every module field,
and `std.zy` repeated the same surface as a record.
Each public operation was written three to seven times across these layers.

## Checker facts established by experiment

- A bare record literal has no principal type; every package-producing source needs a whole-file
  annotation. `.zyi` companions exist to supply it: source assembly elaborates the pair as
  `(implementation : signature)` (`lang/session/src/source/program.rs`), so importers see the ascribed type.
- `def ... that` bindings hoist above any enclosing `let ... in` chain, because the dependency scheduler
  does not track lexical `in`-scopes as dependencies. Defs that use `let`-bound names must sit in an inner
  `begin` below those lets. Computation `let !` does not hoist; helpers that defs depend on must
  themselves be `def !`.
- `def`-bound data types cannot cross file boundaries by disclosure: annotations can only name a
  definition through an import, and only compiler intrinsics are canonical importable terms.
  Existential witnesses therefore remain the only cross-file naming device for library data types,
  which is why each topic keeps `exists` in `package.type.zy` rather than exporting manifest types.
- Type-level applications of transparent `let` functions reduce during analysis
  (`TypeId::normalize` unfolds abstraction spines), and the analytic entry point normalizes an expected
  type whenever its preparation environment is invalid for the current term.
  A record literal checked against an applied body constructor therefore needs no new checker support,
  provided the record nests one sub-record per application, matching the constructor's shape.
- Structural field projection searches nested named products, so the public package may nest its topic
  groups while consumers keep selecting `(/option; /process)` and calling `option/some` unchanged.
  Projection does not descend into unopened existential packages; the std-level `exists` reintroduces
  the shared witnesses once, and topic packages stay opaque inside their groups.

## Attempts that failed, with causes

- Packaging several type constructors from one `.type.zy` source as a record of type functions:
  a record of kind-level fields still needs a whole-file annotation, and a manifest-exists bundle
  cannot be field-selected in a `.zyi`, where `@(import)` stands for the imported source's type, not its
  value. One file still exports one term; topics therefore split `body.type.zy` (record-shape constructor
  and module telescopes) from `package.type.zy` (existential wrapper).
- Sealing a merged implementation with an inline `let package : DataPackage = (record)`:
  checking the ascription computes a least upper bound between the exists and the raw def-based record
  type and fails when the source is loaded as a dependency. Leaving the record bare under the companion
  works: the companion `Ann` is the only ascription point.
- A flat public record against a spliced expected chain (`(prelude :: Prelude) * (DataBody ...) * ...`):
  an experimental checker patch made the field walk splice normalized product applications into the
  component chain, and static checking passed, but resolved projection paths were computed against the
  nested structure while values were laid out flat, so evaluation crashed with
  `only products have product fields`. The consistent fix needed no checker change: the std record nests
  one explicit sub-record per topic, matching the expected applications structurally.
  The patch was reverted.

## Resulting layout

Each topic (`data`, `text`, `system`, `numeric`) provides `package.zy`, `package.zyi`,
`body.type.zy`, and where it owns abstract witnesses `package.type.zy`;
`data` keeps `bool.type.zy` because the numeric builders import `BoolModule`,
and `numeric` keeps the capability and builder telescopes it already had.
`std.type.zy` is gone: `std.zyi` imports the four body constructors and wraps them in one existential.
`lib/std` now holds 87 sources and about 4,000 lines; the net repository diff removes roughly
1,200 lines and 24 files while the public selection surface is unchanged.

Consumer-visible notes:

- `option`, `result`, and `list` operations with clashing names (`fold`, `map`, `and_then`, `unwrap_or`)
  are defined with module-prefixed names inside `data/package.zy`; the exported record fields keep the
  public names, and `bool` operations keep theirs.
- Editor tests (`cajun`) and pretty-printer fixtures reference concrete sources, so they moved to
  `data/package.zy` and hover anchors changed accordingly; the two pretty fixture tables collapsed into
  one shared `standard_library_sources` list to keep future library reshapes single-point edits.
- `editor/zed/grammars/` is an untracked local development copy (ignored by `editor/zed/.gitignore`);
  it is regenerated when needed and is not maintained in step with the tree.

## Open questions

- The nested std record relies on positional application of body constructors; a future
  record-extension or telescoping-include mechanism in the language could remove the remaining
  restatement of field lists between a topic body and its implementation record.
- The manifest canonical-type witnesses in `std.zyi` (`Int8` through `Bytes`, capability constructors)
  restate import disclosures that `builtin.zy` already pins; if the checker grows a way to re-export
  manifest packages without restatement, that block shrinks too.
