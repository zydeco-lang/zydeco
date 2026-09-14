# Reference drift

These outstanding documentation repairs follow the 2026-09-08 and 2026-09-10 audits,
with a source review against `3b4dd665` on 2026-09-14.
Completed repairs and migration histories have been removed from this work list.
The [reference ownership map](../references/README.md#rule-ownership) locates each canonical account;
[CONTRIBUTING](../../CONTRIBUTING.md#maintain-documentation) defines the consolidation workflow.

## Formal calculus

- [ ] **Recursive field search.** The [calculus](../../lang/statics/type-system.typ) still gives
  `field` an immediate-component search and explicitly excludes deeper traversal.
  [L9](../references/language.md#9-polymorphism-and-packages)
  and [field lookup](../../lang/statics/src/check/projection/field.rs) recursively search named wrappers,
  products, and package telescopes.
  Reconcile the mathematical rule with those supported routes and ambiguity checks.
- [ ] **Computation package witnesses.** Compare the calculus's package-dependent computation rules
  with the leading-prefix boundary in [L9](../references/language.md#9-polymorphism-and-packages)
  and [the current application checker](../../lang/statics/src/check/functions/application.rs).
  The [nested-package probe](compiler-boundaries.md#nested-package-witness-diagnostic) reaches an application failure.
  Keep value-function witness routes separate;
  their implemented structural routes do not extend computation application.
- [ ] **Value matches and integer operations.** Add the value-producing match
  and integer value forms supported by [L8](../references/language.md#8-value-functions-and-views)
  and [static elimination](../references/compiler.md#static-elimination).
  The current calculus's value grammar and rules omit them.
  The earlier n-ary product repair does not establish conformance of these other judgments.

Validate revised rules against accepted and rejected source examples, then build the mathematical companion:

```sh
typst compile --root . lang/statics/type-system.typ /tmp/zydeco-type-system.pdf
```

Document construction alone does not establish conformance to the compiler.
Stronger recursive admissibility is an [unimplemented design question](../ideas/recursive-admissibility.md),
not a missing positivity guarantee to add to the reference.

## Duplicate accounts and scratch records

- [ ] **DESIGN and component overlap.** [DESIGN](../../DESIGN.md) still repeats detailed language,
  runtime, and tooling accounts alongside the references.
  Audit each section against its [reference owner](../references/README.md#rule-ownership),
  preserve implemented rationale there, and shorten the secondary account to orientation and links.
  Keep library recipes and component module maps in their local guides.
- [ ] **Temporary records.** Audit the remaining [scratch logs](../logs) for unique rationale,
  unresolved reproducers, and historical measurements before deleting them,
  following [the repository's record policy](../../AGENTS.md#documentation-structure-and-design-records).
  Preserve evidence with its original revision and limitations; re-run measurements used to choose a new default.
  The dated [runtime study](../ideas/cbpv-runtime-evaluation.md) already has a separate evidence role.
  In particular, the September 7 follow-up's `ArgumentFold` root-table mechanism has been removed,
  and `HostString::leak` has been replaced by storage owned and released
  by a [runtime instance](../references/compiler.md#runtime-instances).
  Its six-open-findings count must not be copied into a current backlog.
  Preserve the remaining interpreter cloning, deep-stack, and suffix-layout probes with their historical limits;
  compare long-running host retention separately from normal entry teardown.
