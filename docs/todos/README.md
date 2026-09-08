# Documentation todos

Keep documentation disagreements and proposed repairs here while the references are drafted.
Recording a discrepancy does not settle a language-design decision or authorize a compiler change.

- [Reference drift](reference-drift.md): observed disagreements, evidence, and bounded follow-up actions.
- [Reference plan](reference-plan.md): source inventory, eventual rule ownership, and consolidation sequence.

The [language reference](../references/language.md)
and [compiler reference](../references/compiler.md) now cover their planned chapters.
They own transferred rules and implementation contracts; remaining proposals retain their independent mechanisms,
rationale, and open decisions.
Completed transfers are recorded in the [reference plan](reference-plan.md#completed-proposal-retirements).

Check both references and the library guide's opted-in examples from the repository root:

```sh
cargo run --quiet --bin zydeco -- doc check docs/examples/documentation/counter.zy \
  --guide docs/references/language.md --guide docs/references/compiler.md --guide lib/std/README.md
```

The counter supplies the documentation command's required source root.
Guide examples are complete independent terms with paths relative to the guide.
This checks acceptance and specified rejections; it does not execute the examples.
