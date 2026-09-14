# Documentation

Start with [the language guide](tutorial/zydeco-guide.md) for a source-level walkthrough
and [DESIGN](../DESIGN.md) for the project overview and repository map.
[CONTRIBUTING](../CONTRIBUTING.md) covers building, testing, formatting, and documentation maintenance.

The [references](references/README.md) own approved, implemented language and compiler contracts,
with one home for each rule.
[Proposals](proposals/README.md) contain concrete unfinished designs;
[ideas](ideas/README.md) contain exploratory questions;
[evaluations](evaluations/README.md) retain dated experimental reports and their evidence;
[todos](todos/README.md) record observed discrepancies requiring repair.
The [ownership map](references/README.md#rule-ownership) connects source rules to their implementation accounts.

| Material | Reader's purpose |
| --- | --- |
| [Language reference](references/language.md) | Look up syntax, typing, behavior, packages, capabilities, and execution boundaries |
| [Compiler reference](references/compiler.md) | Find phase contracts, representations, algorithms, runtimes, and validation |
| [Standard library guide](../lib/std/README.md) | Use current interfaces, capabilities, and package composition |
| [Code style](style.md) | Choose readable names, source layout, and composition idioms |
| [Editor guide](../editor/README.md) | Configure Cajun and find client setup |
| [Literate chapters](spell) | Read executable explanations and examples |
| [Design proposals](proposals/README.md) | Review unimplemented extensions and their validation criteria |
| [Design ideas](ideas/README.md) | Explore research questions and conceptual directions |
| [Evaluations](evaluations/README.md) | Inspect dated experiments, measurements, and reproduction artifacts |
| [Component guides](../lang) | Locate local implementation modules |
| [OOPSLA artifact overview](../lib/tests/oopsla/README.md) | Follow paper-specific examples and evaluation instructions |
| [Earlier tutorial](tutorial/intro_to_zydeco.md), [legacy notes](legacy) | Inspect historical language designs |

## Writing and maintaining documentation

Documentation tooling and maintenance of this repository's books are different tasks.
Choose the corresponding reference section or design record below.

| Task | Home |
| --- | --- |
| Write `@[doc]` prose, semantic links, and examples; use the panel or `zydeco doc` | [Source documentation](references/language.md#source-documentation), [tooling workflow](references/compiler.md#documentation-workflow), and [a complete example](examples/documentation/guide.md) |
| Understand implemented attachment, provenance, publication, and verification | [C4: source analysis](references/compiler.md#shared-source-analysis), [C15: documentation](references/compiler.md#documentation-subjects-and-provenance) |
| Design new documentation features | [Documentation proposal](proposals/documentation.md) |
| Improve completion or generated type/source text | [Completion proposal](proposals/completion.md), [type-layout ideas](ideas/type-rendering-layout.md), [source-generation ideas](ideas/typed-source-generation.md) |
| Review, consolidate, format, and verify repository documentation | [Contribution workflow](../CONTRIBUTING.md#maintain-documentation) |
| Repair an observed disagreement or duplicated account | [Reference drift](todos/reference-drift.md) |

Exploratory questions live in [design ideas](ideas/README.md) until they develop into concrete proposals.
Scratch records under [logs](logs) are temporary: extract durable evidence into its owner before deleting the record.
Completed consolidation plans and repair histories remain available in version history.
