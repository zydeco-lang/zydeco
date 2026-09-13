# Documentation

Start with [the language guide](tutorial/zydeco-guide.md) for a source-level walkthrough
and [DESIGN.md](../DESIGN.md) for the project design and repository map.
[CONTRIBUTING.md](../CONTRIBUTING.md) covers building, testing, formatting, and documentation workflows.

Two concise references serve expert programmers and compiler maintainers:

- [Language reference](references/language.md): syntax, typing, computation, static composition, packages,
  host interfaces, and execution profiles.
- [Compiler implementation reference](references/compiler.md): phase contracts, representations, checking,
  lowering, runtimes, tooling, and validation.
- [Documentation todos](todos/README.md): drift findings, proposed repairs, source inventory, ownership,
  and the consolidation sequence.

The references own the rules and implementation accounts transferred
through the [completed consolidations](todos/reference-plan.md#completed-proposal-retirements).
Remaining proposals retain independent rationale and open design questions;
[deferred designs](todos/deferred-designs.md) collects follow-ups from retired records.

Other documentation serves distinct purposes:

| Material | Purpose |
| --- | --- |
| [Standard library guide](../lib/std/README.md) | Current interfaces, capabilities, and package composition |
| [Code style](style.md) | Syntax rationale, naming, layout, and composition conventions |
| [Project documentation guide](documentation.md) | `@[doc]`, editor interaction, references, and verified examples |
| [Editor guide](../editor/README.md) | Cajun behavior, client setup, and runtime configuration |
| [Literate chapters](spell) | Executable explanations and examples |
| [Design proposals](proposals) | Mechanisms, invariants, rationale, and remaining decisions |
| [Explorations](ideas) | Alternatives, experiments, and decision criteria |
| [Component guides](../lang) | Local implementation maps alongside the relevant source |
| [OOPSLA artifact overview](../lib/tests/oopsla/README.md) | Paper-specific examples and evaluation instructions |
| [Earlier tutorial](tutorial/intro_to_zydeco.md), [legacy notes](legacy) | Historical language designs |

Scratch records under [logs](logs) are supplementary; durable rules belong in their owning design document
or completed reference chapter.

[Binary products with right-spine layout](proposals/binary-products.md) preserves an unimplemented alternative
to the current n-ary product semantics.
It requires a separate design decision before adoption.

[Reachability and regions for SPSLow](proposals/reachability-regions.typ) formalizes proposed value support,
environment and stack dependencies, explicit arena operations, and the obligations for safe region retirement.
It is a design draft; the current compiler does not implement these rules.

[Reusable folders and traversal composition](proposals/traversals.md) collects the remaining migration of desugaring,
resolution, and typed rebuilding onto shared structural operations.
The compiler reference defines the implemented [scoped visitor](references/compiler.md#scoped-structural-traversal)
and [surface folder](references/compiler.md#surface-structural-rebuilding).
