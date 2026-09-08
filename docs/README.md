# Documentation

Start with [the language guide](tutorial/zydeco-guide.md) for a source-level walkthrough
and [DESIGN.md](../DESIGN.md) for the current language and implementation account.
[CONTRIBUTING.md](../CONTRIBUTING.md) covers building, testing, formatting, and documentation workflows.

Two references are being compiled for expert programmers and compiler maintainers:

- [Language reference](references/language.md): a concise draft of syntax, typing, computation, static composition,
  packages, host interfaces, and execution profiles.
- [Compiler implementation reference outline](references/compiler.md): phase contracts, representations,
  checking, lowering, runtimes, tooling, and validation.
- [Documentation todos](todos/README.md): drift findings, proposed repairs, source inventory, ownership,
  and the consolidation sequence.

The language draft is under review; the compiler document remains an outline.
Existing DESIGN sections and owning proposals remain authoritative until consolidation is approved.

Other documentation serves distinct purposes:

| Material | Purpose |
| --- | --- |
| [Standard library guide](../lib/std/README.md) | Current interfaces, capabilities, and package composition |
| [Code style](proposals/style.md) | Syntax rationale, naming, layout, and composition conventions |
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
