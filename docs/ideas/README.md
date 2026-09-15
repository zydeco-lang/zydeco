# Design ideas

These exploratory questions and research directions do not extend the current language or implementation contract.
Each design idea has its own file, linked to its implemented starting point.
This index only provides navigation. Develop an idea into a [proposal](../proposals/README.md)
when it has a concrete design, alternatives, and validation criteria.
Keep observed implementation discrepancies in [todos](../todos/README.md).
The [references](../references/README.md) own approved, implemented contracts.
Dated experimental reports and their evidence live in [evaluations](../evaluations/README.md).

## Design questions

| Idea | Question |
| --- | --- |
| [CBPV as a universal FFI](cbpv-universal-ffi.md) | Can typed CBPV adapters compose foreign data and control protocols in either direction? |
| [Refutable view coverage](view-coverage.md) | When can several view arms establish coverage together? |
| [Shared view evaluation](shared-view-evaluation.md) | Should grouped arms evaluate their view once? |
| [Broader view heads](broader-view-heads.md) | Which value terms should be admitted as view heads? |
| [Residual code sharing](residual-code-sharing.md) | When should repeated static applications share residual code? |
| [Demand-directed construction](demand-directed-construction.md) | Can unused high-SPS nodes be avoided during construction? |
| [REPL history and replay](repl-history-replay.md) | How should numbered source history persist and replay effects? |
| [REPL history pruning](repl-history-pruning.md) | When can stored interactive sources be discarded? |
| [REPL command extensions](repl-command-extensions.md) | Which interactions need argument-bearing or stateful commands? |
| [Span lookup indexing](span-lookup-index.md) | Does source lookup need an interval index? |
| [Source-location column caching](source-location-column-cache.md) | Is cached multibyte column information worth its memory cost? |
| [Source locations in later phases](later-phase-source-locations.md) | Should later arenas embed spans or follow existing provenance? |
| [Macro source provenance](macro-source-provenance.md) | Would a future macro system need hygiene in source provenance? |
| [An editor action for typeof](typeof-editor-action.md) | Should the editor construct source classifier queries? |
| [Classifier pattern aliases](classifier-pattern-aliases.md) | What would type and kind pattern aliases mean? |
| [Formatter punning audit retirement](formatter-punning-audit.md) | Does the formatter still need its migration audit helper? |
| [Kind-witness introduction](kind-witness-introduction.md) | How could packages introduce kind witnesses? |
| [Computation witness routes](computation-witness-routes.md) | How could computation application follow witnesses beneath products? |
| [Companion interface generation](companion-interface-generation.md) | Do companion interfaces need generation or synchronization? |
| [Recursive admissibility](recursive-admissibility.md) | Which recursive definitions should a stronger admissibility rule accept? |
| [Elaborated type layout](type-rendering-layout.md) | How should long telescopes and source-shaped term labels be rendered? |
| [Source generation from elaborated terms](typed-source-generation.md) | Which consumers need source reconstruction from typed terms? |
| [Floating comment anchors](floating-comment-anchors.md) | How should future syntax anchor floating comments? |
| [Source fixture directive extensions](fixture-directives.md) | Which additional outcomes and inputs belong in source fixture directives? |
