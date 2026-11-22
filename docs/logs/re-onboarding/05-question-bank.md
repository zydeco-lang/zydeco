# 05 — Question Bank

> Scratch study note for re-onboarding. Answer from memory first, then follow the pointer.
> A question you cannot answer is a reading assignment, not a failure.

## A. Orientation

1. Which two papers define the project's yardstick, and what is one claim each demands the code keep true?
   → [04 §Why Cleanup Needs a Yardstick](04-cleanup-lens.md#why-cleanup-needs-a-yardstick)
2. Which phases publish distinct `Program` products, and which product does SPS lowering consume?
   → [C1 product table](../../references/compiler/src/architecture.md)
3. Where does a source rule live, and where does its implementation account live?
   What happens when they disagree?
   → [Rule ownership](../../references/README.md#rule-ownership), [todos](../../todos/README.md)
4. What is the difference between runtime metadata and a compile-time meta annotation?
   → [01 §Navigation Conventions](01-map.md#navigation-conventions), [AGENTS naming](../../../AGENTS.md)
5. Which work was uncommitted while the manual was written, which commit landed it, and why is it not settled design?
   → [01 §Current Working-Tree State](01-map.md#current-working-tree-state)

## B. Language Semantics

6. What separates a `VType` from a `CType`, and what classifier does `{ M }` have?
   → [L4](../../references/language/src/classification-and-inference.md),
   [L6](../../references/language/src/computations-and-control.md)
7. Why does forcing a thunk run effects while holding the thunk value does not?
   → [L6](../../references/language/src/computations-and-control.md)
8. What does `Ret A` describe, and why is it not a fixed stack frame?
   → [L6 §Ret and stack extent](../../references/language/src/computations-and-control.md#ret-and-stack-extent)
9. What makes a source file an executable root rather than a checkable library term?
   → [L12](../../references/language/src/sources.md), [README quick start](../../../README.md)
10. What does static elimination erase, and what is left in the residual program?
    → [L10](../../references/language/src/static-elimination.md)
11. How does a packed value introduce a witness, and how does a pattern open it?
    → [L9](../../references/language/src/polymorphism-and-packed-values.md)
12. What is a relative monad, and what does a monadic block reinterpret?
    → [Monadic Blocks](../../references/language/src/translations/monadic-blocks.md),
    [Relative Monads](../../references/library/src/effects/relative-monads.md#relative-monads),
    [tutorial §7](../../tutorial/zydeco-guide.md#7-relative-monads)
13. What are source packages, and how are library/binary/test roles declared?
    → [L12 §Source packages](../../references/language/src/sources/packages.md#source-packages)
14. Which surface forms are aliases or rare synonyms, and which have zero uses?
    → [Appendix §F](appendix-cleanup-candidates.md#f-surface-form-counts-at-e2dd8979)
15. What is the difference between `let`, `def`, and `param` binding forms, and when is each appropriate?
    → [tutorial §2](../../tutorial/zydeco-guide.md#2-one-source-file-one-term),
    [L3](../../references/language/src/bindings-and-scope.md)

## C. Front End

16. Which three representations does the front end pass through, and what does each add?
    → [Card 01](cards/01-frontend.md), [C4](../../references/compiler/src/parsing-and-resolution.md)
17. How are comments and layout preserved for the formatter, and what does the parser record for it?
    → [Card 01](cards/01-frontend.md), [C15](../../references/compiler/src/tooling.md)
18. What does name resolution bind, and what does the scoped representation still not know?
    → [C4](../../references/compiler/src/parsing-and-resolution.md)
19. Where are meta annotations decoded, and where is `intrinsic` decoded more than once?
    → [Card 01](cards/01-frontend.md),
    [C4 §source analysis](../../references/compiler/src/sessions-and-queries.md#shared-source-analysis)
20. What is the difference between strict and recovering parsing, and what does each retain on failure?
    → [C15 §recovering parsing](../../references/compiler/src/parsing-and-resolution.md#recovering-parsing)
21. How does completion use a cursor hole without running a strict analysis on it?
    → [C15 §completion](../../references/compiler/src/tooling/interactive-and-docs.md#completion)

## D. Statics

22. What is a derived identifier, and why is replay stability important for queries?
    → [C2](../../references/compiler/src/compiler-data.md)
23. What do `Scope: Allocates<Id>` and `Scope: ArenaSchema<Id, Item = T>` each constrain?
    → [C2](../../references/compiler/src/compiler-data.md)
24. Trace the thunk typing rule from scoped term to typed arena: which checker module,
    which producer query, which arena tables?
    → [Card 02](cards/02-statics.md), [C5](../../references/compiler/src/checking-and-inference.md)
25. Where are inference regions opened, solved, and rolled back, and what marks an unconstrained pattern?
    → [C5](../../references/compiler/src/checking-and-inference.md),
    [C15 diagnostics](../../references/compiler/src/tooling.md#diagnostic-collection)
26. What does finalization resolve, and why is the type graph shared across roots?
    → [C5 §finalization](../../references/compiler/src/checking-and-inference.md#finalization)
27. What does static elaboration produce, and which two consumers read its residual root?
    → [C6](../../references/compiler/src/elaboration-and-validation.md)
28. Why does coverage validation exist separately from ordinary pattern typing?
    → [C6 §coverage](../../references/compiler/src/elaboration-and-validation.md#coverage)
29. What is the n-ary product inference discrepancy, and which probes preserve it?
    → [Compiler boundaries §n-ary](../../todos/compiler-boundaries.md#n-ary-product-inference-refinement)
30. Why does `zydeco --lint-types` not appear in the current CLI help, and what replaced the lint?
    → [Appendix §E](appendix-cleanup-candidates.md#e-verified-deltas-since-the-evaluations)

## E. Stack IR

31. Which two calculi does `lang/stackir` implement, and which product realizes each?
    → [Card 03](cards/03-stackir.md), [C8](../../references/compiler/src/sps.md),
    [C9](../../references/compiler/src/closure-conversion.md)
32. What does SPS make explicit, and what does "the stack is second-class" mean?
    → [C8](../../references/compiler/src/sps.md)
33. Compare the `br`, `dup`, and `full` schemes: where is the consumer stack named in each?
    → [C8 §SPS lowering and schemes](../../references/compiler/src/sps.md)
34. What does normalization do, and what does demand analysis remove from the `fact.zy` residual?
    → [Card 03](cards/03-stackir.md), [02 §Step 4](02-vertical-slice.md#step-4-why-normalization-matters)
35. What is localization, and why must it run before closure conversion?
    → [C8](../../references/compiler/src/sps.md)
36. What does closure conversion produce, and how are closure and continuation packages represented?
    → [C9](../../references/compiler/src/closure-conversion.md)
37. What are protocol descriptors and word entry contracts checking, and who reads the extracted protocol tables? →
    [C8 §partial protocols](../../references/compiler/src/closure-conversion/word-contracts-and-protocols.md#partial-source-protocols),
    [C9 §word entry](../../references/compiler/src/closure-conversion/word-contracts-and-protocols.md#word-entry-contracts)
38. Why do the SPSLow validators present a choice rather than pure scaffolding?
    → [Audit §validators](../../proposals/artifact-boundary.md#backend-translation-validators)
39. What does `--sps-passes none` expose in the `fact.zy` residual, and how large is the difference?
    → [02 §Step 4](02-vertical-slice.md#step-4-why-normalization-matters)

## F. Native Backend

40. What is ZASM's role, and what does stack analysis compute before emission?
    → [C10](../../references/compiler/src/zasm.md)
41. What is a local representation policy, and what evidence permits changing a cell's representation?
    → [C10 §policy selection](../../references/compiler/src/zasm.md#policy-selection)
42. How do activation frames and GC root maps reach the runtime?
    → [C11](../../references/compiler/src/native-emission.md), [C12](../../references/compiler/src/native-runtime.md)
43. What does a native build put under `build/`, and why does it invoke Cargo for the runtime?
    → [Card 04](cards/04-native-backend.md), [cli guide](../../../cli/README.md#native-executables)
44. Which targets require `--target-arch x86-64` on this host, and what happens without it?
    → [02 §The Ladder](02-vertical-slice.md#the-ladder)
45. What is the product-suffix layout question, and why does it lack end-to-end coverage?
    → [todos work needing a baseline](../../todos/README.md#work-that-needs-a-new-baseline)

## G. Execution Backends

46. How does the interpreter link checked syntax, and what does it copy before evaluation?
    → [Card 05](cards/05-execution.md), [C7](../../references/compiler/src/reference-interpreter.md)
47. Why does the size analysis think the dynamics syntax, linker, and printer could be removed? →
    [Size §judgment-queries, "Interpret the statics residual"](../../proposals/artifact-boundary.md#structural-simplifications)
48. What is the Wasm runtime contract, and where do the host imports live?
    → [C13](../../references/compiler/src/webassembly.md), [Card 05](cards/05-execution.md)
49. Which builtin operations are implemented per consumer, and what is the proposed fix?
    → [Size §builtins](../../proposals/artifact-boundary.md#builtins-written-once-per-consumer)
50. Which execution backends exist now, and which two were removed in October 2026?
    → [Appendix §E](appendix-cleanup-candidates.md#e-verified-deltas-since-the-evaluations)

## H. Tooling and Tests

51. What does the session own that the CLI does not, and which frontends share it?
    → [C3](../../references/compiler/src/sessions-and-queries.md), [Card 06](cards/06-tooling.md)
52. What are revisions, overlays, and queries in the session, and what invalidates a memo?
    → [C3](../../references/compiler/src/sessions-and-queries.md),
    [C3 §analysis facts](../../references/compiler/src/sessions-and-queries.md#analysis-facts-and-materialization)
53. How many ways can a source test be registered, and which ones does the harness own?
    → [Card 06](cards/06-tooling.md),
    [Audit §tooling](../../proposals/artifact-boundary.md#tooling-tests-and-documentation)
54. What does `cargo test-resume` do, when should you use it, and what does it not prove?
    → [CONTRIBUTING](../../../CONTRIBUTING.md#iterate-with-resumable-tests)
55. Where does the code tie an installed binary to its build checkout, and why is that a defect?
    → [Appendix §B5](appendix-cleanup-candidates.md#b-implementation-findings)

## I. Cleanup Decisions

56. State the audit's recurring convention in one sentence.
    Apply it to one feature you have read.
    → [Audit §recurring pattern](../../proposals/artifact-boundary.md#recurring-pattern)
57. Which four decisions gate the largest structural savings, and why does the artifact boundary come first?
    → [Appendix §D](appendix-cleanup-candidates.md#d-design-decisions-for-the-artifact),
    [04 §Suggested Order](04-cleanup-lens.md#suggested-order-for-this-campaign)
58. What must any change under `lang/stackir`, ZASM, or the native backend preserve?
    → [04 §Traps](04-cleanup-lens.md#traps)
59. Which findings are already resolved, and what evidence must you check before re-proposing them?
    → [Appendix §E](appendix-cleanup-candidates.md#e-verified-deltas-since-the-evaluations)
60. Which parts did the audit deliberately retain, and why?
    → [Audit §retained](../../proposals/artifact-boundary.md#retained-after-review)
61. Why does removing the per-judgment checker queries need approval even though source behavior is unchanged?
    → [Query ownership](../../references/compiler/src/sessions-and-queries.md#query-and-checker-ownership),
    [AGENTS docs rules](../../../AGENTS.md#documentation-structure-and-design-records)
62. Which two current defects are not yet recorded in `docs/todos/`, and where should they go?
    → [Audit §defects](../../proposals/artifact-boundary.md#defects-found-during-the-audit),
    [todos](../../todos/README.md)
63. What is the first concrete deliverable of the cleanup campaign, and what must it contain?
    → [04 §Suggested Order](04-cleanup-lens.md#suggested-order-for-this-campaign)
64. Where does a settled decision belong, where does observed drift belong, and where does an unstarted design belong?
    → [docs/README document homes](../../README.md),
    [AGENTS docs structure](../../../AGENTS.md#documentation-structure-and-design-records)
65. What is the validation ladder for a finished removal, and which step is intentionally last?
    → [04 §Verification Protocol](04-cleanup-lens.md#verification-protocol-before-proposing-a-removal)
