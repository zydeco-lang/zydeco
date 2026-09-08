# Reference plan

Zydeco needs two complementary references: an expert programmer's account of the language,
and a maintainer's account of the compiler and runtime.
The material exists across DESIGN, proposals, component guides, formal notes, library sources, and tests.
The writing task is to establish a coherent account with one home for each rule,
while preserving the rationale and unresolved questions that deserve separate design records.

The language reference is now a concise draft; the compiler reference remains an outline:

| Reference | Reader's question | Document |
| --- | --- | --- |
| Language | Is this program accepted, what does it mean, and what execution assumptions does it require? | [Language reference](../references/language.md) |
| Compiler implementation | Where is this behavior implemented, what invariants connect the phases, and how can I change it? | [Compiler reference](../references/compiler.md) |

The investigation baseline is commit `77ee657f`, inspected on 2026-09-08.
The outlines draw on repository documentation, implementation entry points,
syntax and representation definitions, and regression tests.
This was a source inspection, not an exhaustive execution or conformance audit.
The language draft was subsequently checked against `289f2a14`; its opted-in examples use the documentation checker.
L1–L15 refer to the language reference's numbered chapters; C1–C16 are the compiler outline's planning identifiers.

## Findings that shape the outlines

[DESIGN.md](../../DESIGN.md) already has substantial coverage, but combines source rules,
compiler storage invariants, runtime ABIs, frontend behavior, and limitations in one document.
It is the primary extraction source rather than merely a repository map.
The [current tutorial](../tutorial/zydeco-guide.md) supplies a useful explanatory progression,
but a reference needs complete construct coverage, explicit rejection boundaries, and lookup tables.

Three distinctions should organize the books:

- **Source semantics and implementation mechanisms.** A computation type describes a stack protocol.
  Native frame slots and Wasm trampoline states implement that protocol and belong in the compiler reference.
- **Required static elimination and optional residual optimization.** Value functions
  and package witnesses obey a shared source phase contract.
  High-SPS demand analysis, closure conversion, and local ZASM unboxing explain the resulting implementation and costs.
- **Language mechanisms and library interfaces.** Packages, codata, and value functions support modules,
  explicit dictionaries, relative monads, and capability APIs.
  Their library uses should link back to those mechanisms, while current operation inventories remain grounded
  in the checked library sources.

The language reference therefore gives operational semantics, phase rules,
host contracts, and systems limits their own chapters.
The compiler reference follows completed representations and their invariants,
then brings cross-cutting host and tooling responsibilities together.
Neither reference should acquire new language features merely because the long-term systems goal makes them desirable.

## Documentation drift

Disagreements, obsolete examples, and their follow-up actions are tracked in [the drift list](reference-drift.md).
Keep that editorial work separate from the reference text.

## Completed proposal retirements

The first two batches were accepted for consolidation on 2026-09-08.
The seven standalone proposals were removed after their durable material and incoming links were transferred:

| Retired proposal | Current homes | Preserved material |
| --- | --- | --- |
| `syntax.md` | [Language syntax](../references/language.md#2-lexical-structure-and-syntax), [values](../references/language.md#5-values-products-and-data), [computations](../references/language.md#6-computations-and-control); [style rationale](../proposals/style.md#reading-the-surface-syntax) | Accepted forms and grouping; the reasons for classifier-directed spines, arrows, and delimiters |
| `primitive-packages.md` | [Primitive identity](../references/language.md#13-primitive-values-and-capabilities); [package rationale](../proposals/package-modularization.md#primitive-identity-and-package-boundaries); [Builtin guide](../../lib/std/README.md#builtin-packages) | Canonical versus provider-owned identities, public carrier names, dependency boundaries, and current package use |
| `numeric-capabilities.md` | [Numeric library guide](../../lib/std/README.md#numeric-capabilities-and-explicit-instances) and its linked interface definitions | Dictionary composition, law boundaries, explicit selection, and checked manifest-instance examples |
| `local-inference.md` | [Classification and inference](../references/language.md#4-classification-and-inference), [source boundaries](../references/language.md#12-sources-imports-and-entry); [solver invariants](../../lang/statics/src/check/README.md#inference-regions-and-solver-invariants) | Binder defaulting, order-independent constraints, shape refinement, source closure, scope intersection, rollback, and diagnostic provenance |
| `typeof.md` | [Classification and inference](../references/language.md#4-classification-and-inference), [binder dependencies](../references/language.md#9-polymorphism-and-packages), [imports and signatures](../references/language.md#12-sources-imports-and-entry); [checked-term reuse](../../lang/statics/src/check/README.md#classifier-extraction-and-checked-term-reuse) | Classifier identity, staging, erasure, witness scope, import cycles, and canonical synthesis under context extension |
| `integer-literal-patterns.md` | [Pattern rules](../references/language.md#7-patterns-and-coverage), [numeric representations](../references/language.md#13-primitive-values-and-capabilities); [pattern implementation](../proposals/exhaustiveness.md#literal-and-alias-patterns) and [remaining decisions](../proposals/exhaustiveness.md#current-boundary) | Range and refutability boundaries, conservative coverage, equality-branch lowering, float equality, and redundancy questions |
| `aliasing.md` | [Pattern rules](../references/language.md#7-patterns-and-coverage); [syntax rationale](../proposals/style.md#pattern-alias-syntax); [pattern implementation](../proposals/exhaustiveness.md#literal-and-alias-patterns) and [remaining decisions](../proposals/exhaustiveness.md#current-boundary) | Shared bindees, ordered scope, composable syntax and alternatives, runtime sharing, and refutable-conjunction questions |

These destinations own the transferred material.
Discarded layout and implementation claims are recorded in the [drift list](reference-drift.md);
no compatibility proposal stubs remain.

## Deferred questions from retired proposals

- **Classifier-query editor affordances.** Decide whether editors should offer an action
  for constructing `@[typeof]` expressions.
  The source construct and the REPL's display-only `@[type]` command already have distinct meanings;
  an editor convenience would not change them.
- **Type and kind pattern aliases.** These remain outside the admitted pattern language.
  Their classification and scope need a separate decision if the feature is pursued;
  the value-pattern usefulness work does not implicitly authorize them.

## Proposed ownership after migration

For topics outside the completed transfers, the current DESIGN sections and owning proposals remain authoritative
while the reference draft is under review.
The table below assigns their eventual reference homes; it does not create a second normative definition now.
When a chapter replaces a rule, move the rule and replace its old detailed account with a link in the same change.

| Rule or contract | Eventual owner | Other accounts should contain |
| --- | --- | --- |
| Lexical scope and mobile binding placement | L3 | C4's dependency graph and resolution algorithm |
| Type identity, compatibility, and inference boundaries | L4 | C5's data structures, judgments, and solver invariants |
| Source computation and stack behavior | L6 | C7's evaluator and C9–C13's implementation mappings |
| Irrefutability, partiality, and source coverage policy | L7 | C6's checking and coverage algorithms |
| Value-function application and view meaning | L8 | C6's evaluator implementation and L10's common phase rule |
| Package witnesses, opening, and structural field search | L9 | Local applications in library guides; C5's evidence and lookup structures |
| Static elimination, erasure, and residual completeness | L10 | Construct examples in L8/L9/L11; enforcement in C6 |
| Monadic-block source translation | L11 | C6's algebra-translation implementation |
| Independent source synthesis and import sharing | L12 | C3/C4's loading, caching, and assembly machinery |
| Primitive and capability observations | L13 | Checked API documentation and C14's host adapters |
| Source FFI and trusted declaration obligations | L14 | C14's validated call plans and target adapters |
| Compiler identity, storage, and provenance | C2, with session retention in C3 | Each phase's use of those shared facilities |
| Residual reductions and demand | C8 | Later chapters' consequences for allocation and code shape |
| Product representation and local unboxing | C10 | Target-specific instruction selection and L15's qualified cost account |
| Native activation and continuation lifetime | C11 | C12's model transitions and collection integration |
| Runtime word and root contracts | C12 for native; C13 for Wasm | Generated ABI tables and L15's profile summary |

The formal calculus should remain one mathematical companion, linked from L4 and the relevant construct chapters.
Revise it to agree with the source account, and identify rule correspondences;
avoid maintaining two independently copied sets of formal typing rules in the books.
The source operational account in L6 is drawn from the interpreter and checked forms.

## Consolidation map

Move current rules and implementation descriptions into the designated chapters.
Keep independently useful motivations, alternatives, and unresolved decisions in their owning proposal.
If extraction leaves no independent design question or rationale,
remove the emptied document and update its incoming links.
An implemented proposal should not retain a second full reference account.

| Existing material | Reference destination | Material to retain separately |
| --- | --- | --- |
| [DESIGN](../../DESIGN.md) | Language model and mechanisms to L1–L14; architecture/runtime/tooling to C1–C15 | A concise project design overview and navigation to the two references |
| [term](../proposals/term.md) | L3–L4, L7, L12; C4–C5 | Binding and scope rationale where it explains actual tradeoffs |
| [Checker guide](../../lang/statics/src/check/README.md) | C5 | Local module ownership, linked to the accepted language rules |
| [value-pi](../proposals/value-pi.md) | L8; C5–C6 | Deferred syntax and residual-code sharing questions |
| [field projection](../proposals/field-projection.md), [package modularization](../proposals/package-modularization.md) | L9, L12–L13; C5–C6 | Independently reviewable package-interface questions; remove repeated shared rules |
| [normalization](../proposals/normalization.md) | Equality to L4, packages to L9, phase contract to L10, algorithms to C5/C6/C8 | Unresolved reduction and implementation-strategy questions |
| [exhaustiveness](../proposals/exhaustiveness.md), [type lint](../proposals/tyck-lint.md) | L7; C6/C16 | Coverage limits and remaining verifier work |
| [bytes](../proposals/bytes.md), [filesystem](../proposals/filesystem.md), [C FFI](../proposals/c-ffi.md) | L13–L15; C12–C14 | Builders, stream extensions, foreign exports/callbacks, and other unimplemented boundaries |
| [query-owned statics](../proposals/query-owned-statics.md), [arena reclamation](../proposals/arena-gc.md) | C2–C3/C5 | Reasons for the achieved query boundary and measured retention decisions |
| [demand analysis](../proposals/demand-analysis.md) | C8 | Any independent future demand-propagation question |
| [escape analysis](../proposals/escape-unboxing.md) | C10 | Stack products and interprocedural escape constraints |
| [native frames](../proposals/native-frames.md) | C11–C12; profile summary in L15 | Experimental environment designs and pending evaluation decisions |
| [Wasm strategies](../proposals/wasm-backends.md) | C13; profile summary in L15 | Default-target choice, alternatives, and historical comparison evidence |
| [completion](../proposals/completion.md), [typed rendering](../proposals/typed-type-rendering.md) | C15 | Incomplete recovery/ranking and source-rendering design questions |
| [formatting](../proposals/formatting.md) | L2 metadata index; C4/C15 | Independent layout-policy rationale; formatting workflow stays in CONTRIBUTING |
| [documentation](../proposals/documentation.md) | C15–C16 | Unresolved authoring/verification mechanisms; [authoring guide](../documentation.md) remains user-facing |
| [REPL](../proposals/repl.md) | L12; C3/C7/C15 | Future interaction design; command and key reference stays in CONTRIBUTING |
| [style](../proposals/style.md) | Links from L2 and the tutorial | A user-facing style guide, separate from accepted syntax and semantics |
| [data-driven cases](../proposals/data-driven-cases.md) | C16 | Harness design rationale; [case directives](../../lang/tests/cases/README.md) retain their local home |
| [source-map idea](../ideas/span-source-map.md) | C2 | Useful alternatives after removing the obsolete implementation account |
| [runtime evaluation](../ideas/cbpv-runtime-evaluation.md) | Evidence links in C11–C13/C16 | Reproducible studies and open comparisons with their original scope and revisions |
| [delimited-control idea](../ideas/delimited-continuations.md) | Context for L11 | Historical motivation; establish current behavior from library code and tests |
| [tutorial](../tutorial/zydeco-guide.md), [literate chapters](../spell), [library guide](../../lib/std/README.md) | Example and API sources throughout L | Distinct learning and library tasks, linked to reference owners for detailed rules |
| [older tutorial](../tutorial/intro_to_zydeco.md), [legacy material](../legacy) | Historical links only | Explicitly historical accounts, outside the current reference's authority |
| [scratch logs](../logs) | Audit for missing durable motivations | Fold unique durable material into its owner, then remove redundant scratch records |

The project-documentation proposal describes Zydeco's `@[doc]`, exposure,
and verification feature; its implementation belongs to C15.
Start with repository Markdown and the existing example-checking tools.

## Writing and migration sequence

The concise language draft covers L1–L15, and the first two proposal batches are retired as recorded above.
Compiler drafting and the remaining consolidation are still to be done.
Record drift and proposed repairs separately; retain existing owners for topics that have not been transferred.
The sequence below applies to those follow-ups.

1. **Establish the semantic spine.** Write L1/L3/L4/L6/L10/L12 and C1–C3 first.
   Resolve source-synthesis, identity, recursion, and phase discrepancies before dependent chapters reuse those rules.
2. **Complete the construct reference.** Write L2/L5/L7–L9/L11 and C4–C7.
   Audit the complete grammar and typed variants; update the formal calculus and link every rejection boundary
   to evidence.
3. **Document systems behavior and lowering.** Write L13–L15 and C8–C14.
   Verify each profile, ABI, allocation claim, and experiment against its actual implementation.
4. **Finish tooling and maintenance lookup.** Write C15–C16 and the appendices; connect existing authoring,
   library, editor, style, and contribution guides.
5. **Complete consolidation as chapters land.** Replace superseded rules with links, remove empty records,
   repair incoming references and stale examples, and shorten DESIGN into the agreed overview.
   Update AGENTS' project-reference and rule-ownership guidance when the references become authoritative.

When a replacement is accepted, migrate its source material and incoming links in the same change.
Keep the current owners until then.

## Chapter acceptance criteria

- All admitted constructs in the chapter's scope have syntax, classification, scope, and semantic coverage;
  rejected counterparts identify the intended error and relevant failure invariant.
- Language rules, algorithmic choices, implementation limits, and unimplemented proposals are visibly distinguished.
  A compiler behavior discovered during inspection is evidence to reconcile, not automatically a desired language rule.
- Every implementation chapter names its entry points and representations, establishes its phase invariants,
  and identifies the consumers and verifiers that rely on them.
- Representative examples reuse complete source terms with explicit imports.
  Use existing `zydeco check` / `zydeco reject=...` documentation fences for static examples;
  [the authoring guide](../documentation.md#verify-examples-explicitly) defines their contract.
  Runtime examples use the existing integration harness because documentation checking does not execute them.
- Each backend claim is tied to a current code path and suitable regression or a clearly labeled evidence gap.
  Performance measurements identify the workload, revision, profiles, and target.
- Reflow edited Markdown and validate local links and anchors.
  Run focused language tests when examples or rules change; outlines alone do not require the full Rust suite.
- Read the chapter in sequence to verify that concepts precede their use.
  Audit the corresponding old document and inbound links so every detailed rule has one authoritative home.
