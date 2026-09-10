# Reference plan

Zydeco needs two complementary references: an expert programmer's account of the language,
and a maintainer's account of the compiler and runtime.
The material exists across DESIGN, proposals, component guides, formal notes, library sources, and tests.
The writing task is to establish a coherent account with one home for each rule,
while preserving the rationale and unresolved questions that deserve separate design records.

Both references now have concise drafts covering their planned chapters:

| Reference | Reader's question | Document |
| --- | --- | --- |
| Language | Is this program accepted, what does it mean, and what execution assumptions does it require? | [Language reference](../references/language.md) |
| Compiler implementation | Where is this behavior implemented, what invariants connect the phases, and how can I change it? | [Compiler reference](../references/compiler.md) |

The investigation baseline is commit `77ee657f`, inspected on 2026-09-08.
The outlines draw on repository documentation, implementation entry points,
syntax and representation definitions, and regression tests.
This was a source inspection, not an exhaustive execution or conformance audit.
The language draft was subsequently checked against `289f2a14`; its opted-in examples use the documentation checker.
The compiler reference and this consolidation were inspected against `fdc4eeee`.
L1–L15 and C1–C16 identify the language and compiler chapters, respectively.

## Findings that shape the references

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

The language-reference batches and compiler-reference consolidation were requested on 2026-09-08.
Fifteen standalone proposals have been removed after transferring their durable material and incoming links:

| Retired proposal | Current homes | Preserved material |
| --- | --- | --- |
| `syntax.md` | [Language syntax](../references/language.md#2-lexical-structure-and-syntax), [values](../references/language.md#5-values-products-and-data), [computations](../references/language.md#6-computations-and-control); [style rationale](../style.md#reading-the-surface-syntax) | Accepted forms and grouping; the reasons for classifier-directed spines, arrows, and delimiters |
| `primitive-packages.md` | [Primitive identity](../references/language.md#13-primitive-values-and-capabilities); [package rationale](../proposals/package-modularization.md#primitive-identity-and-package-boundaries); [Builtin guide](../../lib/std/README.md#builtin-packages) | Canonical versus provider-owned identities, public carrier names, dependency boundaries, and current package use |
| `numeric-capabilities.md` | [Numeric library guide](../../lib/std/README.md#numeric-capabilities-and-explicit-instances) and its linked interface definitions | Dictionary composition, law boundaries, explicit selection, and checked manifest-instance examples |
| `local-inference.md` | [Classification and inference](../references/language.md#4-classification-and-inference), [source boundaries](../references/language.md#12-sources-imports-and-entry); [solver invariants](../references/compiler.md#inference-and-reuse) | Binder defaulting, order-independent constraints, shape refinement, source closure, scope intersection, rollback, and diagnostic provenance |
| `typeof.md` | [Classification and inference](../references/language.md#4-classification-and-inference), [binder dependencies](../references/language.md#9-polymorphism-and-packages), [imports and signatures](../references/language.md#12-sources-imports-and-entry); [checked-term reuse](../references/compiler.md#inference-and-reuse) | Classifier identity, staging, erasure, witness scope, import cycles, and canonical synthesis under context extension |
| `integer-literal-patterns.md` | [Pattern rules](../references/language.md#7-patterns-and-coverage), [numeric representations](../references/language.md#13-primitive-values-and-capabilities); [pattern implementation](../references/compiler.md#pattern-decisions-and-validation) and [remaining decisions](../proposals/exhaustiveness.md) | Range and refutability boundaries, conservative coverage, equality-branch lowering, float equality, and redundancy questions |
| `aliasing.md` | [Pattern rules](../references/language.md#7-patterns-and-coverage); [syntax rationale](../style.md#pattern-alias-syntax); [pattern implementation](../references/compiler.md#pattern-decisions-and-validation) and [remaining decisions](../proposals/exhaustiveness.md) | Shared bindees, ordered scope, composable syntax and alternatives, runtime sharing, and refutable-conjunction questions |
| `normalization.md` | [Package rules](../references/language.md#9-polymorphism-and-packages), [static elimination](../references/language.md#10-static-elimination); [finalization](../references/compiler.md#finalization), [static elaboration](../references/compiler.md#static-elimination), [SPS normalization](../references/compiler.md#c8-high-sps-lowering-normalization-and-demand) | Manifest formation and erasure, residual acceptance, shared graph normalization, and reductions preserving sharing and traps |
| `demand-analysis.md` | [Consumer demands](../references/compiler.md#consumer-demands) and the surrounding normalizer contract | Demand lattice, lexical propagation, physical suffixes, aliases, opaque consumers, and suspended captures |
| `query-owned-statics.md` | [Query/checker ownership](../references/compiler.md#query-and-checker-ownership); [memory design](../proposals/arena-gc.md) | Achieved producer boundary, stateful solver constraints, alternatives, and qualified historical cost evidence |
| `typed-type-rendering.md` | [Tooling implementation](../references/compiler.md#formatting-and-typed-rendering); [formatting choices](../proposals/formatting.md#elaborated-type-rendering) | Printer responsibilities, witness hints, source-shaped types, reification alternatives, and unresolved layout questions |
| `data-driven-cases.md` | [Fixture architecture](../references/compiler.md#source-fixtures-and-runtime-oracles); [case guide](../../lang/tests/cases/README.md) | Discovery, structured assertions, runner rationale, Rust-only relationships, and possible directive extensions |
| `field-projection.md` | [L9](../references/language.md#9-polymorphism-and-packages), [C5](../references/compiler.md#package-evidence-and-lookup), [package rationale](../proposals/package-modularization.md), [coverage extension](../proposals/exhaustiveness.md#refutable-projection-payloads) | Search namespace and opacity, shared openings, typed routes, and future refutable payloads |
| `value-pi.md` | [L8](../references/language.md#8-value-functions-and-views), [library recipes](../../lib/std/README.md#package-composition), [deferred designs](deferred-designs.md#value-views) | Value equations and view coherence; package idioms; refutable-view coverage, syntax, evaluation sharing, and residual factoring as todos |
| `repl.md` | [C15](../references/compiler.md#interactive-engine), [CONTRIBUTING](../../CONTRIBUTING.md#use-the-interactive-repl), [history todos](deferred-designs.md#repl-history-and-replay) | Numbered identity, retry, wrapper behavior, commands, and future persistence/replay |

These destinations own the transferred material. Discarded layout and implementation claims are recorded
in the [drift list](reference-drift.md); no compatibility proposal stubs remain.
The arena-retention, coverage, and typed-lint proposals now retain their design criteria
and open questions; the checker guide retains its module map.
Their current algorithms live in C3/C5/C6/C8/C16.
DESIGN's implementation overview links to those owners.

## Broader consolidation

The follow-up consolidation was inspected against `14e0410a` on 2026-09-08.
Two explorations and a completed scratch log were also removed: `span-source-map.md`
and its 2026-08-30 log transferred compact-position rationale and coordinate invariants to C2,
with remaining lookup/storage work in todos; `delimited-continuations.md` transferred its library-control motivation
and working examples to [the library guide](../../lib/std/README.md#relative-monads-and-control-examples).

[Style guidance](../style.md) moved out of proposals.
Term and package records now retain binding/identity and interface rationale.
Byte APIs and costs and filesystem operations moved to the library guide,
with representation and capability lifetime kept as separate designs.
The memory-backed Writer and byte builder has one home in the filesystem design.
C FFI now concerns exports, callbacks, ownership, reentry, and ABI extensions.
Completion, documentation, and formatting retain their recovery, provenance, publication,
and layout questions; completed roadmaps and parallel implementation accounts were removed.

Escape/unboxing, native frames, and Wasm remain independent representation, lifetime, and backend decisions.
Runtime recommendations moved to the native and Wasm records;
the dated [runtime study](../ideas/cbpv-runtime-evaluation.md) and every raw JSON/CSV evidence file remain together.

[Deferred designs](deferred-designs.md) is the single follow-up list for questions
from retired records, including refutable-view coverage.
Recording them there does not make them accepted language extensions.

## Reference ownership after migration

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
| [Checker guide](../../lang/statics/src/check/README.md) | C5; current account transferred | Local module ownership and links to shared contracts |
| [package modularization](../proposals/package-modularization.md) | L9, L12–L13; C5–C6 | Independently reviewable package-interface questions; remove repeated shared rules |
| [Coverage extensions](../proposals/exhaustiveness.md), [type lint](../proposals/tyck-lint.md) | C6/C8/C16; current account transferred | Usefulness, refutable patterns, dependent copatterns, and stronger verification evidence |
| [bytes](../proposals/bytes.md), [filesystem](../proposals/filesystem.md), [C FFI](../proposals/c-ffi.md) | L13–L15; C12–C14 | Builders, stream extensions, foreign exports/callbacks, and other unimplemented boundaries |
| [Arena retention](../proposals/arena-gc.md) | C3; current account transferred | Memo reclamation, compact facts, normalization strategies, and historical measurement limits |
| [escape analysis](../proposals/escape-unboxing.md) | C10 | Stack products and interprocedural escape constraints |
| [native frames](../proposals/native-frames.md) | C11–C12; profile summary in L15 | Experimental environment designs and pending evaluation decisions |
| [Wasm strategies](../proposals/wasm-backends.md) | C13; profile summary in L15 | Default-target choice, alternatives, and historical comparison evidence |
| [Completion](../proposals/completion.md), [formatting](../proposals/formatting.md#elaborated-type-rendering) | C15 | Recovery/ranking guarantees and source-rendering design questions |
| [formatting](../proposals/formatting.md) | L2 metadata index; C4/C15 | Independent layout-policy rationale; formatting workflow stays in CONTRIBUTING |
| [documentation](../proposals/documentation.md) | C15–C16 | Unresolved authoring/verification mechanisms; [authoring guide](../documentation.md) remains user-facing |
| [style](../style.md) | Links from L2 and the tutorial | A user-facing style guide, separate from accepted syntax and semantics |
| [runtime evaluation](../ideas/cbpv-runtime-evaluation.md) | Evidence links in C11–C13/C16 | Reproducible studies and open comparisons with their original scope and revisions |
| [tutorial](../tutorial/zydeco-guide.md), [literate chapters](../spell), [library guide](../../lib/std/README.md) | Example and API sources throughout L | Distinct learning and library tasks, linked to reference owners for detailed rules |
| [older tutorial](../tutorial/intro_to_zydeco.md), [legacy material](../legacy) | Historical links only | Explicitly historical accounts, outside the current reference's authority |
| [scratch logs](../logs) | Audit for missing durable motivations | Fold unique durable material into its owner, then remove redundant scratch records |

The project-documentation proposal describes Zydeco's `@[doc]`, exposure,
and verification feature; its implementation belongs to C15.
Start with repository Markdown and the existing example-checking tools.

## Remaining consolidation

The language and compiler drafts cover L1–L15 and C1–C16.
Drift and proposed repairs stay in the [separate list](reference-drift.md);
recording a mismatch does not authorize a language or compiler behavior change.

1. Consolidate the broader DESIGN language, runtime, and tooling sections into a shorter overview
   as their detailed owners are reviewed.
   Native frame alternatives, Wasm strategy comparisons,
   and tooling designs retain useful independent content beyond the reference summaries.
2. Reconcile the remaining formal-calculus rules with the source accounts,
   including field search, package-witness boundaries, and value matches.
   The tutorial refresh and n-ary product repair are recorded in the [drift list](reference-drift.md);
   the binary alternative is retained as a proposal.
   Keep mathematical rules in one companion rather than copying a second calculus into the references.
3. Audit historical measurements and remaining scratch records for durable evidence,
   then move that evidence to its owner.
   Reproduce claims used to select a default before presenting them as current results.

Deferred optimization ideas from the retired records include avoiding temporary construction
of unused high-SPS nodes and normalizing only consumer-requested closed types.
Either needs a measured compiler workload; the latter also belongs
to the [memory design](../proposals/arena-gc.md#open-questions).
Fixture argument/exit directives and typed-renderer reification remain in their receiving guides.

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
