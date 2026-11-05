# 03 — Stack IR (high SPS, normalizer, closure conversion, SPSLow, protocol, passes)

> Scratch study note for re-onboarding. Not canonical: contracts below point to their authoritative references.

## 1. Responsibility

`lang/stackir` implements the two calculi of the stack-passing-style paper: lexical **SPS** as `FragmentProgram`
and its first-order target **`SPS_l`** as `SpsLowProgram` ([README.md](../../../../lang/stackir/src/README.md):1-25;
[C8](../../../../docs/references/compiler.md#c8-sps-lowering-normalization-and-demand),
[C9](../../../../docs/references/compiler.md#c9-closure-conversion-and-first-order-spslow)).
The crate owns four jobs: lower one checked residual computation into SPS indexed by its consuming stack;
optionally normalize and localize that tree; closure-convert it into blocks, jumps,
and explicit closure/continuation packages; and validate the products at both frontiers.
It also owns the partial protocol extraction that lowering records and the low validators read,
the builtin-body materializer, and the pass-plan/pipeline types that select the optional stage.

One shared syntax family, arena, traversal, variable analysis,
and printer serve both phases; the phase modules add only their own control-flow forms
and rules ([src/README.md](../../../../lang/stackir/src/README.md):27-61,
[lib.rs:6-36](../../../../lang/stackir/src/lib.rs#L6-L36)).
`low/scalar.rs` additionally computes bounded scalar/memory regions for assembly and Wasm lowering;
the raw-kernel candidate for it is tracked in the native card under [appendix B2](../appendix-cleanup-candidates.md),
not here.

## 2. Position in the Pipeline

Upstream, static elimination leaves one checked residual computation plus the host Builtin signature.
`BackendProgram::lower_with_selection` passes that root through a `pipeline!`
whose first stage is `RootLowerer` or `BuiltinRootLowerer` and
whose second is `SpsLowPipeline` ([cli/src/compile.rs:331-375](../../../../cli/src/compile.rs#L331-L375)).
The lowering passes implement `CompilerPass<ss::CompuId>`
([high/lower.rs:367](../../../../lang/stackir/src/high/lower.rs#L367),
[:397](../../../../lang/stackir/src/high/lower.rs#L397)) and end
at `FragmentProgram::try_new` ([high/lower.rs:344](../../../../lang/stackir/src/high/lower.rs#L344)).

The two boundary products are concrete types:
`FragmentProgram` records its compiling scheme ([high/check.rs:15](../../../../lang/stackir/src/high/check.rs#L15)),
and `SpsLowProgram` freezes an `SpsLowArena`
with a scheme ([low/check.rs:12](../../../../lang/stackir/src/low/check.rs#L12)).
The pass plan and scheme are selected before lowering and stored with the product (`SpsSelection`
at [passes.rs:132](../../../../lang/stackir/src/passes.rs#L132); `BackendProgram`
at [cli/src/compile.rs:279-287](../../../../cli/src/compile.rs#L279-L287)).

Downstream, structured Wasm consumes `SpsLowProgram` directly, while ZASM/native consumers enter via `LoweringPipeline`
([C1 product table](../../../../docs/references/compiler.md#c1-architecture-and-a-programs-path-through-the-compiler),
[C10](../../../../docs/references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices)).
The references assign this position explicitly: `FragmentProgram` is produced by `BuiltinRootLowerer` and consumed
by selected SPS transformations; `SpsLowProgram` is produced by `SpsLowPipeline::run` and consumed by Wasm or assembly.
Source semantics behind the boundary belong to [L10](../../../../docs/references/language.md#10-static-elimination):
optional backend simplification does not relax source acceptance.

## 3. Entry Points and Key Types

| What | Where | Why it matters |
| --- | --- | --- |
| Root lowering | [high/lower.rs:158](../../../../lang/stackir/src/high/lower.rs#L158) `Lowerer`, [:172](../../../../lang/stackir/src/high/lower.rs#L172) `RootLowerer`, [:179](../../../../lang/stackir/src/high/lower.rs#L179) `BuiltinRootLowerer` | The only path from a checked root to `FragmentProgram`; carries scheme, protocols, provenance, errors. |
| Checked SPS product | [high/check.rs:15](../../../../lang/stackir/src/high/check.rs#L15), `try_new` [:48](../../../../lang/stackir/src/high/check.rs#L48), `FragmentValidator` [:82](../../../../lang/stackir/src/high/check.rs#L82), root-closure check [:219](../../../../lang/stackir/src/high/check.rs#L219) | `try_new` enforces lexical ownership, placement in the scheme's fragment, and procedure scope; the pipeline's `check` stage adds the closed-root assertion. |
| Scheme rules | [scheme.rs:11](../../../../lang/stackir/src/scheme.rs#L11) `SpsScheme`, [:59](../../../../lang/stackir/src/scheme.rs#L59) `check`, [:111](../../../../lang/stackir/src/scheme.rs#L111) `StackUse`, [:124](../../../../lang/stackir/src/scheme.rs#L124) `FrameShape` | One placement rule and one frame-shape classification shared by both phases. |
| SPS plan | [passes.rs:24](../../../../lang/stackir/src/passes.rs#L24) `SpsPass`, [:59](../../../../lang/stackir/src/passes.rs#L59) `SpsPlan`, [:164](../../../../lang/stackir/src/passes.rs#L164) `instantiate`, [:223](../../../../lang/stackir/src/passes.rs#L223) `SpsObserver` | Built-in catalog, owned selection, trace/verify/dump hooks. |
| Pass pipeline | [pipeline.rs:8](../../../../lang/stackir/src/pipeline.rs#L8), [:15](../../../../lang/stackir/src/pipeline.rs#L15) `with_optimizations`, [:25](../../../../lang/stackir/src/pipeline.rs#L25) sequence, [:38](../../../../lang/stackir/src/pipeline.rs#L38) default `run` | Required check → optional stages → required check → closure conversion. |
| Normalizer | [high/normalize.rs:29](../../../../lang/stackir/src/high/normalize.rs#L29), fold `Work` at [high/normalize/fold.rs:48](../../../../lang/stackir/src/high/normalize/fold.rs#L48), decision folder at [high/normalize/decision.rs:23](../../../../lang/stackir/src/high/normalize/decision.rs#L23) | Demand-driven rebuilding: β/η reductions, field pruning, known pattern decisions. |
| Localizer | [high/localize.rs:27](../../../../lang/stackir/src/high/localize.rs#L27) | Rewrites call-only closure bindings into `letproc`; revalidates at [:60](../../../../lang/stackir/src/high/localize.rs#L60). |
| Closure conversion | [low/convert.rs:22](../../../../lang/stackir/src/low/convert.rs#L22), `convert` [:98](../../../../lang/stackir/src/low/convert.rs#L98); label placement [low/convert/procedures.rs:1-11](../../../../lang/stackir/src/low/convert/procedures.rs#L1-L11) | Fresh low syntax, ordered captures, local/closed labels, packages. |
| Checked SPSLow product | [low/check.rs:12](../../../../lang/stackir/src/low/check.rs#L12), `try_new` [:57](../../../../lang/stackir/src/low/check.rs#L57), structural validator [:101](../../../../lang/stackir/src/low/check.rs#L101) | Always-on structural, entry-contract, and protocol validation before publication. |
| Entry contracts | [low/entry.rs:8](../../../../lang/stackir/src/low/entry.rs#L8) `EntryProtocol`, [:33](../../../../lang/stackir/src/low/entry.rs#L33) `EntryKind`, [:49](../../../../lang/stackir/src/low/entry.rs#L49) `EntryParameters`, [:69](../../../../lang/stackir/src/low/entry.rs#L69) `words` | Administrative environment/result words that every block declares and every jump supplies. |
| Shared syntax | [syntax.rs:156](../../../../lang/stackir/src/syntax.rs#L156) `Phase`, [:187](../../../../lang/stackir/src/syntax.rs#L187) `Value`, [:202](../../../../lang/stackir/src/syntax.rs#L202) `Stack`, [:214](../../../../lang/stackir/src/syntax.rs#L214) `Computation` | Both IRs are one generic family; phase forms live under `Ext`. |
| Shared arena | [arena.rs:32](../../../../lang/stackir/src/arena.rs#L32) `AdminArena`, [:24](../../../../lang/stackir/src/arena.rs#L24) `Construct`, [:111](../../../../lang/stackir/src/arena.rs#L111) `Nodes`, [:138](../../../../lang/stackir/src/arena.rs#L138) `Builder` | Builders, definition names, site/provenance maps shared by all passes. |
| Protocol extraction | [protocol.rs:29](../../../../lang/stackir/src/protocol.rs#L29) `ValueProtocol`, [:53](../../../../lang/stackir/src/protocol.rs#L53) `StackProtocol`, [:91](../../../../lang/stackir/src/protocol.rs#L91) `ProtocolGraph`; [protocol/source.rs:186](../../../../lang/stackir/src/protocol/source.rs#L186) | Partial source protocols plus canonical destructor tag numbering. |
| Builtin bodies | [builtin.rs:5](../../../../lang/stackir/src/builtin.rs#L5) `HostCallMode`, [:85](../../../../lang/stackir/src/builtin.rs#L85) `make_function` | Materializes intrinsic bodies for primitives, comparisons, offsets, memory steps. |

## 4. Contracts and Invariants

- **SPS exists only as a checked fragment.** `FragmentProgram` is constructed only
  after `FragmentValidator` checks single lexical ownership, procedure scope,
  and the placement rule of the recorded scheme ([high/check.rs:48](../../../../lang/stackir/src/high/check.rs#L48),
  [:82](../../../../lang/stackir/src/high/check.rs#L82); [placement rule](../../../../lang/stackir/src/scheme.rs#L59));
  root closure is a separate assertion that the pipeline's `check` stage runs before and
  after the optional passes ([high/check.rs:219](../../../../lang/stackir/src/high/check.rs#L219),
  [pipeline.rs:21-24](../../../../lang/stackir/src/pipeline.rs#L21-L24)).
  Canonical account: [C8 §SPS Analysis Traversal](../../../../docs/references/compiler.md#sps-analysis-traversal).
- **The scheme fixes where the stack is named.** Branch-join guards each value-coproduct match
  and scalar comparison with a stack let; duplicating copies the consumer per arm;
  full names every pushed frame ([scheme.rs:42-56](../../../../lang/stackir/src/scheme.rs#L42-L56),
  [high/copy.rs:13](../../../../lang/stackir/src/high/copy.rs#L13)).
  Canonical account: [C8 §Correspondence with SPS](../../../../docs/references/compiler.md#correspondence-with-sps).
- **Every selected pass preserves the product type and revalidates.** Normalization and localization end in
  `FragmentProgram::try_new` ([high/normalize.rs:158](../../../../lang/stackir/src/high/normalize.rs#L158),
  [high/localize.rs:60](../../../../lang/stackir/src/high/localize.rs#L60)); the pipeline brackets the optional stage
  with required checks ([pipeline.rs:21-31](../../../../lang/stackir/src/pipeline.rs#L21-L31)). Canonical account:
  [C1 §Built-In Plans and Phase Boundaries](../../../../docs/references/compiler.md#built-in-plans-and-phase-boundaries).
- **Normalization preserves the scheme and definition identities.** It allocates fresh syntax
  for the surviving tree, copies retained protocol evidence, and never computes βη normal forms
  or unfolds recursion ([high/normalize.rs:155-160](../../../../lang/stackir/src/high/normalize.rs#L155-L160);
  [consumer demands](../../../../lang/stackir/src/high/demand.rs#L8)).
  Canonical account:
  [C8 §Normalization Reconstruction](../../../../docs/references/compiler.md#normalization-reconstruction).
- **Closure conversion produces a closed, first-order program with explicit entry words.**
  `SpsLowProgram::try_new` runs the structural validator, the root-closure check, then the entry-contract
  and protocol validators ([low/check.rs:57-74](../../../../lang/stackir/src/low/check.rs#L57-L74);
  [low/contracts.rs:81](../../../../lang/stackir/src/low/contracts.rs#L81);
  [low/protocols.rs:66](../../../../lang/stackir/src/low/protocols.rs#L66)).
  Entry invariants: closure entries consume environment, continuation entries consume result then environment,
  and a jump supplies the matching word ([low/entry.rs:49-84](../../../../lang/stackir/src/low/entry.rs#L49-L84)).
  Canonical accounts:
  [C9 §Closure Conversion Folders](../../../../docs/references/compiler.md#closure-conversion-folders),
  [§Word Entry Contracts](../../../../docs/references/compiler.md#word-entry-contracts).
- **Protocol evidence is partial and agreement is nontransitive.** Unknowns are compatible
  with the word transport; only known conflicts are rejected,
  and the graph never records physical frame extent ([protocol.rs:1-6](../../../../lang/stackir/src/protocol.rs#L1-L6),
  [protocol/agreement.rs:1-4](../../../../lang/stackir/src/protocol/agreement.rs#L1-L4),
  [protocol/source.rs:331-334](../../../../lang/stackir/src/protocol/source.rs#L331-L334)).
  Canonical accounts: [C9 §Partial Source Protocols](../../../../docs/references/compiler.md#partial-source-protocols),
  [L6 Ret and Stack Extent](../../../../docs/references/language.md#ret-and-stack-extent).
- **No pass rewrites SPSLow.** Conversion is the last transformation;
  downstream phases only read the frozen program (the paper's `SPS_l` equations justify conversion
  without being oriented as passes;
  [C9 §Correspondence with `SPS_l`](../../../../docs/references/compiler.md#correspondence-with-sps_l)).

## 5. Data Flow

1. **Residual lowering.** The host Builtin package is planned and validated (`BuiltinPackagePlan::for_executable`),
   the lowering folder materializes its body, and the residual root is rebuilt while inheriting the consumer stack.
   `Work` frames hold unfinished parents, so residual depth does not consume the Rust stack
   ([high/lower/fold.rs:71](../../../../lang/stackir/src/high/lower/fold.rs#L71),
   driven at [:309](../../../../lang/stackir/src/high/lower/fold.rs#L309);
   shared driver [utils/fold.rs:36](../../../../lang/utils/src/fold.rs#L36)).
   This stage extracts partial protocols, canonicalizes codata tag indices, records builtin provenance,
   allocates fresh output syntax per input occurrence, accumulates residual-static errors, and ends
   at `FragmentProgram::try_new` ([high/lower.rs:343-344](../../../../lang/stackir/src/high/lower.rs#L343-L344)).
2. **Pass-sequence boundary.** `SpsLowPipeline::with_optimizations` builds
   `pipeline![check, optimizations, check, converter]`; `run` supplies the default `normalize,localize`
   ([pipeline.rs:21-44](../../../../lang/stackir/src/pipeline.rs#L21-L44)).
   The CLI uses that static composition for the default plan
   and an instantiated `PassSequence` for other plans; inspection wraps occurrences
   with `SpsObserver` ([cli/src/compile.rs:355-370](../../../../cli/src/compile.rs#L355-L370)).
3. **Normalization.** Producer facts flow into the tail; surviving consumers return `Absent`/`Fields`/`Used` demands;
   the folder discards, splits, or rebuilds each binding, resolves known calls and pattern decisions, and preserves the
   scheme's stack discipline ([high/normalize.rs:155-160](../../../../lang/stackir/src/high/normalize.rs#L155-L160)).
4. **Localization.** A bottom-up escape analysis finds closure bindings whose every use is a call
   in the binding's tail; the rewrite replaces them with `LetProc` and the calls with `CallProc` in place,
   then revalidates ([high/localize.rs:1-11](../../../../lang/stackir/src/high/localize.rs#L1-L11)).
5. **Closure conversion.** One traversal computes free-variable summaries; conversion allocates fresh low nodes,
   turns closures into `ClosurePackage` records, continuations into `ContinuationPackage` records, and procedures
   into local or closed labels ([low/convert.rs:27-101](../../../../lang/stackir/src/low/convert.rs#L27-L101),
   [low/convert/fold.rs:15](../../../../lang/stackir/src/low/convert/fold.rs#L15)).
6. **Publication and consumption.** `try_new` validates and freezes the program;
   `--target zir` renders it through the low printer, structured Wasm consumes it directly,
   and assembly lowering builds the ZASM graph from it ([cli/src/compile.rs:422](../../../../cli/src/compile.rs#L422),
   [low/fmt.rs:149-153](../../../../lang/stackir/src/low/fmt.rs#L149-L153)).

## 6. Verification Commands

Run from the repository root at `e2dd8979`.
The installed `zydeco` is stale, so use `cargo run --quiet --bin zydeco --`;
the runs below used `CARGO_TARGET_DIR=target/study-stackir`.
`-t zir` renders the published `SpsLowProgram`, not high SPS; high SPS appears only through `--dump-passes`.

| Purpose | Command | Expected observable outcome |
| --- | --- | --- |
| Discover passes and schemes | `cargo run --quiet --bin zydeco -- passes` | Exit 0; lists `normalize`/`localize` and `dup`/`br`/`full`, then the selection syntax. |
| Explain a selection | `cargo run --quiet --bin zydeco -- passes --sps-passes normalize,normalize --sps-scheme dup` | Prints `SPS scheme: dup`, `SPS selection: normalize,normalize`, then `check closed root` twice, two `normalize` occurrences, and `closure conversion (required)`. |
| Default pass plan | `cargo run --quiet --bin zydeco -- passes --sps-passes default` | Shows scheme `br`, `normalize[1]`, `localize[2]`, required checks, conversion. |
| Reject malformed plans | `cargo run --quiet --bin zydeco -- passes --sps-passes ""` | Exit 1, `empty SPS pass selection; use none to disable optional passes`. |
| Normalization effect | `cargo run --quiet --bin zydeco -- build lib/tests/core/fact.zy --target zir` versus `--sps-passes none` | 231 versus 6,372 output lines; 161 `[parameter:` lines in both; same figures as [02 §Step 4](../02-vertical-slice.md#step-4-why-normalization-matters), re-verified here. |
| Scheme comparison | `cargo run --quiet --bin zydeco -- build lib/tests/core/fact.zy --target zir --sps-passes none --sps-scheme dup` (also `full`) | 6,279 lines for `dup`, 8,144 for `full`; `br` is 6,372 (the no-pass default). |
| Procedure lowering | `cargo run --quiet --bin zydeco -- build lib/tests/core/procedures.zy --target zir` | 36 lines containing 1 `label` and 2 `closed label` bindings; 4 `pack-continuation` and 0 `pack-closure` in this fixture. |
| Protocol display | `cargo run --quiet --bin zydeco -- build lib/tests/core/codata-order.zy --target zir --sps-passes none` | 794 lines; two `[protocol:pN] codata { .read#0: cont(Int); .shift#1: Int :: cont(Int); }` definitions. |
| Inspect occurrences | `cargo run --quiet --bin zydeco -- build lib/tests/core/fact.zy --target zir --sps-passes normalize,normalize --trace-passes --verify-passes --dump-passes` | stdout 191 lines; stderr 5,264 lines beginning `before SPS normalize[1] (invocation 1)`; exit 0. |
| Backend-only flags are gated | `cargo run --quiet --bin zydeco -- run lib/tests/core/fact.zy --sps-passes none` | Exit 2, clap rejects `unexpected argument '--sps-passes'`; `check --sps-scheme dup` likewise. |
| Crate unit tests | `cargo test -p zydeco-stackir --lib --quiet` | `161 passed; 0 failed` in 0.42s (filtered: `normalize` 41 passed; `protocol` 34 passed). |

## 7. Reading Order

1. [C1 §Compiler Pass Composition](../../../../docs/references/compiler.md#compiler-pass-composition):
   typed passes, sequences, observers, built-in plans.
2. [L6 §Ret and Stack Extent](../../../../docs/references/language.md#ret-and-stack-extent)
   and [L10](../../../../docs/references/language.md#10-static-elimination): what stays unknown about stacks
   and the source/optimization boundary.
3. [lang/stackir/src/README.md:27-61](../../../../lang/stackir/src/README.md): the crate map in one page.
4. [C8 introduction and correspondence](../../../../docs/references/compiler.md#correspondence-with-sps):
   the three schemes and the SPS form table.
5. [scheme.rs](../../../../lang/stackir/src/scheme.rs#L11-L157)
   with [high/syntax.rs](../../../../lang/stackir/src/high/syntax.rs#L1-L127)
   and shared [syntax.rs](../../../../lang/stackir/src/syntax.rs#L156-L246): placement plus the generic forms.
6. [high/lower.rs](../../../../lang/stackir/src/high/lower.rs#L158-L411)
   and [fold.rs](../../../../lang/stackir/src/high/lower/fold.rs#L66-L71)
   against [C8 §Residual Lowering Folder](../../../../docs/references/compiler.md#residual-lowering-folder).
7. [high/check.rs](../../../../lang/stackir/src/high/check.rs#L15-L227)
   and [C8 §SPS Analysis Traversal](../../../../docs/references/compiler.md#sps-analysis-traversal):
   ownership, placement, procedure scope.
8. [high/normalize.rs](../../../../lang/stackir/src/high/normalize.rs#L1-L60),
   [demand.rs](../../../../lang/stackir/src/high/demand.rs#L1-L30),
   [decision.rs](../../../../lang/stackir/src/high/normalize/decision.rs#L1-L40),
   then [localize.rs](../../../../lang/stackir/src/high/localize.rs#L1-L80);
   [C8 §Normalization Reconstruction](../../../../docs/references/compiler.md#normalization-reconstruction),
   [§Consumer Demands](../../../../docs/references/compiler.md#consumer-demands),
   [§Second-Class Procedures](../../../../docs/references/compiler.md#second-class-procedures).
9. [low/convert.rs](../../../../lang/stackir/src/low/convert.rs#L1-L101)
   with [procedures.rs](../../../../lang/stackir/src/low/convert/procedures.rs#L1-L60)
   and [low/entry.rs](../../../../lang/stackir/src/low/entry.rs#L1-L100);
   [C9 §Correspondence with `SPS_l`](../../../../docs/references/compiler.md#correspondence-with-sps_l)
   and [§Word Entry Contracts](../../../../docs/references/compiler.md#word-entry-contracts).
10. [low/check.rs](../../../../lang/stackir/src/low/check.rs#L12-L110),
    [contracts.rs](../../../../lang/stackir/src/low/contracts.rs#L1-L100),
    [protocols.rs](../../../../lang/stackir/src/low/protocols.rs#L1-L80),
    [protocol/source.rs](../../../../lang/stackir/src/protocol/source.rs#L186-L376);
    [C9 §Partial Source Protocols](../../../../docs/references/compiler.md#partial-source-protocols).
    Then [passes.rs](../../../../lang/stackir/src/passes.rs#L1-L200),
    [pipeline.rs](../../../../lang/stackir/src/pipeline.rs#L1-L47),
    and [cli/src/compile.rs:331-430](../../../../cli/src/compile.rs#L331-L430).

## 8. Checkpoint Questions

1. Which type marks SPS as complete, which properties does its constructor check, and where is root closure checked?
   → [high/check.rs:48](../../../../lang/stackir/src/high/check.rs#L48),
   [:219](../../../../lang/stackir/src/high/check.rs#L219);
   [C8 §SPS Analysis Traversal](../../../../docs/references/compiler.md#sps-analysis-traversal).
2. What exactly differs between `br`, `dup`, and `full` at a value case split?
   → [scheme.rs:42-81](../../../../lang/stackir/src/scheme.rs#L42-L81);
   [C8 §Correspondence with SPS](../../../../docs/references/compiler.md#correspondence-with-sps).
3. What sequence does `SpsLowPipeline::with_optimizations` build, and where is the default plan? →
   [pipeline.rs:15-46](../../../../lang/stackir/src/pipeline.rs#L15-L46);
   [C1 §Built-In Plans and Phase Boundaries](../../../../docs/references/compiler.md#built-in-plans-and-phase-boundaries).
4. Why does normalization preserve the scheme instead of rebuilding a canonical form?
   → [C8 §Normalization Reconstruction](../../../../docs/references/compiler.md#normalization-reconstruction);
   [high/normalize.rs:155-160](../../../../lang/stackir/src/high/normalize.rs#L155-L160).
5. What is a second-class procedure, and why can it not be stored in a value?
   → [C8 §Second-Class Procedures](../../../../docs/references/compiler.md#second-class-procedures);
   [high/localize.rs:1-11](../../../../lang/stackir/src/high/localize.rs#L1-L11).
6. Which words does a closure entry consume versus a continuation entry, and which word does a jump supply?
   → [low/entry.rs:49-84](../../../../lang/stackir/src/low/entry.rs#L49-L84);
   [C9 §Word Entry Contracts](../../../../docs/references/compiler.md#word-entry-contracts).
7. Why is protocol agreement allowed to accept unknowns, and what does it deliberately not prove?
   → [protocol/agreement.rs:1-4](../../../../lang/stackir/src/protocol/agreement.rs#L1-L4);
   [C9 §Partial Source Protocols](../../../../docs/references/compiler.md#partial-source-protocols);
   [L6 §Ret and Stack Extent](../../../../docs/references/language.md#ret-and-stack-extent).
8. Where do runtime destructor tags come from, and why must producers and consumers agree?
   → [protocol/source.rs:331-334](../../../../lang/stackir/src/protocol/source.rs#L331-L334),
   [:362](../../../../lang/stackir/src/protocol/source.rs#L362);
   [high/lower/fold.rs:542](../../../../lang/stackir/src/high/lower/fold.rs#L542).
9. When does closure conversion emit a local label instead of a closed label?
   → [low/convert/procedures.rs:1-11](../../../../lang/stackir/src/low/convert/procedures.rs#L1-L11);
   [C9 §Correspondence with `SPS_l`](../../../../docs/references/compiler.md#correspondence-with-sps_l).
10. How do you select, explain, and inspect an SPS plan from the CLI, and why does `run` reject it?
    → [C1 §Selecting and Inspecting Passes](../../../../docs/references/compiler.md#selecting-and-inspecting-passes);
    [cli/src/cli.rs:258-276](../../../../cli/src/cli.rs#L258-L276).

## 9. Redundancy and Cleanup Signals

Status follows this card's template: `open` = removable after caller verification;
`decision needed` = an owner/design choice gates it; `resolved` = removed or settled.
The manual's [candidate inventory](../appendix-cleanup-candidates.md) uses a finer vocabulary (`open — code only`,
`open — reference gate`, `decision needed`, `retained`); rows cite appendix candidates where one exists.
Apply the [cleanup lens verification protocol](../04-cleanup-lens.md#verification-protocol-before-proposing-a-removal);
both retired studies call their findings proposals, not decisions.

| # | Signal | Evidence | Status | Assessment |
| --- | --- | --- | --- | --- |
| 1 | SPSLow validators look like scaffolding but are the only check that compiled output is well typed; decide keep, type the IRs, or delete. | [audit §validators](../../../proposals/artifact-boundary.md#backend-translation-validators); always-on at [low/check.rs:57-74](../../../../lang/stackir/src/low/check.rs#L57-L74); measured now: `low/check.rs` 763, `low/contracts.rs` 390, `low/protocols.rs` 651 lines, plus extraction; [appendix A5](../appendix-cleanup-candidates.md) | `decision needed` | Deleting with extraction is ≈3.5k–4.2k lines by the audit's estimate; the alternatives are a typed SPS/SPSLow IR (size analysis, appendix D3) or an unchecked pipeline. No backend reads their results, but they enforce the `SPS_l` typing argument. |
| 2 | Protocol extraction has no backend reader; one integration test and the ZIR printer consume the tables. | [size §types erased](../../../proposals/artifact-boundary.md#types-erased-and-then-re-derived) (only `stack_protocols.rs`, 619 lines); printer [low/fmt.rs:26-27](../../../../lang/stackir/src/low/fmt.rs#L26-L27), [:149-153](../../../../lang/stackir/src/low/fmt.rs#L149-L153); extraction non-test 1,054 lines by the record's split | `decision needed` | Not independently removable: high lowering takes canonical tags from the graph ([high/lower/fold.rs:542](../../../../lang/stackir/src/high/lower/fold.rs#L542), [:1052](../../../../lang/stackir/src/high/lower/fold.rs#L1052)), and the validators read the descriptors. It falls with row 1's typed-IR decision; deleting only the check leaves extraction feeding lowering and printing. |
| 3 | Explicit reconstruction frames (`Work`) versus plain recursion with `stacker::maybe_grow`. | [size §explicit frames](../../../proposals/artifact-boundary.md#explicit-reconstruction-frames); 97 variants at `6aae43d0` versus 126 now (41/41/44, counted with the retired study's variant regex); folder modules [lower/fold.rs:71](../../../../lang/stackir/src/high/lower/fold.rs#L71), [normalize/fold.rs:48](../../../../lang/stackir/src/high/normalize/fold.rs#L48), [convert/fold.rs:15](../../../../lang/stackir/src/low/convert/fold.rs#L15) total 4,026 production lines; no `stacker` dependency anywhere; `RUST_MIN_STACK = 16 MiB` at [.cargo/config.toml:5](../../../../.cargo/config.toml#L5) | `open` | [Appendix C](../appendix-cleanup-candidates.md) already lists it `open — code only (reference note needed)`; the change replaces the [resumable folder contract](../../../../docs/references/compiler.md#resumable-folder-execution), so a reference note must land with it. The ≈1.5k + ≈1.2k estimate predates the 29 added variants; re-estimate before planning. |
| 4 | High/low paired support modules duplicated syntax, arena, traversal, variables, fmt, and check scaffolding. | Size-analysis update paragraph dropped this finding as resolved ([record update](../../../proposals/artifact-boundary.md) top of record); commits `80f63893` (share syntax/traversal/summaries/printing), `e9e3a970` (share admin arena/builders/scope stacks), `5bd19491` (share site lookups/frame naming); current shared modules hold 1,194 production lines while phase modules are hooks | `resolved` | Replicating the record's paired-module method on this tree gives high 648 / low 787 / 339 verbatim-repeated lines, versus 943 / 992 / 474 at the measured revision; the residual repetition appears to be per-phase `impl` headers (inference). No action beyond normal review. |
| 5 | Dead `pats`/`terms` site maps record typed source sites that only the next phase's arena (and tests) copies along. | [size §deletions](../../../proposals/artifact-boundary.md#deletions-without-design-change); fields [arena.rs:43](../../../../lang/stackir/src/arena.rs#L43), [:46](../../../../lang/stackir/src/arena.rs#L46); readers are propagation only ([arena.rs:88](../../../../lang/stackir/src/arena.rs#L88), [:93](../../../../lang/stackir/src/arena.rs#L93); [high/copy.rs:250-273](../../../../lang/stackir/src/high/copy.rs#L250-L273); [low/convert/fold.rs:300](../../../../lang/stackir/src/low/convert/fold.rs#L300)); no reader outside `lang/stackir` | `open` | The cleanup lens already lists the maps as deletions-without-design-change ([lens](../04-cleanup-lens.md#current-snapshot-not-the-retired-studies-snapshot)); note the lens still points at `high/arena.rs`/`low/arena.rs`, but storage moved into the shared `AdminArena` (`arena.rs:43-46`). C8 states the retention ([C8 §Residual Lowering Folder](../../../../docs/references/compiler.md#residual-lowering-folder), lines 1594-1595), so remove or reword that clause in the same change. |
| 6 | The pass framework and `--sps-passes`/`--sps-scheme` options could look like a test-only surface. | Size-analysis update: the pass framework left the deletions because localization became its second pass ([record update](../../../proposals/artifact-boundary.md)); `SpsPlan` [passes.rs:59](../../../../lang/stackir/src/passes.rs#L59); schemes exercised across backends by [sps_schemes.rs:1-28](../../../../lang/tests/tests/sps_schemes.rs#L1-L28) | `resolved` | Kept deliberately: the default plan runs two passes, the scheme matrix test gives a maintained client, and the CLI options are documented in [C1](../../../../docs/references/compiler.md#selecting-and-inspecting-passes). Do not re-litigate without new evidence. |
| 7 | Raw scalar/memory kernel region IR under `low/scalar/` (fused loads, ≤32 operations, store closure). | [audit §test-only](../../../proposals/artifact-boundary.md#test-only-generality); `MAX_OPERATIONS = 32` at [low/scalar.rs:17](../../../../lang/stackir/src/low/scalar.rs#L17); consumed by assembly lowering; [appendix B2](../appendix-cleanup-candidates.md) | `open` | Cross-card item: the candidate is a native/assembly decision (fixed-slot versus region representation), so removal belongs with [04 — Native Backend](04-native-backend.md); this card only records that the code lives in `lang/stackir`. |

## 10. Uncertainties and Drift

- **Dated figures.** The retired cleanup audit measured `7e5ee4c1` and the retired size analysis `6aae43d0`;
  this tree is `e2dd8979`, 74 commits later.
  Re-measured here: `lang/stackir` is 19,090 lines by `wc -l` (12,786 production by the retired study's split,
  matching [appendix E](../appendix-cleanup-candidates.md)), versus 15,823 at `6aae43d0`;
  `Work` variants are 126 versus 97; validator-plus-extraction non-test lines are 2,304 versus the audit's ≈2,050.
  Do not quote the retired studies' removal deltas as current.
- **Frame-variant growth is undocumented.** The tree has 126 `Work` variants versus 97 at `6aae43d0`;
  the intervening commits include localization-by-default, the duplicating and full schemes,
  and procedure labels, but I did not bisect which commit added which frame.
  No study update paragraph records the increase,
  so the retired size analysis's ≈1.5k frame estimate should be re-derived before planning.
- **Paired-module measurement caveat.** The 339 repeated lines come
  from re-running the record's `report_folders`/`report_sps_pairs` logic on this tree.
  The retired script stopped earlier, in its support-module stage,
  because `lang/surface/src/textual/ugly.rs` was removed in `0f0f311f`;
  it never reached the folder and paired-module sections.
  Treat 339 as an order-of-magnitude check, not an official figure.
- **Cleanup-lens anchors are stale for site maps.** The lens names `high/arena.rs`/`low/arena.rs`;
  the storage now lives in the shared `AdminArena` ([arena.rs:43-46](../../../../lang/stackir/src/arena.rs#L43-L46)),
  with phase aliases in `high/arena.rs` and `low/arena.rs`.
  Likewise `appendix A5`'s "protocol extraction 1,214 lines" counts `protocol.rs` + `source.rs` + `agreement.rs` +
  `protocol/tests.rs`, not the two further test modules; `protocol/` now totals 1,587 lines.
- **Readers of provenance found by grep only.** Claims that no consumer reads `pats`/`terms`
  or protocol tables outside `lang/stackir` rely on tracked-source greps for `term_site`/`pattern_site`/`pats`/ `terms`
  and protocol types; generated code, macros, or a future backend consumer could falsify them.
- **Not run here.** No native (`-t exe`/`-t asm`) or Wasm (`-t wasm-sps`) execution,
  no full workspace suite, no evaluation reproduction script on a clean archive.
  Behavioral claims rest on the references and on the crate's 161 unit tests;
  the `fact.zy`/`procedures.zy` outputs above are rendering probes only.
- **Depth guarantees.** The workspace raises `RUST_MIN_STACK` to 16 MiB under Cargo,
  and depth fixtures run on 512 KiB worker stacks; I did not test the CLI main thread's platform-default stack
  or `--dump-passes` beyond the measured 5,264-line stderr run.
