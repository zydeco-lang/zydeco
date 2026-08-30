# WebAssembly Backend Strategies

Status: discussion draft, based on the two working prototypes as of 2026-08-30.

## Review Questions

Zydeco can now reach core WebAssembly through two different compiler boundaries. Before either target becomes
the unqualified `wasm` backend, the review should answer four questions:

1. Is first-order SPSLow (`SPS_l`) the right long-term boundary for structured WebAssembly generation?
2. Which parts of the runtime representation and host ABI must be shared by every WebAssembly backend?
3. Should the ZASM abstract-machine backend remain a semantic reference, a supported alternative, or only a
   temporary prototype?
4. What evidence is required before one implementation becomes the default `wasm` target?

The current names make those questions explicit. `wasm-am` means WebAssembly implementing the ZASM abstract
machine; `wasm-sps` means structured WebAssembly generated directly from SPSLow. There is deliberately no
compatibility alias named `wasm` while the default remains undecided.

## Why the Pipeline Forks at SPSLow

High SPS still contains higher-order code values and continuations whose runtime representation has not been
chosen. Compiling it directly would make the WebAssembly backend perform closure conversion, duplicating a
language-wide lowering decision inside one target. At the other end, ZASM has already decomposed lexical
computations into individual machine program points, so reconstructing structured code from it would be both
expensive and unreliable.

SPSLow is the narrow useful boundary between those extremes. Closure conversion has made functions and
continuations first order, with explicit environments and residual stacks, while block bodies still preserve
the lexical structure needed for ordinary WebAssembly instructions and locals. The two paths therefore share
all source-language lowering through SPSLow and diverge only at the backend strategy:

```text
surface -> statics -> dynamics -> high SPS -> SPSLow -> wasm-sps
                                                |
                                                +-> ZASM -> wasm-am
                                                         -> AMD64 / LLVM
```

This fork is a real pipeline distinction rather than two renderers over the same assembly. `BackendProgram`
owns SPSLow and lazily caches ZASM in a `OnceLock`; requesting `wasm-sps` never constructs an assembly program.
Assembly-derived targets continue to share one lowering result when they need it.

## Shared Module and Host Contract

Both prototypes emit deterministic core `wasm32` modules. They avoid the WebAssembly tail-call and typed
function-reference proposals, allowing the same modules to run in ordinary core WebAssembly engines. Each
module exports:

- `memory`, containing static strings and backend-managed runtime allocations;
- `entry`, the Zydeco program entry point; and
- `_start`, an alias with the same behavior as `entry`.

The generated module is not currently a standalone WASI program. An embedding supplies builtin functions in
the `zydeco` import namespace and invokes `entry` or `_start`.

### Runtime words

Source-level data crosses the host boundary as `i64` runtime words. An odd low bit denotes an immediate value;
an aligned even word is pointer-shaped. Small integers, characters, constructor tags, and the SPS backend's
block handles use immediate encodings. Full-width scalar values that do not fit the immediate representation
are boxed in linear memory.

The shared data convention does not require code addresses to have the same internal representation.
`wasm-sps` stores a tagged table index when a block handle occurs in a closure or continuation. `wasm-am`
keeps ZASM program counters as private, untagged table indices outside the source-level data representation.

### Imported functions

The compiler derives each builtin import signature from its typed builtin metadata rather than parsing names.
The forms shared by both backends are:

- A returning builtin accepts its Zydeco arguments as `i64` parameters and returns one `i64` word.
- A control builtin accepts its Zydeco arguments and returns four `i64` values: an untagged argument count,
  a module-created closure pointer, and up to two argument words. The count is currently limited to zero,
  one, or two.
- An operation that may produce a boxed 64-bit scalar receives a trailing `i32` address for a spare one-word
  box. Narrow operations pass zero when they share a signature containing that parameter.
- `string_literal(i32, i32) -> i64` receives a byte offset and UTF-8 length in exported memory and returns an
  opaque host string word.

Closures, continuations, products, and stack frames remain module-owned layouts. The host may return a closure
previously created by the module, but it should otherwise treat these pointers as opaque.

## `wasm-am`: ZASM as an Abstract Machine

The abstract-machine backend consumes `AssemblyProgram`. It assigns every ZASM program point a private table
index and emits one `() -> ()` WebAssembly function for that point. A trampoline in `entry` repeatedly performs
an indirect call through the table until the program counter signals termination.

The backend materializes the ZASM machine state in linear memory and globals:

- a reusable address-indexed environment for ZASM variables;
- a fixed one-megabyte operand and control stack;
- a program-counter global; and
- a growing bump heap for products, closures, and boxed scalars.

Direct jumps, dynamic continuation jumps, branches, and calls all become updates to the program counter. This
gives a close correspondence between the ZASM interpreter and the generated module and makes higher-order
control independent of the host call stack.

Its main strength is that it reuses an established lowering boundary. ZASM has already selected stack
operations, environment slots, product layout, and local-unboxing decisions, so the emitter primarily realizes
one explicit machine in WebAssembly. That also makes `wasm-am` a useful semantic reference for another backend.

The cost follows from the same correspondence. Every ZASM instruction boundary becomes a WebAssembly function
and table entry, even when several operations could have remained ordinary instructions in one structured
function. The mutable global environment also discards lexical locality that WebAssembly locals could retain.
Module size and validation work therefore scale with machine program points rather than source-level blocks.

## `wasm-sps`: Structured SPSLow Lowering

The structured backend consumes `SpsLowProgram` directly. It emits one WebAssembly function for the root and
one for each first-order SPS block. Ordinary computations within a block remain structured WebAssembly code;
linear computation chains are emitted iteratively, and lexical `DefId` bindings use WebAssembly locals.

Block-to-block control still uses a private function table and trampoline. Closures contain an environment and
a tagged block handle. Continuations contain a tagged block handle and their residual stack. The dispatcher
decodes the handle only at the indirect-call boundary, so recursive Zydeco control transfers do not grow the
host call stack.

SPS stacks use persistent bump-allocated frames of `[head, tail]`. A product is a contiguous boxed sequence,
and a partial product is represented by a pointer into its suffix. This matches SPSLow's ability to share a
residual stack across closures and continuations without copying or mutating it. Constructors use a two-word
`[tag, payload]` object, while closures and continuations use two-word packages with layouts determined by the
operation that consumes them.

This path retains considerably more structure than ZASM and avoids a mutable whole-program environment. Its
main implementation costs are now visible as optimization opportunities rather than semantic requirements:

- persistent frames and all other heap objects are bump allocated and never reclaimed;
- products are boxed uniformly instead of using ZASM's local-unboxing analysis; and
- the prototype assigns a uniform whole-program local-index plan to every block function, although only a
  subset of those locals is live in any one block.

Per-block liveness, escape-sensitive representation, or later WebAssembly-specific unboxing can improve those
details without changing the SPSLow boundary.

## Prototype Comparison

The following measurement compiled `lib/tests/compile/fact.zy` with both backends. It is one directional size
measurement, not a runtime benchmark, but it exposes the structural difference clearly.

| Property | `wasm-am` | `wasm-sps` |
| --- | ---: | ---: |
| Backend input | ZASM `AssemblyProgram` | `SpsLowProgram` |
| Defined WebAssembly functions | 13,935 | 525 |
| Module size | 713,838 bytes | 222,451 bytes |
| Assembly construction required | yes | no |
| Lexical bindings | memory environment | WebAssembly locals |
| Control stack | fixed one-megabyte region | persistent heap frames |
| Product representation | ZASM-guided, with local unboxing | uniformly boxed |

For this input, preserving SPS block structure reduces the emitted module by about 69 percent and reduces the
number of defined functions by about 96 percent. These figures motivate the higher-level path, but they do not
yet establish execution speed, peak memory, or behavior on a representative program corpus.

## Chosen Invariants

The prototypes already establish several decisions that should survive implementation changes:

1. Backend names identify their lowering boundary. Artifacts are correspondingly named `.am.wasm` and
   `.sps.wasm`, so side-by-side builds cannot overwrite each other.
2. Both backends expose the same module exports and typed host-import forms. A host should choose a module
   without reimplementing builtin semantics.
3. The direct SPS path must remain independent of ZASM. Shared helper code is appropriate; hidden assembly
   construction would erase the experiment it is meant to conduct.
4. Dynamic source control uses a module-owned trampoline. Neither backend relies on host-stack recursion or a
   non-core WebAssembly proposal.
5. Runtime data and backend-private dispatch state are distinct concepts, even when both ultimately contain
   function-table indices.
6. Emission is deterministic for the same compiler input. Plans sort arena-derived identifiers before assigning
   function indices, import indices, locals, and static-data offsets.
7. Unsupported source or IR forms produce typed emission errors instead of silently choosing placeholder code.

These are interface and correctness invariants. Fixed stack size, uniform boxing, a particular heap layout,
and the current local plan are prototype policies and remain open to replacement.

## Validation Evidence

The current implementations have the following evidence:

- both emitted modules pass `wasmparser` validation in focused backend tests;
- focused CLI tests verify the distinct target names, suffixes, module validity, and relative function counts;
- the committed Node.js host runs 126 existing source cases on each backend, for 252 WebAssembly executions;
- that corpus includes the compile tests, examples, the `exec` suite, OOPSLA artifacts, effects, packs,
  stack/control cases, tutorial programs, and standard-library collection, text, numeric, argument-list,
  and filesystem tests;
- the cases cover direct calls, higher-order closures, tuples and product suffixes, match/comatch,
  cloned continuations, string and byte operations, every numeric width, host control transfers,
  and persistent-stack allocation stress;
- signed and unsigned tagged-word boundary cases execute successfully;
- the 4,096-iteration allocation stress program passes on both backends; an earlier SPS measurement grew
  to 85 WebAssembly pages, approximately 5.5 MiB; and
- two independent SPS builds of `fact.zy` produced identical bytes
  (`84ca8e63a78995214b941649570695fdf6a3ffd9796d20eca14efd47340b24cf`).

The source harness applies the same successful-exit oracle to the interpreter, AMD64, and both WebAssembly
backends. This is now a committed cross-backend runtime corpus, although it does not yet compare captured output
or cover the argument-fold limitation below. The stress result demonstrates growth rather than an acceptable
long-running memory policy.

## Known Limitations

- A `zydeco` host embedding is required; the CLI builds modules but does not execute them.
- The committed Node.js host is a test embedding, not yet a packaged application runtime.
- Both heaps grow without collection. SPS persistent frames make allocation proportional to dynamic stack use,
  while the abstract machine instead has a bounded operand/control stack.
- The modules target `wasm32`, so addresses and module-owned allocation are limited to 32 bits.
- Host control transfers currently carry at most two arguments.
- An `arg_fold` over two or more process arguments requires the host to create a lazy Zydeco tail closure.
  The native runtime has a private host-to-Zydeco bridge for this, but the shared WebAssembly ABI cannot
  currently express it. Empty and singleton folds work; the two-argument fixture remains outside the
  WebAssembly execution corpus.
- Full-width scalar boxing and spare-box ownership are part of the host contract but are not yet independently
  versioned or described by a generated interface definition.
- The SPS backend currently gives every case the whole-program local plan and uniformly boxes products.
- The abstract-machine backend's fixed stack can overflow on programs whose machine stack exceeds one megabyte.
- Runtime performance, compilation time, validation time, and peak memory have not been measured across a
  representative corpus.

## Alternatives Considered

### Start from high SPS

High SPS preserves even more structure, but it has not yet selected explicit closure and continuation layouts.
A backend starting there would either duplicate closure conversion or make WebAssembly-specific choices that
other targets could not reuse. SPSLow retains the useful structure after that shared semantic decision.

### Recover structure from ZASM

Grouping ZASM points back into WebAssembly functions would require discovering lexical regions and stack
effects after they have been erased. It could reduce the abstract-machine module mechanically, but would not
test whether SPSLow is a better compiler boundary.

### Use direct recursive indirect calls

Calling the next block directly is simpler than maintaining a program counter, but unbounded Zydeco recursion
would then consume the engine's native call stack. A trampoline keeps source control behavior independent of
engine stack limits.

### Require tail calls or typed function references

WebAssembly proposals could express dynamic tail transfer more directly. Making them mandatory would narrow
the set of usable engines before measurements show that the core trampoline is a bottleneck. They remain a
possible optional target feature rather than a baseline requirement.

### Choose an unqualified `wasm` target now

Aliasing one prototype now would turn an architectural experiment into a compatibility promise. Explicit names
make scripts state which lowering they rely on and let both artifacts coexist until the evidence supports a
default. When a default is selected, the transition should be direct: add `wasm`, update callers, and document
whether the non-default implementation remains supported, without retaining an ambiguous legacy alias.

## Criteria for Selecting `wasm`

Correctness is the first gate. The committed source corpus now covers recursion, numeric boxing boundaries,
products, closures, continuations, match/comatch, strings, and most builtin calling modes against the same
successful-exit expectations as the interpreter. Completing the gate still requires multi-argument process
folds, nonempty input fixtures, observable-output comparison, panics, and expected nonzero exits.

After semantic parity, selection should use measured engineering properties:

1. module byte size and defined-function count across representative programs;
2. backend compile time and engine validation/instantiation time;
3. execution time for computation-heavy, allocation-heavy, and control-heavy programs;
4. peak and retained linear memory under long-running workloads;
5. quality and coverage of typed unsupported-form diagnostics; and
6. stability and implementability of the shared host contract.

If the structured backend preserves semantic parity and its current structural advantage survives those
measurements, `wasm-sps` is the natural candidate to become `wasm`. `wasm-am` can then remain a reference and
diagnostic backend if its semantic correspondence continues to justify its maintenance cost. If SPS-specific
runtime complexity causes correctness or memory regressions, the abstract machine remains a complete fallback
rather than a discarded experiment.

## Open Questions

- Should persistent SPS frames be reclaimed by tracing, regions, a shadow stack, or a different continuation
  representation?
- Should product unboxing be decided in SPSLow, in a shared analysis after SPSLow, or independently per backend?
- Is the low-bit runtime-word encoding the stable host ABI, or only an internal convention that should be hidden
  behind canonical ABI adapters?
- Should control builtins return a typed transfer record in linear memory rather than four multi-values, removing
  the two-argument limit?
- Should process arguments use an indexed returning ABI, or should WebAssembly gain an explicit bridge for
  module-compatible host-created closures?
- Does `_start` remain a convenience alias, or should a WASI adapter own process-style startup and exit behavior?
- Which runtime layouts belong in a shared WebAssembly support crate, and which should remain backend-private?
- If `wasm-sps` becomes the default, is `wasm-am` valuable enough to support publicly or only as a testing oracle?

## Proposed Next Steps

1. Resolve multi-argument process enumeration without requiring an unrepresentable host-created closure.
2. Add nonempty input fixtures to both WebAssembly backends.
3. Compare captured output and expected failure behavior, rather than only successful exit status.
4. Measure the committed corpus before optimizing either backend, including compile, validation, execution, and
   linear-memory metrics.
5. Replace the SPS whole-program local plan with per-block locals and measure the effect.
6. Design a bounded or reclaiming allocation policy, especially for persistent SPS stack frames.
7. Decide which ABI details are stable enough to version and expose to external hosts.
8. Apply the selection criteria, then introduce the unqualified `wasm` target in one clean transition.

## Implementation Map

- `lang/wasm-am/src/emit.rs`: ZASM abstract-machine planning, layout, and emission.
- `lang/wasm-sps/src/emit.rs`: direct SPSLow planning, structured block emission, and persistent stacks.
- `cli/src/compile.rs`: the shared SPSLow product and lazy ZASM cache.
- `cli/src/cli.rs`: explicit target names.
- `lang/tests/wasm-host.mjs`: the shared Node.js test embedding.
- `lang/tests/src/lib.rs`: cross-backend source-corpus execution.
- `DESIGN.md`: canonical pipeline and current shared host ABI.
