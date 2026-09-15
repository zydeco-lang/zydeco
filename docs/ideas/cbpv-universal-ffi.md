# Towards a Universal FFI: Typed, Bidirectional Interoperability with CBPV

Research proposal, 2026-09-15. The calculus, guarantees, and experiments below are proposed work.
The [existing foundation](#5-existing-foundation) identifies implemented results;
the [references](../references/README.md) remain authoritative for current behavior.

## Abstract

Interoperability requires agreement about both data and control: how values are represented,
how arguments reach code, where results return, and which runtime keeps resources alive.
We propose investigating call-by-push-value (CBPV) as a language for expressing and compiling these agreements.
CBPV separates values from computations and gives computation types an interpretation as stack protocols.
Our hypothesis is that this structure, combined with explicit representation contracts and target ABI profiles,
can support reusable, typed adapters with Zydeco acting as either caller or callee.
The research will define a small boundary calculus, formulate conditional correctness results,
and evaluate bidirectional adapters against manually written glue.
Zydeco's scalar foreign calls, native compilation units,
and checked representation conversions provide a working foundation.
Callbacks, adapter composition, and general interoperability guarantees remain research goals.
Here, universality means an extensible framework whose supported conventions and assumptions are explicit.

## 1. Motivation and research question

A foreign function interface (FFI) makes another language's operations usable from a program.
An application binary interface (ABI) supplies the concrete agreement that makes those calls possible.
The same logical operation may cross a platform C ABI, a language runtime's closure convention,
or a virtual machine's component interface.
Its implementation can be called directly, invoked through a callback, or reached through a continuation.
Every boundary must reconcile the participating representations and protocols.

This motivates an executable interoperability language: programmers describe the agreements
and write adapters between them, while a compiler checks and lowers those adapters.
Zydeco would occupy this role without requiring that it own the application or either component's implementation.
Its native ABI would be one supported profile among others.

CBPV is a candidate foundation because its value/computation distinction exposes evaluation
and control structure ([Levy, 2003](https://doi.org/10.1007/978-94-007-0954-6)).
Zydeco's existing work develops computation types
as stack protocols ([Jiang et al., 2025](https://doi.org/10.1145/3720434)).
The research question is:

> Can CBPV, extended with explicit representation and ABI contracts, support compositional, bidirectional
> interoperability while preserving the participating interfaces' data and control protocols?

The proposed contribution is a connection between typed control, representation conversion, and executable glue.
Expressing machine calling conventions and typing unboxed values already have substantial prior work.
Section 7 identifies the comparisons needed to establish what this connection adds.

## 2. Running example: a buffer visitor

Consider this interface, already represented by a [C specimen](../../lib/tests/ffi/contracts.c):

```c
typedef int64_t (*sample_step)(void *context, int64_t value);

int64_t sample_visit(const int64_t *values, size_t length,
                     sample_step step, void *context);
```

The visitor calls `step` on each element in order and sums its results.
Its contract requires readable, suitably aligned storage for `length` elements throughout the call.
The callback and context are used synchronously on the calling thread and are not retained.
For this C specimen, every addition must remain within the signed 64-bit range.
These conditions make the example's intended behavior precise without relying on C signed overflow.

A returning CBPV boundary interface could have this shape:

```text
Step  = Thk (Int64 -> Ret Int64)
Visit = Thk (I64Buffer -> Step -> Ret Int64)
```

This is schematic notation. `I64Buffer` names an abstract buffer interface for the study;
it is not a current Zydeco type or proposed final syntax.
`Thk` makes a computation available as a value, and `Ret` describes its ordinary return protocol.
The type exposes callback invocation and return, but does not itself establish non-retention, callback multiplicity,
buffer validity, or termination.

CBPV also lets the source implementation abstract over the caller's remaining computation protocol.
Following Zydeco's [public CPS convention](../references/language.md#ret-and-explicit-cps),
an effectful visitor and callback could instead expose:

```text
StepCps  = Thk (forall (R : CType) . Int64 -> Thk (Int64 -> R) -> R)
VisitCps = Thk (forall (R : CType) . I64Buffer -> StepCps -> Thk (Int64 -> R) -> R)
```

Here the caller chooses `R` and supplies the continuation receiving the result.
An export to C must connect this protocol to a bridge that returns
to C. This makes the relationship between a source continuation and a foreign return an explicit part of the adapter.
Whether that structure simplifies composition is a central claim to test.

The study will implement both directions:

- **Zydeco calls the C visitor.** An adapter supplies the buffer address and length,
  creates a foreign callback entry, and retains the capturing Zydeco thunk as a live GC root.
  Each callback converts its raw argument to `Int64`, invokes the thunk, and converts the result back
  before returning to C.
- **C or Rust calls a Zydeco visitor.** An exported adapter receives the same C signature
  and supplies a logical callback whose invocation calls the foreign `sample_step`.
  The source visitor controls iteration; the adapter returns the final result using the declared C convention.

This example connects representation changes with nested control transfers.
A callback may allocate and trigger collection while the enclosing foreign call is active,
so correct numeric conversion alone is insufficient.
The C specimen currently exercises C callers; the corresponding Zydeco foreign callbacks remain unimplemented.

## 3. Proposed approach

### Separate the agreements, then connect them

A computation type describes an abstract stack protocol.
It does not determine machine registers, aggregate classification, or foreign pointer validity.
The proposed boundary description will connect five complementary agreements:

| Agreement | Question it answers | Visitor example |
| --- | --- | --- |
| Logical interface | Which values and operations are exchanged? | An integer buffer and a callback |
| Representation | Which storage represents each value? | Contiguous C integers or ordinary Zydeco scalar boxes |
| Physical calling convention | How are arguments, results, and machine state transported? | Argument registers, stack alignment, result register, preserved registers |
| Control protocol | Which transfers are permitted, and where do they return? | Repeated synchronous callback calls, each returning to the visitor |
| Runtime obligations | Who maintains resources during those transfers? | Buffer owner, rooted closure environment, active runtime instance |

An **ABI profile** will supply target-specific transport rules and identify its required runtime services.
The compiler will resolve and validate each call's signature before emitting the adapter.
Existing static packed values and value functions are candidates for assembling descriptions;
whether the current type system can express the necessary evidence is an explicit research question.
The core language may need additional boundary or resource constructs.

### Imports, exports, and composition

An import adapter presents a foreign entry through a logical CBPV interface.
An export adapter presents a CBPV implementation through a foreign entry.
Their conversions are directional: importing a result and exporting an argument may require different checks,
allocation, copying, or resource transfer.
Equal storage sizes do not make the signatures interchangeable.

For two compatible profiles, glue can import an implementation from one and export it through the other.
The intended benefit is reuse of interface descriptions and conversion components across these compositions.
Composition must connect the same logical meaning and satisfy the intermediate resource obligations.
It must also preserve declared failures and callback ordering; matching argument types alone is insufficient.
No claim that every language pair has a lossless adapter is required.

The compiler may specialize fixed descriptions and remove redundant conversions where their laws justify it.
This is a performance hypothesis to measure.
A general logical representation need not become an allocated intermediate object at every foreign call.

### A first conversion law

Representation correctness can begin with the `Int64` boundary that already exists in Zydeco.
Relate an abstract integer to both its ordinary scalar box and its raw 64-bit transport.
For a valid integer `n`, successful boxing followed by unboxing should return `n` exactly.
Unboxing a valid box and boxing the result should preserve the integer observed through the scalar interface;
the new allocation need not preserve the original address.

The generalization is a representation relation indexed by type, profile, and relevant runtime state.
For buffers, that state includes the accessible storage and its lifetime.
For closures, it includes the captured environment and its roots, even when collection changes their addresses.
An adapter must preserve this relation across each transfer.
Primitive round trips provide useful lemmas and tests, while callbacks require reasoning about subsequent execution.

## 4. Research questions and correctness targets

**RQ1: Which interoperability protocols can a small CBPV extension express compositionally?** Begin
with returning calls, capturing synchronous callbacks, and source computations using explicit continuations.
Determine which guarantees follow from existing types, which need additional typing rules,
and which remain assumptions about foreign code.
An encoding that requires a new unchecked primitive for every example would weaken the proposed abstraction.

**RQ2: What correctness result can be established for adapters and their composition?** Define an
operational boundary semantics and observable traces of calls, callbacks, returns, and declared failures.
The initial theorem target is conditional: given related inputs,
valid runtime resources, and foreign components satisfying their declared contracts,
a checked adapter preserves the interface's permitted control traces
and relates each observable result to its logical meaning.
The composition theorem should reuse the constituent adapters' obligations rather than re-prove an entire language pair.

These are proposed results, not properties established by the current compiler.
The initial argument can assume adequate allocation resources and model terminal runtime faults separately.
Target instruction selection, runtime primitives, and foreign implementations form an explicit trust boundary.
Source type checking cannot certify arbitrary foreign machine code.

**RQ3: Does the organization improve reuse without imposing unavoidable overhead?** Compare the approach
with equivalent hand-written glue and a typed CPS or typed ABI description language.
Keep boundary contracts and target machinery comparable so the study isolates the contribution of CBPV's structure.
The comparison should identify reusable definitions, proof obligations, rejected mistakes, and residual runtime work.
CBPV's contribution must be visible in those differences; implementability alone would not establish it.

## 5. Existing foundation

The repository supplies preliminary engineering evidence, with each current contract owned by its reference section:

| Foundation | Evidence and limit |
| --- | --- |
| Foreign entry and exit | [C imports and scalar exports](../references/language.md#14-foreign-interfaces) exercise concrete conversions. Foreign callbacks and general exported values remain deferred. |
| Separate compilation | [Native unit artifacts](../references/compiler.md#native-unit-artifacts) carry explicit structural interfaces and support source-free composition. They do not provide a persistent foreign host API. |
| Representation discipline | [Scalar boundaries](../references/compiler.md#scalar-value-boundaries) distinguish ordinary values from raw bits and verify local scalar programs. This is bounded compiler validation, not an interoperability soundness theorem. |
| Compilation costs | The [memory-abstraction study](../evaluations/2026-09-14-memory-abstractions/README.md) and [raw-memory-kernel study](../evaluations/2026-09-15-memory-kernels/README.md) record reproducible code-site counts. They establish neither general FFI overhead nor runtime speedups. |

The concrete [foreign-interface proposal](../proposals/c-ffi.md), [Zydeco ABI proposal](../proposals/zydeco-abi.md),
and [Rust host survey](../proposals/rust-host.md) own implementation options and staging.
This research proposal asks what general account those directions can support.

## 6. Evaluation plan

Evaluate expressiveness, correctness, and cost independently.
The first target is one native platform with its C ABI and Zydeco's native convention.
Rust consuming the C interface increases language coverage but does not add another ABI profile.
A subsequent target should vary physical transport, including aggregate argument or result classification,
without rewriting the logical visitor.

| Study | Positive case | Counterpart or rejection case | Evidence |
| --- | --- | --- | --- |
| Scalar transport | Signed and unsigned full-width round trips, plus bit-preserving float transport | Type/representation mismatch and out-of-range tagged integer conversion | Boundary tests and agreement with an independent C/Rust implementation |
| Bidirectional visitor | Capturing callback, empty/nonempty buffer, collection during callback | Incompatible callback signature; invalid checked extent; expired or wrong-instance managed handle where exposed | Matching call/return traces and results; rejection before foreign entry where promised |
| Adapter composition | Compose imports and exports, then link separately compiled units | Incompatible profile, interface, or runtime identity | Successful composition and rejection at the declared compilation/link boundary |
| Physical ABI coverage | Equivalent interfaces under distinct target transport rules | Equal-size aggregates requiring different transport | Agreement with platform compiler output and executable cross-language tests |
| Cost and reuse | Fixed adapters specialized from shared descriptions | Optimized hand-written glue; the same generated adapter with specialization disabled | Runtime overhead, executed allocations, code size, and an inventory of reusable versus target-specific code |

Unsafe foreign violations require care in interpretation.
A raw invalid pointer or a C callback retained against its contract may be undetectable;
the proposal must state that assumption rather than count such behavior as safely rejected.
Managed-handle checks should have explicit failure invariants, including no invocation after rejection.

Measurements will record compiler revision, target, optimization settings,
runtime configuration, workload size, repetitions, and variation.
Separate callback work from boundary overhead, and measure cold setup separately from repeated calls.
Static allocation sites and executed allocations are different metrics.
Use UniFFI or Component Model adapters as practical comparisons only where their interface semantics match.

## 7. Related work and the novelty test

**CBPV and representation.** Levy's CBPV provides the value/computation decomposition.
Jiang et al. connect stack-manipulating computation, typed stack protocols, and relative monads in Zydeco.
Downen's [Call-by-Unboxed-Value](https://doi.org/10.1145/3674654) develops explicit boxed/unboxed representations
and higher-order calling conventions while preserving types through compilation.
The proposed work must explain how it builds on these results to express and verify foreign boundaries,
including their resource and control assumptions.

**Calling conventions and runtime interfaces.** Blume, Rainey, and Reppy (2008) generate variadic calling machinery
from [declarative state-machine descriptions](https://www.andrew.cmu.edu/user/mrainey/papers/ml-varargs.pdf).
C-- connects a portable compiler target with a runtime interface for services such
as garbage collection ([Peyton Jones et al., 1999](https://www.cs.tufts.edu/~nr/pubs/c--gc-abstract.html)).
These systems establish that describing transport and separating runtime responsibilities are prior ideas.
The proposed contribution concerns their integration with typed logical protocols and composable adapters.

**Semantic interoperability.** [Patterson et al. (2022)](https://arxiv.org/abs/2202.13158) define sound interoperability
after compilation through type-convertibility relations, target glue, and semantic models.
That framework is a direct starting point for the correctness argument.
The open question is what an executable CBPV boundary calculus contributes to the expression, composition,
and implementation of those conversions.

**Binding and component systems.** [UniFFI](https://mozilla.github.io/uniffi-rs/latest/) generates
foreign-language bindings around Rust components, including callback interfaces. The WebAssembly Component Model's
[Canonical ABI](https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md) specifies lifting
and lowering between component interfaces and Core WebAssembly.
Both are relevant comparisons for interface reuse and conversion machinery.
Zydeco's ambition is to let programmers express multiple ABI profiles and adapters in either direction;
the study must demonstrate the additional protocols or reusable structure this permits.
The existing [UniFFI integration discussion](../proposals/zydeco-abi.md#foreign-language-hosts-through-a-rust-adapter)
describes a possible application of generated bindings, separate from this broader research claim.

This is a focused starting bibliography, not an exhaustive novelty survey.
The strongest alternative explanation is that the result is a typed ABI description language
whose CBPV choice has little effect on expressiveness, guarantees, or cost.
The running example, formalization, and CPS comparison are designed to test that explanation.

## 8. Scope and milestones

The first study assumes synchronous execution on one thread, explicitly owned buffers,
and callbacks whose lifetime is bounded by the enclosing call.
It must implement controlled callback reentry and rooting, which current scalar exports do not supply.
Retained callbacks, asynchronous suspension, cross-language unwinding, and concurrent runtimes require further profiles.
Ownership remains an explicit contract and proof assumption until a stronger account is developed.

Proceed through three milestones:

1. Define the visitor's boundary semantics, representation relations, and core adapter typing rules.
   State the supported protocol family and identify the additional constructs it requires.
2. Implement both visitor directions and their negative cases, then establish the first conditional correctness result.
   Connect the implementation to the modeled subset and record the remaining trusted code.
3. Compose adapters across compilation units, extend physical ABI coverage, and run the comparative evaluation.
   Use the results to narrow or strengthen the universality claim.

The intended outcome is a small executable calculus with a precise scope, reusable adapters,
and evidence explaining why CBPV is a useful foundation for interoperability.

## Selected references

- Blume, M., Rainey, M., and Reppy, J. (2008).
  [Calling Variadic Functions from a Strongly-typed Language](https://www.andrew.cmu.edu/user/mrainey/papers/ml-varargs.pdf).
  ACM SIGPLAN Workshop on ML, 47–58.
- Downen, P. (2024). [Call-by-Unboxed-Value](https://doi.org/10.1145/3674654).
  Proceedings of the ACM on Programming Languages, 8(ICFP), article 265.
- Jiang, Y., Xue, R., and New, M. S. (2025).
  [Notions of Stack-manipulating Computation and Relative Monads](https://doi.org/10.1145/3720434).
  Proceedings of the ACM on Programming Languages, 9(OOPSLA1), article 100.
  [Extended version](https://arxiv.org/abs/2502.15031).
- Levy, P. B. (2003).
  [Call-By-Push-Value: A Functional/Imperative Synthesis](https://doi.org/10.1007/978-94-007-0954-6).
  Semantics Structures in Computation, volume 2.
  Kluwer Academic Publishers.
- Patterson, D., Mushtak, N., Wagner, A., and Ahmed, A. (2022).
  [Semantic Soundness for Language Interoperability](https://doi.org/10.1145/3519939.3523703).
  PLDI, 609–624. [Author manuscript](https://arxiv.org/abs/2202.13158).
- Peyton Jones, S., Ramsey, N., and Reig, F. (1999).
  [C--: a Portable Assembly Language that Supports Garbage Collection](https://www.cs.tufts.edu/~nr/pubs/c--gc-abstract.html).
  Principles and Practice of Declarative Programming, LNCS 1702, 1–28.
- UniFFI contributors. [The UniFFI User Guide](https://mozilla.github.io/uniffi-rs/latest/).
  Documentation accessed 2026-09-15.
- WebAssembly Component Model contributors.
  [Canonical ABI Explainer](https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md).
  Design document accessed 2026-09-15.
