# Shared environments for second-class procedures

## Motivation and boundary

Producing one procedure body and direct jumps does not necessarily eliminate capture transport.
The [current procedure lowering](../references/compiler.md#correspondence-with-sps_l) uses a local label
when every call lies in its binding block.
A call from a continuation block instead requires a closed label whose callers push the procedure's free variables.
This is correct, but can copy the same captured values at each call.

The [commuted short-circuit example](../../lib/tests/demos/short-circuit-commuted.zy) proves the direct-jump shape
with an empty specialized environment.
A useful next fixture must make `M5` read runtime-dependent captured values
and reach it both after M1 and inside M2's return continuation.
That distinguishes sharing code from sharing the storage containing its environment.

This proposal owns eliminating procedure capture transport when activation ownership justifies it.
[Join-point optimization](join-point-optimization.md) owns forming the procedure and its direct call sites;
[native environment alternatives](native-frames.md) owns physical frame storage and reclamation choices.
[Escape and representation](escape-unboxing.md) continues to own general first-class value representations.
No new source lifetime guarantee or default frame representation is proposed here.

## First milestone: same-activation native procedure entries

Native continuation lowering can restore a retained caller activation instead of unpacking copied captures.
Consequently, two portable blocks can execute in the same dynamic activation after native lowering.
The proposed first optimization should recognize call sites already executing in the procedure's binding activation,
then place its body in that owner and read captured definitions from their existing slots rather than push and pop them.
It should require no new runtime restoration operation.

Preserve structured procedure provenance through closure conversion: the binding site,
ordered captured definitions, their entry bindings, the original body boundary, and associated direct calls.
This information should describe the executable SPSLow program, not introduce another executable copy of its body.
Validate it against the actual capture pushes and entry preamble, following the existing treatment
of [continuation provenance](../references/compiler.md#c11-native-preparation-activation-frames-and-amd64-emission).
Do not infer a source capture relation from generated names or from a coincidentally similar block layout.

Native preparation must establish all of the following before replacing the closed entry:

- Every transformed call reaches the particular dynamic activation that owns the captured bindings.
  Equal layout IDs, lexical nesting, or equal source procedure names alone do not establish that fact.
- Every required capture is initialized and still live at the call, and its alias resolves to the owning definition.
- Pending continuations preserve those bindings against slot reuse until every relevant resumption has finished.
- Removing the capture preamble preserves the supplied stack protocol, active-frame state, and subsequent reclamation.

Analyze candidates before rewriting frame-entry edges, then rebuild and revalidate the complete ownership,
initialization, liveness, interference, and root information affected by those rewrites.
The proof must not assume the entry is local merely because the proposed rewrite removes its fresh-frame marker.
Use the shared SPSLow traversal for provenance analysis and the existing native dataflow machinery for graph facts;
keep their evidence independent until the native lowering decision combines them.

Start with one binding-owner relation that covers all call sites of a procedure.
Each dynamic instance of that binding still has its own activation; recursive
or repeated source invocations must not share captured slots merely because they share code.
If any call lacks the required proof, retain the existing closed representation for that procedure as a whole.
That is a supported representation choice, not a compatibility layer.
A disagreement between recorded provenance and executable syntax is a validation error,
not a reason to hide corrupted evidence behind the general representation.

## Alternatives and deferred scope

| Approach | What it could remove | Additional obligations |
| --- | --- | --- |
| Native same-activation refinement | Capture pushes/pops and fresh entry for proven local transfers | Preserve provenance and prove ownership after continuation lowering |
| Portable explicit environment reference | Repeated transport of individual captures on more backends | Define an environment representation, lifetime, tracing, and entry contract for every consumer |
| Restore a different retained owner at a procedure jump | Copies even when the caller is not already executing in the owner | Establish an owner handle and a legal restoration/retirement transition without consuming an unrelated suspension |

Prefer evaluating the first approach before expanding the runtime protocol.
Portable ZASM and Wasm can continue to use closed labels
until a separately checked representation supports environment sharing there.
Direct procedure calls must remain available on those targets.
Do not disguise a native owner reference as an ordinary source value or retain a raw pointer
into growable frame storage.
Detached control, multi-shot stacks, recursive procedure groups, and escaping closures are outside the first milestone.

The proof and fallback policy are more important than naming a new label variant in advance.
Decide whether the evidence belongs in validated SPSLow provenance
or a native preparation input once the existing closure-conversion and frame-lowering paths have been traced end to end.
If those interfaces change, update their consumers and remove the superseded path together.

## Acceptance and failure cases

Add a captured variant of the commuted short-circuit fixture without replacing its capture-free structural control.
Use a runtime string or boxed scalar whose only surviving use is in `M5`; check both false paths,
the true path, and collection between capture and use.
For eligible native calls, require one procedure body, direct jumps without environment transport,
and reads from the validated owning slots.
Compare the same program's portable result and runtime behavior.

| Accepted case | Rejected or conservative counterpart |
| --- | --- |
| A return continuation has restored the procedure's owner | A call from another dynamic activation retains the closed representation |
| All captures are initialized and live in that owner | Corrupted provenance, unavailable slots, or wrong capture aliases fail preparation |
| Nested suspensions preserve a value needed by a later call | A proposed slot reuse conflicting with any pending capture is rejected |
| Collection moves a captured managed value | The eventual procedure reads its updated root, never a stale copied address |
| Frame storage grows between suspension and resumption | Logical ownership remains valid without retaining raw slot pointers |
| Repeated tail transfers have a fixed pending continuation set | Frame extent does not grow with the number of optimized transfers |

Extend [native-model tests](../../lang/tests/tests/native_model.rs)
and frame-plan mutation tests alongside the source fixture.
Include two invocations of the same source procedure so equal layouts cannot accidentally pass
for equal dynamic owners, and a mixed-call case that forces the conservative representation.
Preserve the existing interpreter and Wasm behavior even when they do not receive the native optimization.

Measure capture words transported, activation entries, frame high-water usage, and GC roots separately.
Empty-capture examples cannot establish a capture-copying improvement;
retained storage must also be checked for keeping dead values alive.
The [activation-lifetime contract](../references/compiler.md#activation-lifetime) remains the governing
implemented boundary until a reviewed extension replaces it.
