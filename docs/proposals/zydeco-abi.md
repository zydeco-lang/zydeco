# A Zydeco ABI for other languages

## Purpose and status

This proposal defines how another language can consume and implement **Zydeco interfaces**: construct its values,
call returned thunks, implement computation protocols, and preserve its abstract types.
The ABI is the binary agreement about values, entry points, control, and runtime ownership;
an FFI binding exposes that agreement in a particular host language.
Zydeco owns the interface being implemented here.
Importing functions with a foreign calling convention remains the separate [C FFI proposal](c-ffi.md).

The proposed ABI builds on the current type system and compilation strategy:
**one value word per source value at general calls, and a typed continuation stack for computations**.
Kinds and type witnesses erase. Products, data, and closures use the ordinary runtime representation.
Other languages should receive generated bindings to this model, rather than having to translate every Zydeco type
into a native struct or every computation into a returning function.

The minimal [native unit profile](../references/language.md#native-zydeco-units) is implemented.
It permits source-free composition through explicit initializers and closed structural interfaces;
its [artifact contract](../references/compiler.md#native-unit-artifacts) is canonical.
This proposal owns the remaining richer type interfaces and host-language bindings.
The existing [word entries](../references/compiler.md#word-entry-contracts),
[runtime model](../references/compiler.md#c12-shared-native-model-allocation-and-collection),
and [native activation rules](../references/compiler.md#activation-lifetime) supply its runtime foundation.
Compiled libraries also support a separate
[scalar C entry profile](../references/language.md#compiled-libraries-and-c-exports).
Persistent host instances, external root handles, and the binding APIs below require implementation.
The [compilation-unit extensions](#9-compilation-unit-extensions) can precede host binding work;
the [workload assessment](#10-workload-and-pitfalls) distinguishes their remaining costs.

## 1. The agreement a foreign implementation needs

For `v : A`, the ABI describes a runtime value representation `V(A)`.
For `M : B`, it describes the continuation stack `S(B)` that the computation consumes.
For `{ M } : Thk B`, it additionally describes how the code receives its environment and that stack.
These are complementary parts of one interface:

| Part | Agreement |
| --- | --- |
| Values | Word encoding, product fields, data tags, closure records, and opaque carriers |
| Control | Argument order, observation tags, return continuation entry, and residual stack ownership |
| Types | Exported classifiers, nominal identities, binders, equations, and package witness relationships |
| Runtime | Instance membership, live roots, allocation, code lifetime, and host entry/exit |

Uniform value transport matters for polymorphism.
A worker compiled at `forall (A : VType) . A -> Ret A` can forward its argument
without learning its size, layout, or concrete type.
That works because the argument is one ordinary value word and type arguments erase.
Giving each instantiation a different number of argument slots would need a different calling contract.
The current [representation policies](../references/compiler.md#policy-selection) do not select such a contract.

The proposal has two access levels to this same model:

- A **native integration interface** for compilers or generated assembly sharing the exact target runtime model.
  It uses value words and Zydeco control transfers directly.
- A **runtime binding interface** for Rust, Python, and other ordinary host code.
  It roots values and drives those transfers through generated adapters.
  Its entry functions may use the platform's ordinary calling convention, while the values
  and computations they expose retain Zydeco's types and behavior.

The implemented native profile requires matching compiler/runtime model identities.
The runtime binding interface can be versioned separately so bindings need not expose moving heap addresses,
Rust object layouts, or native frame tokens.
An internal optimization may change local packing only while preserving or adapting at the published word boundary.
A direct native integration is a trusted implementation of this agreement;
interface metadata does not verify arbitrary foreign machine code.

## 2. Value representation by type

The table below uses the current AMD64 word model as the proposed native baseline.
It summarizes implementation evidence for this proposal; the linked references remain the canonical account
of current behavior.
Interpreter and Wasm implementations can expose the same binding operations through their own carriers,
as discussed in section 8.

### Scalar values

Let `W` be a 64-bit word. An immediate integer payload is encoded as `(bits << 1) | 1`;
signed decoding uses an arithmetic shift. The meaning of the bits comes from the source classifier.
The low bit distinguishes an immediate from a pointer-shaped word, not `Int` from `Char` or a constructor tag.

| Type | Current native `V(A)` | Host binding behavior |
| --- | --- | --- |
| `Unit` | Immediate word `1` | Construct/read unit; it still occupies a value slot when supplied as a source argument or result |
| `Int8`, `Int16`, `Int32` | Sign-extended integer encoded as an immediate | Check input range and use the signed constructor/accessor for that width |
| `UInt8`, `UInt16`, `UInt32` | Unsigned integer encoded as an immediate | Check input range and use the unsigned constructor/accessor |
| `Int` | Signed tagged machine integer | Check the [source range](../references/language.md#13-primitive-values-and-capabilities) before constructing an immediate |
| `UInt` | Unsigned tagged machine integer | Check the source range before constructing an immediate |
| `Float32` | Its 32 payload bits encoded as an immediate | Bit-preserving float constructor/accessor, including signed zero and NaN payloads |
| `Float64` | Pointer to an opaque scalar box holding its 64 payload bits | Runtime allocation plus bit-preserving conversion |
| `Char` | Unicode scalar value encoded as an immediate | Validate Unicode scalar range, including exclusion of surrogates |
| `String` | Stable address of an instance-owned host string object | Construct from valid UTF-8 and expose copying/window operations; no foreign access to the Rust object layout |
| `Addr` | Raw unmanaged data address word | An explicitly unsafe address carrier whose extent, alignment, permission, and lifetime belong to its owner |
| `Reader`, `Writer` | Tagged instance-local handle indices | Typed resource handles from the matching provider; no assumption that their bits are operating-system descriptors |

The [word model](../../lang/machine/src/word.rs), [native scalar helpers](../../runtime/stub.rs),
and [literal emission](../../lang/amd64/src/emit.rs) implement these encodings today.
Scalar boxes are opaque to tracing; their bits must never be scanned as managed references.
A future exact-width `Int64` or `UInt64` should follow `Float64` with a distinct boxed representation.
Those types and their operations are deferred to a later change.

### Products, sums, and closures

Managed pointer values refer to payloads allocated by the matching runtime.
Allocation headers and collector bookkeeping are outside the payload descriptions below.
Foreign code should use runtime constructors; placing similar bytes
in an arbitrary host allocation does not register a managed object or its references.

| Type | Native payload behind its value word | Meaning for another language |
| --- | --- | --- |
| `A1 * ... * An` | Ordered words `[V(A1), ..., V(An)]` | Construct/project a product using its actual arity and field classifiers |
| `#field :: A` | Same representation as `A`; label and resolved route erase | Use the published field route without expecting a runtime name field |
| `data ... end` | `[tag(Ci), V(Ai)]` | Use the interface's constructor map and validate the selected payload type |
| Recursive data | The same tag/payload and product forms linked through value words | Traverse through typed accessors; recursion adds no per-value type descriptor |
| `Thk B` | Closure record `[environment, code]`; environment is an ordinary value word | Retain the whole thunk and invoke it against a stack satisfying `B` |
| `exists (X : K) . A` | Representation of the runtime payload of `A`; the witness contributes no word | Open the interface's witness scope and expose the corresponding payload and operations |
| Nominal or abstract `A` | Its selected implementation's value word | Forward/store it opaquely and use the operations supplied by its defining interface |

The current product lowering and the `[tag, payload]` constructor representation are visible
in [ZASM lowering](../../lang/assembly/src/lower.rs).
The [shared closure record](../../lang/machine/src/closure.rs) fixes environment before code.
Code and environment must stay paired; a function
with the same apparent argument types cannot consume an unrelated closure environment.
The environment itself can contain further products, closures, and resource handles.

For example, `(1, 2, 3)` has one product payload with three words;
`(1, (2, 3))` has an outer two-word payload whose second word refers to another product.
Source labels do not change that distinction.
Local pack/unpack elimination can remove cells inside a compiled function,
but a separately compiled consumer still receives the published representation.
Product suffix pointers already occur internally;
rooting and projection must respect the collector's interior-pointer support rather
than assuming every payload pointer is an allocation base.

`Bool`, `Option A`, `Result A E`, and `List A` are source data types using these rules.
They do not acquire another language's enum layout, null-pointer optimization, or vector representation implicitly.
The interface manifest publishes the actual constructor-name/index map from the compiler's checked/lowered type.
Do not guess it from a foreign language's declaration order or confuse it with codata observation numbering.

For an existential, erasure removes witness entries while preserving the remaining runtime product structure.
It neither inserts a type-descriptor pointer nor reveals a sealed representation.
An opaque payload may occupy one word even when its hidden implementation is a large product.

### Source memory abstractions and static interfaces

| Source form | Compilation consequence | Host interface |
| --- | --- | --- |
| `Ptr L S` | One `Addr`; `L` and `S` are erased parameters | Typed wrapper shares layout/state evidence with its operations; no new allocation owner |
| `Slice L S` | Address and count in the source library's payload | Query its count and use matching element operations; retain its actual allocation owner separately |
| `Bytes` | Private address/count payload; storage retained by the runtime instance | Preserve the source abstraction through exported byte operations or a checked generated adapter |
| `Buffer`, array builders | Library-selected address/capacity/initialization payload | Use the published operations and their caller obligations, not guessed field offsets |
| `Representation A`, allocator packages, runtime-selected dictionaries | Their runtime operations and captured context remain where needed | Opaque package or typed operations sharing the same witness opening |
| Fixed plans, static field paths, fixed views | Recipes eliminate before runtime readiness | Static binding-generation input when explicitly supported; no runtime descriptor export by default |
| Dynamic layouts, dynamic field paths/views | Runtime placement values or ordinary thunks | Preserve the library's payload and operations, with explicit runtime metadata where needed |

These private source-library representations need not become stable public layouts just
because their present implementations are small products.
The source interface owns which representation facts it discloses.
The [memory reference](../references/language.md#manual-memory) owns their current meanings and obligations.
Keeping a handle to a `Ptr` or `Slice` does not keep manually managed storage alive;
keeping an instance alive does protect that instance's retained immutable byte allocations.
Initialization states are type parameters, not flags stored beside every pointer.

## 3. Computation types describe stacks

The control ABI is as important as the value ABI.
A computation `M : B` consumes a stack of protocol `B`; it is not generally a host function returning one value.
Use the following logical descriptions, with the next consumed component on the left:

| Computation type | `S(B)` and entry behavior |
| --- | --- |
| `A -> B` | An argument word `V(A)` followed by a stack satisfying `B` |
| `Ret A` | An installed continuation accepting `V(A)` and owning its hidden residual stack |
| `codata ... end` | One observation tag selecting `.di`, followed by `S(Bi)` |
| `forall (X : K) . B` | The instantiated `S(B)`; no runtime type-argument word |
| Package-dependent `pi (p : P) . B` | One payload word `V(P)` followed by the protocol obtained with the statically related package witnesses |
| `OS` | The host's root protocol; no ordinary source return |

Type functions, named kinds, manifest equations, aliases, and classifier queries normalize or erase.
They supply no stack frames. Total value functions and their applications must statically eliminate;
they cannot be exported as executable foreign closures.
To publish a dynamically callable implementation, the source interface uses `Thk B`.
These are the existing [static elimination rules](../references/language.md#10-static-elimination).

### Calling and returning

For `f : Thk (Int -> Int -> Ret Int)`, a foreign caller supplies two encoded arguments
and a return continuation, then enters the closure with its environment.
In the current native implementation, the conceptual entry stack is:

```text
environment_f :: argument_1 :: argument_2 :: label_k :: token(F) :: residual_stack
```

The closure establishes its own activation and consumes arguments in source order.
Returning stages the result, transfers to `label_k`, restores the continuation's retained activation,
and delivers the result word.
The portable lowering uses a captured environment in place of `token(F)`;
native preparation selects the retained-frame form.
The [activation reference](../references/compiler.md#activation-lifetime) owns those transitions.

An ordinary host `call` instruction to `code` does not establish this state.
Generated entry adapters must create the invocation context, root arguments,
arrange the stack, and install a return delimiter that can return to the host.
The adapter can then make this particular source interface look like an ordinary host call.

**`Ret A` is not an end-of-stack marker or an allocation boundary.** Its continuation hides the residual stack,
whose extent can depend on runtime control flow.
An entry may receive arbitrarily many argument/observation frames before a later return is reached.
The ABI must preserve that stack, not infer a fixed frame size by counting arrows before a `Ret`.

### Codata and generic protocols

For a protocol such as:

```text
Stream = codata
  | .item : Int -> Stream
  | .done : Ret Int
end
```

a caller can supply this logical stack:

```text
.item :: 3 :: .item :: 4 :: .done :: return_continuation
```

The computation consumes observations and arguments until it returns through the installed continuation.
This is a protocol invocation, not a vtable stored inside each `Stream` value:
`Stream` is a computation type, and `Thk Stream` is the closure value.
A binding can offer method-like syntax, but a nonreturning observation does not promise
to hand control back to the host immediately.
It prepares more of the source stack until a complete invocation is supplied.

Current codata observation tags use lexicographic order of complete destructor names;
the interface publishes this map and its branch classifiers.
For the example, `.done` has index 0 and `.item` index 1.
The [protocol reference](../references/compiler.md#partial-source-protocols) owns that ordering.
Generated bindings should use the published map instead of duplicating an ordering algorithm.

Computation polymorphism makes this useful for effectful APIs.
For `forall (R : CType) . A -> Thk (B -> R) -> R`, the caller provides an `A`,
a successor thunk, and a residual stack satisfying `R`.
The implementation can invoke the successor with a `B` and forward that same residual stack.
It needs no runtime representation of the type `R`.

## 4. Types and abstraction across a module boundary

Erasure makes the binary carrier uniform, but external callers still need the source typing relationships.
Publish a **typed interface manifest** before erasure discards that information.
It records exported classifiers, product structure and named routes, data and codata maps,
nominal type identities, abstract and manifest package entries, type binders, and supported type-level definitions.
This is interface metadata, not an extra field on every runtime value.

The current SPS [partial protocols](../references/compiler.md#partial-source-protocols) are insufficient for this job.
They intentionally admit unknowns, lose some existential and higher-kinded evidence,
and use an agreement relation that is not type equality.
Export preparation must retain a complete interface from the checked source arena.
Unsupported interface forms must reject binding generation explicitly;
a partial descriptor containing `?` cannot certify an external call.

### Polymorphism

A generic identity entry at `forall A. A -> Ret A` can return its argument word unchanged at every instantiation.
No monomorphization or implicit runtime dictionary is required by that ABI.
A generated typed host wrapper checks the instantiation in its host type system;
a dynamic binding can validate schema instantiations before driving the computation.
Either form erases type arguments from the actual Zydeco stack.

Foreign implementations of generic operations must respect their abstract arguments.
They may forward or retain an unknown `A` through the generic value API; they cannot assume it is an integer
or inspect a hidden product without an operation that exposes that ability.
The low-bit tag does not provide such type evidence.

### Existentials and nominal identities

Opening `exists X. Payload X` exposes an opaque carrier and operations under one fresh witness scope.
The binding retains that relationship in its type wrappers or checked handle metadata.
Aliases from the same opening share the witness; an independent opening does not acquire equality
by comparing addresses, field names, or sizes.
A dynamic binding may allocate a scope token for this check, but it cannot recover the erased concrete witness
from the runtime payload.

Use artifact/interface identities for published nominal definitions and generative scope identities
for abstract openings.
Compiler-local arena indices are not cross-artifact identities.
An exposed nominal data shape can publish constructors while keeping its nominal type identity distinct.
Manifest equations disclose the relationships clients may use; hidden equations stay hidden even
when the provider's implementation is available to the loader.

Package-dependent arrows connect their result classifier to witnesses opened from an argument.
The interface manifest must preserve that binder/substitution relation,
rather than describing the result as an unrelated opaque type.
General runtime values still do not index types.
Higher-kinded and recursively defined interfaces should retain finite binder/definition syntax;
eagerly enumerating all instances would fail for growing recursive families.
Binding support for these forms can be staged without changing their uniform native transport.

## 5. Runtime bindings and ownership

The host-facing interface should provide typed wrappers over registered runtime roots.
The following names are proposed binding concepts, not new Zydeco source types:

```text
Instance                 owns one compatible runtime and its loaded modules
Value<A>                 a rooted source value in that instance
Thunk<B>                 Value<Thk B>
StackPlan<B>             a host-owned recipe for constructing a valid invocation stack
Cursor<B>                temporary access to the active stack supplied to a host implementation
ReturnPort<A>            temporary permission to deliver a value to one installed continuation
```

`Value<A>` can use an opaque slot/generation handle.
Its registry entry holds an updatable word, instance identity, and the interface evidence needed for checked operations.
Moving collection updates the root slot; foreign code retains the handle rather than a stale address.
Copies of an owned host handle retain a root according to the binding's ownership convention;
releasing one handle does not consume the source value or run a source destructor.
Explicit instance close requires no active invocation, cursor, or raw-payload access scope.
It marks the instance closed, invalidates public handles, and tears down its contexts and resources.
Outstanding wrappers retain enough closed-instance state to reject later access without dereferencing freed storage.
Handle release after close only releases that wrapper's bookkeeping.

Root all argument and result intermediates before an operation can allocate,
including earlier product fields while later fields are being constructed.
Unsafe native integrations may instead register writable root slots and reload their values after collection.
No live managed word may remain solely in an unregistered foreign local.
Access to a raw managed payload is bounded by an explicit no-collection scope or by a copying accessor;
the present collector provides no general pinning facility.

Dropping a root makes managed storage eligible for collection; it does not promise immediate reclamation.
Strings and immutable byte storage retain their current instance-lifetime policy.
Dropping a handle to a manually managed address does not release its allocation.
The binding must not add ownership or implicit destruction absent from the source resource interface.

### Stack recipes and live continuations have different lifetimes

A `StackPlan<B>` contains retained argument/thunk values and observation choices.
Each execution materializes a fresh invocation stack and fresh host return delimiter.
Reusing the plan can repeat the computation, just as forcing the same thunk repeats its body.
The plan is not a captured native stack.

A `Cursor<B>` or `ReturnPort<A>` refers to one active invocation and cannot outlive it,
be replayed, or be reconstructed from raw bits.
Runtime scope tokens provide dynamic validation where the host language cannot enforce the borrowing discipline.
Native frame tokens are not exported as durable handles.
Supporting detached or multi-shot machine continuations would require a separate extension
to the [native frame model](../references/compiler.md#activation-lifetime).
This restriction leaves ordinary reusable `Thk B` values fully usable.

## 6. Calling from another language

Consider this complete source factory, accepted by the current language:

```zydeco check
param val (/Thk; /Ret; /Int; /numeric) : @(import("../../lib/std/builtin.zy")) in
(
  #make_adder = ({
    fn (delta : Int) =>
      ret { fn (value : Int) => ! numeric/int/add delta value }
  } : Thk (Int -> Ret (Thk (Int -> Ret Int))))
)
```

The implemented `library(zydeco)` role publishes the complete prepared runtime value.
The [native unit rules](../references/language.md#native-zydeco-units) describe its factory preparation
and current public type subset.
A future selection form such as `library(zydeco, export(field(make_adder)))` could narrow the public interface;
its logical export name would come from that selection, while linker symbol names remain generated details.
Public type definitions would additionally contribute manifest entries once richer interfaces are supported.

A generated Python binding could offer the following schematic API:

```python
instance = zydeco.Instance()
math = instance.load("example.math.zyabi")

add_seven = math.make_adder(7)
assert add_seven(35) == 42

add_seven.close()
math.close()
instance.close()
```

The first invocation encodes `7`, enters `make_adder`, and registers the returned closure as a root.
Its environment retains `delta`; the instance remains alive after the host call returns.
The second invocation builds a new `Int -> Ret Int` stack, supplies `35`, and enters that same closure.
The host reads `42` through the scalar accessor after the source return reaches its delimiter.
Closing the wrapper releases its root, with no source-level single-use restriction on the thunk.

This requires a persistent runtime instance.
The existing fresh-instance scalar export adapter cannot destroy the instance
after `make_adder` returns and still hand out a usable closure.
Ordinary products, data, abstract objects, and package payloads can be returned through the same rooted-value mechanism;
they need no separate C aggregate schema.

For codata, a binding builds observation/argument recipes ending in an appropriate host return delimiter.
For explicit CPS, it supplies source-compatible successor thunks and the residual protocol.
A convenience `call` operation is available when the completed protocol reaches a host-owned `Ret A`;
it is not the primitive meaning of every `CType`.

## 7. Implementing a Zydeco thunk in another language

Foreign code should be able to supply a value classified by `Thk B`.
Register a host implementation, its context owner, and its exact source protocol.
The runtime constructs an ordinary Zydeco closure whose code is a generated bridge and
whose environment identifies the registered host context.
Source code can capture, return, copy, and force it normally.
The arbitrary host function address is not itself a Zydeco closure entry.

The proposed host entry receives a context and a scoped `Cursor<B>`.
It consumes arguments/observations through checked operations and returns a transfer description to the driver:

```text
Enter(thunk, matching_stack)       continue by forcing a thunk
Deliver(return_port, value)       resume the installed source continuation
Fault(diagnostic)                 terminate under the selected runtime fault profile
```

These are driver operations with associated protocol checks, not source constructors.
Returning a transfer description from the host function is an ordinary platform return;
the driver then performs the indicated Zydeco transfer without accumulating host call frames for a tail chain.
The existing [HostTransfer](../../lang/machine/src/native.rs)
and [Wasm host transfers](../references/compiler.md#module-and-host-abi) are bounded examples of this pattern,
not implementations of the general interface proposed here.

For a returning `Int -> Ret Int` worker, the host reads one integer and a return port,
computes the result, and returns `Deliver(port, encoded_result)`.
For a generic effectful worker of `forall R. A -> Thk (B -> R) -> R`, it reads the argument and successor,
computes `b : B`, then returns `Enter(successor, b :: residual_R)`.
It forwards the caller's `R` stack without inspecting its unknown tail or treating it as a native return address.

Host contexts can contain references into a second language's heap.
The registration must keep those objects alive through that language's rooting/ownership mechanism,
while Zydeco values captured by the context remain registered Zydeco roots.
The initial profile can retain registered context owners until instance teardown,
matching existing retained resource policies.
Early host-context finalization requires a separate reachability/lifetime mechanism;
dropping one foreign thunk wrapper does not prove that source code retained no copies.

Start with synchronous entry on one owner thread and transfer-driven callbacks.
An unrelated nested `instance.run` on an already active instance rejects before altering its state;
host code requests further source computation through the transfer protocol and explicit successor thunks.
Supporting synchronous nested host calls, asynchronous suspension, other threads,
or detached cursors requires additional entry and root-lifetime profiles.

Host exceptions must be handled inside the bridge and translated into the declared fault behavior;
they cannot unwind through Zydeco assembly.
The first native profile keeps current fatal runtime faults while reporting validation/loading failures normally.
A recoverable-fault profile needs explicit runtime exit and cleanup paths
before bindings can promise exception-style recovery.
`OS` is exposed only through an explicit process-entry mode with its termination contract.
A general returning `call` operation does not admit it.

## 8. Compiler work, artifacts, and target profiles

### Preserve the interface before erasure

The [implemented unit preparation](../references/compiler.md#native-unit-artifacts) retains a complete closed
structural classifier before erasure and publishes a versioned initializer manifest.
Extend that schema with scoped binders, nominal definitions, and public witness relationships.
Export selection, if introduced, must preserve shared captures and apply the same static-readiness checks.
Preparation must not execute exported computations to discover their interface.

The runtime binding interface additionally registers module exports in its external-root table.
Artifact selection stays explicit
under the existing [compilation-unit contract](../references/compiler.md#compilation-unit-preparation-and-artifacts).
Modules loaded into one instance must agree on the runtime model, value representation, and resource provider.
Values from unrelated instances cannot be passed as words between them; explicit copying is possible
for transferable data, while closures and abstract resources remain with their owner.

Public root transport remains one value word, even if internal optimization expands a product or closure.
Exported entry preparation preserves or reconstructs the required boundary representation.
Imported values carry the corresponding source classifier and use the same ordinary call lowering.
A future unboxed inter-module profile must establish matching field transports and tracing at both ends;
the [machine-call proposal](escape-unboxing.md#remaining-machine-call-boundary) owns that extension.

### Native entry and binding generation

The runtime binding interface needs allocation/accessor operations, an external-root registry,
module initialization, typed stack-plan validation, host thunk registration, and invocation adapters.
Native invocation adapters preserve platform registers and stack state, activate the correct instance,
install a return delimiter, and drive the published Zydeco protocol.
Return values must enter registered roots before temporary invocation state is retired.
The module and its code stay loaded while any reachable closure can invoke them.
The initial lifetime rule can retain loaded modules until instance close.

Generate host bindings from the same interface schema used to validate imports.
For a typed host language, expose opaque abstract types and typed values/thunks
where its type system permits; use checked wrappers for remaining relationships.
For a dynamic host, retain schema and witness-scope evidence in handles and validate each boundary operation.
Neither approach relies on reconstructing a type from tagged bits.
Validation failures must occur before source entry or partial publication of roots, stack plans, or module exports.

### Interpreter and Wasm

The interpreter can implement handles over `SemValue` and build its semantic argument/observation stack.
It need not imitate native heap addresses or reinterpret Rust enums as ABI structs.
Wasm bindings use module-owned values and dispatch, with explicit conversion between host references
and module carriers.
A native pointer, a Wasm module offset, and a host handle are distinct address domains.
The [existing Wasm host ABI](../references/compiler.md#c13-webassembly-backends-and-embedding) is a starting point,
not a portable binary layout shared with AMD64.

The same exported source classifier can therefore have several target implementations of the runtime binding interface.
Direct native interoperability requires an exact native profile; the language-facing binding operations
and observable source semantics remain shared.

## 9. Compilation-unit extensions

Separately compiled Zydeco code needs the native agreement in section 1, but does not need a foreign-language SDK.
The implemented [native units](../references/language.md#native-zydeco-units) supply independently compiled objects,
typed initializer imports, exact compatibility checks, and source-free transitive linking.
The [workflow](../../CONTRIBUTING.md#compile-and-consume-native-zydeco-units) demonstrates this minimum.
The remaining questions concern richer public types and optional shared dependency initialization.

### The unit boundary

The current initializer has no dependency argument and is forced explicitly by source code.
A future profile could instead accept a prepared dependency environment:

```text
init_U : Thk (Deps_U -> Ret Exports_U)
```

Here `Deps_U` is the ordered runtime dependency environment, and `Exports_U` is the runtime export value.
Their static components have already been interpreted by unit preparation.
Provider-dependent classifiers must preserve their package witness binders when that interface form is supported;
the displayed ordinary arrow abbreviates the case with no such dependency.
Builtin preparation must use the same validated provider contract as the surrounding program.
It can reuse the current lowering hook that materializes Builtin operations against the active runtime.

A generated linking driver could initialize dependencies in topological order,
supply their export values to consumers, then enter the process computation.
Initializing each resolved unit once per runtime instance is a possible future rule;
it requires a separate profile because the current explicit initializer runs on every force.
Different static specializations would remain different unit identities,
and cyclic initialization dependencies would require rejection or an explicit recursive-initialization design.

Initialization materializes a source value; it does not automatically force an exported thunk.
An operation that creates a counter, opens a file, or chooses a provider
at runtime remains an explicitly invoked computation with its ordinary execution multiplicity.
Such a driver would introduce a compiled-unit instantiation rule.
Ordinary source imports retain their [existing semantics](../references/language.md#12-sources-imports-and-entry),
including repeated execution of imported computations at dynamic occurrences.

Imported runtime values would become typed dependency parameters, captured by ordinary closure conversion where needed.
The generated driver would keep live dependency/export values in traced stack slots or reachable closure environments.
They must be rooted before any initialization allocation can collect, and moved values must be reloaded afterward.
A linker global containing a managed address is insufficient because the collector does not currently trace it.

This design does not require a new public handle registry or a runtime surviving a return to Python.
The existing process invocation already keeps its instance alive for the whole Zydeco program.
Code remains statically linked for that lifetime, and calls between units stay
within the same instance, native frame store, and continuation stack.
In that profile only bootstrap code would need the initializer's linker symbol;
consumers would use the supplied ordinary values and closures.

### Where the compiler must change

The checked unit boundary, relocatable initializer, word entry, and structural imports are implemented
in the [native artifact pipeline](../references/compiler.md#native-unit-artifacts).
A dependency-environment profile would add driver generation and parameterized import preparation.
It must preserve imported closure provenance, the ordinary word boundary, and runtime ownership.

Retain a versioned public type graph before erasure and reconstruct it in the consumer's checking arena.
Use module-qualified definition keys for published nominal identities and scoped references for binders.
Intern repeated references to one dependency interface once in a consumer;
do not merge unrelated nominal definitions because their structures or names match.
Only public equations belong in this graph. Dumping the producer's complete arena can expose sealed definitions
and includes local identifiers that have no meaning in another compilation.
Reconstructed data and codata maps must agree with the producer's maps during consumer lowering.

Extend the current structural subset with exposed nominal data definitions, regular recursive data/codata,
and universals over `VType` and `CType` before supporting arbitrary package interfaces.
Returned capturing thunks already work; ordinary runtime polymorphism still needs public binder import.
Continue rejecting other public forms explicitly until their complete interface import is implemented.
Existentials, package-dependent arrows, and higher-kinded definitions extend this schema and its checker integration;
they do not require changing the one-word runtime convention.

### Static factories set the limit on separate compilation

Zydeco's value functions are compile-time programs.
A consumer that must apply one needs its definition or a producer specialization;
an object containing runtime code cannot replace that definition.
For example, a factory that computes a fixed storage plan must finish that calculation
before its residual implementation becomes a native unit.
A runtime `forall` function simply forwards value words and can remain generic.

The existing minimum requires specializing static factories in the producer before publication.
Keep source imports for consumers that need to apply arbitrary static functions.
Supporting source-free distribution of those functions would require a separate static-code artifact
and evaluator contract; serializing types alone does not supply their implementations.
Consequently, native units enable reuse of prepared runtime libraries
but do not automatically eliminate [repeated functor specialization](../ideas/residual-code-sharing.md)
or permit every source package to become an object.

Richer incremental compilation should give interface identity, implementation identity,
and static specialization identity distinct fields.
Consumers typecheck against the interface; linking verifies the selected implementation and its dependency hashes.
The current exact-identity policy requires rebuilding dependents conservatively.
Later incremental-build work can reuse a consumer when all of its static and ABI dependencies remain compatible;
an unchanged list of runtime symbols alone is insufficient evidence.

## 10. Workload and pitfalls

### What is already available

The minimum now has a native integration test that removes producer source and retains captured boxed values
across calls and collection through a dependent unit.
That validates the structural boundary; nominal interfaces and host ownership remain separate work.

| Foundation | Reusable work | Remaining obligation |
| --- | --- | --- |
| [Machine model](../../lang/machine/src/native.rs) and [closure representation](../../lang/machine/src/closure.rs) | Value words, closure environment/code pairing, shared model identity | Preserve these contracts for richer protocols and host adapters |
| [Library checking](../../lang/statics/src/check/library.rs) | Shared export preparation, Builtin specialization, readiness checks | Retain/import nominal definitions and scoped binders |
| [Unit builder](../../cli/src/unit.rs) | Typed initializer artifacts, exact hashes, transitive dependency validation, atomic publication | Extend interface identity and optional dependency environments |
| [Native runtime](../../runtime/stub.rs) | Instance-owned heap, frame store, traced stack, source host operations | Persistent activation and external roots for host bindings |
| [Unit regressions](../../cli/tests/unit.rs) and [manifest regressions](../../cli/tests/unit_manifest.rs) | Source-free consumers, captures, collection, dependency and compatibility failures | Add nominal identity, recursive protocols, and host-lifetime cases |

### Effort estimate

For one developer familiar with this compiler,
budget approximately **15–30 engineering days, roughly 3–6 working weeks**,
for the richer native unit subset and dependency driver described in section 9.
This is a planning estimate for work beyond the implemented structural profile:

| Work package | Estimated days | Main uncertainty |
| --- | --- | --- |
| Public type graph, nominal identity, interface import, and rejection diagnostics | 7–12 | Reconstructing recursive definitions and scoped binders without leaking implementation equations |
| Dependency parameters, shared initialization driver, and static readiness | 3–6 | Making initialization multiplicity explicit while preserving source semantics |
| Extended identities, representation maps, and linkage | 1–3 | Agreement between independently reconstructed public definitions |
| Source-free integration tests, GC stress, invalid artifacts, documentation, and fixes | 4–9 | Cross-phase failures involving nominal identity and recursive protocols |

These are planning estimates, not measured delivery times.
They include focused tests and integration work, assume one target and an acyclic dependency graph,
and exclude cross-version ABI stability, dynamic loading/unloading, cross-unit optimization,
source-only factory replacement, and new runtime control features.
Re-estimate after importing a nominal type through a source-free dependency diamond.

Follow-on work has different costs:

| Extension beyond that subset | Additional estimated days | Completion scope |
| --- | --- | --- |
| Full current representable type interfaces | 10–20 | Existentials, package witness dependencies, higher-kinded and growing recursive definitions; source-free checking with preserved abstraction |
| Synchronous host embedding and one basic binding | 15–25 | Persistent instances, external roots, value construction/access, returning and returned thunks, close semantics, one host language |
| General protocol bindings and host-implemented thunks | 10–20 | Checked stack plans, recursive codata, generic/witness relationships in host wrappers, transfer driver, cross-runtime context rooting |

Together these suggest roughly **3–5 engineering months**
for an initial synchronous native interlanguage platform covering the proposed type interfaces.
This is an order-of-magnitude estimate with lower confidence than the unit extensions.
Async callbacks, detached continuations, concurrent entry, recoverable native faults, additional host languages,
and equivalent Wasm/interpreter support require separate estimates.

### Principal pitfalls

1. **A complete calling convention is not a complete static interface.** Current SPS protocol agreement admits unknowns
   and is non-transitive.
   Treating it as an import compatibility check can accept unrelated interfaces.
   Retain checked public types before erasure and reject unsupported schema forms.
2. **Abstract identity must survive both sharing and separation.** Repeated imports
   of one nominal definition must agree; unrelated declarations
   and independent existential openings must remain distinct.
   Representation equality cannot replace those relationships.
   Dependency graph deduplication and binder reconstruction are correctness work.
3. **A static function cannot become an opaque runtime function automatically.** Consumers may need its body
   to reduce a value match, select a view, or construct a plan.
   Stage producer specialization explicitly and report remaining static requirements instead
   of deferring their failure into lowering.
4. **The existing C library lifetime is unsuitable for word sharing.** Its entries create fresh instances,
   and its shared libraries hide private runtime support.
   A Zydeco unit must use the caller's heap and frame store.
   Linking a second private support image or passing its closure pointer into the first heap violates that agreement.
5. **Roots and entry contracts must work across allocation.** Export environments,
   imported captures, boxed scalars, and temporary initializer results can all move.
   Exercise collection during initialization and inside a callee, odd/even argument stacks,
   and returns into another unit.
   Do not assume whole-program stack-parity knowledge at externally reachable code entries
   or expose native frame tokens as reusable values.
6. **Initialization sharing must be deliberate.** Re-evaluating each export independently can duplicate captures;
   caching arbitrary imported computations can suppress effects.
   Prepare one value environment, keep computation invocation explicit, reject initialization cycles,
   and preserve source-import behavior.
7. **A binding adds a second lifetime system.** Host contexts may retain Zydeco values
   while Zydeco closures retain host contexts.
   Rooting, teardown, exceptions, and reentry require explicit rules even when the source type is simple.
   The synchronous profile and instance-lifetime context retention in sections 5–7 bound this work.

The implemented minimum settles the word entry, runtime ownership, explicit initialization,
and exact compatibility boundary for structural units.
Extending public types can proceed independently of Python wrappers, general callbacks,
or a stable cross-version public ABI.

## 11. Implementation order and validation

Extend the implemented minimum in this order:

1. The richer unit interfaces in section 9, including nominal identities, recursive definitions, and binders;
   introduce a dependency-environment profile if shared automatic initialization is needed.
2. Complete abstract/dependent interface import, then persistent host instances with registered roots
   and immutable scalar/product/data access.
3. Host returning-thunk invocation, including a returned capturing thunk and state surviving host calls.
4. Argument/observation stack plans, generic forwarding, and transfer-driven host thunk implementations.
5. Abstract packages and dependent witness relationships in generated bindings,
   with explicit diagnostics for interface forms that a binding generator cannot yet represent.
6. Additional runtime profiles for nested entry, recoverable faults, or asynchronous work only after their lifetime
   and control rules are implemented.

Native word transport already supports more source forms than a first binding generator will expose.
Staging the generator does not turn those forms into C signatures or change their source semantics.
Direct compiler integrations can use the checked native profile where they implement its full obligations.

The richer compilation-unit profile has its own acceptance gate before host binding work.
Current structural-unit coverage belongs to the [implemented contract](../references/compiler.md#native-unit-artifacts):

| Accepted case | Rejected counterpart | Required evidence |
| --- | --- | --- |
| Compile a consumer using only producer interface/object artifacts | Missing interface definition or unresolved static factory | Remove the producer implementation source before checking/building the consumer |
| Pass a product, nominal constructor, and capturing thunk through two units | Wrong field layout, tag map, or nominal identity | Consumer-created values are consumed by the producer and vice versa |
| Share one dependency through a diamond-shaped graph | Two conflicting artifacts for the same resolved unit | Same published identity and one initialized export environment across both paths |
| Initialize and call under forced collection | An import/export retained only in an untraced global | Captures and pending return values remain correct after relocation |
| Forward a generic value and exercise recursive codata across units | Unsupported public binder form or incompatible residual protocol | Consumer checks the interface before entry; native tail chains keep bounded control state |
| Link matching target/model/runtime/profile artifacts | Mismatch, corrupted content, or initialization cycle | Fail before executing initialization or publishing the resulting artifact |

Native closure tests should return into the caller while the callee allocates,
and tail-transfer repeatedly between units.
Include both argument-stack parities and source compilation with local unboxing enabled,
checking that the public word boundary is still honored.

| Accepted case | Rejected counterpart or required contrast | Observation |
| --- | --- | --- |
| Immediate integers and boxed `Float64` values | Out-of-range integer input, invalid `Char` | Correct payload conversion; invalid construction publishes no value |
| Products with known order and nesting | Wrong arity, field type, or nested shape | Typed projection and construction agree with source behavior |
| Constructor creation through its published map | Unknown tag or wrong payload classifier | No malformed source value enters the program |
| A returned capturing thunk called repeatedly | Invocation after instance close or from another instance | Roots and code remain live; invalid invocation rejects before entry |
| Forced GC between host calls | An unregistered raw word held across collection | Registered roots relocate correctly; unsafe raw-word misuse is outside the checked API |
| Recursive codata observation sequences | Unknown observation or wrong branch argument | Preserve the full residual stack and reject malformed plans |
| Generic identity at several `A` types | A result reinterpreted at an unrelated instantiation | Instantiations erase from transport but remain related in interface checking |
| Shared existential opening | Carrier supplied to operations from an independent opening | Witness relationships survive generated bindings |
| Reused `StackPlan` and thunk | Replayed active `Cursor`, return port, or native frame token | New invocations get fresh delimiters; stale active tokens never dispatch |
| Host callback returning `Enter` or `Deliver` | Wrong protocol, wrong result, or host unwinding | Correct source transfer and bounded host stack for tail chains |
| Compatible independently compiled modules | Runtime/interface mismatch or incomplete descriptor | Loading fails before publishing an incompatible export |

Binding tests should cover both host-consumed and host-implemented interfaces, especially returned closures,
recursive codata, generics, abstract packages, and collection with external roots.
Manual address misuse remains a caller contract violation; typed handles cannot validate arbitrary foreign memory.
The initial context-retention and fatal-fault limits must appear in the published entry profile.

Focused tests of the implemented foundations are:

```sh
cargo test -p zydeco-machine --lib
cargo test -p zydeco-tests --test stack_protocols -- --skip every_backend
```

The source factory above can be verified by the documentation checker.
The extended package selection syntax and host APIs are schematic.
New interoperability tests are required as those boundaries are implemented;
passing current internal protocol tests alone does not establish a public ABI.
