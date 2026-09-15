# Compiling memory abstractions

A systems program should be able to describe a byte layout, allocate it with a chosen allocator,
initialize selected fields, and update an element without manufacturing a logical copy of the object.

**Ordinary values** leave their physical representation to the compiler.
**Explicit memory interfaces** establish programmer-selected storage contracts; optimization must preserve them. This
applies the existing [storage and representation boundary](../references/compiler.md#storage-evidence-and-machine-calls)
to the proposed interfaces and lowering.

The worked example follows a header-array update from source types to machine code.
The proposal retains the gaps identified on 2026-09-14 and gives them concrete interfaces and implementation boundaries.

The implemented foundation remains [L13](../references/language.md#manual-memory).
[`Storage`, `DynamicStorage`, `Codec`, `DynamicCodec`, and `StaticAlloc`](../references/language.md#independent-storage-and-codecs)
and the geometry-only record/array factories are implemented. Their contracts now live in L13.
[Pure address calculations](../references/compiler.md#address-calculations) and
[ordered scalar accesses](../references/compiler.md#ordered-scalar-memory-accesses) are implemented.
[Bounded raw memory kernels](../references/compiler.md#raw-memory-kernels) connect wide scalar accesses to arithmetic;
raw component transport across calls remains proposed compiler work. The
[interface example](../examples/memory-compilation/interfaces.zy) imports the implemented sealed std interfaces and
demonstrates geometry queries and explicit codec materialization. No source lifetimes, linearity rules, or new universe
of layout types is required.

## 1. Start with the bytes

Consider the [existing header-array example](../../lib/tests/std/header-array.zy):
an `Int` length followed by four `UInt32` elements, with the payload aligned to 16 bytes.
The length is mutable runtime metadata; capacity four and element stride four are fixed choices.

| Byte offsets | Meaning | Size | Required allocation alignment |
| --- | --- | ---: | ---: |
| 0–7 | Little-endian `Int` length | 8 | 8 |
| 8–15 | Padding before the payload | 8 | No value to initialize |
| 16–31 | Four little-endian `UInt32` elements | 16 | Payload boundary raised to 16 |
| Whole object | Original allocation base | 32 | 16 |

These offsets describe the addressed object.
An ordinary record containing the same logical values leaves its physical representation to the compiler;
selecting this storage layout fixes the bytes visible through its address.
The compiler can still optimize temporary values used to initialize or read those bytes.

The payload handle can be just `base + 16`; its type carries no length or alignment integer.
An inline-header view starts at `base`, reads the length there, and yields the payload pointer and that observation.
A prefix view starts at the payload pointer and reads the length at displacement `-16`.
A fat view instead receives a pointer and an explicit length from its caller.
These choices determine whether opening performs a load, arithmetic, or neither.
Reopening a header performs a fresh observation; a saved length does not track later mutation.

Nothing initializes padding implicitly. Whole-array initialization writes the four elements directly.
Initializing a partial prefix preserves its count in a builder;
spare capacity is not initialized by changing the header.
The allocation is freed using `base`, size 32, alignment 16, and the original allocator,
even if the logical length has since become zero.

For runtime capacity `n`, the same format has extent `round_up(16 + 4*n, 16)`.
Validate nonnegative capacity, multiplication, addition, and rounding overflow before allocation.
Retain `n` and the resulting geometry where allocation, indexing, and release need them;
they need not become fields of every pointer or fields of the stored header.
A zero-capacity payload has no dereferenceable element even though the header still occupies storage.

## 2. Give each source type one job

Storage describes a region of bytes; a codec relates that region to a logical value.
Here, *codec* means the encoding/decoding recipes: initialize storage from a value, or read a value from storage.
For the example, record construction needs the geometry of an element array, but never needs a `List UInt32`.
A field path answers where a child lies, while a view answers how a chosen handle reaches it.

| Type | Status and purpose | Runtime data |
| --- | --- | --- |
| `Addr` and scalar types, including `Int` and `Int64` | Existing compiler primitives | Address bits or the compiler's scalar representation |
| `Ptr L S` | Existing std pointer interpretation | One `Addr`; `L` and `S` erase |
| `Uninit`, `Init`, `Fields S T` | Existing std initialization states | None |
| `Storage L` | Implemented fixed allocation geometry, independent of a logical value type | Static recipe; querying it can materialize constants |
| `DynamicStorage L` | Implemented validated runtime geometry | Size and alignment, held separately from the pointer |
| `Codec L A` | Implemented statically selected initialization/read recipes | Recipes erase; any runtime operands or captured context remain subject to lowering |
| `DynamicCodec L A` | Implemented explicit materialization for runtime codec selection | Ordinary thunks and their necessary context |
| `Field Parent Child`, `DynamicField Parent Child` | Existing fixed/runtime displacement witnesses | No carried displacement for fixed fields; an integer for dynamic fields |
| `View P H A`, `DynamicView P H A` | Existing fixed/runtime handle interpretation | Chosen handle data; a materialized dynamic view additionally has an ordinary thunk |
| `StaticAlloc Context`, existing `Alloc` | Implemented fixed allocator code with explicit context; existing runtime operation interface | Context where needed; ordinary operation thunks for `Alloc` |

These interface types are source-defined. The compiler work is recognizing primitive effects,
supplying target facts, specializing known code, and selecting valid runtime representations.
There is no proposed builtin `Cell`, `Storage`, `Codec`, or `View`.

Using the existing `Cps A = forall R. Thk (A -> R) -> R`, the central shapes are:

```text
SizeAlign = (#size :: Int) * (#alignment :: Int)
Storage L = val pi (_ : Unit). SizeAlign
DynamicStorage L = SizeAlign                         -- private backing representation

Codec L A =
    (#init :: val pi (p : Ptr L Uninit) (a : A). Thk (Cps (Ptr L Init)))
  * (#read :: View Cps (Ptr L Init) A)

DynamicCodec L A =
    (#init :: Thk (Ptr L Uninit -> A -> Cps (Ptr L Init)))
  * (#read :: Thk (Ptr L Init -> Cps A))
```

### Cross between ordinary values and explicit storage

For an eight-byte little-endian layout, `Codec L Int64` stores all 64 payload bits through `init`.
Its `read` supplies an ordinary `Int64` to the successor; that value can use a register,
spill, or box according to the compiler and the agreed entry contract.
The stored bytes do not change when this temporary representation changes.
The same boundary applies to other logical types and codecs.
`Codec L A` assumes an existing `A`; defining its codec does not redefine its ordinary value representation.

The interfaces each constrain their documented boundary.
`Storage` supplies geometry, `Codec` supplies interpretation, `Ptr L S` supplies the address
and layout/state relationship, and the allocator supplies reservation/release behavior.
`Field` and `View` select access paths and observations.
Together they express explicit storage choices; they do not prescribe the native packing
of a handle product or change a function's calling convention.
Lifetime, alias, and synchronization obligations remain those of the selected operations.

### Keep geometry static until runtime data is requested

The [storage contract](../references/language.md#independent-storage-and-codecs) owns validated construction,
explicit materialization, layout identity, and retention of runtime geometry.
Fixed constructors enforce static operands; a value-function type alone does not prove its captured integers static.
Geometry remains bounded by nonnegative `Int`.
A full pointer-width size/offset interface and compiler-supplied target facts remain part
of the [memory extension](memory.md#additional-control-and-storage-boundaries).
The [integer and storage contracts](../references/language.md#storage-and-foreign-transport) determine
numeric domains independently of temporary boxing costs.

### Compose storage before selecting a logical codec

[Records](../references/language.md#typed-records-and-field-paths) compose `Storage Left` and `Storage Right`;
[arrays](../references/language.md#array-storage-and-element-builders) take element storage, capacity, and alignment.
Both expose geometry without requiring a whole-value codec.
Optional `codecs/product` and array codec selection supply logical conversion afterward.
Fixed and dynamic `Plan`/`Representation` conveniences use the same split.
The former combined `Operations L A` interface has been removed together with its callers.

For the example, storage construction fixes offsets 0 and 16, size 32, and alignment 16.
Its header view needs only the length codec. Whole-array reads are an explicit choice to construct `Values A`,
whose current representation is a managed list; direct element access and `init_each` avoid that work.
The [header-array fixture](../../lib/tests/std/header-array.zy) selects no array codec.
A requested logical list remains useful work and must be counted separately in comparisons.

Runtime arrays expose ordinary thunk interfaces and `DynamicStorage`; returning static value-function fields
through their success continuation would violate static elimination. Runtime record paths still need the
[checked dynamic field factory](memory.md#additional-control-and-storage-boundaries).
Typed unchecked indexing and no-read discard also remain
in the [memory interface extension](memory.md#independently-selectable-storage-operations).
Removing checks from only one benchmark path does not establish zero-cost abstraction.

## 3. Allocate and change state explicitly

The implemented `allocation/reserve` adapter obtains fixed geometry and explicit provider context,
asks the selected allocator for bytes, and passes `Ptr L Uninit` to its success continuation.
Initialization invokes the chosen codec at that address.
The codec interface does not require an allocator; the standard scalar/product codecs write
into the supplied destination.
A custom codec can capture an allocator or allocate logical resources, so its own contract and cost still matter.
A requested list or a runtime-selected codec environment can likewise have additional costs.

`StaticAlloc Context` separates known code from runtime allocator state:

```text
allocate : val pi (ctx : Context) (size : Int) (alignment : Int).
             Thk (Checked Fault Addr)
free     : val pi (ctx : Context) (base : Addr) (size : Int) (alignment : Int).
             Thk (forall R. Thk (Fault -> R) -> Thk R -> R)
```

A fixed heap adapter can use `Context = Unit`; an arena adapter can use an address of its mutable state.
Selecting that code statically permits a direct call or inlining, while the state is still explicit runtime data.
`materialize_alloc : StaticAlloc Context -> Context -> Alloc` is a value-function adapter
to the existing dynamic interface.
Passing `Alloc` from an unknown caller keeps its operation dispatch; it adds no allocator field to `Ptr`.
An allocation may be supplied by a heap, caller-owned buffer, or arena when
that provider's release convention is defined.
Stack/static placement and bulk arena retirement still need the lifetime boundaries described below.

The example's initialization path is:

```text
Ptr Object Uninit
  -> Ptr Object (Fields Uninit Uninit)       -- interpret empty record
  -> Ptr Object (Fields Init Uninit)         -- initialize the length
  -> Build Payload                          -- initialize elements, retaining prefix count
  -> Ptr Object (Fields Init Init)           -- complete payload, replace that child state
  -> Ptr Object Init                        -- finish the record
```

`Build Payload` here denotes the array package's abstract `Build`, not a new public type constructor.
Keep the enclosing record handle alongside that builder until `replace` supplies the updated parent state.
Its address and prefix count are ordinary values; type erasure does not yet prove their pair will be unboxed.
On element failure, the callback settles the current slot and returns it as `Uninit`;
the existing failure protocol exposes the completed prefix for cleanup or resumption.
Abandoning a callback runs no cleanup.
Copied pointers/builders can remain stale after a transition.

The additional access operations have these schematic shapes, with `L`, `S`, and `A` shared from their packages:

```text
at_checked   : Ptr Array S -> Int -> Checked Fault (Ptr Element S)
at_unchecked : Ptr Array S -> Int -> Ret (Ptr Element S)
forget       : val pi (p : Ptr L Init). Ptr L Uninit
read         : Ptr L Init -> Cps A
take         : Ptr L Init -> forall R. Thk (Ptr L Uninit -> A -> R) -> R
```

Fixed element operations are recipes returning these computations; dynamic selection materializes ordinary thunks.
`at_checked` retains capacity and displacement checks; `at_unchecked` transfers bounds
and arithmetic validity obligations to its caller.
The current `elements/unsafe/at` is still checked.
`forget` is an unsafe reinterpretation with no load, store, byte clearing, or destructor;
owned contents and aliases must first be settled.
`take` instead reads the logical value and then changes interpretation.
`free` requires `Uninit` and the original allocation geometry and allocator.
Pure address/state calculations use value functions or `Ret`; memory observations and writes use CPS.

To increment an initialized `UInt32`, obtain its pointer, read the old value, compute wrapping addition,
forget that slot's old interpretation, and initialize it with the new value.
For this scalar there are no owned subresources to destroy; the update requires a valid exclusive mutation discipline
from the caller, without introducing a language-level uniqueness proof.
The same sequence for a resource-bearing codec must settle its old resources first.

## 4. Compile the operation, preserving its contract

The explicit storage contract constrains optimization without prescribing every machine instruction.
Redundant accesses or temporary storage may disappear only when the selected contract and observable behavior survive:
chosen layout and encoding, access semantics, observable address identity, allocator effects, and failure behavior.
In particular, a nonescaping pointer alone does not justify deleting a user-selected allocator call.
Volatile/atomic access and guaranteed storage placement need their own contracts
in [the memory extension](memory.md#additional-control-and-storage-boundaries).

Compare an abstraction with a direct implementation of the same observable behavior on the same target:
the same bounds and failure behavior, alias obligations, representation, and explicitly selected runtime dispatch.
Required dynamic checks, requested logical values, and deliberately materialized runtime metadata count as useful work.
Additional packaging, allocation, copying, dispatch, or retained control storage needs justification.
Matching the raw Zydeco API is an intermediate milestone because that path also has avoidable overhead.

[Static elimination](../references/language.md#10-static-elimination) already removes fixed recipes and type witnesses.
`Ptr L S` has no state or layout fields, and fixed fields need no carried displacement.
Ordinary products, operation thunks, captured values,
and continuation frames still follow the [compiler representation policy](../references/compiler.md#policy-selection).
The [small View comparison](../evaluations/2026-09-14-memory-abstractions/README.md) shows one fixed view
matching its raw baseline, while a library header view retains extra generated work.
Its counts are static code sites, not executed allocations or timings.

### 4.1 Specialize known operations

Fixed layout construction can still produce ordinary operation packages containing integers and thunks.
Erasing the static recipe does not prove that all of those residual values disappear.
Current [local reductions](../references/compiler.md#local-reductions)
and [consumer demands](../references/compiler.md#consumer-demands) have limited sharing and call visibility;
they do not provide general recursive or interprocedural specialization.

Propagate known layout operations and callbacks through helpers, shared calls, and recursive loops.
Remove operation dictionaries, captured constants, and indirect calls when their selection is known.
Preserve explicit runtime selection at unknown boundaries, and bound specialization by code size and compilation cost.
The [escape and representation proposal](escape-unboxing.md) owns the supporting analysis;
a specialized entry must also respect its [machine-call boundary](escape-unboxing.md#remaining-machine-call-boundary).

The intended staging boundary is concrete: fixed geometry and paths reduce to integer constants;
applying `Codec/read`, `Codec/init`, or a fixed `View` exposes its computation before closure conversion.
A runtime address, index, length, or allocator context remains a runtime operand.
A computed dynamic descriptor remains data until ordinary analysis proves its contents known.
Specialize a shared worker by known recipe identity and entry contract, reusing that worker at matching calls;
recursive specialization needs a finite cache and a conservative fallback, not unrestricted unfolding.

### 4.2 Represent memory effects in the IR

The compiler implements [pure address calculations](../references/compiler.md#address-calculations)
and [ordered scalar memory accesses](../references/compiler.md#ordered-scalar-memory-accesses).
Those references own builtin recognition, effect order, scalar-domain validation, exact carrier widths,
unaligned access, native instructions, and interpreter/Wasm adapters.
The earlier header-view probe predates these operations; its remaining dictionary, callback,
and scalar-boundary costs still need end-to-end measurement.

Extend the access domain to overlap-safe `Copy` and byte-pattern `Fill` computations;
these currently retain their existing host calls.
Propagate stronger alignment only from validated allocation/projection evidence.
An erased witness or asserted raw address alone does not authorize an aligned machine access,
`noalias`, or an in-bounds pointer promise.
Volatile and atomic operations need their own ordering contracts.

Expose known arithmetic and redundant checks while preserving required validation and effect order.
Deleting or moving an access needs semantic evidence; neither an unsafe API nor `Ret` supplies it.
Keep effects outside pure arithmetic evaluation and value commoning.
Allocator calls remain provider calls; direct scalar access does not require replacing the allocator.
Bounded local kernels now remove eligible scalar encoding between an access and its arithmetic consumer.
Section 4.4 covers the remaining transport boundaries.
Measure target and embedding costs separately, including Wasm virtual-memory lookup.

### 4.3 Make eligible continuations into blocks

Residual success and failure thunks can require closure environments and retained activation storage.
The proposed [contification analysis](escape-unboxing.md#local-cps-continuations-proposed) would turn eligible known,
fully applied, nonescaping uses into blocks and jumps.
Compatibility of ambient stacks and entry contracts must survive lowering.
Host operations need explicit retention and invocation contracts before their callbacks qualify.

Preserve invocation multiplicity, effects, and escaping or unknown uses.
The [Ret/CPS convention](../references/language.md#ret-and-explicit-cps) does not prove purity to the optimizer,
single use, nonescape, cleanup, or bounded stack extent.
Frame reclamation belongs to the [native environment proposal](native-frames.md#remaining-decisions).

For the local update, successful indexing, load completion, and store completion become block edges.
This removes the need to manufacture a callback closure at each step.
A callback saved in an object, passed to an unknown function, or entered
under an incompatible residual stack keeps the existing first-class representation.
Repeated invocation is preserved; CPS is not assumed affine.
Contification alone supplies neither a stack-allocation lifetime nor permission to reclaim a retained activation.

### 4.4 Carry raw components only across agreeing entries

The implemented [raw memory kernels](../references/compiler.md#raw-memory-kernels) handle
bounded load–arithmetic–store chains of wide scalars with no implicit allocation inside the kernel.
Their [code-generation probe](../evaluations/2026-09-15-memory-kernels/README.md) separately counts
ordinary boundary costs; raw stack homes and Wasm virtual-memory imports remain.
This is a local computation contract.
Unknown callbacks, shared results, and other control edges still use the existing source calling convention,
so the broader worker and component transport below remains proposed.

Once a codec supplies an ordinary value, scalar representation and ABI design determine its transport.
The [scalar representation contract](../references/compiler.md#scalar-value-boundaries) owns the current implementation;
the [escape and unboxing proposal](escape-unboxing.md) owns its remaining extensions.
The [machine-call proposal](escape-unboxing.md#remaining-machine-call-boundary) owns entry and tracing prerequisites.
This memory proposal requires compatible lowering across those boundaries, without fixing the unfinished `Int64` ABI.
Both `Ret Int64` and a CPS successor accepting `Int64` can use raw transport when their entry contracts support it.
Their control protocols do not themselves select boxing.

[Local unboxing](../references/compiler.md#product-layout-and-local-unboxing) removes some product cells
while retaining the tagged-word field convention.
Wide scalars can still box; pairs, fat handles, array builders, and closure environments can still allocate,
especially when passed or captured.
Selecting the layout of manually allocated payloads does not select the representation of these ordinary values.

Extend representation evidence across producers, consumers, calls, returns, recursion, and captures.
Support raw scalar registers and aggregate components where valid, with matching caller/callee contracts,
explicit conversion boundaries, and exact live-reference maps.
Source layout plans alone do not establish a machine ABI or reference-scanning contract.
Interprocedural escape and demand evidence must justify stack storage or cell elimination.

The following separates proposed raw workers from the currently documented general word interface;
the latter is a boundary convention, not a permanent boxing requirement for the source type:

| Value | Local specialized worker (proposed) | General word interface (current boundary) |
| --- | --- | --- |
| `Ptr L S` | Raw address register/slot | One unmanaged address word |
| `UInt32` element | Raw 32-bit integer operand | Tagged immediate word |
| `Int` index or loaded length | Raw 64-bit carrier for a 63-bit payload | Tagged immediate |
| `Int64` or `UInt64` in a full-width variant | Raw 64-bit payload | Pointer to an opaque scalar box |
| `(payload, length)` or `Build` | Separate components when uses and entries agree | Ordinary product unless current local analysis eliminates it |
| Static recipe/layout/state witness | No transported component | Static forms must already have erased |
| Escaping callback/environment | Proven local components only where possible | Existing closure and lifetime convention |

The representation analysis must record component identities on both caller and callee entries.
Insert encode/box or decode/unbox adapters where producer and consumer representations differ;
changing one side's packing is invalid.
The current general word entry can require boxing; a future typed raw entry can accept raw components even
when the callee's body is unknown.
Raw scalar spills are not GC roots. Managed components retain precise root locations across collection,
and mixed raw/reference aggregates require the corresponding tracing contract.
An unknown callback using the word convention may therefore retain wide-scalar boxing even
after the load itself becomes a native instruction.

C already transports `Int64` as raw `int64_t`
under the [foreign contract](../references/language.md#storage-and-foreign-transport).
Eliminating intermediate boxes around a C call remains a representation optimization; all surviving adapters count.
Declaring stored bytes never supplies a C aggregate classifier or changes the ordinary Zydeco call contract.

The thin-payload update should reach this illustrative control-flow IR:

```text
update(payload: addr, index: i64, delta: u32):
  if index < 0 or index >= 4: jump bounds
  slot = addr.offset(payload, index * 4)
  old = load.le.u32(slot, alignment=1)
  new = add.wrap.u32(old, delta)
  store.le.u32(slot, new, alignment=1)
  jump done
```

With raw components already in registers and local `bounds`/`done` targets, the intended AMD64 hot path can be
as small as this schematic sequence:

```asm
; rdi = payload, rsi = index, edx = delta; this is not a published calling convention
 test rsi, rsi
 js bounds
 cmp rsi, 4
 jae bounds
 mov eax, [rdi + rsi*4]
 add eax, edx
 mov [rdi + rsi*4], eax
 jmp done
```

The failure target and any interface adapters must be counted too; this is a compilation target, not current output.
For fixed index three, even the index checks and multiplication disappear after proof of bounds.
For the inline-header version, add one fresh 64-bit length load with `Int` carrier validation,
then checks for `0 <= length <= 4` and `index < length`; address the element at `base + 16 + 4*index`.
Remove or combine checks only with evidence preserving their rejection behavior and effect order.
That version has two necessary loads: header metadata and the element.
The thin version needs only the element load.
Neither version reads padding, copies the array, performs a whole-value `take`, or needs runtime layout dispatch.
Unknown dynamic bounds retain their checks and unknown operation selection retains dispatch.

## 5. Extend control without conflating the boundaries

The [memory proposal](memory.md#additional-control-and-storage-boundaries) also records missing capabilities:
packed and overlapping typed layouts, checked dynamic field paths, target facts, stack/static/arena storage conventions,
managed-reference storage, pointer operations, atomics, volatile access, and memory ordering.
These extend what programmers can express; they are distinct from overhead in operations already expressible.
That proposal links byte ownership/reuse, growable storage, foreign ABI work, and backend-specific costs.
The approved retained-`Bytes` policy remains deliberate.
Early specialization and lowering work requires no new source lifetimes or linearity rules.

## 6. Establish costs and implement in slices

Functional tests do not establish allocation, copying, dispatch, or space bounds.
The [representation comparison](../../cli/examples/representations.rs) counts portable product/closure
construction sites; it does not count their executions, all native allocations, or retained frames.
Add focused code-generation oracles and executed allocation/copy counters,
then measure representative optimized native workloads, peak retained storage, code size, and compilation cost.
Pair each abstraction with an equivalent direct implementation and keep target/profile information with results.

For selected hot paths, explore compiler diagnostics or enforceable requirements
for no residual implicit allocation or dispatch within a defined scope.
Such a contract needs explicit dynamic boundaries.
An explicit strict requirement must reject a path the compiler cannot establish;
ordinary compilation retains a correct fallback.
Do not infer a universal optimal-code guarantee from a few examples or from static erasure.

### First acceptance target and order

Start with a typed indexed update using a known layout and callback.
Against a direct implementation with identical checks, it should perform the necessary bounds checks,
address calculation, one scalar load, and one scalar store, with no additional managed allocation,
operation dispatch, or callback packaging.
Also record avoidable costs shared by both paths so the raw baseline does not become the final ceiling.

1. Retain paired fixtures and establish code-generation and executed-cost baselines.
   Cover fixed and runtime layouts, zero-sized elements, overflow and out-of-bounds rejection,
   unchanged storage on rejected operations, and effect order.
   Pair removable private accesses with address-observing consumers and allocators whose effects/failures must survive.
2. Close the residual primitive, known-call, CPS, and local-representation gaps exposed by that case.
   Pair optimizable callbacks with unknown, retained, and repeatedly invoked counterparts;
   preserve aliases and general control behavior when optimization is unavailable.
3. Extend the same checks to loops, builders, modular calls, recursion, and mixed raw/reference values.
   Introduce component transport and stronger cost contracts only with the corresponding entry,
   lifetime, and collection evidence.
   Include full-width `Int64`/`UInt64` arithmetic and C transport, plus `Int` payload boundaries and invalid carriers;
   compare raw and word entries with every required conversion counted.

Treat each successful target as a scoped guarantee before expanding its domain.

### Concrete implementation boundaries

| Slice | Files/boundary to change | Required evidence |
| --- | --- | --- |
| Source interfaces (implemented) | [L13](../references/language.md#independent-storage-and-codecs), std factories and callers | Shared witnesses; storage-only array composition; explicit logical conversion; fixed/runtime carrier rejection |
| Static target facts | Layout factory inputs and compiler target/profile identity | Pointer width/alignment and geometry bounds known before static layout reduction; runtime inputs rejected on the fixed path |
| Memory operations | `lang/stackir/src/high/lower/builtin.rs`, high/low syntax and conversion, assembly, AMD64, interpreter/Wasm adapters | Source-domain validation and exact access width/endian/alignment; wrapping addresses; ordered effects; no zero-offset host call |
| Known workers and contification | High SPS use analysis and normalization before closure conversion | Shared/recursive calls, stack compatibility, unknown/escaping/repeated callbacks |
| Raw component transport | Representation analysis, low entry contracts, native preparation, root maps, emitters | Matching entries and adapters; wide scalar extremes; mixed references surviving collection |
| Cost regression and diagnostics | Paired code-generation fixtures and execution counters | Allocations, copies, retained frames, code size, compilation cost, and scoped strict-contract failures |

The existing [memory-control boundaries](memory.md) keep packed/overlapping layout, dynamic field validation,
additional allocators, managed references, pointer operations, and synchronization on the agenda.
Functional byte reuse and FIP-style resource guarantees keep their separate ownership evidence;
no memory-state alias becomes a uniqueness proof through this proposal.

## 7. What existing practice supports

The following are design precedents, not evidence that Zydeco already generates the proposed code.
Zig links are pinned to 0.15.2 so the interface being discussed is reproducible.

| Precedent | Mechanism to learn from | Application here |
| --- | --- | --- |
| [Zig memory management](https://ziglang.org/documentation/0.15.2/#Memory) | Allocating APIs conventionally receive an allocator; lifetimes remain programmer responsibilities | Keep allocation and release explicit |
| [Zig comptime](https://ziglang.org/documentation/0.15.2/#Compile-Time-Parameters) | Static parameters specialize code; required compile-time evaluation rejects runtime dependence | Fixed storage/recipe factories need a checked staging boundary |
| [Zig alignment](https://ziglang.org/documentation/0.15.2/#Alignment) and [slices](https://ziglang.org/documentation/0.15.2/#Slices) | Pointer alignment is typed; array length is static, slice length is runtime | Separate geometric facts from carried counts; retain evidence for stronger accesses |
| [Zig result locations](https://ziglang.org/documentation/0.15.2/#Result-Locations) | Supported initializers propagate destinations into fields | Initialize directly into projected destinations |
| [Zig extern structs](https://ziglang.org/documentation/0.15.2/#extern-struct) and [packed structs](https://ziglang.org/documentation/0.15.2/#packed-struct) | C layout and packed bit layout are distinct choices | Specify packing and foreign ABI independently |
| [Zig Allocator source](https://github.com/ziglang/zig/blob/0.15.2/lib/std/mem/Allocator.zig) | Context pointer plus vtable; size/alignment pass to raw allocation/free | Static allocator code and explicitly materialized dispatch are separate choices |
| [Zig fixed-buffer allocator](https://github.com/ziglang/zig/blob/0.15.2/lib/std/heap/FixedBufferAllocator.zig) and [arena](https://github.com/ziglang/zig/blob/0.15.2/lib/std/heap/arena_allocator.zig) | Caller-owned buffer/cursor; arena chunks released through a backing allocator | Storage policy and retirement belong to the provider, with explicit state |
| [Zig AIR](https://github.com/ziglang/zig/blob/0.15.2/src/Air.zig) | Explicit pointer arithmetic, field/element addressing, loads, stores, and allocation instructions | Give memory operations typed IR forms before target emission |

Zig's `comptime` guarantees staging; its allocator interface still exposes runtime dispatch.
Its lexical `defer` and ordinary function extents do not justify cleanup or stack retirement for arbitrary Zydeco CPS.
The transferable ideas are explicit representation and allocation boundaries,
not an assertion of identical control semantics.

[LLVM's memory instructions](https://llvm.org/docs/LangRef.html#memory-access-and-addressing-operations)
separate address computation, access alignment, volatile behavior, and atomic ordering.
That is a useful contract checklist for our IR; this proposal does not require adopting LLVM as a backend.

Andrew Kennedy's
[*Compiling with Continuations, Continued*, Section 5](https://www.microsoft.com/en-us/research/wp-content/uploads/2007/10/compilingwithcontinuationscontinued.pdf)
explains contification from functions to local continuations. Maurer, Downen, Ariola, and Peyton Jones's
[*Compiling without Continuations*](https://pauldownen.com/publications/pldi17.pdf) provides GHC join points as an
implementation precedent for preserving local control-flow identity.

The [escape/reuse proposal](escape-unboxing.md#related-work) retains the CPS, Perceus, and FP² references
and their distinct guarantees; the [byte proposal](bytes.md#related-work) retains destination-passing
and bufferization references.
