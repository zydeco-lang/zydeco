# Runtime Garbage Collection for Zydeco

Status: draft extended abstract. This note proposes the first garbage collector for the
Zydeco native runtime, names it, and places it in the academic design space of garbage
collection. Incremental and concurrent collection are out of scope here except as forward
compatibility constraints.

## Implementation Status

The first three roadmap phases are implemented and covered by `gc_stress` end-to-end tests.
The interpreter has a toy mark-sweep over its `Vec<Value>` heap; the runtime has hidden
headers, growable segments, exact-size free lists, host-cell finalizers, and a precise
mark-sweep driven by emitted descriptors and stack maps. Three conservative backstops
remain, each documented below as a known limitation rather than a hidden assumption:
single-word cells are pinned because stack layouts are not yet propagated through
dynamically entered continuation blocks; the raw control-stack and environment ranges are
scanned for possible pointers; and freed cells wait one collection cycle before reuse. The
LLVM backend still links against the legacy non-collecting `zydeco_alloc`.

Related notes: `arena-gc.md` covers reclamation of the compiler's `StaticsArena`, a
different problem with deterministic rules; `paper-aligned-stackir.md` records the IR
presentation that supplies the stack layouts used below.

## Abstract

Zydeco's native runtime currently allocates language values from a fixed 1 MiB bump buffer
through `zydeco_alloc` (`runtime/stub.rs`) and leaks host-side cells such as `HostString`,
`HostBytes`, and `ControlTransfer`; no memory is ever reclaimed, so long-running programs
exhaust the heap by construction. We propose, as the first collector, a **type-directed,
tag-free, non-moving, stop-the-world mark-sweep garbage collector**. Compiler-generated
stack maps identify roots at allocation safepoints, derived from the stack analysis that
ZASM already performs at every program point, and per-allocation-site object descriptors
identify pointer fields inside flat product cells. The collector is type-accurate wherever
static types survive to lowering, and degrades to a conservative address-range test on
erased polymorphic fields. The design follows from five properties of the current system:
compiler-available static types, per-program-point stack layouts, a single-threaded
runtime, immutable heap objects, and a raw-word ABI shared with Rust host code. We
position the collector in the lineage that runs from McCarthy's mark-sweep through
Goldberg's and Tolmach's tag-free collectors and Diwan, Moss, and Hudson's
compiler-support approach; its closest engineering relative is the Go 1.3 collector, and
its layout-metadata machinery is cousin to GHC's info tables and stack-frame bitmaps. The
collector keeps tricolor marking and born-black allocation so that incremental collection
remains a possible later step, and it exploits object immutability so that a future
generational mode can build remembered sets at promotion time rather than at every write.

## 1. Problem

The native runtime in `runtime/stub.rs` has a bump allocator over one fixed 1 MiB buffer:

```rust
#[unsafe(export_name = "\x01zydeco_alloc")]
extern "sysv64" fn zydeco_alloc(size: usize) -> *mut u8 {
    // bump *HEAP_SIZE by size * 8; assert that the buffer is not exhausted
}
```

Every product pack emits a call to `zydeco_alloc`; the buffer never shrinks, and a
long-running program terminates with "Zydeco heap exhausted" once the bump pointer reaches
the end. Host-side values are in the same state: `HostString::leak`, `HostBytes::leak`,
and `ControlTransfer::leak` move Rust boxes into the raw-word world and are never freed,
and host-created `ZydecoClosure` boxes are consumed by assembly resume bridges without
being reclaimed. The runtime therefore needs a real collector before any long-lived use
of compiled Zydeco programs.

The tested native backend is AMD64 (`lang/amd64`); the LLVM backend (`lang/llvm`) is
experimental, and the assembly interpreter (`lang/assembly/src/interp.rs`) keeps a
`Vec<Value>` heap with its own index-based representation. This note plans the AMD64 path
first and treats the interpreter as a validation scaffold and the LLVM backend as a
follow-up.

The first collector should be **simple and correct** first, with the general-purpose
performance that can be obtained without redesigning the runtime ABI. Incremental
collection is deliberately deferred: it buys pause-time smoothness at the cost of total
throughput, and the current primary workloads are short-lived programs.

## 2. Academic Positioning

The first collector is, in full:

> a precise (type-accurate), tag-free, non-moving, stop-the-world mark-sweep garbage
> collector with compiler-generated stack maps and per-allocation-site object descriptors,
> and with a conservative fallback for erased polymorphic fields.

We use **type-directed tag-free mark-sweep** as the working name. Each term selects one
side of a standard classification:

| Dimension | This collector | Opposite pole |
| --- | --- | --- |
| Mechanism | tracing (reachability) | reference counting |
| Pointer identification | precise / type-accurate | conservative (Boehm) |
| Value representation | tag-free (out-of-band metadata) | tagged (Lua `TValue`, OCaml tags) |
| Reclamation algorithm | mark-sweep | copying, mark-compact |
| Object motion | non-moving | relocating |
| Scheduling | stop-the-world | incremental, concurrent |
| Generations | single-generation | generational |

The algorithm is in the direct line of McCarthy's mark-sweep collector [1]. The
conservative pole against which the design is defined is the Boehm–Demers–Weiser collector,
which treats register, stack, and static words as pointer candidates because the C/C++
toolchain supplies no layout information [2]. Zydeco sits on the opposite side of that
split: the compiler owns the types, so it can emit the missing information instead of
guessing at run time.

The tag-free lineage begins with Goldberg's argument that a strongly typed language does
not need runtime tags on values: types known to the compiler are sufficient to locate
pointers [3]. Tolmach's follow-up keeps scanning precise under polymorphic erasure by
passing explicit runtime type parameters [4]; this note records that alternative and
chooses, for the first step, a cheaper fallback in which a field of unknown erased type is
scanned as a *possible* pointer. The collector is therefore **type-accurate on monomorphic
fields and partially conservative on polymorphic fields**, and the plan should state that
qualification explicitly rather than claim full precision.

The engineering methodology is the compiler-support approach of Diwan, Moss, and Hudson:
a statically typed language generates pointer maps so that collection does not depend on
cooperating source code [5]. Its closest working relative is the Go 1.3 collector: a
non-moving, stop-the-world mark-sweep collector made precise by compiler-emitted stack
maps and per-type bitmaps, before Go 1.5 replaced it with a concurrent tricolor collector
[6]. The object-layout side is cousin to GHC's info tables and stack-frame pointer
bitmaps, which also keep values tag-free; GHC layers generational and moving collection on
top, while this plan takes only the metadata layer first [7]. The safepoint vocabulary
(`GC point`, root map) follows the JVM tradition of safepoints and OopMaps [8]. Lua's
incremental tricolor collector [9] is the named, deliberately deferred neighbor: the
tricolor discipline in Section 4.5 is kept so that this deferral is a later increment, not
a rewrite.

Finally, the CBPV value/computation distinction appears concretely as Zydeco's two-stack
machine, and the plan inherits its shape from that fact: roots have two channels (the
control stack `Rsp` and the environment stack `Rbp`), and both already have static layouts
per program point.

## 3. Observations That Determine the Design

The following facts about the current system fix most of the design decisions.

**O1. Precise types are available until lowering.** SPS lowering consumes the typed
`StaticsArena`, and `product_layout` already walks static types to compute physical
arity. The same walk can emit a pointer classification per field. Type arguments and
package witnesses are erased before evaluation, so fields whose type is a variable have no
static classification; those become `MaybePointer` (Section 4.2).

**O2. ZASM already computes per-program-point stack layouts.** `StackAnalyzer`
(`lang/assembly/src/analyze.rs`) computes `Layout { control, context }` for every program
point, with `Slot` shapes and with `Clear` recording variable liveness in context frames.
A GC root map is an extension of this analysis from shape to pointer-ness; it does not
require a new analysis framework.

**O3. The runtime is single-threaded.** The heap and host state are thread-local, so
stop-the-world means "stop the one world." There is no thread suspension machinery to
build.

**O4. Heap objects are immutable after allocation.** ZASM has no store-to-heap
instruction: `PackProduct` fills a fresh cell exactly once, and subsequent code only reads
fields. Closures are two-word packages (environment, code label) and strings are
immutable. Consequently no write barrier is needed for a full-heap collector, and a future
generational mode can compute old-to-young remembered sets by scanning objects at
promotion time rather than by instrumenting stores.

**O5. The ABI is raw words, shared with Rust host code.** Zydeco values, product
pointers, interior suffix pointers, and host cells (`String`, `Bytes`,
`ControlTransfer`) are all `usize` words exchanged across `extern "sysv64"`. Host callbacks
hold Zydeco closure words in Rust boxes and dereference the two-word layout directly.
Moving collection would force pointer updates inside Rust-owned structs and machine
registers, and interior pointers would need forwarding; non-moving collection eliminates
that whole problem class at the cost of fragmentation.

**O6. Memory growth passes through allocation sites.** A running program acquires
Zydeco memory only at `zydeco_alloc` call sites, so those sites are sufficient as
safepoints: a program that allocates nothing cannot exhaust the heap and needs no
collection. Host allocations do not trigger collection by themselves; Section 7 records
this as an open question for workloads that allocate strings without allocating products.

## 4. Collector Design

### 4.1 Object model: hidden headers

Every GC-managed object gets a header placed **before** its payload, while every Zydeco
word continues to point at the payload:

```text
[ header ] [ word0 ] [ word1 ] ... [ word(n-1) ]
^ base       ^ returned word (8-aligned, field offsets unchanged)
```

The header records the object kind, the payload size in words, and a reference to the
object descriptor. Because the returned pointer is unchanged, the existing AMD64 field
offsets (`[p]`, `[p+8]`, ...), the closure ABI (environment at offset 0, code label at
offset 8), and the `zydeco_str_*` / `HostBytes` ABI all survive the introduction of
headers.

The same rule applies to host cells: a `repr(C)` Rust cell stores the header in front of
its existing payload and leaks a pointer to the payload field, so `zydeco_str_byte_length`
still receives a `&String`-compatible pointer and the collector can find the header at a
fixed negative offset.

Interior pointers created by `UnpackProduct` (`pointer + last` suffix pointers) are
handled without moving anything: the map that describes the slot or field records the
static offset `k`, and the collector recovers the base as `word - 8*k`. Offsets that have
been erased into a `MaybePointer` field require a segment-level base lookup; this is
recorded as an open question rather than assumed away.

### 4.2 Object descriptors

Each allocation site emits one static descriptor, deduplicated at assembly time, derived
from the static type of the product being packed. A descriptor contains the payload size
in words and one classification per field:

| Class | Meaning |
| --- | --- |
| `Scalar` | fixed-width number, tag, code label: do not scan |
| `HeapPointer` | a pointer to a GC-managed object: follow it |
| `InteriorPointer(offset)` | points `offset` words into its base object: follow after recovery |
| `MaybePointer` | erased polymorphic field: test against the managed heap, follow if inside |

The closure descriptor is `[HeapPointer, Scalar]` (environment, code label). Strings and
byte buffers are leaves with finalizers. With this scheme, monomorphic code is fully
precise; only erased polymorphic fields retain the conservative address-range test. The
alternative of Tolmach-style explicit type parameters [4] would remove that residue by
changing the runtime ABI and is recorded for the future.

### 4.3 Stack maps and safepoints

Each `zydeco_alloc` call site is a safepoint. The emitter attaches a root map produced by
extending `StackAnalyzer`:

- **Control stack (`Rsp`).** For each live slot: scalar, heap pointer, or interior
  pointer with offset. `Slot::Sym(StringLiteral)` values are roots to host string cells;
  `Slot::Sym(Prog)` code labels are scalars.
- **Environment stack (`Rbp`).** For each live context variable, the same
  classification; `Clear` already tells the analysis which variables are dead.
- **Machine registers.** The emitted code must maintain the invariant that no root lives
  only in a caller-saved register across an allocator call; the stack-machine IR makes
  this the natural discipline, but the emitter must make it an asserted invariant. If a
  future optimization introduces register-resident roots, it must either spill them before
  the safepoint or record them in the map.
- **Runtime roots.** Host-owned cells that hold Zydeco words (active `ControlTransfer`s
  and closure boxes) are either registered as explicit roots or made GC-managed objects
  with trace functions; Section 7 keeps the choice open.

### 4.4 Allocation and collection

- The heap is a list of segments obtained from the Rust global allocator, replacing the
  single fixed buffer.
- Free space is organized as size-class free lists indexed by payload word count, with a
  separate path for large objects. Product cells have exact static arities, so exact size
  classes should absorb nearly all allocations.
- Mark bits live in a side bitmap and use the standard white/gray/black tricolor
  discipline with an explicit gray stack; marking is iterative, never recursive.
- Marking starts from the root maps plus runtime roots, follows descriptors, and stops at
  `Scalar` fields and leaves.
- Sweep clears mark bits, returns unreachable cells to their size class, and runs
  finalizers for dead host cells (freeing the Rust box). Finalizers initially must not
  resurrect objects or allocate managed memory; resurrection is out of scope for the
  first collector.
- Collection is triggered when bytes allocated since the previous collection exceed a
  threshold, or when allocation fails to find a free cell of the requested size.
- Allocation remains effectively O(1) per site: a size-class free-list hit plus the same
  initializing stores `PackProduct` already emits.

### 4.5 Forward compatibility

The collector is single-generation stop-the-world, but two decisions keep the later
options cheap. First, the tricolor state machine is used from the start; objects
allocated during a future incremental marking phase can then be born black without
reworking the mark loop. Second, because of O4, a future generational mode needs no store
write barrier: a remembered set entry is created only when an object is promoted and its
fields are scanned. The roadmap therefore treats incremental and generational collection
as independent, profile-driven extensions on top of this first collector.

## 5. Roadmap

Each phase ends with a validation criterion before the next one starts.

**Phase 0 — Toy collector in the interpreter.** Implement mark-sweep over the
interpreter's `Vec<Value>` heap, whose `Value` enum already carries runtime tags. Roots
are `runtime.stack` and `runtime.context`. Validate marking, free-list reuse, and
finalizer ordering with focused tests. This phase is deliberately ABI-free and exercises
the algorithm before metadata work.

**Phase 1 — Runtime object model.** Replace the fixed buffer with segments; add hidden
headers, descriptor records, size-class free lists, and finalizers. Bring `HostString`,
`HostBytes`, and `ControlTransfer` under the same header discipline. Validation: existing
native test suite remains green with unchanged field offsets and ABI.

**Phase 2 — Compiler metadata.** Emit per-site descriptors during SPS lowering and carry
pointer classifications into ZASM `Slot`s; extend `StackAnalyzer` to produce root maps at
allocation sites. Validation: a checker pass asserts every safepoint has a map and every
field has a class; no runtime behavior changes yet.

**Phase 3 — AMD64 end-to-end collection.** Thread descriptors and root maps through the
emitter, invoke the collector from `zydeco_alloc`, and add differential tests against the
interpreter plus long-running programs that would previously exhaust the buffer.
Validation: no exhaustion, no dangling references, interpreter agreement.

**Phase 4 — Backend unification.** Follow the same ABI for the experimental LLVM backend
and decide whether the interpreter adopts the shared object model or keeps its
self-describing `Value` representation.

**Phase 5 — Measurement gates.** Measure allocation throughput, maximum and distribution
of pause times, floating garbage from `MaybePointer` fields, and fragmentation on
long-running workloads. The results decide between the two deferred extensions:
generational collection (preferred if allocation rate or collection frequency dominates)
and incremental collection (preferred if pause tails dominate).

## 6. Alternatives Considered

**Conservative collection (Boehm-style) [2].** Rejected because it wastes the type
information Zydeco already has: false roots produce floating garbage, and host cells would
still need explicit integration.

**Reference counting.** Rejected because values move between the two stacks constantly,
and every such movement would pay a count update. Closure recursion creates cycles, so a
cycle collector would eventually be required anyway.

**Copying or generational collection first.** Rejected for the first step because moving
objects forces pointer updates inside Rust-owned cells, machine registers, and interior
suffix pointers. The payoff is real, but the risk should be taken only after the
metadata layer exists and measurement justifies it.

**Runtime tags on values.** Rejected because they change the raw-word ABI and every
product offset for no gain when static types are available.

**Immix.** Rejected as too much machinery for a first collector; revisit if Phase 5
measurements show fragmentation-dominated allocation.

## 7. Open Questions

Each question is annotated with the choice made by the implemented collector, or with
the reason it remains open.

**Descriptor placement. Resolved for the first collector.** `FieldClass` is computed in
SPS lowering from normalized statics types (`lang/stackir/src/sps/lower.rs`), carried
through SpsLow into ZASM `ProductLayout::fields` (`lang/assembly/src/lower.rs`), and
encoded into per-allocation-site byte descriptors by `lang/assembly/src/gc.rs` for the
AMD64 emitter. SPS remains the single source of type knowledge; ZASM is the carrier and
the emitter is only an encoder.

**Erased interior pointers. Preliminarily resolved with segment-level base maps.** The
runtime rebuilds a sorted `cell_bases` index before each collection and maps arbitrary
words to their containing cell with `resolve_segment_payload`. Explicit
`InteriorPointer` offsets are still used wherever lowering knows them; the alternative
of proving erased interior pointers impossible after lowering was not attempted.

**Host cells. Resolved: GC-managed traceable objects.** `String` and `Bytes` cells are
leaves with finalizers, `ControlTransfer` cells trace their three word fields, and
host-created closures are leaves with finalizers. They are registered in a payload
address set and swept alongside product cells.

**Finalization. Preliminarily resolved.** Finalizers run after sweep and only drop the
Rust box behind the host cell; they may not allocate or resurrect. There is no ordering
guarantee among finalizers, and an unresumed host closure still leaks its
argument-fold environment exactly as it did before collection.

**Host-only string allocation trigger. Open.** Collection still triggers only at
`zydeco_gc_alloc`; a program that allocates host strings without allocating products
can grow the host registry without bound. No host-side trigger has been added.

**Conservative scanning rate. Open; not yet measured.** The current implementation
scans more than the plan proposed (the raw control-stack and environment ranges are
full backstops), so any measurement now would overstate the polymorphic-field rate.
Tolmach-style explicit type parameters remain an unquantified future option.

**Root-map encoding. Preliminarily resolved.** The encoding is fixed-width: two `u32`
counts followed by 12-byte entries (`offset_words`, class, padding,
`interior_offset_words`). It is simple and shared by emitter and runtime, but no map
size or compression measurement has been made.

Newly identified during implementation:

- **Dynamic continuation entry layouts.** Stack analysis still seeds every named block
  with an empty control stack, so roots carried into dynamically entered continuation
  blocks can escape the emitted maps. The current mitigations are the raw stack and
  environment backstop, the one-cycle reuse delay, and pinning single-word cells. The
  real fix is fixed-point stack-layout propagation across jumps; until then the
  collector is precise only modulo these backstops.
- **One-cycle reuse delay.** Cells freed by a sweep become eligible only at the next
  collection. The delay is a safety margin, not a measured requirement; it should be
  revisited once the stack-layout fix above lands.
- **Environment stack growth.** The 1 MiB environment buffer is still a hard limit and
  is intentionally outside this collector's scope; it needs a separate growth
  mechanism (reserved address space, segmented stack, or heap-allocated frames).

## 8. References

1. J. McCarthy. Recursive functions of symbolic expressions and their computation by
   machine, Part I. *Communications of the ACM*, 3(4), 1960.
2. H.-J. Boehm and M. Weiser. Garbage collection in an uncooperative environment.
   *Software: Practice and Experience*, 18(9), 1988.
   <https://hboehm.info/spe_gc_paper/>
3. B. Goldberg. Tag-free garbage collection for strongly typed programming languages.
   *PLDI*, 1991. <https://dl.acm.org/doi/10.1145/113446.113460>
4. A. Tolmach. Tag-free garbage collection using explicit type parameters. *LFP*, 1994.
   <https://dl.acm.org/doi/10.1145/182409.182411>
5. A. Diwan, J. E. B. Moss, and R. Hudson. Compiler support for garbage collection in a
   statically typed language. *PLDI*, 1992.
6. The Go project. Go 1.3 release notes (precise stack scanning); Go 1.5 release notes
   (concurrent tricolor mark-sweep). <https://go.dev/doc/go1.3>,
   <https://go.dev/doc/go1.5>
7. GHC developers. GHC runtime system commentary: heap object info tables and stack
   frame layouts.
   <https://gitlab.com/bgamari/ghc-wiki/-/wikis/commentary/rts/storage/heap-objects>
8. OpenJDK. HotSpot Glossary: safepoint, OopMap.
   <https://openjdk.org/groups/hotspot/docs/HotSpotGlossary.html>
9. R. Ierusalimschy, L. H. de Figueiredo, and W. Celes. Lua 5.1 Reference Manual,
   garbage collection section. <https://www.lua.org/manual/5.1/manual.html#2.10>
