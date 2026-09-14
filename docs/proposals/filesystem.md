# Stream construction and capability extensions

[L13](../references/language.md#streams-and-process-arguments) owns current byte-stream observations,
aliasing, and resource boundaries;
[C14](../references/compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls) owns host adapters.
The [library guide](../../lib/std/README.md#streams-and-files) lists operations and usage.
This proposal retains growable construction and additional stream protocols.

## Memory-backed Writer and byte builder

Repeated immutable append copies accumulated content, making byte-at-a-time construction quadratic.
A memory-backed writer could reuse the existing capability model for amortized constant-time pushes,
then produce one immutable `Bytes` result.
This is the sole design home for that builder.
The [immutable-byte reference](../references/language.md#immutable-owners-and-source-bytes) owns the resulting
value's observations; [memory extensions](bytes.md#alternatives-and-decision-criteria) retain storage alternatives.

Use the [CPS destination construction](bytes.md#cps-destination-construction) sequence for the proposed builder.
Each successful push advances to a completion continuation; it does not produce an immutable snapshot.
The construction's explicit finish should transfer its allocation to retention before delivering bytes to the consumer.
An optional snapshot operation would have a distinct name and preserve its contents across later writes.
The implemented byte builder provides the fixed-capacity core; choose growth policy
and integration with `Writer` during the extension.
The existing stream capability does not yet support this builder.

A memory-only builder can use a caller-chosen answer protocol when its allocator and memory provider support it.
Adapters using the current stream services retain their `OS` protocol; the manual allocator is answer-polymorphic.
Growth must reserve capacity and validate sizes before changing the builder's observable state.
Completion is reached only after the new chunk is committed; failure preserves earlier committed chunks.
Finish, failure, and explicit close must settle owner state before handing control to external successors.
Abandoning a completion thunk does not run cleanup, as specified
by the [current capability boundary](../references/language.md#allocation-and-release).

The builder must not expose a mutable alias to storage already borrowed as immutable bytes by foreign code.
Compare copying snapshots with explicit ownership transfer using workloads that construct many small chunks.
The implemented [fixed-capacity Buffer](../references/language.md#byte-builders) uses copying snapshots
and explicit retention; callers retire stale or published handles.
Pair successful growth with failed reservations that preserve the initialized prefix.
A stronger checked-handle wrapper would be a separate policy with its own runtime cost.
Measure growth copies, published snapshots, and continuation allocations separately.

## Incremental stream processing

The [current reader](../references/language.md#streams-and-process-arguments) already delivers a chunk
through a continuation.
A proposed chunk-processing driver can pass that chunk to its consumer and request the next read only
when the consumer invokes completion.
This avoids collecting the entire input before encoding, writing, or deciding to stop.
Keep the driver in `OS` while it uses the current blocking stream operations.
Explicit continuations give control over when to request more input;
asynchronous execution remains a separate extension.

The initial driver should require a positive chunk size so an empty successful read identifies EOF.
It must distinguish EOF, read failure, consumer failure, and an explicit stop.
Give each consumer step checked completion state so duplicate or cancelled resumptions are rejected before another read.
This is an additional driver contract; ordinary `Thk OS` does not provide it.
An omitted completion pauses the driver without automatically closing a caller-owned reader.
A file-owning wrapper must route EOF, failure, and stop through its close path before invoking the final successor,
using the existing whole-file error precedence.

Test a consumer that continues, stops after one chunk, fails, retains a chunk, or invokes completion twice.
Verify the number of underlying reads and that no read occurs after stop or close.
Retained chunks keep the immutable-byte contract;
buffer reuse needs the separate [ownership evidence](bytes.md#functional-updates-with-allocation-reuse-proposed).

## Other extensions

Buffered and seekable streams need explicit position and aliasing behavior.
Asynchronous operations need cancellation, pending-operation lifetime, and completion ordering;
changing a blocking implementation does not settle those protocol choices.
Platform-specific paths should be independently reviewable and preserve byte-stream composition.

Each extension should reuse the existing success/error boundary where it fits,
and pair normal use with closed handles, failed operations, and cleanup after partial progress.
Resource lifetime remains a separate question from immutable-byte representation and native continuation lifetime.
