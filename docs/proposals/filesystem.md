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

Before choosing an interface, decide whether obtaining contents snapshots the buffer or freezes and closes the writer.
A snapshot must remain unchanged after later writes; a freeze must define what every writer alias observes afterward.
Specify close, repeated observation, failure, and buffer-transfer ownership together.
The builder must not expose a mutable alias to storage already borrowed as immutable bytes by foreign code.
Compare copying snapshots with a consuming freeze using workloads that construct many small chunks.
The implemented [fixed-capacity Buffer](../references/language.md#mutable-destination-capabilities)
uses copying snapshots and closing freeze with checked alias invalidation.
A growable memory-backed `Writer` remains separate; no spelling or integration
with the stream capability is selected here.

## Other extensions

Buffered and seekable streams need explicit position and aliasing behavior.
Asynchronous operations need cancellation, pending-operation lifetime, and completion ordering;
changing a blocking implementation does not settle those protocol choices.
Platform-specific paths should be independently reviewable and preserve byte-stream composition.

Each extension should reuse the existing success/error boundary where it fits,
and pair normal use with closed handles, failed operations, and cleanup after partial progress.
Resource lifetime remains a separate question from immutable-byte representation and native continuation lifetime.
