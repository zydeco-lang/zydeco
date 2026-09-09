# Stream capabilities and construction

Files and terminals need a common byte-stream abstraction so clients can reuse copying, encoding, and error handling.
The implemented [stream and file guide](../../lib/std/README.md#streams-and-files) owns the operations,
EOF distinctions, error precedence, and close behavior.
[C14](../references/compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls) owns host handle tables.
This design keeps the capability rationale and future stream questions together.

## Boundary choices

Immutable bytes are data; readers and writers grant access to a resource whose observations can change.
Keeping resource operations in `OS` exposes that distinction in their protocols.
Opaque handle IDs permit ordinary value aliasing while giving close one observable meaning across every alias.
This supports resource safety without pretending that the language already has linear ownership.

The host reports success or an error kind and message; the library constructs algebraic `Result` values.
That keeps the ABI independent of one library's data representation while giving programs stable cases to inspect.
Messages remain display text. Whole-file composition centralizes cleanup and error precedence instead
of requiring clients to rebuild the same open/operate/close protocol.

`io` owns shared streams, `fs` supplies file capabilities, and `stdio` supplies reserved process streams.
Process control remains a separate capability family.
Text conveniences explicitly encode or validate UTF-8; a typed UTF-8 `Path` prevents accidental text/path interchange
but cannot express every platform's native paths.

## Memory-backed Writer and byte builder

Repeated immutable append copies accumulated content, making byte-at-a-time construction quadratic.
A memory-backed writer could reuse the existing capability model for amortized constant-time pushes,
then produce one immutable `Bytes` result.
This is the sole design home for that builder; [byte representation](bytes.md) owns the resulting value's observations
and storage alternatives.

Before choosing an interface, decide whether obtaining contents snapshots the buffer or freezes and closes the writer.
A snapshot must remain unchanged after later writes; a freeze must define what every writer alias observes afterward.
Specify close, repeated observation, failure, and buffer-transfer ownership together.
The builder must not expose a mutable alias to storage already borrowed as immutable bytes by foreign code.
Compare copying snapshots with a consuming freeze using workloads that construct many small chunks.
The implemented [fixed-capacity Buffer](bytes.md#mutable-destination-capabilities) uses copying snapshots
and closing freeze with checked alias invalidation.
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
