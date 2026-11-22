# C11. Native Preparation, Activation Frames, and AMD64 Emission

[NativeProgram::prepare](../../../../lang/assembly/src/frames.rs) validates the frame-aware ZASM result
before [AMD64 emission](../../../../lang/amd64/src/emit.rs).
The checked product contains activation ownership, entry contracts, initialized bindings,
continuation provenance, packed frame slots, and root/suspension maps.
Invalid preparation is a `FramePlanError`; emission cannot silently fall back to an unchecked environment layout.

`BackendProgram::emit_amd64` returns an `Amd64Artifact` containing assembly text
and the foreign imports and libraries collected from that same native program.
CLI and test builds link this artifact after one lowering pass.

| Entry role | Required transition |
| --- | --- |
| Local branch | Keep the current activation and its established bindings. |
| Closure entry | Establish an activation from captures and incoming arguments. |
| Return continuation | Restore the retained activation and initialize the result binding. |

`ContinuationEntry` provenance records the returned-value pattern, the body after the portable capture preamble,
and the source-to-capture binding relation.
Validation checks these against the actual record and preamble;
metadata does not introduce another executable occurrence.
Native lowering replaces capture construction and unpacking with `RetainFrame` and resumption aliases.
Ownership follows local and suspension/resumption edges; forward dataflow verifies initialized bindings independently
of ZASM context annotations.
Captured aliases resolve recursively to their original definitions.

Slot assignment combines ordinary liveness with preservation by pending continuations.
A later result may reuse a dead slot only when no pending continuation still needs its binding.
Nested continuations can retain different subsets of one activation;
root and interference information must include all pending uses.
A forward may-analysis unions pending captures at joins; a resumption consumes its own suspension
while preserving older ones.
A write interferes with every binding preserved across it, even when the written result is dead.
Deterministic greedy coloring assigns canonical definitions to slots; aliases inherit their source slot.
Packed size is a safe bound from the assigned slots, not an optimal-coloring or recursion theorem.
The same offsets are used for accesses, capture descriptors, and active root maps.

## Activation Lifetime

An activation is one dynamic invocation, potentially spanning local blocks and several resumptions;
a block label alone does not establish a new activation.
The environment stores the current bindings, while a capture environment stores the values needed
when suspended code begins or resumes.
Portable lowering copies continuation captures into a heap product and unpacks it on return.
Retained native frames preserve the same values in their existing slots, avoiding that capture copying.
Ordinary escaping closures still own heap capture environments.

With the control-stack top on the left, a portable call enters with `E_f :: argument :: L_k :: E_k :: S`,
where `E_k` is the continuation's capture product.
Native preparation replaces it with `E_f :: argument :: L_k :: suspension(F) :: S`;
the suspension ID retains the caller activation `F` and the entry descriptor identifies its resumption layout.
An independent allocation frontier tracks reserved environment storage as well as the active base.
Restoring only the base would not reclaim or preserve the correct extent.
The environment and control stacks remain separate, preserving the ordinary argument protocol.

The default retained environment stores frames in growable Rust-owned storage.
Closure entry may reserve and relocate that storage; generated code reloads its active base into `rbp`.
Saved references are logical offsets and suspension IDs, never raw environment-slot pointers
in managed values or closures.
Suspend and Resume preserve the model's nesting discipline.
Resuming consumes the most recent pending suspension, restores the owning frame and frontier,
and releases younger unretained storage.
A tail transfer reuses an unretained active frame or preserves it beneath its callee when a continuation still needs it.

The lifetime justification comes from the concrete lowered stack operations:
SPSLow values cannot contain a residual machine stack, checked continuation bodies cannot refer
to their own entry label, and native continuation opening consumes its suspension ID destructively.
Recursive closure invocation establishes another activation instead of reentering a suspended one.
Source `Ret` types and lexical node occurrence counts alone do not establish this property.
Escaping ordinary closures own heap captures with adequate lifetime.
A future machine-stack capture or multi-shot continuation operation must revisit preparation and storage together.

Arguments and closure captures must be staged before their source storage can be reused.
Every pending continuation preserves its own binding set;
resuming an inner continuation cannot destroy an older continuation's captures.
A return stages its result and target before restoring the owner and frontier.
Environment transitions do not collect managed values.

A jump is not itself evidence of a tail call: calls with explicit continuations also lower to jumps.
With a fixed pending continuation set, a tail chain's logical frame extent is bounded
by its retained storage plus the largest active frame, independently of chain length.
Cached capacity can retain earlier peaks.
Reclamation requires that active use has ended and no pending continuation can reach the frame.
No managed value, escaping closure, or foreign borrow may contain a raw pointer to an environment slot.
Knowing an entry layout establishes slot availability, not constant contents or a general static fact system.

At collection, root maps include active and all suspended live slots, together with control-stack and host roots.
Reserved or dead slots are not roots even when their words look pointer-shaped.
Changing frame storage must preserve this distinction:
retaining one small capture must not keep a dead large value alive.
The [environment capability](native-runtime.md#environment-actions-and-roots) defines address stability
and model transitions.

The emitter serializes model-defined actions and implements SysV register placement,
stack alignment, jumps, return prologues, and host bridges.
Stack alignment propagates the parity of `rsp` along static edges from the root.
Every other frame entry starts with both parities, because blocks and closed labels can be entered
from several activations; a program reached with both parities aligns its host calls dynamically.
[Native packaging](../../../../cli/src/native.rs) bundles the runtime and shared model
and invokes the target toolchain using [the CLI build workflow](../../../../cli/README.md#native-executables).
[Native-model tests](../../../../lang/tests/tests/suite/native_model.rs) check preparation and transitions;
[native frame proposal](../../../proposals/native-frames.md) retains alternative storage and evaluation decisions.
Model regressions reject mismatched layouts, uninitialized reads, stale or out-of-order suspension IDs,
and reservation overflow without corrupting callers.
Tail-chain tests check bounded usage, while GC tests keep a moved object alive solely through a suspension
and exclude dead slots in the same frame.

## Compilation-Unit Preparation and Artifacts

Independent emission fixes a contract between a producer and consumers that need not share its source.
The designs of compilation units, [FFI](../../language/src/foreign-interfaces.md),
and [package management](../../language/src/sources/packages.md#source-packages) are therefore coupled.
For compiled libraries, FFI determines which representations, control transfers, and ownership may cross the boundary;
unit selection checks that the chosen source implements a supported entry profile.
Package management names and resolves providers.
Selecting or distributing compiled providers also depends on their interfaces, target and profile compatibility,
and dependency closure.

Changes to any of these contracts must be reviewed against the other two,
with affected rules and interfaces updated in the same design change.
An FFI extension can change unit eligibility, import/export adaptation, runtime entry
and teardown, generated interfaces, and artifact compatibility.
Conversely, changes to unit boundaries or package resolution can require new foreign contracts.
For example, retained callbacks would require a unit lifetime and reentry protocol and compatibility information
for consumers of its distributed artifacts.

The checked [entry representations](../../../../lang/session/src/source/query.rs) describe a process program,
a collection of C exports, or a [native unit initializer](#native-unit-artifacts).
The source roles and legality rules belong
to [L14](../../language/src/foreign-interfaces.md#compiled-libraries-and-c-exports).
[Library checking](../../../../lang/statics/src/check/library.rs) reuses the source judgment query,
Builtin domain validator, ordinary field resolver, directional foreign classifier,
static elaborator, and readiness checker.
The checked product retains one residual root and scalar signature per export in a shared typed arena.
`RootLowerer::run_with_builtin` supplies the optional validated provider through the same SPS entry path used
for executables; subsequent SPS, representation, and native-frame passes are shared.

Each `CExportEntry` adds a SysV adapter around its prepared native root.
It saves the caller's callee-saved registers and raw arguments outside the traced stack range,
acquires the unit guard, creates the instance, and pushes a typed external return delimiter.
Arguments are converted in reverse stack order without integer allocation.
Scalar encoders truncate unspecified upper argument bits to the C carrier width and check the source payload range.
The residual root receives the ordinary argument stack and `Ret` completion, without an artificial `OS` exit.
On return, the adapter decodes the result before teardown and restores the C stack and preserved registers.

[LibraryBuilder](../../../../cli/src/library.rs) validates exports and dependencies before invoking native tools.
NASM emits one object per export and one shared guard object.
Private source labels remain local to each object.
A relocatable link combines them for `object`; `ar` creates `staticlib`.
`sharedlib` links them with matching runtime support built in release mode with PIC and aborting panics.
ELF external calls use PLT relocations; source code references are relative
and jump-table relocations follow the existing ELF/Mach-O emitter conventions.
An ELF version script or Mach-O export list hides every symbol except the declared C exports, including runtime support.
Shared runtime references bind within their own image.
Host resumption bridges belong to the model's runtime support once per image, rather than each compiled unit.

Raw objects and static archives ship a separate matching runtime archive.
A C link includes that support once after the unit code and its dependencies;
a Zydeco executable supplies it through its own runtime build.
Executable packaging first combines raw units with the program object,
ensuring their helper references are visible when the final linker scans the Rust runtime archives.
Model and runtime identities must agree across raw units sharing support.
A shared artifact contains private support and can be consumed across compiler versions compatible
with its public profile.
AMD64 target and manifest compatibility are checked before linking or interpreter execution.

The versioned manifest records package identity, target, entry profile, compiler/model/runtime fingerprints,
artifact and interface content hashes, selected export paths and C signatures, imports, and dependency manifests.
Its association with resolved packages follows the
[package resolution implementation](sessions-and-queries.md#package-resolution-implementation).
The generated header spells fixed-width C types and `void`;
the generated `.imports.zy` preserves the selected field structure using typed `ffi` declarations
and canonical type intrinsics.
It requires neither the producer's source nor its static-only interface.

`LinkedLibraries` resolves explicit manifests and verifies all transitive content, including build provenance.
Conflicting logical identities, colliding public symbols, incompatible signatures
or profiles, and stale artifacts fail preparation.
Signature deserialization reuses the constructor's arity bound.
Raw dependencies already included in a shared image remain verified provenance rather than duplicate link inputs;
shared dependencies reachable through them still contribute loader requirements.
Such embedded raw interfaces are private to the image; consumers needing them directly select their own manifest.
The Unix interpreter receives exact shared-library paths and still loads symbols lazily.
Native linking adds exact artifacts and loader paths; manifest-free C imports retain ordinary system-library resolution.

Publication uses a staging directory and an immutable content-addressed bundle.
After assembly, runtime compilation, linking, and interface generation succeed,
the builder publishes convenience artifact/header/binding symlinks and finally the public `*.library.json` symlink.
That manifest is the authoritative commit point and resolves to a complete immutable bundle;
dependencies record immutable manifest paths rather than mutable convenience names.
A failed compiler or native tool leaves an earlier publication usable.
Content hashes identify changes, not the authenticity of an untrusted producer.
Preserve bundle-relative dependency locations when distributing artifacts.

[Library regressions](../../../../cli/tests/library.rs) exercise independent selection, erased private helpers,
rejected signatures, C scalar transport, repeated calls, nested collection through raw and shared dependencies,
entry guards, source-free consumers, and failed-build preservation.
Manifest unit tests cover compatibility, corruption, hidden dependencies, and identity collisions without loading code.

### Native Unit Artifacts

[Native Zydeco units](../../language/src/foreign-interfaces.md#native-zydeco-units) use the same checking,
static specialization, readiness, SPS, and native-frame machinery as other boundaries.
`CheckedUnit` retains one `UnitInitializer`, its exported classifier, optional Builtin plan, and residual return root.
Preparing that return together preserves sharing between the exported values.
The closed structural classifier is retained before erasure
as [`UnitValueType` and `UnitStackType`](../../../../lang/syntax/src/unit.rs), independently of SPS partial protocols.
Unsupported source types reject through the [unit classifier](../../../../lang/statics/src/unit.rs).
Generated source reconstructs the complete supported classifier without producer-local arena identities.

A native initializer receives the caller's ordinary `Ret Exports` stack
and enters a fresh native activation within the active runtime.
It has no extra closure-environment argument.
`NativeEntry::Unit` publishes this root under its generated linker symbol; the root's stack parity is unknown,
so native helper calls use the existing dynamic alignment path where necessary.
An imported initializer lowers to an ordinary thunk whose body tail-jumps to that symbol
after consuming its own closure environment.
It forwards the continuation stack unchanged, including retained-frame suspension IDs and residual data.
Calls through the returned exports use ordinary closure conversion and word entries.
The process supplies the runtime instance and its root-stack extent for all participating units.
Values retained across initialization or calls remain in traced source slots, stacks, or closure environments;
there is no untraced global export table or external handle registry.

[`UnitBuilder`](../../../../cli/src/unit.rs) emits one relocatable object with reachable private code,
a self-contained `.imports.zy`, and a versioned `*.unit.json` manifest.
The `word-initializer-v1` profile records the package, target, compiler executable hash,
runtime model identity, runtime source hash, initializer symbol, complete export type,
imported initializer signatures, artifact/interface hashes, and dependency manifests.
The symbol identity includes source dependencies and the prepared interface;
it is an implementation identity rather than a cross-version ABI promise.
No runtime archive is bundled with a native unit: the consuming executable supplies matching support once.

`--link-library` accepts both manifest kinds.
`LinkedUnits` validates native manifests and their transitive closure, including content hashes, profiles,
exact compatibility, symbol/type agreement, duplicate provider conflicts, and cycles.
Each unit may import only providers in its declared dependency closure.
Repeated references to one artifact share one linked object; this does not cache source initialization.
Raw native objects join the process object before the runtime archive is linked.
Validation fails before executing initialization or publishing a consumer executable.
Compiler-generated objects are trusted to implement their manifests; hashes do not verify machine code or authenticity.

Publication uses the shared staging and atomic symlink mechanism above.
The immutable native bundle contains its object, generated binding, and manifest;
the public `.unit.json` link is published last and selects the complete bundle.
Dependencies refer to immutable manifests with bundle-relative paths.
Library artifacts currently keep native-unit and C dependency boundaries separate.
The language reference owns initialization multiplicity, source-visible eligibility, and supported execution targets.

[Native unit regressions](../../../../cli/tests/unit.rs) remove producer source before dependent compilation,
pass captured boxed values through multiple objects, exercise both stack parities and collection
in callers/callees, and reject incompatible imports before publication.
[Manifest tests](../../../../cli/tests/unit_manifest.rs) cover malformed interfaces, corrupted contents,
compatibility mismatches, shared dependencies, and undeclared or conflicting providers without executing code.
