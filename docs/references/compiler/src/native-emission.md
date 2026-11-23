# C11. Native Cranelift Emission and Compilation Units

The [Cranelift backend](../../../../lang/cranelift/src/lib.rs) compiles `SpsLowProgram` directly
to relocatable ELF (`Linux`) and Mach-O (`macOS`) object bytes for both `x86_64` and `aarch64` architectures
without shelling out to external assemblers or maintaining handwritten instruction emitters.
[`Emitter::run`](../../../../lang/cranelift/src/lib.rs) returns a
[`NativeArtifact`](../../../../lang/cranelift/src/lib.rs) holding the object bytes together
with the sorted foreign imports, foreign libraries, and unit imports collected from that program.

```text
SpsLowProgram ─▶ ModulePlan (cases, strings, externs, LocalUnboxing, ScalarPlans)
                   ├─▶ CaseLiveness (per-case DefId GC root maps)
                   ├─▶ CaseEmitter + scalar / externs (Cranelift IR per case)
                   └─▶ cranelift-object ObjectModule ─▶ ELF / Mach-O .o bytes
```

[`ModulePlan`](../../../../lang/cranelift/src/plan.rs) performs deterministic pre-planning
before creating any Cranelift function:
- Collects the root computation, every `ValueExt::Block`, and every `LabelKind::Closed` label into an ordered list
  of [`Case`](../../../../lang/cranelift/src/plan.rs) functions and binds each label `DefId` to its case index.
- Collects UTF-8 string literals
  into aligned `.rodata` records (`[len: u64 LE, utf8_bytes..., 8-byte padding]`) shared globally across cases.
- Gathers deduplicated host roles (`BuiltinValueRole`), C foreign imports (`ForeignImport`), foreign libraries,
  and separately compiled unit imports (`UnitImport`).
- Runs shared [`LocalUnboxing`](../../../../lang/stackir/src/low/unbox.rs)
  and [`ScalarPlans`](../../../../lang/stackir/src/low/scalar.rs) for the selected `RepresentationStrategy`.

[`target::build_isa`](../../../../lang/cranelift/src/target.rs) configures Cranelift
with PIC relocation (`is_pic = true`), preserved frame pointers (`preserve_frame_pointers = true`),
disabled probestack, and speed optimization across all four target triples (`x86_64-unknown-linux-gnu`,
`x86_64-apple-darwin`, `aarch64-unknown-linux-gnu`, `aarch64-apple-darwin`).
[`BackendProgram::emit_native`](../../../../cli/src/compile.rs), `emit_c_export`,
and `emit_unit` share this single emission pipeline via `NativeEntry::{Process, C, Unit}`.

## Activation Lifetime

Each `Case` compiles as a standalone Cranelift function using the tail calling convention (`CallConv::Tail`)
with two pointer-sized parameters: `(vmctx: I64, sps_sp: I64) -> ()`.
`vmctx` points to [`ClifVmCtxHeader`](../../../../lang/machine/src/clif.rs) (at offset 0
of the active runtime instance), while `sps_sp` holds the current top-of-stack cursor
into the growable SPS stack buffer (`sps_storage`).

Every SPS stack frame (`Stack::Arg`, `Stack::Tag`, `ContinuationPackage { code, residual }`) pushes tagged words
onto `sps_storage` (growing automatically via `zydeco_clif_grow_sps` when headroom runs out)
and every entry parameter, `LetArg`, or `CoCase` tag pops from `sps_sp`.
Keeping the Zydeco control and argument stack in `sps_storage` separate from the native CPU stack lets `Jump`
and closed `JumpLabel` emit `return_call` / `return_call_indirect` with `CallConv::Tail`,
executing unbounded tail recursion across `x86_64` and `aarch64` with zero native frame growth.
Only non-closed local labels (`LabelKind::Local`) compile inside the enclosing case function
as direct Cranelift `jump` edges passing up to `MAX_PEELED_LABEL_ARGS` peeled words as block parameters.

Ordinary lexical bindings (`DefId`) live in Cranelift virtual SSA registers (`Variable`),
including expanded field slots when `LocalUnboxing` unboxes a `VCons` or `ClosurePackage` variable.
Continuation returns (`ContinuationPackage`) construct explicit continuation records
with their code pointer and residual stack,
while intra-case [`CaseLiveness`](../../../../lang/cranelift/src/liveness.rs) computes backward live `DefId` sets
at every `CompuId`.
Before any heap allocation (`emit_alloc`),
the cold slow-path helper (`zydeco_clif_alloc_slow`) writes all live `DefId` variables
and live temporary `Variable`s into a reusable per-function `StackSlot` (`roots_slot`), updates `vmctx.sps_sp`,
invokes the Cheney collector, and reloads every slot afterward so relocated pointers remain valid.

Process programs enter through [`CLIF_ENTRY_SYMBOL`](../../../../lang/machine/src/clif.rs)
(`zydeco_clif_entry_<fingerprint>`), generated from the deterministic FNV-1a hash
of all embedded `zydeco-machine` sources in [`lang/machine/build.rs`](../../../../lang/machine/build.rs).
[`Native-model tests`](../../../../lang/tests/tests/suite/native_model.rs)
and [`native_exec` tests](../../../../lang/cranelift/tests/native_exec.rs) verify tail recursion,
live closures across GC, returning host continuations, and rejection of mismatched model fingerprints at link time.

## Compilation-Unit Preparation and Artifacts

Independent emission fixes a contract between a producer and consumers that need not share its source.
The designs of compilation units, [FFI](../../language/src/foreign-interfaces.md),
and [package management](../../language/src/sources/packages.md#source-packages) are therefore coupled.
For compiled libraries, FFI determines which representations, control transfers, and ownership may cross the boundary;
unit selection checks that the chosen source implements a supported entry profile;
and package management names and resolves providers.

Changes to any of these contracts must be reviewed against the other two,
with affected rules and interfaces updated in the same design change.
An FFI extension can change unit eligibility, import/export adaptation, runtime entry
and teardown, generated interfaces, and artifact compatibility.
Conversely, changes to unit boundaries or package resolution can require new foreign contracts.

The checked [entry representations](../../../../lang/session/src/source/query.rs) describe a process program,
a collection of C exports, or a [native unit initializer](#native-unit-artifacts).
Source roles and legality rules belong
to [L14](../../language/src/foreign-interfaces.md#compiled-libraries-and-c-exports).
[`check::library`](../../../../lang/statics/src/check/library.rs) reuses the source judgment query,
Builtin domain validator, ordinary field resolver, directional foreign classifier,
static elaborator, and readiness checker.
`RootLowerer::run_with_builtin` supplies the optional validated provider through the same SPS entry path used
for executables; subsequent SPS, unboxing, scalar, and Cranelift lowering stages are shared.

Each `CExportEntry` causes [`emit_c_export_wrapper`](../../../../lang/cranelift/src/externs.rs)
to generate an exported `extern "C"` entrypoint function alongside a standalone zero-initialized 8-byte
guard global emitted via [`emit_guard_object`](../../../../lang/cranelift/src/module.rs).
The C wrapper calls `zydeco_clif_entry_begin(guard)` to acquire the reentrancy guard
and install a thread-local [`ClifRuntimeInstance`](native-runtime.md#runtime-instances),
encodes the incoming C arguments (allocating opaque boxes for `Int64`/`UInt64` and checking immediate bounds
for `Int`/`UInt`) onto `sps_storage` beneath a synthesized `zydeco_clif_return_cont` return frame,
tail-invokes case 0, decodes the `Ret` result into the C return register, and calls `zydeco_clif_entry_end(vmctx)`
to reset and park the instance.

[`LibraryBuilder`](../../../../cli/src/library.rs) compiles exports directly to `.o` files via `zydeco-cranelift`
without external assemblers:
- For `object`, `ld -r` combines export objects and the guard object; for `staticlib`, `ar` bundles them.
- For `sharedlib`, exports link against a cached PIC release build of `zydeco-runtime` (`panic=abort`),
  hiding all non-declared symbols via an ELF version script (`Linux`) or Mach-O export list (`macOS`).
- Raw objects and static archives ship a separate matching `zydeco-runtime` archive so the final executable
  or C link includes one runtime support image per target.

The versioned manifest records package identity, [`LibraryPlatform`](../../../../cli/src/library.rs) (`x86_64-linux`,
`x86_64-macos`, `aarch64-linux`, `aarch64-macos`), entry profile, compiler/model/runtime fingerprints,
artifact and interface hashes, exports, imports, and dependency manifests.
[`LinkedLibraries`](../../../../cli/src/library.rs) verifies transitive compatibility,
signatures, hashes, and cycles before linking or interpreter loading.
Publication uses a staging directory and atomic symlinks so a failed late tool leaves earlier publications intact.

### Native Unit Artifacts

[Native Zydeco units](../../language/src/foreign-interfaces.md#native-zydeco-units) share the same checking,
static specialization, readiness, SPS, and Cranelift emission pipeline.
`CheckedUnit` retains one `UnitInitializer`, its exported classifier, optional Builtin plan,
and residual return root, preserving sharing between exported values.
Closed structural classifiers are preserved in [`UnitValueType` and `UnitStackType`](../../../../lang/abi/src/unit.rs)
without producer-local arena identities.

`NativeEntry::Unit` exports the unit initializer using `CallConv::Tail` `(vmctx, sps_sp) -> ()`
under its content-addressed linker symbol.
An imported initializer lowers to an ordinary thunk that forwards `(vmctx, sps_sp)` via `return_call` to that symbol,
sharing the active `ClifRuntimeInstance` heap and `sps_storage` across linked objects without global export tables.
Values retained across initialization or calls remain in traced source slots, closure environments,
or the shared SPS control stack.

[`UnitBuilder`](../../../../cli/src/unit.rs) emits one relocatable `.o` via `BackendProgram::emit_unit`,
writes `.imports.zy` and `*.unit.json` (`word-initializer-v1`), and publishes them atomically.
Raw unit objects link before `libzydeco_runtime.a` when building a host executable via `--link-library`.

[Library regressions](../../../../cli/tests/library.rs)
and [native unit regressions](../../../../cli/tests/unit.rs) check C scalar transport, reentrancy guards,
collection across raw/shared/unit boundaries, cross-target ELF and Mach-O object emission (`x86_64` and `aarch64`),
source-free consumers, and manifest compatibility rejections.
