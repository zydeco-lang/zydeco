use crate::{EmitError, NativeTarget};
use cranelift_codegen::isa::{self, OwnedTargetIsa};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_module::default_libcall_names;
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::str::FromStr;
use target_lexicon::Triple;

/// Configure Cranelift's target ISA for relocatable PIC native objects.
pub(crate) fn build_isa(target: NativeTarget) -> Result<OwnedTargetIsa, EmitError> {
    let triple =
        Triple::from_str(target.triple()).map_err(|err| EmitError::Target(err.to_string()))?;
    let mut flags = settings::builder();
    flags.set("is_pic", "true").map_err(|err| EmitError::Target(err.to_string()))?;
    flags
        .set("preserve_frame_pointers", "true")
        .map_err(|err| EmitError::Target(err.to_string()))?;
    flags.set("enable_probestack", "false").map_err(|err| EmitError::Target(err.to_string()))?;
    flags.set("opt_level", "speed").map_err(|err| EmitError::Target(err.to_string()))?;
    flags
        .set("enable_verifier", if cfg!(debug_assertions) { "true" } else { "false" })
        .map_err(|err| EmitError::Target(err.to_string()))?;
    let isa_builder = isa::lookup(triple).map_err(|err| EmitError::Target(err.to_string()))?;
    isa_builder
        .finish(settings::Flags::new(flags))
        .map_err(|err| EmitError::Target(err.to_string()))
}

pub(crate) fn create_object_module(
    target: NativeTarget, name: &str,
) -> Result<ObjectModule, EmitError> {
    let isa = build_isa(target)?;
    let builder = ObjectBuilder::new(isa, name, default_libcall_names())?;
    Ok(ObjectModule::new(builder))
}
