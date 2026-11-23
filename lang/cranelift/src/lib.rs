//! Cranelift native backend emitting relocatable ELF and Mach-O objects for `aarch64` and `x86_64`.

mod externs;
mod liveness;
mod lower;
mod module;
mod plan;
mod scalar;
mod target;

use std::fmt;
use thiserror::Error;
use zydeco_machine::word::WordError;
use zydeco_stackir::{
    SpsLowProgram, low::representation::RepresentationStrategy, low::syntax::DefId,
};
use zydeco_syntax::{ForeignImport, ForeignLibraryName, ForeignSymbolName, UnitImport};

pub use module::emit_guard_object;

/// Target CPU architecture supported by `zydeco-cranelift`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NativeArch {
    X86_64,
    Aarch64,
}

impl NativeArch {
    /// Detect the compiler host architecture.
    pub fn host() -> Result<Self, &'static str> {
        match std::env::consts::ARCH {
            | "x86_64" => Ok(Self::X86_64),
            | "aarch64" => Ok(Self::Aarch64),
            | other => Err(other),
        }
    }
}

impl fmt::Display for NativeArch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            | Self::X86_64 => "x86-64",
            | Self::Aarch64 => "aarch64",
        })
    }
}

/// Target operating system / object container format supported by `zydeco-cranelift`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NativeOs {
    Linux,
    Macos,
}

impl NativeOs {
    /// Detect the compiler host operating system.
    pub fn host() -> Result<Self, &'static str> {
        match std::env::consts::OS {
            | "linux" => Ok(Self::Linux),
            | "macos" => Ok(Self::Macos),
            | other => Err(other),
        }
    }
}

impl fmt::Display for NativeOs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            | Self::Linux => "linux",
            | Self::Macos => "macos",
        })
    }
}

/// Native machine target pair supported by `zydeco-cranelift`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NativeTarget {
    pub arch: NativeArch,
    pub os: NativeOs,
}

impl NativeTarget {
    pub const fn new(arch: NativeArch, os: NativeOs) -> Self {
        Self { arch, os }
    }

    pub fn host() -> Result<Self, EmitError> {
        let arch = NativeArch::host().map_err(EmitError::UnsupportedHostArch)?;
        let os = NativeOs::host().map_err(EmitError::UnsupportedHostOs)?;
        Ok(Self { arch, os })
    }

    pub fn triple(self) -> &'static str {
        match (self.arch, self.os) {
            | (NativeArch::X86_64, NativeOs::Linux) => "x86_64-unknown-linux-gnu",
            | (NativeArch::X86_64, NativeOs::Macos) => "x86_64-apple-darwin",
            | (NativeArch::Aarch64, NativeOs::Linux) => "aarch64-unknown-linux-gnu",
            | (NativeArch::Aarch64, NativeOs::Macos) => "aarch64-apple-darwin",
        }
    }
}

/// Checked C ABI export boundary and shared reentrancy guard symbol.
#[derive(Clone, Debug)]
pub struct CExportEntry {
    pub symbol: ForeignSymbolName,
    pub signature: zydeco_syntax::ForeignSignature,
    pub guard: ForeignSymbolName,
}

/// Entry-point profile emitted for one compiled `SpsLowProgram`.
#[derive(Clone, Debug, Default)]
pub enum NativeEntry {
    /// Executable program entered via `zydeco_clif_entry_<fingerprint>`.
    #[default]
    Process,
    /// Standalone C ABI function managing its own isolated runtime instance.
    C(CExportEntry),
    /// Separately compiled Zydeco unit exported via `CallConv::Tail`.
    Unit(ForeignSymbolName),
}

/// Relocatable native object bytes and resolved external link requirements.
#[derive(Debug)]
pub struct NativeArtifact {
    pub object: Vec<u8>,
    pub foreign_libraries: Vec<ForeignLibraryName>,
    pub foreign_imports: Vec<ForeignImport>,
    pub unit_imports: Vec<UnitImport>,
}

/// Errors emitted while planning or encoding native machine code via Cranelift.
#[derive(Debug, Error)]
pub enum EmitError {
    #[error("unsupported host architecture `{0}`")]
    UnsupportedHostArch(&'static str),
    #[error("unsupported host operating system `{0}`")]
    UnsupportedHostOs(&'static str),
    #[error("SPS Cranelift backend cannot find code label {0:?}")]
    MissingLabel(DefId),
    #[error("SPS Cranelift backend cannot find variable binding {0:?}")]
    MissingVariable(DefId),
    #[error("invalid SPS coproduct match: constructor arms cannot be mixed with a catch-all arm")]
    InvalidCoprodMatch,
    #[error("unsupported local unboxing shape for pattern {0:?}")]
    InvalidUnboxingPattern(zydeco_stackir::low::syntax::VPatId),
    #[error(transparent)]
    Word(#[from] WordError),
    #[error("Cranelift target setup failed: {0}")]
    Target(String),
    #[error("Cranelift module error: {0}")]
    Module(#[from] Box<cranelift_module::ModuleError>),
    #[error("Cranelift codegen error: {0}")]
    Codegen(#[from] Box<cranelift_codegen::CodegenError>),
}

impl From<cranelift_module::ModuleError> for EmitError {
    fn from(error: cranelift_module::ModuleError) -> Self {
        Self::Module(Box::new(error))
    }
}

impl From<cranelift_codegen::CodegenError> for EmitError {
    fn from(error: cranelift_codegen::CodegenError) -> Self {
        Self::Codegen(Box::new(error))
    }
}

/// Direct `SpsLowProgram`-to-native `.o` emitter backed by Cranelift.
pub struct Emitter<'a> {
    program: &'a SpsLowProgram,
    target: NativeTarget,
    entry: NativeEntry,
    representation: RepresentationStrategy,
}

impl<'a> Emitter<'a> {
    pub fn new(program: &'a SpsLowProgram, target: NativeTarget) -> Self {
        Self {
            program,
            target,
            entry: NativeEntry::Process,
            representation: RepresentationStrategy::default(),
        }
    }

    pub fn with_entry(mut self, entry: NativeEntry) -> Self {
        self.entry = entry;
        self
    }

    pub fn with_representation(mut self, strategy: RepresentationStrategy) -> Self {
        self.representation = strategy;
        self
    }

    pub fn run(self) -> Result<NativeArtifact, EmitError> {
        let plan = plan::ModulePlan::new(self.program, self.representation)?;
        module::ModuleEmitter::new(self.program, self.target, &self.entry, plan)?.emit()
    }
}
