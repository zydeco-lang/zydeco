//! Root-source compiler and execution driver for Zydeco.

pub(crate) mod backend;
pub mod conf;
pub mod diagnostics;
pub mod err;
pub mod source;
pub mod verbosity;

pub mod check {
    pub mod err;
}

pub mod zir {
    pub(crate) mod pipeline;
    pub(crate) use pipeline::StackPipeline;
}

pub mod zasm {
    pub mod err;
    pub(crate) mod pipeline;
    pub(crate) use pipeline::AssemblyPipeline;
}

pub mod amd64 {
    pub mod err;
    pub mod pack;
    pub(crate) mod pipeline;
    pub use pack::PackageAmd64;
    pub(crate) use pipeline::Amd64Pipeline;
}

pub mod llvm {
    pub mod err;
    pub mod pack;
    pub(crate) mod pipeline;
    pub use pack::PackageLlvm;
    pub(crate) use pipeline::LlvmPipeline;
}

/// Namespaces for the successive Zydeco compiler representations.
pub mod prelude {
    pub use zydeco_surface::textual::syntax as t;

    pub use zydeco_surface::bitter::syntax as b;

    pub use zydeco_surface::scoped::syntax as sc;

    pub use zydeco_statics::tyck::syntax as ss;

    pub use zydeco_dynamics::syntax as d;

    pub use zydeco_stackir::sps::syntax as sk;

    pub use zydeco_stackir::snorm::syntax as sn;

    pub use zydeco_assembly::syntax as sa;
}

pub use conf::{BuildConf, PipelineConf};
pub use err::{BuildError, Result};
pub use source::{SourceDriver, SourceGraph, SourceId, SourceImportId, SourceLoadError};
pub use verbosity::Verbosity;
pub use zydeco_dynamics::ProgKont;
