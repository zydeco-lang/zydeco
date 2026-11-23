pub mod clif;
mod clif_memory;

#[cfg(feature = "process-entry")]
pub use clif::{run_process, zydeco_clif_run_process};
