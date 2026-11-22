//! Shared runtime host services for Zydeco's interpreter, native runtime, and WASI targets.

#![no_std]

extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

#[cfg(target_pointer_width = "64")]
pub mod gc;
#[cfg(feature = "std")]
pub mod io;
#[cfg(feature = "std")]
pub mod random;
pub mod string;
#[cfg(all(feature = "wasm-runtime", target_arch = "wasm32"))]
pub mod wasm;

pub use zydeco_abi::{HostIoErrorKind, RuntimeFailure};

#[cfg(feature = "bundle")]
pub mod bundle {
    /// One source file in a self-contained copy of this crate.
    pub struct SourceFile {
        pub path: &'static str,
        pub contents: &'static str,
    }

    include!(concat!(env!("OUT_DIR"), "/bundle.rs"));
}
