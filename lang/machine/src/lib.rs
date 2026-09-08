//! Runtime contracts without dependencies on compiler syntax or backend instructions.
//!
//! [`word`] owns the common scalar representation. [`native`] describes the current
//! AMD64 runtime; future runtime schemes can have their own state and protocols.
//! The `runtime` feature enables target-side bindings. The `bundle` feature embeds
//! these sources so a compiler can supply its exact model to a standalone stub build.

#![no_std]

pub mod closure;
pub mod native;
pub mod word;

#[cfg(feature = "bundle")]
pub mod bundle {
    /// One source file in a self-contained copy of this crate.
    pub struct SourceFile {
        pub path: &'static str,
        pub contents: &'static str,
    }

    include!(concat!(env!("OUT_DIR"), "/bundle.rs"));
}
