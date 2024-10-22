//! The package notation of zydeco.

use crate::{compile::pack::*, interp::pack::*, prelude::*, *};
use derive_more::From;
use rayon::prelude::*;
use sculptor::{FileIO, SerdeStr, ShaSnap};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, io, path::PathBuf, sync::Arc};
use zydeco_surface::{
    bitter::{DesugarOut, Desugarer},
    textual::{HashLexer, Lexer, ParseError, TopLevelParser},
};
use zydeco_utils::{
    arena::*,
    span::{FileInfo, LocationCtx},
};

#[derive(From)]
pub enum Package {
    Local(LocalPackage),
    Repl(String),
}

impl Package {
    pub fn deps(&self) -> &[Dependency] {
        match self {
            | Package::Local(pack) => &pack.deps,
            | Package::Repl(_) => &[],
        }
    }
}

mod _impl {
    use super::*;
    impl Package {
        fn compile_package(
            alloc: ArcGlobalAlloc, name: &str, pack: PackageStew,
        ) -> Result<PackageChecked> {
            // resolving
            let pack = pack.resolve(alloc.alloc())?.self_check(name);
            // tycking
            let checked = pack.tyck(alloc, name)?;
            Ok(checked)
        }
        fn link_dynamics(name: &str, pack: PackageChecked) -> Result<d::DynamicsArena> {
            // compiling
            let dynamics = pack.dynamics(name)?;
            Ok(dynamics)
        }
        fn run_dynamics(dynamics: d::DynamicsArena) -> Result<()> {
            let () = PackageRuntime { dynamics }.run()?;
            Ok(())
        }
        fn test_dynamics(dynamics: d::DynamicsArena, name: &str) -> Result<()> {
            let () = PackageRuntime { dynamics }.test(name)?;
            Ok(())
        }
    }
}
