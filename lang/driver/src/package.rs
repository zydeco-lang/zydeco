//! The package notation of zydeco.

use crate::{compile::pack::*, interp::pack::*, prelude::*, *};
use derive_more::From;
use serde::{Deserialize, Serialize};
use std::{path::PathBuf, sync::Arc};
use zydeco_surface::{
    bitter::{DesugarOut, Desugarer},
    textual::{Lexer, ParseError, TopLevelParser},
};
use zydeco_utils::{
    arena::*,
    span::{FileInfo, LocationCtx},
};

#[derive(From)]
pub enum Package {
    Local(LocalPackage),
    Binary(PathBuf),
    Repl(String),
}

impl Package {
    pub fn deps(&self) -> Vec<Dependency> {
        match self {
            | Package::Local(pack) => pack
                .deps
                .iter()
                .map(|dep| match dep {
                    | Dependency::Local(path) => Dependency::Local(pack.path.join(path.clone())),
                })
                .collect(),
            | Package::Binary(_) => Vec::new(),
            | Package::Repl(_) => Vec::new(),
        }
    }
    pub fn name(&self) -> String {
        match self {
            | Package::Local(pack) => pack.name.clone(),
            | Package::Binary(path) => path.file_stem().unwrap().to_str().unwrap().to_string(),
            | Package::Repl(_) => "<repl>".to_string(),
        }
    }
    pub fn bins(&self) -> Vec<PathBuf> {
        match self {
            | Package::Local(pack) => {
                pack.bins.iter().cloned().map(|file| pack.path.join(file)).collect()
            }
            | Package::Binary(_) => vec![],
            | Package::Repl(_) => vec![],
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub enum Dependency {
    #[serde(rename = "local")]
    Local(PathBuf),
}

mod _impl {
    use super::*;
    use local::err::LocalError;

    impl Package {
        pub fn parse_package(&self, alloc: ArcGlobalAlloc) -> Result<PackageStew> {
            match self {
                | Package::Local(LocalPackage { path, name, srcs, deps: _, bins: _, std: _ }) => {
                    let stew = LocalPackage::parse_package(
                        alloc.clone(),
                        name.as_str(),
                        path,
                        srcs.iter(),
                    )?;
                    Ok(stew)
                }
                | Package::Binary(path) => {
                    let source = std::fs::read_to_string(path.as_path())?;
                    Package::parse_source(alloc.clone(), source, path.clone())
                }
                | Package::Repl(source) => {
                    let path = PathBuf::new();
                    Package::parse_source(alloc.clone(), source.clone(), path)
                }
            }
        }
        pub fn parse_source(
            alloc: ArcGlobalAlloc, source: String, path: PathBuf,
        ) -> Result<PackageStew> {
            let mut parser = t::Parser::new(alloc.alloc());

            let top = TopLevelParser::new()
                .parse(&source, &LocationCtx::Plain, &mut parser, Lexer::new(&source))
                .map_err(|error| {
                    LocalError::ParseError(
                        ParseError {
                            error,
                            file_info: &FileInfo::new(&source, Arc::new(path.clone())),
                        }
                        .to_string(),
                    )
                })?;

            let t::Parser { spans, arena: textual } = parser;
            let bitter = b::Arena::new_arc(alloc.clone());
            let desugarer = Desugarer { spans, textual, bitter, prim: b::PrimTerms::default() };
            let DesugarOut { spans, arena, prim: prim_term, top } =
                desugarer.run(top).map_err(|err| LocalError::DesugarError(err.to_string()))?;

            Ok(PackageStew {
                sources: [(path, source)].into_iter().collect(),
                spans,
                arena,
                prim_term,
                top,
            })
        }
        pub fn compile_package(
            alloc: ArcGlobalAlloc, name: &str, pack: PackageStew,
        ) -> Result<PackageChecked> {
            // resolving
            let pack = pack.resolve(alloc.alloc())?.self_check(name);
            // tycking
            let checked = pack.tyck(alloc, name)?;
            Ok(checked)
        }
        pub fn link_dynamics(name: &str, pack: PackageChecked) -> Result<d::DynamicsArena> {
            // compiling
            let dynamics = pack.dynamics(name)?;
            Ok(dynamics)
        }
        pub fn run_dynamics(dynamics: d::DynamicsArena) -> Result<()> {
            let () = PackageRuntime { dynamics }.run()?;
            Ok(())
        }
        pub fn test_dynamics(dynamics: d::DynamicsArena, name: &str, aloud: bool) -> Result<()> {
            let () = PackageRuntime { dynamics }.test(name, aloud)?;
            Ok(())
        }
    }
}
