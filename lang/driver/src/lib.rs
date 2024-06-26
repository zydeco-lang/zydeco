//! A minimal build system for the zydeco language.

pub mod conf;
pub mod err;

/// the topmost compilation pipeline led by a package configuration
pub mod local {
    pub mod pack;
    pub use pack::*;
    pub mod err;
    pub use err::*;

    #[cfg(test)]
    mod tests;
}

pub use err::*;
pub use local::pack::{Dependency, LocalPackage};

use derive_more::From;
use std::{collections::HashMap, path::PathBuf};
use zydeco_utils::{
    arena::{new_key_type, ArenaDense},
    deps::DepGraph,
};

new_key_type! {
    pub struct PackId<()>;
}

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

pub struct BuildSystem {
    /// all the packages in the build system
    pub packages: ArenaDense<PackId, Package, ()>,
    /// a map from the canonicalized path of package file to the package id
    pub seen: HashMap<PathBuf, PackId>,
    pub depends_on: DepGraph<PackId>,
}

impl Default for BuildSystem {
    fn default() -> Self {
        Self::new()
    }
}

impl BuildSystem {
    pub fn new() -> Self {
        Self { packages: ArenaDense::default(), seen: HashMap::new(), depends_on: DepGraph::new() }
    }
    pub fn add_local_package(&mut self, path: impl Into<PathBuf>) -> Result<()> {
        // add all dependent packages to the build system
        let pack = LocalPackage::new(path)?;
        let mut stack = vec![self.add(pack)?];
        while let Some(id) = stack.pop() {
            let deps = self.probe(id)?;
            stack.extend(deps);
        }
        Ok(())
    }
    pub fn add_local_file(&mut self, _path: impl Into<PathBuf>) -> Result<()> {
        todo!()
    }
}

impl BuildSystem {
    /// add a package to the build system
    pub fn add(&mut self, pack: LocalPackage) -> Result<PackId> {
        let path = pack.path.clone().canonicalize()?;
        let pack_id = self.packages.alloc(pack.into());
        self.seen.insert(path, pack_id);
        Ok(pack_id)
    }
    /// probe unseen dependencies of a package within one step and add them
    pub fn probe(&mut self, id: PackId) -> Result<Vec<PackId>> {
        let pack = &self.packages[&id];
        let mut deps_old = Vec::new();
        let mut deps_new = Vec::new();
        for dep in pack.deps() {
            match dep {
                | Dependency::Local(path) => {
                    let path = path.canonicalize()?;
                    if let Some(id) = self.seen.get(&path) {
                        deps_old.push(*id);
                    } else {
                        deps_new.push(path);
                    }
                }
            }
        }
        let deps_new = deps_new
            .into_iter()
            .map(|path| {
                let pack = LocalPackage::new(path)?;
                self.add(pack)
            })
            .collect::<Result<Vec<_>>>()?;
        self.depends_on.add(id, deps_new.iter().cloned().chain(deps_old));
        Ok(deps_new)
    }
}
