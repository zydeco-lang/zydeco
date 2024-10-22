//! A minimal build system for the zydeco language.

pub mod conf;
pub mod err;
pub mod package;

/// the topmost compilation pipeline led by a package configuration
pub mod local {
    pub mod pack;
    pub mod err;
}

pub mod check {
    pub mod pack;
    pub mod err;
}

pub mod interp {
    pub mod pack;
    pub mod err;
}

pub mod prelude {
    pub use zydeco_dynamics::syntax as d;
    pub use zydeco_statics::syntax as ss;
    pub use zydeco_surface::bitter::syntax as b;
    pub use zydeco_surface::scoped::syntax as sc;
    pub use zydeco_surface::textual::syntax as t;
}

#[cfg(test)]
mod tests;

pub use conf::Conf;
pub use err::{BuildError, Result};
pub use local::pack::LocalPackage;
pub use package::{Dependency, Package};
pub use zydeco_utils::arena::ArcGlobalAlloc;

use crate::check::pack::PackageStew;
use prelude::*;
use sculptor::{FileIO, ProjectInfo};
use std::{collections::HashMap, path::PathBuf};
use zydeco_utils::{
    arena::{new_key_type, ArenaDense},
    deps::DepGraph,
    scc::Kosaraju,
};

new_key_type! {
    pub struct PackId<()>;
}

pub struct BuildSystem {
    /// configuration
    pub conf: Conf,
    /// all the packages in the build system
    pub packages: ArenaDense<PackId, Package>,
    /// a map from the canonicalized path of package file to the package id
    pub seen: HashMap<PathBuf, PackId>,
    /// a map from the named packages to their package id, typically for binaries
    pub marked: HashMap<String, PackId>,
    /// dependency graph, key depends on value
    pub depends_on: DepGraph<PackId>,
}

impl Default for BuildSystem {
    fn default() -> Self {
        Self::new()
    }
}

/// public interface
impl BuildSystem {
    pub fn new() -> Self {
        let path = Conf::config_dir().join("zydeco.toml");
        let file_conf = FileIO::new(path.clone());
        let conf = file_conf.load().unwrap_or_else(|_| {
            log::warn!("Using default configuration; suppose to find one at `{}`.", path.display());
            let conf: Conf = Default::default();
            file_conf.save(&conf).unwrap();
            conf
        });
        let mut build_sys = Self {
            conf,
            packages: ArenaDense::default(),
            seen: HashMap::new(),
            marked: HashMap::new(),
            depends_on: DepGraph::new(),
        };
        for path in build_sys.conf.default_packages.clone() {
            build_sys.add_local_package(path.clone()).unwrap();
        }
        build_sys
    }
    pub fn add_local_package(&mut self, path: impl Into<PathBuf>) -> Result<PackId> {
        let pack = LocalPackage::new(path)?;
        let pack_id = self.add(pack)?;
        // add all dependent packages to the build system
        let mut stack = vec![pack_id];
        while let Some(id) = stack.pop() {
            let deps = self.probe(id)?;
            stack.extend(deps);
        }
        Ok(pack_id)
    }
    pub fn add_orphan_file(&mut self, path: impl Into<PathBuf>) -> Result<PackId> {
        let pack = self.packages.alloc(Package::Binary(path.into()));
        // assuming that all packages added before are dependencies
        self.depends_on.add(pack, (&self.packages).into_iter().map(|(id, _)| id));
        Ok(pack)
    }
    pub fn add_binary_in_package(&mut self, pack: PackId) -> Result<HashMap<String, PackId>> {
        let package = &self.packages[&pack];
        let mut binaries = HashMap::new();
        for path in package.bins() {
            let binpack = self.packages.alloc(Package::Binary(path.clone()));
            self.depends_on.add(binpack, std::iter::once(pack));
            let name = self.mark(binpack)?;
            binaries.insert(name, binpack);
        }
        Ok(binaries)
    }
    pub fn mark(&mut self, binpack: PackId) -> Result<String> {
        let name = self.packages[&binpack].name();
        if !self.marked.contains_key(&name) {
            Err(BuildError::DuplicateMark(name.clone()))?
        } else {
            self.marked.insert(name.clone(), binpack);
        }
        Ok(name)
    }
    pub fn pick_marked(&self, name: Option<String>) -> Result<PackId> {
        if self.marked.len() == 1 {
            Ok(self.marked.iter().next().unwrap().1.clone())
        } else {
            match name {
                | Some(name) => {
                    if let Some(pack) = self.marked.get(&name) {
                        return Ok(*pack);
                    }
                }
                | None => {}
            }
            Err(BuildError::AmbiguousMark(self.marked.keys().cloned().collect()))?
        }
    }
    pub fn run_pack(&self, pack: PackId, dry: bool, verbose: bool) -> Result<()> {
        let dynamics = self.dynamics_pack(pack, verbose)?;
        if dry {
            return Ok(());
        }
        Package::run_dynamics(dynamics)
    }
    pub fn test_pack(&self, pack: PackId, dry: bool) -> Result<()> {
        let name = self.packages[&pack].name();
        let dynamics = self.dynamics_pack(pack, false)?;
        if dry {
            return Ok(());
        }
        Package::test_dynamics(dynamics, name.as_str(), false)
    }
    pub fn dynamics_pack(
        &self, pack: PackId, _verbose: bool,
    ) -> Result<d::DynamicsArena> {
        let alloc = ArcGlobalAlloc::new();
        let mut scc = Kosaraju::new(&self.depends_on).run();
        scc.keep_only([pack]);
        let mut stew = None;
        loop {
            let active = scc.top();
            if active.is_empty() {
                break;
            }
            for group in active {
                for pack in group {
                    let stew_ = self.packages[&pack].parse_package(alloc.clone())?;
                    if let Some(s) = stew {
                        stew = Some(s + stew_);
                    } else {
                        stew = Some(stew_);
                    }
                    scc.release([pack]);
                }
            }
        }
        let name = self.packages[&pack].name();
        let stew = stew.unwrap_or_else(|| PackageStew::new(alloc.clone()));
        let checked = Package::check_package(alloc.clone(), name.as_str(), stew)?;
        let dynamics = Package::link_dynamics(name.as_str(), checked)?;
        Ok(dynamics)
    }
}

impl BuildSystem {
    /// add a package to the build system
    fn add(&mut self, pack: LocalPackage) -> Result<PackId> {
        let path = pack.path.clone().canonicalize()?;
        let pack_id = self.packages.alloc(pack.into());
        self.seen.insert(path, pack_id);
        Ok(pack_id)
    }
    /// probe unseen dependencies of a package within one step and add them
    fn probe(&mut self, id: PackId) -> Result<Vec<PackId>> {
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
