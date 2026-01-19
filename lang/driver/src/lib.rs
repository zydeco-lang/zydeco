//! A minimal build system for the zydeco language.

pub mod conf;
pub mod diagnostics;
pub mod err;
pub mod package;

/// the topmost compilation pipeline led by a package configuration
pub mod local {
    pub mod pack;
    pub mod err;
    pub use pack::LocalPackage;
}

pub mod check {
    pub mod pack;
    pub mod err;
    pub use pack::{PackageChecked, PackageStew};
}

pub mod interp {
    pub mod pack;
    pub mod err;
}

pub mod zir {
    pub mod pack;
    pub use pack::PackageStack;
}

pub mod zasm {
    pub mod pack;
    pub mod err;
    pub use pack::PackageAssembly;
}

pub mod x86 {
    pub mod pack;
    pub mod err;
    pub use pack::PackageX86;
}

/// Namespaces for the Zydeco language ecosystem.
/// Newlines are added to prevent reordering of the imports.
pub mod prelude {
    pub use zydeco_surface::textual::syntax as t;

    pub use zydeco_surface::bitter::syntax as b;

    pub use zydeco_surface::scoped::syntax as sc;

    pub use zydeco_statics::tyck::syntax as ss;

    pub use zydeco_dynamics::syntax as d;

    pub use zydeco_stackir::syntax as sk;

    pub use zydeco_assembly::syntax as sa;
}

pub use conf::{BuildConf, Conf};
pub use err::{BuildError, Result};
pub use local::LocalPackage;
pub use package::{Dependency, Package};
pub use zydeco_dynamics::ProgKont;

use crate::{
    check::{PackageChecked, PackageStew},
    x86::PackageX86,
    zasm::pack::PackageAssembly,
    zir::pack::PackageStack,
};
use sculptor::{FileIO, ProjectInfo};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};
use zydeco_x86::TargetFormat;
use zydeco_utils::prelude::{
    ArcGlobalAlloc, ArenaAccess, ArenaAssoc, ArenaDense, CompilerPass, DepGraph, Kosaraju,
};

zydeco_utils::new_key_type! {
    pub struct PackId<()>;
}

pub struct Driver {
    pub build_sys: BuildSystem,
}

impl Driver {
    pub fn setup(paths: Vec<PathBuf>) -> Result<Self> {
        let mut build_sys = BuildSystem::new();
        let mut packs = Vec::new();
        let mut files = Vec::new();

        for path in paths {
            // for dir, try finding "proj.toml" under it
            if path.is_dir() {
                let proj = path.join("proj.toml");
                if proj.exists() {
                    let pack = build_sys.add_local_package(proj)?;
                    packs.push(pack);
                    continue;
                }
                // fallback to adding the dir itself
            }
            match path.extension() {
                | Some(ext) if ext == "toml" => {
                    // package
                    let pack = build_sys.add_local_package(path)?;
                    packs.push(pack);
                }
                | Some(_) | None => {
                    // single file
                    files.push(path);
                }
            }
        }

        if files.is_empty() {
            for pack in packs {
                build_sys.add_binary_in_package(pack)?;
            }
        } else {
            for file in files.iter() {
                let pack = build_sys.add_orphan_file(file)?;
                build_sys.mark(pack)?;
            }
        }

        Ok(Self { build_sys })
    }

    pub fn pack_for_project_path(&self, path: impl AsRef<Path>) -> Option<PackId> {
        let path = path.as_ref();
        let key = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        self.build_sys.seen.get(&key).copied()
    }

    pub fn packages_with_deps(&self, root: PackId) -> Vec<PackId> {
        let mut stack = vec![root];
        let mut seen = std::collections::HashSet::new();
        let mut order = Vec::new();
        while let Some(pack) = stack.pop() {
            if !seen.insert(pack) {
                continue;
            }
            order.push(pack);
            for dep in self.build_sys.depends_on.query(&pack) {
                stack.push(dep);
            }
        }
        order
    }

    pub fn find_project_toml(path: impl AsRef<Path>) -> Option<PathBuf> {
        // TODO: remove this workaround after stablizing how Zydeco projects are managed
        let mut dir = {
            let path = path.as_ref();
            if path.is_dir() { path.to_path_buf() } else { path.parent()?.to_path_buf() }
        };
        loop {
            let candidate = dir.join("proj.toml");
            if candidate.is_file() {
                return Some(candidate);
            }
            if !dir.pop() {
                break;
            }
        }
        None
    }
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
    /// per-package build config
    pub build_confs: ArenaAssoc<PackId, BuildConf>,
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
        log::info!("Loading configuration from `{}`.", path.display());
        let file_conf = FileIO::new(path.clone());
        let conf = file_conf.load().unwrap_or_else(|_| {
            log::warn!("Using default configuration; saving it to `{}`.", path.display());
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
            build_confs: ArenaAssoc::default(),
        };
        for path in build_sys.conf.default_packages.clone() {
            build_sys.add_local_package(path.clone()).unwrap();
        }
        build_sys
    }
    pub fn add_local_package(&mut self, path: impl Into<PathBuf>) -> Result<PackId> {
        let local = LocalPackage::new(path)?;
        let pack = self.__add_path_indexed_package(local)?;
        // add all dependent packages to the build system
        let mut stack = vec![pack];
        while let Some(id) = stack.pop() {
            let deps = self.__probe_path_indexed_package(id)?;
            stack.extend(deps);
        }
        Ok(pack)
    }
    pub fn add_orphan_file(&mut self, path: impl Into<PathBuf>) -> Result<PackId> {
        let path = path.into();
        log::info!("Adding orphan file {}", path.display());

        let pack = self.packages.alloc(Package::Binary(path));
        // assuming that all packages added before are dependencies
        self.depends_on
            .add(pack, self.packages.iter().filter_map(|(id, _)| (id != pack).then_some(id)));
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
        if self.marked.contains_key(&name) {
            Err(BuildError::DuplicateMark(name.clone()))?
        } else {
            self.marked.insert(name.clone(), binpack);
        }
        Ok(name)
    }
    pub fn pick_marked(&self, name: Option<String>) -> Result<PackId> {
        match name {
            | Some(name) => {
                if let Some(pack) = self.marked.get(&name) {
                    Ok(*pack)
                } else {
                    Err(BuildError::NoSuitableMark(name, self.marked.keys().cloned().collect()))?
                }
            }
            | None => {
                if self.marked.len() == 1 {
                    Ok(*self.marked.iter().next().unwrap().1)
                } else {
                    Err(BuildError::AmbiguousMark(self.marked.keys().cloned().collect()))
                }
            }
        }
    }
    pub fn run_pack(
        &self, pack: PackId, args: &[String], dry: bool, verbose: bool,
    ) -> Result<ProgKont> {
        let alloc = ArcGlobalAlloc::new();
        let checked = self.__tyck_pack(pack, alloc, verbose)?;
        let name = self.packages[&pack].name();
        let runtime = Package::link_interp(name.as_str(), checked)?;
        if dry {
            return Ok(ProgKont::Dry);
        }
        Ok(Package::run_interp(runtime, args))
    }
    pub fn test_pack(&self, pack: PackId, dry: bool) -> Result<()> {
        let name = self.packages[&pack].name();
        let alloc = ArcGlobalAlloc::new();
        let checked = self.__tyck_pack(pack, alloc, false)?;
        let runtime = Package::link_interp(name.as_str(), checked)?;
        if dry {
            return Ok(());
        }
        Package::test_interp(runtime, name.as_str(), false)
    }
    pub fn codegen_zir_pack(&self, pack: PackId) -> Result<()> {
        let PackageStack { stack, scoped, statics, .. } =
            self.__compile_zir_pack(pack, ArcGlobalAlloc::new(), false)?;
        // pretty print the ZIR
        use zydeco_stackir::fmt::*;
        let fmt = Formatter::new(&stack, &scoped, &statics);
        let doc = stack.pretty(&fmt);
        let mut buf = String::new();
        doc.render_fmt(100, &mut buf).unwrap();
        println!("{}", buf);
        Ok(())
    }
    pub fn codegen_zasm_pack(&self, pack: PackId, execute: bool, verbose: bool) -> Result<()> {
        let PackageAssembly { assembly, .. } =
            self.__compile_zasm_pack(pack, ArcGlobalAlloc::new(), verbose)?;
        if execute {
            let interpreter = zydeco_assembly::interp::Interpreter::new(assembly);
            let output = interpreter.run()?;
            let msg = match output {
                | zydeco_assembly::interp::Output::Exit => "Program exited with code 0".to_string(),
                | zydeco_assembly::interp::Output::Panic => "Program panicked".to_string(),
            };
            println!("{}", msg);
        } else {
            // pretty print the ZASM
            use zydeco_assembly::fmt::*;
            let fmt = Formatter::new(&assembly);
            let doc = assembly.pretty(&fmt);
            let mut buf = String::new();
            doc.render_fmt(100, &mut buf).unwrap();
            println!("{}", buf);
        }
        Ok(())
    }
    pub fn codegen_x86_pack(&self, pack: PackId, verbose: bool) -> Result<PackageX86> {
        let build_conf = self
            .build_confs
            .get(&pack)
            .cloned()
            .ok_or_else(|| BuildError::MissingBuildConfig(self.packages[&pack].name()))?;
        let target_format = match build_conf.target_os.as_str() {
            | "linux" => TargetFormat::Elf,
            | "macos" | "darwin" => TargetFormat::MachO,
            | other => return Err(BuildError::UnsupportedTargetOs(other.to_string())),
        };
        let PackageAssembly { spans, scoped, statics, stack, assembly } =
            self.__compile_zasm_pack(pack, ArcGlobalAlloc::new(), verbose)?;
        let assembly = zydeco_x86::Emitter::new(
            &spans,
            &scoped,
            &statics,
            &stack,
            &assembly,
            target_format,
        )
            .run()?
            .to_string();
        if verbose {
            log::trace!("x86 assembly:\n{}", &assembly);
        }
        Ok(x86::PackageX86 { name: self.packages[&pack].name(), assembly, build_conf })
    }
    pub fn test_x86_pack(&self, pack: PackId, verbose: bool) -> Result<()> {
        let x86 = self.codegen_x86_pack(pack, verbose)?;
        let executable = x86.link()?;
        let status = executable.run()?;
        if status.success() { Ok(()) } else { Err(BuildError::X86RunError(status)) }
    }
}

impl BuildSystem {
    /// add a package to the build system
    fn __add_path_indexed_package(&mut self, pack: LocalPackage) -> Result<PackId> {
        let path = pack.path.clone().canonicalize()?;
        log::info!("Adding local package: {}", path.display());
        if let Some(id) = self.seen.get(&path) {
            log::warn!("Package already added: {}", path.display());
            return Ok(*id);
        }
        let pack_id = self.packages.alloc(pack.into());
        self.seen.insert(path, pack_id);
        Ok(pack_id)
    }
    /// probe unseen dependencies of a package within one step and add them
    fn __probe_path_indexed_package(&mut self, id: PackId) -> Result<Vec<PackId>> {
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
                self.__add_path_indexed_package(pack)
            })
            .collect::<Result<Vec<_>>>()?;
        self.depends_on.add(id, deps_new.iter().cloned().chain(deps_old));
        Ok(deps_new)
    }
    /// type check a package
    fn __tyck_pack(
        &self, pack: PackId, alloc: ArcGlobalAlloc, _verbose: bool,
    ) -> Result<PackageChecked> {
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
                    log::info!("Checking {}{}", self.packages[&pack].name(), pack.concise());
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
        Ok(checked)
    }
    /// compile a package to ZIR
    fn __compile_zir_pack(
        &self, pack: PackId, alloc: ArcGlobalAlloc, verbose: bool,
    ) -> Result<PackageStack> {
        let PackageChecked { spans, mut scoped, statics } =
            self.__tyck_pack(pack, alloc.clone(), verbose)?;
        let mut stack =
            zydeco_stackir::Lowerer::new(alloc.clone(), &spans, &mut scoped, &statics).run();
        {
            use zydeco_stackir::fmt::*;
            let fmt = Formatter::new(&stack, &scoped, &statics);
            let doc = stack.pretty(&fmt);
            let mut buf = String::new();
            doc.render_fmt(100, &mut buf).unwrap();
            if verbose {
                log::trace!("ZIR right after lowering:\n{}", buf);
            }
        }
        zydeco_stackir::ClosureConverter::new(&mut stack, &mut scoped, &statics).convert();
        {
            use zydeco_stackir::fmt::*;
            let fmt = Formatter::new(&stack, &scoped, &statics);
            let doc = stack.pretty(&fmt);
            let mut buf = String::new();
            doc.render_fmt(100, &mut buf).unwrap();
            if verbose {
                log::trace!("ZIR after closure conversion:\n{}", buf);
            }
        }
        Ok(PackageStack { spans, scoped, statics, stack })
    }
    /// compile a package to ZASM
    fn __compile_zasm_pack(
        &self, pack: PackId, alloc: ArcGlobalAlloc, verbose: bool,
    ) -> Result<PackageAssembly> {
        let PackageStack { spans, scoped, statics, stack } =
            self.__compile_zir_pack(pack, alloc.clone(), verbose)?;
        let assembly =
            zydeco_assembly::lower::Lowerer::new(alloc.clone(), &spans, &scoped, &statics, &stack)
                .run();
        {
            use zydeco_assembly::fmt::*;
            let fmt = Formatter::new(&assembly);
            let doc = assembly.pretty(&fmt);
            let mut buf = String::new();
            doc.render_fmt(100, &mut buf).unwrap();
            if verbose {
                log::trace!("ZASM:\n{}", buf);
            }
        }
        Ok(PackageAssembly { spans, scoped, statics, stack, assembly })
    }
}
