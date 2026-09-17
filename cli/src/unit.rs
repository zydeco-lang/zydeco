//! Native word-ABI units: one explicit initializer, a closed type interface, and exact linking.

use crate::{BuildOptions, CommandCompiler, library::*, native::NativeTool};
use serde::{Deserialize, Serialize};
use sha3::{Digest, Sha3_256};
use std::{
    collections::{BTreeMap, BTreeSet},
    path::{Path, PathBuf},
    process::Command,
};
use zydeco_session::{ProgramAnalysis, UnitProgram};
use zydeco_surface::metadata::PackageName;
use zydeco_syntax::{
    ForeignAbi, ForeignLibraryName, ForeignSymbolName, ForeignTarget, UnitImport, UnitValueType,
};

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum UnitEntryProfile {
    WordInitializerV1,
}

/// Public structural types are complete for this profile; unknowns and local arena IDs never serialize.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct UnitManifest {
    pub schema: u32,
    pub abi: ForeignAbi,
    pub profile: UnitEntryProfile,
    pub package: PackageName,
    pub platform: LibraryPlatform,
    pub compiler: String,
    pub model: String,
    pub runtime: String,
    pub symbol: ForeignSymbolName,
    pub exports: UnitValueType,
    pub artifact: LibraryFile,
    pub bindings: LibraryFile,
    pub imports: Vec<UnitImport>,
    pub dependencies: Vec<LibraryFile>,
}

impl UnitManifest {
    pub fn library_name(&self) -> ForeignLibraryName {
        ForeignLibraryName::parse(self.package.to_string().replace('/', "."))
            .expect("validated package name")
    }

    pub fn import(&self) -> UnitImport {
        UnitImport {
            target: ForeignTarget {
                abi: ForeignAbi::Zydeco,
                library: self.library_name(),
                symbol: self.symbol.clone(),
            },
            exports: self.exports.clone(),
        }
    }

    fn validate(&self) -> Result<(), LibraryError> {
        if self.schema != 1
            || self.abi != ForeignAbi::Zydeco
            || !self.symbol.as_str().starts_with("zydeco_unit_")
        {
            return Err(LibraryError::Schema);
        }
        self.exports.validate()?;
        for import in &self.imports {
            if import.target.abi != ForeignAbi::Zydeco {
                return Err(LibraryError::Schema);
            }
            import.exports.validate()?;
        }
        Ok(())
    }
}

pub struct UnitBuilder<'a> {
    pub compiler: &'a CommandCompiler,
    pub options: &'a BuildOptions,
    pub dependencies: &'a LinkedLibraries,
}

impl UnitBuilder<'_> {
    pub fn build(
        &self, name: &PackageName, analysis: &ProgramAnalysis, program: &UnitProgram,
    ) -> Result<PathBuf, LibraryError> {
        let platform =
            LibraryPlatform::for_target(self.options.architecture, self.options.operating_system)?;
        let stem = name.to_string().replace('/', ".");
        let library = ForeignLibraryName::parse(stem.clone()).expect("validated package name");
        if self.dependencies.contains(&library) || self.dependencies.units.contains(&library) {
            return Err(LibraryError::Conflict(library));
        }
        let compiler = LibraryDigest::file(&std::env::current_exe()?)?;
        let runtime = LibraryDigest::runtime(&self.options.runtime_dir)?;
        let exports = program.unit.initializer.exports.clone();
        let mut hash = Sha3_256::new();
        // Include static source dependencies and the completed exported classifier.
        for (path, text) in analysis.sources() {
            hash.update(path.as_os_str().as_encoded_bytes());
            hash.update([0]);
            hash.update(text);
            hash.update([0]);
        }
        hash.update(serde_json::to_vec(&(name, platform, &exports, &compiler, &runtime))?);
        for file in self.dependencies.units.manifest_files(&self.options.build_dir)? {
            hash.update(serde_json::to_vec(&file)?);
        }
        let identity = LibraryDigest::finish(hash);
        let symbol = ForeignSymbolName::parse(format!("zydeco_unit_{identity}_init")).unwrap();
        let code = self
            .compiler
            .lower_unit(program)?
            .emit_unit(self.options.operating_system, symbol.clone());
        if !code.foreign_imports.is_empty() {
            return Err(LibraryError::UnitCDependency);
        }
        self.dependencies.validate_units(&code.unit_imports, platform, Some(&runtime), true)?;

        std::fs::create_dir_all(&self.options.build_dir)?;
        let staging =
            tempfile::Builder::new().prefix(".zydeco-unit-").tempdir_in(&self.options.build_dir)?;
        let directory = staging.path().canonicalize()?;
        let assembly = directory.join("unit.s");
        let object_name = format!("{stem}.unit.o");
        let object = directory.join(&object_name);
        std::fs::write(&assembly, &code.assembly)?;
        NativeTool::Nasm.run(
            Command::new("nasm")
                .args(["-f", if platform == LibraryPlatform::Macos { "macho64" } else { "elf64" }])
                .arg("-o")
                .arg(&object)
                .arg(&assembly),
        )?;

        let bindings_name = format!("{stem}.imports.zy");
        let bindings = format!(
            "-- Force this initializer explicitly; retain its returned export value.\n\
             (@(ffi(zydeco, library({stem:?}), symbol({symbol:?}))) :\n\
             (@(intrinsic(thk))) ((@(intrinsic(ret))) ({})))\n",
            exports.source()?,
            symbol = symbol.as_str(),
        );
        std::fs::write(directory.join(&bindings_name), bindings)?;
        let mut bundle = Sha3_256::new();
        bundle.update(identity);
        bundle.update(LibraryDigest::file(&object)?);
        bundle.update(LibraryDigest::file(&directory.join(&bindings_name))?);
        let bundle_name = format!("{stem}.unit.{}", LibraryDigest::finish(bundle));
        let manifest = UnitManifest {
            schema: 1,
            abi: ForeignAbi::Zydeco,
            profile: UnitEntryProfile::WordInitializerV1,
            package: name.clone(),
            platform,
            compiler,
            model: zydeco_machine::native::ENTRY_SYMBOL.into(),
            runtime,
            symbol,
            exports,
            artifact: LibraryFile::in_directory(&directory, &object_name)?,
            bindings: LibraryFile::in_directory(&directory, &bindings_name)?,
            imports: code.unit_imports,
            dependencies: self
                .dependencies
                .units
                .manifest_files(&self.options.build_dir.join(&bundle_name))?,
        };
        manifest.validate()?;
        std::fs::write(
            directory.join("manifest.unit.json"),
            serde_json::to_vec_pretty(&manifest)?,
        )?;
        let destination = self.options.build_dir.join(&bundle_name);
        if !destination.exists() {
            std::fs::rename(&directory, &destination)?;
        }
        for filename in [&object_name, &bindings_name] {
            LibraryBuilder::publish_link(
                &self.options.build_dir,
                filename,
                &Path::new(&bundle_name).join(filename),
            )?;
        }
        let manifest_name = format!("{stem}.unit.json");
        LibraryBuilder::publish_link(
            &self.options.build_dir,
            &manifest_name,
            &Path::new(&bundle_name).join("manifest.unit.json"),
        )?;
        Ok(self.options.build_dir.join(manifest_name))
    }
}

#[derive(Clone, Debug)]
struct LinkedUnit {
    manifest_path: PathBuf,
    manifest: UnitManifest,
    object: PathBuf,
    dependencies: Vec<ForeignLibraryName>,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct LinkedUnits {
    entries: BTreeMap<ForeignLibraryName, LinkedUnit>,
    roots: BTreeSet<ForeignLibraryName>,
}

enum Visit {
    Enter(ForeignLibraryName),
    Leave(ForeignLibraryName),
}

impl LinkedUnits {
    pub(crate) fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub(crate) fn contains(&self, name: &ForeignLibraryName) -> bool {
        self.entries.contains_key(name)
    }

    pub(crate) fn objects(&self) -> impl Iterator<Item = &Path> {
        self.entries.values().map(|entry| entry.object.as_path())
    }

    pub(crate) fn manifest_files(&self, base: &Path) -> Result<Vec<LibraryFile>, LibraryError> {
        self.roots
            .iter()
            .map(|name| {
                let path = &self.entries[name].manifest_path;
                Ok(LibraryFile {
                    path: LibraryPaths::relative(base, path)?,
                    sha3: LibraryDigest::file(path)?,
                })
            })
            .collect()
    }

    pub(crate) fn load(paths: &[PathBuf]) -> Result<Self, LibraryError> {
        let mut units = Self::default();
        let roots = paths.iter().map(|path| path.canonicalize()).collect::<Result<Vec<_>, _>>()?;
        let mut pending = roots.clone();
        let mut visited = BTreeMap::new();
        let mut children = BTreeMap::new();
        while let Some(path) = pending.pop() {
            if visited.contains_key(&path) {
                continue;
            }
            let manifest: UnitManifest = serde_json::from_slice(&std::fs::read(&path)?)?;
            manifest.validate()?;
            let base = path.parent().unwrap();
            let object = manifest.artifact.resolve(base)?;
            manifest.bindings.resolve(base)?;
            let dependencies = manifest
                .dependencies
                .iter()
                .map(|file| file.resolve(base))
                .collect::<Result<Vec<_>, _>>()?;
            let name = manifest.library_name();
            visited.insert(path.clone(), name.clone());
            pending.extend(dependencies.clone());
            children.insert(name.clone(), dependencies);
            if let Some(previous) = units.entries.get(&name) {
                if LibraryDigest::file(&previous.manifest_path)? != LibraryDigest::file(&path)? {
                    return Err(LibraryError::Conflict(name));
                }
            } else {
                units.entries.insert(
                    name,
                    LinkedUnit { manifest_path: path, manifest, object, dependencies: Vec::new() },
                );
            }
        }
        for (name, paths) in children {
            units.entries.get_mut(&name).unwrap().dependencies =
                paths.iter().map(|path| visited[path].clone()).collect();
        }
        units.roots = roots.iter().map(|path| visited[path].clone()).collect();
        units.check_cycles()?;
        Ok(units)
    }

    fn check_cycles(&self) -> Result<(), LibraryError> {
        let mut active = BTreeSet::new();
        let mut complete = BTreeSet::new();
        let mut pending = self.roots.iter().cloned().map(Visit::Enter).collect::<Vec<_>>();
        while let Some(visit) = pending.pop() {
            match visit {
                | Visit::Enter(name) => {
                    if complete.contains(&name) {
                        continue;
                    }
                    if !active.insert(name.clone()) {
                        return Err(LibraryError::UnitCycle(name));
                    }
                    pending.push(Visit::Leave(name.clone()));
                    pending
                        .extend(self.entries[&name].dependencies.iter().cloned().map(Visit::Enter));
                }
                | Visit::Leave(name) => {
                    active.remove(&name);
                    complete.insert(name);
                }
            }
        }
        Ok(())
    }

    fn closure(
        &self, roots: impl IntoIterator<Item = ForeignLibraryName>,
    ) -> BTreeSet<ForeignLibraryName> {
        let mut pending = roots.into_iter().collect::<Vec<_>>();
        let mut result = BTreeSet::new();
        while let Some(name) = pending.pop() {
            if result.insert(name.clone()) {
                pending.extend(self.entries[&name].dependencies.iter().cloned());
            }
        }
        result
    }

    fn validate_import(
        &self, import: &UnitImport, available: &BTreeSet<ForeignLibraryName>,
    ) -> Result<(), LibraryError> {
        if !available.contains(&import.target.library)
            || self.entries[&import.target.library].manifest.import() != *import
        {
            return Err(LibraryError::UnitSignature(import.target.symbol.clone()));
        }
        Ok(())
    }

    pub(crate) fn validate(
        &self, imports: &[UnitImport], platform: LibraryPlatform, runtime: Option<&str>,
        native: bool,
    ) -> Result<(), LibraryError> {
        if self.is_empty() && imports.is_empty() {
            return Ok(());
        }
        if !native {
            return Err(LibraryError::UnitTarget);
        }
        let compiler = LibraryDigest::file(&std::env::current_exe()?)?;
        let mut symbols = BTreeSet::new();
        for entry in self.entries.values() {
            let manifest = &entry.manifest;
            if manifest.platform != platform {
                return Err(LibraryError::UnitTarget);
            }
            if manifest.compiler != compiler {
                return Err(LibraryError::UnitCompiler(manifest.package.clone()));
            }
            if manifest.model != zydeco_machine::native::ENTRY_SYMBOL
                || runtime != Some(manifest.runtime.as_str())
            {
                return Err(LibraryError::Runtime(manifest.package.clone()));
            }
            if !symbols.insert(&manifest.symbol) {
                return Err(LibraryError::SymbolCollision(manifest.symbol.clone()));
            }
            let available = self.closure(entry.dependencies.iter().cloned());
            for import in &manifest.imports {
                self.validate_import(import, &available)?;
            }
        }
        let available = self.closure(self.roots.iter().cloned());
        for import in imports {
            self.validate_import(import, &available)?;
        }
        Ok(())
    }
}
