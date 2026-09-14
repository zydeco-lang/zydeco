//! Independently compiled C libraries, their reproducible interface, and exact artifact linking.

use crate::native::NativeTool;
use crate::{
    BuildOptions, CommandCompiler, CompileError, NativeError, TargetArchitecture, TargetOs,
};
use serde::{Deserialize, Serialize};
use sha3::{Digest, Sha3_256};
use std::{
    collections::{BTreeMap, BTreeSet},
    io::Read,
    path::{Path, PathBuf},
    process::Command,
};
use zydeco_session::{LibraryProgram, ProgramAnalysis};
use zydeco_surface::metadata::{ExportSelector, LibraryContract, PackageName};
use zydeco_syntax::{
    ForeignAbi, ForeignImport, ForeignLibraryName, ForeignParameter, ForeignResult,
    ForeignSignature, ForeignSymbolName, IntegerType,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum LibraryArtifactKind {
    Object,
    Staticlib,
    Sharedlib,
}

impl LibraryArtifactKind {
    pub fn name(self) -> &'static str {
        match self {
            | Self::Object => "object",
            | Self::Staticlib => "staticlib",
            | Self::Sharedlib => "sharedlib",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum LibraryPlatform {
    #[serde(rename = "x86_64-unknown-linux-gnu")]
    Linux,
    #[serde(rename = "x86_64-apple-darwin")]
    Macos,
}

impl LibraryPlatform {
    pub fn triple(self) -> &'static str {
        match self {
            | Self::Linux => "x86_64-unknown-linux-gnu",
            | Self::Macos => "x86_64-apple-darwin",
        }
    }

    pub fn for_target(
        architecture: TargetArchitecture, os: TargetOs,
    ) -> Result<Self, LibraryError> {
        if architecture != TargetArchitecture::X86_64 {
            return Err(LibraryError::Target);
        }
        Ok(match os {
            | TargetOs::Linux => Self::Linux,
            | TargetOs::Macos => Self::Macos,
        })
    }

    fn extension(self) -> &'static str {
        match self {
            | Self::Linux => "so",
            | Self::Macos => "dylib",
        }
    }

    fn cc(self) -> Command {
        let mut command = Command::new("cc");
        match self {
            | Self::Macos => {
                command.args(["-arch", "x86_64"]);
            }
            | Self::Linux => {
                command.arg("-m64");
            }
        }
        command
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum CEntryProfile {
    FreshFatalGuardedV1,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LibraryFile {
    pub path: PathBuf,
    pub sha3: String,
}

impl LibraryFile {
    fn in_directory(directory: &Path, name: impl Into<PathBuf>) -> Result<Self, LibraryError> {
        let path = name.into();
        Ok(Self { sha3: LibraryDigest::file(&directory.join(&path))?, path })
    }

    fn resolve(&self, base: &Path) -> Result<PathBuf, LibraryError> {
        let path = base.join(&self.path).canonicalize()?;
        if LibraryDigest::file(&path)? != self.sha3 {
            return Err(LibraryError::Hash(path));
        }
        Ok(path)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PublishedExport {
    pub selector: ExportSelector,
    pub symbol: ForeignSymbolName,
    pub signature: ForeignSignature,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LibraryManifest {
    pub schema: u32,
    pub package: PackageName,
    pub platform: LibraryPlatform,
    pub abi: ForeignAbi,
    pub profile: CEntryProfile,
    pub kind: LibraryArtifactKind,
    pub compiler: String,
    pub model: String,
    pub runtime: String,
    pub fingerprint: String,
    pub artifact: LibraryFile,
    pub support: Option<LibraryFile>,
    pub header: LibraryFile,
    pub bindings: LibraryFile,
    pub exports: Vec<PublishedExport>,
    pub imports: Vec<ForeignImport>,
    pub dependencies: Vec<LibraryFile>,
}

impl LibraryManifest {
    pub fn library_name(&self) -> ForeignLibraryName {
        ForeignLibraryName::parse(self.package.to_string().replace('/', "."))
            .expect("validated package name")
    }

    fn validate(&self) -> Result<(), LibraryError> {
        if self.schema != 1 || self.exports.is_empty() {
            return Err(LibraryError::Schema);
        }
        let mut symbols = BTreeSet::new();
        let mut selectors = BTreeSet::new();
        for export in &self.exports {
            if export.symbol.as_str().starts_with("zydeco_")
                || !symbols.insert(&export.symbol)
                || !selectors.insert(&export.selector)
                || export
                    .signature
                    .parameters()
                    .iter()
                    .any(|p| !matches!(p, ForeignParameter::Integer(_)))
                || ForeignSignature::new(
                    export.signature.parameters().to_vec(),
                    export.signature.result(),
                )
                .is_err()
            {
                return Err(LibraryError::Schema);
            }
        }
        if self.exports.len() > 1 && selectors.contains(&ExportSelector::Root) {
            return Err(LibraryError::Schema);
        }
        if self.support.is_some() != (self.kind != LibraryArtifactKind::Sharedlib) {
            return Err(LibraryError::Schema);
        }
        Ok(())
    }
}

/// Digests identify content and compatibility, without asserting its authenticity.
pub struct LibraryDigest;

impl LibraryDigest {
    pub fn bytes(bytes: &[u8]) -> String {
        format!("{:x}", Sha3_256::digest(bytes))
    }

    pub fn file(path: &Path) -> Result<String, std::io::Error> {
        let mut file = std::fs::File::open(path)?;
        let mut hash = Sha3_256::new();
        let mut buffer = [0; 65536];
        loop {
            let count = file.read(&mut buffer)?;
            if count == 0 {
                break;
            }
            hash.update(&buffer[..count]);
        }
        Ok(format!("{:x}", hash.finalize()))
    }

    pub fn runtime(directory: &Path) -> Result<String, std::io::Error> {
        let mut paths = std::fs::read_dir(directory)?
            .map(|entry| entry.map(|entry| entry.path()))
            .collect::<Result<Vec<_>, _>>()?;
        paths.sort();
        let mut hash = Sha3_256::new();
        hash.update(zydeco_machine::native::ENTRY_SYMBOL);
        for path in paths.into_iter().filter(|path| path.is_file()) {
            // Build products and lockfiles are not runtime source inputs.
            if path.extension().is_some_and(|ext| ext == "rs")
                || path.file_name().is_some_and(|name| name == "Cargo.toml")
            {
                hash.update(path.file_name().unwrap().as_encoded_bytes());
                hash.update([0]);
                hash.update(std::fs::read(path)?);
                hash.update([0]);
            }
        }
        Ok(format!("{:x}", hash.finalize()))
    }
}

pub struct LibraryBuilder<'a> {
    pub compiler: &'a CommandCompiler,
    pub options: &'a BuildOptions,
    pub dependencies: &'a LinkedLibraries,
}

impl LibraryBuilder<'_> {
    pub fn build(
        &self, name: &PackageName, contract: &LibraryContract, analysis: &ProgramAnalysis,
        program: &LibraryProgram, kind: LibraryArtifactKind,
    ) -> Result<PathBuf, LibraryError> {
        let platform =
            LibraryPlatform::for_target(self.options.architecture, self.options.operating_system)?;
        let stem = name.to_string().replace('/', ".");
        let library_name = ForeignLibraryName::parse(stem.clone()).expect("validated package name");
        if self.dependencies.entries.contains_key(&library_name) {
            return Err(LibraryError::Conflict(library_name));
        }
        let runtime = LibraryDigest::runtime(&self.options.runtime_dir)?;
        let compiler = LibraryDigest::file(&std::env::current_exe()?)?;
        let exports = program
            .library
            .exports
            .iter()
            .zip(&contract.exports)
            .map(|(checked, declared)| PublishedExport {
                selector: declared.selector.clone(),
                symbol: checked.symbol.clone(),
                signature: checked.signature.clone(),
            })
            .collect::<Vec<_>>();
        let mut hash = Sha3_256::new();
        for (path, text) in analysis.sources() {
            hash.update(path.as_os_str().as_encoded_bytes());
            hash.update([0]);
            hash.update(text);
            hash.update([0]);
        }
        hash.update(name.to_string());
        hash.update(serde_json::to_vec(&exports)?);
        hash.update(&compiler);
        hash.update(&runtime);
        hash.update(platform.triple());
        for root in &self.dependencies.roots {
            hash.update(root.as_str());
            hash.update([0]);
        }
        for dependency in self.dependencies.entries.values() {
            hash.update(&dependency.manifest.fingerprint);
            hash.update(LibraryDigest::file(&dependency.manifest_path)?);
        }
        let identity = format!("{:x}", hash.finalize());
        let guard = ForeignSymbolName::parse(format!("zydeco_unit_{identity}_guard")).unwrap();
        let code = program
            .library
            .exports
            .iter()
            .map(|export| {
                let backend = self.compiler.lower_export(program, export)?;
                Ok(backend.emit_c_export(
                    self.options.operating_system,
                    zydeco_amd64::emit::CExportEntry {
                        symbol: export.symbol.clone(),
                        signature: export.signature.clone(),
                        guard: guard.clone(),
                    },
                ))
            })
            .collect::<Result<Vec<_>, CompileError>>()?;
        let imports =
            code.iter().flat_map(|entry| entry.foreign_imports.clone()).collect::<Vec<_>>();
        self.dependencies.validate_imports(&imports, platform, Some(&runtime), false)?;
        for import in imports.iter().filter(|import| import.target.library.as_str() == stem) {
            if !exports.iter().any(|export| {
                export.symbol == import.target.symbol && export.signature == import.signature
            }) {
                return Err(LibraryError::Signature(import.target.symbol.clone()));
            }
        }
        for export in &exports {
            if self.dependencies.linked().any(|library| {
                library.manifest.exports.iter().any(|other| other.symbol == export.symbol)
            }) {
                return Err(LibraryError::SymbolCollision(export.symbol.clone()));
            }
        }
        // All compiler validation finishes before a tool runs or a published output is touched.
        std::fs::create_dir_all(&self.options.build_dir)?;
        let staging = tempfile::Builder::new()
            .prefix(".zydeco-library-")
            .tempdir_in(&self.options.build_dir)?;
        let directory = staging.path().canonicalize()?;
        let mut objects = Vec::new();
        for (index, entry) in code.iter().enumerate() {
            objects.push(self.assemble(
                &directory,
                &format!("entry_{index}"),
                &entry.assembly,
                platform,
            )?);
        }
        objects.push(self.assemble(
            &directory,
            "guard",
            &format!("section .data\nalign 8\nglobal {guard}\n{guard}: db 0\n"),
            platform,
        )?);
        let artifact_name = match kind {
            | LibraryArtifactKind::Object => format!("{stem}.o"),
            | LibraryArtifactKind::Staticlib => format!("lib{stem}.a"),
            | LibraryArtifactKind::Sharedlib => format!("lib{stem}.{}", platform.extension()),
        };
        let artifact_path = directory.join(&artifact_name);
        match kind {
            | LibraryArtifactKind::Object => {
                NativeTool::Linker.run(
                    platform
                        .cc()
                        .args(["-nostdlib", "-Wl,-r"])
                        .args(&objects)
                        .arg("-o")
                        .arg(&artifact_path),
                )?;
            }
            | LibraryArtifactKind::Staticlib => NativeTool::Archive
                .run(Command::new("ar").arg("crs").arg(&artifact_path).args(&objects))?,
            | LibraryArtifactKind::Sharedlib => {}
        }
        let support_name = format!("libzydeco_runtime_{runtime}.a");
        self.runtime_support(&directory, &support_name, platform)?;
        if kind == LibraryArtifactKind::Sharedlib {
            let mut linker = platform.cc();
            linker.arg(if platform == LibraryPlatform::Macos { "-dynamiclib" } else { "-shared" });
            linker.args(&objects);
            let external_imports = imports
                .iter()
                .filter(|import| import.target.library.as_str() != stem)
                .cloned()
                .collect::<Vec<_>>();
            self.dependencies.configure_linker(
                &mut linker,
                &external_imports,
                platform,
                &directory,
            )?;
            linker.arg(directory.join(&support_name));
            let export_list = directory.join("exports.txt");
            match platform {
                | LibraryPlatform::Linux => {
                    std::fs::write(
                        &export_list,
                        format!(
                            "{{ global: {}; local: *; }};\n",
                            exports
                                .iter()
                                .map(|e| e.symbol.to_string())
                                .collect::<Vec<_>>()
                                .join("; ")
                        ),
                    )?;
                    linker.arg(format!("-Wl,--version-script={}", export_list.display())).args([
                        "-Wl,-Bsymbolic",
                        "-Wl,-z,defs",
                        "-ldl",
                        "-lpthread",
                        "-lm",
                    ]);
                }
                | LibraryPlatform::Macos => {
                    std::fs::write(
                        &export_list,
                        exports.iter().map(|e| format!("_{}\n", e.symbol)).collect::<String>(),
                    )?;
                    linker
                        .arg(format!("-Wl,-exported_symbols_list,{}", export_list.display()))
                        .arg(format!("-Wl,-install_name,@rpath/{artifact_name}"));
                }
            }
            NativeTool::Linker.run(linker.arg("-o").arg(&artifact_path))?;
        }
        let header_name = format!("{stem}.h");
        let bindings_name = format!("{stem}.imports.zy");
        std::fs::write(directory.join(&header_name), LibraryInterface::header(&exports))?;
        std::fs::write(
            directory.join(&bindings_name),
            LibraryInterface::bindings(&stem, &exports),
        )?;
        let mut fingerprint = Sha3_256::new();
        fingerprint.update(&identity);
        fingerprint.update(kind.name());
        for entry in &code {
            fingerprint.update(&entry.assembly);
        }
        fingerprint.update(LibraryDigest::file(&artifact_path)?);
        fingerprint.update(LibraryDigest::file(&directory.join(&support_name))?);
        let fingerprint = format!("{:x}", fingerprint.finalize());
        let bundle_name = format!("{stem}.{}.{}", kind.name(), fingerprint);
        let dependencies =
            self.dependencies.manifest_files(&self.options.build_dir.join(&bundle_name))?;
        let manifest = LibraryManifest {
            schema: 1,
            package: name.clone(),
            platform,
            abi: contract.abi,
            profile: CEntryProfile::FreshFatalGuardedV1,
            kind,
            compiler,
            model: zydeco_machine::native::ENTRY_SYMBOL.into(),
            runtime,
            fingerprint,
            artifact: LibraryFile::in_directory(&directory, &artifact_name)?,
            support: if kind == LibraryArtifactKind::Sharedlib {
                None
            } else {
                Some(LibraryFile::in_directory(&directory, &support_name)?)
            },
            header: LibraryFile::in_directory(&directory, &header_name)?,
            bindings: LibraryFile::in_directory(&directory, &bindings_name)?,
            exports,
            imports,
            dependencies,
        };
        let manifest_name = format!("{stem}.{}.library.json", kind.name());
        let manifest_text = serde_json::to_vec_pretty(&manifest)?;
        std::fs::write(directory.join("manifest.library.json"), manifest_text)?;
        std::fs::remove_dir_all(directory.join("runtime"))?;
        let destination = self.options.build_dir.join(&bundle_name);
        if !destination.exists() {
            std::fs::rename(&directory, &destination)?;
        }
        // Convenience names can be used by C tools and source imports. The manifest is the
        // authoritative commit point and resolves inside the immutable, complete bundle.
        for filename in [&artifact_name, &header_name, &bindings_name] {
            Self::publish_link(
                &self.options.build_dir,
                filename,
                &Path::new(&bundle_name).join(filename),
            )?;
        }
        let manifest_path = self.options.build_dir.join(manifest_name);
        Self::publish_link(
            &self.options.build_dir,
            manifest_path.file_name().unwrap().to_str().unwrap(),
            &Path::new(&bundle_name).join("manifest.library.json"),
        )?;
        Ok(manifest_path)
    }

    fn publish_link(directory: &Path, filename: &str, target: &Path) -> Result<(), LibraryError> {
        let staging = tempfile::Builder::new().prefix(".publish-").tempdir_in(directory)?;
        let link = staging.path().join("link");
        #[cfg(unix)]
        std::os::unix::fs::symlink(target, &link)?;
        #[cfg(not(unix))]
        return Err(LibraryError::Target);
        std::fs::rename(link, directory.join(filename))?;
        Ok(())
    }

    fn assemble(
        &self, directory: &Path, name: &str, text: &str, platform: LibraryPlatform,
    ) -> Result<PathBuf, LibraryError> {
        let source = directory.join(format!("{name}.s"));
        let object = directory.join(format!("{name}.o"));
        std::fs::write(&source, text)?;
        NativeTool::Nasm.run(
            Command::new("nasm")
                .arg("-f")
                .arg(if platform == LibraryPlatform::Macos { "macho64" } else { "elf64" })
                .arg("-o")
                .arg(&object)
                .arg(&source),
        )?;
        Ok(object)
    }

    fn runtime_support(
        &self, directory: &Path, name: &str, platform: LibraryPlatform,
    ) -> Result<(), LibraryError> {
        let directory = directory.join("runtime");
        BuildOptions { build_dir: directory.clone(), ..self.options.clone() }.prepare()?;
        NativeTool::Cargo.run(
            Command::new("cargo")
                .args([
                    "build",
                    "--lib",
                    "--no-default-features",
                    "--release",
                    "--target",
                    platform.triple(),
                ])
                .arg("--manifest-path")
                .arg(directory.join("Cargo.toml"))
                .env(
                    "CARGO_ENCODED_RUSTFLAGS",
                    [
                        "-Cpanic=abort".to_owned(),
                        "-Crelocation-model=pic".to_owned(),
                        format!("--remap-path-prefix={}=/zydeco-runtime", directory.display()),
                    ]
                    .join("\x1f"),
                ),
        )?;
        std::fs::copy(
            directory.join("target").join(platform.triple()).join("release/libzydeco_runtime.a"),
            directory.parent().unwrap().join(name),
        )?;
        Ok(())
    }
}

struct LibraryInterface;

impl LibraryInterface {
    fn c_type(integer: IntegerType) -> String {
        format!("{}int{}_t", if integer.is_signed() { "" } else { "u" }, integer.bits())
    }

    fn header(exports: &[PublishedExport]) -> String {
        let mut text = String::from(
            "#pragma once\n#include <stdint.h>\n#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n/* Each call owns fresh state. Runtime faults terminate the process.\n   Concurrent or reentrant entry into this library is rejected. */\n",
        );
        for export in exports {
            let result = match export.signature.result() {
                | ForeignResult::Unit => "void".into(),
                | ForeignResult::Integer(integer) => Self::c_type(integer),
            };
            let args = export
                .signature
                .parameters()
                .iter()
                .enumerate()
                .map(|(i, param)| match param {
                    | ForeignParameter::Integer(integer) => {
                        format!("{} arg{i}", Self::c_type(*integer))
                    }
                    | _ => unreachable!(),
                })
                .collect::<Vec<_>>();
            text.push_str(&format!(
                "{result} {}({});\n",
                export.symbol,
                if args.is_empty() { "void".into() } else { args.join(", ") }
            ));
        }
        text.push_str("\n#ifdef __cplusplus\n}\n#endif\n");
        text
    }

    fn binding(library: &str, export: &PublishedExport) -> String {
        let args = export
            .signature
            .parameters()
            .iter()
            .map(|p| match p {
                | ForeignParameter::Integer(integer) => format!("{} -> ", integer.type_name()),
                | _ => unreachable!(),
            })
            .collect::<String>();
        let result = match export.signature.result() {
            | ForeignResult::Unit => "Unit",
            | ForeignResult::Integer(integer) => integer.type_name(),
        };
        format!(
            "(@(ffi(c, library({library:?}), symbol({:?}))) : Thk ({args}Ret {result}))",
            export.symbol.as_str()
        )
    }

    fn fields(library: &str, exports: &[(&[&str], &PublishedExport)]) -> String {
        let mut groups = BTreeMap::<&str, Vec<(&[&str], &PublishedExport)>>::new();
        for (path, export) in exports {
            groups.entry(path[0]).or_default().push((&path[1..], export));
        }
        format!(
            "(\n{}\n)",
            groups
                .into_iter()
                .map(|(field, exports)| {
                    let value = if exports[0].0.is_empty() {
                        Self::binding(library, exports[0].1)
                    } else {
                        Self::fields(library, &exports)
                    };
                    format!("#{field} = {value}")
                })
                .collect::<Vec<_>>()
                .join(",\n")
        )
    }

    fn bindings(library: &str, exports: &[PublishedExport]) -> String {
        let mut text = String::from(
            "let Thk = @(intrinsic(thk)) in\nlet Ret = @(intrinsic(ret)) in\nlet Unit = @(intrinsic(unit)) in\n",
        );
        for integer in [
            IntegerType::Int8,
            IntegerType::Int16,
            IntegerType::Int32,
            IntegerType::Int64,
            IntegerType::UInt8,
            IntegerType::UInt16,
            IntegerType::UInt32,
            IntegerType::UInt64,
        ] {
            let intrinsic =
                format!("{}{}", if integer.is_signed() { "i" } else { "u" }, integer.bits());
            text.push_str(&format!("let {} = @(intrinsic({intrinsic})) in\n", integer.type_name()));
        }
        if matches!(exports[0].selector, ExportSelector::Root) {
            text.push_str(&Self::binding(library, &exports[0]));
        } else {
            let paths = exports
                .iter()
                .map(|export| match &export.selector {
                    | ExportSelector::Field(path) => path.to_string(),
                    | _ => unreachable!(),
                })
                .collect::<Vec<_>>();
            let components =
                paths.iter().map(|path| path.split('/').collect::<Vec<_>>()).collect::<Vec<_>>();
            text.push_str(&Self::fields(
                library,
                &components
                    .iter()
                    .zip(exports)
                    .map(|(path, export)| (path.as_slice(), export))
                    .collect::<Vec<_>>(),
            ));
        }
        text.push('\n');
        text
    }
}

#[derive(Clone, Debug, Default)]
pub struct LinkedLibraries {
    // The catalog includes build provenance; only `active` enters the consuming image.
    entries: BTreeMap<ForeignLibraryName, LinkedLibrary>,
    roots: BTreeSet<ForeignLibraryName>,
    active: BTreeSet<ForeignLibraryName>,
}

#[derive(Clone, Debug)]
struct LinkedLibrary {
    manifest_path: PathBuf,
    manifest: LibraryManifest,
    artifact: PathBuf,
    dependencies: Vec<ForeignLibraryName>,
}

impl LinkedLibraries {
    pub fn is_empty(&self) -> bool {
        self.roots.is_empty()
    }

    fn linked(&self) -> impl Iterator<Item = &LinkedLibrary> {
        self.active.iter().map(|name| &self.entries[name])
    }

    /// Shared images already contain their raw dependencies. Follow those dependencies
    /// for shared-loader requirements, without linking or loading their raw code again.
    fn closure(
        &self, roots: impl IntoIterator<Item = ForeignLibraryName>,
    ) -> BTreeSet<ForeignLibraryName> {
        let mut pending = roots.into_iter().map(|name| (name, true)).collect::<Vec<_>>();
        let mut visited = BTreeSet::new();
        let mut active = BTreeSet::new();
        while let Some((name, raw)) = pending.pop() {
            if !visited.insert((name.clone(), raw)) {
                continue;
            }
            let library = &self.entries[&name];
            let shared = library.manifest.kind == LibraryArtifactKind::Sharedlib;
            if raw || shared {
                active.insert(name);
            }
            pending.extend(library.dependencies.iter().cloned().map(|name| (name, raw && !shared)));
        }
        active
    }

    pub fn paths(&self) -> BTreeMap<ForeignLibraryName, PathBuf> {
        self.active.iter().map(|name| (name.clone(), self.entries[name].artifact.clone())).collect()
    }

    /// Bundle raw units with the program before rustc examines runtime archives. A
    /// late link-arg archive can otherwise introduce helper references after the linker
    /// has already scanned the runtime. Keep one member name across incremental builds.
    pub(crate) fn bundle_raw(&self, object: &Path, os: TargetOs) -> Result<(), NativeError> {
        let raw = self
            .linked()
            .filter(|entry| entry.manifest.kind != LibraryArtifactKind::Sharedlib)
            .collect::<Vec<_>>();
        if raw.is_empty() {
            return Ok(());
        }
        let platform = match os {
            | TargetOs::Linux => LibraryPlatform::Linux,
            | TargetOs::Macos => LibraryPlatform::Macos,
        };
        let combined = object.with_extension("units.o");
        let mut command = platform.cc();
        command.args(["-nostdlib", "-Wl,-r"]).arg(object);
        for library in raw {
            match (platform, library.manifest.kind) {
                | (_, LibraryArtifactKind::Object) => {
                    command.arg(&library.artifact);
                }
                | (LibraryPlatform::Linux, LibraryArtifactKind::Staticlib) => {
                    command
                        .arg("-Wl,--whole-archive")
                        .arg(&library.artifact)
                        .arg("-Wl,--no-whole-archive");
                }
                | (LibraryPlatform::Macos, LibraryArtifactKind::Staticlib) => {
                    command.arg(format!("-Wl,-force_load,{}", library.artifact.display()));
                }
                | (_, LibraryArtifactKind::Sharedlib) => unreachable!(),
            }
        }
        NativeTool::Linker.run(command.arg("-o").arg(&combined))?;
        std::fs::rename(combined, object).map_err(NativeError::WriteBackendOutput)
    }

    /// Final dependencies for rustc after raw units have joined its program archive.
    pub(crate) fn rust_flags(&self) -> Vec<String> {
        let mut flags = Vec::new();
        for library in
            self.linked().filter(|entry| entry.manifest.kind == LibraryArtifactKind::Sharedlib)
        {
            flags.push(format!("-Clink-arg={}", library.artifact.display()));
            flags.push(format!(
                "-Clink-arg=-Wl,-rpath,{}",
                library.artifact.parent().unwrap().display()
            ));
        }
        for library in self
            .linked()
            .filter(|entry| entry.manifest.kind != LibraryArtifactKind::Sharedlib)
            .flat_map(|entry| &entry.manifest.imports)
            .map(|import| &import.target.library)
            .filter(|library| !self.entries.contains_key(*library))
            .collect::<BTreeSet<_>>()
        {
            flags.push(format!("-Clink-arg=-l{library}"));
        }
        flags
    }

    pub fn contains(&self, library: &ForeignLibraryName) -> bool {
        self.active.contains(library)
    }
    pub fn load(paths: &[PathBuf]) -> Result<Self, LibraryError> {
        let mut libraries = Self::default();
        let roots = paths.iter().map(|path| path.canonicalize()).collect::<Result<Vec<_>, _>>()?;
        let mut pending = roots.clone();
        let mut visited = BTreeMap::new();
        let mut dependencies = BTreeMap::new();
        while let Some(path) = pending.pop() {
            let path = path.canonicalize()?;
            if visited.contains_key(&path) {
                continue;
            }
            let manifest: LibraryManifest = serde_json::from_slice(&std::fs::read(&path)?)?;
            manifest.validate()?;
            let base = path.parent().unwrap();
            let artifact = manifest.artifact.resolve(base)?;
            manifest.support.as_ref().map(|file| file.resolve(base)).transpose()?;
            manifest.header.resolve(base)?;
            manifest.bindings.resolve(base)?;
            let children = manifest
                .dependencies
                .iter()
                .map(|file| file.resolve(base))
                .collect::<Result<Vec<_>, _>>()?;
            pending.extend(children.clone());
            let name = manifest.library_name();
            visited.insert(path.clone(), name.clone());
            dependencies.insert(name.clone(), children);
            if let Some(previous) = libraries.entries.get(&name) {
                if LibraryDigest::file(&previous.manifest_path)? != LibraryDigest::file(&path)? {
                    return Err(LibraryError::Conflict(name));
                }
            } else {
                libraries.entries.insert(
                    name,
                    LinkedLibrary {
                        manifest_path: path,
                        manifest,
                        artifact,
                        dependencies: Vec::new(),
                    },
                );
            }
        }
        for (name, paths) in dependencies {
            libraries.entries.get_mut(&name).unwrap().dependencies =
                paths.iter().map(|path| visited[path].clone()).collect();
        }
        libraries.roots = roots.iter().map(|path| visited[path].clone()).collect();
        libraries.active = libraries.closure(libraries.roots.iter().cloned());
        Ok(libraries)
    }

    pub fn validate_imports(
        &self, imports: &[ForeignImport], platform: LibraryPlatform, runtime: Option<&str>,
        interpreter: bool,
    ) -> Result<(), LibraryError> {
        for library in self.entries.values() {
            if library.manifest.platform != platform {
                return Err(LibraryError::Target);
            }
            // Validate each producer's own link image, including raw dependencies now
            // hidden inside a shared artifact. Shared dependencies own independent runtimes.
            let available = self.closure(library.dependencies.iter().cloned());
            let mut image = available.clone();
            image.insert(library.manifest.library_name());
            self.validate_image(
                &image,
                &library.manifest.model,
                Some(&library.manifest.runtime),
                false,
            )?;
            for import in &library.manifest.imports {
                self.validate_import(import, &available, Some(&library.manifest.library_name()))?;
            }
        }
        self.validate_image(
            &self.active,
            zydeco_machine::native::ENTRY_SYMBOL,
            runtime,
            interpreter,
        )?;
        for import in imports {
            self.validate_import(import, &self.active, None)?;
        }
        Ok(())
    }

    fn validate_image(
        &self, names: &BTreeSet<ForeignLibraryName>, model: &str, runtime: Option<&str>,
        interpreter: bool,
    ) -> Result<(), LibraryError> {
        let mut symbols = BTreeSet::new();
        for name in names {
            let library = &self.entries[name];
            if library.manifest.kind != LibraryArtifactKind::Sharedlib {
                if interpreter {
                    return Err(LibraryError::Target);
                }
                if library.manifest.model != model
                    || runtime.is_some_and(|runtime| library.manifest.runtime != runtime)
                {
                    return Err(LibraryError::Runtime(library.manifest.package.clone()));
                }
            }
            for export in &library.manifest.exports {
                if !symbols.insert(&export.symbol) {
                    return Err(LibraryError::SymbolCollision(export.symbol.clone()));
                }
            }
        }
        Ok(())
    }

    fn validate_import(
        &self, import: &ForeignImport, available: &BTreeSet<ForeignLibraryName>,
        own: Option<&ForeignLibraryName>,
    ) -> Result<(), LibraryError> {
        if let Some(library) = self.entries.get(&import.target.library) {
            if !available.contains(&import.target.library) && own != Some(&import.target.library) {
                return Err(LibraryError::Unavailable(import.target.library.clone()));
            }
            if !library.manifest.exports.iter().any(|export| {
                export.symbol == import.target.symbol && export.signature == import.signature
            }) {
                return Err(LibraryError::Signature(import.target.symbol.clone()));
            }
        }
        Ok(())
    }

    fn manifest_files(&self, directory: &Path) -> Result<Vec<LibraryFile>, LibraryError> {
        self.roots
            .iter()
            .map(|name| {
                let library = &self.entries[name];
                Ok(LibraryFile {
                    path: LibraryPaths::relative(directory, &library.manifest_path)?,
                    sha3: LibraryDigest::file(&library.manifest_path)?,
                })
            })
            .collect()
    }

    pub fn configure_linker(
        &self, command: &mut Command, imports: &[ForeignImport], platform: LibraryPlatform,
        origin: &Path,
    ) -> Result<(), LibraryError> {
        if platform == LibraryPlatform::Linux {
            command.arg("-Wl,--start-group");
        }
        for library in self.linked() {
            command.arg(&library.artifact);
        }
        if platform == LibraryPlatform::Linux {
            command.arg("-Wl,--end-group");
        }
        for directory in self
            .linked()
            .filter(|library| library.manifest.kind == LibraryArtifactKind::Sharedlib)
            .map(|library| library.artifact.parent().unwrap())
            .collect::<BTreeSet<_>>()
        {
            let relative = LibraryPaths::relative(origin, directory)?;
            let loader =
                if platform == LibraryPlatform::Macos { "@loader_path" } else { "$ORIGIN" };
            command.arg(format!("-Wl,-rpath,{loader}/{}", relative.display()));
        }
        let ordinary = imports
            .iter()
            .chain(
                self.linked()
                    .filter(|library| library.manifest.kind != LibraryArtifactKind::Sharedlib)
                    .flat_map(|library| &library.manifest.imports),
            )
            .map(|import| &import.target.library)
            .filter(|name| !self.entries.contains_key(*name))
            .collect::<BTreeSet<_>>();
        for library in ordinary {
            command.arg(format!("-l{library}"));
        }
        Ok(())
    }
}

struct LibraryPaths;

impl LibraryPaths {
    fn absolute(path: &Path) -> Result<PathBuf, std::io::Error> {
        let path = std::env::current_dir()?.join(path);
        match path.canonicalize() {
            | Ok(path) => Ok(path),
            | Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                let Some(parent) = path.parent() else { return Err(error) };
                let Some(name) = path.file_name() else { return Err(error) };
                Ok(Self::absolute(parent)?.join(name))
            }
            | Err(error) => Err(error),
        }
    }

    fn relative(base: &Path, target: &Path) -> Result<PathBuf, std::io::Error> {
        // Normalize existing ancestors, including macOS /var -> /private/var. The
        // final immutable bundle need not exist yet when its manifest is assembled.
        let base = Self::absolute(base)?;
        let target = Self::absolute(target)?;
        let mut left = base.components().peekable();
        let mut right = target.components().peekable();
        while left.peek().is_some() && left.peek() == right.peek() {
            left.next();
            right.next();
        }
        Ok(left.map(|_| std::path::Component::ParentDir).chain(right).collect())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum LibraryError {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error(transparent)]
    Json(#[from] serde_json::Error),
    #[error(transparent)]
    Compile(#[from] CompileError),
    #[error(transparent)]
    Native(#[from] NativeError),
    #[error(
        "compiled C libraries require matching AMD64 Linux or macOS artifacts; the interpreter requires shared libraries matching its host"
    )]
    Target,
    #[error("invalid or unsupported compiled-library manifest")]
    Schema,
    #[error("compiled-library content hash mismatch: {}", .0.display())]
    Hash(PathBuf),
    #[error(
        "library `{0}` is private to a shared dependency; link its own manifest to use it directly"
    )]
    Unavailable(ForeignLibraryName),
    #[error("conflicting artifacts for library `{0}`")]
    Conflict(ForeignLibraryName),
    #[error("compiled library `{0}` requires a different native runtime")]
    Runtime(PackageName),
    #[error("compiled-library symbol `{0}` is absent or has a different C signature")]
    Signature(ForeignSymbolName),
    #[error("multiple compiled libraries declare C symbol `{0}`")]
    SymbolCollision(ForeignSymbolName),
}

#[cfg(test)]
mod tests {
    use super::*;
    use zydeco_syntax::ForeignTarget;

    struct Fixture {
        directory: tempfile::TempDir,
    }

    impl Fixture {
        fn new() -> Self {
            Self { directory: tempfile::tempdir().unwrap() }
        }

        fn publish(
            &self, label: &str, package: &str, symbol: &str, kind: LibraryArtifactKind,
            dependencies: &[PathBuf],
        ) -> PathBuf {
            let directory = self.directory.path().join(label);
            std::fs::create_dir(&directory).unwrap();
            for (name, bytes) in [
                ("artifact", label),
                ("support", "support"),
                ("header", "header"),
                ("bindings", "bindings"),
            ] {
                std::fs::write(directory.join(name), bytes).unwrap();
            }
            let manifest = LibraryManifest {
                schema: 1,
                package: package.parse().unwrap(),
                platform: LibraryPlatform::Linux,
                abi: ForeignAbi::C,
                profile: CEntryProfile::FreshFatalGuardedV1,
                kind,
                compiler: "test-compiler".into(),
                model: zydeco_machine::native::ENTRY_SYMBOL.into(),
                runtime: "test-runtime".into(),
                fingerprint: LibraryDigest::bytes(label.as_bytes()),
                artifact: LibraryFile::in_directory(&directory, "artifact").unwrap(),
                support: (kind != LibraryArtifactKind::Sharedlib)
                    .then(|| LibraryFile::in_directory(&directory, "support").unwrap()),
                header: LibraryFile::in_directory(&directory, "header").unwrap(),
                bindings: LibraryFile::in_directory(&directory, "bindings").unwrap(),
                exports: vec![PublishedExport {
                    selector: ExportSelector::Root,
                    symbol: ForeignSymbolName::parse(symbol).unwrap(),
                    signature: Self::signature(IntegerType::Int64),
                }],
                imports: vec![],
                dependencies: dependencies
                    .iter()
                    .map(|path| LibraryFile {
                        path: path.clone(),
                        sha3: LibraryDigest::file(path).unwrap(),
                    })
                    .collect(),
            };
            let path = directory.join("manifest.library.json");
            std::fs::write(&path, serde_json::to_vec(&manifest).unwrap()).unwrap();
            path
        }

        fn signature(integer: IntegerType) -> ForeignSignature {
            ForeignSignature::new(
                vec![ForeignParameter::Integer(integer)],
                ForeignResult::Integer(integer),
            )
            .unwrap()
        }

        fn import(package: &str, symbol: &str, integer: IntegerType) -> ForeignImport {
            ForeignImport {
                target: ForeignTarget {
                    abi: ForeignAbi::C,
                    library: ForeignLibraryName::parse(package).unwrap(),
                    symbol: ForeignSymbolName::parse(symbol).unwrap(),
                },
                signature: Self::signature(integer),
            }
        }

        fn edit(path: &Path, edit: impl FnOnce(&mut serde_json::Value)) {
            let mut value = serde_json::from_slice(&std::fs::read(path).unwrap()).unwrap();
            edit(&mut value);
            std::fs::write(path, serde_json::to_vec(&value).unwrap()).unwrap();
        }
    }

    #[test]
    fn manifests_reject_invalid_contracts_before_loading_code() {
        let fixture = Fixture::new();
        let path = fixture.publish(
            "valid",
            "example/math",
            "identity",
            LibraryArtifactKind::Sharedlib,
            &[],
        );
        let original = std::fs::read(&path).unwrap();
        LinkedLibraries::load(std::slice::from_ref(&path))
            .unwrap()
            .validate_imports(
                &[Fixture::import("example.math", "identity", IntegerType::Int64)],
                LibraryPlatform::Linux,
                None,
                true,
            )
            .unwrap();
        for (field, value) in [
            ("schema", serde_json::json!(2)),
            ("profile", serde_json::json!("persistent")),
            ("package", serde_json::json!("bad.name")),
            ("abi", serde_json::json!("rust")),
            ("exports", serde_json::json!([])),
        ] {
            Fixture::edit(&path, |manifest| manifest[field] = value);
            assert!(
                LinkedLibraries::load(std::slice::from_ref(&path)).is_err(),
                "accepted invalid {field}"
            );
            std::fs::write(&path, &original).unwrap();
        }
        for import in [
            Fixture::import("example.math", "absent", IntegerType::Int64),
            Fixture::import("example.math", "identity", IntegerType::UInt64),
        ] {
            assert!(matches!(
                LinkedLibraries::load(std::slice::from_ref(&path)).unwrap().validate_imports(
                    &[import],
                    LibraryPlatform::Linux,
                    None,
                    true
                ),
                Err(LibraryError::Signature(_))
            ));
        }
        assert!(matches!(
            LinkedLibraries::load(std::slice::from_ref(&path)).unwrap().validate_imports(
                &[],
                LibraryPlatform::Macos,
                None,
                true
            ),
            Err(LibraryError::Target)
        ));
        Fixture::edit(&path, |manifest| {
            manifest["exports"][0]["signature"]["parameters"] = serde_json::json!(["Address"])
        });
        assert!(matches!(
            LinkedLibraries::load(std::slice::from_ref(&path)),
            Err(LibraryError::Schema)
        ));
        std::fs::write(&path, &original).unwrap();
        Fixture::edit(&path, |manifest| {
            manifest["imports"] =
                serde_json::json!([Fixture::import("c", "foreign", IntegerType::Int64)]);
            manifest["imports"][0]["signature"]["parameters"] =
                serde_json::json!(vec![ForeignParameter::Integer(IntegerType::Int64); 7]);
        });
        assert!(matches!(
            LinkedLibraries::load(std::slice::from_ref(&path)),
            Err(LibraryError::Json(_))
        ));
        std::fs::write(&path, &original).unwrap();
        for name in ["artifact", "header", "bindings"] {
            let file = path.parent().unwrap().join(name);
            let content = std::fs::read(&file).unwrap();
            std::fs::write(&file, "corrupted").unwrap();
            assert!(matches!(
                LinkedLibraries::load(std::slice::from_ref(&path)),
                Err(LibraryError::Hash(_))
            ));
            std::fs::write(&file, content).unwrap();
        }
    }

    #[test]
    fn raw_dependencies_share_a_runtime_and_shared_images_hide_embedded_code() {
        let fixture = Fixture::new();
        let raw = fixture.publish("raw", "example/raw", "raw", LibraryArtifactKind::Staticlib, &[]);
        let libraries = LinkedLibraries::load(std::slice::from_ref(&raw)).unwrap();
        libraries
            .validate_imports(&[], LibraryPlatform::Linux, Some("test-runtime"), false)
            .unwrap();
        assert!(matches!(
            libraries.validate_imports(&[], LibraryPlatform::Linux, Some("other-runtime"), false),
            Err(LibraryError::Runtime(_))
        ));
        assert!(matches!(
            libraries.validate_imports(&[], LibraryPlatform::Linux, None, true),
            Err(LibraryError::Target)
        ));
        let shared = fixture.publish(
            "shared",
            "example/shared",
            "shared",
            LibraryArtifactKind::Sharedlib,
            std::slice::from_ref(&raw),
        );
        let libraries = LinkedLibraries::load(std::slice::from_ref(&shared)).unwrap();
        libraries
            .validate_imports(
                &[],
                LibraryPlatform::Linux,
                Some("independent-consumer-runtime"),
                false,
            )
            .unwrap();
        libraries.validate_imports(&[], LibraryPlatform::Linux, None, true).unwrap();
        assert_eq!(libraries.paths().len(), 1);
        assert!(!libraries.contains(&ForeignLibraryName::parse("example.raw").unwrap()));
        assert!(matches!(
            libraries.validate_imports(
                &[Fixture::import("example.raw", "raw", IntegerType::Int64)],
                LibraryPlatform::Linux,
                None,
                true
            ),
            Err(LibraryError::Unavailable(_))
        ));
        let both = LinkedLibraries::load(&[shared, raw.clone()]).unwrap();
        assert_eq!(both.paths().len(), 2);
        both.validate_imports(&[], LibraryPlatform::Linux, Some("test-runtime"), false).unwrap();
        let broken = fixture.publish(
            "broken",
            "example/broken",
            "broken",
            LibraryArtifactKind::Sharedlib,
            &[raw],
        );
        Fixture::edit(&broken, |manifest| {
            manifest["runtime"] = serde_json::json!("different-producer-runtime")
        });
        assert!(matches!(
            LinkedLibraries::load(&[broken]).unwrap().validate_imports(
                &[],
                LibraryPlatform::Linux,
                None,
                true
            ),
            Err(LibraryError::Runtime(_))
        ));
    }

    #[test]
    fn logical_identity_and_public_symbols_are_unambiguous() {
        let fixture = Fixture::new();
        let first = fixture.publish(
            "first",
            "example/first",
            "identity",
            LibraryArtifactKind::Sharedlib,
            &[],
        );
        let duplicate = fixture.publish(
            "duplicate",
            "example/first",
            "identity",
            LibraryArtifactKind::Sharedlib,
            &[],
        );
        assert!(matches!(
            LinkedLibraries::load(&[first.clone(), duplicate]),
            Err(LibraryError::Conflict(_))
        ));
        let collision = fixture.publish(
            "collision",
            "example/second",
            "identity",
            LibraryArtifactKind::Sharedlib,
            &[],
        );
        assert!(matches!(
            LinkedLibraries::load(&[first.clone(), collision]).unwrap().validate_imports(
                &[],
                LibraryPlatform::Linux,
                None,
                true
            ),
            Err(LibraryError::SymbolCollision(_))
        ));
        assert_eq!(LinkedLibraries::load(&[first.clone(), first]).unwrap().paths().len(), 1);
    }
}
