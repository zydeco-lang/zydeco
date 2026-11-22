use crate::{TargetArchitecture, TargetOs};
use std::{
    fmt::{Display, Formatter},
    path::{Path, PathBuf},
    process::{Command, ExitStatus, Stdio},
    sync::{Mutex, OnceLock},
    time::{Duration, Instant},
};
use thiserror::Error;

/// Filesystem and target policy selected by the command-line frontend.
#[derive(Clone, Debug)]
pub struct BuildOptions {
    pub build_dir: PathBuf,
    pub runtime_dir: PathBuf,
    pub architecture: TargetArchitecture,
    pub operating_system: TargetOs,
}

impl BuildOptions {
    pub fn new(
        build_dir: PathBuf, runtime_dir: PathBuf, architecture: TargetArchitecture,
        operating_system: TargetOs,
    ) -> Self {
        Self { build_dir, runtime_dir, architecture, operating_system }
    }

    pub fn host(build_dir: PathBuf, runtime_dir: PathBuf) -> Result<Self, NativeError> {
        let architecture =
            TargetArchitecture::host().map_err(NativeError::UnsupportedHostArchitecture)?;
        let operating_system =
            TargetOs::host().map_err(NativeError::UnsupportedHostOperatingSystem)?;
        Ok(Self::new(build_dir, runtime_dir, architecture, operating_system))
    }

    pub(crate) fn prepare(&self) -> Result<(), NativeError> {
        std::fs::create_dir_all(&self.build_dir).map_err(NativeError::PrepareBuildDirectory)?;
        std::fs::read_dir(&self.runtime_dir)
            .map_err(NativeError::ReadRuntimeDirectory)?
            .map(|entry| entry.map_err(NativeError::ReadRuntimeDirectory))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .filter(|entry| entry.path().is_file())
            .try_for_each(|entry| {
                std::fs::copy(entry.path(), self.build_dir.join(entry.file_name()))
                    .map(|_| ())
                    .map_err(NativeError::CopyRuntimeFile)
            })?;

        let manifest_path = self.build_dir.join("Cargo.toml");
        let mut manifest = std::fs::read_to_string(&manifest_path)
            .map_err(NativeError::PackageRuntimeModel)?
            .parse::<toml_edit::DocumentMut>()
            .map_err(NativeError::RuntimeManifest)?;
        for (crate_name, directory_name) in
            [("zydeco-machine", "machine"), ("zydeco-abi", "abi"), ("zydeco-host", "host")]
        {
            let inherits_crate = manifest
                .get("dependencies")
                .and_then(|item| item.get(crate_name))
                .and_then(|item| item.get("workspace"))
                .and_then(toml_edit::Item::as_bool);
            if inherits_crate != Some(true) {
                return Err(NativeError::RuntimeModelDependency);
            }
            let dependency = manifest
                .get_mut("workspace")
                .and_then(|item| item.get_mut("dependencies"))
                .and_then(|item| item.get_mut(crate_name))
                .and_then(toml_edit::Item::as_table_like_mut)
                .ok_or(NativeError::RuntimeModelDependency)?;
            dependency.insert("path", toml_edit::value(directory_name));
        }

        // Use the compiler's embedded sources, including when installed outside this
        // repository. Remove previous generated sources so deleted bundle files cannot
        // influence the next build's identity.
        Self::unpack_bundle(
            &self.build_dir.join("machine"),
            zydeco_machine::bundle::FILES.iter().map(|source| (source.path, source.contents)),
        )?;
        Self::unpack_bundle(
            &self.build_dir.join("abi"),
            zydeco_abi::bundle::FILES.iter().map(|source| (source.path, source.contents)),
        )?;
        Self::unpack_bundle(
            &self.build_dir.join("host"),
            zydeco_host::bundle::FILES.iter().map(|source| (source.path, source.contents)),
        )?;
        std::fs::write(manifest_path, manifest.to_string())
            .map_err(NativeError::PackageRuntimeModel)
    }

    fn unpack_bundle<'a>(
        target_dir: &Path, entries: impl Iterator<Item = (&'a str, &'a str)>,
    ) -> Result<(), NativeError> {
        if target_dir.exists() {
            std::fs::remove_dir_all(target_dir).map_err(NativeError::PackageRuntimeModel)?;
        }
        for (relative, contents) in entries {
            let path = target_dir.join(relative);
            std::fs::create_dir_all(path.parent().unwrap())
                .map_err(NativeError::PackageRuntimeModel)?;
            std::fs::write(path, contents).map_err(NativeError::PackageRuntimeModel)?;
        }
        Ok(())
    }

    pub fn link_amd64(
        &self, artifact: &str, assembly: &str,
        foreign_libraries: &[zydeco_syntax::ForeignLibraryName],
    ) -> Result<Executable, NativeError> {
        self.link_amd64_resolved(
            artifact,
            assembly,
            foreign_libraries,
            &crate::library::LinkedLibraries::default(),
        )
    }

    pub fn link_amd64_resolved(
        &self, artifact: &str, assembly: &str,
        foreign_libraries: &[zydeco_syntax::ForeignLibraryName],
        libraries: &crate::library::LinkedLibraries,
    ) -> Result<Executable, NativeError> {
        if self.architecture != TargetArchitecture::X86_64 {
            return Err(NativeError::UnsupportedAmd64Architecture(self.architecture));
        }
        self.prepare()?;

        let (nasm_format, cargo_target) = match self.operating_system {
            | TargetOs::Linux => ("elf64", "x86_64-unknown-linux-gnu"),
            | TargetOs::Macos => ("macho64", "x86_64-apple-darwin"),
        };
        let library = format!("zy{artifact}");
        let assembly_path = self.build_dir.join(format!("{artifact}.s"));
        let object_path = self.build_dir.join(format!("{artifact}.o"));
        let library_path =
            self.build_dir.join(format!("{}{library}.a", std::env::consts::DLL_PREFIX));
        let executable_path = self.build_dir.join(format!("{artifact}.exe"));

        std::fs::write(&assembly_path, assembly).map_err(NativeError::WriteBackendOutput)?;
        NativeTool::Nasm.run(
            Command::new("nasm")
                .arg("-f")
                .arg(nasm_format)
                .arg("-o")
                .arg(&object_path)
                .arg(&assembly_path),
        )?;
        libraries.bundle_raw(&object_path, self.operating_system)?;
        NativeTool::Archive
            .run(Command::new("ar").arg("crs").arg(&library_path).arg(&object_path))?;

        let mut rustc = Command::new("rustc");
        let cached = self.ensure_cached_runtime(&self.build_dir, cargo_target)?;
        let staged_executable = self.build_dir.join(format!(".{artifact}.main.exe"));
        rustc
            .arg(self.build_dir.join("main.rs"))
            .args([
                "--crate-name",
                "main",
                "--crate-type",
                "bin",
                "--edition",
                "2024",
                "--target",
                cargo_target,
                "-C",
                "opt-level=0",
                "-C",
                "panic=abort",
                "-C",
                "lto=off",
            ])
            .arg("-L")
            .arg(format!("dependency={}", cached.dir.display()))
            .arg("--extern")
            .arg(format!("zydeco_runtime={}", cached.rlib.display()))
            .arg("-L")
            .arg(format!("native={}", self.build_dir.display()))
            .arg("-l")
            .arg(format!("static={library}"));
        for foreign in foreign_libraries.iter().filter(|foreign| !libraries.contains(foreign)) {
            rustc.arg("-l").arg(format!("dylib={}", foreign.as_str()));
        }
        for flag in libraries.rust_flags() {
            rustc.arg(flag);
        }
        rustc.arg("-o").arg(&staged_executable);
        if let Err(error) = NativeTool::Rustc.run(&mut rustc) {
            let _ = std::fs::remove_file(&staged_executable);
            return Err(error);
        }

        Self::publish_executable(&staged_executable, &executable_path)?;
        Ok(Executable { path: executable_path })
    }

    pub(crate) fn ensure_cached_runtime(
        &self, prepared_dir: &Path, target_triple: &str,
    ) -> Result<CachedRuntime, NativeError> {
        let runtime_digest = crate::library::LibraryDigest::runtime(&self.runtime_dir)
            .map_err(NativeError::ReadRuntimeDirectory)?;
        let compiler_version = rustc_version()?;
        let cache_key = crate::library::LibraryDigest::bytes(
            format!("{runtime_digest}\0{target_triple}\0{compiler_version}").as_bytes(),
        );
        let cache_root = runtime_cache_root();
        let entry_dir = cache_root.join(&cache_key);
        let cached = CachedRuntime::at(entry_dir.clone());
        if cached.is_ready() {
            return Ok(cached);
        }

        static RUNTIME_CACHE_LOCK: Mutex<()> = Mutex::new(());
        let _thread_guard = RUNTIME_CACHE_LOCK.lock().unwrap_or_else(|poison| poison.into_inner());
        if cached.is_ready() {
            return Ok(cached);
        }

        std::fs::create_dir_all(&cache_root).map_err(NativeError::PrepareBuildDirectory)?;
        let lock_dir = cache_root.join(format!(".{cache_key}.lock"));
        let _process_guard = acquire_cache_lock(&lock_dir, || cached.is_ready())?;
        if cached.is_ready() {
            return Ok(cached);
        }

        let stage_dir = lock_dir.join("stage");
        let machine_out = stage_dir.join("machine_out");
        std::fs::create_dir_all(&machine_out).map_err(NativeError::PrepareBuildDirectory)?;
        Self::write_machine_contract(&machine_out)?;

        let machine_rlib = stage_dir.join("libzydeco_machine.rlib");
        Self::compile_runtime_crate(
            &prepared_dir.join("machine/src/lib.rs"),
            "zydeco_machine",
            "rlib",
            target_triple,
            &["feature=\"runtime\""],
            &[],
            Some(("OUT_DIR", &machine_out)),
            None,
            Some("pic"),
            &machine_rlib,
        )?;

        let abi_rlib = stage_dir.join("libzydeco_abi.rlib");
        Self::compile_runtime_crate(
            &prepared_dir.join("abi/src/lib.rs"),
            "zydeco_abi",
            "rlib",
            target_triple,
            &["feature=\"std\""],
            &[("zydeco_machine", &machine_rlib)],
            None,
            None,
            Some("pic"),
            &abi_rlib,
        )?;

        let host_rlib = stage_dir.join("libzydeco_host.rlib");
        Self::compile_runtime_crate(
            &prepared_dir.join("host/src/lib.rs"),
            "zydeco_host",
            "rlib",
            target_triple,
            &["feature=\"std\""],
            &[("zydeco_machine", &machine_rlib), ("zydeco_abi", &abi_rlib)],
            None,
            None,
            Some("pic"),
            &host_rlib,
        )?;

        let runtime_externs = [
            ("zydeco_machine", machine_rlib.as_path()),
            ("zydeco_abi", abi_rlib.as_path()),
            ("zydeco_host", host_rlib.as_path()),
        ];
        let runtime_rlib = stage_dir.join("libzydeco_runtime.rlib");
        Self::compile_runtime_crate(
            &prepared_dir.join("stub.rs"),
            "zydeco_runtime",
            "rlib",
            target_triple,
            &["feature=\"process-entry\""],
            &runtime_externs,
            None,
            Some(&stage_dir),
            Some("pic"),
            &runtime_rlib,
        )?;

        let runtime_staticlib = stage_dir.join("libzydeco_runtime.a");
        let mut staticlib_cmd = Self::rustc_runtime_command(
            &prepared_dir.join("stub.rs"),
            "zydeco_runtime",
            "staticlib",
            target_triple,
            &[],
            &runtime_externs,
            None,
            Some(&stage_dir),
            Some("pic"),
            &runtime_staticlib,
        );
        staticlib_cmd
            .arg("--remap-path-prefix")
            .arg(format!("{}=/zydeco-runtime", prepared_dir.display()));
        NativeTool::Rustc.run(&mut staticlib_cmd)?;

        std::fs::create_dir_all(&entry_dir).map_err(NativeError::PrepareBuildDirectory)?;
        for (source, destination) in [
            (&machine_rlib, &cached.machine_rlib),
            (&abi_rlib, &cached.abi_rlib),
            (&host_rlib, &cached.host_rlib),
            (&runtime_rlib, &cached.rlib),
            (&runtime_staticlib, &cached.staticlib),
        ] {
            std::fs::copy(source, destination).map_err(NativeError::CopyRuntimeFile)?;
        }
        std::fs::write(&cached.ready_marker, b"ok").map_err(NativeError::PrepareBuildDirectory)?;
        Ok(cached)
    }

    pub(crate) fn ensure_cached_wasm_runtime(&self) -> Result<CachedWasmRuntime, NativeError> {
        let mut digest_input = format!("wasm32-wasip1\0{}", rustc_version()?).into_bytes();
        let mut record_bundle = |path: &str, contents: &str| {
            digest_input.extend_from_slice(path.as_bytes());
            digest_input.push(0);
            digest_input.extend_from_slice(contents.as_bytes());
            digest_input.push(0);
        };
        for source in zydeco_machine::bundle::FILES {
            record_bundle(source.path, source.contents);
        }
        for source in zydeco_abi::bundle::FILES {
            record_bundle(source.path, source.contents);
        }
        for source in zydeco_host::bundle::FILES {
            record_bundle(source.path, source.contents);
        }
        let cache_key = crate::library::LibraryDigest::bytes(&digest_input);
        let cache_root = runtime_cache_root().join("wasm32-wasip1");
        let entry_dir = cache_root.join(&cache_key);
        let cached = CachedWasmRuntime::at(entry_dir.clone());
        if cached.is_ready() {
            return Ok(cached);
        }

        static WASM_CACHE_LOCK: Mutex<()> = Mutex::new(());
        let _thread_guard = WASM_CACHE_LOCK.lock().unwrap_or_else(|poison| poison.into_inner());
        if cached.is_ready() {
            return Ok(cached);
        }

        std::fs::create_dir_all(&cache_root).map_err(NativeError::PrepareBuildDirectory)?;
        let lock_dir = cache_root.join(format!(".{cache_key}.lock"));
        let _process_guard = acquire_cache_lock(&lock_dir, || cached.is_ready())?;
        if cached.is_ready() {
            return Ok(cached);
        }

        let stage_dir = lock_dir.join("stage");
        let src_dir = stage_dir.join("src");
        Self::unpack_bundle(
            &src_dir.join("machine"),
            zydeco_machine::bundle::FILES.iter().map(|source| (source.path, source.contents)),
        )?;
        Self::unpack_bundle(
            &src_dir.join("abi"),
            zydeco_abi::bundle::FILES.iter().map(|source| (source.path, source.contents)),
        )?;
        Self::unpack_bundle(
            &src_dir.join("host"),
            zydeco_host::bundle::FILES.iter().map(|source| (source.path, source.contents)),
        )?;

        let machine_out = stage_dir.join("machine_out");
        std::fs::create_dir_all(&machine_out).map_err(NativeError::PrepareBuildDirectory)?;
        Self::write_machine_contract(&machine_out)?;

        let machine_rlib = stage_dir.join("libzydeco_machine.rlib");
        Self::compile_runtime_crate(
            &src_dir.join("machine/src/lib.rs"),
            "zydeco_machine",
            "rlib",
            "wasm32-wasip1",
            &[],
            &[],
            Some(("OUT_DIR", &machine_out)),
            None,
            None,
            &machine_rlib,
        )?;

        let abi_rlib = stage_dir.join("libzydeco_abi.rlib");
        Self::compile_runtime_crate(
            &src_dir.join("abi/src/lib.rs"),
            "zydeco_abi",
            "rlib",
            "wasm32-wasip1",
            &["feature=\"std\""],
            &[("zydeco_machine", &machine_rlib)],
            None,
            None,
            None,
            &abi_rlib,
        )?;

        let host_rlib = stage_dir.join("libzydeco_host.rlib");
        Self::compile_runtime_crate(
            &src_dir.join("host/src/lib.rs"),
            "zydeco_host",
            "rlib",
            "wasm32-wasip1",
            &["feature=\"std\"", "feature=\"wasm-runtime\""],
            &[("zydeco_machine", &machine_rlib), ("zydeco_abi", &abi_rlib)],
            None,
            None,
            None,
            &host_rlib,
        )?;

        std::fs::create_dir_all(&entry_dir).map_err(NativeError::PrepareBuildDirectory)?;
        for (source, destination) in [
            (&machine_rlib, &cached.machine_rlib),
            (&abi_rlib, &cached.abi_rlib),
            (&host_rlib, &cached.host_rlib),
        ] {
            std::fs::copy(source, destination).map_err(NativeError::CopyRuntimeFile)?;
        }
        std::fs::write(&cached.ready_marker, b"ok").map_err(NativeError::PrepareBuildDirectory)?;
        Ok(cached)
    }

    fn write_machine_contract(machine_out: &Path) -> Result<(), NativeError> {
        let entry_symbol = zydeco_machine::native::ENTRY_SYMBOL;
        let export_entry_symbol = zydeco_machine::native::EXPORT_ENTRY_SYMBOL;
        let contract = format!(
            "pub const ENTRY_SYMBOL: &str = {entry_symbol:?};\n\
             #[cfg(feature = \"runtime\")]\nunsafe extern \"sysv64\" {{\n\
             #[link_name = {:?}]\n\
             pub fn entry() -> Word;\n}}\n\
             pub const EXPORT_ENTRY_SYMBOL: &str = {export_entry_symbol:?};\n\
             #[macro_export]\nmacro_rules! export_native_entry {{ ($item:item) => {{\n\
             #[unsafe(export_name = {:?})] $item\n}}; }}\n",
            format!("\x01{entry_symbol}"),
            format!("\x01{export_entry_symbol}"),
        );
        std::fs::write(machine_out.join("contract.rs"), contract)
            .map_err(NativeError::PrepareBuildDirectory)
    }

    #[allow(clippy::too_many_arguments)]
    fn rustc_runtime_command(
        root_source: &Path, crate_name: &str, crate_type: &str, target_triple: &str,
        cfg_flags: &[&str], externs: &[(&str, &Path)], env_dir: Option<(&str, &Path)>,
        dependency_dir: Option<&Path>, relocation_model: Option<&str>, output: &Path,
    ) -> Command {
        let mut command = Command::new("rustc");
        command.arg(root_source).args([
            "--crate-name",
            crate_name,
            "--crate-type",
            crate_type,
            "--edition",
            "2024",
            "--target",
            target_triple,
            "-C",
            "opt-level=2",
            "-C",
            "codegen-units=1",
            "-C",
            "panic=abort",
            "-C",
            "lto=off",
        ]);
        if let Some(model) = relocation_model {
            command.args(["-C", &format!("relocation-model={model}")]);
        }
        for cfg in cfg_flags {
            command.args(["--cfg", cfg]);
        }
        if let Some(dir) = dependency_dir {
            command.arg("-L").arg(format!("dependency={}", dir.display()));
        }
        for (name, path) in externs {
            command.arg("--extern").arg(format!("{name}={}", path.display()));
        }
        if let Some((key, value)) = env_dir {
            command.env(key, value);
        }
        command.arg("-o").arg(output);
        command
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_runtime_crate(
        root_source: &Path, crate_name: &str, crate_type: &str, target_triple: &str,
        cfg_flags: &[&str], externs: &[(&str, &Path)], env_dir: Option<(&str, &Path)>,
        dependency_dir: Option<&Path>, relocation_model: Option<&str>, output: &Path,
    ) -> Result<(), NativeError> {
        let mut command = Self::rustc_runtime_command(
            root_source,
            crate_name,
            crate_type,
            target_triple,
            cfg_flags,
            externs,
            env_dir,
            dependency_dir,
            relocation_model,
            output,
        );
        NativeTool::Rustc.run(&mut command)
    }

    fn publish_executable(source: &Path, destination: &Path) -> Result<(), NativeError> {
        // Keep executable writes inside child processes. Copying here would open the destination
        // for writing in this multithreaded process, allowing another concurrent fork to inherit
        // the descriptor briefly and make Linux reject an immediate exec with ETXTBSY.
        std::fs::rename(source, destination).map_err(NativeError::PublishExecutable)?;
        Ok(())
    }

    /// Link a relocatable `wasm-sps` object file with `zydeco-host` (`wasm32-wasip1`) into
    /// a standalone WASI executable whose imports are strictly `wasi_snapshot_preview1`.
    pub fn link_wasm(&self, artifact: &str, object: &[u8]) -> Result<WasmArtifact, NativeError> {
        std::fs::create_dir_all(&self.build_dir).map_err(NativeError::PrepareBuildDirectory)?;
        let cached = self.ensure_cached_wasm_runtime()?;
        let stage =
            tempfile::tempdir_in(&self.build_dir).map_err(NativeError::PrepareBuildDirectory)?;
        let obj_path = stage.path().join(format!("{artifact}.sps.o"));
        let main_rs = stage.path().join("wasm_main.rs");
        std::fs::write(&obj_path, object).map_err(NativeError::WriteBackendOutput)?;
        std::fs::write(
            &main_rs,
            "unsafe extern \"C\" {\n    fn zydeco_wasm_entry();\n}\n\
             fn main() {\n    zydeco_host::wasm::run_process(zydeco_wasm_entry);\n}\n",
        )
        .map_err(NativeError::WriteBackendOutput)?;

        let staged_wasm = self.build_dir.join(format!(".{artifact}.main.sps.wasm"));
        let final_wasm = self.build_dir.join(format!("{artifact}.sps.wasm"));
        let mut rustc = Command::new("rustc");
        rustc
            .arg(&main_rs)
            .args([
                "--crate-name",
                "main",
                "--crate-type",
                "bin",
                "--edition",
                "2024",
                "--target",
                "wasm32-wasip1",
                "-C",
                "opt-level=2",
                "-C",
                "panic=abort",
                "-C",
                "lto=off",
                "-C",
                "target-feature=+tail-call",
                "-C",
                "link-arg=--no-check-features",
            ])
            .arg("-C")
            .arg(format!("link-arg={}", obj_path.display()))
            .arg("-L")
            .arg(format!("dependency={}", cached.dir.display()))
            .arg("--extern")
            .arg(format!("zydeco_host={}", cached.host_rlib.display()))
            .arg("-o")
            .arg(&staged_wasm);
        if let Err(error) = NativeTool::Rustc.run(&mut rustc) {
            let _ = std::fs::remove_file(&staged_wasm);
            return Err(error);
        }
        Self::publish_executable(&staged_wasm, &final_wasm)?;
        Ok(WasmArtifact { path: final_wasm })
    }
}

pub(crate) struct CachedRuntime {
    pub(crate) dir: PathBuf,
    pub(crate) machine_rlib: PathBuf,
    pub(crate) abi_rlib: PathBuf,
    pub(crate) host_rlib: PathBuf,
    pub(crate) rlib: PathBuf,
    pub(crate) staticlib: PathBuf,
    ready_marker: PathBuf,
}

impl CachedRuntime {
    fn at(dir: PathBuf) -> Self {
        Self {
            machine_rlib: dir.join("libzydeco_machine.rlib"),
            abi_rlib: dir.join("libzydeco_abi.rlib"),
            host_rlib: dir.join("libzydeco_host.rlib"),
            rlib: dir.join("libzydeco_runtime.rlib"),
            staticlib: dir.join("libzydeco_runtime.a"),
            ready_marker: dir.join(".ready"),
            dir,
        }
    }

    fn is_ready(&self) -> bool {
        self.ready_marker.is_file()
            && self.machine_rlib.is_file()
            && self.abi_rlib.is_file()
            && self.host_rlib.is_file()
            && self.rlib.is_file()
            && self.staticlib.is_file()
    }
}

pub(crate) struct CachedWasmRuntime {
    pub(crate) dir: PathBuf,
    pub(crate) machine_rlib: PathBuf,
    pub(crate) abi_rlib: PathBuf,
    pub(crate) host_rlib: PathBuf,
    ready_marker: PathBuf,
}

impl CachedWasmRuntime {
    fn at(dir: PathBuf) -> Self {
        Self {
            machine_rlib: dir.join("libzydeco_machine.rlib"),
            abi_rlib: dir.join("libzydeco_abi.rlib"),
            host_rlib: dir.join("libzydeco_host.rlib"),
            ready_marker: dir.join(".ready"),
            dir,
        }
    }

    fn is_ready(&self) -> bool {
        self.ready_marker.is_file()
            && self.machine_rlib.is_file()
            && self.abi_rlib.is_file()
            && self.host_rlib.is_file()
    }
}

struct CacheLockGuard(PathBuf);

impl Drop for CacheLockGuard {
    fn drop(&mut self) {
        if !self.0.as_os_str().is_empty() {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }
}

fn runtime_cache_root() -> PathBuf {
    std::env::var_os("ZYDECO_RUNTIME_CACHE_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| std::env::temp_dir().join("zydeco-runtime-cache-v2"))
}

fn acquire_cache_lock(
    lock_dir: &Path, is_ready: impl Fn() -> bool,
) -> Result<CacheLockGuard, NativeError> {
    let deadline = Instant::now() + Duration::from_secs(180);
    loop {
        match std::fs::create_dir(lock_dir) {
            | Ok(()) => return Ok(CacheLockGuard(lock_dir.to_path_buf())),
            | Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {
                if is_ready() || Instant::now() >= deadline {
                    if is_ready() {
                        // Return a dummy non-locked guard path so the caller sees `is_ready()`.
                        return Ok(CacheLockGuard(PathBuf::new()));
                    }
                    let _ = std::fs::remove_dir_all(lock_dir);
                    continue;
                }
                std::thread::sleep(Duration::from_millis(20));
            }
            | Err(error) => return Err(NativeError::PrepareBuildDirectory(error)),
        }
    }
}

fn rustc_version() -> Result<&'static str, NativeError> {
    static VERSION: OnceLock<String> = OnceLock::new();
    if let Some(version) = VERSION.get() {
        return Ok(version.as_str());
    }
    let output = Command::new("rustc")
        .args(["-v", "-V"])
        .output()
        .map_err(|source| NativeError::StartTool { tool: NativeTool::Rustc, source })?;
    if !output.status.success() {
        return Err(NativeError::ToolFailed {
            tool: NativeTool::Rustc,
            status: output.status,
            stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
        });
    }
    let version = String::from_utf8_lossy(&output.stdout).into_owned();
    Ok(VERSION.get_or_init(|| version).as_str())
}

#[cfg(all(test, unix))]
mod tests {
    use super::{BuildOptions, NativeError};
    use crate::{TargetArchitecture, TargetOs};
    use std::os::unix::fs::MetadataExt;

    #[test]
    fn publishes_an_executable_by_moving_its_inode() {
        let directory = tempfile::tempdir().unwrap();
        let source = directory.path().join("cargo-output");
        let destination = directory.path().join("program.exe");
        std::fs::write(&source, "new executable").unwrap();
        std::fs::write(&destination, "old executable").unwrap();
        let source_inode = source.metadata().unwrap().ino();

        BuildOptions::publish_executable(&source, &destination).unwrap();

        assert!(!source.exists());
        assert_eq!(destination.metadata().unwrap().ino(), source_inode);
        assert_eq!(std::fs::read_to_string(destination).unwrap(), "new executable");
    }

    #[test]
    fn packages_a_self_contained_model_and_removes_superseded_sources() {
        let directory = tempfile::tempdir().unwrap();
        let runtime = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../runtime");
        let options = BuildOptions::new(
            directory.path().to_path_buf(),
            runtime,
            TargetArchitecture::X86_64,
            TargetOs::Linux,
        );
        let obsolete = directory.path().join("machine/src/obsolete.rs");
        std::fs::create_dir_all(obsolete.parent().unwrap()).unwrap();
        std::fs::write(&obsolete, "obsolete model source").unwrap();

        options.prepare().unwrap();

        assert!(!obsolete.exists());
        let manifest = std::fs::read_to_string(directory.path().join("Cargo.toml"))
            .unwrap()
            .parse::<toml_edit::DocumentMut>()
            .unwrap();
        assert_eq!(
            manifest["workspace"]["dependencies"]["zydeco-machine"]["path"].as_str(),
            Some("machine"),
        );
        assert_eq!(
            manifest["workspace"]["dependencies"]["zydeco-abi"]["path"].as_str(),
            Some("abi")
        );
        assert_eq!(
            manifest["workspace"]["dependencies"]["zydeco-host"]["path"].as_str(),
            Some("host"),
        );
        for source in zydeco_machine::bundle::FILES {
            assert_eq!(
                std::fs::read_to_string(directory.path().join("machine").join(source.path))
                    .unwrap(),
                source.contents,
            );
        }
        for source in zydeco_abi::bundle::FILES {
            assert_eq!(
                std::fs::read_to_string(directory.path().join("abi").join(source.path)).unwrap(),
                source.contents,
            );
        }
        for source in zydeco_host::bundle::FILES {
            assert_eq!(
                std::fs::read_to_string(directory.path().join("host").join(source.path)).unwrap(),
                source.contents,
            );
        }
        for subdir in ["machine", "abi", "host"] {
            let manifest =
                std::fs::read_to_string(directory.path().join(subdir).join("Cargo.toml"))
                    .unwrap()
                    .parse::<toml_edit::DocumentMut>()
                    .unwrap();
            assert!(manifest["package"]["version"].is_str());
            assert!(manifest["package"]["edition"].is_str());
        }
        let model = std::fs::read_to_string(directory.path().join("machine/Cargo.toml"))
            .unwrap()
            .parse::<toml_edit::DocumentMut>()
            .unwrap();
        assert!(!model.contains_key("dependencies"));
    }

    #[test]
    fn rejects_runtime_manifests_without_an_inherited_model_dependency() {
        let directory = tempfile::tempdir().unwrap();
        let runtime = directory.path().join("runtime");
        std::fs::create_dir_all(&runtime).unwrap();
        for manifest in [
            "[workspace]\n[dependencies]\n",
            "[workspace.dependencies]\nzydeco-machine = '0.3'\n\
             [dependencies]\nzydeco-machine = { workspace = true }\n",
        ] {
            std::fs::write(runtime.join("Cargo.toml"), manifest).unwrap();
            let build = directory.path().join("build");
            let options = BuildOptions::new(
                build.clone(),
                runtime.clone(),
                TargetArchitecture::X86_64,
                TargetOs::Linux,
            );
            assert!(matches!(options.prepare(), Err(NativeError::RuntimeModelDependency)));
            assert!(!build.join("machine").exists());
        }
    }
}

/// A WebAssembly module ready to instantiate with the Zydeco host imports.
#[derive(Clone, Debug)]
pub struct WasmArtifact {
    path: PathBuf,
}

impl WasmArtifact {
    pub fn path(&self) -> &Path {
        &self.path
    }
}

/// A native artifact ready to execute with inherited terminal streams.
#[derive(Clone, Debug)]
pub struct Executable {
    path: PathBuf,
}

impl Executable {
    pub fn exit_code(status: ExitStatus) -> i32 {
        if let Some(code) = status.code() {
            return code;
        }
        #[cfg(unix)]
        {
            use std::os::unix::process::ExitStatusExt;
            if let Some(signal) = status.signal() {
                return 128 + signal;
            }
        }
        1
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn run(&self, arguments: &[String]) -> Result<ExitStatus, NativeError> {
        Command::new(&self.path)
            .args(arguments)
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .status()
            .map_err(NativeError::RunExecutable)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum NativeTool {
    Nasm,
    Archive,
    Cargo,
    Rustc,
    Linker,
}

impl NativeTool {
    pub(crate) fn run(self, command: &mut Command) -> Result<(), NativeError> {
        let output =
            command.output().map_err(|source| NativeError::StartTool { tool: self, source })?;
        if output.status.success() {
            Ok(())
        } else {
            Err(NativeError::ToolFailed {
                tool: self,
                status: output.status,
                stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
            })
        }
    }
}

impl Display for NativeTool {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            | Self::Nasm => "nasm",
            | Self::Archive => "ar",
            | Self::Cargo => "cargo",
            | Self::Rustc => "rustc",
            | Self::Linker => "cc",
        })
    }
}

#[derive(Debug, Error)]
pub enum NativeError {
    #[error("unsupported host architecture `{0}`")]
    UnsupportedHostArchitecture(&'static str),
    #[error("unsupported host operating system `{0}`")]
    UnsupportedHostOperatingSystem(&'static str),
    #[error(
        "the AMD64 backend cannot target {0}; the architecture defaults to the host, so pass `--target-arch x86-64`"
    )]
    UnsupportedAmd64Architecture(TargetArchitecture),
    #[error("cannot prepare the build directory: {0}")]
    PrepareBuildDirectory(#[source] std::io::Error),
    #[error("cannot read the runtime directory: {0}")]
    ReadRuntimeDirectory(#[source] std::io::Error),
    #[error("cannot copy a runtime file: {0}")]
    CopyRuntimeFile(#[source] std::io::Error),
    #[error("cannot package the compiler's runtime model: {0}")]
    PackageRuntimeModel(#[source] std::io::Error),
    #[error("invalid runtime Cargo manifest: {0}")]
    RuntimeManifest(#[source] toml_edit::TomlError),
    #[error("runtime manifest must inherit zydeco-machine from its workspace dependencies")]
    RuntimeModelDependency,
    #[error("cannot write backend output: {0}")]
    WriteBackendOutput(#[source] std::io::Error),
    #[error("cannot start {tool}: {source}")]
    StartTool {
        tool: NativeTool,
        #[source]
        source: std::io::Error,
    },
    #[error("{tool} exited with {status}:\n{stderr}")]
    ToolFailed { tool: NativeTool, status: ExitStatus, stderr: String },
    #[error("cannot publish the linked executable: {0}")]
    PublishExecutable(#[source] std::io::Error),
    #[error("cannot run the executable: {0}")]
    RunExecutable(#[source] std::io::Error),
    #[error("build --execute is not supported for WebAssembly; use run --target wasm-sps")]
    WasmBuildExecution,
}
