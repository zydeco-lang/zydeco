use crate::{TargetArchitecture, TargetOs};
use std::{
    fmt::{Display, Formatter},
    path::{Path, PathBuf},
    process::{Command, ExitStatus, Stdio},
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

    fn prepare(&self) -> Result<(), NativeError> {
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
            })
    }

    pub fn link_amd64(
        &self, artifact: &str, assembly: &str,
        foreign_libraries: &[zydeco_syntax::ForeignLibraryName],
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
        NativeTool::Archive
            .run(Command::new("ar").arg("crs").arg(&library_path).arg(&object_path))?;

        let mut cargo = Command::new("cargo");
        cargo
            .env("ZYDECO_STATIC_LIB", &library)
            .env("ZYDECO_LIB_DIR", ".")
            .env(
                "ZYDECO_DYNAMIC_LIBS",
                foreign_libraries
                    .iter()
                    .map(zydeco_syntax::ForeignLibraryName::as_str)
                    .collect::<Vec<_>>()
                    .join(","),
            )
            .arg("build")
            .arg("--manifest-path")
            .arg(self.build_dir.join("Cargo.toml"))
            .arg("--target")
            .arg(cargo_target);
        if self.operating_system == TargetOs::Macos {
            cargo.env("RUSTFLAGS", "-C panic=abort");
        }
        NativeTool::Cargo.run(&mut cargo)?;

        let cargo_executable =
            self.build_dir.join("target").join(cargo_target).join("debug").join("main");
        Self::publish_executable(&cargo_executable, &executable_path)?;
        Ok(Executable { path: executable_path })
    }

    fn publish_executable(source: &Path, destination: &Path) -> Result<(), NativeError> {
        // Keep executable writes inside child processes. Copying here would open the destination
        // for writing in this multithreaded process, allowing another concurrent fork to inherit
        // the descriptor briefly and make Linux reject an immediate exec with ETXTBSY.
        std::fs::rename(source, destination).map_err(NativeError::PublishExecutable)?;
        Ok(())
    }

    pub fn link_llvm(&self, artifact: &str, ir: &str) -> Result<Executable, NativeError> {
        self.prepare()?;
        let target = match (self.architecture, self.operating_system) {
            | (TargetArchitecture::X86_64, TargetOs::Linux) => "x86_64-linux-gnu",
            | (TargetArchitecture::X86_64, TargetOs::Macos) => "x86_64-apple-darwin",
            | (TargetArchitecture::Aarch64, TargetOs::Linux) => "aarch64-linux-gnu",
            | (TargetArchitecture::Aarch64, TargetOs::Macos) => "aarch64-apple-darwin",
        };
        let ir_path = self.build_dir.join(format!("{artifact}.ll"));
        let object_path = self.build_dir.join(format!("{artifact}.o"));
        let executable_path = self.build_dir.join(format!("{artifact}.exe"));
        std::fs::write(&ir_path, ir).map_err(NativeError::WriteBackendOutput)?;
        NativeTool::Clang.run(
            Command::new("clang")
                .arg("-target")
                .arg(target)
                .arg("-c")
                .arg(&ir_path)
                .arg("-o")
                .arg(&object_path),
        )?;
        NativeTool::Clang.run(
            Command::new("clang")
                .arg("-target")
                .arg(target)
                .arg("-o")
                .arg(&executable_path)
                .arg(&object_path),
        )?;
        Ok(Executable { path: executable_path })
    }

    pub fn write_wasm(
        &self, artifact: &str, backend: WasmBackendKind, module: &[u8],
    ) -> Result<WasmArtifact, NativeError> {
        std::fs::create_dir_all(&self.build_dir).map_err(NativeError::PrepareBuildDirectory)?;
        let path = self.build_dir.join(format!("{artifact}.{}.wasm", backend.artifact_label()));
        std::fs::write(&path, module).map_err(NativeError::WriteBackendOutput)?;
        Ok(WasmArtifact { path })
    }
}

#[cfg(all(test, unix))]
mod tests {
    use super::BuildOptions;
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
}

/// WebAssembly lowering strategy used to disambiguate side-by-side artifacts.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum WasmBackendKind {
    AbstractMachine,
    SpsLow,
}

impl WasmBackendKind {
    fn artifact_label(self) -> &'static str {
        match self {
            | Self::AbstractMachine => "am",
            | Self::SpsLow => "sps",
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
    Clang,
}

impl NativeTool {
    fn run(self, command: &mut Command) -> Result<(), NativeError> {
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
            | Self::Clang => "clang",
        })
    }
}

#[derive(Debug, Error)]
pub enum NativeError {
    #[error("unsupported host architecture `{0}`")]
    UnsupportedHostArchitecture(&'static str),
    #[error("unsupported host operating system `{0}`")]
    UnsupportedHostOperatingSystem(&'static str),
    #[error("the amd64 backend cannot target {0:?}")]
    UnsupportedAmd64Architecture(TargetArchitecture),
    #[error("cannot prepare the build directory: {0}")]
    PrepareBuildDirectory(#[source] std::io::Error),
    #[error("cannot read the runtime directory: {0}")]
    ReadRuntimeDirectory(#[source] std::io::Error),
    #[error("cannot copy a runtime file: {0}")]
    CopyRuntimeFile(#[source] std::io::Error),
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
    #[error(
        "WebAssembly execution requires an embedding that implements the `zydeco` host imports"
    )]
    WasmExecutionRequiresHost,
}
