use super::err::{LinkError, Result};
use crate::{BuildConf, Verbosity, backend::RuntimeFiles};
use std::{
    fs::File,
    io::Write,
    path::PathBuf,
    process::{Command, ExitStatus, Stdio},
};

pub struct PackageLlvm {
    pub name: String,
    pub ir: String,
    pub build_conf: BuildConf,
    pub verbosity: Verbosity,
}

impl PackageLlvm {
    pub fn link(self) -> Result<PackageLlvmExecutable> {
        let PackageLlvm { name, ir, build_conf, verbosity } = self;
        let BuildConf { build_dir, runtime_dir, link_existing, target_arch, target_os } =
            build_conf;

        // Determine target triple
        let llvm_target = match (target_arch.as_str(), target_os.as_str()) {
            | ("x86" | "x86_64" | "amd64", "linux") => "x86_64-linux-gnu",
            | ("x86" | "x86_64" | "amd64", "macos" | "darwin") => "x86_64-apple-darwin",
            | ("aarch64" | "arm64", "linux") => "aarch64-linux-gnu",
            | ("aarch64" | "arm64", "macos" | "darwin") => "aarch64-apple-darwin",
            | (arch, _) => return Err(LinkError::UnsupportedTargetArch(arch.to_string())),
        };

        RuntimeFiles::new(&build_dir, &runtime_dir, link_existing)
            .prepare()
            .map_err(LinkError::BuildPreparationError)?;

        // Write LLVM IR to file
        let ir_fname = build_dir.join(format!("{}.ll", name));
        let mut ir_file =
            File::create(&ir_fname).map_err(|e| LinkError::LlvmCompileError(e.to_string()))?;
        ir_file.write(ir.as_bytes()).map_err(|e| LinkError::LlvmCompileError(e.to_string()))?;
        ir_file.flush().map_err(|e| LinkError::LlvmCompileError(e.to_string()))?;

        // Try to find llvm tools
        let clang = find_llvm_tool("clang").ok_or(LinkError::LlvmNotFound)?;
        let _llvm_link = find_llvm_tool("llvm-link").ok_or(LinkError::LlvmNotFound)?;
        let _llvm_llc = find_llvm_tool("llc").ok_or(LinkError::LlvmNotFound)?;

        let obj_fname = build_dir.join(format!("{}.o", name));
        let exe_fname = build_dir.join(format!("{}.exe", name));

        // Compile IR to object file using clang
        let clang_out = Command::new(&clang)
            .arg("-target")
            .arg(llvm_target)
            .arg("-c")
            .arg(&ir_fname)
            .arg("-o")
            .arg(&obj_fname)
            .output()
            .map_err(|e| LinkError::LlvmCompileError(e.to_string()))?;

        if !clang_out.status.success() {
            let stderr = std::str::from_utf8(&clang_out.stderr).unwrap_or("unknown error");
            return Err(LinkError::LlvmCompileError(format!("clang failed: {}", stderr)));
        }

        // Link using clang
        let link_out = Command::new(&clang)
            .arg("-target")
            .arg(llvm_target)
            .arg("-o")
            .arg(&exe_fname)
            .arg(&obj_fname)
            .output()
            .map_err(|e| LinkError::LlvmCompileError(e.to_string()))?;

        if !link_out.status.success() {
            let stderr = std::str::from_utf8(&link_out.stderr).unwrap_or("unknown error");
            return Err(LinkError::LlvmCompileError(format!("linking failed: {}", stderr)));
        }

        let executable = PackageLlvmExecutable { name, executable: exe_fname, verbosity };
        Ok(executable)
    }
}

/// Try to find an LLVM tool in common locations
fn find_llvm_tool(name: &str) -> Option<String> {
    // Check if tool exists in PATH
    if Command::new(name).arg("--version").output().is_ok() {
        return Some(name.to_string());
    }

    // Try with llvm- prefix
    let prefixed = format!("llvm-{}", name);
    if Command::new(&prefixed).arg("--version").output().is_ok() {
        return Some(prefixed);
    }

    None
}

pub struct PackageLlvmExecutable {
    pub name: String,
    pub executable: PathBuf,
    pub verbosity: Verbosity,
}

impl PackageLlvmExecutable {
    pub fn run(self) -> Result<ExitStatus> {
        let PackageLlvmExecutable { name, executable, verbosity } = self;
        log::info!("Running program: {}", name);
        let mut command = Command::new(&executable);
        if verbosity.enables_runtime_trace_env() {
            command.env("RUST_LOG", "trace");
        }
        let mut child = command
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .spawn()
            .map_err(LinkError::ExecutableRunError)?;
        let status = child.wait().map_err(LinkError::ExecutableRunError)?;
        log::info!("Program exited with {}", status);
        Ok(status)
    }
}
