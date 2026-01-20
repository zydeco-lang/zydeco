use super::err::{LinkError, Result};
use crate::BuildConf;
use std::{
    fs::File,
    io::Write,
    path::PathBuf,
    process::{Command, ExitStatus, Stdio},
};

pub struct PackageAmd64 {
    pub name: String,
    pub assembly: String,
    pub build_conf: BuildConf,
}

impl PackageAmd64 {
    pub fn link(self) -> Result<PackageAmd64Executable> {
        let PackageAmd64 { name, assembly, build_conf } = self;
        let BuildConf { build_dir, runtime_dir, link_existing, target_arch, target_os } =
            build_conf;
        let target_arch = match target_arch.as_str() {
            | "x86" | "x86_64" | "amd64" => "x86_64",
            | _ => return Err(LinkError::UnsupportedTargetArch(target_arch)),
        };
        if !link_existing {
            // Hack: clean build dir and create it
            // Todo: make it safer by checking build profile if not nonexistent or empty
            std::fs::remove_dir_all(&build_dir).ok();
            std::fs::create_dir_all(&build_dir).expect("Failed to create build dir");
        }

        // copy everything in runtime dir to build dir
        for entry in std::fs::read_dir(&runtime_dir).expect("Failed to read runtime dir") {
            let entry = entry.expect("Failed to read entry");
            let path = entry.path();
            let target = build_dir.join(path.file_name().unwrap());
            std::fs::copy(&path, &target).expect("Failed to copy entry");
        }

        let lib_name = format!("zy{}", name);

        let (nasm_format, cargo_target) = match target_os.as_str() {
            | "linux" => ("elf64", format!("{target_arch}-unknown-linux-gnu")),
            | "macos" | "darwin" => ("macho64", format!("{target_arch}-apple-darwin")),
            | _ => return Err(LinkError::UnsupportedTargetOs(target_os)),
        };
        let lib_name_full = format!("{}{}.a", std::env::consts::DLL_PREFIX, lib_name);

        let asm_fname = build_dir.join(format!("{}.s", name));
        let obj_fname = build_dir.join(format!("{}.o", name));
        let lib_fname = build_dir.join(lib_name_full.as_str());
        let exe_fname = build_dir.join(format!("{}.exe", name));

        // remove existing files
        if !link_existing {
            std::fs::remove_file(&asm_fname).ok();
        }
        std::fs::remove_file(&obj_fname).ok();
        std::fs::remove_file(&lib_fname).ok();
        std::fs::remove_file(&exe_fname).ok();

        // first put the assembly in a new file zyprog.s
        if !link_existing {
            let mut asm_file = File::create(&asm_fname).map_err(LinkError::AssemblyWriteError)?;
            asm_file.write(assembly.as_bytes()).map_err(LinkError::AssemblyWriteError)?;
            asm_file.flush().map_err(LinkError::AssemblyWriteError)?;
        }

        // nasm -fFORMAT -o zyprog.o zyprog.s
        let nasm_out = Command::new("nasm")
            .arg("-f")
            .arg(nasm_format)
            .arg("-o")
            .arg(&obj_fname)
            .arg(&asm_fname)
            .output()
            .map_err(LinkError::NasmRunError)?;
        if !nasm_out.status.success() {
            Err(LinkError::NasmOutputError(format!(
                "{}\n{}",
                nasm_out.status,
                std::str::from_utf8(&nasm_out.stderr).expect("nasm produced invalid UTF-8")
            )))?
        }

        // ar r libzyprog.a zyprog.o
        let ar_out = Command::new("ar")
            .arg("rus")
            .arg(lib_fname)
            .arg(&obj_fname)
            .output()
            .map_err(LinkError::ArRunError)?;
        if !ar_out.status.success() {
            Err(LinkError::ArOutputError(format!(
                "{}\n{}",
                ar_out.status,
                std::str::from_utf8(&ar_out.stderr).expect("ar produced invalid UTF-8")
            )))?
        }

        // cargo build
        let mut cargo_cmd = Command::new("cargo");
        cargo_cmd
            .env("ZYDECO_STATIC_LIB", lib_name)
            .env("ZYDECO_LIB_DIR", ".")
            .arg("build")
            .arg("--manifest-path")
            .arg(build_dir.join("Cargo.toml"))
            // .arg("--release")
            // .arg("features=log_rt")
            .arg("--target")
            .arg(&cargo_target);
        if target_os == "macos" || target_os == "darwin" {
            cargo_cmd.env("RUSTFLAGS", "-C panic=abort");
        }
        let cargo_out = cargo_cmd.output().map_err(LinkError::CargoRunError)?;

        if !cargo_out.status.success() {
            Err(LinkError::CargoOutputError(format!(
                "{}\n{}",
                cargo_out.status,
                std::str::from_utf8(&cargo_out.stderr).expect("rustc produced invalid UTF-8")
            )))?
        }
        // copy the cargo generated executable to the build dir
        let cargo_exe_fname = build_dir.join(format!("target/{}/debug/main", cargo_target));
        std::fs::copy(&cargo_exe_fname, &exe_fname).map_err(LinkError::ExecutableCopyError)?;

        // Hack: avoid text file busy error on linux
        #[cfg(target_os = "linux")]
        {
            std::thread::sleep(std::time::Duration::from_millis(100));
        }

        let executable = PackageAmd64Executable { name, executable: exe_fname };
        Ok(executable)
    }
}

pub struct PackageAmd64Executable {
    pub name: String,
    pub executable: PathBuf,
}

impl PackageAmd64Executable {
    pub fn run(self) -> Result<ExitStatus> {
        let PackageAmd64Executable { name, executable } = self;
        log::info!("Running program: {}", name);
        // run the program with interactive I/O
        let mut child = Command::new(&executable)
            // .env("RUST_LOG", "trace")
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
