use std::{
    fs::File,
    io::Write,
    path::PathBuf,
    process::{Command, Stdio},
};

pub struct PackageX86 {
    pub name: String,
    pub assembly: String,
    pub build_dir: PathBuf,
    pub runtime_dir: PathBuf,
    pub link_existing: bool,
    pub execute: bool,
}

impl PackageX86 {
    pub fn link(self) -> Result<(), String> {
        let PackageX86 { name, assembly, build_dir, runtime_dir, link_existing, execute } = self;
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

        let lib_name = "zyprog";

        let (nasm_format, lib_name_full) = if cfg!(target_os = "linux") {
            ("elf64", format!("lib{}.a", lib_name))
        } else if cfg!(target_os = "macos") {
            ("macho64", format!("lib{}.a", lib_name))
        } else {
            panic!("Runner script only supports linux and macos")
        };

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
            let mut asm_file = File::create(&asm_fname).map_err(|e| e.to_string())?;
            asm_file.write(assembly.as_bytes()).map_err(|e| e.to_string())?;
            asm_file.flush().map_err(|e| e.to_string())?;
        }

        // nasm -fFORMAT -o zyprog.o zyprog.s
        let nasm_out = Command::new("nasm")
            .arg("-f")
            .arg(nasm_format)
            .arg("-o")
            .arg(&obj_fname)
            .arg(&asm_fname)
            .output()
            .map_err(|e| format!("nasm err: {}", e))?;
        if !nasm_out.status.success() {
            return Err(format!(
                "Failure in nasm call: {}\n{}",
                nasm_out.status,
                std::str::from_utf8(&nasm_out.stderr).expect("nasm produced invalid UTF-8")
            ));
        }

        // ar r libzyprog.a zyprog.o
        let ar_out = Command::new("ar")
            .arg("rus")
            .arg(lib_fname)
            .arg(&obj_fname)
            .output()
            .map_err(|e| format!("ar err: {}", e))?;
        if !ar_out.status.success() {
            return Err(format!(
                "Failure in ar call:\n{}\n{}",
                ar_out.status,
                std::str::from_utf8(&ar_out.stderr).expect("ar produced invalid UTF-8")
            ));
        }

        // rustc stub.rs -L build_dir
        // use cargo instead of rustc directly
        let rustc_out = if cfg!(target_os = "macos") {
            // Command::new("rustc")
            //     .arg("-v")
            //     .arg(stub_path)
            //     .arg("--target")
            //     .arg("x86_64-apple-darwin")
            //     .arg("-C")
            //     .arg("panic=abort")
            //     .arg("-L")
            //     .arg(build_dir)
            //     .arg("-o")
            //     .arg(&exe_fname)
            //     .output()
            //     .map_err(|e| format!("rustc err: {}", e))?

            // add "ZYDECO_STATIC_LIB" environment variable to the command
            Command::new("cargo")
                .env("ZYDECO_STATIC_LIB", lib_name)
                .env("ZYDECO_LIB_DIR", ".")
                .env("RUSTFLAGS", "-C panic=abort")
                .arg("build")
                .arg("--manifest-path")
                .arg(build_dir.join("Cargo.toml"))
                .arg("--target")
                .arg("x86_64-apple-darwin")
                // .arg("--features=log_rt")
                // .arg("--release")
                .output()
                .map_err(|e| format!("cargo err: {}", e))?
        } else {
            // Command::new("rustc")
            //     .arg(stub_path)
            //     .arg("-L")
            //     .arg(build_dir)
            //     .arg("-o")
            //     .arg(&exe_fname)
            //     .output()
            //     .map_err(|e| format!("rustc err: {}", e))?
            Command::new("cargo")
                .env("ZYDECO_STATIC_LIB", lib_name)
                .env("ZYDECO_LIB_DIR", ".")
                .arg("build")
                .arg("--manifest-path")
                .arg(build_dir.join("Cargo.toml"))
                // .arg("--features=log_rt")
                // .arg("--release")
                .output()
                .map_err(|e| format!("cargo err: {}", e))?
        };
        if !rustc_out.status.success() {
            Err(format!(
                "Failure in rustc call: {}\n{}",
                rustc_out.status,
                std::str::from_utf8(&rustc_out.stderr).expect("rustc produced invalid UTF-8")
            ))?
        }
        // copy the cargo generated executable to the build dir
        let cargo_exe_fname = if cfg!(target_os = "macos") {
            build_dir.join("target/x86_64-apple-darwin/debug/main")
        } else {
            build_dir.join("target/debug/main")
        };
        std::fs::copy(&cargo_exe_fname, &exe_fname).map_err(|e| e.to_string())?;

        if !execute {
            return Ok(());
        }

        // run the program with interactive I/O
        let mut child = Command::new(&exe_fname)
            // .env("RUST_LOG", "trace")
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .spawn()
            .map_err(|e| e.to_string())?;
        // println!("child: {:?}", child);
        let status = child.wait().map_err(|e| e.to_string())?;
        if !status.success() {
            Err(format!("Failure in program call: {}", status))?;
        }
        Ok(())
    }
}
