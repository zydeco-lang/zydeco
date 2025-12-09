use clap::Parser;
use std::{fs::File, io::Write, path::PathBuf, process::Command};
// use zydeco_cli::{Cli, Commands, Repl};
use zydeco_cli::{Cli, Commands};
use zydeco_driver::{BuildSystem, PackId, ProgKont};

fn main() -> Result<(), ()> {
    env_logger::init();

    let res = match Cli::parse().command {
        | Commands::Run { files, bin, dry, verbose, args } => {
            run_files(files, bin, dry, verbose, args)
        }
        | Commands::Check { files, verbose } => run_files(files, None, true, verbose, vec![]),
        // | Commands::Repl { .. } => Repl::launch(),
        | Commands::Build {
            files,
            bin,
            target,
            build_dir,
            stub,
            link_existing,
            execute,
            dry,
            verbose,
        } => build_files(files, bin, target, build_dir, stub, link_existing, execute, dry, verbose),
    };
    match res {
        | Ok(x) => {
            std::process::exit(x);
        }
        | Err(e) => {
            eprintln!("{}", e);
            Ok(())
        }
    }
}

fn setup_build_system(
    paths: Vec<PathBuf>, bin: Option<String>,
) -> Result<(BuildSystem, PackId), String> {
    let mut build_sys = BuildSystem::new();
    let mut packs = Vec::new();
    let mut files = Vec::new();

    for path in paths {
        // for dir, try finding "proj.toml" under it
        if path.is_dir() {
            let proj = path.join("proj.toml");
            if proj.exists() {
                let pack = build_sys.add_local_package(proj).map_err(|e| e.to_string())?;
                packs.push(pack);
                continue;
            }
            // fallback to adding the dir itself
        }
        match path.extension() {
            | Some(ext) if ext == "toml" => {
                // package
                let pack = build_sys.add_local_package(path).map_err(|e| e.to_string())?;
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
            build_sys.add_binary_in_package(pack).map_err(|e| e.to_string())?;
        }
    } else {
        for file in files.iter() {
            let pack = build_sys.add_orphan_file(file).map_err(|e| e.to_string())?;
            build_sys.mark(pack).map_err(|e| e.to_string())?;
        }
    }

    let pack = build_sys.pick_marked(bin).map_err(|e| e.to_string())?;
    Ok((build_sys, pack))
}

fn run_files(
    paths: Vec<PathBuf>, bin: Option<String>, dry: bool, verbose: bool, args: Vec<String>,
) -> Result<i32, String> {
    let (build_sys, pack) = setup_build_system(paths, bin)?;
    match build_sys.run_pack(pack, &args, dry, verbose).map_err(|e| e.to_string())? {
        | ProgKont::Dry => Ok(0),
        | ProgKont::Ret(_) => unreachable!(),
        | ProgKont::ExitCode(code) => Ok(code),
    }
}

fn build_files(
    paths: Vec<PathBuf>, bin: Option<String>, target: String, build_dir: PathBuf, stub: PathBuf,
    link_existing: bool, execute: bool, _dry: bool, verbose: bool,
) -> Result<i32, String> {
    let (build_sys, pack) = setup_build_system(paths, bin)?;

    match target.as_str() {
        | "zir" => {
            let zir = build_sys.codegen_zir_pack(pack).map_err(|e| e.to_string())?;
            println!("{}", zir);
            Ok(0)
        }
        | "zasm" => {
            let zasm = build_sys.codegen_zasm_pack(pack).map_err(|e| e.to_string())?;
            println!("{}", zasm);
            Ok(0)
        }
        | "x86" => {
            let x86 = build_sys.codegen_x86_pack(pack).map_err(|e| e.to_string())?;
            if verbose {
                println!("{}", x86);
            }
            let name = build_sys.packages[&pack].name();
            // link with stub
            link_x86(name, x86, build_dir, stub, link_existing, execute)?;
            Ok(0)
        }
        | _ => return Err(format!("Invalid target: {}", target)),
    }
}

fn link_x86(
    name: String, assembly: String, build_dir: PathBuf, stub: PathBuf, link_existing: bool,
    execute: bool,
) -> Result<(), String> {
    if !link_existing {
        // Hack: clean build dir and create it
        // Todo: make it safer by checking build profile if not nonexistent or empty
        std::fs::remove_dir_all(&build_dir).ok();
        std::fs::create_dir_all(&build_dir).expect("Failed to create build dir");
    }

    // copy stub to build dir
    let stub_path = build_dir.join(stub.file_name().unwrap());
    std::fs::copy(&stub, &stub_path).expect("Failed to copy stub");

    let (nasm_format, lib_name) = if cfg!(target_os = "linux") {
        ("elf64", format!("libzyprog.a"))
    } else if cfg!(target_os = "macos") {
        ("macho64", format!("libzyprog.a"))
    } else {
        panic!("Runner script only supports linux and macos")
    };

    let asm_fname = build_dir.join(format!("{}.s", name));
    let obj_fname = build_dir.join(format!("{}.o", name));
    let lib_fname = build_dir.join(lib_name);
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
    let rustc_out = if cfg!(target_os = "macos") {
        Command::new("rustc")
            .arg("-v")
            .arg(stub_path)
            .arg("--target")
            .arg("x86_64-apple-darwin")
            .arg("-C")
            .arg("panic=abort")
            .arg("-L")
            .arg(build_dir)
            .arg("-o")
            .arg(&exe_fname)
            .output()
            .map_err(|e| format!("rustc err: {}", e))?
    } else {
        Command::new("rustc")
            .arg(stub_path)
            .arg("-L")
            .arg(build_dir)
            .arg("-o")
            .arg(&exe_fname)
            .output()
            .map_err(|e| format!("rustc err: {}", e))?
    };
    if !rustc_out.status.success() {
        Err(format!(
            "Failure in rustc call: {}\n{}",
            rustc_out.status,
            std::str::from_utf8(&rustc_out.stderr).expect("rustc produced invalid UTF-8")
        ))?
    }

    if !execute {
        return Ok(());
    }

    // run the program
    let output = Command::new(&exe_fname).output().map_err(|e| e.to_string())?;
    if !output.status.success() {
        Err(format!(
            "Failure in program call: {}\n{}",
            output.status,
            std::str::from_utf8(&output.stderr).expect("program produced invalid UTF-8")
        ))?;
    }
    println!("Program output:");
    print!("{}", std::str::from_utf8(&output.stdout).expect("program produced invalid UTF-8"));
    std::io::stdout().flush().map_err(|e| e.to_string())?;
    Ok(())
}
