use clap::CommandFactory;
use clap::ValueEnum as _;
use clap_complete::{Shell, generate_to};
use std::io;

include!("src/cli.rs");

fn main() -> Result<(), io::Error> {
    // The script writes tracked completion files inside this package, so the
    // default "any package file changed" heuristic would make it rerun on
    // every build and rebuild this crate and its dependents forever.
    // Declare the actual inputs instead; completions still regenerate when the
    // CLI definition (or this script) changes.
    println!("cargo::rerun-if-changed=build.rs");
    println!("cargo::rerun-if-changed=src/cli.rs");

    let mut app = Cli::command();

    let outdir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("completions");
    for &shell in Shell::value_variants() {
        generate_to(shell, &mut app, "zydeco", &outdir)?;
    }

    Ok(())
}
