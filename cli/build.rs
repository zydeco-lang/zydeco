use clap::{CommandFactory, ValueEnum};
use clap_complete::{generate_to, Shell};
use std::io;

include!("src/cli.rs");

fn main() -> Result<(), io::Error> {
    let mut app = Cli::command();

    let outdir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("completions");
    for &shell in Shell::value_variants() {
        generate_to(shell, &mut app, "zydeco", &outdir)?;
    }

    Ok(())
}
