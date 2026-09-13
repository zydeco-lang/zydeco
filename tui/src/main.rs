fn main() {
    match zydeco_tui::Repl::launch(Default::default()) {
        | Ok(code) => std::process::exit(code),
        | Err(error) => {
            eprintln!("{error}");
            std::process::exit(1);
        }
    }
}
