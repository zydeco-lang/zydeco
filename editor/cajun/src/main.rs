use cajun::Cajun;
use clap::Parser;
use tower_lsp::{LspService, Server};

#[derive(Parser)]
#[command(version, about = "Language Server Protocol implementation for Zydeco")]
struct Arguments {
    #[arg(long, hide = true)]
    documentation_example_worker: bool,
}

#[tokio::main]
async fn main() {
    let arguments = Arguments::parse();
    if arguments.documentation_example_worker {
        if let Err(error) = zydeco_session::source::DocumentationExampleWorker::serve() {
            eprintln!("documentation worker failed: {error}");
            std::process::exit(1);
        }
        return;
    }
    let (service, socket) = LspService::build(Cajun::new)
        .custom_method("zydeco/documentation", Cajun::documentation)
        .custom_method("zydeco/checkDocumentationExample", Cajun::check_documentation_example)
        .finish();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    Server::new(stdin, stdout, socket).serve(service).await
}
