use cajun::Cajun;
use clap::Parser;
use tower_lsp::{LspService, Server};

#[derive(Parser)]
#[command(version, about = "Language Server Protocol implementation for Zydeco")]
struct Arguments;

#[tokio::main]
async fn main() {
    Arguments::parse();
    env_logger::init();

    let (service, socket) = LspService::build(Cajun::new).finish();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    Server::new(stdin, stdout, socket).serve(service).await
}
