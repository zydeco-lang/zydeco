use cajun::Cajun;
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    env_logger::init();

    let (service, socket) = LspService::build(Cajun::new).finish();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    Server::new(stdin, stdout, socket).serve(service).await
}
