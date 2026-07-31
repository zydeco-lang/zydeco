use serde_json::{Value, json};
use std::{
    io::{BufRead, BufReader, Read, Write},
    process::{Child, ChildStdin, ChildStdout, Command, Stdio},
};
use tower_lsp::lsp_types::Url;

struct LspProcess {
    child: Child,
    input: Option<ChildStdin>,
    output: BufReader<ChildStdout>,
    next_id: u64,
}

impl LspProcess {
    fn start() -> Self {
        let mut child = Command::new(env!("CARGO_BIN_EXE_cajun"))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .unwrap();
        let input = child.stdin.take().unwrap();
        let output = BufReader::new(child.stdout.take().unwrap());
        Self { child, input: Some(input), output, next_id: 1 }
    }

    fn request(&mut self, method: &str, params: Value) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        let message = if params.is_null() {
            json!({ "jsonrpc": "2.0", "id": id, "method": method })
        } else {
            json!({ "jsonrpc": "2.0", "id": id, "method": method, "params": params })
        };
        self.send(message);
        std::iter::from_fn(|| Some(self.receive()))
            .find(|message| message.get("id") == Some(&json!(id)))
            .unwrap()
    }

    fn request_without_notifications(&mut self, method: &str, params: Value) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        self.send(json!({ "jsonrpc": "2.0", "id": id, "method": method, "params": params }));
        let response = self.receive();
        assert_eq!(response.get("id"), Some(&json!(id)), "unexpected LSP message: {response}");
        response
    }

    fn notify(&mut self, method: &str, params: Value) {
        let message = if params.is_null() {
            json!({ "jsonrpc": "2.0", "method": method })
        } else {
            json!({ "jsonrpc": "2.0", "method": method, "params": params })
        };
        self.send(message);
    }

    fn notification(&mut self, method: &str) -> Value {
        std::iter::from_fn(|| Some(self.receive()))
            .find(|message| message.get("method") == Some(&json!(method)))
            .unwrap()
    }

    fn finish(mut self) {
        let response = self.request("shutdown", Value::Null);
        assert!(response.get("error").is_none(), "shutdown failed: {response}");
        self.notify("exit", Value::Null);
        self.input.take();
        assert!(self.child.wait().unwrap().success());
    }

    fn send(&mut self, message: Value) {
        let body = serde_json::to_vec(&message).unwrap();
        let input = self.input.as_mut().unwrap();
        write!(input, "Content-Length: {}\r\n\r\n", body.len()).unwrap();
        input.write_all(&body).unwrap();
        input.flush().unwrap();
    }

    fn receive(&mut self) -> Value {
        let headers = std::iter::from_fn(|| {
            let mut header = String::new();
            let bytes = self.output.read_line(&mut header).unwrap();
            (bytes != 0 && header != "\r\n").then_some(header)
        })
        .collect::<Vec<_>>();
        let content_length = headers
            .into_iter()
            .find_map(|header| {
                header
                    .strip_prefix("Content-Length:")
                    .and_then(|value| value.trim().parse::<usize>().ok())
            })
            .expect("Cajun closed stdout before sending a complete LSP header");
        let mut body = vec![0; content_length];
        self.output.read_exact(&mut body).unwrap();
        serde_json::from_slice(&body).unwrap()
    }
}

#[test]
fn stdio_server_synchronizes_documents_and_answers_navigation_requests() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("main.zy");
    let original = "begin\n  let answer = () that\n  answer\nend\n";
    std::fs::write(&path, original).unwrap();
    let uri = Url::from_file_path(&path).unwrap().to_string();
    let mut server = LspProcess::start();

    let initialize = server.request(
        "initialize",
        json!({
            "processId": null,
            "rootUri": Url::from_file_path(directory.path()).unwrap(),
            "capabilities": {},
        }),
    );
    let capabilities = &initialize["result"]["capabilities"];
    assert_eq!(capabilities["positionEncoding"], "utf-16");
    assert_eq!(capabilities["textDocumentSync"]["openClose"], true);
    assert_eq!(capabilities["textDocumentSync"]["change"], 1);
    assert_eq!(capabilities["definitionProvider"], true);
    assert_eq!(capabilities["documentSymbolProvider"], true);
    server.notify("initialized", json!({}));

    server.notify(
        "textDocument/didOpen",
        json!({
            "textDocument": {
                "uri": uri,
                "languageId": "zydeco",
                "version": 1,
                "text": original,
            },
        }),
    );
    let diagnostics = server.notification("textDocument/publishDiagnostics");
    assert!(diagnostics["params"]["diagnostics"].as_array().unwrap().is_empty());
    let symbols =
        server.request("textDocument/documentSymbol", json!({ "textDocument": { "uri": uri } }));
    assert!(symbols["result"].as_array().unwrap().iter().any(|symbol| symbol["name"] == "answer"));

    let definition = server.request(
        "textDocument/definition",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": 2, "character": 3 },
        }),
    );
    let definition_path =
        Url::parse(definition["result"]["uri"].as_str().unwrap()).unwrap().to_file_path().unwrap();
    assert_eq!(definition_path, path.canonicalize().unwrap());
    assert_eq!(definition["result"]["range"]["start"]["line"], 1);

    let changed = "begin\n  let result = () that\n  result\nend\n";
    server.notify(
        "textDocument/didChange",
        json!({
            "textDocument": { "uri": uri, "version": 2 },
            "contentChanges": [{ "text": changed }],
        }),
    );
    let diagnostics = server.notification("textDocument/publishDiagnostics");
    assert!(diagnostics["params"]["diagnostics"].as_array().unwrap().is_empty());
    let symbols =
        server.request("textDocument/documentSymbol", json!({ "textDocument": { "uri": uri } }));
    assert!(symbols["result"].as_array().unwrap().iter().any(|symbol| symbol["name"] == "result"));

    server.notify(
        "textDocument/didChange",
        json!({
            "textDocument": { "uri": uri, "version": 3 },
            "contentChanges": [{ "text": "begin ?" }],
        }),
    );
    let diagnostics = server.notification("textDocument/publishDiagnostics");
    assert_eq!(diagnostics["params"]["diagnostics"].as_array().unwrap().len(), 1);

    server.finish();
}

#[test]
fn stdio_server_ignores_documents_outside_zydeco_source_extensions() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join(".gitignore");
    std::fs::write(&path, "begin ?").unwrap();
    let uri = Url::from_file_path(&path).unwrap().to_string();
    let mut server = LspProcess::start();

    server.request(
        "initialize",
        json!({
            "processId": null,
            "rootUri": Url::from_file_path(directory.path()).unwrap(),
            "capabilities": {},
        }),
    );
    server.notify("initialized", json!({}));
    server.notification("window/logMessage");
    server.notify(
        "textDocument/didOpen",
        json!({
            "textDocument": {
                "uri": uri,
                "languageId": "ignore",
                "version": 1,
                "text": "begin ?",
            },
        }),
    );

    let symbols = server.request_without_notifications(
        "textDocument/documentSymbol",
        json!({ "textDocument": { "uri": uri } }),
    );
    assert!(symbols["result"].is_null());

    server.finish();
}
