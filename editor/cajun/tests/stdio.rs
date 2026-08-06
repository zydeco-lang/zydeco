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
    assert_eq!(capabilities["referencesProvider"], true);
    assert_eq!(capabilities["hoverProvider"], true);
    assert_eq!(capabilities["documentSymbolProvider"], true);
    assert_eq!(capabilities["semanticTokensProvider"]["full"], true);
    let semantic_legend = &capabilities["semanticTokensProvider"]["legend"];
    assert!(semantic_legend["tokenTypes"].as_array().unwrap().iter().any(|kind| kind == "keyword"));
    assert!(
        semantic_legend["tokenModifiers"]
            .as_array()
            .unwrap()
            .iter()
            .any(|modifier| modifier == "computationType")
    );
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
    let semantic = server
        .request("textDocument/semanticTokens/full", json!({ "textDocument": { "uri": uri } }));
    assert!(!semantic["result"]["data"].as_array().unwrap().is_empty());
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

    let definition_from_binder = server.request(
        "textDocument/definition",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": 1, "character": 7 },
        }),
    );
    assert_eq!(definition_from_binder["result"], definition["result"]);

    let references = server.request(
        "textDocument/references",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": 1, "character": 7 },
            "context": { "includeDeclaration": true },
        }),
    );
    let references = references["result"].as_array().unwrap();
    assert_eq!(references.len(), 2);
    assert_eq!(references[0]["range"]["start"]["line"], 1);
    assert_eq!(references[1]["range"]["start"]["line"], 2);

    let uses = server.request(
        "textDocument/references",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": 2, "character": 3 },
            "context": { "includeDeclaration": false },
        }),
    );
    let uses = uses["result"].as_array().unwrap();
    assert_eq!(uses.len(), 1);
    assert_eq!(uses[0]["range"]["start"]["line"], 2);

    let binder_hover = server.request(
        "textDocument/hover",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": 1, "character": 7 },
        }),
    );
    let use_hover = server.request(
        "textDocument/hover",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": 2, "character": 3 },
        }),
    );
    assert_eq!(binder_hover["result"]["contents"], use_hover["result"]["contents"]);
    assert_eq!(binder_hover["result"]["contents"]["kind"], "markdown");
    assert_eq!(binder_hover["result"]["contents"]["value"], "```zydeco\nanswer : Unit\n```");

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
    let semantic = server
        .request("textDocument/semanticTokens/full", json!({ "textDocument": { "uri": uri } }));
    let data = semantic["result"]["data"].as_array().unwrap();
    let keyword = semantic_legend["tokenTypes"]
        .as_array()
        .unwrap()
        .iter()
        .position(|kind| kind == "keyword")
        .unwrap();
    assert_eq!(data[0..5], [json!(0), json!(0), json!(5), json!(keyword), json!(0)]);

    server.finish();
}

#[test]
fn stdio_hover_links_referenced_type_definitions() {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../lib/tests/exec/forall.zy")
        .canonicalize()
        .unwrap();
    let source = std::fs::read_to_string(&path).unwrap();
    let mut definition = Url::from_file_path(&path).unwrap();
    definition.set_fragment(Some("L15"));
    let uri = Url::from_file_path(&path).unwrap().to_string();
    let mut server = LspProcess::start();

    server.request(
        "initialize",
        json!({
            "processId": null,
            "rootUri": Url::from_file_path(path.parent().unwrap()).unwrap(),
            "capabilities": {},
        }),
    );
    server.notify("initialized", json!({}));
    server.notify(
        "textDocument/didOpen",
        json!({
            "textDocument": {
                "uri": uri,
                "languageId": "zydeco",
                "version": 1,
                "text": source,
            },
        }),
    );
    let diagnostics = server.notification("textDocument/publishDiagnostics");
    assert!(diagnostics["params"]["diagnostics"].as_array().unwrap().is_empty());

    let hover = server.request(
        "textDocument/hover",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": 15, "character": 9 },
        }),
    );
    assert_eq!(hover["result"]["contents"]["kind"], "markdown");
    assert_eq!(
        hover["result"]["contents"]["value"],
        format!("```zydeco\nvalue : A\n```\n\nTypes:\n\n- [`A` ↗](<{definition}>)")
    );

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
    let semantic = server.request_without_notifications(
        "textDocument/semanticTokens/full",
        json!({ "textDocument": { "uri": uri } }),
    );
    assert!(semantic["result"].is_null());
    let references = server.request_without_notifications(
        "textDocument/references",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": 0, "character": 0 },
            "context": { "includeDeclaration": true },
        }),
    );
    assert!(references["result"].is_null());
    let hover = server.request_without_notifications(
        "textDocument/hover",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": 0, "character": 0 },
        }),
    );
    assert!(hover["result"].is_null());

    server.finish();
}
