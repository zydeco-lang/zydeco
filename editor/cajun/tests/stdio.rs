use serde_json::{Value, json};
use std::{
    collections::{BTreeSet, VecDeque},
    io::{BufRead, BufReader, Read, Write},
    process::{Child, ChildStdin, ChildStdout, Command, Stdio},
};
use tower_lsp::lsp_types::{Position, Url};

/// Locate a needle's UTF-16 line-and-column in a source string, so test
/// positions survive source reformatting.
fn source_position(source: &str, needle: &str) -> Position {
    let byte = source.find(needle).unwrap_or_else(|| panic!("missing source text: {needle}"));
    let before = &source[..byte];
    let line = before.bytes().filter(|byte| *byte == b'\n').count() as u32;
    let line_start = before.rfind('\n').map_or(0, |newline| newline + 1);
    let character = source[line_start..byte].encode_utf16().count() as u32;
    Position::new(line, character)
}

struct LspProcess {
    child: Child,
    input: Option<ChildStdin>,
    output: BufReader<ChildStdout>,
    pending: VecDeque<Value>,
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
        Self { child, input: Some(input), output, pending: VecDeque::new(), next_id: 1 }
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
        let id = json!(id);
        self.message(|message| message.get("id") == Some(&id) && message.get("method").is_none())
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
        self.message(|message| message.get("method") == Some(&json!(method)))
    }

    fn acknowledge_server_request(&mut self, method: &str) -> Value {
        let request = self.message(|message| {
            message.get("method") == Some(&json!(method)) && message.get("id").is_some()
        });
        self.send(json!({
            "jsonrpc": "2.0",
            "id": request["id"],
            "result": null,
        }));
        request
    }

    fn configure(&mut self, settings: Value) {
        self.notify("workspace/didChangeConfiguration", json!({ "settings": settings }));
    }

    fn hover(&mut self, uri: &Url, position: Position) -> Value {
        let response = self.request(
            "textDocument/hover",
            json!({ "textDocument": { "uri": uri }, "position": position }),
        );
        assert!(response.get("error").is_none(), "hover failed: {response}");
        response["result"].clone()
    }

    fn configuration_request(&mut self) -> Value {
        let request = self.message(|message| {
            message.get("method") == Some(&json!("workspace/configuration"))
                && message.get("id").is_some()
        });
        assert_eq!(request["params"], json!({ "items": [{ "section": "cajun" }] }));
        request
    }

    fn respond(&mut self, request: &Value, result: Value) {
        self.send(json!({ "jsonrpc": "2.0", "id": request["id"], "result": result }));
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

    fn message(&mut self, predicate: impl Fn(&Value) -> bool) -> Value {
        if let Some(index) = self.pending.iter().position(&predicate) {
            return self.pending.remove(index).unwrap();
        }
        loop {
            let message = self.receive();
            if predicate(&message) {
                return message;
            }
            self.pending.push_back(message);
        }
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
fn stdio_server_invalidates_semantic_tokens_during_reanalysis_and_requests_refresh() {
    let repository =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..").canonicalize().unwrap();
    let path = repository.join("lib/examples/algebra.zydeco").canonicalize().unwrap();
    let changed = std::fs::read_to_string(&path).unwrap();
    let original = "begin\n  let old = () that\n  old\nend\n";
    let uri = Url::from_file_path(&path).unwrap().to_string();
    let mut server = LspProcess::start();

    server.request(
        "initialize",
        json!({
            "processId": null,
            "rootUri": Url::from_file_path(&repository).unwrap(),
            "capabilities": {
                "workspace": {
                    "semanticTokens": { "refreshSupport": true }
                }
            },
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
                "text": original,
            },
        }),
    );
    server.notification("textDocument/publishDiagnostics");
    server.acknowledge_server_request("workspace/semanticTokens/refresh");
    let original_tokens = server
        .request("textDocument/semanticTokens/full", json!({ "textDocument": { "uri": uri } }));

    server.notify(
        "textDocument/didChange",
        json!({
            "textDocument": { "uri": uri, "version": 2 },
            "contentChanges": [{ "text": changed }],
        }),
    );
    let current_tokens = server
        .request("textDocument/semanticTokens/full", json!({ "textDocument": { "uri": uri } }));
    assert_ne!(
        current_tokens["result"]["data"], original_tokens["result"]["data"],
        "a token response issued after didChange reused the preceding document revision"
    );

    server.notification("textDocument/publishDiagnostics");
    server.acknowledge_server_request("workspace/semanticTokens/refresh");
    let analyzed_tokens = server
        .request("textDocument/semanticTokens/full", json!({ "textDocument": { "uri": uri } }));
    assert_ne!(analyzed_tokens["result"]["data"], original_tokens["result"]["data"]);

    server.finish();
}

#[test]
fn stdio_server_exposes_import_document_links_after_resolution_errors() {
    let directory = tempfile::tempdir().unwrap();
    let library = directory.path().join("library.zy");
    let root = directory.path().join("main.zy");
    let source = "(@[import(\"library.zy\")] _, missing)\n";
    std::fs::write(&library, "()\n").unwrap();
    std::fs::write(&root, source).unwrap();
    let uri = Url::from_file_path(&root).unwrap().to_string();
    let target = Url::from_file_path(library.canonicalize().unwrap()).unwrap().to_string();
    let mut server = LspProcess::start();

    let initialize = server.request(
        "initialize",
        json!({
            "processId": null,
            "rootUri": Url::from_file_path(directory.path()).unwrap(),
            "capabilities": {},
        }),
    );
    assert_eq!(
        initialize["result"]["capabilities"]["documentLinkProvider"],
        json!({ "resolveProvider": false })
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
    assert_eq!(diagnostics["params"]["diagnostics"][0]["source"], "zydeco");

    let response =
        server.request("textDocument/documentLink", json!({ "textDocument": { "uri": uri } }));
    let [link] = response["result"].as_array().unwrap().as_slice() else {
        panic!("expected one import document link: {response}")
    };
    let start = source.find("library.zy").unwrap();
    let end = start + "library.zy".len();
    assert_eq!(
        link["range"],
        json!({
            "start": { "line": 0, "character": start },
            "end": { "line": 0, "character": end },
        })
    );
    assert_eq!(link["target"], target);
    assert!(link.get("tooltip").is_none());
    assert!(link.get("data").is_none());

    server.finish();
}

#[test]
fn stdio_server_completes_incomplete_metadata_with_snippets() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("main.zy");
    let source = "@[intr";
    std::fs::write(&path, source).unwrap();
    let uri = Url::from_file_path(&path).unwrap().to_string();
    let mut server = LspProcess::start();

    let initialize = server.request(
        "initialize",
        json!({
            "processId": null,
            "rootUri": Url::from_file_path(directory.path()).unwrap(),
            "capabilities": {
                "textDocument": {
                    "completion": {
                        "completionItem": { "snippetSupport": true }
                    }
                }
            },
        }),
    );
    assert_eq!(
        initialize["result"]["capabilities"]["completionProvider"],
        json!({
            "resolveProvider": false,
            "triggerCharacters": ["[", "(", ",", "\"", "/", "\\"],
        })
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
    server.notification("textDocument/publishDiagnostics");

    let response = server.request(
        "textDocument/completion",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": 0, "character": source.encode_utf16().count() },
        }),
    );
    let [item] = response["result"].as_array().unwrap().as_slice() else {
        panic!("expected one filtered metadata completion: {response}")
    };
    assert_eq!(item["label"], "intrinsic");
    assert_eq!(item["insertTextFormat"], 2);
    assert_eq!(item["textEdit"]["newText"], "intrinsic(${1:role})");
    assert_eq!(
        item["textEdit"]["range"],
        json!({
            "start": { "line": 0, "character": 2 },
            "end": { "line": 0, "character": 6 },
        })
    );

    server.finish();
}

#[test]
fn stdio_server_completes_import_paths_while_typing() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("main.zy");
    std::fs::write(&path, "()").unwrap();
    std::fs::create_dir(directory.path().join("nested")).unwrap();
    std::fs::write(directory.path().join("nested/library.zy"), "()").unwrap();
    std::fs::write(directory.path().join("nested/notes.md"), "notes").unwrap();
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
    server.notify(
        "textDocument/didOpen",
        json!({
            "textDocument": { "uri": uri, "languageId": "zydeco", "version": 1, "text": "()" },
        }),
    );
    server.notification("textDocument/publishDiagnostics");

    for (version, source, context, label, start) in [
        (2, r#"@[import(""#, json!({ "triggerKind": 2, "triggerCharacter": "\"" }), "nested/", 10),
        (
            3,
            r#"@[import("nested/"#,
            json!({ "triggerKind": 2, "triggerCharacter": "/" }),
            "library.zy",
            17,
        ),
        (4, r#"@[import("nested/li"#, json!({ "triggerKind": 3 }), "library.zy", 17),
    ] {
        server.notify(
            "textDocument/didChange",
            json!({
                "textDocument": { "uri": uri, "version": version },
                "contentChanges": [{ "text": source }],
            }),
        );
        server.notification("textDocument/publishDiagnostics");
        let response = server.request_without_notifications(
            "textDocument/completion",
            json!({
                "textDocument": { "uri": uri },
                "position": { "line": 0, "character": source.len() },
                "context": context,
            }),
        );
        assert_eq!(response["result"]["isIncomplete"], true);
        let [item] = response["result"]["items"].as_array().unwrap().as_slice() else {
            panic!("one import path should match the current edit: {response}")
        };
        assert_eq!(item["label"], label);
        assert_eq!(item["kind"], if label.ends_with('/') { 19 } else { 17 });
        assert_eq!(item["textEdit"]["newText"], label);
        assert_eq!(
            item["textEdit"]["range"],
            json!({
                "start": { "line": 0, "character": start },
                "end": { "line": 0, "character": source.len() },
            })
        );
    }

    let source = r#""nested/""#;
    server.notify(
        "textDocument/didChange",
        json!({
            "textDocument": { "uri": uri, "version": 5 },
            "contentChanges": [{ "text": source }],
        }),
    );
    server.notification("textDocument/publishDiagnostics");
    let response = server.request(
        "textDocument/completion",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": 0, "character": source.len() - 1 },
            "context": { "triggerKind": 2, "triggerCharacter": "/" },
        }),
    );
    assert!(response["result"].is_null(), "unrelated strings do not complete import paths");
    server.finish();
}

#[test]
fn stdio_server_completes_current_names_after_parse_errors_with_optional_type_labels() {
    for label_details in [true, false] {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("main.zy");
        let original = "let old = 1 in old";
        std::fs::write(&path, original).unwrap();
        let uri = Url::from_file_path(&path).unwrap().to_string();
        let mut server = LspProcess::start();
        server.request(
            "initialize",
            json!({
                "processId": null,
                "rootUri": Url::from_file_path(directory.path()).unwrap(),
                "capabilities": {
                    "textDocument": {
                        "completion": {
                            "completionItem": { "labelDetailsSupport": label_details }
                        }
                    }
                },
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
                    "text": original,
                },
            }),
        );
        server.notification("textDocument/publishDiagnostics");

        for (version, source) in [(2, "let current = 1 in curr"), (3, "let current = 1 in ")] {
            server.notify(
                "textDocument/didChange",
                json!({
                    "textDocument": { "uri": uri, "version": version },
                    "contentChanges": [{ "text": source }],
                }),
            );
            let diagnostics = server.notification("textDocument/publishDiagnostics");
            assert!(!diagnostics["params"]["diagnostics"].as_array().unwrap().is_empty());
            let response = server.request_without_notifications(
                "textDocument/completion",
                json!({
                    "textDocument": { "uri": uri },
                    "position": { "line": 0, "character": source.len() },
                }),
            );
            let [item] = response["result"].as_array().unwrap().as_slice() else {
                panic!("the current source should have exactly one visible name: {response}")
            };
            assert_eq!(item["label"], "current");
            assert_eq!(item["detail"], "Int64");
            assert_eq!(item["textEdit"]["newText"], "current");
            assert_eq!(item["filterText"], "current");
            assert_eq!(
                item["labelDetails"]["detail"],
                if label_details { json!(" : Int64") } else { Value::Null }
            );
        }

        let source =
            "let matching = 1 in let other = 'x' in val unknown => (_ : @[intrinsic(i64)] _)";
        server.notify(
            "textDocument/didChange",
            json!({
                "textDocument": { "uri": uri, "version": 4 },
                "contentChanges": [{ "text": source }],
            }),
        );
        server.notification("textDocument/publishDiagnostics");
        let response = server.request_without_notifications(
            "textDocument/completion",
            json!({
                "textDocument": { "uri": uri },
                "position": source_position(source, "_ :"),
            }),
        );
        let items = response["result"].as_array().unwrap();
        assert_eq!(
            items.iter().map(|item| item["label"].as_str().unwrap()).collect::<Vec<_>>(),
            ["matching", "unknown"]
        );
        assert_eq!(items[0]["detail"], "Int64");

        let source = "let current = 1 in fn binder => current";
        server.notify(
            "textDocument/didChange",
            json!({
                "textDocument": { "uri": uri, "version": 5 },
                "contentChanges": [{ "text": source }],
            }),
        );
        server.notification("textDocument/publishDiagnostics");
        let response = server.request(
            "textDocument/completion",
            json!({
                "textDocument": { "uri": uri },
                "position": source_position(source, "binder"),
            }),
        );
        assert!(response["result"].is_null(), "binder positions are not name references");
        server.finish();
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
    assert_eq!(capabilities["renameProvider"], json!({ "prepareProvider": true }));
    assert_eq!(capabilities["hoverProvider"], true);
    assert_eq!(capabilities["documentSymbolProvider"], true);
    assert_eq!(capabilities["documentFormattingProvider"], true);
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
    let [diagnostic] = diagnostics["params"]["diagnostics"].as_array().unwrap().as_slice() else {
        panic!("expected one parser diagnostic: {diagnostics}")
    };
    assert_eq!(diagnostic["source"], "zydeco");
    assert_eq!(
        diagnostic["range"],
        json!({
            "start": { "line": 0, "character": 6 },
            "end": { "line": 0, "character": 7 },
        })
    );
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

    server.notify(
        "textDocument/didChange",
        json!({
            "textDocument": { "uri": uri, "version": 4 },
            "contentChanges": [{ "text": "begin\n  missing\nend\n" }],
        }),
    );
    let diagnostics = server.notification("textDocument/publishDiagnostics");
    let [diagnostic] = diagnostics["params"]["diagnostics"].as_array().unwrap().as_slice() else {
        panic!("expected one resolution diagnostic: {diagnostics}")
    };
    assert_eq!(diagnostic["source"], "zydeco");
    assert_eq!(
        diagnostic["range"],
        json!({
            "start": { "line": 1, "character": 2 },
            "end": { "line": 1, "character": 9 },
        })
    );

    server.finish();
}

#[test]
fn stdio_server_renames_resolved_symbols() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("main.zy");
    let original = "begin\n  let answer = () that\n  answer\nend\n";
    std::fs::write(&path, original).unwrap();
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
    server.notification("textDocument/publishDiagnostics");

    let prepared = server.request(
        "textDocument/prepareRename",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": 2, "character": 4 },
        }),
    );
    assert_eq!(
        prepared["result"],
        json!({
            "range": {
                "start": { "line": 2, "character": 2 },
                "end": { "line": 2, "character": 8 },
            },
            "placeholder": "answer",
        })
    );
    let unprepared = server.request(
        "textDocument/prepareRename",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": 0, "character": 1 },
        }),
    );
    assert!(unprepared["result"].is_null());

    let renamed = server.request(
        "textDocument/rename",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": 2, "character": 4 },
            "newName": "result",
        }),
    );
    let canonical = Url::from_file_path(path.canonicalize().unwrap()).unwrap().to_string();
    let edits = renamed["result"]["changes"][canonical.as_str()].as_array().unwrap();
    assert_eq!(edits.len(), 2);
    assert!(
        edits.iter().all(|edit| edit["newText"] == "result"),
        "every occurrence should be rewritten: {edits:?}"
    );
    assert_eq!(edits[0]["range"]["start"], json!({ "line": 1, "character": 6 }));
    assert_eq!(edits[1]["range"]["start"], json!({ "line": 2, "character": 2 }));

    let refused = server.request(
        "textDocument/rename",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": 2, "character": 4 },
            "newName": "let",
        }),
    );
    assert_eq!(refused["error"]["code"], -32803);
    assert!(
        refused["error"]["message"].as_str().unwrap().contains("reserved"),
        "the refusal should explain the reserved word: {refused}"
    );

    server.finish();
}

#[test]
fn stdio_server_formats_the_open_document() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("main.zy");
    let source = "(#field = field, ((x)))";
    std::fs::write(&path, source).unwrap();
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
    assert_eq!(initialize["result"]["capabilities"]["documentFormattingProvider"], true);
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
    server.notification("textDocument/publishDiagnostics");

    let formatted = server.request(
        "textDocument/formatting",
        json!({
            "textDocument": { "uri": uri },
            "options": { "tabSize": 2, "insertSpaces": true },
        }),
    );
    let edits = formatted["result"].as_array().unwrap();
    assert_eq!(edits.len(), 1);
    assert_eq!(edits[0]["range"]["start"], json!({ "line": 0, "character": 0 }));
    assert_eq!(
        edits[0]["range"]["end"],
        json!({ "line": 0, "character": source.encode_utf16().count() })
    );
    assert_eq!(edits[0]["newText"], "(= field, x)\n");

    server.finish();
}

#[test]
fn stdio_server_follows_source_format_annotations() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("main.zy");
    let source = concat!(
        "@[format(layout(ignore))] ! (bool/if)\n",
        "  (Ret Int64)\n",
        "  greater\n",
        "  { ret left }\n",
        "  { ret right }\n",
    );
    std::fs::write(&path, source).unwrap();
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
    server.notification("textDocument/publishDiagnostics");

    let formatted = server.request(
        "textDocument/formatting",
        json!({
            "textDocument": { "uri": uri },
            "options": { "tabSize": 2, "insertSpaces": true },
        }),
    );
    let edits = formatted["result"].as_array().unwrap();
    assert_eq!(edits.len(), 1);
    assert_eq!(
        edits[0]["newText"],
        "@[format(layout(ignore))] ! bool/if (Ret Int64) greater { ret left } { ret right }\n"
    );

    server.finish();
}

#[test]
fn stdio_server_warns_about_ineffective_text_blocks() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("main.zy");
    let source = "--| Ineffective documentation.\n_";
    std::fs::write(&path, source).unwrap();
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

    let published = server.notification("textDocument/publishDiagnostics");
    let diagnostics = published["params"]["diagnostics"].as_array().unwrap();
    let [warning] = diagnostics.as_slice() else { panic!("expected one warning: {published}") };
    assert_eq!(warning["severity"], 2);
    assert_eq!(warning["code"], "unattached-text-block");
    assert_eq!(warning["source"], "zydeco");
    assert!(warning["message"].as_str().unwrap().contains("has no effect"));
    assert_eq!(
        warning["range"],
        json!({
            "start": { "line": 0, "character": 0 },
            "end": { "line": 1, "character": 0 },
        })
    );

    let attached = "--| Effective documentation.\n@[doc] _";
    server.notify(
        "textDocument/didChange",
        json!({
            "textDocument": { "uri": uri, "version": 2 },
            "contentChanges": [{ "text": attached }],
        }),
    );
    let published = server.notification("textDocument/publishDiagnostics");
    assert!(published["params"]["diagnostics"].as_array().unwrap().is_empty());

    server.finish();
}

#[test]
fn stdio_hover_links_referenced_type_definitions() {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../lib/tests/exec/forall.zy")
        .canonicalize()
        .unwrap();
    let source = std::fs::read_to_string(&path).unwrap();
    let value = source_position(&source, "value : A");
    let type_parameter = source_position(&source, "A : VType");
    let mut definition = Url::from_file_path(&path).unwrap();
    definition.set_fragment(Some(&format!(
        "L{},{}",
        type_parameter.line + 1,
        type_parameter.character + 1
    )));
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
            "position": { "line": value.line, "character": value.character },
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
fn stdio_hover_updates_endpoint_policy_without_restarting() {
    let directory = tempfile::tempdir().unwrap();
    let mut server = LspProcess::start();
    let initialized = server.request(
        "initialize",
        json!({
            "processId": null,
            "rootUri": Url::from_file_path(directory.path()).unwrap(),
            "capabilities": {},
            // Runtime preferences are no longer read through this startup-only path.
            "initializationOptions": { "hover": { "inclusiveEnd": true } },
        }),
    );
    assert!(initialized.get("error").is_none(), "initialization failed: {initialized}");
    server.notify("initialized", json!({}));

    for name in ["x", "alpha"] {
        let path = directory.path().join(format!("{name}.zy"));
        let source =
            format!("begin\r\n  let {name} = () that\r\n  /- 😀 -/ ({name} , {name})\r\nend\r\n");
        std::fs::write(&path, &source).unwrap();
        let uri = Url::from_file_path(&path).unwrap();
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

        for (settings, inclusive_end) in [
            (None, false),
            (Some(json!({ "cajun": { "hover": { "inclusiveEnd": true } } })), true),
            (Some(json!({ "cajun": { "hover": { "inclusiveEnd": false } } })), false),
            (Some(json!({ "cajun": { "hover": { "inclusiveEnd": true } } })), true),
            (Some(json!({ "cajun": { "hover": { "inclusiveEnd": "false" } } })), true),
            (Some(Value::Null), true),
            (Some(json!({})), false),
        ] {
            if let Some(settings) = settings {
                server.configure(settings);
            }
            let pair = format!("({name} , {name})");
            let pair_signature = format!("```zydeco\n({name}, {name}) : Unit * Unit\n```");
            for (needle, length, signature) in [
                (format!("{name} ="), name.len() as u32, format!("{name} : Unit")),
                (format!("{name} ,"), name.len() as u32, format!("{name} : Unit")),
                ("() that".to_owned(), 2, "() : Unit".to_owned()),
                (pair.clone(), pair.len() as u32, format!("({name}, {name}) : Unit * Unit")),
            ] {
                let start = source_position(&source, &needle);
                let end = Position::new(start.line, start.character + length);
                let inside = server.request(
                    "textDocument/hover",
                    json!({ "textDocument": { "uri": uri }, "position": start }),
                );
                assert_eq!(
                    inside["result"]["contents"]["value"],
                    format!("```zydeco\n{signature}\n```")
                );
                assert_eq!(inside["result"]["range"], json!({ "start": start, "end": end }));
                let at_end = server.request(
                    "textDocument/hover",
                    json!({ "textDocument": { "uri": uri }, "position": end }),
                );
                assert!(at_end.get("error").is_none(), "hover failed: {at_end}");
                assert!(at_end["result"]["contents"]["value"].is_string());
                if inclusive_end {
                    assert_eq!(at_end["result"], inside["result"], "endpoint of {needle}");
                } else {
                    assert_ne!(at_end["result"], inside["result"], "endpoint of {needle}");
                }
            }

            let use_start = source_position(&source, &format!("{name} ,"));
            let use_end = Position::new(use_start.line, use_start.character + name.len() as u32);
            let comma = Position::new(use_end.line, use_end.character + 1);
            let beyond_end = server.request(
                "textDocument/hover",
                json!({ "textDocument": { "uri": uri }, "position": comma }),
            );
            assert_eq!(beyond_end["result"]["contents"]["value"], pair_signature);
            for method in ["textDocument/definition", "textDocument/prepareRename"] {
                let inside = server.request(
                    method,
                    json!({ "textDocument": { "uri": uri }, "position": use_start }),
                );
                assert!(!inside["result"].is_null());
                let at_end = server.request(
                    method,
                    json!({ "textDocument": { "uri": uri }, "position": use_end }),
                );
                assert!(at_end["result"].is_null(), "{method} must retain exclusive endpoints");
            }
            let outside = server.request(
                "textDocument/hover",
                json!({ "textDocument": { "uri": uri }, "position": { "line": 4, "character": 0 } }),
            );
            assert!(outside["result"].is_null());
        }
    }
    server.finish();
}

#[test]
fn stdio_hover_updates_line_width_without_restarting() {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../lib/std/data/package.zy")
        .canonicalize()
        .unwrap();
    let source = std::fs::read_to_string(&path).unwrap();
    let zip = source_position(&source, "zip (A : VType)");
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

    let default = server.hover(&Url::parse(&uri).unwrap(), zip);
    server.configure(json!({ "cajun": { "hover": { "lineWidth": 32 } } }));
    let hover = server.request(
        "textDocument/hover",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": zip.line, "character": zip.character },
        }),
    );
    assert_eq!(hover["result"]["contents"]["kind"], "markdown");
    let markdown = hover["result"]["contents"]["value"].as_str().unwrap();
    let fenced_sources = markdown
        .split("```zydeco\n")
        .skip(1)
        .map(|fence| fence.split_once("\n```").expect("Zydeco fence should be closed").0)
        .collect::<Vec<_>>();
    assert!(
        fenced_sources.first().is_some_and(|source| source.lines().count() > 1),
        "narrow hover should wrap:\n{markdown}"
    );
    assert!(
        fenced_sources
            .iter()
            .flat_map(|source| source.lines())
            .all(|line| line.chars().count() <= 32),
        "hover should honor the live line width:\n{markdown}"
    );

    assert_ne!(default["contents"], hover["result"]["contents"]);
    server.configure(json!({ "cajun": { "hover": { "lineWidth": 0 } } }));
    assert_eq!(server.hover(&Url::parse(&uri).unwrap(), zip), hover["result"]);
    server.configure(json!({ "cajun": { "hover": { "lineWidth": 100 } } }));
    assert_eq!(server.hover(&Url::parse(&uri).unwrap(), zip), default);
    server.configure(json!({ "cajun": { "hover": { "lineWidth": 32 } } }));
    assert_eq!(server.hover(&Url::parse(&uri).unwrap(), zip), hover["result"]);
    server.configure(json!({ "cajun": { "hover": {} } }));
    assert_eq!(server.hover(&Url::parse(&uri).unwrap(), zip), default);

    server.finish();
}

#[test]
fn stdio_configuration_registers_and_pulls_live_settings() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("configuration.zy");
    let source = "begin\n  let x = () that\n  (x , x)\nend\n";
    std::fs::write(&path, source).unwrap();
    let uri = Url::from_file_path(&path).unwrap();
    let start = source_position(source, "x ,");
    let end = Position::new(start.line, start.character + 1);
    let mut server = LspProcess::start();
    server.request(
        "initialize",
        json!({
            "processId": null,
            "rootUri": Url::from_file_path(directory.path()).unwrap(),
            "capabilities": {
                "workspace": {
                    "configuration": true,
                    "didChangeConfiguration": { "dynamicRegistration": true }
                }
            }
        }),
    );
    server.notify("initialized", json!({}));
    let registration = server.acknowledge_server_request("client/registerCapability");
    assert_eq!(
        registration["params"]["registrations"],
        json!([{
            "id": "cajun-configuration",
            "method": "workspace/didChangeConfiguration",
            "registerOptions": { "section": "cajun" }
        }])
    );
    let request = server.configuration_request();
    server.respond(&request, json!([{ "hover": { "inclusiveEnd": true } }]));
    assert_eq!(server.notification("window/logMessage")["params"]["message"], "Cajun initialized");
    server.notify(
        "textDocument/didOpen",
        json!({
            "textDocument": { "uri": uri, "languageId": "zydeco", "version": 1, "text": source }
        }),
    );
    let diagnostics = server.notification("textDocument/publishDiagnostics");
    assert!(diagnostics["params"]["diagnostics"].as_array().unwrap().is_empty());
    let variable = server.hover(&uri, start);
    assert_eq!(server.hover(&uri, end), variable, "initial settings must be fetched");

    for (result, error) in [
        (json!([]), "expected one workspace/configuration result"),
        (json!([{}, {}]), "expected one workspace/configuration result"),
        (json!([{ "hover": { "inclusiveEnd": "false" } }]), "invalid settings"),
    ] {
        server.configure(Value::Null);
        let request = server.configuration_request();
        server.respond(&request, result);
        let warning = server.notification("window/logMessage");
        assert_eq!(warning["params"]["type"], 2);
        assert!(warning["params"]["message"].as_str().unwrap().contains(error));
        assert_eq!(server.hover(&uri, end), variable, "failed pulls must retain the last settings");
    }

    server.configure(Value::Null);
    let request = server.configuration_request();
    server.send(json!({
        "jsonrpc": "2.0", "id": request["id"],
        "error": { "code": -32603, "message": "configuration unavailable" }
    }));
    let warning = server.notification("window/logMessage");
    assert_eq!(warning["params"]["type"], 2);
    assert!(
        warning["params"]["message"].as_str().unwrap().contains("workspace/configuration failed")
    );
    assert_eq!(server.hover(&uri, end), variable);

    for (result, inclusive) in [
        (json!([{ "hover": { "inclusiveEnd": false } }]), false),
        (json!([{ "hover": { "inclusiveEnd": true } }]), true),
        (json!([null]), false),
    ] {
        server.configure(Value::Null);
        let request = server.configuration_request();
        server.respond(&request, result);
        assert_eq!(server.hover(&uri, end) == variable, inclusive);
    }
    server.finish();
}

#[test]
fn stdio_configuration_ignores_a_pull_superseded_by_a_push() {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("configuration.zy");
    let source = "begin\n  let x = () that\n  (x , x)\nend\n";
    std::fs::write(&path, source).unwrap();
    let uri = Url::from_file_path(&path).unwrap();
    let start = source_position(source, "x ,");
    let end = Position::new(start.line, start.character + 1);
    let mut server = LspProcess::start();
    server.request(
        "initialize",
        json!({
            "processId": null,
            "rootUri": Url::from_file_path(directory.path()).unwrap(),
            "capabilities": { "workspace": { "configuration": true } }
        }),
    );
    server.notify("initialized", json!({}));
    let delayed_request = server.configuration_request();
    server.configure(json!({ "cajun": { "hover": { "inclusiveEnd": true } } }));
    server.notify(
        "textDocument/didOpen",
        json!({
            "textDocument": { "uri": uri, "languageId": "zydeco", "version": 1, "text": source }
        }),
    );
    let diagnostics = server.notification("textDocument/publishDiagnostics");
    assert!(diagnostics["params"]["diagnostics"].as_array().unwrap().is_empty());
    let variable = server.hover(&uri, start);
    assert_eq!(server.hover(&uri, end), variable);

    server.respond(&delayed_request, json!([{ "hover": { "inclusiveEnd": false } }]));
    assert_eq!(server.notification("window/logMessage")["params"]["message"], "Cajun initialized");
    assert_eq!(server.hover(&uri, end), variable, "a delayed pull must not replace a newer push");
    server.configure(json!({}));
    assert_ne!(
        server.hover(&uri, end),
        variable,
        "removing settings must still work after a stale pull"
    );
    server.finish();
}

#[test]
fn stdio_server_treats_overlapping_open_analyses_as_superseded() {
    let repository =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..").canonicalize().unwrap();
    let documents = [
        "lib/std/builtin.zy",
        "lib/std/data/package.zy",
        "lib/std/control/monad.zy",
        "lib/std/std.zy",
    ]
    .into_iter()
    .map(|relative| {
        let path = repository.join(relative).canonicalize().unwrap();
        let uri = Url::from_file_path(&path).unwrap().to_string();
        let source = std::fs::read_to_string(path).unwrap();
        (uri, source)
    })
    .collect::<Vec<_>>();
    let mut server = LspProcess::start();

    server.request(
        "initialize",
        json!({
            "processId": null,
            "rootUri": Url::from_file_path(&repository).unwrap(),
            "capabilities": {},
        }),
    );
    server.notify("initialized", json!({}));
    documents.iter().for_each(|(uri, source)| {
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
        )
    });

    let published = (0..documents.len())
        .map(|_| server.notification("textDocument/publishDiagnostics"))
        .collect::<Vec<_>>();
    published.iter().for_each(|notification| {
        let diagnostics = notification["params"]["diagnostics"].as_array().unwrap();
        assert!(
            diagnostics.iter().all(|diagnostic| diagnostic["severity"] == 2),
            "cancelled analysis leaked an error diagnostic: {notification}"
        );
    });
    let expected = documents.iter().map(|(uri, _)| uri.clone()).collect::<BTreeSet<_>>();
    let reported = published
        .iter()
        .map(|notification| notification["params"]["uri"].as_str().unwrap().to_owned())
        .collect::<BTreeSet<_>>();
    assert_eq!(reported, expected);

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
