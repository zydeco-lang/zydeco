use super::*;

struct Fixture {
    directory: tempfile::TempDir,
    server: LspProcess,
}

impl Fixture {
    fn new() -> Self {
        let directory = tempfile::tempdir().unwrap();
        let mut server = LspProcess::start();
        let initialized =
            server.request("initialize", json!({ "processId": null, "capabilities": {} }));
        assert_eq!(
            initialized["result"]["capabilities"]["experimental"]["zydecoDocumentation"]["version"],
            1
        );
        assert_eq!(initialized["result"]["capabilities"]["hoverProvider"], true);
        server.notify("initialized", json!({}));
        Self { directory, server }
    }

    fn open(&mut self, filename: &str, source: &str) -> Url {
        let path = self.directory.path().canonicalize().unwrap().join(filename);
        std::fs::write(&path, source).unwrap();
        let uri = Url::from_file_path(path).unwrap();
        self.server.notify("textDocument/didOpen", json!({ "textDocument": { "uri": uri, "languageId": "zydeco", "version": 1, "text": source } }));
        self.server.notification("textDocument/publishDiagnostics");
        uri
    }

    fn change(&mut self, uri: &Url, source: &str) {
        self.server.notify("textDocument/didChange", json!({ "textDocument": { "uri": uri, "version": 2 }, "contentChanges": [{ "text": source }] }));
        self.server.notification("textDocument/publishDiagnostics");
    }

    fn view(&mut self, target: Value) -> Value {
        let response = self.server.request("zydeco/documentation", target);
        assert!(response.get("error").is_none(), "{response}");
        response["result"].clone()
    }
}

#[test]
fn documentation_panel_checks_current_examples_and_rejects_stale_requests() {
    let mut fixture = Fixture::new();
    let source = "--| The answer.\n--|\n--| Longer explanation.\n--|\n--| ```zydeco check\n--| 42\n--| ```\n--|\n--| ```zydeco reject=tyck.missing-named-field at=1:15\n--| (#value = 42)/missing\n--| ```\n@[doc] let answer = 42 in answer";
    let uri = fixture.open("main.zy", source);
    let position = source_position(source, "in answer");
    let position = Position::new(position.line, position.character + 3);
    let target = json!({ "textDocument": { "uri": uri }, "position": position });
    let view = fixture.view(target.clone());
    assert!(view["sections"][0]["markdown"].as_str().unwrap().contains("Longer explanation."));
    assert_eq!(view["signature"], "Int64");
    assert_eq!(view["examples"].as_array().unwrap().len(), 2);
    for example in [0, 1] {
        let checked = fixture.server.request(
            "zydeco/checkDocumentationExample",
            json!({ "target": target, "revision": view["revision"], "example": example }),
        );
        assert_eq!(checked["result"]["status"], "Passed", "{checked}");
        if example == 1 {
            let diagnostic = &checked["result"]["diagnostics"][0];
            assert!(diagnostic["path"].as_str().unwrap().ends_with("main.zy"));
            assert!(
                diagnostic["range"]["start"].as_u64().unwrap()
                    >= source.find("(#value = 42)").unwrap() as u64
            );
        }
    }
    let wrong_index = fixture.server.request(
        "zydeco/checkDocumentationExample",
        json!({ "target": target, "revision": view["revision"], "example": 100 }),
    );
    assert!(wrong_index["result"].is_null());
    fixture.change(&uri, &source.replace("The answer.", "Current answer."));
    let stale = fixture.server.request(
        "zydeco/checkDocumentationExample",
        json!({ "target": target, "revision": view["revision"], "example": 0 }),
    );
    assert!(stale["result"].is_null(), "{stale}");
    let current = fixture.view(target);
    assert!(current["revision"].as_u64().unwrap() > view["revision"].as_u64().unwrap());
    assert!(current["sections"][0]["markdown"].as_str().unwrap().contains("Current answer."));
    fixture.server.finish();
}

#[test]
fn documentation_refreshes_unchanged_consumers_after_import_edits() {
    let mut fixture = Fixture::new();
    let provider = fixture.open("library.zy", "--| Original contract.\n@[doc] (#value = 42)");
    let source = "let library = @(import(\"library.zy\")) in library/value";
    let consumer = fixture.open("main.zy", source);
    let position = source_position(source, "value");
    let original = fixture.server.hover(&consumer, position);
    assert!(original["contents"]["value"].as_str().unwrap().contains("Original contract."));
    fixture.change(&provider, "--| Updated contract.\n@[doc] (#value = 42)");
    let changed = fixture.server.hover(&consumer, position);
    let hover = changed["contents"]["value"].as_str().unwrap();
    assert!(hover.contains("Updated contract."), "{hover}");
    assert!(!hover.contains("Original contract."));
    fixture.server.finish();
}

#[test]
fn documentation_recovery_keeps_current_prose_without_reusing_detached_comments() {
    let mut fixture = Fixture::new();
    let uri = fixture.open("main.zy", "--| Current prose.\n@[doc] (42, missing)");
    let target =
        json!({ "textDocument": { "uri": uri }, "position": { "line": 0, "character": 5 } });
    let view = fixture.view(target.clone());
    assert!(view["sections"][0]["markdown"].as_str().unwrap().contains("Current prose."), "{view}");
    assert!(view["signature"].is_null());
    fixture.change(&uri, "--| Detached prose.\n\n@[doc] (42, missing)");
    assert!(fixture.view(target).is_null());
    fixture.server.finish();
}
