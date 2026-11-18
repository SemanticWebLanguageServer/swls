//! End-to-end tests of the `swls` binary over stdio, the way an editor talks
//! to it: real `Backend`, real positions, real file system and cache.
//!
//! Each test starts its own server with private cache and config directories,
//! and only uses built-in ontologies (foaf, rdf, rdfs, owl), so no network is
//! needed.

use std::{
    io::{BufRead, BufReader, Read, Write},
    path::PathBuf,
    process::{Child, ChildStdin, Command, Stdio},
    sync::mpsc,
    time::{Duration, Instant},
};

use serde_json::{json, Value};

const TIMEOUT: Duration = Duration::from_secs(30);

struct Server {
    child: Child,
    stdin: ChildStdin,
    messages: mpsc::Receiver<Value>,
    /// Notifications received while waiting for other messages.
    notifications: Vec<Value>,
    next_id: i64,
    dir: PathBuf,
}

impl Server {
    fn start(name: &str) -> Self {
        let dir = std::env::temp_dir().join(format!("swls-stdio-{}-{}", name, std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();

        let mut child = Command::new(env!("CARGO_BIN_EXE_swls"))
            .env("XDG_CACHE_HOME", dir.join("cache"))
            .env("XDG_CONFIG_HOME", dir.join("config"))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .expect("swls starts");
        let stdin = child.stdin.take().unwrap();
        let stdout = child.stdout.take().unwrap();

        let (tx, messages) = mpsc::channel();
        std::thread::spawn(move || {
            let mut reader = BufReader::new(stdout);
            loop {
                let mut length = None;
                loop {
                    let mut line = String::new();
                    if reader.read_line(&mut line).unwrap_or(0) == 0 {
                        return;
                    }
                    let line = line.trim();
                    if line.is_empty() {
                        break;
                    }
                    if let Some(v) = line.strip_prefix("Content-Length:") {
                        length = v.trim().parse::<usize>().ok();
                    }
                }
                let Some(length) = length else { return };
                let mut body = vec![0; length];
                if reader.read_exact(&mut body).is_err() {
                    return;
                }
                if let Ok(value) = serde_json::from_slice(&body) {
                    if tx.send(value).is_err() {
                        return;
                    }
                }
            }
        });

        let mut server = Self {
            child,
            stdin,
            messages,
            notifications: Vec::new(),
            next_id: 0,
            dir,
        };
        server.request(
            "initialize",
            json!({ "processId": null, "rootUri": null, "capabilities": {} }),
        );
        server.notify("initialized", json!({}));
        server
    }

    fn send(&mut self, message: Value) {
        let body = message.to_string();
        write!(self.stdin, "Content-Length: {}\r\n\r\n{}", body.len(), body).unwrap();
        self.stdin.flush().unwrap();
    }

    fn notify(&mut self, method: &str, params: Value) {
        self.send(json!({ "jsonrpc": "2.0", "method": method, "params": params }));
    }

    /// Next message from the server, answering its own requests on the way.
    fn next_message(&mut self, deadline: Instant) -> Option<Value> {
        loop {
            let left = deadline.checked_duration_since(Instant::now())?;
            let message = self.messages.recv_timeout(left).ok()?;
            if message.get("method").is_some() && message.get("id").is_some() {
                let id = message["id"].clone();
                self.send(json!({ "jsonrpc": "2.0", "id": id, "result": null }));
                continue;
            }
            return Some(message);
        }
    }

    fn request(&mut self, method: &str, params: Value) -> Value {
        self.next_id += 1;
        let id = self.next_id;
        self.send(json!({ "jsonrpc": "2.0", "id": id, "method": method, "params": params }));
        let deadline = Instant::now() + TIMEOUT;
        while let Some(message) = self.next_message(deadline) {
            if message.get("id") == Some(&json!(id)) {
                assert!(message.get("error").is_none(), "{method} failed: {message}");
                return message["result"].clone();
            }
            self.notifications.push(message);
        }
        panic!("no response to {method} within {TIMEOUT:?}");
    }

    /// Wait for a notification matching `pred`, including ones already received.
    fn wait_for(&mut self, what: &str, pred: impl Fn(&Value) -> bool) -> Value {
        if let Some(i) = self.notifications.iter().position(&pred) {
            return self.notifications.remove(i);
        }
        let deadline = Instant::now() + TIMEOUT;
        while let Some(message) = self.next_message(deadline) {
            if pred(&message) {
                return message;
            }
            self.notifications.push(message);
        }
        panic!("no {what} within {TIMEOUT:?}");
    }

    /// Repeat `request` until `pred` holds for its result (data loads in the
    /// background).
    fn request_until(
        &mut self,
        method: &str,
        params: Value,
        pred: impl Fn(&Value) -> bool,
    ) -> Value {
        let deadline = Instant::now() + TIMEOUT;
        loop {
            let result = self.request(method, params.clone());
            if pred(&result) {
                return result;
            }
            assert!(
                Instant::now() < deadline,
                "{method} never satisfied: {result}"
            );
            std::thread::sleep(Duration::from_millis(200));
        }
    }

    fn open(&mut self, uri: &str, text: &str) {
        self.notify(
            "textDocument/didOpen",
            json!({ "textDocument": {
                "uri": uri, "languageId": "turtle", "version": 1, "text": text
            }}),
        );
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
        let _ = std::fs::remove_dir_all(&self.dir);
    }
}

fn at(uri: &str, line: u32, character: u32) -> Value {
    json!({ "textDocument": { "uri": uri }, "position": { "line": line, "character": character } })
}

fn labels(result: &Value) -> Vec<String> {
    let items = result.get("items").unwrap_or(result);
    items
        .as_array()
        .map(|a| {
            a.iter()
                .filter_map(|i| i["label"].as_str().map(String::from))
                .collect()
        })
        .unwrap_or_default()
}

const DOC: &str = "file:///project/people.ttl";
const TEXT: &str =
    "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n<#me> a foaf:Person .\n<#you> a foaf:";

#[test]
fn completion_hover_and_goto_over_stdio() {
    let mut server = Server::start("features");
    server.open(DOC, TEXT);

    // Completion right after `foaf:` (cursor at the end of the last line), once the
    // built-in foaf ontology is loaded.
    let completions = server.request_until("textDocument/completion", at(DOC, 2, 14), |r| {
        labels(r).iter().any(|l| l == "foaf:Person")
    });
    assert!(labels(&completions).contains(&"foaf:Agent".to_string()));

    // Hover on the first character of `foaf:Person` (column 9 of line 1).
    let hover = server.request("textDocument/hover", at(DOC, 1, 9));
    let text = hover["contents"]["value"].as_str().unwrap_or_default();
    assert!(text.contains("Person"), "{hover}");

    // Goto-definition on the prefix opens the read-only cached ontology.
    let targets = server.request("textDocument/definition", at(DOC, 0, 9));
    let uri = targets[0]["uri"].as_str().expect("a definition");
    assert!(uri.ends_with("/swls/lov/foaf.ttl"), "{uri}");
    let path = url_path(uri);
    let wait = Instant::now() + TIMEOUT;
    while !path.exists() && Instant::now() < wait {
        std::thread::sleep(Duration::from_millis(100));
    }
    assert!(std::fs::metadata(&path).unwrap().permissions().readonly());
}

#[test]
fn closing_a_document_clears_its_diagnostics() {
    let mut server = Server::start("close");
    let uri = "file:///project/broken.ttl";
    server.open(uri, "<a> <b> .");

    let has_diagnostics = |m: &Value| {
        m["method"] == "textDocument/publishDiagnostics"
            && m["params"]["uri"] == uri
            && !m["params"]["diagnostics"].as_array().unwrap().is_empty()
    };
    server.wait_for("diagnostics for the broken document", has_diagnostics);

    server.notify(
        "textDocument/didClose",
        json!({ "textDocument": { "uri": uri } }),
    );
    server.wait_for("cleared diagnostics", |m| {
        m["method"] == "textDocument/publishDiagnostics"
            && m["params"]["uri"] == uri
            && m["params"]["diagnostics"].as_array().unwrap().is_empty()
    });
}

#[test]
fn requests_for_unknown_documents_are_answered() {
    let mut server = Server::start("unknown");
    let result = server.request("textDocument/hover", at("file:///nowhere.ttl", 0, 0));
    assert!(result.is_null(), "{result}");

    // The server still works afterwards.
    server.open(DOC, TEXT);
    server.request_until("textDocument/completion", at(DOC, 2, 14), |r| {
        labels(r).iter().any(|l| l == "foaf:Person")
    });
}

fn url_path(uri: &str) -> PathBuf {
    let path = uri.strip_prefix("file://").unwrap();
    PathBuf::from(path.replace("%20", " "))
}

/// Shapes imported with a relative `owl:imports`, as in a real project.
const SHAPES: &str = r#"@prefix sh: <http://www.w3.org/ns/shacl#>.
@prefix foaf: <http://xmlns.com/foaf/0.1/>.

<SomeShape> a sh:NodeShape;
  sh:targetClass foaf:Person;
  sh:property [ a sh:PropertyShape;
    sh:path foaf:name;
    sh:minCount 1;
    sh:maxCount 1;
    sh:name "Your Name";
    sh:description "Your name is required";
  ], [
    sh:path foaf:name22;
    sh:minCount 1;
    sh:maxCount 1;
    sh:name "Your Second Name";
    sh:description "Your Second name is also required";
  ].
"#;

const MAPPING_HEAD: &str = "@prefix owl: <http://www.w3.org/2002/07/owl#>.
@prefix rml: <http://w3id.org/rml/>.
@prefix foaf: <http://xmlns.com/foaf/0.1/>.

<> a owl:Ontology; owl:imports <./shape.ttl>.

<myPerson> a foaf:Person, owl:Thing, rml:ExpressionMap;
  foaf:name \"Arthur\";
  ";

/// Completion on the empty line after `foaf:name "Arthur";` offers the missing
/// required `foaf:name22` first, then the template that adds it.
fn required_property_is_suggested_first(language: &str, rest_of_file: &str) {
    let mut server = Server::start(&format!("required-{language}-{}", rest_of_file.len()));
    let project = server.dir.join("project");
    std::fs::create_dir_all(&project).unwrap();
    std::fs::write(project.join("shape.ttl"), SHAPES).unwrap();
    let mapping = project.join("mapping.ttl");
    let text = format!("{MAPPING_HEAD}{rest_of_file}");
    std::fs::write(&mapping, &text).unwrap();
    let uri = format!("file://{}", mapping.display());

    server.notify(
        "textDocument/didOpen",
        json!({ "textDocument": {
            "uri": uri, "languageId": language, "version": 1, "text": text
        }}),
    );
    let result = server.request_until("textDocument/completion", at(&uri, 8, 2), |r| {
        labels(r).iter().any(|l| l == "foaf:name22")
    });

    let items = result.as_array().unwrap();
    assert_eq!(
        items[0]["label"],
        "foaf:name22",
        "{:?}",
        &labels(&result)[..5]
    );
    assert_eq!(items[0]["labelDetails"]["description"], "required by shape");
    assert_eq!(items[1]["label"], "shape template: required properties");
    // `foaf:name` is already there, so it is not suggested as required.
    assert!(items
        .iter()
        .filter(|i| i["label"] == "foaf:name")
        .all(|i| i["sortText"].as_str().is_none_or(|s| !s.starts_with('!'))));
}

#[test]
fn required_property_is_suggested_first_in_turtle() {
    required_property_is_suggested_first("turtle", "\n  rml:template \"Testing\".\n");
}

#[test]
fn required_property_is_suggested_first_in_trig() {
    required_property_is_suggested_first("trig", "\n  rml:template \"Testing\".\n");
}

#[test]
fn required_property_is_suggested_first_at_the_end_of_an_unfinished_file() {
    required_property_is_suggested_first("turtle", "");
}
