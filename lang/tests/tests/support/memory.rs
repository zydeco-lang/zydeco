use std::path::PathBuf;

pub fn source(body: &str) -> String {
    let directory = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../lib/std/memory")
        .canonicalize()
        .unwrap();
    format!(
        r#"
let (/Uninit; /Init; /Fields; /Field; /DynamicField; /runtime_fields; /Ptr; /fields; /records; /headers; /arrays; /views; /Slice; /slices; /fixed; /dynamic; /allocation; /pointer; /raw) =
  builtin |> (@(import("{directory}/package.zy"))) in
let (= Plan, = Layout, memory) = fixed in
let (/Alloc; /heap) = allocation in
let Fault = @(import("{directory}/fault.zy")) in
let size = @(import("{directory}/size.zy")) in
let fail = {{ ! exit 1 }} in
let no = {{ fn (_ : Fault) => ! fail }} in
{body}
"#,
        directory = directory.display()
    )
}
