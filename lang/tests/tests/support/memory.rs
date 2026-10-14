use std::path::PathBuf;
use zydeco_tests::utils::SourceTemplate;

pub struct MemoryCase;

impl MemoryCase {
    pub fn source(body: &str) -> String {
        let directory = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/std/memory")
            .canonicalize()
            .unwrap()
            .display()
            .to_string();
        SourceTemplate::parse(include_str!("../../templates/memory.zy.in"))
            .and_then(|template| {
                template.render([("directory", directory.as_str()), ("body", body)])
            })
            .expect("the memory source template and its arguments must agree")
    }
}
