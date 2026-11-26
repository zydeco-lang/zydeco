include!("../machine/bundle_support.rs");

fn main() {
    emit_rerun_directives(&["../machine/bundle_support.rs"]);

    if env::var_os("CARGO_FEATURE_BUNDLE").is_none() {
        return;
    }

    emit_bundle(&BundleConfig {
        crate_label: "abi",
        extra_files: &[],
        manifest_rewrites: &[
            ManifestRewrite {
                find: "serde = { workspace = true, optional = true }",
                replace: None,
            },
            ManifestRewrite { find: r#"serde = ["dep:serde"]"#, replace: Some("serde = []") },
            ManifestRewrite {
                find: "zydeco-machine = { workspace = true }",
                replace: Some(r#"zydeco-machine = { path = "../machine" }"#),
            },
        ],
        emit_digest: true,
    });
}
