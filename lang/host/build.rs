include!("../machine/bundle_support.rs");

fn main() {
    emit_rerun_directives(&["../machine/bundle_support.rs"]);

    if env::var_os("CARGO_FEATURE_BUNDLE").is_none() {
        return;
    }

    emit_bundle(&BundleConfig {
        crate_label: "host",
        extra_files: &[],
        manifest_rewrites: &[
            ManifestRewrite {
                find: "zydeco-machine = { workspace = true }",
                replace: Some(r#"zydeco-machine = { path = "../machine" }"#),
            },
            ManifestRewrite {
                find: "zydeco-abi = { workspace = true }",
                replace: Some(r#"zydeco-abi = { path = "../abi", default-features = false }"#),
            },
        ],
        emit_digest: true,
    });
}
