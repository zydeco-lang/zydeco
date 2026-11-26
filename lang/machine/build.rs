include!("bundle_support.rs");

fn main() {
    emit_rerun_directives(&["bundle_support.rs"]);

    let fingerprint = emit_bundle(&BundleConfig {
        crate_label: "model",
        extra_files: &["bundle_support.rs"],
        manifest_rewrites: &[],
        emit_digest: false,
    });

    let output = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let clif_entry = format!("zydeco_clif_entry_{fingerprint:032x}");
    let clif_contract = format!(
        "pub const CLIF_ENTRY_SYMBOL: &str = {clif_entry:?};\n\
         #[cfg(feature = \"runtime\")]\nunsafe extern \"C\" {{\n\
         #[link_name = {clif_entry:?}]\n\
         pub fn clif_entry(vmctx: *mut ClifVmCtxHeader);\n}}\n",
    );
    fs::write(output.join("clif_contract.rs"), clif_contract)
        .expect("write cranelift contract identity");
}
