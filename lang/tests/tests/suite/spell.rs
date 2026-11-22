use zydeco_tests::e2e_sources;

e2e_sources!({
    one_term => "spell/0-toplevel.zy",
    thunk_ret => "spell/1-thunk-ret.zy",
    data_codata => "spell/2-data-codata.zy",
    object => "spell/3-object.zy",
    y_combinator => "spell/4-y-combinator.zy",
    mutual_recursion => "spell/5-mutual-rec.zy",
    cps => "spell/6-cps.zy",
    optics => "spell/8-lense-prism-optics.zy",
});

// The call-by-need chapter's cache library has no builtin implementation, so
// the term cannot execute; unlike a library source, the chapter exports a
// computation rather than a value, so it is only analyzed.
#[test]
fn call_by_need_checks() {
    use zydeco_cli::CommandCompiler;

    let path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../lib/spell/7-call-by-need.zy");
    CommandCompiler::default()
        .analyze(&path)
        .unwrap_or_else(|error| panic!("Error checking source {}: {error}", path.display()));
}
