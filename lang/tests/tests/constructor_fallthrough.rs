use zydeco_tests::e2e_sources;

e2e_sources!({
    constructor_fallthrough => "tests/core/constructor-fallthrough.zy",
    constructor_fallback_sharing => "tests/core/constructor-fallback-sharing.zy",
});

#[test]
fn unmatched_tags_share_the_remaining_pattern_rows() {
    let source = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../lib/tests/core/constructor-fallback-sharing.zy");
    let backend = zydeco_cli::CommandCompiler::default().lower(&source).unwrap();
    let blocks = backend.assembly().arena().programs.len();
    assert!(
        blocks < 2000,
        "constructor fallthrough duplicated the remaining rows: {blocks} blocks"
    );
}
