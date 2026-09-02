use super::*;

struct MetadataFixture;

impl MetadataFixture {
    fn expand(source: &str) -> syn::Result<TokenStream> {
        let input = syn::parse_str(source)?;
        TokenMetadata::parse(&input).map(|metadata| metadata.expand())
    }
}

#[test]
fn invalid_metadata_is_rejected_with_a_valid_counterpart() {
    [
        ("struct Tok;", "enum Tok { Word }", "TokenMetadata requires an enum"),
        ("enum Tok {}", "enum Tok { Word }", "a token enum must have variants"),
        (
            "#[token_metadata(kind = Tok)] enum Tok { Word }",
            "#[token_metadata(kind = TokenKind)] enum Tok { Word }",
            "the token and kind names must differ",
        ),
        (
            "enum Tok { #[token(\"word\")] #[token(\"alias\")] Word }",
            "enum Tok { #[token(\"word\")] #[token(\"alias\")] #[token_metadata(canonical = \"word\")] Word }",
            "multiple token spellings require a canonical choice",
        ),
        (
            "enum Tok { #[token(\"word\")] #[token_metadata(canonical = \"typo\")] Word }",
            "enum Tok { #[token(\"word\")] #[token_metadata(canonical = \"word\")] Word }",
            "canonical spelling must match a #[token] attribute",
        ),
        (
            "enum Tok { #[regex(\"[a-z]+\")] #[token_metadata(canonical = \"word\")] Ident }",
            "enum Tok { #[regex(\"[a-z]+\")] Ident }",
            "canonical spelling must match a #[token] attribute",
        ),
        (
            "enum Tok { #[token_metadata(skip, parser = \"Bad\")] Bad }",
            "enum Tok { #[token_metadata(skip)] Bad }",
            "a skipped token cannot name a parser terminal",
        ),
        (
            "enum Tok { #[token(\"\")] Empty }",
            "enum Tok { #[token(\"word\")] Word }",
            "token spellings must not be empty",
        ),
        (
            "enum Tok { #[token_metadata(parser = \"\")] Word }",
            "enum Tok { #[token_metadata(parser = \"Word\")] Word }",
            "parser terminal names must not be empty",
        ),
        (
            "enum Tok { #[token(\"word\")] Word, #[token_metadata(parser = \"word\")] Other }",
            "enum Tok { #[token(\"word\")] Word, #[token_metadata(parser = \"Other\")] Other }",
            "duplicate parser terminal name",
        ),
        (
            "#[token_metadata(kind = Kind, kind = Other)] enum Tok { Word }",
            "#[token_metadata(kind = Kind)] enum Tok { Word }",
            "expected one `kind = Name` setting",
        ),
        (
            "enum Tok { #[token_metadata(skip, skip)] Word }",
            "enum Tok { #[token_metadata(skip)] Word }",
            "unknown or duplicate token metadata setting",
        ),
        (
            "enum Tok { #[token_metadata(typo)] Word }",
            "enum Tok { #[token_metadata(skip)] Word }",
            "unknown or duplicate token metadata setting",
        ),
    ]
    .into_iter()
    .for_each(|(invalid, valid, error)| {
        assert_eq!(MetadataFixture::expand(invalid).unwrap_err().to_string(), error, "{invalid}");
        MetadataFixture::expand(valid).unwrap_or_else(|error| panic!("{valid}: {error}"));
    });
}

#[test]
fn logos_options_are_not_interpreted_as_metadata() {
    let source = r#"enum Tok {
        #[token("word", priority = 3)]
        #[token("alias", callback)]
        #[token_metadata(canonical = "word")]
        Word,
    }"#;
    let input = syn::parse_str(source).unwrap();
    let metadata = TokenMetadata::parse(&input).unwrap();
    assert_eq!(
        metadata.variants[0].spellings.iter().map(LitStr::value).collect::<Vec<_>>(),
        ["word", "alias"]
    );
    assert_eq!(metadata.variants[0].canonical.as_ref().unwrap().value(), "word");
}
