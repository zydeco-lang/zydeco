use logos::Logos;
use strum::VariantArray;
use zydeco_derive::TokenMetadata;

#[derive(Logos, TokenMetadata, Debug, PartialEq)]
#[token_metadata(kind = FixtureKind)]
enum Fixture<'source> {
    #[token("alias")]
    #[token("word", priority = 3)]
    #[token_metadata(canonical = "word")]
    Word,
    #[regex("[a-z]+")]
    #[token_metadata(parser = "Identifier")]
    Ident(&'source str),
    #[token(" ")]
    #[token_metadata(skip)]
    Space,
}

#[test]
fn kinds_and_spellings_are_generated_from_the_same_declaration() {
    assert_eq!(FixtureKind::VARIANTS, [FixtureKind::Word, FixtureKind::Ident, FixtureKind::Space]);
    assert_eq!(FixtureKind::Word.source_spellings(), ["alias", "word"]);
    assert_eq!(FixtureKind::Word.source_spelling(), Some("word"));
    for source in FixtureKind::Word.source_spellings() {
        let token = Fixture::lexer(source).next().unwrap().unwrap();
        assert_eq!(token.kind(), FixtureKind::Word);
    }
    let token = Fixture::lexer("name").next().unwrap().unwrap();
    assert_eq!(token, Fixture::Ident("name"));
    assert_eq!(token.kind(), FixtureKind::Ident);
    assert_eq!(token.kind().source_spelling(), None);
    assert!(token.kind().source_spellings().is_empty());
    assert_eq!(token.kind().parser_name(), Some("Identifier"));
    assert_eq!(token.kind().to_string(), "Identifier");
    assert_eq!(FixtureKind::from_parser_name("Identifier"), Some(FixtureKind::Ident));
    assert_eq!(FixtureKind::from_parser_name("word"), Some(FixtureKind::Word));
    assert_eq!(FixtureKind::from_parser_name("alias"), None);
    assert_eq!(FixtureKind::from_parser_name("unknown"), None);
}

#[test]
fn skipped_tokens_keep_lexical_metadata_without_becoming_expectations() {
    let token = Fixture::lexer(" ").next().unwrap().unwrap();
    assert_eq!(token.kind(), FixtureKind::Space);
    assert_eq!(token.kind().source_spelling(), Some(" "));
    assert_eq!(token.kind().parser_name(), None);
    assert_eq!(FixtureKind::from_parser_name(" "), None);
    assert_eq!(FixtureKind::from_parser_name("Space"), None);
    assert_eq!(FixtureKind::from_parser_name("Identifier"), Some(FixtureKind::Ident));
}

#[derive(TokenMetadata)]
enum Generic<'source, T, const N: usize> {
    Unit,
    Tuple(&'source T),
    Named {
        values: &'source [T; N],
    },
    #[cfg(any())]
    Disabled,
}

#[test]
fn kinds_erase_all_field_shapes_and_generic_parameters() {
    let values = [1, 2];
    let tuple: Generic<'_, i32, 2> = Generic::Tuple(&values[0]);
    let named = Generic::Named { values: &values };
    assert_eq!(tuple.kind(), GenericKind::Tuple);
    assert_eq!(named.kind(), GenericKind::Named);
    assert_eq!(Generic::<i32, 2>::Unit.kind(), GenericKind::Unit);
    assert_eq!(GenericKind::VARIANTS.len(), 3);
    // Erasing the fields must not affect the original token's payload.
    assert!(matches!(tuple, Generic::Tuple(value) if *value == 1));
    assert!(matches!(named, Generic::Named { values } if *values == [1, 2]));
}
