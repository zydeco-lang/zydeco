//! Lexer and derived token metadata contracts.

use super::{Lexer, LexicalError, LexicalToken, LexicalTokenKind, LexicalTokens, Tok, TokenKind};
use logos::Logos;
use strum::VariantArray;

#[test]
fn every_fixed_spelling_lexes_to_its_declared_kind_and_formats_canonically() {
    TokenKind::VARIANTS.iter().copied().for_each(|kind| {
        for spelling in kind.source_spellings() {
            let mut tokens = Tok::lexer(spelling);
            let token = tokens.next().unwrap().unwrap();
            assert_eq!(token.kind(), kind, "{spelling:?}");
            assert!(tokens.next().is_none(), "{spelling:?}");
            assert_eq!(token.to_string(), kind.source_spelling().unwrap(), "{spelling:?}");
        }
    });
    assert_eq!(TokenKind::Define.source_spellings(), ["def", "define"]);
    assert_eq!(TokenKind::Define.source_spelling(), Some("define"));
    assert_eq!(TokenKind::from_parser_name("define"), Some(TokenKind::Define));
    assert_eq!(TokenKind::from_parser_name("def"), None);
}

#[test]
fn variable_tokens_keep_their_diagnostic_payloads_without_fake_source_spellings() {
    [
        ("Word", TokenKind::UpperIdent, "UpperIdent(Word)"),
        ("word", TokenKind::LowerIdent, "LowerIdent(word)"),
        ("+Some", TokenKind::CtorIdent, "CtorIdent(+Some)"),
        (".get", TokenKind::DtorIdent, "DtorIdent(.get)"),
        ("#field", TokenKind::FieldIdent, "FieldIdent(#field)"),
        ("1.5", TokenKind::FloatLit, "FloatLit(1.5)"),
        ("10", TokenKind::IntLit, "IntLit(10)"),
        ("\"text\"", TokenKind::StrLit, r#"StrLit("\"text\"")"#),
        ("'a'", TokenKind::CharLit, r"CharLit('\'a\'')"),
    ]
    .into_iter()
    .for_each(|(source, kind, diagnostic)| {
        let token = Tok::lexer(source).next().unwrap().unwrap();
        assert_eq!(token.kind(), kind);
        assert_eq!(token.to_string(), diagnostic);
        assert!(kind.source_spelling().is_none());
        assert!(kind.source_spellings().is_empty());
        assert!(kind.parser_name().is_some());
    });
}

#[test]
fn trivia_and_malformed_tokens_are_never_parser_expectations() {
    ["-- comment", "--| docs", "/-", "-/", "\"unfinished", "'ab'", "?"].into_iter().for_each(
        |source| {
            let token = Tok::lexer(source).next().unwrap().unwrap();
            let kind = token.kind();
            assert_eq!(kind.parser_name(), None, "{source:?}");
            assert_eq!(TokenKind::from_parser_name(&kind.to_string()), None);
        },
    );
    let token = Tok::lexer("let").next().unwrap().unwrap();
    assert_eq!(token.kind().parser_name(), Some("let"));
}

struct LexicalFixture<'source> {
    source: &'source str,
}

impl<'source> LexicalFixture<'source> {
    fn new(source: &'source str) -> Self {
        Self { source }
    }

    fn tokens(&self) -> Vec<LexicalToken> {
        LexicalTokens::new(self.source).collect()
    }

    fn text(&self, token: &LexicalToken) -> &'source str {
        &self.source[token.range.clone()]
    }
}

#[test]
fn tooling_tokens_retain_line_comments_at_end_of_file() {
    let fixture = LexicalFixture::new("begin --| documentation");
    let tokens = fixture.tokens();
    assert_eq!(tokens.len(), 2);
    assert_eq!(tokens[1].kind, LexicalTokenKind::TextBlock);
    assert_eq!(fixture.text(&tokens[1]), "--| documentation");
}

#[test]
fn tooling_tokens_combine_nested_and_unterminated_block_comments() {
    ["/- outer /- nested -/ tail -/", "/- unfinished"].into_iter().for_each(|source| {
        let fixture = LexicalFixture::new(source);
        let tokens = fixture.tokens();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, LexicalTokenKind::Comment);
        assert_eq!(fixture.text(&tokens[0]), source);
    });
}

#[test]
fn nested_comments_share_boundaries_and_do_not_lex_their_contents_as_code() {
    let comment = "/- 🦀 \"unclosed string -- line /- nested -/ tail -/";
    let source = format!("first {comment} second");
    let parsed = Lexer::new(&source).collect::<Result<Vec<_>, _>>().unwrap();
    assert!(matches!(
        parsed.as_slice(),
        [(_, Tok::LowerIdent("first"), _), (_, Tok::LowerIdent("second"), _)]
    ));
    let comments = LexicalTokens::new(&source)
        .filter(|token| token.kind == LexicalTokenKind::Comment)
        .collect::<Vec<_>>();
    assert_eq!(comments.len(), 1);
    assert_eq!(&source[comments[0].range.clone()], comment);

    let unfinished = "/- 🦀 /- inner -/";
    let error = Lexer::new(unfinished).next().unwrap().unwrap_err();
    assert_eq!(error.inner, LexicalError::UnterminatedBlockComment);
    assert_eq!(error.info.range(), 0..unfinished.len());
    assert!(LexicalTokens::new(unfinished).next().unwrap().is_opaque_at(unfinished.len()));
}

#[test]
fn lexical_errors_consume_their_ranges_and_continue_to_later_tokens() {
    let source = "first -/ 🦀 second";
    let mut lexer = Lexer::new(source);
    assert!(matches!(lexer.next(), Some(Ok((0, Tok::LowerIdent("first"), 5)))));
    let close = lexer.next().unwrap().unwrap_err();
    assert_eq!(close.inner, LexicalError::UnexpectedCommentClose);
    assert_eq!(close.info.range(), 6..8);
    let unknown = lexer.next().unwrap().unwrap_err();
    assert_eq!(unknown.inner, LexicalError::UnrecognizedToken);
    assert_eq!(&source[unknown.info.range()], "🦀");
    assert!(matches!(lexer.next(), Some(Ok((_, Tok::LowerIdent("second"), _)))));
    assert!(lexer.next().is_none());
    assert!(lexer.next().is_none());
}

#[test]
fn lexer_accepts_crlf_and_complete_escaped_literals() {
    ["first\r\nsecond", r#""a\"b""#, r"'\n'", r"'\''", r"'\\'", "'''", "'a'", "value'"]
        .into_iter()
        .for_each(|source| {
            assert!(Lexer::new(source).collect::<Result<Vec<_>, _>>().is_ok(), "{source:?}");
        });
    ["\"unfinished\\", "'\\", "''", "'ab'"].into_iter().for_each(|source| {
        assert!(Lexer::new(source).collect::<Result<Vec<_>, _>>().is_err(), "{source:?}");
    });
}
