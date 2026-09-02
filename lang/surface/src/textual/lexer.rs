use logos::Logos;
use std::{
    fmt::{Debug, Display},
    ops::Range,
};
use thiserror::Error;
use zydeco_utils::span::{Sp, Span};

/// Tokens produced by the surface lexer.
#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+")]
#[logos(subpattern ident = r"[a-zA-Z0-9_]|'|\?|\+|\*|-|=|~")]
#[logos(subpattern string_char = r#"[^"\\]|\\."#)]
pub enum Tok<'input> {
    #[regex(r"[A-Z](?&ident)*")]
    UpperIdent(&'input str),
    #[regex(r"[a-z](?&ident)*")]
    #[regex(r"_(?&ident)+")]
    LowerIdent(&'input str),
    #[regex(r"\+[A-Z](?&ident)*")]
    CtorIdent(&'input str),
    #[regex(r"\.[a-z](?&ident)*")]
    DtorIdent(&'input str),
    #[regex(r"\#[A-Za-z](?&ident)*")]
    #[regex(r"\#_(?&ident)+")]
    FieldIdent(&'input str),

    #[token("end")]
    End,
    #[token("begin")]
    Begin,
    #[token("data")]
    Data,
    #[token("codata")]
    Codata,
    #[token("as")]
    As,
    #[token("def")]
    #[token("define")]
    Define,
    #[token("let")]
    Let,
    #[token("param")]
    Param,
    #[token("val")]
    Val,
    #[token("in")]
    In,
    #[token("that")]
    That,
    #[token("do")]
    Do,
    #[token("ret")]
    Ret,
    #[token("fn")]
    Fn,
    #[token("pi")]
    Pi,
    #[token("fix")]
    Fix,
    #[token("match")]
    Match,
    #[token("comatch")]
    Comatch,
    #[token("forall")]
    Forall,
    #[token("sigma")]
    Sigma,
    #[token("exists")]
    Exists,
    #[token("pack")]
    Pack,
    #[token("where")]
    Where,
    #[token("is")]
    Is,

    #[regex(r"[\+-]?(?:[0-9]+\.[0-9]+(?:[eE][\+-]?[0-9]+)?|[0-9]+[eE][\+-]?[0-9]+)")]
    FloatLit(&'input str),
    #[regex(r"[\+-]?[0-9]+")]
    IntLit(&'input str),
    #[regex(r#""(?&string_char)*""#)]
    StrLit(&'input str),
    #[regex(r#""(?&string_char)*\\?"#)]
    UnterminatedString(&'input str),
    #[regex(r#"'([ -~]|\\[nrt'|(\\)])'"#, priority = 3)]
    CharLit(&'input str),
    #[regex(r"'[^'\n\r]*'?", priority = 1)]
    MalformedChar(&'input str),

    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClose,
    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,
    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("::")]
    ColonColon,
    #[token("=")]
    Equals,
    #[token(";")]
    Semicolon,
    #[token("!")]
    Force,
    #[token("/")]
    Slash,
    #[token("|")]
    Branch,
    #[token("+")]
    Plus,
    #[token("*")]
    Star,
    #[token(".")]
    Dot,
    #[token("=>")]
    TermArrow,
    #[token("->")]
    TypeArrow,
    #[token("~>")]
    ViewArrow,
    #[token("|>")]
    PipeForward,
    #[token("<|")]
    PipeBackward,
    #[token("<-")]
    Assign,
    #[token("_")]
    Hole,
    #[token("@")]
    At,

    #[regex(r"--\|[^\n]*\n?", allow_greedy = true)]
    TextLine(&'input str),
    #[regex(r"--[^\n]*\n?", allow_greedy = true)]
    CommentLine(&'input str),
    #[token("/-")]
    CommentOpen,
    #[token("-/")]
    CommentClose,
    #[regex(".", priority = 0)]
    Unknown(&'input str),
}

/// Malformed source emitted as a token to the parser's ordinary recovery machinery.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Error)]
pub enum LexicalError {
    #[error("unrecognized source token")]
    UnrecognizedToken,
    #[error("block comment closing delimiter without an opening delimiter")]
    UnexpectedCommentClose,
    #[error("unterminated block comment")]
    UnterminatedBlockComment,
    #[error("unterminated string literal")]
    UnterminatedString,
    #[error("unterminated character literal")]
    UnterminatedCharacter,
    #[error("invalid character literal")]
    InvalidCharacter,
}

enum SourceToken<'source> {
    Syntax(Tok<'source>),
    Trivia { kind: LexicalTokenKind, open_ended: bool },
    Invalid(LexicalError),
}

/// Shared lexical boundaries for parsing, highlighting, and completion cursors.
struct SourceTokens<'source> {
    inner: logos::Lexer<'source, Tok<'source>>,
}

impl<'source> SourceTokens<'source> {
    fn new(source: &'source str) -> Self {
        Self { inner: Tok::lexer(source) }
    }

    fn block_comment(&mut self) -> SourceToken<'source> {
        let remaining = self.inner.remainder().as_bytes();
        let mut depth = 1;
        let mut offset = 0;
        while offset + 1 < remaining.len() {
            match &remaining[offset..offset + 2] {
                | b"/-" => {
                    depth += 1;
                    offset += 2;
                }
                | b"-/" => {
                    depth -= 1;
                    offset += 2;
                    if depth == 0 {
                        // ASCII delimiters end on UTF-8 boundaries, even in a Unicode comment.
                        self.inner.bump(offset);
                        return SourceToken::Trivia {
                            kind: LexicalTokenKind::Comment,
                            open_ended: false,
                        };
                    }
                }
                | _ => offset += 1,
            }
        }
        self.inner.bump(remaining.len());
        SourceToken::Invalid(LexicalError::UnterminatedBlockComment)
    }
}

impl<'source> Iterator for SourceTokens<'source> {
    type Item = (Range<usize>, SourceToken<'source>);

    fn next(&mut self) -> Option<Self::Item> {
        let token = match self.inner.next()? {
            | Ok(Tok::CommentOpen) => self.block_comment(),
            | Ok(Tok::CommentClose) => SourceToken::Invalid(LexicalError::UnexpectedCommentClose),
            | Ok(Tok::TextLine(text)) => SourceToken::Trivia {
                kind: LexicalTokenKind::TextBlock,
                open_ended: !text.ends_with('\n'),
            },
            | Ok(Tok::CommentLine(text)) => SourceToken::Trivia {
                kind: LexicalTokenKind::Comment,
                open_ended: !text.ends_with('\n'),
            },
            | Ok(Tok::UnterminatedString(_)) => {
                SourceToken::Invalid(LexicalError::UnterminatedString)
            }
            | Ok(Tok::MalformedChar(text)) => {
                SourceToken::Invalid(if text.len() > 1 && text.ends_with('\'') {
                    LexicalError::InvalidCharacter
                } else {
                    LexicalError::UnterminatedCharacter
                })
            }
            | Ok(Tok::Unknown(_)) | Err(()) => {
                SourceToken::Invalid(LexicalError::UnrecognizedToken)
            }
            | Ok(token) => SourceToken::Syntax(token),
        };
        Some((self.inner.span(), token))
    }
}

/// The lexical role of a source token, independent of a particular editor
/// protocol or theme.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LexicalTokenKind {
    UpperIdentifier,
    LowerIdentifier,
    Constructor,
    Destructor,
    Field,
    Keyword,
    Number,
    String,
    Comment,
    TextBlock,
    Operator,
    Punctuation,
    Hole,
    Attribute,
}

/// A source token retained for tooling, including tokens ignored by parsing.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LexicalToken {
    pub range: Range<usize>,
    pub kind: LexicalTokenKind,
    open_ended: bool,
}

impl LexicalToken {
    /// Whether an insertion cursor is inside a comment or quoted literal.
    /// An unfinished token also owns the cursor at its end, including EOF.
    pub fn is_opaque_at(&self, offset: usize) -> bool {
        matches!(
            self.kind,
            LexicalTokenKind::Comment | LexicalTokenKind::TextBlock | LexicalTokenKind::String
        ) && self.range.start < offset
            && (offset < self.range.end || (self.open_ended && offset == self.range.end))
    }
}

/// A lexer view for source tooling.
///
/// Unlike [`Lexer`], this view retains comments and combines a nested block
/// comment into one source range. It deliberately reports only lexical roles;
/// later compiler phases may refine identifier tokens without having to
/// reproduce lexical recovery themselves.
pub struct LexicalTokens<'source> {
    inner: SourceTokens<'source>,
}

impl<'source> LexicalTokens<'source> {
    /// Observe all highlightable tokens in a source string.
    pub fn new(source: &'source str) -> Self {
        Self { inner: SourceTokens::new(source) }
    }

    fn classify(tok: &Tok<'_>) -> Option<LexicalTokenKind> {
        use LexicalTokenKind as Kind;
        Some(match tok {
            | Tok::UpperIdent(_) => Kind::UpperIdentifier,
            | Tok::LowerIdent(_) => Kind::LowerIdentifier,
            | Tok::CtorIdent(_) => Kind::Constructor,
            | Tok::DtorIdent(_) => Kind::Destructor,
            | Tok::FieldIdent(_) => Kind::Field,
            | Tok::End
            | Tok::Begin
            | Tok::Data
            | Tok::Codata
            | Tok::As
            | Tok::Define
            | Tok::Let
            | Tok::Param
            | Tok::Val
            | Tok::In
            | Tok::That
            | Tok::Do
            | Tok::Ret
            | Tok::Fn
            | Tok::Pi
            | Tok::Fix
            | Tok::Match
            | Tok::Comatch
            | Tok::Forall
            | Tok::Sigma
            | Tok::Exists
            | Tok::Pack
            | Tok::Where
            | Tok::Is => Kind::Keyword,
            | Tok::FloatLit(_) | Tok::IntLit(_) => Kind::Number,
            | Tok::StrLit(_)
            | Tok::CharLit(_)
            | Tok::UnterminatedString(_)
            | Tok::MalformedChar(_) => Kind::String,
            | Tok::ParenOpen
            | Tok::ParenClose
            | Tok::BracketOpen
            | Tok::BracketClose
            | Tok::BraceOpen
            | Tok::BraceClose
            | Tok::Comma
            | Tok::Colon
            | Tok::ColonColon
            | Tok::Semicolon => Kind::Punctuation,
            | Tok::Equals
            | Tok::Force
            | Tok::Slash
            | Tok::Branch
            | Tok::Plus
            | Tok::Star
            | Tok::Dot
            | Tok::TermArrow
            | Tok::TypeArrow
            | Tok::ViewArrow
            | Tok::PipeForward
            | Tok::PipeBackward
            | Tok::Assign
            | Tok::CommentClose => Kind::Operator,
            | Tok::Hole => Kind::Hole,
            | Tok::At => Kind::Attribute,
            | Tok::TextLine(_) => Kind::TextBlock,
            | Tok::CommentLine(_) => Kind::Comment,
            | Tok::CommentOpen | Tok::Unknown(_) => return None,
        })
    }
}

impl Iterator for LexicalTokens<'_> {
    type Item = LexicalToken;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (range, token) = self.inner.next()?;
            let (kind, open_ended) = match token {
                | SourceToken::Syntax(token) => match Self::classify(&token) {
                    | Some(kind) => (kind, false),
                    | None => continue,
                },
                | SourceToken::Trivia { kind, open_ended } => (kind, open_ended),
                | SourceToken::Invalid(LexicalError::UnterminatedBlockComment) => {
                    (LexicalTokenKind::Comment, true)
                }
                | SourceToken::Invalid(
                    LexicalError::UnterminatedString | LexicalError::UnterminatedCharacter,
                ) => (LexicalTokenKind::String, true),
                | SourceToken::Invalid(LexicalError::InvalidCharacter) => {
                    (LexicalTokenKind::String, false)
                }
                | SourceToken::Invalid(_) => continue,
            };
            return Some(LexicalToken { range, kind, open_ended });
        }
    }
}

impl Display for Tok<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Tok::UpperIdent(s) => write!(f, "UpperIdent({})", s),
            | Tok::LowerIdent(s) => write!(f, "LowerIdent({})", s),
            | Tok::CtorIdent(s) => write!(f, "CtorIdent({})", s),
            | Tok::DtorIdent(s) => write!(f, "DtorIdent({})", s),
            | Tok::FieldIdent(s) => write!(f, "FieldIdent({})", s),
            | Tok::End => write!(f, "end"),
            | Tok::Begin => write!(f, "begin"),
            | Tok::Data => write!(f, "data"),
            | Tok::Codata => write!(f, "codata"),
            | Tok::As => write!(f, "as"),
            | Tok::Define => write!(f, "define"),
            | Tok::Let => write!(f, "let"),
            | Tok::Param => write!(f, "param"),
            | Tok::Val => write!(f, "val"),
            | Tok::In => write!(f, "in"),
            | Tok::That => write!(f, "that"),
            | Tok::Do => write!(f, "do"),
            | Tok::Ret => write!(f, "ret"),
            | Tok::Fn => write!(f, "fn"),
            | Tok::Pi => write!(f, "pi"),
            | Tok::Fix => write!(f, "fix"),
            | Tok::Match => write!(f, "match"),
            | Tok::Comatch => write!(f, "comatch"),
            | Tok::Forall => write!(f, "forall"),
            | Tok::Sigma => write!(f, "sigma"),
            | Tok::Exists => write!(f, "exists"),
            | Tok::Pack => write!(f, "pack"),
            | Tok::Where => write!(f, "where"),
            | Tok::Is => write!(f, "is"),
            | Tok::FloatLit(s) => write!(f, "FloatLit({})", s),
            | Tok::IntLit(s) => write!(f, "IntLit({})", s),
            | Tok::StrLit(s) => write!(f, "StrLit(\"{}\")", s.escape_debug()),
            | Tok::UnterminatedString(s) => {
                write!(f, "UnterminatedString(\"{}\")", s.escape_debug())
            }
            | Tok::CharLit(s) => write!(f, "CharLit(\'{}\')", s.escape_debug()),
            | Tok::MalformedChar(s) => write!(f, "MalformedChar(\"{}\")", s.escape_debug()),
            | Tok::ParenOpen => write!(f, "("),
            | Tok::ParenClose => write!(f, ")"),
            | Tok::BracketOpen => write!(f, "["),
            | Tok::BracketClose => write!(f, "]"),
            | Tok::BraceOpen => write!(f, "{{"),
            | Tok::BraceClose => write!(f, "}}"),
            | Tok::Comma => write!(f, ","),
            | Tok::Colon => write!(f, ":"),
            | Tok::ColonColon => write!(f, "::"),
            | Tok::Equals => write!(f, "="),
            | Tok::Semicolon => write!(f, ";"),
            | Tok::Force => write!(f, "!"),
            | Tok::Slash => write!(f, "/"),
            | Tok::Branch => write!(f, "|"),
            | Tok::Plus => write!(f, "+"),
            | Tok::Star => write!(f, "*"),
            | Tok::Dot => write!(f, "."),
            | Tok::TermArrow => write!(f, "=>"),
            | Tok::TypeArrow => write!(f, "->"),
            | Tok::ViewArrow => write!(f, "~>"),
            | Tok::PipeForward => write!(f, "|>"),
            | Tok::PipeBackward => write!(f, "<|"),
            | Tok::Assign => write!(f, "<-"),
            | Tok::Hole => write!(f, "_"),
            | Tok::At => write!(f, "@"),
            | Tok::TextLine(s) => write!(f, "TextLine(\"{}\")", s.escape_debug()),
            | Tok::CommentLine(s) => write!(f, "CommentLine(\"{}\")", s.escape_debug()),
            | Tok::CommentOpen => write!(f, "/-"),
            | Tok::CommentClose => write!(f, "-/"),
            | Tok::Unknown(s) => write!(f, "Unknown(\"{}\")", s.escape_debug()),
        }
    }
}

/// Streaming lexer that skips trivia and retains malformed source as typed errors.
/// An error consumes its range; subsequent calls continue after that range.
pub struct Lexer<'source> {
    inner: SourceTokens<'source>,
}

impl<'source> Lexer<'source> {
    /// Create a new lexer for a source string.
    pub fn new(source: &'source str) -> Self {
        Self { inner: SourceTokens::new(source) }
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = Result<(usize, Tok<'source>, usize), Sp<LexicalError>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (range, token) = self.inner.next()?;
            match token {
                | SourceToken::Trivia { .. } => continue,
                | SourceToken::Syntax(token) => return Some(Ok((range.start, token, range.end))),
                | SourceToken::Invalid(error) => {
                    return Some(Err(Span::new(range.start, range.end).make(error)));
                }
            }
        }
    }
}

#[cfg(test)]
mod tooling_tests {
    use super::{Lexer, LexicalError, LexicalToken, LexicalTokenKind, LexicalTokens, Tok};

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
}
