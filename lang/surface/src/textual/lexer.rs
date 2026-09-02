use logos::Logos;
use std::{
    fmt::{Debug, Display},
    ops::Range,
};
use thiserror::Error;
use zydeco_utils::span::{Sp, Span};

/// Tokens produced by the surface lexer.
#[derive(Logos, zydeco_derive::TokenMetadata, Clone, Debug, PartialEq)]
#[token_metadata(kind = TokenKind)]
#[logos(skip r"[ \t\r\n\f]+")]
#[logos(subpattern ident = r"[a-zA-Z0-9_]|'|\?|\+|\*|-|=|~")]
#[logos(subpattern string_char = r#"[^"\\]|\\."#)]
pub enum Tok<'input> {
    #[regex(r"[A-Z](?&ident)*")]
    #[token_metadata(parser = "UpperId")]
    UpperIdent(&'input str),
    #[regex(r"[a-z](?&ident)*")]
    #[regex(r"_(?&ident)+")]
    #[token_metadata(parser = "LowerId")]
    LowerIdent(&'input str),
    #[regex(r"\+[A-Z](?&ident)*")]
    #[token_metadata(parser = "CtorId")]
    CtorIdent(&'input str),
    #[regex(r"\.[a-z](?&ident)*")]
    #[token_metadata(parser = "DtorId")]
    DtorIdent(&'input str),
    #[regex(r"\#[A-Za-z](?&ident)*")]
    #[regex(r"\#_(?&ident)+")]
    #[token_metadata(parser = "FieldId")]
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
    #[token_metadata(canonical = "define")]
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
    #[token_metadata(skip)]
    UnterminatedString(&'input str),
    #[regex(r#"'([ -~]|\\[nrt'|(\\)])'"#, priority = 3)]
    CharLit(&'input str),
    #[regex(r"'[^'\n\r]*'?", priority = 1)]
    #[token_metadata(skip)]
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
    #[token_metadata(skip)]
    TextLine(&'input str),
    #[regex(r"--[^\n]*\n?", allow_greedy = true)]
    #[token_metadata(skip)]
    CommentLine(&'input str),
    #[token("/-")]
    #[token_metadata(skip)]
    CommentOpen,
    #[token("-/")]
    #[token_metadata(skip)]
    CommentClose,
    #[regex(".", priority = 0)]
    #[token_metadata(skip)]
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
        let kind = self.kind();
        match self {
            | Tok::UpperIdent(s)
            | Tok::LowerIdent(s)
            | Tok::CtorIdent(s)
            | Tok::DtorIdent(s)
            | Tok::FieldIdent(s)
            | Tok::FloatLit(s)
            | Tok::IntLit(s) => write!(f, "{kind:?}({s})"),
            | Tok::StrLit(s)
            | Tok::UnterminatedString(s)
            | Tok::MalformedChar(s)
            | Tok::TextLine(s)
            | Tok::CommentLine(s)
            | Tok::Unknown(s) => write!(f, "{kind:?}(\"{}\")", s.escape_debug()),
            | Tok::CharLit(s) => write!(f, "{kind:?}('{}')", s.escape_debug()),
            | token => match kind.source_spelling() {
                | Some(spelling) => f.write_str(spelling),
                | None => Debug::fmt(token, f),
            },
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
mod tests;
