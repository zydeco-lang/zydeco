use logos::{Logos, SpannedIter};
use std::{
    fmt::{Debug, Display},
    ops::Range,
};

/// Tokens produced by the surface lexer.
#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(subpattern ident = r"[a-zA-Z0-9_]|'|\?|\+|\*|-|=|~")]
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
    #[regex(r#""[^"\\]*(?:\\.[^"\\]*)*""#)]
    StrLit(&'input str),
    #[regex(r#"'([ -~]|\\[nrt'|(\\)])'"#)]
    CharLit(&'input str),

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
}

impl LexicalToken {
    fn new(range: Range<usize>, kind: LexicalTokenKind) -> Self {
        Self { range, kind }
    }
}

/// A lexer view for source tooling.
///
/// Unlike [`Lexer`], this view retains comments and combines a nested block
/// comment into one source range. It deliberately reports only lexical roles;
/// later compiler phases may refine identifier tokens without having to
/// reproduce lexical recovery themselves.
pub struct LexicalTokens<'source> {
    inner: SpannedIter<'source, Tok<'source>>,
    source_len: usize,
    comment_start: Option<usize>,
    comment_depth: usize,
}

impl<'source> LexicalTokens<'source> {
    /// Observe all highlightable tokens in a source string.
    pub fn new(source: &'source str) -> Self {
        Self {
            inner: Tok::lexer(source).spanned(),
            source_len: source.len(),
            comment_start: None,
            comment_depth: 0,
        }
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
            | Tok::StrLit(_) | Tok::CharLit(_) => Kind::String,
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
            let Some((token, range)) = self.inner.next() else {
                return self.comment_start.take().map(|start| {
                    LexicalToken::new(start..self.source_len, LexicalTokenKind::Comment)
                });
            };
            let Ok(token) = token else {
                continue;
            };

            if self.comment_depth > 0 {
                match token {
                    | Tok::CommentOpen => self.comment_depth += 1,
                    | Tok::CommentClose => {
                        self.comment_depth -= 1;
                        if self.comment_depth == 0 {
                            let start = self
                                .comment_start
                                .take()
                                .expect("a nested comment has an opening range");
                            return Some(LexicalToken::new(
                                start..range.end,
                                LexicalTokenKind::Comment,
                            ));
                        }
                    }
                    | _ => {}
                }
                continue;
            }

            if matches!(token, Tok::CommentOpen) {
                self.comment_start = Some(range.start);
                self.comment_depth = 1;
                continue;
            }
            if let Some(kind) = Self::classify(&token) {
                return Some(LexicalToken::new(range, kind));
            }
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
            | Tok::CharLit(s) => write!(f, "CharLit(\'{}\')", s.escape_debug()),
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

/// Streaming lexer that skips comments and nested block comments.
pub struct Lexer<'source> {
    inner: SpannedIter<'source, Tok<'source>>,
    comment_depth: usize,
}

impl<'source> Lexer<'source> {
    /// Create a new lexer for a source string.
    pub fn new(source: &'source str) -> Self {
        Self { inner: Tok::lexer(source).spanned(), comment_depth: 0 }
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = (usize, Tok<'source>, usize);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.inner.next() {
                | Some((Ok(Tok::TextLine(_)), _)) => continue,
                | Some((Ok(Tok::CommentLine(_)), _)) => continue,
                | Some((Ok(Tok::CommentOpen), _)) => {
                    self.comment_depth += 1;
                    continue;
                }
                | Some((Ok(Tok::CommentClose), _)) => {
                    if self.comment_depth == 0 {
                        break None;
                    }
                    self.comment_depth -= 1;
                }
                | Some((Ok(_tok), _)) if self.comment_depth > 0 => continue,
                | Some((Ok(tok), range)) => break Some((range.start, tok, range.end)),
                | _ => break None,
            }
        }
    }
}

#[cfg(test)]
mod tooling_tests {
    use super::{LexicalToken, LexicalTokenKind, LexicalTokens};

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
}
