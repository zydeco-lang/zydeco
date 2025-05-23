use logos::{Logos, SpannedIter};
use std::fmt::{Debug, Display};

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"/\*([^*]|\*[^/])*\*/")]
#[logos(skip r"//.*\n")]
#[logos(skip r"#.*\n")]
#[logos(skip r"[ \t\n\f]+")]
pub enum Tok<'input> {
    #[regex(r"[A-Z]([a-zA-Z0-9_]|'|\?|\+|\*|-|=|~)*")]
    UpperIdent(&'input str),
    #[regex(r"[a-z]([a-zA-Z0-9_]|'|\?|\+|\*|-|=|~)*")]
    #[regex(r"_([a-zA-Z0-9_]|'|\?|\+|\*|-|=|~)+")]
    LowerIdent(&'input str),

    #[regex(r"\+[A-Z]([a-zA-Z0-9_]|'|\?|\+|\*|-|=|~)*")]
    CtorIdent(&'input str),
    #[regex(r"\.[a-z]([a-zA-Z0-9_]|'|\?|\+|\*|-|=|~)*")]
    DtorIdent(&'input str),

    #[token("pub")]
    Public,
    #[token("where")]
    Where,
    #[token("end")]
    End,
    #[token("module")]
    Module,
    #[token("root")]
    Root,
    #[token("use")]
    Use,
    #[token("data")]
    Data,
    #[token("codata")]
    Codata,
    #[token("alias")]
    Alias,
    #[token("def")]
    #[token("define")]
    Define,
    #[token("extern")]
    Extern,
    #[token("main")]
    Exec,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("do")]
    Do,
    #[token("do~")]
    DoTilde,
    #[token("ret")]
    Ret,
    #[token("monadic")]
    Monadic,
    #[token("monadically")]
    Monadically,
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
    #[token("..")]
    DotDot,
    #[token("->")]
    Arrow,
    #[token("<-")]
    Assign,
    #[token("_")]
    Hole,
}

impl Display for Tok<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Tok::UpperIdent(s) => write!(f, "UpperIdent({})", s),
            | Tok::LowerIdent(s) => write!(f, "LowerIdent({})", s),
            | Tok::CtorIdent(s) => write!(f, "CtorIdent({})", s),
            | Tok::DtorIdent(s) => write!(f, "DtorIdent({})", s),
            | Tok::Where => write!(f, "where"),
            | Tok::End => write!(f, "end"),
            | Tok::Public => write!(f, "pub"),
            | Tok::Module => write!(f, "module"),
            | Tok::Root => write!(f, "root"),
            | Tok::Use => write!(f, "use"),
            | Tok::Data => write!(f, "data"),
            | Tok::Codata => write!(f, "codata"),
            | Tok::Alias => write!(f, "alias"),
            | Tok::Define => write!(f, "define"),
            | Tok::Extern => write!(f, "extern"),
            | Tok::Exec => write!(f, "main"),
            | Tok::Let => write!(f, "let"),
            | Tok::In => write!(f, "in"),
            | Tok::Do => write!(f, "do"),
            | Tok::DoTilde => write!(f, "do~"),
            | Tok::Ret => write!(f, "ret"),
            | Tok::Monadic => write!(f, "monadic"),
            | Tok::Monadically => write!(f, "monadically"),
            | Tok::Fn => write!(f, "fn"),
            | Tok::Pi => write!(f, "pi"),
            | Tok::Fix => write!(f, "fix"),
            | Tok::Match => write!(f, "match"),
            | Tok::Comatch => write!(f, "comatch"),
            | Tok::Forall => write!(f, "forall"),
            | Tok::Sigma => write!(f, "sigma"),
            | Tok::Exists => write!(f, "exists"),
            | Tok::IntLit(s) => write!(f, "IntLit({})", s),
            | Tok::StrLit(s) => write!(f, "StrLit({})", s),
            | Tok::CharLit(s) => write!(f, "CharLit({})", s),
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
            | Tok::DotDot => write!(f, ".."),
            | Tok::Arrow => write!(f, "->"),
            | Tok::Assign => write!(f, "<-"),
            | Tok::Hole => write!(f, "_"),
        }
    }
}

pub struct Lexer<'source> {
    inner: SpannedIter<'source, Tok<'source>>,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self { inner: Tok::lexer(&source).spanned() }
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = (usize, Tok<'source>, usize);

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            | Some((Ok(tok), range)) => Some((range.start, tok, range.end)),
            | _ => None,
        }
    }
}

pub struct HashLexer<'source> {
    inner: SpannedIter<'source, Tok<'source>>,
}

impl<'source> HashLexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self { inner: Tok::lexer(&source).spanned() }
    }
    pub fn hash_string(self, info: &zydeco_utils::span::FileInfo) -> Result<String, String> {
        let mut h = String::new();
        for (l, t, r) in self {
            h += &format!(
                "{}",
                t.map_err(|()| {
                    let span = zydeco_utils::span::Span::new(l, r);
                    span.set_info(&info);
                    format!("{}", span)
                })?
            );
        }
        Ok(h)
    }
}

impl<'source> Iterator for HashLexer<'source> {
    type Item = (usize, Result<Tok<'source>, ()>, usize);

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            | Some((tok, range)) => Some((range.start, tok, range.end)),
            | _ => None,
        }
    }
}
