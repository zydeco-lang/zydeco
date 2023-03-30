use logos::Logos;
use std::fmt::{Debug, Display};

#[derive(Logos, Clone, Debug, PartialEq)]
pub enum Tok<'input> {
    #[regex(r"[A-Z]([a-zA-Z0-9_]|'|\?|\+|\*|-|=|~)*")]
    UpperIdent(&'input str),
    #[regex(r"(([_a-z]|\?|\*|=)([a-zA-Z0-9_]|'|\?|\+|\*|-|=|~)*)")]
    #[regex(r"((\+|\-)([a-zA-Z_]|'|\?|\+|\*|-|=|~)*)")]
    LowerIdent(&'input str),

    #[token("module")]
    Module,
    #[token("where")]
    Where,
    #[token("end")]
    End,
    #[token("pub")]
    Pub,
    #[regex(r"(ext|extern)")]
    Extern,
    #[regex(r"(def|define)")]
    Define,
    #[token("data")]
    Data,
    #[token("codata")]
    Codata,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("do")]
    Do,
    #[token("ret")]
    Ret,
    #[token("fn")]
    Fn,
    #[token("rec")]
    Rec,
    #[token("match")]
    Match,
    #[token("comatch")]
    Comatch,
    #[token("forall")]
    Forall,
    #[token("exists")]
    Exists,
    #[token("@")]
    At,
    #[token("pack")]
    Pack,

    #[regex(r"[\+-]?[0-9]+")]
    NumLit(&'input str),
    #[regex(r#""[^"\\]*(?:\\.[^"\\]*)*""#)]
    StrLit(&'input str),
    #[regex(r#"'([ -~]|\\[nrt'|(\\)])'"#)]
    CharLit(&'input str),
    #[token("VType")]
    VType,
    #[token("CType")]
    CType,

    #[regex("(F|Ret)")]
    RetType,
    #[regex("(U|Comp|Thunk)")]
    CompType,

    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClose,
    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,
    // #[token("[")]
    // BracketOpen,
    // #[token("]")]
    // BracketClose,
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
    #[token("|")]
    Branch,
    #[token(".")]
    Dot,
    #[token("->")]
    Arrow,
    #[token("<-")]
    Assign,
    #[token("_?")]
    Underscore,

    #[error]
    #[regex(r"#.*\n", logos::skip, priority = 2)]
    #[regex(r"[ \t\n\f]+", logos::skip, priority = 1)]
    Error,
}

impl<'input> Display for Tok<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tok::UpperIdent(s) => write!(f, "UpperIdentifier({})", s),
            Tok::LowerIdent(s) => write!(f, "LowerIdentifier({})", s),
            Tok::Module => write!(f, "module"),
            Tok::Where => write!(f, "where"),
            Tok::End => write!(f, "end"),
            Tok::Pub => write!(f, "pub"),
            Tok::Extern => write!(f, "extern"),
            Tok::Define => write!(f, "define"),
            Tok::Data => write!(f, "data"),
            Tok::Codata => write!(f, "codata"),
            Tok::Let => write!(f, "let"),
            Tok::In => write!(f, "in"),
            Tok::Do => write!(f, "do"),
            Tok::Ret => write!(f, "ret"),
            Tok::Fn => write!(f, "fn"),
            Tok::Rec => write!(f, "rec"),
            Tok::Match => write!(f, "match"),
            Tok::Comatch => write!(f, "comatch"),
            Tok::Forall => write!(f, "Forall"),
            Tok::Exists => write!(f, "Exists"),
            Tok::At => write!(f, "@"),
            Tok::Pack => write!(f, "exists"),
            Tok::NumLit(s) => write!(f, "NumLiteral({})", s),
            Tok::StrLit(s) => write!(f, "StrLiteral({})", s),
            Tok::CharLit(s) => write!(f, "CharLiteral({})", s),
            Tok::VType => write!(f, "VType"),
            Tok::CType => write!(f, "CType"),
            Tok::RetType => write!(f, "F"),
            Tok::CompType => write!(f, "U"),
            Tok::ParenOpen => write!(f, "("),
            Tok::ParenClose => write!(f, ")"),
            Tok::BraceOpen => write!(f, "{{"),
            Tok::BraceClose => write!(f, "}}"),
            // Tok::BracketOpen => write!(f, "["),
            // Tok::BracketClose => write!(f, "]"),
            Tok::Comma => write!(f, ","),
            Tok::Colon => write!(f, ":"),
            Tok::ColonColon => write!(f, "::"),
            Tok::Equals => write!(f, "="),
            Tok::Semicolon => write!(f, ";"),
            Tok::Force => write!(f, "!"),
            Tok::Branch => write!(f, "|"),
            Tok::Dot => write!(f, "."),
            Tok::Arrow => write!(f, "->"),
            Tok::Assign => write!(f, "<-"),
            Tok::Underscore => write!(f, "_?"),

            Tok::Error => write!(f, "Error"),
        }
    }
}
