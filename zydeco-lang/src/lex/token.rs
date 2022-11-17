use logos::Logos;
use std::fmt::{Debug, Display};

#[derive(Logos, Clone, Debug, PartialEq)]
pub enum Tok<'input> {
    #[regex(r"[A-Z]([a-zA-Z0-9_-]|'|\?)*")]
    IdentBig(&'input str),
    #[regex(r"[_a-z]([a-zA-Z0-9_-]|'|\?)*")]
    IdentSmall(&'input str),

    #[token("data")]
    Data,
    #[token("codata")]
    Codata,
    #[token("where")]
    Where,
    #[token("pub")]
    Pub,
    #[token("define")]
    Define,
    #[token("let")]
    Let,
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
    #[token("if")]
    If,
    #[token("else")]
    Else,

    #[token("true")]
    True,
    #[token("false")]
    False,
    #[regex(r"[\+-]?[0-9]+")]
    NumLit(&'input str),
    #[regex(r#""[^"\\]*(?:\\.[^"\\]*)*""#)]
    StrLit(&'input str),
    #[regex(r#"'[ -~]'"#)]
    CharLit(&'input str),

    #[regex("(F|Ret)")]
    RetType,
    #[regex("(U|Comp|Thunk)")]
    CompType,
    #[token("OS")]
    OSType,
    #[token("Bool")]
    BoolType,
    #[token("Int")]
    IntType,
    #[token("String")]
    StringType,
    #[token("Char")]
    CharType,
    #[token("Unit")]
    UnitType,

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
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,

    #[error]
    #[regex(r"#.*\n", logos::skip, priority = 2)]
    #[regex(r"[ \t\n\f]+", logos::skip, priority = 1)]
    Error,
}

impl<'input> Display for Tok<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tok::IdentBig(s) => write!(f, "IdentBig({})", s),
            Tok::IdentSmall(s) => write!(f, "IdentSmall({})", s),
            Tok::Data => write!(f, "Data"),
            Tok::Codata => write!(f, "Codata"),
            Tok::Where => write!(f, "Where"),
            Tok::Pub => write!(f, "Pub"),
            Tok::Define => write!(f, "Define"),
            Tok::Let => write!(f, "Let"),
            Tok::Do => write!(f, "Do"),
            Tok::Ret => write!(f, "Ret"),
            Tok::Fn => write!(f, "Fn"),
            Tok::Rec => write!(f, "Rec"),
            Tok::Match => write!(f, "Match"),
            Tok::Comatch => write!(f, "Comatch"),
            Tok::If => write!(f, "If"),
            Tok::Else => write!(f, "Else"),
            Tok::True => write!(f, "True"),
            Tok::False => write!(f, "False"),
            Tok::NumLit(s) => write!(f, "NumLit({})", s),
            Tok::StrLit(s) => write!(f, "StrLit({})", s),
            Tok::CharLit(s) => write!(f, "CharLit({})", s),
            Tok::RetType => write!(f, "RetType"),
            Tok::CompType => write!(f, "CompType"),
            Tok::OSType => write!(f, "OSType"),
            Tok::BoolType => write!(f, "BoolType"),
            Tok::IntType => write!(f, "IntType"),
            Tok::StringType => write!(f, "StringType"),
            Tok::CharType => write!(f, "CharType"),
            Tok::UnitType => write!(f, "UnitType"),
            Tok::ParenOpen => write!(f, "ParenOpen"),
            Tok::ParenClose => write!(f, "ParenClose"),
            Tok::BraceOpen => write!(f, "BraceOpen"),
            Tok::BraceClose => write!(f, "BraceClose"),
            // Tok::BracketOpen => write!(f, "BracketOpen"),
            // Tok::BracketClose => write!(f, "BracketClose"),
            Tok::Comma => write!(f, "Comma"),
            Tok::Colon => write!(f, "Colon"),
            Tok::Equals => write!(f, "Equals"),
            Tok::Semicolon => write!(f, "Semicolon"),
            Tok::Force => write!(f, "Force"),
            Tok::Branch => write!(f, "Branch"),
            Tok::Dot => write!(f, "Dot"),
            Tok::Arrow => write!(f, "Arrow"),
            Tok::Assign => write!(f, "Assign"),
            Tok::Plus => write!(f, "Plus"),
            Tok::Minus => write!(f, "Minus"),
            Tok::Star => write!(f, "Star"),

            Tok::Error => write!(f, "Error"),
        }
    }
}
