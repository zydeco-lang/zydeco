use logos::Logos;
use std::fmt::{Display, Debug};

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

    #[regex("(F|Ret)")]
    RetType,
    #[regex("(U|Comp|Thunk)")]
    CompType,
    #[token("Bool")]
    BoolType,
    #[token("Int")]
    IntType,
    #[token("String")]
    StringType,
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
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

impl<'input> Display for Tok<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // self.fmt(f)
        Debug::fmt(self, f)
    }
}
