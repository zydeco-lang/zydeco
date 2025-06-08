use logos::{Logos, SpannedIter};
use std::fmt::{Debug, Display};

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"#.*\n")]
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
    #[token("@")]
    At,

    #[regex(r"--\|.*\n")]
    DocLine(&'input str),
    #[regex(r"--.*\n")]
    CommentLine(&'input str),
    #[token("/-")]
    CommentOpen,
    #[token("-/")]
    CommentClose,
    #[regex(".", priority = 0)]
    Unknown(&'input str),
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
            | Tok::DotDot => write!(f, ".."),
            | Tok::Arrow => write!(f, "->"),
            | Tok::Assign => write!(f, "<-"),
            | Tok::Hole => write!(f, "_"),
            | Tok::At => write!(f, "@"),
            | Tok::DocLine(s) => write!(f, "DocLine(\"{}\")", s.escape_debug()),
            | Tok::CommentLine(s) => write!(f, "CommentLine(\"{}\")", s.escape_debug()),
            | Tok::CommentOpen => write!(f, "/-"),
            | Tok::CommentClose => write!(f, "-/"),
            | Tok::Unknown(s) => write!(f, "Unknown(\"{}\")", s.escape_debug()),
        }
    }
}

pub struct Lexer<'source> {
    inner: SpannedIter<'source, Tok<'source>>,
    comment_depth: usize,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self { inner: Tok::lexer(&source).spanned(), comment_depth: 0 }
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = (usize, Tok<'source>, usize);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.inner.next() {
                | Some((Ok(Tok::DocLine(_)), _)) => continue,
                | Some((Ok(Tok::CommentLine(_)), _)) => continue,
                | Some((Ok(Tok::CommentOpen), _)) => {
                    self.comment_depth += 1;
                    continue;
                }
                | Some((Ok(Tok::CommentClose), _)) => {
                    if self.comment_depth <= 0 {
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

pub struct HashLexer<'source> {
    inner: Lexer<'source>,
}

impl<'source> HashLexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self { inner: Lexer::new(source) }
    }
    pub fn hash_string(self) -> Result<String, String> {
        let mut h = String::new();
        for (_, t, _) in self {
            h += &format!("{}", t);
        }
        Ok(h)
    }
}

impl<'source> Iterator for HashLexer<'source> {
    type Item = (usize, Tok<'source>, usize);

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            | Some((l, tok, r)) => Some((l, tok, r)),
            | _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_simple() {
        let src = "--| aeioa`s`nkl\nlet x = 1 in -- ` \n /- ` /- `x` \n-/\n\n -/ \n ret x`\n\n";
        let toks: Vec<_> = Lexer::new(src).map(|(_, x, _)| format!("{}", x)).collect();
        println!("{}", toks.join(", "))
    }
}
