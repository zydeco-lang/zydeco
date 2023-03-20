use super::token::Tok;
use crate::syntax::ann::{Cursor1, FileInfo};
use std::fmt::Display;

pub struct ParseError<'input>(
    pub lalrpop_util::ParseError<Cursor1, Tok<'input>, &'input str>,
    pub &'input FileInfo,
);

impl Display for ParseError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use lalrpop_util::ParseError::*;
        let ParseError(e, gen) = self;
        match e {
            User { ref error } => write!(f, "{error}"),
            InvalidToken { ref location } => {
                write!(f, "Invalid token at {:?}", gen.trans_span2(*location))
            }
            UnrecognizedEOF { ref location, ref expected } => {
                write!(
                    f,
                    "Unrecognized EOF found at {:?}{}",
                    gen.trans_span2(*location),
                    fmt_expected(expected)
                )
            }
            UnrecognizedToken {
                token: (ref start, ref token, ref end),
                ref expected,
            } => {
                write!(
                    f,
                    "Unrecognized token `{token}` found at {:?} - {:?}{}",
                    gen.trans_span2(*start),
                    gen.trans_span2(*end),
                    fmt_expected(expected)
                )
            }
            ExtraToken { token: (ref start, ref token, ref end) } => {
                write!(
                    f,
                    "Extra token `{token}` found at {:?} - {:?}",
                    gen.trans_span2(*start),
                    gen.trans_span2(*end),
                )
            }
        }
    }
}

fn fmt_expected(expected: &[String]) -> String {
    let mut res = format!("");
    if !expected.is_empty() {
        res += &format!("; ");
        for (i, e) in expected.iter().enumerate() {
            let sep = match i {
                0 => "Expected one of",
                _ if i < expected.len() - 1 => ",",
                // Last expected message to be written
                _ => " or",
            };
            res += &format!("{} {}", sep, e);
        }
    }
    res
}
