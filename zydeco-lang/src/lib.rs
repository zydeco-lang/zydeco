pub mod syntax;

pub mod parse {
    use lalrpop_util::lalrpop_mod;
    lalrpop_mod!(pub parser, "/parse/parser.rs");
    pub mod token;
    pub mod syntax;
    pub mod err;
    pub mod legacy;
    mod span;

    pub use legacy::parser::{ExpressionParser, ZydecoParser};

    use logos::{Logos, SpannedIter};
    use token::Tok;

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
            self.inner.next().map(|(tok, range)| (range.start, tok, range.end))
        }
    }

    /// Expand escape characters in a string literal, converting the source code
    /// representation to the text it represents. The `idx0` argument should be the
    /// position in the input stream of the first character of `text`, the position
    /// after the opening double-quote.
    /// https://github.com/lalrpop/lalrpop/blob/d09a1433d181de12fd1ebfc89c2ecaeed734943c/lalrpop/src/tok/mod.rs#L749
    pub fn apply_string_escapes(code: &str) -> String {
        if !code.contains('\\') {
            code.into()
        } else {
            let mut iter = code.char_indices();
            let mut text = String::new();
            while let Some((_, mut ch)) = iter.next() {
                if ch == '\\' {
                    // The parser should never have accepted an ill-formed string
                    // literal, so we know it can't end in a backslash.
                    let (_, next_ch) = iter.next().unwrap();
                    ch = match next_ch {
                        '\\' | '\"' => next_ch,
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        _ => next_ch,
                    }
                }
                text.push(ch);
            }
            text.into()
        }
    }
}

pub mod statics {
    pub mod syntax;
    mod resolve;
    pub mod err;
    mod legacy;
    mod elab;
    pub mod tyck;
    mod fmt;

    pub use self::legacy::{ctx::Ctx, err::TypeCheckError, tyck::TypeCheck};
}

pub mod library {
    pub mod syntax;
    pub mod legacy;
    mod link;
    pub mod builtins;

    pub use legacy::{declarations, linker};
}

pub mod dynamics {
    pub mod syntax;
    mod legacy;
    mod eval;

    pub use legacy::{
        env::Env,
        eval::*,
        syntax::{PrimComp, ZCompute, ZValue},
    };
}

pub mod utils {
    pub mod fmt;
    pub mod span;
    pub mod never;
    pub mod wrappers;
    pub mod monoid;
}

pub mod zydeco;
