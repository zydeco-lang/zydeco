pub mod token;

use self::token::Tok;
use logos::{Logos, SpannedIter};

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
