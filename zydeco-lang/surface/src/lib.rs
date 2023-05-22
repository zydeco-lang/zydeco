pub mod arena;
pub mod syntax;
pub mod lexer;
#[allow(clippy::all)]
pub mod parser {
    use lalrpop_util::lalrpop_mod;
    lalrpop_mod!(parser_impl, "/parser.rs");
    pub use parser_impl::*;
}
pub mod escape;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        use super::*;
        let source = "!(!1)";
        let mut arena = arena::Arena::default();
        let t = parser::ZydecoParser::new()
            .parse(&source, &mut arena, lexer::Lexer::new(&source))
            .unwrap();
        assert!(arena.terms.get(t).is_some());
    }
}
