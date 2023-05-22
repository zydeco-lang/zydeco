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
    use super::*;
    #[test]
    fn it_works_1() {
        let source = "!(!1)";
        let mut arena = arena::Arena::default();
        let t = parser::SingleTermParser::new()
            .parse(&source, &mut arena, lexer::Lexer::new(&source))
            .unwrap();
        assert!(arena.terms.get(t).is_some());
    }
    #[test]
    fn it_works_2() {
        let source = "main { let x = 1 in ! exit x } end";
        let mut arena = arena::Arena::default();
        let _t = parser::TopLevelParser::new()
            .parse(&source, &mut arena, lexer::Lexer::new(&source))
            .unwrap();
    }
}
