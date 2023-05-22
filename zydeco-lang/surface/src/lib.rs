pub mod files {
    pub mod driver;
}

pub mod textual {
    pub mod arena;
    pub mod syntax;
    pub mod lexer;
    #[allow(clippy::all)]
    pub mod parser {
        use lalrpop_util::lalrpop_mod;
        lalrpop_mod!(parser_impl, "/textual/parser.rs");
        pub use parser_impl::*;
    }
    pub mod escape;

    #[cfg(test)]
    mod tests;
}

pub mod binded {
    pub mod resolver;
}
