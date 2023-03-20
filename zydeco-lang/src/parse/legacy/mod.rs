use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub parser, "/parse/legacy/parser.rs");

mod span;
mod fmt;
pub mod syntax;
