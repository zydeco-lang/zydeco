use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub parser, "/parse/legacy/parser.rs");

mod ann;
mod fmt;
pub mod syntax;
