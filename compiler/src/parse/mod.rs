use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub parser, "/parse/parser.rs");

pub mod fmt;
pub mod syntax;

pub use parser::VitProgramParser;
