use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub parser, "/parse/parser.rs");

pub mod fmt;
pub mod syntax;
pub mod new_syntax;

pub use parser::{ExpressionParser, ZydecoParser};

use std::path::PathBuf;

pub struct ZydecoFile {
    pub path: PathBuf,
    pub content: String,
}
