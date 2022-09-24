pub mod syntax;
pub mod dynamics;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser);
