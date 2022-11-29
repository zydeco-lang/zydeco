use crate::{
    lex::token::Tok,
    parse::{parser::DeclarationsParser, syntax::Declare},
};
use logos::Logos;

pub fn std_decls() -> Result<Vec<Declare<()>>, String> {
    // Static linking. Resolve std library at compile time.
    // Probably won't work in the future if there are more library files to include.
    let std = include_str!("std.zydeco");
    let lexer = Tok::lexer(&std)
        .spanned()
        .map(|(tok, range)| (range.start, tok, range.end));
    DeclarationsParser::new().parse(&std, lexer).map_err(|e| e.to_string())
}
