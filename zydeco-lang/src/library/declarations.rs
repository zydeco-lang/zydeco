use crate::{
    lex::Lexer,
    parse::{parser::DeclarationsParser, syntax::Declare},
    statics::ctx::Ctx,
};

pub fn std_decls() -> Result<Vec<Declare>, String> {
    // Static linking. Resolve std library at compile time.
    // Probably won't work in the future if there are more library files to include.
    let std = include_str!("std.zydeco");
    DeclarationsParser::new()
        .parse(&std, Lexer::new(&std))
        .map_err(|e| e.to_string())
}

pub fn inject_ctx(ctx: &mut Ctx, decls: &Vec<Declare>) -> Result<(), String> {
    for decl in decls {
        ctx.decl(decl).map_err(|e| e.to_string())?;
    }
    Ok(())
}
