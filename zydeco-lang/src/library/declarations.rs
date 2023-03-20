use crate::{
    parse::{
        legacy::{parser::DeclarationsParser, syntax::Declare},
        Lexer,
    },
    statics::Ctx,
    syntax::span::{FileInfo, SpanHolder},
};
use std::{path::PathBuf, rc::Rc};

pub fn std_decls() -> Result<Vec<Declare>, String> {
    // Static linking. Resolve std library at compile time.
    // Probably won't work in the future if there are more library files to include.
    let std = include_str!("std.zydeco");
    let mut ds = DeclarationsParser::new()
        .parse(&std, Lexer::new(&std))
        .map_err(|e| e.to_string())?;
    let path_rc = Rc::new(PathBuf::from("zydeco-lang/src/library/std.zydeco"));
    let file_info = FileInfo::new(&std, path_rc.clone());
    for d in &mut ds {
        d.span_map_mut(|ann| {
            ann.set_info(&file_info);
        });
    }
    Ok(ds)
}

pub fn inject_ctx(ctx: &mut Ctx, decls: &Vec<Declare>) -> Result<(), String> {
    for decl in decls {
        ctx.decl(decl).map_err(|e| e.to_string())?;
    }
    Ok(())
}
