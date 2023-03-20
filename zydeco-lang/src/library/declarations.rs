use crate::{
    parse::{
        legacy::{parser::DeclarationsParser, syntax::Declare},
        Lexer,
    },
    statics::ctx::Ctx,
    syntax::ann::{AnnHolder, FileInfo},
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
    let file_info = FileInfo::new(&std);
    for d in &mut ds {
        d.ann_map_mut(|ann| {
            ann.set_span2(&file_info);
            ann.path.set(path_rc.clone()).unwrap();
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
