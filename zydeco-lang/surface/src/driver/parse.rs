use super::err::SurfaceError;
use crate::textual::{
    err::ParseError,
    lexer::Lexer,
    parser::TopLevelParser,
    syntax::{Ctx, TopLevel},
};
use std::{path::Path, rc::Rc};
use zydeco_utils::span::FileInfo;

pub struct ParseFile {
    pub top: TopLevel,
    pub ctx: Ctx,
}

impl ParseFile {
    pub fn run(path: impl AsRef<Path>) -> Result<ParseFile, SurfaceError> {
        let path = path.as_ref().to_owned();
        let source = std::fs::read_to_string(&path)
            .map_err(|_| SurfaceError::PathNotFound { path: path.clone() })?;
        let file_info = FileInfo::new(&source, Rc::new(path.clone()));

        let mut ctx = Ctx::default();
        let top = TopLevelParser::new().parse(&source, &mut ctx, Lexer::new(&source)).map_err(
            |error| {
                SurfaceError::ParseError(ParseError { error, file_info: &file_info }.to_string())
            },
        )?;
        ctx.span_map(&file_info);

        Ok(ParseFile { top, ctx })
    }
}

#[cfg(test)]
mod tests {
    // use super::*;
    #[test]
    fn it_works() -> anyhow::Result<()> {
        // let ParseFile { top, ctx: _ } = ParseFile::run("../../docs/monad_interpreter/cbv.zz")?;
        // println!("{:#?}", top);
        Ok(())
    }
}
