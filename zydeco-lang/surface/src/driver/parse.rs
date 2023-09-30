use super::err::SurfaceError;
use crate::textual::{
    err::ParseError,
    lexer::Lexer,
    parser::TopLevelParser,
    syntax::{Ctx, TopLevel},
};
use std::{
    path::{Path, PathBuf},
    rc::Rc,
};
use zydeco_utils::span::FileInfo;

pub struct ParseFile {
    path: PathBuf,
}
pub struct ParseFileOut {
    pub top: TopLevel,
    pub ctx: Ctx,
}

impl ParseFile {
    pub fn with_path(path: impl AsRef<Path>) -> Self {
        Self { path: path.as_ref().to_owned() }
    }
    pub fn run(&self) -> Result<ParseFileOut, SurfaceError> {
        let source = std::fs::read_to_string(&self.path)
            .map_err(|_| SurfaceError::PathNotFound { path: self.path.clone() })?;
        let file_info = FileInfo::new(&source, Rc::new(self.path.clone()));

        let mut ctx = Ctx::default();
        let top = TopLevelParser::new().parse(&source, &mut ctx, Lexer::new(&source)).map_err(
            |error| {
                SurfaceError::ParseError(ParseError { error, file_info: &file_info }.to_string())
            },
        )?;
        ctx.span_map(&file_info);

        Ok(ParseFileOut { top, ctx })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() -> anyhow::Result<()> {
        let pf = ParseFile::with_path("../../docs/monad_interpreter/cbv.zz");
        let ParseFileOut { top, ctx: _ } = pf.run()?;
        println!("{:#?}", top);
        Ok(())
    }
}
