use super::syntax::*;
use crate::utils::{
    ann::AnnT,
    fmt::{Args, FmtArgs, FmtDefault},
};

impl<Ann: AnnT> FmtArgs for ZProgram<Ann> {
    fn fmt_args(&self, _args: Args) -> String {
        format!("{:?}", self)
    }
}

impl<Ann: AnnT> FmtArgs for ZValue<Ann> {
    fn fmt_args(&self, _args: Args) -> String {
        format!("{:?}", self)
    }
}

impl<Ann: AnnT> FmtArgs for ZCompute<Ann> {
    fn fmt_args(&self, _args: Args) -> String {
        format!("{:?}", self)
    }
}

impl FmtDefault for ZProgram<()> {}
impl FmtDefault for ZValue<()> {}
impl FmtDefault for ZCompute<()> {}
