use super::err::*;
use super::syntax::*;
use crate::bitter::syntax as b;
// use crate::syntax::*;s

pub struct Detective {
    pub spans: b::SpanArenaBitter,
    pub ctx: b::Ctx,
    pub tree: LayerTree,
}

/// collects the layers in the current package
pub trait DetectLayer {
    fn detect(&self, path: NameRef<()>, detective: &mut Detective) -> Result<()>;
}

impl DetectLayer for b::TopLevel {
    fn detect(&self, _path: NameRef<()>, _detective: &mut Detective) -> Result<()> {
        todo!()
    }
}
