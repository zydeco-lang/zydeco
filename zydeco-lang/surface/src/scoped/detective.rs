use super::err::*;
use super::syntax::*;
use crate::bitter::syntax as b;

pub struct Detective<'ctx> {
    pub spans: &'ctx b::SpanArenaBitter,
    pub ctx: &'ctx b::Ctx,
    pub tree: LayerTree,
    /// temporary path for the current layer
    path: NameRef<()>,
}
impl<'ctx> Detective<'ctx> {
    pub fn new(spans: &'ctx b::SpanArenaBitter, ctx: &'ctx b::Ctx) -> Detective<'ctx> {
        Detective { spans, ctx, path: b::NameRef::from(vec![]), tree: LayerTree::default() }
    }
    fn register(&mut self, name: VarName, def: DefId) -> Result<()> {
        let res =
            self.tree.layer.entry(self.path.clone().into()).or_default().insert(name.clone(), def);
        if let Some(def_) = res {
            let span = &self.spans.defs[def];
            let name_ = self.ctx.defs[def_].clone();
            let span_ = &self.spans.defs[def_];
            Err(ResolveError::DuplicateDefinition(span_.make(name_), span.make(name)))?;
        }
        Ok(())
    }
    pub fn run(mut self, top: &b::TopLevel) -> Result<LayerTree> {
        top.detect(&mut self)?;
        Ok(self.tree)
    }
}

/// collects the layers in the current package
pub trait DetectLayer {
    fn detect(&self, detective: &mut Detective) -> Result<()>;
}

impl DetectLayer for b::TopLevel {
    fn detect(&self, detective: &mut Detective) -> Result<()> {
        let b::TopLevel(decls) = self;
        for b::Modifiers { public: _, inner } in decls {
            use b::Declaration as Decl;
            match inner {
                Decl::Alias(decl) => {
                    let b::Alias { binder, bindee: _ } = decl;
                    binder.detect(detective)?;
                }
                Decl::Extern(decl) => {
                    let b::Extern { comp: _, binder, params: _, ty: _ } = decl;
                    binder.detect(detective)?;
                }
                Decl::Layer(decl) => {
                    let b::Layer { name, uses: _, top } = decl;
                    if let Some(name) = name {
                        let old_path = detective.path.clone();
                        detective.path.extend(name.clone().into_iter());
                        top.detect(detective)?;
                        detective.path = old_path;
                    }
                }
                Decl::UseDef(_decl) => {
                    unimplemented!("use-defs unimplemented")
                }
                Decl::Main(_) => {}
            }
        }
        Ok(())
    }
}

impl DetectLayer for b::PatId {
    fn detect(&self, detective: &mut Detective) -> Result<()> {
        let pat = detective.ctx.pats[*self].clone();
        match pat {
            b::Pattern::Ann(pat) => {
                let b::Ann { tm, ty: _ } = pat;
                tm.detect(detective)?;
            }
            b::Pattern::Hole(pat) => {
                let b::Hole = pat;
            }
            b::Pattern::Var(def) => {
                let name = detective.ctx.defs[def].clone();
                detective.register(name, def)?;
            }
            b::Pattern::Ctor(pat) => {
                let b::Ctor(_, arg) = pat;
                arg.detect(detective)?;
            }
            b::Pattern::Paren(pat) => {
                let b::Paren(pats) = pat;
                for pat in pats {
                    pat.detect(detective)?;
                }
            }
        }
        Ok(())
    }
}
