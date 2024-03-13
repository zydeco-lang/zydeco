use super::err::*;
use super::syntax::*;

pub struct Detective<'ctx> {
    pub spans: &'ctx SpanArenaBitter,
    pub ctx: &'ctx b::Ctx,
    pub tree: LayerTree,
    /// temporary path for the current layer
    path: NameRef<()>,
}
impl<'ctx> Detective<'ctx> {
    pub fn new(spans: &'ctx SpanArenaBitter, ctx: &'ctx b::Ctx) -> Detective<'ctx> {
        Detective { spans, ctx, path: NameRef::from(vec![]), tree: LayerTree::default() }
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
    pub fn run(mut self, top: &TopLevel) -> Result<LayerTree> {
        top.detect(&mut self)?;
        Ok(self.tree)
    }
}

/// collects the layers in the current package
pub trait DetectLayer {
    fn detect(&self, detective: &mut Detective) -> Result<()>;
}

impl DetectLayer for TopLevel {
    fn detect(&self, detective: &mut Detective) -> Result<()> {
        let TopLevel(decls) = self;
        for Modifiers { public: _, inner } in decls {
            use Declaration as Decl;
            match inner {
                Decl::Alias(decl) => {
                    let Alias { binder, bindee: _ } = decl;
                    binder.detect(detective)?;
                }
                Decl::Extern(decl) => {
                    let Extern { comp: _, binder, params: _, ty: _ } = decl;
                    binder.detect(detective)?;
                }
                Decl::Layer(decl) => {
                    let Layer { name, uses: _, top } = decl;
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

impl DetectLayer for PatId {
    fn detect(&self, detective: &mut Detective) -> Result<()> {
        let pat = detective.ctx.pats[*self].clone();
        match pat {
            Pattern::Ann(pat) => {
                let Ann { tm, ty: _ } = pat;
                tm.detect(detective)?;
            }
            Pattern::Hole(pat) => {
                let Hole = pat;
            }
            Pattern::Var(def) => {
                let name = detective.ctx.defs[def].clone();
                detective.register(name, def)?;
            }
            Pattern::Ctor(pat) => {
                let Ctor(_, arg) = pat;
                arg.detect(detective)?;
            }
            Pattern::Paren(pat) => {
                let Paren(pats) = pat;
                for pat in pats {
                    pat.detect(detective)?;
                }
            }
        }
        Ok(())
    }
}
