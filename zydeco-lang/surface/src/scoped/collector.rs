use super::err::*;
use super::syntax::*;
use crate::bitter::syntax as b;
use crate::syntax::*;

pub struct Collector {
    pub spans: b::SpanArenaBitter,
    pub ctx: b::Ctx,
    pub global: InternalSymbols,
}

/// collects the symbols visible in the current package, regardless of their visibility
/// (i.e. `public` keyword only affects the visibility OUTSIDE the package)
pub trait Collect {
    fn collect(&self, collector: &Collector) -> Result<InternalSymbols>;
}

impl Collect for b::TopLevel {
    fn collect(&self, collector: &Collector) -> Result<InternalSymbols> {
        let b::TopLevel(decls) = self;
        let mut internal = InternalSymbols::new();
        let InternalSymbols { map } = &mut internal;
        for Modifiers { public: _, inner } in decls {
            use b::Declaration as Decl;
            match inner {
                Decl::Alias(b::Alias { binder, bindee: _ }) => {
                    for (b, d) in collector.ctx.pats[*binder].var_intro() {
                        let res = map.insert(b.clone(), Symbol::Def(d));
                        if let Some(_) = res {
                            let span = collector.spans[d].clone();
                            // Todo: record the span of the first definition
                            Err(ResolveError::DefineTwice(span.make(b)))?;
                        }
                    }
                }
                Decl::Extern(b::Extern { comp: _, binder, params: _, ty: _ }) => {
                    for (b, d) in collector.ctx.pats[*binder].var_intro() {
                        let inner = Symbol::Def(d);
                        let res = map.insert(b.clone(), inner);
                        if let Some(_) = res {
                            let span = collector.spans[d].clone();
                            // Todo: record the span of the first definition
                            Err(ResolveError::DefineTwice(span.make(b)))?;
                        }
                    }
                }
                Decl::Layer(b::Layer { name: _, uses: _, top: _ }) => {
                    // let inner = top.collect(collector)?;
                    // match map.get_mut(name) {
                    //     Some(Symbol::Module(m)) => {
                    //         for (k, v) in inner.map {
                    //             let res = m.insert(k.clone(), v);
                    //             // Todo: report the span of the first definition
                    //             if let Some(_) = res {
                    //                 // Err(ResolveError::DefineTwice(
                    //                 //     collector.spans[v].clone().make(k),
                    //                 // ))?;
                    //                 // Fixme: Report the span of a module
                    //                 panic!("Module name conflict: {:?}", k)
                    //             }
                    //         }
                    //     }
                    //     Some(Symbol::Def(_d)) => {
                    //         // Fixme: Report if the symbol is defined as a definition
                    //         panic!("Module name conflict: {:?}", name)
                    //     }
                    //     None => {
                    //         map.insert(name.clone(), Symbol::Module(inner));
                    //     }
                    // }
                }
                // Decl::UseDef(_) => todo!(),
                // Decl::UseBlock(_) => todo!(),
                Decl::Main(_) => todo!(),
            }
        }
        Ok(internal)
    }
}

pub trait VarIntro {
    fn var_intro(&self) -> Vec<(VarName, b::DefId)>;
}

impl VarIntro for b::Pattern {
    fn var_intro(&self) -> Vec<(VarName, b::DefId)> {
        todo!()
    }
}

impl VarIntro for b::UsePath {
    fn var_intro(&self) -> Vec<(VarName, b::DefId)> {
        todo!()
    }
}
