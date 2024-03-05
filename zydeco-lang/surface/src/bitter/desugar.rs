use serde::de;

use crate::{
    arena::*,
    bitter::syntax as b,
    textual::syntax::{self as t},
};

pub trait Desugar {
    type Out;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out;
}

pub struct DesugarIn {
    pub spans: SpanArena,
    pub ctx: t::Ctx,
    pub top: t::TopLevel,
}

pub struct Desugarer {
    pub spans: SpanArena,
    pub tctx: t::Ctx,
    pub bctx: b::Ctx,
}

pub struct DesugarOut {
    pub spans: SpanArena,
    pub ctx: b::Ctx,
    pub top: b::TopLevel,
}

impl DesugarIn {
    pub fn run(mut self) -> DesugarOut {
        let mut desugarer =
            Desugarer { spans: self.spans, tctx: self.ctx, bctx: b::Ctx::default() };
        let top = self.top.desugar(&mut desugarer);
        let Desugarer { bctx: ctx, spans, .. } = desugarer;
        DesugarOut { spans, ctx, top }
    }
}

impl<T> Desugar for Vec<T>
where
    T: Desugar,
{
    type Out = Vec<T::Out>;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        self.into_iter().map(|x| x.desugar(desugarer)).collect()
    }
}

impl Desugar for t::TopLevel {
    type Out = b::TopLevel;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let t::TopLevel(decls) = self;
        let mut decls_new = Vec::new();
        for t::Modifiers { public, inner } in decls {
            use t::Declaration as Decl;
            let inner = match inner {
                Decl::DataDef(decl) => {
                    let t::DataDef { name, params, def } = decl;
                    let span = desugarer.spans.defs[name].clone();
                    // name -> pat
                    let name = name.desugar(desugarer);
                    let pat = desugarer.spans.pats.alloc(span.clone());
                    let pat = desugarer.pat(pat, name.into());
                    // params -> copattern
                    let params = params.desugar(desugarer);
                    let params = params
                        .into_iter()
                        .map(|param| {
                            // param -> copattern
                            let span = desugarer.spans.pats[param].clone();
                            let co = desugarer.spans.copats.alloc(span);
                            let param = desugarer.copat(co, param.into());
                            param
                        })
                        .collect::<Vec<_>>();
                    let co = desugarer.spans.copats.alloc(span.clone());
                    let params = desugarer.copat(co, b::App(params).into());
                    // def -> term
                    let def = span.make(def).desugar(desugarer);
                    // params & def -> abs term
                    let abs = desugarer.spans.terms.alloc(span.clone());
                    let abs = desugarer.term(abs, b::Abs(params, def).into());
                    // abs -> sealed
                    let sealed = desugarer.spans.terms.alloc(span.clone());
                    let sealed = desugarer.term(sealed, b::Sealed(abs).into());
                    // pat & sealed -> alias
                    b::Alias { binder: pat, bindee: sealed }.into()
                }
                Decl::CoDataDef(decl) => {
                    let t::CoDataDef { name, params, def } = decl;
                    let span = desugarer.spans.defs[name].clone();
                    // name -> pat
                    let name = name.desugar(desugarer);
                    let pat = desugarer.spans.pats.alloc(span.clone());
                    let pat = desugarer.pat(pat, name.into());
                    // params -> copattern
                    let params = params.desugar(desugarer);
                    let params = params
                        .into_iter()
                        .map(|param| {
                            // param -> copattern
                            let span = desugarer.spans.pats[param].clone();
                            let co = desugarer.spans.copats.alloc(span);
                            let param = desugarer.copat(co, param.into());
                            param
                        })
                        .collect::<Vec<_>>();
                    let co = desugarer.spans.copats.alloc(span.clone());
                    let params = desugarer.copat(co, b::App(params).into());
                    // def -> term
                    let def = span.make(def).desugar(desugarer);
                    // params & def -> abs term
                    let abs = desugarer.spans.terms.alloc(span.clone());
                    let abs = desugarer.term(abs, b::Abs(params, def).into());
                    // abs -> sealed
                    let sealed = desugarer.spans.terms.alloc(span.clone());
                    let sealed = desugarer.term(sealed, b::Sealed(abs).into());
                    // pat & sealed -> alias
                    b::Alias { binder: pat, bindee: sealed }.into()
                }
                Decl::Define(decl) => {
                    let t::Define(genbind) = decl;
                    let (pat, term) = genbind.desugar(desugarer);
                    // sealed
                    let sealed = desugarer.spans.terms.alloc(desugarer.spans.pats[pat].clone());
                    let sealed = desugarer.term(sealed, b::Sealed(term).into());
                    // pat & sealed -> alias
                    b::Alias { binder: pat, bindee: sealed }.into()
                }
                Decl::Alias(decl) => {
                    let t::Alias(genbind) = decl;
                    let (pat, term) = genbind.desugar(desugarer);
                    // pat & sealed -> alias
                    b::Alias { binder: pat, bindee: term }.into()
                }
                Decl::Extern(decl) => {
                    let t::Extern(t::GenBind { rec: _, comp: _, binder, params, ty, bindee: () }) =
                        decl;
                    b::Extern { binder, params, ty }.into()
                }
                Decl::Module(decl) => {
                    let t::Module { name, top } = decl;
                    let top = top.desugar(desugarer);
                    b::Module { name, top }.into()
                }
                Decl::UseDef(decl) => {
                    let t::UseDef(uses) = decl;
                    // Todo: traverse uses
                    b::UseDef(uses).into()
                }
                Decl::UseBlock(decl) => {
                    let t::UseBlock { uses, top } = decl;
                    // Todo: traverse uses
                    let top = top.desugar(desugarer);
                    b::UseBlock { uses, top }.into()
                }
                Decl::Main(decl) => {
                    let t::Main(term) = decl;
                    let term = term.desugar(desugarer);
                    b::Main(term).into()
                }
            };
            decls_new.push(b::Modifiers { public, inner })
        }
        b::TopLevel(decls_new)
    }
}

impl Desugar for t::DefId {
    type Out = b::DefId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let id = self;
        let def = desugarer.lookup_def(id);
        desugarer.def(id, def)
    }
}

impl Desugar for t::PatternId {
    type Out = b::PatternId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let id = self;
        let pat = desugarer.lookup_pat(id);
        use t::Pattern as Pat;
        todo!()
    }
}

impl Desugar for t::CoPatternId {
    type Out = b::CoPatternId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let id = self;
        let copat = desugarer.lookup_copat(id);
        use t::CoPattern as CoPat;
        todo!()
    }
}

impl Desugar for t::TermId {
    type Out = b::TermId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let id = self;
        let term = desugarer.lookup_term(id);
        use t::Term as Tm;
        match term {
            Tm::Ann(term) => {
                let t::Ann { tm, ty } = term;
                let tm = tm.desugar(desugarer);
                let ty = ty.desugar(desugarer);
                desugarer.term(id, b::Ann { tm, ty }.into())
            }
            Tm::Hole(term) => {
                let t::Hole = term;
                desugarer.term(id, b::Hole.into())
            }
            Tm::Var(name) => desugarer.term(id, b::Term::Var(name).into()),
            Tm::Paren(term) => {
                let t::Paren(terms) = term;

                // Todo: rec
                desugarer.term(id, b::Paren(terms).into())
            }
            Tm::Abs(term) => {
                let t::Abs(params, tail) = term;
                // Todo: rec
                desugarer.term(id, b::Abs(params, tail).into())
            }
            Tm::App(term) => {
                let t::App(terms) = term;
                // Todo: rec
                desugarer.term(id, b::App(terms).into())
            }
            Tm::Rec(term) => {
                let t::Rec(pat, term) = term;
                // Todo: rec
                desugarer.term(id, b::Rec(pat, term).into())
            }
            Tm::Pi(term) => {
                let t::Pi(params, ty) = term;
                // Todo: rec
                desugarer.term(id, b::Pi(params, ty).into())
            }
            Tm::Arrow(term) => {
                let t::Arrow(params, ty) = term;
                // Todo: rec
                desugarer.term(id, b::Arrow(params, ty).into())
            }
            Tm::Forall(term) => {
                let t::Forall(params, ty) = term;
                // Todo: rec
                desugarer.term(id, b::Forall(params, ty).into())
            }
            Tm::Sigma(term) => {
                let t::Sigma(params, ty) = term;
                // Todo: rec
                desugarer.term(id, b::Sigma(params, ty).into())
            }
            Tm::Prod(term) => {
                let t::Prod(terms) = term;
                // Todo: rec
                desugarer.term(id, b::Prod(terms).into())
            }
            Tm::Exists(term) => {
                let t::Exists(params, ty) = term;
                // Todo: rec
                desugarer.term(id, b::Exists(params, ty).into())
            }
            Tm::Thunk(term) => {
                let t::Thunk(term) = term;
                // Todo: rec
                desugarer.term(id, b::Thunk(term).into())
            }
            Tm::Force(term) => {
                let t::Force(term) = term;
                // Todo: rec
                desugarer.term(id, b::Force(term).into())
            }
            Tm::Ret(term) => {
                let t::Return(term) = term;
                // Todo: rec
                desugarer.term(id, b::Return(term).into())
            }
            Tm::Do(term) => {
                let t::Bind { binder, bindee, tail } = term;
                // Todo: rec
                desugarer.term(id, b::Bind { binder, bindee, tail }.into())
            }
            Tm::Let(term) => {
                let t::PureBind {
                    binding: t::GenBind { rec, comp, binder, params, ty, bindee },
                    tail,
                } = term;
                // Fixme: xxx
                todo!()
            }
            Tm::UseLet(term) => {
                let t::UseBind { uses, tail } = term;
                // Todo: rec
                desugarer.term(id, b::UseBind { uses, tail }.into())
            }
            Tm::Data(term) => {
                let t::Data { arms } = term;
                // Todo: rec
                todo!()
            }
            Tm::CoData(term) => {
                let t::CoData { arms } = term;
                // Todo: rec
                todo!()
            }
            Tm::Ctor(term) => {
                let t::Ctor(name, term) = term;
                // Todo: rec
                desugarer.term(id, b::Ctor(name, term).into())
            }
            Tm::Match(term) => {
                let t::Match { scrut, arms } = term;
                // Fixme: xxx
                todo!()
            }
            Tm::CoMatch(term) => {
                let t::CoMatch { arms } = term;
                // Fixme: xxx
                todo!()
            }
            Tm::Dtor(term) => {
                let t::Dtor(term, name) = term;
                // Todo: rec
                desugarer.term(id, b::Dtor(term, name).into())
            }
            Tm::Lit(term) => desugarer.term(id, term.into()),
        }
    }
}

impl Desugar for t::Sp<t::Data> {
    type Out = b::TermId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let t::Data { arms } = self.inner;
        let arms = arms
            .into_iter()
            .map(|t::DataArm { name, param }| {
                let param = param.desugar(desugarer);
                b::DataArm { name, param }
            })
            .collect();
        let term = desugarer.spans.terms.alloc(self.info);
        desugarer.term(term, b::Data { arms }.into())
    }
}

impl Desugar for t::GenBind<t::TermId> {
    type Out = (PatternId, TermId);
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let t::GenBind { rec, comp, binder, params, ty, bindee } = self;
        // bindee & ty -> ann
        let bindee = bindee.desugar(desugarer);
        let ty = ty.map(|ty| ty.desugar(desugarer));
        let bindee = if let Some(ty) = ty {
            let span = desugarer.spans.terms[bindee].clone();
            let ann = desugarer.spans.terms.alloc(span);
            desugarer.term(ann, b::Ann { tm: bindee, ty }.into())
        } else {
            bindee
        };
        // params? & bindee -> abs term binding
        let mut binding = if let Some(params) = params {
            let span = desugarer.spans.pats[binder].clone();
            let binding = desugarer.spans.terms.alloc(span);
            desugarer.term(binding, b::Abs(params, bindee).into())
        } else {
            bindee
        };
        // rec?
        if rec {
            let span = desugarer.spans.terms[binding].clone();
            let rec = desugarer.spans.terms.alloc(span);
            binding = desugarer.term(rec, b::Rec(binder, binding).into());
        }
        // add thunk?
        if rec || comp {
            let span = desugarer.spans.terms[binding].clone();
            let thunk = desugarer.spans.terms.alloc(span);
            binding = desugarer.term(thunk, b::Thunk(binding).into());
        }
        (binder, binding)
    }
}

impl Desugar for t::Sp<t::CoData> {
    type Out = b::TermId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let t::CoData { arms } = self.inner;
        let arms = arms
            .into_iter()
            .map(|t::CoDataArm { name, params, out }| {
                let params = params.map(|params| params.desugar(desugarer));
                let out = out.desugar(desugarer);
                b::CoDataArm { name, params, out }
            })
            .collect();
        let term = desugarer.spans.terms.alloc(self.info);
        desugarer.term(term, b::CoData { arms }.into())
    }
}

mod impls {
    use super::*;

    impl Desugarer {
        pub fn lookup_def(&self, id: t::DefId) -> t::VarName {
            self.tctx.defs[id].clone()
        }
        pub fn lookup_pat(&self, id: t::PatternId) -> t::Pattern {
            self.tctx.pats[id].clone()
        }
        pub fn lookup_copat(&self, id: t::CoPatternId) -> t::CoPattern {
            self.tctx.copats[id].clone()
        }
        pub fn lookup_term(&self, id: t::TermId) -> t::Term<t::NameRef<t::VarName>> {
            self.tctx.terms[id].clone()
        }
        pub fn def(&mut self, id: b::DefId, def: b::VarName) -> b::DefId {
            self.bctx.defs.insert(id, def);
            id
        }
        pub fn pat(&mut self, id: b::PatternId, pat: b::Pattern) -> b::PatternId {
            self.bctx.pats.insert(id, pat);
            id
        }
        pub fn copat(&mut self, id: b::CoPatternId, copat: b::CoPattern) -> b::CoPatternId {
            self.bctx.copats.insert(id, copat);
            id
        }
        pub fn term(&mut self, id: b::TermId, term: b::Term<b::NameRef<b::VarName>>) -> b::TermId {
            self.bctx.terms.insert(id, term);
            id
        }
    }
}
