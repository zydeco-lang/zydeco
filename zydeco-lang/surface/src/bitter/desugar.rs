use crate::{
    bitter::syntax::{self as b},
    syntax::*,
    textual::syntax::{self as t},
};
use zydeco_utils::span::Span;

pub trait Desugar {
    type Out;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out;
}

pub struct Desugarer {
    pub tspans: t::SpanArena,
    pub textual: t::Arena,
    pub bspans: b::SpanArena,
    pub bitter: b::Arena,
    pub prim: b::PrimTerm,
}

pub struct DesugarOut {
    pub spans: b::SpanArena,
    pub arena: b::Arena,
    pub prim: b::PrimTerm,
    pub top: b::TopLevel,
}

impl Desugarer {
    pub fn run(self, top: t::TopLevel) -> DesugarOut {
        let mut desugarer = self;
        let top = top.desugar(&mut desugarer);
        let Desugarer { bitter: arena, bspans: spans, prim, .. } = desugarer;
        DesugarOut { spans, arena, prim, top }
    }
    fn vtype(&mut self, span: Span) -> b::TermId {
        let term = Alloc::alloc(self, span);
        let term = self.term(term, b::Internal::VType.into());
        *self.prim.vtype.extend_one(term)
    }
    fn ctype(&mut self, span: Span) -> b::TermId {
        let term = Alloc::alloc(self, span);
        let term = self.term(term, b::Internal::CType.into());
        *self.prim.ctype.extend_one(term)
    }
    fn thunk(&mut self, span: Span) -> b::TermId {
        let term = Alloc::alloc(self, span);
        let term = self.term(term, b::Internal::Thunk.into());
        *self.prim.thunk.extend_one(term)
    }
    fn ret(&mut self, span: Span) -> b::TermId {
        let term = Alloc::alloc(self, span);
        let term = self.term(term, b::Internal::Ret.into());
        *self.prim.ret.extend_one(term)
    }
    fn os(&mut self, span: Span) -> b::TermId {
        let term = Alloc::alloc(self, span);
        let term = self.term(term, b::Internal::OS.into());
        *self.prim.os.extend_one(term)
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
        b::TopLevel(decls.into_iter().map(|decl| decl.desugar(desugarer)).collect())
    }
}

impl Desugar for t::DeclId {
    type Out = b::DeclId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let Modifiers { public, inner } = desugarer.textual.decls[&self].clone();
        use t::Declaration as Decl;
        let inner = match inner {
            | Decl::DataDef(decl) => {
                let t::DataDef { name, params, def: body } = decl;
                let span = name.span(desugarer);
                // name -> pat
                let name = name.desugar(desugarer);
                let pat = Alloc::alloc(desugarer, span.clone());
                let pat = desugarer.pat(pat, name.into());
                // body -> term
                let body = span.make(body).desugar(desugarer);
                // params & body -> abs term
                let params = params.desugar(desugarer);
                let mut abs = body;
                for param in params.into_iter().rev() {
                    let next = Alloc::alloc(desugarer, span.clone());
                    abs = desugarer.term(next, b::Abs(param, abs).into());
                }
                // abs -> sealed
                let sealed = Alloc::alloc(desugarer, span.clone());
                let sealed = desugarer.term(sealed, b::Sealed(abs).into());
                // pat & sealed -> alias
                b::Alias { binder: pat, bindee: sealed }.into()
            }
            | Decl::CoDataDef(decl) => {
                let t::CoDataDef { name, params, def: body } = decl;
                let span = name.span(desugarer);
                // name -> pat
                let name = name.desugar(desugarer);
                let pat = Alloc::alloc(desugarer, span.clone());
                let pat = desugarer.pat(pat, name.into());
                // body -> term
                let body = span.make(body).desugar(desugarer);
                // params & body -> abs term
                let params = params.desugar(desugarer);
                let mut abs = body;
                for param in params.into_iter().rev() {
                    let next = Alloc::alloc(desugarer, span.clone());
                    abs = desugarer.term(next, b::Abs(param, abs).into());
                }
                // abs -> sealed
                let sealed = Alloc::alloc(desugarer, span.clone());
                let sealed = desugarer.term(sealed, b::Sealed(abs).into());
                // pat & sealed -> alias
                b::Alias { binder: pat, bindee: sealed }.into()
            }
            | Decl::Define(decl) => {
                let t::Define(genbind) = decl;
                let (pat, term) = genbind.desugar(desugarer);
                // sealed
                let span = pat.span(desugarer);
                let sealed = Alloc::alloc(desugarer, span);
                let sealed = desugarer.term(sealed, b::Sealed(term).into());
                // pat & sealed -> alias
                b::Alias { binder: pat, bindee: sealed }.into()
            }
            | Decl::Alias(decl) => {
                let t::Alias(genbind) = decl;
                let (pat, term) = genbind.desugar(desugarer);
                // pat & term -> alias
                b::Alias { binder: pat, bindee: term }.into()
            }
            | Decl::Extern(decl) => {
                // Todo: error on rec
                let t::Extern(t::GenBind { rec: _, comp, binder, params, ty, bindee: () }) = decl;
                let binder = binder.desugar(desugarer);
                let params = if let Some(params) = params {
                    let b::App(items) = params.desugar(desugarer);
                    let mut res = Vec::new();
                    for item in items {
                        match item {
                            | b::CoPatternItem::Pat(pat) => res.push(pat),
                            | b::CoPatternItem::Dtor(_) => {
                                unimplemented!("Dtor in extern params")
                            }
                        }
                    }
                    res
                } else {
                    Vec::new()
                };
                let ty = ty.map(|ty| ty.desugar(desugarer));
                b::Extern { comp, binder, params, ty }.into()
            }
            // Decl::Layer(decl) => {
            //     let t::Layer { name, uses, top } = decl;
            //     let top = top.desugar(desugarer);
            //     let uses = uses
            //         .into_iter()
            //         .map(|Modifiers { public, inner }| Modifiers { public, inner })
            //         .collect();
            //     b::Layer { name, uses, top }.into()
            // }
            // Decl::UseDef(decl) => {
            //     let t::UseDef(uses) = decl;
            //     b::UseDef(uses).into()
            // }
            // Decl::UseBlock(decl) => {
            //     let t::UseBlock { uses, top } = decl;
            //     let top = top.desugar(desugarer);
            //     b::UseBlock { uses, top }.into()
            // }
            | Decl::Main(decl) => {
                let t::Main(term) = decl;
                let term = term.desugar(desugarer);
                b::Main(term).into()
            }
        };
        let span = self.span(desugarer);
        let decl = Alloc::alloc(desugarer, span);
        desugarer.decl(decl, Modifiers { public, inner })
    }
}

impl Desugar for t::DefId {
    type Out = b::DefId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let id = self;
        // look old span
        let span = id.span(desugarer);
        // lookup def
        let def = desugarer.lookup_def(id);
        // write new span
        let id = Alloc::alloc(desugarer, span);
        // write new def
        desugarer.def(id, def)
    }
}

impl Desugar for t::PatId {
    type Out = b::PatId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let id = self;
        let span = id.span(desugarer);
        let pat = desugarer.lookup_pat(id);
        let id = Alloc::alloc(desugarer, span);
        use t::Pattern as Pat;
        match pat {
            | Pat::Ann(pat) => {
                let t::Ann { tm, ty } = pat;
                let tm = tm.desugar(desugarer);
                let ty = ty.desugar(desugarer);
                desugarer.pat(id, b::Ann { tm, ty }.into())
            }
            | Pat::Hole(pat) => {
                let t::Hole = pat;
                desugarer.pat(id, b::Hole.into())
            }
            | Pat::Var(name) => {
                let name = name.desugar(desugarer).into();
                desugarer.pat(id, name)
            }
            | Pat::Ctor(pat) => {
                let t::Ctor(name, pat) = pat;
                let pat = pat.desugar(desugarer);
                desugarer.pat(id, b::Ctor(name, pat).into())
            }
            | Pat::Paren(pat) => {
                let t::Paren(pats) = pat;
                let pats = pats.desugar(desugarer);
                // if there is only one pat like `(p)`, remove the redundant paren
                if pats.len() == 1 {
                    pats.into_iter().next().unwrap()
                } else {
                    desugarer.pat(id, b::Paren(pats).into())
                }
            }
        }
    }
}

impl Desugar for t::CoPatId {
    type Out = b::App<b::CoPatternItem>;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        use t::CoPattern as CoPat;
        match desugarer.lookup_copat(self) {
            | CoPat::Pat(pat) => {
                let pat = pat.desugar(desugarer);
                b::App(vec![pat.into()])
            }
            | CoPat::Dtor(name) => b::App(vec![name.into()]),
            | CoPat::App(copat) => {
                let t::App(copats) = copat;
                let iter = copats.into_iter();
                let mut copats = Vec::new();
                for copat in iter {
                    match desugarer.lookup_copat(copat) {
                        | CoPat::Pat(copat) => {
                            let pat = copat.desugar(desugarer);
                            copats.push(pat.into())
                        }
                        | CoPat::Dtor(name) => copats.push(name.into()),
                        | CoPat::App(copat) => {
                            let t::App(inner) = copat;
                            for items in inner.into_iter().map(|copat| {
                                let b::App(items) = copat.desugar(desugarer);
                                items
                            }) {
                                copats.extend(items);
                            }
                        }
                    }
                }
                b::App(copats)
            }
        }
    }
}

impl Desugar for t::TermId {
    type Out = b::TermId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let id = self;
        let span = id.span(desugarer);
        let term = desugarer.lookup_term(id);
        let id = Alloc::alloc(desugarer, span);
        use t::Term as Tm;
        match term {
            | Tm::Ann(term) => {
                let t::Ann { tm, ty } = term;
                let tm = tm.desugar(desugarer);
                let ty = ty.desugar(desugarer);
                desugarer.term(id, b::Ann { tm, ty }.into())
            }
            | Tm::Hole(term) => {
                let t::Hole = term;
                desugarer.term(id, b::Hole.into())
            }
            | Tm::Var(name) => desugarer.term(id, b::Term::Var(name)),
            | Tm::Paren(term) => {
                let t::Paren(terms) = term;
                let terms = terms.desugar(desugarer);
                // if there is only one term like `(t)`, remove the redundant paren
                if terms.len() == 1 {
                    terms.into_iter().next().unwrap()
                } else {
                    desugarer.term(id, b::Paren(terms).into())
                }
            }
            | Tm::Abs(term) => {
                let t::Abs(params, tail) = term;
                let b::App(params) = params.desugar(desugarer);
                let mut tail = tail.desugar(desugarer);
                for param in params.into_iter().rev() {
                    match param {
                        | b::CoPatternItem::Pat(pat) => {
                            let span = pat.span(desugarer);
                            let id = Alloc::alloc(desugarer, span);
                            tail = desugarer.term(id, b::Abs(pat, tail).into())
                        }
                        | b::CoPatternItem::Dtor(dtor) => {
                            let span = tail.span(desugarer);
                            let id = Alloc::alloc(desugarer, span);
                            tail = desugarer.term(
                                id,
                                b::CoMatch { arms: vec![b::CoMatcher { dtor, tail }] }.into(),
                            )
                        }
                    }
                }
                tail
            }
            | Tm::App(term) => {
                let t::App(terms) = term;
                let mut iter = terms.into_iter();
                let mut terms = Vec::new();
                if let Some(head) = iter.next() {
                    if let Tm::App(term) = desugarer.lookup_term(head) {
                        let t::App(inner) = term;
                        terms.extend(inner.into_iter().map(|term| term.desugar(desugarer)));
                    } else {
                        terms.push(head.desugar(desugarer))
                    }
                }
                terms.extend(iter.map(|term| term.desugar(desugarer)));
                desugarer.term(id, b::App(terms).into())
            }
            | Tm::Rec(term) => {
                let t::Rec(pat, term) = term;
                let pat = pat.desugar(desugarer);
                let term = term.desugar(desugarer);
                desugarer.term(id, b::Rec(pat, term).into())
            }
            | Tm::Pi(term) => {
                let t::Pi(params, ty) = term;
                let b::App(params) = params.desugar(desugarer);
                let mut ty = ty.desugar(desugarer);
                for param in params.into_iter().rev() {
                    match param {
                        | b::CoPatternItem::Pat(pat) => {
                            let span = pat.span(desugarer);
                            let id = Alloc::alloc(desugarer, span);
                            ty = desugarer.term(id, b::Pi(pat, ty).into())
                        }
                        | b::CoPatternItem::Dtor(_dtor) => {
                            unimplemented!("handle dtor in pi type error")
                        }
                    }
                }
                ty
            }
            | Tm::Arrow(term) => {
                let t::Arrow(params, ty) = term;
                // params -> ann = (hole: params)
                let params = params.desugar(desugarer);
                let span = params.span(desugarer);
                let hole = Alloc::alloc(desugarer, span.clone());
                let hole = desugarer.pat(hole, b::Hole.into());
                let ann = Alloc::alloc(desugarer, span.clone());
                let ann = desugarer.pat(ann, b::Ann { tm: hole, ty: params }.into());
                // ann & ty -> pi
                let ty = ty.desugar(desugarer);
                desugarer.term(id, b::Pi(ann, ty).into())
            }
            | Tm::Forall(term) => {
                let t::Forall(params, ty) = term;
                let b::App(params) = params.desugar(desugarer);
                let mut ty = ty.desugar(desugarer);
                for param in params.into_iter().rev() {
                    match param {
                        | b::CoPatternItem::Pat(pat) => {
                            let span = pat.span(desugarer);
                            let id = Alloc::alloc(desugarer, span);
                            ty = desugarer.term(id, b::Pi(pat, ty).into())
                        }
                        | b::CoPatternItem::Dtor(_dtor) => {
                            unimplemented!("handle dtor in forall type error")
                        }
                    }
                }
                let forall = ty;
                // forall -> ann
                let span = forall.span(desugarer);
                let ann = Alloc::alloc(desugarer, span.clone());
                let ctype = desugarer.ctype(span);
                desugarer.term(ann, b::Ann { tm: forall, ty: ctype }.into())
            }
            | Tm::Sigma(term) => {
                let t::Sigma(params, ty) = term;
                let b::App(params) = params.desugar(desugarer);
                let mut ty = ty.desugar(desugarer);
                for param in params.into_iter().rev() {
                    match param {
                        | b::CoPatternItem::Pat(pat) => {
                            let span = pat.span(desugarer);
                            let id = Alloc::alloc(desugarer, span);
                            ty = desugarer.term(id, b::Sigma(pat, ty).into())
                        }
                        | b::CoPatternItem::Dtor(_dtor) => {
                            unimplemented!("handle dtor in sigma type error")
                        }
                    }
                }
                ty
            }
            | Tm::Prod(term) => {
                let t::Prod(terms) = term;
                let mut terms = terms.desugar(desugarer);
                let mut out = terms.pop().unwrap();
                while let Some(term) = terms.pop() {
                    // term -> ann = (hole: pat)
                    let span = term.span(desugarer);
                    let hole = Alloc::alloc(desugarer, span.clone());
                    let hole = desugarer.pat(hole, b::Hole.into());
                    let ann = Alloc::alloc(desugarer, span.clone());
                    let ann = desugarer.pat(ann, b::Ann { tm: hole, ty: term }.into());
                    // ann & sigma -> sigma
                    let span = out.span(desugarer);
                    let sigma = Alloc::alloc(desugarer, span);
                    out = desugarer.term(sigma, b::Sigma(ann, out).into());
                }
                out
            }
            | Tm::Exists(term) => {
                let t::Exists(params, ty) = term;
                let b::App(params) = params.desugar(desugarer);
                let mut ty = ty.desugar(desugarer);
                for param in params.into_iter().rev() {
                    match param {
                        | b::CoPatternItem::Pat(pat) => {
                            let span = pat.span(desugarer);
                            let id = Alloc::alloc(desugarer, span);
                            ty = desugarer.term(id, b::Sigma(pat, ty).into())
                        }
                        | b::CoPatternItem::Dtor(_dtor) => {
                            unimplemented!("handle dtor in exists type error")
                        }
                    }
                }
                let exists = ty;
                // exists -> ann
                let span = exists.span(desugarer);
                let ann = Alloc::alloc(desugarer, span.clone());
                let vtype = desugarer.vtype(span);
                desugarer.term(ann, b::Ann { tm: exists, ty: vtype }.into())
            }
            | Tm::Thunk(term) => {
                let t::Thunk(body) = term;
                let body = body.desugar(desugarer);
                // body -> tm
                let tm = desugarer.term(id, b::Thunk(body).into());
                // thunk & hole -> ty
                let span = self.span(desugarer);
                let thunk = desugarer.thunk(span.clone());
                let hole = Alloc::alloc(desugarer, span.clone());
                let hole = desugarer.term(hole, b::Hole.into());
                let ty = Alloc::alloc(desugarer, span.clone());
                let ty = desugarer.term(ty, b::App(vec![thunk, hole]).into());
                // tm & ty -> ann
                let ann = Alloc::alloc(desugarer, span);
                desugarer.term(ann, b::Ann { tm, ty }.into())
            }
            | Tm::Force(term) => {
                let t::Force(term) = term;
                let term = term.desugar(desugarer);
                desugarer.term(id, b::Force(term).into())
            }
            | Tm::Ret(term) => {
                let t::Return(body) = term;
                let body = body.desugar(desugarer);
                // body -> tm
                let tm = desugarer.term(id, b::Return(body).into());
                // ret & hole -> ty
                let span = self.span(desugarer);
                let ret = desugarer.ret(span.clone());
                let hole = Alloc::alloc(desugarer, span.clone());
                let hole = desugarer.term(hole, b::Hole.into());
                let ty = Alloc::alloc(desugarer, span.clone());
                let ty = desugarer.term(ty, b::App(vec![ret, hole]).into());
                // tm & ty -> ann
                let ann = Alloc::alloc(desugarer, span);
                desugarer.term(ann, b::Ann { tm, ty }.into())
            }
            | Tm::Do(term) => {
                let t::Bind { binder, bindee, tail } = term;
                let binder = binder.desugar(desugarer);
                let bindee = bindee.desugar(desugarer);
                let tail = tail.desugar(desugarer);
                desugarer.term(id, b::Bind { binder, bindee, tail }.into())
            }
            | Tm::Let(term) => {
                let t::PureBind { binding, tail } = term;
                let (binder, bindee) = binding.desugar(desugarer);
                let tail = tail.desugar(desugarer);
                desugarer.term(id, b::PureBind { binder, bindee, tail }.into())
            }
            // Tm::UseLet(term) => {
            //     let t::UseBind { uses, tail } = term;
            //     // Todo: uses
            //     let tail = tail.desugar(desugarer);
            //     desugarer.term(id, b::UseBind { uses, tail }.into())
            // }
            | Tm::Data(data) => {
                let span = id.span(desugarer);
                span.make(data).desugar(desugarer)
            }
            | Tm::CoData(codata) => {
                let span = id.span(desugarer);
                span.make(codata).desugar(desugarer)
            }
            | Tm::Ctor(term) => {
                let t::Ctor(name, term) = term;
                let term = term.desugar(desugarer);
                desugarer.term(id, b::Ctor(name, term).into())
            }
            | Tm::Match(term) => {
                let t::Match { scrut, arms } = term;
                let scrut = scrut.desugar(desugarer);
                let arms = arms
                    .into_iter()
                    .map(|t::Matcher { binder, tail }| {
                        let binder = binder.desugar(desugarer);
                        let tail = tail.desugar(desugarer);
                        b::Matcher { binder, tail }
                    })
                    .collect();
                desugarer.term(id, b::Match { scrut, arms }.into())
            }
            | Tm::CoMatch(term) => {
                let t::CoMatch { arms } = term;
                let arms = arms
                    .into_iter()
                    .map(|t::CoMatcher { params, tail }| {
                        let b::App(params) = params.desugar(desugarer);
                        let mut tail = tail.desugar(desugarer);
                        let mut iter = params.into_iter();
                        let Some(b::CoPatternItem::Dtor(dtor)) = iter.next() else {
                            unimplemented!("handle error where no dtor in comatch params")
                        };
                        for param in iter.rev() {
                            match param {
                                | b::CoPatternItem::Pat(pat) => {
                                    let span = pat.span(desugarer);
                                    let id = Alloc::alloc(desugarer, span);
                                    tail = desugarer.term(id, b::Abs(pat, tail).into())
                                }
                                | b::CoPatternItem::Dtor(dtor) => {
                                    let span = tail.span(desugarer);
                                    let id = Alloc::alloc(desugarer, span);
                                    tail = desugarer.term(
                                        id,
                                        b::CoMatch { arms: vec![b::CoMatcher { dtor, tail }] }
                                            .into(),
                                    )
                                }
                            }
                        }
                        b::CoMatcher { dtor, tail }
                    })
                    .collect();
                desugarer.term(id, b::CoMatch { arms }.into())
            }
            | Tm::Dtor(term) => {
                let t::Dtor(term, name) = term;
                let term = term.desugar(desugarer);
                desugarer.term(id, b::Dtor(term, name).into())
            }
            | Tm::Lit(term) => desugarer.term(id, term.into()),
        }
    }
}

impl Desugar for t::GenBind<t::TermId> {
    type Out = (b::PatId, b::TermId);
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let t::GenBind { rec, comp, binder, params, ty, bindee } = self;
        // binder
        let binder = binder.desugar(desugarer);
        // bindee & ty -> ann
        let bindee = bindee.desugar(desugarer);
        let ty = ty.map(|ty| ty.desugar(desugarer));
        let bindee = if let Some(ty) = ty {
            let span = bindee.span(desugarer);
            let ann = Alloc::alloc(desugarer, span);
            desugarer.term(ann, b::Ann { tm: bindee, ty }.into())
        } else {
            bindee
        };
        // params
        let params = params.map(|params| params.desugar(desugarer));
        // params? & bindee -> abs term binding
        let mut binding = bindee;
        if let Some(b::App(params)) = params {
            for param in params.into_iter().rev() {
                match param {
                    | b::CoPatternItem::Pat(pat) => {
                        let span = pat.span(desugarer);
                        let id = Alloc::alloc(desugarer, span);
                        binding = desugarer.term(id, b::Abs(pat, binding).into())
                    }
                    | b::CoPatternItem::Dtor(dtor) => {
                        let span = binding.span(desugarer);
                        let id = Alloc::alloc(desugarer, span);
                        binding = desugarer.term(
                            id,
                            b::CoMatch { arms: vec![b::CoMatcher { dtor, tail: binding }] }.into(),
                        )
                    }
                }
            }
        };
        // rec?
        if rec {
            let span = binding.span(desugarer);
            let rec = Alloc::alloc(desugarer, span);
            let binder = binder.deep_clone(desugarer);
            binding = desugarer.term(rec, b::Rec(binder, binding).into());
        }
        // add thunk?
        if rec || comp {
            let span = binding.span(desugarer);
            let thunk = Alloc::alloc(desugarer, span);
            binding = desugarer.term(thunk, b::Thunk(binding).into());
        }
        (binder, binding)
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
        let term = Alloc::alloc(desugarer, self.info.clone());
        let data = desugarer.term(term, b::Data { arms }.into());
        // data -> ann
        let ann = Alloc::alloc(desugarer, self.info.clone());
        let vtype = desugarer.vtype(self.info);
        desugarer.term(ann, b::Ann { tm: data, ty: vtype }.into())
    }
}

impl Desugar for t::Sp<t::CoData> {
    type Out = b::TermId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let t::CoData { arms } = self.inner;
        let arms = arms
            .into_iter()
            .map(|t::CoDataArm { name, params, out }| {
                // Todo: deal with params as if they are pi type inputs
                assert!(params.is_none());
                let out = out.desugar(desugarer);
                b::CoDataArm { name, out }
            })
            .collect();
        let term = Alloc::alloc(desugarer, self.info.clone());
        let codata = desugarer.term(term, b::CoData { arms }.into());
        // codata -> ann
        let ann = Alloc::alloc(desugarer, self.info.clone());
        let ctype = desugarer.ctype(self.info);
        desugarer.term(ann, b::Ann { tm: codata, ty: ctype }.into())
    }
}

use impls::*;

mod impls {
    use super::*;

    pub(super) trait Spanned {
        fn span(&self, desugarer: &mut Desugarer) -> Span;
    }

    impl Spanned for t::DefId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.tspans.defs[self].clone()
        }
    }
    impl Spanned for t::PatId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.tspans.pats[self].clone()
        }
    }
    impl Spanned for t::CoPatId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.tspans.copats[self].clone()
        }
    }
    impl Spanned for t::TermId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.tspans.terms[self].clone()
        }
    }
    impl Spanned for t::DeclId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.tspans.decls[self].clone()
        }
    }
    impl Spanned for b::DefId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.bspans.defs[self].clone()
        }
    }
    impl Spanned for b::PatId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.bspans.pats[self].clone()
        }
    }
    impl Spanned for b::TermId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.bspans.terms[self].clone()
        }
    }
    impl Spanned for b::DeclId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.bspans.decls[self].clone()
        }
    }

    pub(super) trait Alloc {
        fn alloc(desugarer: &mut Desugarer, span: Span) -> Self;
    }

    impl Alloc for b::DefId {
        fn alloc(desugarer: &mut Desugarer, span: Span) -> Self {
            desugarer.bspans.defs.alloc(span)
        }
    }
    impl Alloc for b::PatId {
        fn alloc(desugarer: &mut Desugarer, span: Span) -> Self {
            desugarer.bspans.pats.alloc(span)
        }
    }
    impl Alloc for b::TermId {
        fn alloc(desugarer: &mut Desugarer, span: Span) -> Self {
            desugarer.bspans.terms.alloc(span)
        }
    }
    impl Alloc for b::DeclId {
        fn alloc(desugarer: &mut Desugarer, span: Span) -> Self {
            desugarer.bspans.decls.alloc(span)
        }
    }

    pub(super) trait DeepClone {
        fn deep_clone(&self, desugarer: &mut Desugarer) -> Self;
    }
    impl<T> DeepClone for Vec<T>
    where
        T: DeepClone,
    {
        fn deep_clone(&self, desugarer: &mut Desugarer) -> Self {
            self.iter().map(|x| x.deep_clone(desugarer)).collect()
        }
    }
    impl DeepClone for b::DefId {
        fn deep_clone(&self, desugarer: &mut Desugarer) -> Self {
            let span = self.span(desugarer);
            let def = desugarer.bitter.defs[self].clone();
            let id = Alloc::alloc(desugarer, span);
            desugarer.def(id, def)
        }
    }
    impl DeepClone for b::PatId {
        fn deep_clone(&self, desugarer: &mut Desugarer) -> Self {
            let span = self.span(desugarer);
            let pat = desugarer.bitter.pats[self].clone();
            let pat = match &pat {
                | b::Pattern::Ann(pat) => {
                    let b::Ann { tm, ty } = pat;
                    let tm = tm.deep_clone(desugarer);
                    let ty = ty.deep_clone(desugarer);
                    b::Ann { tm, ty }.into()
                }
                | b::Pattern::Hole(_pat) => b::Hole.into(),
                | b::Pattern::Var(pat) => pat.deep_clone(desugarer).into(),
                | b::Pattern::Ctor(pat) => {
                    let b::Ctor(name, pat) = pat;
                    let pat = pat.deep_clone(desugarer);
                    b::Ctor(name.clone(), pat).into()
                }
                | b::Pattern::Paren(pat) => {
                    let b::Paren(pats) = pat;
                    let pats = pats.deep_clone(desugarer);
                    b::Paren(pats).into()
                }
            };
            let id = Alloc::alloc(desugarer, span);
            desugarer.pat(id, pat)
        }
    }
    impl DeepClone for b::TermId {
        fn deep_clone(&self, desugarer: &mut Desugarer) -> Self {
            let span = self.span(desugarer);
            let term = desugarer.bitter.terms[self].clone();
            let term = match &term {
                | b::Term::Internal(term) => {
                    use crate::syntax::Internal;
                    match term {
                        | Internal::VType => {
                            return desugarer.vtype(span);
                        }
                        | Internal::CType => {
                            return desugarer.ctype(span);
                        }
                        | Internal::Thunk => {
                            return desugarer.thunk(span);
                        }
                        | Internal::Ret => {
                            return desugarer.ret(span);
                        }
                        | Internal::OS => {
                            return desugarer.os(span);
                        }
                    }
                }
                | b::Term::Sealed(_term) => {
                    unreachable!()
                    // let b::Sealed(term) = term;
                    // let term = term.deep_clone(desugarer);
                    // b::Sealed(term).into()
                }
                | b::Term::Ann(term) => {
                    let b::Ann { tm, ty } = term;
                    let tm = tm.deep_clone(desugarer);
                    let ty = ty.deep_clone(desugarer);
                    b::Ann { tm, ty }.into()
                }
                | b::Term::Hole(_term) => b::Hole.into(),
                | b::Term::Var(name) => b::Term::Var(name.clone()),
                | b::Term::Paren(term) => {
                    let b::Paren(terms) = term;
                    let terms = terms.deep_clone(desugarer);
                    b::Paren(terms).into()
                }
                | b::Term::Abs(term) => {
                    let b::Abs(params, tail) = term;
                    let params = params.deep_clone(desugarer);
                    let tail = tail.deep_clone(desugarer);
                    b::Abs(params, tail).into()
                }
                | b::Term::App(term) => {
                    let b::App(terms) = term;
                    let terms = terms.deep_clone(desugarer);
                    b::App(terms).into()
                }
                | b::Term::Rec(term) => {
                    let b::Rec(pat, term) = term;
                    let pat = pat.deep_clone(desugarer);
                    let term = term.deep_clone(desugarer);
                    b::Rec(pat, term).into()
                }
                | b::Term::Pi(term) => {
                    let b::Pi(params, ty) = term;
                    let params = params.deep_clone(desugarer);
                    let ty = ty.deep_clone(desugarer);
                    b::Pi(params, ty).into()
                }
                | b::Term::Sigma(term) => {
                    let b::Sigma(params, ty) = term;
                    let params = params.deep_clone(desugarer);
                    let ty = ty.deep_clone(desugarer);
                    b::Sigma(params, ty).into()
                }
                | b::Term::Thunk(term) => {
                    let b::Thunk(term) = term;
                    let term = term.deep_clone(desugarer);
                    b::Thunk(term).into()
                }
                | b::Term::Force(term) => {
                    let b::Force(term) = term;
                    let term = term.deep_clone(desugarer);
                    b::Force(term).into()
                }
                | b::Term::Ret(term) => {
                    let b::Return(term) = term;
                    let term = term.deep_clone(desugarer);
                    b::Return(term).into()
                }
                | b::Term::Do(term) => {
                    let b::Bind { binder, bindee, tail } = term;
                    let binder = binder.deep_clone(desugarer);
                    let bindee = bindee.deep_clone(desugarer);
                    let tail = tail.deep_clone(desugarer);
                    b::Bind { binder, bindee, tail }.into()
                }
                | b::Term::Let(term) => {
                    let b::PureBind { binder, bindee, tail } = term;
                    let binder = binder.deep_clone(desugarer);
                    let bindee = bindee.deep_clone(desugarer);
                    let tail = tail.deep_clone(desugarer);
                    b::PureBind { binder, bindee, tail }.into()
                }
                | b::Term::Data(term) => {
                    let b::Data { arms } = term;
                    let arms = arms
                        .into_iter()
                        .map(|b::DataArm { name, param }| {
                            let name = name.clone();
                            let param = param.deep_clone(desugarer);
                            b::DataArm { name, param }
                        })
                        .collect();
                    b::Data { arms }.into()
                }
                | b::Term::CoData(term) => {
                    let b::CoData { arms } = term;
                    let arms = arms
                        .into_iter()
                        .map(|b::CoDataArm { name, out }| {
                            let name = name.clone();
                            let out = out.deep_clone(desugarer);
                            b::CoDataArm { name, out }
                        })
                        .collect();
                    b::CoData { arms }.into()
                }
                | b::Term::Ctor(term) => {
                    let b::Ctor(name, term) = term;
                    let term = term.deep_clone(desugarer);
                    let name = name.clone();
                    b::Ctor(name, term).into()
                }
                | b::Term::Match(term) => {
                    let b::Match { scrut, arms } = term;
                    let scrut = scrut.deep_clone(desugarer);
                    let arms = arms
                        .into_iter()
                        .map(|b::Matcher { binder, tail }| {
                            let binder = binder.deep_clone(desugarer);
                            let tail = tail.deep_clone(desugarer);
                            b::Matcher { binder, tail }
                        })
                        .collect();
                    b::Match { scrut, arms }.into()
                }
                | b::Term::CoMatch(term) => {
                    let b::CoMatch { arms } = term;
                    let arms = arms
                        .into_iter()
                        .map(|b::CoMatcher { dtor, tail }| {
                            let dtor = dtor.clone();
                            let tail = tail.deep_clone(desugarer);
                            b::CoMatcher { dtor, tail }
                        })
                        .collect();
                    b::CoMatch { arms }.into()
                }
                | b::Term::Dtor(term) => {
                    let b::Dtor(term, name) = term;
                    let term = term.deep_clone(desugarer);
                    let name = name.clone();
                    b::Dtor(term, name).into()
                }
                | b::Term::Lit(term) => term.clone().into(),
            };
            let id = Alloc::alloc(desugarer, span);
            desugarer.term(id, term)
        }
    }

    impl Desugarer {
        pub fn lookup_def(&self, id: t::DefId) -> t::VarName {
            self.textual.defs[&id].clone()
        }
        pub fn lookup_pat(&self, id: t::PatId) -> t::Pattern {
            self.textual.pats[&id].clone()
        }
        pub fn lookup_copat(&self, id: t::CoPatId) -> t::CoPattern {
            self.textual.copats[&id].clone()
        }
        pub fn lookup_term(&self, id: t::TermId) -> t::Term {
            self.textual.terms[&id].clone()
        }
        pub fn lookup_decl(&self, id: t::DeclId) -> Modifiers<t::Declaration> {
            self.textual.decls[&id].clone()
        }

        pub fn def(&mut self, id: b::DefId, def: b::VarName) -> b::DefId {
            self.bitter.defs.insert(id, def);
            id
        }
        pub fn pat(&mut self, id: b::PatId, pat: b::Pattern) -> b::PatId {
            self.bitter.pats.insert(id, pat);
            id
        }
        pub fn term(&mut self, id: b::TermId, term: b::Term<b::NameRef<b::VarName>>) -> b::TermId {
            self.bitter.terms.insert(id, term);
            id
        }
        pub fn decl(&mut self, id: b::DeclId, decl: Modifiers<b::Declaration>) -> b::DeclId {
            self.bitter.decls.insert(id, decl);
            id
        }
    }
}
