use crate::{
    bitter::{syntax as b, *},
    syntax::*,
    textual::syntax::{self as t, GenBind},
};
use derive_more::{AsMut, AsRef};
use zydeco_syntax::SpanView;
use zydeco_utils::prelude::CompilerPass;

/// Desugar a textual node into bitter syntax using a shared `Desugarer`.
pub trait Desugar {
    type Out;
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out>;
}

/// Stateful desugaring pass from textual to bitter syntax.
#[derive(AsRef, AsMut)]
pub struct Desugarer<'a> {
    pub spans: &'a t::SpanArena,
    pub textual: &'a t::TextArena,
    pub top: t::TopLevel,
    #[as_ref(b::BitterArena)]
    #[as_mut(b::BitterArena)]
    pub bitter: b::BitterArena,
    pub prim: b::PrimTerms,
}
impl<'a> Desugarer<'a> {
    pub fn new(
        spans: &'a t::SpanArena, textual: &'a t::TextArena, top: t::TopLevel, bitter: b::BitterArena,
    ) -> Self {
        Self { spans, textual, top, bitter, prim: b::PrimTerms::default() }
    }
}

/// Output of the desugaring pass.
pub struct DesugarOut {
    pub arena: b::BitterArena,
    pub prim: b::PrimTerms,
    pub top: b::TopLevel,
}

impl CompilerPass for Desugarer<'_> {
    type Arena = b::BitterArena;
    type Out = DesugarOut;
    type Error = DesugarError;
    fn run(self) -> Result<DesugarOut> {
        let mut desugarer = self;
        let top = desugarer.top.clone().desugar(&mut desugarer)?;
        let Desugarer { bitter: arena, prim, .. } = desugarer;
        Ok(DesugarOut { arena, prim, top })
    }
}

impl<T> Desugar for Vec<T>
where
    T: Desugar,
{
    type Out = Vec<T::Out>;
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out> {
        self.into_iter().map(|x| x.desugar(desugarer)).collect()
    }
}

impl Desugar for t::TopLevel {
    type Out = b::TopLevel;
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out> {
        let t::TopLevel(decls) = self;
        // b::TopLevel(decls.into_iter().map(|decl| decl.desugar(desugarer)).collect())
        let mut decls_ = Vec::new();
        for decl in decls {
            let decl = decl.desugar(desugarer)?;
            match &desugarer.bitter.decls[&decl] {
                | Modifiers { public: _, external: _, inner: b::Declaration::Module(module) } => {
                    let b::Module { name: _, top: b::TopLevel(decls) } = module;
                    decls_.extend(decls)
                }
                | _ => decls_.push(decl),
            }
        }
        let res = b::TopLevel(decls_);
        Ok(res)
    }
}

impl Desugar for t::DeclId {
    type Out = b::DeclId;
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out> {
        let Modifiers { public, external, inner } = desugarer.lookup_decl(self).clone();
        use t::Declaration as Decl;
        let inner = match inner {
            | Decl::Meta(decl) => {
                let t::MetaT(meta, decl) = decl;
                let decl = decl.desugar(desugarer)?;
                b::MetaT(meta, decl).into()
            }
            | Decl::DataDef(decl) => {
                let t::DataDef { name, params, def: body } = decl;
                // name -> pat
                let name = name.desugar(desugarer)?;
                let pat = Alloc::alloc(desugarer, name.into(), self.into());
                // body -> term
                let body = (body, self.into()).desugar(desugarer)?;
                // params & body -> abs term
                let params = params.desugar(desugarer)?;
                let mut abs = body;
                let mut ann = desugarer.vtype(self.into());
                for param in params.into_iter().rev() {
                    abs = Alloc::alloc(desugarer, b::Abs(param, abs).into(), self.into());
                    let tpat = param.deep_clone(desugarer);
                    ann = Alloc::alloc(desugarer, b::Pi(tpat, ann).into(), self.into());
                }
                // abs & ann -> anno
                let anno = Alloc::alloc(desugarer, b::Ann { tm: abs, ty: ann }.into(), self.into());
                // anno -> sealed
                let sealed = Alloc::alloc(desugarer, b::Sealed(anno).into(), self.into());
                // pat & sealed -> alias
                b::AliasBody { binder: pat, bindee: sealed }.into()
            }
            | Decl::CoDataDef(decl) => {
                let t::CoDataDef { name, params, def: body } = decl;
                // name -> pat
                let name = name.desugar(desugarer)?;
                let pat = Alloc::alloc(desugarer, name.into(), self.into());
                // body -> term
                let body = (body, self.into()).desugar(desugarer)?;
                // params & body -> abs term
                let params = params.desugar(desugarer)?;
                let mut abs = body;
                let mut ann = desugarer.ctype(self.into());
                for param in params.into_iter().rev() {
                    abs = Alloc::alloc(desugarer, b::Abs(param, abs).into(), self.into());
                    let tpat = param.deep_clone(desugarer);
                    ann = Alloc::alloc(desugarer, b::Pi(tpat, ann).into(), self.into());
                }
                // abs & ann -> anno
                let anno = Alloc::alloc(desugarer, b::Ann { tm: abs, ty: ann }.into(), self.into());
                // anno -> sealed
                let sealed = Alloc::alloc(desugarer, b::Sealed(anno).into(), self.into());
                // pat & sealed -> alias
                b::AliasBody { binder: pat, bindee: sealed }.into()
            }
            | Decl::Define(decl) => {
                let t::Define(GenBind { fix, comp, binder, params, ty, bindee }) = decl;
                if let Some(bindee) = bindee {
                    let (pat, term) =
                        t::GenBind { fix, comp, binder, params, ty, bindee }.desugar(desugarer)?;
                    // sealed
                    let sealed = Alloc::alloc(desugarer, b::Sealed(term).into(), self.into());
                    // pat & sealed -> alias
                    b::AliasBody { binder: pat, bindee: sealed }.into()
                } else {
                    assert!(external);
                    assert!(!comp);
                    let binder = binder.desugar(desugarer)?;
                    let ty = if let Some(ty) = ty {
                        let mut ty = ty.desugar(desugarer)?;
                        if let Some(params) = params {
                            let b::Appli(items) = params.desugar(desugarer)?;
                            for item in items {
                                match item {
                                    | b::CoPatternItem::Pat(pat) => {
                                        ty = Alloc::alloc(
                                            desugarer,
                                            b::Pi(pat, ty).into(),
                                            self.into(),
                                        )
                                    }
                                    | b::CoPatternItem::Dtor(_) => {
                                        unimplemented!("Dtor in extern params")
                                    }
                                }
                            }
                        };
                        Some(ty)
                    } else {
                        None
                    };
                    b::AliasHead { binder, ty }.into()
                }
            }
            | Decl::Alias(decl) => {
                let t::Alias(genbind) = decl;
                let (pat, term) = genbind.desugar(desugarer)?;
                // pat & term -> alias
                b::AliasBody { binder: pat, bindee: term }.into()
            }
            | Decl::Module(decl) => {
                let t::Module { name, top } = decl;
                let top = top.desugar(desugarer)?;
                b::Module { name, top }.into()
            }
            // Decl::Layer(decl) => {
            //     let t::Layer { name, uses, top } = decl;
            //     let top = top.desugar(desugarer)?;
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
            //     let top = top.desugar(desugarer)?;
            //     b::UseBlock { uses, top }.into()
            // }
            | Decl::Exec(decl) => {
                let t::Exec(term) = decl;
                let term = term.desugar(desugarer)?;
                // // term -> ann
                // let os = desugarer.os(self.into());
                // let ann = Alloc::alloc(desugarer, b::Ann { tm: term, ty: os }.into(), self.into());
                // b::Exec(ann).into()
                b::Exec(term).into()
            }
        };
        let res = Alloc::alloc(desugarer, Modifiers { public, external, inner }, self.into());
        Ok(res)
    }
}

impl Desugar for t::DefId {
    type Out = b::DefId;
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out> {
        let id = self;
        // lookup def
        let def = desugarer.lookup_def(id);
        // write new def
        let res = Alloc::alloc(desugarer, def, self.into());
        Ok(res)
    }
}

impl Desugar for t::PatId {
    type Out = b::PatId;
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out> {
        let id = self;
        let pat = desugarer.lookup_pat(id);
        use t::Pattern as Pat;
        let res = match pat {
            | Pat::Ann(pat) => {
                let t::Ann { tm, ty } = pat;
                let tm = tm.desugar(desugarer)?;
                let ty = ty.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Ann { tm, ty }.into(), self.into())
            }
            | Pat::Hole(pat) => {
                let t::Hole = pat;
                Alloc::alloc(desugarer, b::Hole.into(), self.into())
            }
            | Pat::Var(name) => {
                let name = name.desugar(desugarer)?.into();
                Alloc::alloc(desugarer, name, self.into())
            }
            | Pat::Ctor(pat) => {
                let t::Ctor(name, pat) = pat;
                let pat = pat.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Ctor(name, pat).into(), self.into())
            }
            | Pat::Paren(pat) => {
                let t::Paren(pats) = pat;
                let pats = pats.desugar(desugarer)?;
                match pats.len() {
                    // if there is no pat like `()`, replace it with `unit`
                    | 0 => Alloc::alloc(desugarer, b::Triv.into(), self.into()),
                    // if there is only one pat like `(p)`, remove the redundant paren
                    | 1 => pats.into_iter().next().unwrap(),
                    // otherwise, re-expand the paren into cons
                    | _ => {
                        // re-expand from right to left, like:
                        // (p1, (p2, (p3, p4)))
                        let mut iter = pats.into_iter().rev();
                        let snd = iter.next().unwrap();
                        let fst = iter.next().unwrap();
                        let mut body = b::Cons(fst, snd).into();
                        for pat in iter {
                            let id = Alloc::alloc(desugarer, body, self.into());
                            body = b::Cons(pat, id).into()
                        }
                        Alloc::alloc(desugarer, body, self.into())
                    }
                }
            }
        };
        Ok(res)
    }
}

impl Desugar for t::CoPatId {
    type Out = b::Appli<b::CoPatternItem>;
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out> {
        use t::CoPattern as CoPat;
        let res = match desugarer.lookup_copat(self) {
            | CoPat::Pat(pat) => {
                let pat = pat.desugar(desugarer)?;
                b::Appli(vec![pat.into()])
            }
            | CoPat::Dtor(name) => b::Appli(vec![name.into()]),
            | CoPat::App(copat) => {
                let t::Appli(copats) = copat;
                let iter = copats.into_iter();
                let mut copats = Vec::new();
                for copat in iter {
                    match desugarer.lookup_copat(copat) {
                        | CoPat::Pat(copat) => {
                            let pat = copat.desugar(desugarer)?;
                            copats.push(pat.into())
                        }
                        | CoPat::Dtor(name) => copats.push(name.into()),
                        | CoPat::App(copat) => {
                            let t::Appli(inner) = copat;
                            for items in {
                                inner
                                    .into_iter()
                                    .map(|copat| {
                                        let b::Appli(items) = copat.desugar(desugarer)?;
                                        Ok(items)
                                    })
                                    .collect::<Result<Vec<_>>>()?
                            } {
                                copats.extend(items);
                            }
                        }
                    }
                }
                b::Appli(copats)
            }
        };
        Ok(res)
    }
}

impl Desugar for t::TermId {
    type Out = b::TermId;
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out> {
        let id = self;
        let term = desugarer.lookup_term(id);
        use t::Term as Tm;
        let res = match term {
            | Tm::Meta(term) => {
                let t::MetaT(meta, term) = term;
                let term = term.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::MetaT(meta, term).into(), self.into())
            }
            | Tm::Ann(term) => {
                let t::Ann { tm, ty } = term;
                let tm = tm.desugar(desugarer)?;
                let ty = ty.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Ann { tm, ty }.into(), self.into())
            }
            | Tm::Hole(term) => {
                let t::Hole = term;
                Alloc::alloc(desugarer, b::Hole.into(), self.into())
            }
            | Tm::Var(name) => Alloc::alloc(desugarer, b::Term::Var(name), self.into()),
            | Tm::Paren(term) => {
                let t::Paren(terms) = term;
                let mut iter = terms.into_iter();
                let mut terms = Vec::new();
                // merge the first nested paren
                if let Some(head) = iter.next() {
                    if let Tm::Paren(term) = desugarer.lookup_term(head) {
                        let t::Paren(inner) = term;
                        terms.extend(
                            inner
                                .into_iter()
                                .map(|term| term.desugar(desugarer))
                                .collect::<Result<Vec<_>>>()?,
                        );
                    } else {
                        terms.push(head.desugar(desugarer)?)
                    }
                }
                terms.extend(iter.map(|term| term.desugar(desugarer)).collect::<Result<Vec<_>>>()?);
                match terms.len() {
                    // if there is no term like `()`, replace it with `unit`
                    | 0 => Alloc::alloc(desugarer, b::Triv.into(), self.into()),
                    // if there is only one term like `(t)`, remove the redundant paren
                    | 1 => terms.into_iter().next().unwrap(),
                    // otherwise, re-expand the paren into cons
                    | _ => {
                        // re-expand from right to left, like:
                        // (t1, (t2, (t3, t4)))
                        let mut iter = terms.into_iter().rev();
                        let snd = iter.next().unwrap();
                        let fst = iter.next().unwrap();
                        let mut body = b::Cons(fst, snd).into();
                        for term in iter {
                            let id = Alloc::alloc(desugarer, body, self.into());
                            body = b::Cons(term, id).into()
                        }
                        Alloc::alloc(desugarer, body, self.into())
                    }
                }
            }
            | Tm::Abs(term) => {
                let t::Abs(params, tail) = term;
                let b::Appli(params) = params.desugar(desugarer)?;
                let (mut tail, mut annotation) =
                    if let Tm::Ann(t::Ann { tm, ty }) = desugarer.lookup_term(tail) {
                        (tm.desugar(desugarer)?, Some(ty.desugar(desugarer)?))
                    } else {
                        (tail.desugar(desugarer)?, None)
                    };
                for param in params.into_iter().rev() {
                    match param {
                        | b::CoPatternItem::Pat(pat) => {
                            tail = Alloc::alloc(desugarer, b::Abs(pat, tail).into(), self.into());
                            let pat_ty = pat.deep_clone(desugarer);
                            if let Some(annotation) = &mut annotation {
                                *annotation = Alloc::alloc(
                                    desugarer,
                                    b::Pi(pat_ty, *annotation).into(),
                                    self.into(),
                                );
                            }
                        }
                        | b::CoPatternItem::Dtor(dtor) => {
                            tail = Alloc::alloc(
                                desugarer,
                                b::CoMatch { arms: vec![b::CoMatcher { dtor, tail }] }.into(),
                                self.into(),
                            );
                            annotation = None;
                        }
                    }
                }
                if let Some(annotation) = annotation {
                    Alloc::alloc(desugarer, b::Ann { tm: tail, ty: annotation }.into(), self.into())
                } else {
                    tail
                }
            }
            | Tm::App(term) => {
                let t::Appli(terms) = term;
                let mut iter = terms.into_iter();
                let mut terms = Vec::new();
                // merge the first nested app
                if let Some(head) = iter.next() {
                    if let Tm::App(term) = desugarer.lookup_term(head) {
                        let t::Appli(inner) = term;
                        terms.extend(
                            inner
                                .into_iter()
                                .map(|term| term.desugar(desugarer))
                                .collect::<Result<Vec<_>>>()?,
                        );
                    } else {
                        terms.push(head.desugar(desugarer)?)
                    }
                }
                terms.extend(iter.map(|term| term.desugar(desugarer)).collect::<Result<Vec<_>>>()?);
                match terms.len() {
                    // app with no term is invalid
                    | 0 => unreachable!(),
                    // app with one term is just the term itself
                    | 1 => terms.into_iter().next().unwrap(),
                    // if there are more than one term, expand the app into a chain of apps
                    | _ => {
                        let mut iter = terms.into_iter();
                        let mut body = b::App(iter.next().unwrap(), iter.next().unwrap()).into();
                        for term in iter {
                            let id = Alloc::alloc(desugarer, body, self.into());
                            body = b::App(id, term).into()
                        }
                        Alloc::alloc(desugarer, body, self.into())
                    }
                }
            }
            | Tm::KontCall(term) => {
                let t::KontCall { body, tail } = term;
                let body = body.desugar(desugarer)?;
                let tail = tail.desugar(desugarer)?;
                // tail -> thunk
                let thunk = Alloc::alloc(desugarer, b::Thunk(tail).into(), self.into());
                // body & thunk -> app
                Alloc::alloc(desugarer, b::App(body, thunk).into(), self.into())
            }
            | Tm::Fix(term) => {
                let t::Fix(pat, term) = term;
                let pat = pat.desugar(desugarer)?;
                let term = term.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Fix(pat, term).into(), self.into())
            }
            | Tm::Pi(term) => {
                let t::Pi(params, ty) = term;
                let b::Appli(params) = params.desugar(desugarer)?;
                let mut ty = ty.desugar(desugarer)?;
                for param in params.into_iter().rev() {
                    match param {
                        | b::CoPatternItem::Pat(pat) => {
                            ty = Alloc::alloc(desugarer, b::Pi(pat, ty).into(), self.into())
                        }
                        | b::CoPatternItem::Dtor(_dtor) => {
                            unimplemented!("handle dtor in pi type error")
                        }
                    }
                }
                ty
            }
            | Tm::Arrow(term) => {
                let t::Arrow(ty_in, ty_out) = term;
                // ty_in -> ann = (hole: ty_in)
                let ty_in = ty_in.desugar(desugarer)?;
                let hole = Alloc::alloc(desugarer, b::Hole.into(), self.into());
                let ann =
                    Alloc::alloc(desugarer, b::Ann { tm: hole, ty: ty_in }.into(), self.into());
                // ann & ty_out -> pi
                let ty_out = ty_out.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Pi(ann, ty_out).into(), self.into())
            }
            | Tm::Forall(term) => {
                let t::Forall(params, ty) = term;
                let b::Appli(params) = params.desugar(desugarer)?;
                let mut ty = ty.desugar(desugarer)?;
                for param in params.into_iter().rev() {
                    match param {
                        | b::CoPatternItem::Pat(pat) => {
                            ty = Alloc::alloc(desugarer, b::Pi(pat, ty).into(), self.into())
                        }
                        | b::CoPatternItem::Dtor(_dtor) => {
                            unimplemented!("handle dtor in forall type error")
                        }
                    }
                }
                let forall = ty;
                // forall -> ann
                let ctype = desugarer.ctype(self.into());
                Alloc::alloc(desugarer, b::Ann { tm: forall, ty: ctype }.into(), self.into())
            }
            | Tm::Sigma(term) => {
                let t::Sigma(params, ty) = term;
                let b::Appli(params) = params.desugar(desugarer)?;
                let mut ty = ty.desugar(desugarer)?;
                for param in params.into_iter().rev() {
                    match param {
                        | b::CoPatternItem::Pat(pat) => {
                            ty = Alloc::alloc(desugarer, b::Sigma(pat, ty).into(), self.into())
                        }
                        | b::CoPatternItem::Dtor(_dtor) => {
                            unimplemented!("handle dtor in sigma type error")
                        }
                    }
                }
                ty
            }
            | Tm::Prod(term) => {
                let t::Prod(ty_l, ty_r) = term;
                // ty_l -> ann = (hole: ty_l)
                let ty_l = ty_l.desugar(desugarer)?;
                let hole = Alloc::alloc(desugarer, b::Hole.into(), self.into());
                let ann =
                    Alloc::alloc(desugarer, b::Ann { tm: hole, ty: ty_l }.into(), self.into());
                // ann & ty_r -> sigma
                let ty_r = ty_r.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Sigma(ann, ty_r).into(), self.into())
            }
            | Tm::Exists(term) => {
                let t::Exists(params, ty) = term;
                let b::Appli(params) = params.desugar(desugarer)?;
                let mut ty = ty.desugar(desugarer)?;
                for param in params.into_iter().rev() {
                    match param {
                        | b::CoPatternItem::Pat(pat) => {
                            ty = Alloc::alloc(desugarer, b::Sigma(pat, ty).into(), self.into())
                        }
                        | b::CoPatternItem::Dtor(_dtor) => {
                            unimplemented!("handle dtor in exists type error")
                        }
                    }
                }
                let exists = ty;
                // exists -> ann
                let vtype = desugarer.vtype(self.into());
                Alloc::alloc(desugarer, b::Ann { tm: exists, ty: vtype }.into(), self.into())
            }
            | Tm::Thunk(term) => {
                let t::Thunk(body) = term;
                let body = body.desugar(desugarer)?;
                // body -> tm
                let tm = Alloc::alloc(desugarer, b::Thunk(body).into(), self.into());
                // thunk & hole -> ty
                let thunk = desugarer.thunk(self.into());
                let hole = Alloc::alloc(desugarer, b::Hole.into(), self.into());
                let ty = Alloc::alloc(desugarer, b::App(thunk, hole).into(), self.into());
                // tm & ty -> ann
                Alloc::alloc(desugarer, b::Ann { tm, ty }.into(), self.into())
            }
            | Tm::Force(term) => {
                let t::Force(term) = term;
                let term = term.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Force(term).into(), self.into())
            }
            | Tm::Ret(term) => {
                let t::Return(body) = term;
                let body = body.desugar(desugarer)?;
                // body -> tm
                let tm = Alloc::alloc(desugarer, b::Return(body).into(), self.into());
                // ret & hole -> ty
                let ret = desugarer.ret(self.into());
                let hole = Alloc::alloc(desugarer, b::Hole.into(), self.into());
                let ty = Alloc::alloc(desugarer, b::App(ret, hole).into(), self.into());
                // tm & ty -> ann
                Alloc::alloc(desugarer, b::Ann { tm, ty }.into(), self.into())
            }
            | Tm::Do(term) => {
                let t::Bind { binder, bindee, tail } = term;
                let binder = binder.desugar(desugarer)?;
                let bindee = bindee.desugar(desugarer)?;
                let tail = tail.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Bind { binder, bindee, tail }.into(), self.into())
            }
            | Tm::Let(term) => {
                let t::GenLet { binding, tail } = term;
                let (binder, bindee) = binding.desugar(desugarer)?;
                let tail = tail.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Let { binder, bindee, tail }.into(), self.into())
            }
            // Tm::UseLet(term) => {
            //     let t::UseBind { uses, tail } = term;
            //     // Todo: uses
            //     let tail = tail.desugar(desugarer)?;
            //     Alloc::alloc(desugarer, b::UseBind { uses, tail }.into(), self.into())
            // }
            | Tm::MoBlock(term) => {
                let t::MoBlock(body) = term;
                let body = body.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::MoBlock(body).into(), self.into())
            }
            | Tm::Data(data) => (data, self.into()).desugar(desugarer)?,
            | Tm::CoData(codata) => (codata, self.into()).desugar(desugarer)?,
            | Tm::Ctor(term) => {
                let t::Ctor(name, term) = term;
                let term = term.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Ctor(name, term).into(), self.into())
            }
            | Tm::Match(term) => {
                let t::Match { scrut, arms } = term;
                let scrut = scrut.desugar(desugarer)?;
                let arms = arms
                    .into_iter()
                    .map(|t::Matcher { binder, tail }| {
                        let binder = binder.desugar(desugarer)?;
                        let tail = tail.desugar(desugarer)?;
                        Ok(b::Matcher { binder, tail })
                    })
                    .collect::<Result<Vec<_>>>()?;
                Alloc::alloc(desugarer, b::Match { scrut, arms }.into(), self.into())
            }
            | Tm::CoMatch(term) => {
                let t::CoMatchParam { arms } = term;
                let arms = arms
                    .into_iter()
                    .map(|t::CoMatcherParam { params, tail }| {
                        let b::Appli(params) = params.desugar(desugarer)?;
                        let mut tail = tail.desugar(desugarer)?;
                        let mut iter = params.into_iter();
                        let Some(b::CoPatternItem::Dtor(dtor)) = iter.next() else {
                            unimplemented!("handle error where no dtor in comatch params")
                        };
                        for param in iter.rev() {
                            match param {
                                | b::CoPatternItem::Pat(pat) => {
                                    tail = Alloc::alloc(
                                        desugarer,
                                        b::Abs(pat, tail).into(),
                                        self.into(),
                                    )
                                }
                                | b::CoPatternItem::Dtor(dtor) => {
                                    tail = Alloc::alloc(
                                        desugarer,
                                        b::CoMatch { arms: vec![b::CoMatcher { dtor, tail }] }
                                            .into(),
                                        self.into(),
                                    )
                                }
                            }
                        }
                        Ok(b::CoMatcher { dtor, tail })
                    })
                    .collect::<Result<Vec<_>>>()?;
                Alloc::alloc(desugarer, b::CoMatch { arms }.into(), self.into())
            }
            | Tm::Dtor(term) => {
                let t::Dtor(term, name) = term;
                let term = term.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Dtor(term, name).into(), self.into())
            }
            | Tm::Lit(term) => {
                use zydeco_syntax::Literal as Lit;
                let lit_term = Alloc::alloc(desugarer, term.clone().into(), self.into());
                let ty = match term {
                    | Lit::Int(_) => desugarer.int(self.into()),
                    | Lit::String(_) => desugarer.string(self.into()),
                    | Lit::Char(_) => desugarer.char(self.into()),
                };
                Alloc::alloc(desugarer, b::Ann { tm: lit_term, ty }.into(), self.into())
            }
        };
        Ok(res)
    }
}

impl Desugar for t::GenBind<t::TermId> {
    type Out = (b::PatId, b::TermId);
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out> {
        let t::GenBind { fix, comp, binder, params, ty, bindee } = self;
        let prev = bindee.into();
        // binder
        let binder = binder.desugar(desugarer)?;
        let bindee = bindee.desugar(desugarer)?;
        // ty -> ann
        let ty = match ty {
            | Some(ty) => Some(ty.desugar(desugarer)?),
            | None => None,
        };
        let mut ann = match ty {
            | Some(ty) => ty,
            | None => Alloc::alloc(desugarer, b::Hole.into(), prev),
        };
        // params
        // let params = params.map(|params| params.desugar(desugarer));
        let params = match params {
            | Some(params) => Some(params.desugar(desugarer)?),
            | None => None,
        };
        // params? & bindee -> abs term binding
        let mut binding = bindee;
        if let Some(b::Appli(params)) = params {
            for param in params.into_iter().rev() {
                match param {
                    | b::CoPatternItem::Pat(pat) => {
                        binding = Alloc::alloc(desugarer, b::Abs(pat, binding).into(), prev);
                        let tpat = pat.deep_clone(desugarer);
                        ann = Alloc::alloc(desugarer, b::Pi(tpat, ann).into(), prev);
                    }
                    | b::CoPatternItem::Dtor(dtor) => {
                        binding = Alloc::alloc(
                            desugarer,
                            b::CoMatch { arms: vec![b::CoMatcher { dtor, tail: binding }] }.into(),
                            prev,
                        );
                        ann = Alloc::alloc(desugarer, b::Hole.into(), prev);
                    }
                }
            }
        };
        // fix?
        if fix {
            if comp {
                let span = binder.span(desugarer);
                Err(DesugarError::CompWhileFix(span.make(binder)))?
            }
            let binder = binder.deep_clone(desugarer);
            binding = Alloc::alloc(desugarer, b::Fix(binder, binding).into(), prev);
        }
        // add thunk?
        if fix || comp {
            binding = Alloc::alloc(desugarer, b::Thunk(binding).into(), prev);
            let thunk = desugarer.thunk(prev);
            ann = Alloc::alloc(desugarer, b::App(thunk, ann).into(), prev);
        }
        // binding & ann -> anno
        let anno = Alloc::alloc(desugarer, b::Ann { tm: binding, ty: ann }.into(), prev);
        Ok((binder, anno))
    }
}

impl Desugar for (t::Data, t::EntityId) {
    type Out = b::TermId;
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out> {
        let (t::Data { arms }, prev) = self;
        let arms = arms
            .into_iter()
            .map(|t::DataArm { name, param }| {
                let param = param.desugar(desugarer)?;
                Ok(b::DataArm { name, param })
            })
            .collect::<Result<_>>()?;
        let data = Alloc::alloc(desugarer, b::Data { arms }.into(), prev);
        // data -> ann
        let vtype = desugarer.vtype(prev);
        let res = Alloc::alloc(desugarer, b::Ann { tm: data, ty: vtype }.into(), prev);
        Ok(res)
    }
}

impl Desugar for (t::CoData, t::EntityId) {
    type Out = b::TermId;
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out> {
        let (t::CoData { arms }, prev) = self;
        let arms = arms
            .into_iter()
            .map(|t::CoDataArm { name, params, out }| {
                let mut out = out.desugar(desugarer)?;
                if let Some(params) = params {
                    let b::Appli(params) = params.desugar(desugarer)?;
                    for param in params.into_iter().rev() {
                        match param {
                            | b::CoPatternItem::Pat(pat) => {
                                out = Alloc::alloc(desugarer, b::Pi(pat, out).into(), prev)
                            }
                            | b::CoPatternItem::Dtor(dtor) => {
                                panic!("dtor in codata arm params: {:?}", dtor)
                            }
                        }
                    }
                }
                Ok(b::CoDataArm { name, out })
            })
            .collect::<Result<_>>()?;
        let codata = Alloc::alloc(desugarer, b::CoData { arms }.into(), prev);
        // codata -> ann
        let ctype = desugarer.ctype(prev);
        let res = Alloc::alloc(desugarer, b::Ann { tm: codata, ty: ctype }.into(), prev);
        Ok(res)
    }
}

mod impls {
    use super::*;

    impl Desugarer<'_> {
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
    }

    impl Desugarer<'_> {
        pub(crate) fn vtype(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::VType.into(), prev);
            *self.prim.vtype.extend_one(term)
        }
        pub(crate) fn ctype(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::CType.into(), prev);
            *self.prim.ctype.extend_one(term)
        }
        pub(crate) fn thunk(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::Thk.into(), prev);
            *self.prim.thk.extend_one(term)
        }
        pub(crate) fn ret(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::Ret.into(), prev);
            *self.prim.ret.extend_one(term)
        }
        pub(crate) fn unit(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::Unit.into(), prev);
            *self.prim.unit.extend_one(term)
        }
        pub(crate) fn int(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::Int.into(), prev);
            *self.prim.int.extend_one(term)
        }
        pub(crate) fn char(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::Char.into(), prev);
            *self.prim.char.extend_one(term)
        }
        pub(crate) fn string(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::String.into(), prev);
            *self.prim.string.extend_one(term)
        }
        pub(crate) fn os(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::OS.into(), prev);
            *self.prim.os.extend_one(term)
        }
        pub(crate) fn monad(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::Monad.into(), prev);
            *self.prim.monad.extend_one(term)
        }
        pub(crate) fn algebra(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::Algebra.into(), prev);
            *self.prim.algebra.extend_one(term)
        }
    }
}
