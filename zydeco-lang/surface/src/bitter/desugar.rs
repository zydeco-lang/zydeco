use crate::{
    bitter::syntax as b,
    syntax::*,
    textual::syntax::{self as t, GenBind},
};

pub trait Desugar {
    type Out;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out;
}

pub struct Desugarer {
    pub textual: t::Arena,
    pub bitter: b::Arena,
    pub prim: b::PrimTerms,
}

pub struct DesugarOut {
    pub arena: b::Arena,
    pub prim: b::PrimTerms,
    pub top: b::TopLevel,
}

impl Desugarer {
    pub fn run(self, top: t::TopLevel) -> DesugarOut {
        let mut desugarer = self;
        let top = top.desugar(&mut desugarer);
        let Desugarer { bitter: arena, prim, .. } = desugarer;
        DesugarOut { arena, prim, top }
    }
    fn vtype(&mut self) -> b::TermId {
        let term = Alloc::alloc(self, b::Internal::VType.into());
        *self.prim.vtype.extend_one(term)
    }
    fn ctype(&mut self) -> b::TermId {
        let term = Alloc::alloc(self, b::Internal::CType.into());
        *self.prim.ctype.extend_one(term)
    }
    fn thunk(&mut self) -> b::TermId {
        let term = Alloc::alloc(self, b::Internal::Thunk.into());
        *self.prim.thunk.extend_one(term)
    }
    fn ret(&mut self) -> b::TermId {
        let term = Alloc::alloc(self, b::Internal::Ret.into());
        *self.prim.ret.extend_one(term)
    }
    fn unit(&mut self) -> b::TermId {
        let term = Alloc::alloc(self, b::Internal::Unit.into());
        *self.prim.unit.extend_one(term)
    }
    fn int(&mut self) -> b::TermId {
        let term = Alloc::alloc(self, b::Internal::Int.into());
        *self.prim.int.extend_one(term)
    }
    fn char(&mut self) -> b::TermId {
        let term = Alloc::alloc(self, b::Internal::Char.into());
        *self.prim.char.extend_one(term)
    }
    fn string(&mut self) -> b::TermId {
        let term = Alloc::alloc(self, b::Internal::String.into());
        *self.prim.string.extend_one(term)
    }
    fn os(&mut self) -> b::TermId {
        let term = Alloc::alloc(self, b::Internal::OS.into());
        *self.prim.os.extend_one(term)
    }
    fn monad(&mut self) -> b::TermId {
        let term = Alloc::alloc(self, b::Internal::Monad.into());
        *self.prim.monad.extend_one(term)
    }
    fn algebra(&mut self) -> b::TermId {
        let term = Alloc::alloc(self, b::Internal::Algebra.into());
        *self.prim.algebra.extend_one(term)
    }
}

pub trait Alloc {
    type Entity;
    fn alloc(desugarer: &mut Desugarer, entity: Self::Entity) -> Self;
}

impl Alloc for b::DefId {
    type Entity = b::VarName;
    fn alloc(desugarer: &mut Desugarer, entity: Self::Entity) -> Self {
        desugarer.bitter.defs.alloc(entity)
    }
}
impl Alloc for b::PatId {
    type Entity = b::Pattern;
    fn alloc(desugarer: &mut Desugarer, entity: Self::Entity) -> Self {
        desugarer.bitter.pats.alloc(entity)
    }
}
impl Alloc for b::TermId {
    type Entity = b::Term<b::NameRef<b::VarName>>;
    fn alloc(desugarer: &mut Desugarer, entity: Self::Entity) -> Self {
        desugarer.bitter.terms.alloc(entity)
    }
}
impl Alloc for b::DeclId {
    type Entity = b::Modifiers<b::Declaration>;
    fn alloc(desugarer: &mut Desugarer, entity: Self::Entity) -> Self {
        desugarer.bitter.decls.alloc(entity)
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
        let Modifiers { public, external, inner } = desugarer.textual.decls[&self].clone();
        use t::Declaration as Decl;
        let inner = match inner {
            | Decl::DataDef(decl) => {
                let t::DataDef { name, params, def: body } = decl;
                // name -> pat
                let name = name.desugar(desugarer);
                let pat = Alloc::alloc(desugarer, name.into());
                // body -> term
                let body = body.desugar(desugarer);
                // params & body -> abs term
                let params = params.desugar(desugarer);
                let mut abs = body;
                for param in params.into_iter().rev() {
                    abs = Alloc::alloc(desugarer, b::Abs(param, abs).into());
                }
                // abs -> sealed
                let sealed = Alloc::alloc(desugarer, b::Sealed(abs).into());
                // pat & sealed -> alias
                b::AliasBody { binder: pat, bindee: sealed }.into()
            }
            | Decl::CoDataDef(decl) => {
                let t::CoDataDef { name, params, def: body } = decl;
                // name -> pat
                let name = name.desugar(desugarer);
                let pat = Alloc::alloc(desugarer, name.into());
                // body -> term
                let body = body.desugar(desugarer);
                // params & body -> abs term
                let params = params.desugar(desugarer);
                let mut abs = body;
                for param in params.into_iter().rev() {
                    abs = Alloc::alloc(desugarer, b::Abs(param, abs).into());
                }
                // abs -> sealed
                let sealed = Alloc::alloc(desugarer, b::Sealed(abs).into());
                // pat & sealed -> alias
                b::AliasBody { binder: pat, bindee: sealed }.into()
            }
            | Decl::Define(decl) => {
                let t::Define(GenBind { rec, comp, binder, params, ty, bindee }) = decl;
                if let Some(bindee) = bindee {
                    let (pat, term) =
                        t::GenBind { rec, comp, binder, params, ty, bindee }.desugar(desugarer);
                    // sealed
                    let sealed = Alloc::alloc(desugarer, b::Sealed(term).into());
                    // pat & sealed -> alias
                    b::AliasBody { binder: pat, bindee: sealed }.into()
                } else {
                    assert!(external);
                    assert!(!comp);
                    let binder = binder.desugar(desugarer);
                    let ty = if let Some(ty) = ty {
                        let mut ty = ty.desugar(desugarer);
                        if let Some(params) = params {
                            let b::Appli(items) = params.desugar(desugarer);
                            for item in items {
                                match item {
                                    | b::CoPatternItem::Pat(pat) => {
                                        ty = Alloc::alloc(desugarer, b::Pi(pat, ty).into())
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
                let (pat, term) = genbind.desugar(desugarer);
                // pat & term -> alias
                b::AliasBody { binder: pat, bindee: term }.into()
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
                // term -> ann
                let os = desugarer.os();
                let ann = Alloc::alloc(desugarer, b::Ann { tm: term, ty: os }.into());
                b::Main(ann).into()
            }
        };
        Alloc::alloc(desugarer, Modifiers { public, external, inner })
    }
}

impl Desugar for t::DefId {
    type Out = b::DefId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let id = self;
        // lookup def
        let def = desugarer.lookup_def(id);
        // write new def
        Alloc::alloc(desugarer, def)
    }
}

impl Desugar for t::PatId {
    type Out = b::PatId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let id = self;
        let pat = desugarer.lookup_pat(id);
        use t::Pattern as Pat;
        match pat {
            | Pat::Ann(pat) => {
                let t::Ann { tm, ty } = pat;
                let tm = tm.desugar(desugarer);
                let ty = ty.desugar(desugarer);
                Alloc::alloc(desugarer, b::Ann { tm, ty }.into())
            }
            | Pat::Hole(pat) => {
                let t::Hole = pat;
                Alloc::alloc(desugarer, b::Hole.into())
            }
            | Pat::Var(name) => {
                let name = name.desugar(desugarer).into();
                Alloc::alloc(desugarer, name)
            }
            | Pat::Ctor(pat) => {
                let t::Ctor(name, pat) = pat;
                let pat = pat.desugar(desugarer);
                Alloc::alloc(desugarer, b::Ctor(name, pat).into())
            }
            | Pat::Paren(pat) => {
                let t::Paren(pats) = pat;
                let pats = pats.desugar(desugarer);
                match pats.len() {
                    // if there is no pat like `()`, replace it with `unit`
                    | 0 => Alloc::alloc(desugarer, b::Triv.into()),
                    // if there is only one pat like `(p)`, remove the redundant paren
                    | 1 => pats.into_iter().next().unwrap(),
                    // otherwise, re-expand the paren into cons
                    | _ => {
                        let mut iter = pats.into_iter();
                        let mut body = b::Cons(iter.next().unwrap(), iter.next().unwrap()).into();
                        for pat in iter {
                            let id = Alloc::alloc(desugarer, body);
                            body = b::Cons(pat, id).into()
                        }
                        Alloc::alloc(desugarer, body)
                    }
                }
            }
        }
    }
}

impl Desugar for t::CoPatId {
    type Out = b::Appli<b::CoPatternItem>;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        use t::CoPattern as CoPat;
        match desugarer.lookup_copat(self) {
            | CoPat::Pat(pat) => {
                let pat = pat.desugar(desugarer);
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
                            let pat = copat.desugar(desugarer);
                            copats.push(pat.into())
                        }
                        | CoPat::Dtor(name) => copats.push(name.into()),
                        | CoPat::App(copat) => {
                            let t::Appli(inner) = copat;
                            for items in inner.into_iter().map(|copat| {
                                let b::Appli(items) = copat.desugar(desugarer);
                                items
                            }) {
                                copats.extend(items);
                            }
                        }
                    }
                }
                b::Appli(copats)
            }
        }
    }
}

impl Desugar for t::TermId {
    type Out = b::TermId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let id = self;
        let term = desugarer.lookup_term(id);
        use t::Term as Tm;
        match term {
            | Tm::Ann(term) => {
                let t::Ann { tm, ty } = term;
                let tm = tm.desugar(desugarer);
                let ty = ty.desugar(desugarer);
                Alloc::alloc(desugarer, b::Ann { tm, ty }.into())
            }
            | Tm::Hole(term) => {
                let t::Hole = term;
                Alloc::alloc(desugarer, b::Hole.into())
            }
            | Tm::Var(name) => Alloc::alloc(desugarer, b::Term::Var(name)),
            | Tm::Paren(term) => {
                let t::Paren(terms) = term;
                let mut iter = terms.into_iter();
                let mut terms = Vec::new();
                // merge the first nested paren
                if let Some(head) = iter.next() {
                    if let Tm::Paren(term) = desugarer.lookup_term(head) {
                        let t::Paren(inner) = term;
                        terms.extend(inner.into_iter().map(|term| term.desugar(desugarer)));
                    } else {
                        terms.push(head.desugar(desugarer))
                    }
                }
                terms.extend(iter.map(|term| term.desugar(desugarer)));
                match terms.len() {
                    // if there is no term like `()`, replace it with `unit`
                    | 0 => Alloc::alloc(desugarer, b::Triv.into()),
                    // if there is only one term like `(t)`, remove the redundant paren
                    | 1 => terms.into_iter().next().unwrap(),
                    // otherwise, re-expand the paren into cons
                    | _ => {
                        let mut iter = terms.into_iter();
                        let mut body = b::Cons(iter.next().unwrap(), iter.next().unwrap()).into();
                        for term in iter {
                            let id = Alloc::alloc(desugarer, body);
                            body = b::Cons(id, term).into()
                        }
                        Alloc::alloc(desugarer, body)
                    }
                }
            }
            | Tm::Abs(term) => {
                let t::Abs(params, tail) = term;
                let b::Appli(params) = params.desugar(desugarer);
                let mut tail = tail.desugar(desugarer);
                for param in params.into_iter().rev() {
                    match param {
                        | b::CoPatternItem::Pat(pat) => {
                            tail = Alloc::alloc(desugarer, b::Abs(pat, tail).into())
                        }
                        | b::CoPatternItem::Dtor(dtor) => {
                            tail = Alloc::alloc(
                                desugarer,
                                b::CoMatch { arms: vec![b::CoMatcher { dtor, tail }] }.into(),
                            )
                        }
                    }
                }
                tail
            }
            | Tm::App(term) => {
                let t::Appli(terms) = term;
                let mut iter = terms.into_iter();
                let mut terms = Vec::new();
                // merge the first nested app
                if let Some(head) = iter.next() {
                    if let Tm::App(term) = desugarer.lookup_term(head) {
                        let t::Appli(inner) = term;
                        terms.extend(inner.into_iter().map(|term| term.desugar(desugarer)));
                    } else {
                        terms.push(head.desugar(desugarer))
                    }
                }
                terms.extend(iter.map(|term| term.desugar(desugarer)));
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
                            let id = Alloc::alloc(desugarer, body);
                            body = b::App(id, term).into()
                        }
                        Alloc::alloc(desugarer, body)
                    }
                }
            }
            | Tm::Rec(term) => {
                let t::Rec(pat, term) = term;
                let pat = pat.desugar(desugarer);
                let term = term.desugar(desugarer);
                Alloc::alloc(desugarer, b::Rec(pat, term).into())
            }
            | Tm::Pi(term) => {
                let t::Pi(params, ty) = term;
                let b::Appli(params) = params.desugar(desugarer);
                let mut ty = ty.desugar(desugarer);
                for param in params.into_iter().rev() {
                    match param {
                        | b::CoPatternItem::Pat(pat) => {
                            ty = Alloc::alloc(desugarer, b::Pi(pat, ty).into())
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
                let ty_in = ty_in.desugar(desugarer);
                let hole = Alloc::alloc(desugarer, b::Hole.into());
                let ann = Alloc::alloc(desugarer, b::Ann { tm: hole, ty: ty_in }.into());
                // ann & ty_out -> pi
                let ty_out = ty_out.desugar(desugarer);
                Alloc::alloc(desugarer, b::Pi(ann, ty_out).into())
            }
            | Tm::Forall(term) => {
                let t::Forall(params, ty) = term;
                let b::Appli(params) = params.desugar(desugarer);
                let mut ty = ty.desugar(desugarer);
                for param in params.into_iter().rev() {
                    match param {
                        | b::CoPatternItem::Pat(pat) => {
                            ty = Alloc::alloc(desugarer, b::Pi(pat, ty).into())
                        }
                        | b::CoPatternItem::Dtor(_dtor) => {
                            unimplemented!("handle dtor in forall type error")
                        }
                    }
                }
                let forall = ty;
                // forall -> ann
                let ctype = desugarer.ctype();
                Alloc::alloc(desugarer, b::Ann { tm: forall, ty: ctype }.into())
            }
            | Tm::Sigma(term) => {
                let t::Sigma(params, ty) = term;
                let b::Appli(params) = params.desugar(desugarer);
                let mut ty = ty.desugar(desugarer);
                for param in params.into_iter().rev() {
                    match param {
                        | b::CoPatternItem::Pat(pat) => {
                            ty = Alloc::alloc(desugarer, b::Sigma(pat, ty).into())
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
                let ty_l = ty_l.desugar(desugarer);
                let hole = Alloc::alloc(desugarer, b::Hole.into());
                let ann = Alloc::alloc(desugarer, b::Ann { tm: hole, ty: ty_l }.into());
                // ann & ty_r -> sigma
                let ty_r = ty_r.desugar(desugarer);
                Alloc::alloc(desugarer, b::Sigma(ann, ty_r).into())
            }
            | Tm::Exists(term) => {
                let t::Exists(params, ty) = term;
                let b::Appli(params) = params.desugar(desugarer);
                let mut ty = ty.desugar(desugarer);
                for param in params.into_iter().rev() {
                    match param {
                        | b::CoPatternItem::Pat(pat) => {
                            ty = Alloc::alloc(desugarer, b::Sigma(pat, ty).into())
                        }
                        | b::CoPatternItem::Dtor(_dtor) => {
                            unimplemented!("handle dtor in exists type error")
                        }
                    }
                }
                let exists = ty;
                // exists -> ann
                let vtype = desugarer.vtype();
                Alloc::alloc(desugarer, b::Ann { tm: exists, ty: vtype }.into())
            }
            | Tm::Thunk(term) => {
                let t::Thunk(body) = term;
                let body = body.desugar(desugarer);
                // body -> tm
                let tm = Alloc::alloc(desugarer, b::Thunk(body).into());
                // thunk & hole -> ty
                let thunk = desugarer.thunk();
                let hole = Alloc::alloc(desugarer, b::Hole.into());
                let ty = Alloc::alloc(desugarer, b::App(thunk, hole).into());
                // tm & ty -> ann
                Alloc::alloc(desugarer, b::Ann { tm, ty }.into())
            }
            | Tm::Force(term) => {
                let t::Force(term) = term;
                let term = term.desugar(desugarer);
                Alloc::alloc(desugarer, b::Force(term).into())
            }
            | Tm::Ret(term) => {
                let t::Ret(body) = term;
                let body = body.desugar(desugarer);
                // body -> tm
                let tm = Alloc::alloc(desugarer, b::Ret(body).into());
                // ret & hole -> ty
                let ret = desugarer.ret();
                let hole = Alloc::alloc(desugarer, b::Hole.into());
                let ty = Alloc::alloc(desugarer, b::App(ret, hole).into());
                // tm & ty -> ann
                Alloc::alloc(desugarer, b::Ann { tm, ty }.into())
            }
            | Tm::Do(term) => {
                let t::Bind { binder, bindee, tail } = term;
                let binder = binder.desugar(desugarer);
                let bindee = bindee.desugar(desugarer);
                let tail = tail.desugar(desugarer);
                Alloc::alloc(desugarer, b::Bind { binder, bindee, tail }.into())
            }
            | Tm::Let(term) => {
                let t::GenPureBind { binding, tail } = term;
                let (binder, bindee) = binding.desugar(desugarer);
                let tail = tail.desugar(desugarer);
                Alloc::alloc(desugarer, b::PureBind { binder, bindee, tail }.into())
            }
            // Tm::UseLet(term) => {
            //     let t::UseBind { uses, tail } = term;
            //     // Todo: uses
            //     let tail = tail.desugar(desugarer);
            //     Alloc::alloc(desugarer, b::UseBind { uses, tail }.into())
            // }
            | Tm::Data(data) => data.desugar(desugarer),
            | Tm::CoData(codata) => codata.desugar(desugarer),
            | Tm::Ctor(term) => {
                let t::Ctor(name, term) = term;
                let term = term.desugar(desugarer);
                Alloc::alloc(desugarer, b::Ctor(name, term).into())
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
                Alloc::alloc(desugarer, b::Match { scrut, arms }.into())
            }
            | Tm::CoMatch(term) => {
                let t::CoMatch { arms } = term;
                let arms = arms
                    .into_iter()
                    .map(|t::CoMatcher { params, tail }| {
                        let b::Appli(params) = params.desugar(desugarer);
                        let mut tail = tail.desugar(desugarer);
                        let mut iter = params.into_iter();
                        let Some(b::CoPatternItem::Dtor(dtor)) = iter.next() else {
                            unimplemented!("handle error where no dtor in comatch params")
                        };
                        for param in iter.rev() {
                            match param {
                                | b::CoPatternItem::Pat(pat) => {
                                    tail = Alloc::alloc(desugarer, b::Abs(pat, tail).into())
                                }
                                | b::CoPatternItem::Dtor(dtor) => {
                                    tail = Alloc::alloc(
                                        desugarer,
                                        b::CoMatch { arms: vec![b::CoMatcher { dtor, tail }] }
                                            .into(),
                                    )
                                }
                            }
                        }
                        b::CoMatcher { dtor, tail }
                    })
                    .collect();
                Alloc::alloc(desugarer, b::CoMatch { arms }.into())
            }
            | Tm::Dtor(term) => {
                let t::Dtor(term, name) = term;
                let term = term.desugar(desugarer);
                Alloc::alloc(desugarer, b::Dtor(term, name).into())
            }
            | Tm::Lit(term) => Alloc::alloc(desugarer, term.into()),
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
            Alloc::alloc(desugarer, b::Ann { tm: bindee, ty }.into())
        } else {
            bindee
        };
        // params
        let params = params.map(|params| params.desugar(desugarer));
        // params? & bindee -> abs term binding
        let mut binding = bindee;
        if let Some(b::Appli(params)) = params {
            for param in params.into_iter().rev() {
                match param {
                    | b::CoPatternItem::Pat(pat) => {
                        binding = Alloc::alloc(desugarer, b::Abs(pat, binding).into())
                    }
                    | b::CoPatternItem::Dtor(dtor) => {
                        binding = Alloc::alloc(
                            desugarer,
                            b::CoMatch { arms: vec![b::CoMatcher { dtor, tail: binding }] }.into(),
                        )
                    }
                }
            }
        };
        // rec?
        if rec {
            let binder = binder.deep_clone(desugarer);
            binding = Alloc::alloc(desugarer, b::Rec(binder, binding).into());
        }
        // add thunk?
        if rec || comp {
            binding = Alloc::alloc(desugarer, b::Thunk(binding).into());
        }
        (binder, binding)
    }
}

impl Desugar for t::Data {
    type Out = b::TermId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let t::Data { arms } = self;
        let arms = arms
            .into_iter()
            .map(|t::DataArm { name, param }| {
                let param = param.desugar(desugarer);
                b::DataArm { name, param }
            })
            .collect();
        let data = Alloc::alloc(desugarer, b::Data { arms }.into());
        // data -> ann
        let vtype = desugarer.vtype();
        Alloc::alloc(desugarer, b::Ann { tm: data, ty: vtype }.into())
    }
}

impl Desugar for t::CoData {
    type Out = b::TermId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let t::CoData { arms } = self;
        let arms = arms
            .into_iter()
            .map(|t::CoDataArm { name, params, out }| {
                // Todo: deal with params as if they are pi type inputs
                assert!(params.is_none());
                let out = out.desugar(desugarer);
                b::CoDataArm { name, out }
            })
            .collect();
        let codata = Alloc::alloc(desugarer, b::CoData { arms }.into());
        // codata -> ann
        let ctype = desugarer.ctype();
        Alloc::alloc(desugarer, b::Ann { tm: codata, ty: ctype }.into())
    }
}

use impls::*;

mod impls {
    use super::*;

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
            let def = desugarer.bitter.defs[self].clone();
            Alloc::alloc(desugarer, def)
        }
    }
    impl DeepClone for b::PatId {
        fn deep_clone(&self, desugarer: &mut Desugarer) -> Self {
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
                | b::Pattern::Triv(_pat) => b::Triv.into(),
                | b::Pattern::Cons(pat) => {
                    let b::Cons(a, b) = pat;
                    let a = a.deep_clone(desugarer);
                    let b = b.deep_clone(desugarer);
                    b::Cons(a, b).into()
                }
            };
            Alloc::alloc(desugarer, pat)
        }
    }
    impl DeepClone for b::TermId {
        fn deep_clone(&self, desugarer: &mut Desugarer) -> Self {
            let term = desugarer.bitter.terms[self].clone();
            let term = match &term {
                | b::Term::Internal(term) => {
                    use crate::syntax::Internal;
                    match term {
                        | Internal::VType => {
                            return desugarer.vtype();
                        }
                        | Internal::CType => {
                            return desugarer.ctype();
                        }
                        | Internal::Thunk => {
                            return desugarer.thunk();
                        }
                        | Internal::Ret => {
                            return desugarer.ret();
                        }
                        | Internal::Unit => {
                            return desugarer.unit();
                        }
                        | Internal::Int => {
                            return desugarer.int();
                        }
                        | Internal::Char => {
                            return desugarer.char();
                        }
                        | Internal::String => {
                            return desugarer.string();
                        }
                        | Internal::OS => {
                            return desugarer.os();
                        }
                        | Internal::Monad => {
                            return desugarer.monad();
                        }
                        | Internal::Algebra => {
                            return desugarer.algebra();
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
                | b::Term::Triv(_term) => b::Triv.into(),
                | b::Term::Cons(term) => {
                    let b::Cons(a, b) = term;
                    let a = a.deep_clone(desugarer);
                    let b = b.deep_clone(desugarer);
                    b::Cons(a, b).into()
                }
                | b::Term::Abs(term) => {
                    let b::Abs(params, tail) = term;
                    let params = params.deep_clone(desugarer);
                    let tail = tail.deep_clone(desugarer);
                    b::Abs(params, tail).into()
                }
                | b::Term::App(term) => {
                    let b::App(a, b) = term;
                    let a = a.deep_clone(desugarer);
                    let b = b.deep_clone(desugarer);
                    b::App(a, b).into()
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
                    let b::Ret(term) = term;
                    let term = term.deep_clone(desugarer);
                    b::Ret(term).into()
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
            Alloc::alloc(desugarer, term)
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
    }
}
