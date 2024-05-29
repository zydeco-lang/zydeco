use crate::err::*;
use crate::surface_syntax::{self as su, PrimDef, ScopedArena, SpanArena};
use crate::syntax::{self as ss, StaticsArena};
use std::collections::HashSet;

pub struct Tycker {
    pub spans: SpanArena,
    pub prim: PrimDef,
    pub scoped: ScopedArena,
    pub statics: StaticsArena,
}

impl Tycker {
    pub fn run(mut self) -> Result<()> {
        let mut top = self.scoped.top.clone();
        loop {
            let groups = top.top();
            // if no more groups are at the top, we're done
            if groups.is_empty() {
                break;
            }
            for group in groups {
                // each group should be type checked on its own
                SccDeclarations(&group).tyck(&mut self, ())?;
                top.release(group);
            }
        }
        Ok(())
    }
}

pub trait Tyck {
    type Out;
    type Mode;
    type Action;
    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<Self::Out>;
    fn tyck_step(&self, tycker: &mut Tycker, action: Self::Action) -> Result<Self::Mode>;
}

pub enum Mode<Ctx, Tm, Ty> {
    Task(Task<Ctx, Tm, Ty>),
    Done(Ty),
}

pub struct Task<Ctx, Tm, Ty> {
    pub deps: Vec<Action<Ctx, Tm, Ty>>,
    pub kont: Tm,
}

pub struct Action<Ctx, Tm, Ty> {
    pub ctx: Ctx,
    pub target: Tm,
    pub switch: Switch<Ty>,
}

#[derive(Clone, Copy, Debug)]
pub enum Switch<Ty> {
    Syn,
    Ana(Ty),
}

type SelfAction<Ty> = Action<(), (), Ty>;

impl<Ctx, Ty> Action<Ctx, (), Ty> {
    pub fn syn(ctx: Ctx) -> Self {
        Self { ctx, target: (), switch: Switch::Syn }
    }
    pub fn ana(ctx: Ctx, ty: Ty) -> Self {
        Self { ctx, target: (), switch: Switch::Ana(ty) }
    }
    pub fn switch(ctx: Ctx, switch: Switch<Ty>) -> Action<Ctx, (), Ty> {
        Action { ctx, target: (), switch }
    }
}

pub trait SyntacticallyAnnotated {
    fn syntactically_annotated(&self, tycker: &mut Tycker) -> Option<su::TermId>;
}
impl SyntacticallyAnnotated for su::Declaration {
    fn syntactically_annotated(&self, tycker: &mut Tycker) -> Option<su::TermId> {
        use su::Declaration as Decl;
        match self {
            | Decl::Alias(su::Alias { binder, bindee }) => {
                let _ = binder;
                bindee.syntactically_annotated(tycker)
            }
            | Decl::Extern(su::Extern { binder, params, ty }) => {
                // Todo: implement this
                assert!(params.is_empty());
                let _ = binder;
                if let Some(ty) = ty {
                    Some(*ty)
                } else {
                    None
                }
            }
            | Decl::Main(su::Main(_term)) => None,
        }
    }
}
impl SyntacticallyAnnotated for su::TermId {
    fn syntactically_annotated(&self, tycker: &mut Tycker) -> Option<su::TermId> {
        let term = tycker.scoped.terms[self].clone();
        use su::Term as Tm;
        match &term {
            | Tm::Internal(_) => unreachable!(),
            | Tm::Sealed(term) => {
                let su::Sealed(term) = term;
                term.syntactically_annotated(tycker)
            }
            | Tm::Ann(term) => {
                let su::Ann { tm: _, ty } = term;
                Some(*ty)
            }
            | Tm::Abs(term) => {
                let su::Abs(param, body) = term;
                // Todo: add param to pi
                let span = tycker.spans.terms[body].clone();
                let pi = tycker.spans.terms.alloc(span);
                let body = body.syntactically_annotated(tycker)?;
                tycker.scoped.terms.insert(pi, su::Pi(*param, body).into());
                Some(pi)
            }
            | Tm::Var(_)
            | Tm::Hole(_)
            | Tm::Unit(_)
            | Tm::Cons(_)
            | Tm::App(_)
            | Tm::Rec(_)
            | Tm::Pi(_)
            | Tm::Sigma(_)
            | Tm::Thunk(_)
            | Tm::Force(_)
            | Tm::Ret(_)
            | Tm::Do(_)
            | Tm::Let(_)
            | Tm::Data(_)
            | Tm::CoData(_)
            | Tm::Ctor(_)
            | Tm::Match(_)
            | Tm::CoMatch(_)
            | Tm::Dtor(_)
            | Tm::Lit(_) => None,
        }
    }
}

pub struct SccDeclarations<'decl>(pub &'decl HashSet<su::DeclId>);
impl<'decl> Tyck for SccDeclarations<'decl> {
    type Out = ();
    type Mode = ();
    type Action = ();

    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<Self::Out> {
        self.tyck_step(tycker, action)
    }

    fn tyck_step(&self, tycker: &mut Tycker, (): Self::Action) -> Result<Self::Mode> {
        let SccDeclarations(decls) = self;
        let decls: &HashSet<_> = decls;
        match decls.len() {
            | 0 => unreachable!(),
            | 1 => {
                // just synthesize
                let id = decls.iter().next().unwrap();
                use su::Declaration as Decl;
                match tycker.scoped.decls[id].clone() {
                    | Decl::Alias(decl) => {
                        let su::Alias { binder, bindee } = decl;
                        todo!()
                    }
                    | Decl::Extern(_decl) => {
                        let (internal, _def) = &tycker.scoped.exts[id];
                        match internal {
                            | su::Internal::VType => {
                                let _ = tycker.statics.kinds.alloc(ss::VType.into());
                            }
                            | su::Internal::CType => {
                                let _ = tycker.statics.kinds.alloc(ss::CType.into());
                            }
                            | su::Internal::Thunk => {
                                let _ = tycker.statics.types.alloc(ss::ThunkTy.into());
                                // let _ = tycker.statics.type_of_defs.insert(*def, CType -> VType);
                            }
                            | su::Internal::Ret => {
                                let _ = tycker.statics.types.alloc(ss::RetTy.into());
                                // let _ = tycker.statics.type_of_defs.insert(*def, CType -> VType);
                            }
                            | su::Internal::Unit => {
                                let _ = tycker.statics.types.alloc(ss::UnitTy.into());
                                // let _ = tycker.statics.type_of_defs.insert(*def, CType -> VType);
                            }
                            | su::Internal::Int => {
                                let _ = tycker.statics.types.alloc(ss::IntTy.into());
                                // let _ = tycker.statics.type_of_defs.insert(*def, CType -> VType);
                            }
                            | su::Internal::Char => {
                                let _ = tycker.statics.types.alloc(ss::CharTy.into());
                                // let _ = tycker.statics.type_of_defs.insert(*def, CType -> VType);
                            }
                            | su::Internal::String => {
                                let _ = tycker.statics.types.alloc(ss::StringTy.into());
                                // let _ = tycker.statics.type_of_defs.insert(*def, CType -> VType);
                            }
                            | su::Internal::OS => {
                                let _ = tycker.statics.types.alloc(ss::OSTy.into());
                                // let _ = tycker.statics.type_of_defs.insert(*def, CType -> VType);
                            }
                            | su::Internal::Monad | su::Internal::Algebra => {
                                unreachable!()
                            }
                        }
                        todo!()
                    }
                    | Decl::Main(decl) => {
                        let su::Main(term) = decl;
                        let _ty = term.tyck(tycker, Action::syn(()))?;
                        // Todo: check that ty is OS
                        Ok(())
                    }
                }
            }
            | _ => {
                // mutually recursive declarations must..
                // 1. all be types, and
                // 2. all have kind annotations
                todo!()
            }
        }
    }
}

impl Tyck for su::DefId {
    type Out = ();
    type Mode = ();
    type Action = ();

    fn tyck(&self, _tycker: &mut Tycker, (): Self::Action) -> Result<Self::Out> {
        todo!()
    }

    fn tyck_step(&self, _tycker: &mut Tycker, (): Self::Action) -> Result<Self::Mode> {
        // Fixme: nonsense right now
        Ok(())
    }
}

impl Tyck for su::PatId {
    type Out = ss::PatId;
    type Mode = Mode<(), su::PatId, ss::PatId>;
    type Action = SelfAction<ss::PatId>;

    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<Self::Out> {
        todo!()
    }

    fn tyck_step(
        &self, tycker: &mut Tycker, Action { ctx: (), target: (), switch }: Self::Action,
    ) -> Result<Self::Mode> {
        // Fixme: nonsense right now
        let pat = tycker.scoped.pats[self].clone();
        use su::Pattern as Pat;
        match pat {
            | Pat::Ann(pat) => {
                let su::Ann { tm, ty } = pat;
                tm.tyck(tycker, Action::switch((), switch))?;
                ty.tyck(tycker, Action::syn(()))?;
            }
            | Pat::Hole(pat) => {
                let su::Hole = pat;
            }
            | Pat::Var(_def) => {}
            | Pat::Ctor(pat) => {
                let su::Ctor(_ctor, tail) = pat;
                tail.tyck(tycker, Action::switch((), switch))?;
            }
            | Pat::Unit(pat) => {
                let su::Unit = pat;
            }
            | Pat::Cons(pat) => {
                let su::Cons(a, b) = pat;
                a.tyck(tycker, Action::switch((), switch))?;
                b.tyck(tycker, Action::switch((), switch))?;
            }
        }
        todo!()
    }
}

impl Tyck for su::TermId {
    type Out = ss::TermId;
    type Mode = Mode<(), su::TermId, ss::TermId>;
    type Action = Action<(), (), ss::TermId>;

    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<Self::Out> {
        todo!()
    }

    fn tyck_step(
        &self, tycker: &mut Tycker, Action { ctx, target, switch }: Self::Action,
    ) -> Result<Self::Mode> {
        // Fixme: nonsense right now
        let term = tycker.scoped.terms[self].clone();
        use su::Term as Tm;
        match term {
            | Tm::Internal(_) => unreachable!(),
            | Tm::Sealed(term) => {
                todo!()
            }
            | Tm::Ann(term) => {
                todo!()
            }
            | Tm::Hole(term) => {
                todo!()
            }
            | Tm::Var(term) => {
                todo!()
            }
            | Tm::Unit(term) => {
                todo!()
            }
            | Tm::Cons(term) => {
                todo!()
            }
            | Tm::Abs(term) => {
                todo!()
            }
            | Tm::App(term) => {
                todo!()
            }
            | Tm::Rec(term) => {
                todo!()
            }
            | Tm::Pi(term) => {
                todo!()
            }
            | Tm::Sigma(term) => {
                todo!()
            }
            | Tm::Thunk(term) => {
                todo!()
            }
            | Tm::Force(term) => {
                todo!()
            }
            | Tm::Ret(term) => {
                todo!()
            }
            | Tm::Do(term) => {
                todo!()
            }
            | Tm::Let(term) => {
                todo!()
            }
            | Tm::Data(term) => {
                todo!()
            }
            | Tm::CoData(term) => {
                todo!()
            }
            | Tm::Ctor(term) => {
                todo!()
            }
            | Tm::Match(term) => {
                todo!()
            }
            | Tm::CoMatch(term) => {
                todo!()
            }
            | Tm::Dtor(term) => {
                todo!()
            }
            | Tm::Lit(term) => {
                todo!()
            }
        }
    }
}
