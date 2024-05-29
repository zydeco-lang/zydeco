use crate::err::*;
use crate::lub::*;
use crate::surface_syntax::{self as su, PrimDef, ScopedArena, SpanArena};
use crate::syntax::{self as ss, StaticsArena};
use std::collections::{HashMap, HashSet};
use zydeco_utils::arena::ArenaAccess;

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

impl Tycker {
    pub fn vtype(&mut self) -> ss::KindId {
        let ss::TermId::Kind(kd) = self.statics.defs[self.prim.vtype.get()] else { unreachable!() };
        kd
    }
    pub fn ctype(&mut self) -> ss::KindId {
        let ss::TermId::Kind(kd) = self.statics.defs[self.prim.ctype.get()] else { unreachable!() };
        kd
    }
    pub fn register_prim_ty(
        &mut self, def: ss::DefId, prim: ss::Type, syn_kd: su::TermId,
    ) -> Result<()> {
        let kd = syn_kd.tyck_out(self, Action::syn())?.as_kind();
        let ty = Alloc::alloc(self, prim);
        self.statics.type_of_defs.insert(def, kd.into());
        self.statics.defs.insert(def, ty.into());
        Ok(())
    }
    pub fn thunk(&mut self) -> ss::TypeId {
        let ss::TermId::Type(ty) = self.statics.defs[self.prim.thunk.get()] else { unreachable!() };
        ty
    }
    pub fn ret(&mut self) -> ss::TypeId {
        let ss::TermId::Type(ty) = self.statics.defs[self.prim.ret.get()] else { unreachable!() };
        ty
    }
    pub fn unit(&mut self) -> ss::TypeId {
        let ss::TermId::Type(ty) = self.statics.defs[self.prim.unit.get()] else { unreachable!() };
        ty
    }
    pub fn int(&mut self) -> ss::TypeId {
        let ss::TermId::Type(ty) = self.statics.defs[self.prim.int.get()] else { unreachable!() };
        ty
    }
    pub fn char(&mut self) -> ss::TypeId {
        let ss::TermId::Type(ty) = self.statics.defs[self.prim.char.get()] else { unreachable!() };
        ty
    }
    pub fn string(&mut self) -> ss::TypeId {
        let ss::TermId::Type(ty) = self.statics.defs[self.prim.string.get()] else {
            unreachable!()
        };
        ty
    }
    pub fn os(&mut self) -> ss::TypeId {
        let ss::TermId::Type(ty) = self.statics.defs[self.prim.os.get()] else { unreachable!() };
        ty
    }
}

pub trait Tyck {
    type Out;
    type Ann;
    type Mode;
    type Action;
    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<(Self::Out, Self::Ann)>;
    fn tyck_ann(&self, tycker: &mut Tycker, action: Self::Action) -> Result<Self::Ann> {
        self.tyck(tycker, action).map(|(_, ann)| ann)
    }
    fn tyck_out(&self, tycker: &mut Tycker, action: Self::Action) -> Result<Self::Out> {
        self.tyck(tycker, action).map(|(out, _)| out)
    }
    fn tyck_step(&self, tycker: &mut Tycker, action: Self::Action) -> Result<Self::Mode>;
}

pub enum Mode<In, Out, Ann> {
    Task(Task<In, Ann>),
    Done(Out, Ann),
}

pub struct Task<In, Ann> {
    pub deps: Vec<Action<In, Ann>>,
    pub kont: In,
}

pub struct Action<In, Ann> {
    pub target: In,
    pub switch: Switch<Ann>,
}

#[derive(Clone, Copy, Debug)]
pub enum Switch<Ann> {
    Syn,
    Ana(Ann),
}

type SelfAction<Ty> = Action<(), Ty>;

impl<Ty> Action<(), Ty> {
    pub fn syn() -> Self {
        Self { target: (), switch: Switch::Syn }
    }
    pub fn ana(ty: Ty) -> Self {
        Self { target: (), switch: Switch::Ana(ty) }
    }
    pub fn switch(switch: Switch<Ty>) -> Action<(), Ty> {
        Action { target: (), switch }
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
            | Decl::Extern(su::Extern { binder, ty }) => {
                let _ = binder;
                // add params to pi
                *ty
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
                // add param to pi
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
    type Ann = ();
    type Mode = ((), ());
    type Action = ();

    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<(Self::Out, Self::Ann)> {
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
                        // we need to get the annotation for the sake of self referencing type definitions
                        let syn_ann = bindee.syntactically_annotated(tycker);
                        if let Some(syn_ann) = syn_ann {
                            // try analyzing the bindee after synthesizing the type
                            let ann = syn_ann.tyck_ann(tycker, Action::syn())?;
                            let _ = match ann {
                                | ss::AnnId::Kind(kd) => {
                                    // the binder is a type, register it before analyzing the bindee
                                    let ann = binder.tyck_ann(tycker, Action::ana(kd.into()))?;
                                    let _ = bindee.tyck(tycker, Action::ana(ann))?;
                                }
                                | ss::AnnId::Type(ty) => {
                                    // the binder is a value, directly analyze the bindee
                                    let ann = bindee.tyck_ann(tycker, Action::ana(ty.into()))?;
                                    // and then register the binder as a value
                                    let _ = binder.tyck(tycker, Action::ana(ann))?;
                                }
                            };
                            Ok(((), ()))
                        } else {
                            // synthesize the bindee
                            let ann = bindee.tyck_ann(tycker, Action::syn())?;
                            binder.tyck(tycker, Action::ana(ann))?;
                            Ok(((), ()))
                        }
                    }
                    | Decl::Extern(decl) => {
                        let su::Extern { binder, ty } = decl;
                        let internal_or = tycker.scoped.exts.get(id).cloned();
                        match internal_or {
                            | Some((internal, def)) => {
                                // the extern is a internal type
                                let syn_kd = ty.unwrap();
                                match internal {
                                    | su::Internal::VType => {
                                        let kd = Alloc::alloc(tycker, ss::VType);
                                        // no type_of_defs for VType since it's the largest universe level we can get
                                        tycker.statics.defs.insert(def, kd.into());
                                    }
                                    | su::Internal::CType => {
                                        let kd = Alloc::alloc(tycker, ss::CType);
                                        // no type_of_defs for CType since it's the largest universe level we can get
                                        tycker.statics.defs.insert(def, kd.into());
                                    }
                                    | su::Internal::Thunk => {
                                        tycker.register_prim_ty(def, ss::ThunkTy.into(), syn_kd)?
                                    }
                                    | su::Internal::Ret => {
                                        tycker.register_prim_ty(def, ss::RetTy.into(), syn_kd)?
                                    }
                                    | su::Internal::Unit => {
                                        tycker.register_prim_ty(def, ss::UnitTy.into(), syn_kd)?
                                    }
                                    | su::Internal::Int => {
                                        tycker.register_prim_ty(def, ss::IntTy.into(), syn_kd)?
                                    }
                                    | su::Internal::Char => {
                                        tycker.register_prim_ty(def, ss::CharTy.into(), syn_kd)?
                                    }
                                    | su::Internal::String => {
                                        tycker.register_prim_ty(def, ss::StringTy.into(), syn_kd)?
                                    }
                                    | su::Internal::OS => {
                                        tycker.register_prim_ty(def, ss::OSTy.into(), syn_kd)?
                                    }
                                    | su::Internal::Monad | su::Internal::Algebra => {
                                        unreachable!()
                                    }
                                }
                            }
                            | None => {
                                // the extern is a primitive value that needs to be linked later
                                let Some(ty) = ty else {
                                    Err(TyckError::MissingAnnotation(
                                        tycker.spans.decls[id].clone(),
                                    ))?
                                };
                                let ty = ty.tyck_ann(tycker, Action::syn())?.as_type();
                                let _ = binder.tyck(tycker, Action::ana(ty.into()))?;
                            }
                        }
                        Ok(((), ()))
                    }
                    | Decl::Main(decl) => {
                        let su::Main(term) = decl;
                        let ty = term.tyck_ann(tycker, Action::syn())?.as_type();
                        // Todo: check that ty is OS, waiting for lub
                        Ok(((), ()))
                    }
                }
            }
            | _ => {
                // mutually recursive declarations must..
                // 1. all be types, and
                // 2. all have kind annotations
                let mut anns = HashMap::new();
                for id in decls {
                    let decl = tycker.scoped.decls[id].clone();
                    use su::Declaration as Decl;
                    match decl {
                        | Decl::Alias(decl) => {
                            let su::Alias { binder: _, bindee } = decl;
                            let syn_ann =
                                bindee.syntactically_annotated(tycker).ok_or_else(|| {
                                    let span = tycker.spans.decls[id].clone();
                                    TyckError::MissingAnnotation(span)
                                })?;
                            anns.insert(id, syn_ann.tyck_out(tycker, Action::syn())?);
                        }
                        | Decl::Extern(_) | Decl::Main(_) => {
                            unreachable!()
                        }
                    }
                }
                todo!()
            }
        }
    }
}

impl Tyck for su::DefId {
    type Out = ();
    type Ann = ();
    type Mode = ();
    type Action = ();

    fn tyck(&self, _tycker: &mut Tycker, (): Self::Action) -> Result<(Self::Out, Self::Ann)> {
        todo!()
    }

    fn tyck_step(&self, _tycker: &mut Tycker, (): Self::Action) -> Result<Self::Mode> {
        // Fixme: nonsense right now
        Ok(())
    }
}

impl Tyck for su::PatId {
    type Out = ss::PatId;
    type Ann = ss::AnnId;
    type Mode = Mode<Self, Self::Out, Self::Ann>;
    type Action = SelfAction<Self::Ann>;

    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<(Self::Out, Self::Ann)> {
        todo!()
    }

    fn tyck_step(
        &self, tycker: &mut Tycker, Action { target: (), switch }: Self::Action,
    ) -> Result<Self::Mode> {
        // Fixme: nonsense right now
        let pat = tycker.scoped.pats[self].clone();
        use su::Pattern as Pat;
        match pat {
            | Pat::Ann(pat) => {
                let su::Ann { tm, ty } = pat;
                tm.tyck(tycker, Action::switch(switch))?;
                ty.tyck(tycker, Action::syn())?;
            }
            | Pat::Hole(pat) => {
                let su::Hole = pat;
            }
            | Pat::Var(_def) => {}
            | Pat::Ctor(pat) => {
                let su::Ctor(_ctor, tail) = pat;
                tail.tyck(tycker, Action::switch(switch))?;
            }
            | Pat::Unit(pat) => {
                let su::Unit = pat;
            }
            | Pat::Cons(pat) => {
                let su::Cons(a, b) = pat;
                a.tyck(tycker, Action::switch(switch))?;
                b.tyck(tycker, Action::switch(switch))?;
            }
        }
        todo!()
    }
}

impl Tyck for su::TermId {
    type Out = ss::TermId;
    type Ann = ss::AnnId;
    type Mode = Mode<Self, Self::Out, Self::Ann>;
    type Action = Action<(), Self::Ann>;

    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<(Self::Out, Self::Ann)> {
        todo!()
    }

    fn tyck_step(
        &self, tycker: &mut Tycker, Action { target, switch }: Self::Action,
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

pub(crate) use impls::*;
mod impls {
    use super::*;

    pub(crate) trait Alloc<T> {
        fn alloc(tycker: &mut Tycker, val: Self) -> T;
    }

    impl Alloc<ss::KindId> for ss::Kind {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::KindId {
            let kind = tycker.statics.kinds.alloc(val);
            kind
        }
    }
    impl Alloc<ss::KindId> for ss::VType {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::KindId {
            Alloc::alloc(tycker, ss::Kind::from(val))
        }
    }
    impl Alloc<ss::KindId> for ss::CType {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::KindId {
            Alloc::alloc(tycker, ss::Kind::from(val))
        }
    }
    impl Alloc<ss::KindId> for ss::Arrow<ss::KindId> {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::KindId {
            Alloc::alloc(tycker, ss::Kind::from(val))
        }
    }

    impl Alloc<ss::TypeId> for ss::Type {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            let ty = tycker.statics.types.alloc(val.into());
            ty
        }
    }
}
