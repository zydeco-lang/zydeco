use crate::{
    surface_syntax::{PrimDef, ScopedArena, SpanArena},
    syntax::{Context, StaticsArena},
    *,
};
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
        let mut ctx = Context::new();
        loop {
            let groups = top.top();
            // if no more groups are at the top, we're done
            if groups.is_empty() {
                break;
            }
            for group in groups {
                // each group should be type checked on its own
                SccDeclarations(&group).tyck(&mut self, Action::syn(ctx.clone()))?;
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
        &mut self, ctx: Context<ss::AnnId>, def: ss::DefId, prim: ss::Type, syn_kd: su::TermId,
    ) -> Result<()> {
        let kd = syn_kd.tyck_out(self, Action::syn(ctx))?.as_kind();
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
    type Ctx;
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

pub enum Mode<Ctx, In, Out, Ann> {
    Task(Task<Ctx, In, Ann>),
    Done(Out, Ann),
}

pub struct Task<Ctx, In, Ann> {
    pub deps: Vec<Action<Ctx, In, Ann>>,
    pub kont: In,
}

pub struct Action<Ctx, In, Ann> {
    pub ctx: Ctx,
    pub input: In,
    pub switch: Switch<Ann>,
}

#[derive(Clone, Copy, Debug)]
pub enum Switch<Ann> {
    Syn,
    Ana(Ann),
}

type SelfAction<Ctx, Ty> = Action<Ctx, (), Ty>;

impl<Ctx, Ann> SelfAction<Ctx, Ann> {
    pub fn syn(ctx: Ctx) -> Self {
        Self { ctx, input: (), switch: Switch::Syn }
    }
    pub fn ana(ctx: Ctx, ann: Ann) -> Self {
        Self { ctx, input: (), switch: Switch::Ana(ann) }
    }
    pub fn switch(ctx: Ctx, switch: Switch<Ann>) -> Self {
        Action { ctx, input: (), switch }
    }
}

pub struct SccDeclarations<'decl>(pub &'decl HashSet<su::DeclId>);
impl<'decl> Tyck for SccDeclarations<'decl> {
    type Ctx = Context<ss::AnnId>;
    type Out = ();
    // type Out = Context<ss::AnnId>;
    type Ann = ();
    type Mode = Mode<Self::Ctx, Self, Self::Out, Self::Ann>;
    type Action = SelfAction<Self::Ctx, Self::Ann>;

    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<(Self::Out, Self::Ann)> {
        let mode = self.tyck_step(tycker, action)?;
        Ok(((), ()))
    }

    fn tyck_step(
        &self, tycker: &mut Tycker, Action { ctx, input: (), switch }: Self::Action,
    ) -> Result<Self::Mode> {
        let SccDeclarations(decls) = self;
        let decls: &HashSet<_> = decls;
        match decls.len() {
            | 0 => unreachable!(),
            | 1 => {
                // just synthesize
                let id = decls.iter().next().unwrap();
                use su::Declaration as Decl;
                match tycker.scoped.decls[id].clone() {
                    | Decl::AliasBody(decl) => {
                        let su::AliasBody { binder, bindee } = decl;
                        // we need to get the annotation for the sake of self referencing type definitions
                        let syn_ann = bindee.syntactically_annotated(tycker);
                        if let Some(syn_ann) = syn_ann {
                            // try analyzing the bindee after synthesizing the type
                            let ann = syn_ann.tyck_ann(tycker, Action::syn(ctx.clone()))?;
                            let _ = match ann {
                                | ss::AnnId::Kind(kd) => {
                                    // the binder is a type, register it before analyzing the bindee
                                    let ann = binder
                                        .tyck_ann(tycker, Action::ana(ctx.clone(), kd.into()))?;
                                    let _ = bindee.tyck(tycker, Action::ana(ctx, ann))?;
                                }
                                | ss::AnnId::Type(ty) => {
                                    // the binder is a value, directly analyze the bindee
                                    let ann = bindee
                                        .tyck_ann(tycker, Action::ana(ctx.clone(), ty.into()))?;
                                    // and then register the binder as a value
                                    let _ = binder.tyck(tycker, Action::ana(ctx, ann))?;
                                }
                            };
                        } else {
                            // synthesize the bindee
                            let ann = bindee.tyck_ann(tycker, Action::syn(ctx.clone()))?;
                            binder.tyck(tycker, Action::ana(ctx, ann))?;
                        }
                    }
                    | Decl::AliasHead(decl) => {
                        let su::AliasHead { binder, ty } = decl;
                        let internal_or = tycker.scoped.exts.get(id).cloned();
                        match internal_or {
                            | Some((internal, def)) => {
                                // the alias head is a internal type
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
                                    | su::Internal::Thunk => tycker.register_prim_ty(
                                        ctx,
                                        def,
                                        ss::ThunkTy.into(),
                                        syn_kd,
                                    )?,
                                    | su::Internal::Ret => tycker.register_prim_ty(
                                        ctx,
                                        def,
                                        ss::RetTy.into(),
                                        syn_kd,
                                    )?,
                                    | su::Internal::Unit => tycker.register_prim_ty(
                                        ctx,
                                        def,
                                        ss::UnitTy.into(),
                                        syn_kd,
                                    )?,
                                    | su::Internal::Int => tycker.register_prim_ty(
                                        ctx,
                                        def,
                                        ss::IntTy.into(),
                                        syn_kd,
                                    )?,
                                    | su::Internal::Char => tycker.register_prim_ty(
                                        ctx,
                                        def,
                                        ss::CharTy.into(),
                                        syn_kd,
                                    )?,
                                    | su::Internal::String => tycker.register_prim_ty(
                                        ctx,
                                        def,
                                        ss::StringTy.into(),
                                        syn_kd,
                                    )?,
                                    | su::Internal::OS => tycker.register_prim_ty(
                                        ctx,
                                        def,
                                        ss::OSTy.into(),
                                        syn_kd,
                                    )?,
                                    | su::Internal::Monad | su::Internal::Algebra => {
                                        unreachable!()
                                    }
                                }
                            }
                            | None => {
                                // the alias head is a primitive value that needs to be linked later
                                let Some(ty) = ty else {
                                    Err(TyckError::MissingAnnotation(
                                        tycker.spans.decls[id].clone(),
                                    ))?
                                };
                                let ty = ty.tyck_ann(tycker, Action::syn(ctx.clone()))?.as_type();
                                let _ = binder.tyck(tycker, Action::ana(ctx, ty.into()))?;
                            }
                        }
                    }
                    | Decl::Main(decl) => {
                        let su::Main(term) = decl;
                        let ty = term.tyck_ann(tycker, Action::syn(ctx))?.as_type();
                        // Todo: check that ty is OS, waiting for lub
                        Lub::lub(&ty, &tycker.os(), tycker, Debruijn::new())?;
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
                        | Decl::AliasBody(decl) => {
                            let su::AliasBody { binder: _, bindee } = decl;
                            let syn_ann =
                                bindee.syntactically_annotated(tycker).ok_or_else(|| {
                                    let span = tycker.spans.decls[id].clone();
                                    TyckError::MissingAnnotation(span)
                                })?;
                            let ann = syn_ann.tyck_ann(tycker, Action::syn(ctx.clone()))?;
                            anns.insert(id, ann);
                        }
                        | Decl::AliasHead(_) | Decl::Main(_) => {
                            unreachable!()
                        }
                    }
                }
                todo!()
            }
        }
        Ok(Mode::Done((), ()))
    }
}

impl Tyck for su::DefId {
    type Ctx = ();
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
    type Ctx = Context<ss::AnnId>;
    type Out = ss::PatId;
    type Ann = ss::AnnId;
    type Mode = Mode<Self::Ctx, Self, Self::Out, Self::Ann>;
    type Action = SelfAction<Self::Ctx, Self::Ann>;

    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<(Self::Out, Self::Ann)> {
        todo!()
    }

    fn tyck_step(
        &self, tycker: &mut Tycker, Action { ctx, input: (), switch }: Self::Action,
    ) -> Result<Self::Mode> {
        // Fixme: nonsense right now
        let pat = tycker.scoped.pats[self].clone();
        use su::Pattern as Pat;
        match pat {
            | Pat::Ann(pat) => {
                let su::Ann { tm, ty } = pat;
                tm.tyck(tycker, Action::switch(ctx.clone(), switch))?;
                ty.tyck(tycker, Action::syn(ctx))?;
            }
            | Pat::Hole(pat) => {
                let su::Hole = pat;
            }
            | Pat::Var(_def) => {}
            | Pat::Ctor(pat) => {
                let su::Ctor(_ctor, tail) = pat;
                tail.tyck(tycker, Action::switch(ctx, switch))?;
            }
            | Pat::Triv(pat) => {
                let su::Triv = pat;
            }
            | Pat::Cons(pat) => {
                let su::Cons(a, b) = pat;
                a.tyck(tycker, Action::switch(ctx.clone(), switch.clone()))?;
                b.tyck(tycker, Action::switch(ctx, switch))?;
            }
        }
        todo!()
    }
}

impl Tyck for su::TermId {
    type Ctx = Context<ss::AnnId>;
    type Out = ss::TermId;
    type Ann = ss::AnnId;
    type Mode = Mode<Self::Ctx, Self, Self::Out, Self::Ann>;
    type Action = SelfAction<Self::Ctx, Self::Ann>;

    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<(Self::Out, Self::Ann)> {
        todo!()
    }

    fn tyck_step(
        &self, tycker: &mut Tycker, Action { ctx, input, switch }: Self::Action,
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
            | Tm::Triv(term) => {
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
    impl Alloc<ss::TypeId> for ss::Sealed<ss::TypeId> {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
    impl Alloc<ss::TypeId> for ss::Hole {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
    impl Alloc<ss::TypeId> for ss::Abstract {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
    impl Alloc<ss::TypeId> for ss::DefId {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
    impl Alloc<ss::TypeId> for ss::Abs<ss::TPatId, ss::TypeId> {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
    impl Alloc<ss::TypeId> for ss::App<ss::TypeId, ss::TypeId> {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
    impl Alloc<ss::TypeId> for ss::ThunkTy {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
    impl Alloc<ss::TypeId> for ss::RetTy {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
    impl Alloc<ss::TypeId> for ss::UnitTy {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
    impl Alloc<ss::TypeId> for ss::IntTy {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
    impl Alloc<ss::TypeId> for ss::CharTy {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
    impl Alloc<ss::TypeId> for ss::StringTy {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
    impl Alloc<ss::TypeId> for ss::OSTy {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
    impl Alloc<ss::TypeId> for ss::Arrow<ss::TypeId> {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
    impl Alloc<ss::TypeId> for ss::Forall {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
    impl Alloc<ss::TypeId> for ss::Prod<ss::TypeId> {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
    impl Alloc<ss::TypeId> for ss::Exists {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
    impl Alloc<ss::TypeId> for ss::Data {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
    impl Alloc<ss::TypeId> for ss::CoData {
        fn alloc(tycker: &mut Tycker, val: Self) -> ss::TypeId {
            Alloc::alloc(tycker, ss::Type::from(val))
        }
    }
}
