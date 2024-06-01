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
                ctx = SccDeclarations(&group).tyck_out(&mut self, ByAction::syn(ctx.clone()))?;
                top.release(group);
            }
        }
        Ok(())
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
    pub deps: Vec<(In, ByAction<Ctx, Ann>)>,
    pub kont: In,
}

pub struct ByAction<Ctx, Ann> {
    pub ctx: Ctx,
    pub switch: Switch<Ann>,
}

#[derive(Clone, Copy, Debug)]
pub enum Switch<Ann> {
    Syn,
    Ana(Ann),
}

impl<Ctx, Ann> ByAction<Ctx, Ann> {
    pub fn syn(ctx: Ctx) -> Self {
        Self { ctx, switch: Switch::Syn }
    }
    pub fn ana(ctx: Ctx, ann: Ann) -> Self {
        Self { ctx, switch: Switch::Ana(ann) }
    }
    pub fn switch(ctx: Ctx, switch: Switch<Ann>) -> Self {
        ByAction { ctx, switch }
    }
}

pub struct SccDeclarations<'decl>(pub &'decl HashSet<su::DeclId>);
impl<'decl> Tyck for SccDeclarations<'decl> {
    type Ctx = Context<ss::AnnId>;
    type Out = Self::Ctx;
    // type Out = Context<ss::AnnId>;
    type Ann = ();
    type Mode = Mode<Self::Ctx, Self, Self::Out, Self::Ann>;
    type Action = ByAction<Self::Ctx, Self::Ann>;

    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<(Self::Out, Self::Ann)> {
        let mode = self.tyck_step(tycker, action)?;
        match mode {
            | Mode::Task(_) => unimplemented!(),
            | Mode::Done(ctx, _) => Ok((ctx, ())),
        }
    }

    fn tyck_step(
        &self, tycker: &mut Tycker, ByAction { ctx, switch: _ }: Self::Action,
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
                            let ann = syn_ann.tyck_ann(tycker, ByAction::syn(ctx.clone()))?;
                            let _ = match ann {
                                | ss::AnnId::Kind(kd) => {
                                    // the binder is a type, register it before analyzing the bindee
                                    let ann = binder
                                        .tyck_ann(tycker, ByAction::ana(ctx.clone(), kd.into()))?;
                                    let _ = bindee.tyck(tycker, ByAction::ana(ctx.clone(), ann))?;
                                }
                                | ss::AnnId::Type(ty) => {
                                    // the binder is a value, directly analyze the bindee
                                    let ann = bindee
                                        .tyck_ann(tycker, ByAction::ana(ctx.clone(), ty.into()))?;
                                    // and then register the binder as a value
                                    let _ = binder.tyck(tycker, ByAction::ana(ctx.clone(), ann))?;
                                }
                            };
                        } else {
                            // synthesize the bindee
                            let ann = bindee.tyck_ann(tycker, ByAction::syn(ctx.clone()))?;
                            binder.tyck(tycker, ByAction::ana(ctx.clone(), ann))?;
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
                                        ctx.clone(),
                                        def,
                                        ss::ThunkTy.into(),
                                        syn_kd,
                                    )?,
                                    | su::Internal::Ret => tycker.register_prim_ty(
                                        ctx.clone(),
                                        def,
                                        ss::RetTy.into(),
                                        syn_kd,
                                    )?,
                                    | su::Internal::Unit => tycker.register_prim_ty(
                                        ctx.clone(),
                                        def,
                                        ss::UnitTy.into(),
                                        syn_kd,
                                    )?,
                                    | su::Internal::Int => tycker.register_prim_ty(
                                        ctx.clone(),
                                        def,
                                        ss::IntTy.into(),
                                        syn_kd,
                                    )?,
                                    | su::Internal::Char => tycker.register_prim_ty(
                                        ctx.clone(),
                                        def,
                                        ss::CharTy.into(),
                                        syn_kd,
                                    )?,
                                    | su::Internal::String => tycker.register_prim_ty(
                                        ctx.clone(),
                                        def,
                                        ss::StringTy.into(),
                                        syn_kd,
                                    )?,
                                    | su::Internal::OS => tycker.register_prim_ty(
                                        ctx.clone(),
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
                                let Some(ty) = ty else { Err(TyckError::MissingAnnotation)? };
                                let ty = ty.tyck_ann(tycker, ByAction::syn(ctx.clone()))?.as_type();
                                let _ =
                                    binder.tyck(tycker, ByAction::ana(ctx.clone(), ty.into()))?;
                            }
                        }
                    }
                    | Decl::Main(decl) => {
                        let su::Main(term) = decl;
                        let ty = term.tyck_ann(tycker, ByAction::syn(ctx.clone()))?.as_type();
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
                            let syn_ann = bindee
                                .syntactically_annotated(tycker)
                                .ok_or_else(|| TyckError::MissingAnnotation)?;
                            let ann = syn_ann.tyck_ann(tycker, ByAction::syn(ctx.clone()))?;
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
        Ok(Mode::Done(ctx, ()))
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
    type Action = ByAction<Self::Ctx, Self::Ann>;

    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<(Self::Out, Self::Ann)> {
        todo!()
    }

    fn tyck_step(
        &self, tycker: &mut Tycker, ByAction { ctx, switch }: Self::Action,
    ) -> Result<Self::Mode> {
        // Fixme: nonsense right now
        let pat = tycker.scoped.pats[self].clone();
        use su::Pattern as Pat;
        match pat {
            | Pat::Ann(pat) => {
                let su::Ann { tm, ty } = pat;
                tm.tyck(tycker, ByAction::switch(ctx.clone(), switch))?;
                ty.tyck(tycker, ByAction::syn(ctx))?;
            }
            | Pat::Hole(pat) => {
                let su::Hole = pat;
            }
            | Pat::Var(_def) => {}
            | Pat::Ctor(pat) => {
                let su::Ctor(_ctor, tail) = pat;
                tail.tyck(tycker, ByAction::switch(ctx, switch))?;
            }
            | Pat::Triv(pat) => {
                let su::Triv = pat;
            }
            | Pat::Cons(pat) => {
                let su::Cons(a, b) = pat;
                a.tyck(tycker, ByAction::switch(ctx.clone(), switch.clone()))?;
                b.tyck(tycker, ByAction::switch(ctx, switch))?;
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
    type Action = ByAction<Self::Ctx, Self::Ann>;

    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<(Self::Out, Self::Ann)> {
        todo!()
    }

    fn tyck_step(
        &self, tycker: &mut Tycker, ByAction { ctx, switch }: Self::Action,
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
                let su::Ann { tm, ty } = term;
                let (ty, ann) = ty.tyck(tycker, ByAction::switch(ctx.clone(), switch))?;
                match (ty, ann) {
                    | (ss::TermId::Type(ty), ss::AnnId::Kind(_kd)) => {
                        let (out, ann) = tm.tyck(tycker, ByAction::ana(ctx.clone(), ty.into()))?;
                        Ok(Mode::Done(out, ann))
                    }
                    | (_ty, _ann) => Err(TyckError::TypeMismatch)?,
                }
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
