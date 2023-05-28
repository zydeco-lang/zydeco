use super::{
    err::ResolveError,
    syntax::{Context, Declaration, TopLevel},
};
use crate::textual::syntax::{self as ts, *};

/* -------------------------------- Resolver -------------------------------- */

pub struct Resolver<'a> {
    // references to textual syntax
    pub textual_ctx: &'a ts::Ctx,
    pub textual_top: &'a ts::TopLevel,
    // new binded syntax that is being built
    pub context: Context,
    pub top: TopLevel,
    // temp
    pub span: Span,
}

impl Resolver<'_> {
    pub fn new<'a>(textual_ctx: &'a ts::Ctx, textual_top: &'a ts::TopLevel) -> Resolver<'a> {
        Resolver {
            textual_ctx,
            textual_top,
            context: Context::default(),
            top: TopLevel::default(),
            span: Span::dummy(),
        }
    }
}

pub trait Fold<S, E, U> {
    fn fold(&self, state: &mut S) -> Result<U, E>;
}

pub trait Resolve {
    type Out;
    type Error;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error>;
}

/* ---------------------------------- Meta ---------------------------------- */

impl<T, U, E> Fold<Resolver<'_>, E, U> for T
where
    T: Resolve<Out = U, Error = E>,
{
    fn fold(&self, state: &mut Resolver) -> Result<U, E> {
        self.resolve(state)
    }
}

impl Resolve for bool {
    type Out = bool;
    type Error = ResolveError;
    fn resolve(&self, _state: &mut Resolver<'_>) -> Result<bool, Self::Error> {
        Ok(*self)
    }
}

impl Resolve for Hole {
    type Out = Hole;
    type Error = ResolveError;
    fn resolve(&self, _state: &mut Resolver<'_>) -> Result<Hole, Self::Error> {
        Ok(self.clone())
    }
}

impl Resolve for CtorName {
    type Out = CtorName;
    type Error = ResolveError;
    fn resolve(&self, _state: &mut Resolver<'_>) -> Result<CtorName, Self::Error> {
        Ok(self.clone())
    }
}

impl Resolve for DtorName {
    type Out = DtorName;
    type Error = ResolveError;
    fn resolve(&self, _state: &mut Resolver<'_>) -> Result<DtorName, Self::Error> {
        Ok(self.clone())
    }
}

impl Resolve for TypeArmName {
    type Out = TypeArmName;
    type Error = ResolveError;
    fn resolve(&self, _state: &mut Resolver<'_>) -> Result<TypeArmName, Self::Error> {
        Ok(self.clone())
    }
}

impl<T, U, E> Resolve for Option<T>
where
    T: Resolve<Out = U, Error = E>,
{
    type Out = Option<U>;
    type Error = E;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        match self {
            Some(t) => t.resolve(state).map(Some),
            None => Ok(None),
        }
    }
}

impl<T, U, E> Resolve for Vec<T>
where
    T: Resolve<Out = U, Error = E>,
{
    type Out = Vec<U>;
    type Error = E;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        self.iter().map(|t| t.resolve(state)).collect()
    }
}

/* ---------------------------- Binders and Refs ---------------------------- */

impl Resolve for DefId {
    type Out = DefId;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<DefId, Self::Error> {
        let name = state.textual_ctx.defs[*self].clone().inner();
        // Todo: work out module resolution
        state.context.lookup.insert(NameRef(Vec::new(), name), *self);
        Ok(*self)
    }
}

impl Resolve for NameRef<VarName> {
    type Out = DefId;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<DefId, Self::Error> {
        Ok(*state
            .context
            .lookup
            .get(self)
            .ok_or_else(|| ResolveError::UnboundVar(state.span.make(self.clone())))?)
    }
}

/* --------------------------------- Pattern -------------------------------- */

impl Resolve for PatternId {
    type Out = PatternId;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<PatternId, Self::Error> {
        let sp_pattern = &state.textual_ctx.patterns[*self];
        let span = state.span.clone();
        state.span = sp_pattern.info.clone();
        let pattern = sp_pattern.try_map_ref(|pattern| pattern.resolve(state))?;
        state.span = span;
        Ok(state.context.pattern(pattern))
    }
}
impl Resolve for Pattern {
    type Out = Pattern;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Pattern, Self::Error> {
        match self {
            Pattern::Ann(Annotation { term, ty }) => {
                let term = term.resolve(state)?;
                let ty = ty.resolve(state)?;
                Ok(Annotation { term, ty }.into())
            }
            Pattern::Var(def) => {
                let def = def.resolve(state)?;
                Ok(Pattern::Var(def))
            }
            Pattern::Hole(_) => Ok(self.clone()),
        }
    }
}

/* ---------------------------------- Term ---------------------------------- */

impl Resolve for GenBind {
    type Out = GenBind;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<GenBind, Self::Error> {
        let GenBind { rec, fun, binder, params, ty, bindee } = self;
        let rec = rec.resolve(state)?;
        let fun = fun.resolve(state)?;
        let binder = binder.resolve(state)?;
        let params = params.resolve(state)?;
        let ty = ty.resolve(state)?;
        let bindee = bindee.resolve(state)?;
        Ok(GenBind { rec, fun, binder, params, ty, bindee })
    }
}

impl Resolve for Matcher<TermId> {
    type Out = Matcher<TermId>;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Matcher<TermId>, Self::Error> {
        let Matcher { name, binders, tail } = self;
        let name = name.resolve(state)?;
        let binders = binders.resolve(state)?;
        let tail = tail.resolve(state)?;
        Ok(Matcher { name, binders, tail })
    }
}

impl Resolve for CoMatcher<TermId> {
    type Out = CoMatcher<TermId>;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<CoMatcher<TermId>, Self::Error> {
        let CoMatcher { name, binders, tail } = self;
        let name = name.resolve(state)?;
        let binders = binders.resolve(state)?;
        let tail = tail.resolve(state)?;
        Ok(CoMatcher { name, binders, tail })
    }
}

impl Resolve for TermId {
    type Out = TermId;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<TermId, Self::Error> {
        let sp_term = &state.textual_ctx.terms[*self];
        let span = state.span.clone();
        state.span = sp_term.info.clone();
        let term = sp_term.try_map_ref(|term| term.resolve(state))?;
        state.span = span;
        Ok(state.context.term(term))
    }
}
impl Resolve for Term<NameRef<VarName>> {
    type Out = Term<DefId>;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver) -> Result<Term<DefId>, Self::Error> {
        match self {
            Term::Ann(Annotation { term, ty }) => {
                let term = term.resolve(state)?;
                let ty = ty.resolve(state)?;
                Ok(Annotation { term, ty }.into())
            }
            Term::Hole(hole) => Ok(hole.resolve(state)?.into()),
            Term::Var(v) => {
                let def = v.resolve(state)?;
                Ok(Term::Var(def))
            }
            Term::Abs(Abstraction(params, term)) => {
                let params = params.resolve(state)?;
                let term = term.resolve(state)?;
                Ok(Abstraction(params, term).into())
            }
            Term::App(Application(term, args)) => {
                let term = term.resolve(state)?;
                let args = args.resolve(state)?;
                Ok(Application(term, args).into())
            }
            Term::Rec(Recursion(binder, term)) => {
                let binder = binder.resolve(state)?;
                let term = term.resolve(state)?;
                Ok(Recursion(binder, term).into())
            }
            Term::Pi(Pi(params, term)) => {
                let params = params.resolve(state)?;
                let term = term.resolve(state)?;
                Ok(Pi(params, term).into())
            }
            Term::Arrow(Arrow(ty_in, ty_out)) => {
                let ty_in = ty_in.resolve(state)?;
                let ty_out = ty_out.resolve(state)?;
                Ok(Arrow(ty_in, ty_out).into())
            }
            Term::Forall(Forall(params, ty)) => {
                let params = params.resolve(state)?;
                let ty = ty.resolve(state)?;
                Ok(Forall(params, ty).into())
            }
            Term::Exists(Exists(params, ty)) => {
                let params = params.resolve(state)?;
                let ty = ty.resolve(state)?;
                Ok(Exists(params, ty).into())
            }
            Term::Thunk(Thunk(term)) => {
                let term = term.resolve(state)?;
                Ok(Thunk(term).into())
            }
            Term::Force(Force(term)) => {
                let term = term.resolve(state)?;
                Ok(Force(term).into())
            }
            Term::Ret(Return(term)) => {
                let term = term.resolve(state)?;
                Ok(Return(term).into())
            }
            Term::Do(Bind { binder, bindee, tail }) => {
                let binder = binder.resolve(state)?;
                let bindee = bindee.resolve(state)?;
                let tail = tail.resolve(state)?;
                Ok(Bind { binder, bindee, tail }.into())
            }
            Term::Let(PureBind { binding, tail }) => {
                let binding = binding.resolve(state)?;
                let tail = tail.resolve(state)?;
                Ok(PureBind { binding, tail }.into())
            }
            Term::Ctor(Constructor(name, args)) => {
                let name = name.resolve(state)?;
                let args = args.resolve(state)?;
                Ok(Constructor(name, args).into())
            }
            Term::Match(Match { scrut, arms }) => {
                let scrut = scrut.resolve(state)?;
                let arms = arms.resolve(state)?;
                Ok(Match { scrut, arms }.into())
            }
            Term::CoMatch(CoMatch { arms }) => {
                let arms = arms.resolve(state)?;
                Ok(CoMatch { arms }.into())
            }
            Term::Dtor(Destructor(term, name, args)) => {
                let term = term.resolve(state)?;
                let name = name.resolve(state)?;
                let args = args.resolve(state)?;
                Ok(Destructor(term, name, args).into())
            }
            Term::Lit(l) => Ok(l.clone().into()),
        }
    }
}

/* -------------------------------- TopLevel -------------------------------- */

impl Resolve for TypeDefHead {
    type Out = TypeDefHead;
    type Error = ResolveError;
    fn resolve(&self, _state: &mut Resolver<'_>) -> Result<TypeDefHead, Self::Error> {
        Ok(self.clone())
    }
}

impl Resolve for TypeArm {
    type Out = TypeArm;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<TypeArm, Self::Error> {
        let TypeArm { name, args, out } = self;
        let name = name.resolve(state)?;
        let args = args.resolve(state)?;
        let out = out.resolve(state)?;
        Ok(TypeArm { name, args, out })
    }
}

impl Resolve for Modifiers<ts::Declaration> {
    type Out = Vec<Declaration>;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Vec<Declaration>, Self::Error> {
        let Modifiers { public: _, external, inner: decl } = self;
        match decl {
            ts::Declaration::Type(TypeDef { head, name, params, arms }) => {
                if *external && arms.is_some() {
                    let name = &state.textual_ctx.defs[*name];
                    Err(ResolveError::ExternButDefined(name.clone()))?
                } else if !*external && arms.is_none() {
                    // peeking ahead to see if this is a definition later
                    let def = name;
                    let name = state.textual_ctx.defs[*def].clone().inner();
                    // Todo: work out module resolution
                    if let Some(prev_def) =
                        state.context.peeks.insert(NameRef(Vec::new(), name), *def)
                    {
                        let name = state.textual_ctx.defs[prev_def].clone();
                        Err(ResolveError::DeclaredButNotDefined(name))?
                    }
                    Ok(Vec::new())
                } else {
                    let head = head.resolve(state)?;
                    let name = name.resolve(state)?;
                    let params = params.resolve(state)?;
                    let arms = arms.resolve(state)?;
                    Ok(vec![TypeDef { head, name, params, arms }.into()])
                }
            }
            ts::Declaration::Define(Define(gen)) => {
                let gen = gen.resolve(state)?;
                if *external && gen.bindee.is_some() {
                    let pat = &state.textual_ctx.patterns[gen.binder];
                    let def = pat.inner.get_def_id(state.textual_ctx);
                    let name = match def {
                        Some(def) => state.textual_ctx.defs[def].clone(),
                        None => pat.info.make(VarName(String::from("<internal>"))),
                    };
                    Err(ResolveError::ExternButDefined(name))?
                }
                Ok(vec![Define(gen).into()])
            }
            ts::Declaration::Module(Module { name: _, top: None }) => {
                // Todo: work out module resolution
                unimplemented!()
            }
            ts::Declaration::Module(Module { name: _, top: Some(TopLevel(top_)) }) => {
                // Todo: work out module resolution
                let mut top = Vec::new();
                for decl in top_ {
                    top.extend(decl.resolve(state)?);
                }
                Ok(top)
            }
            ts::Declaration::UseDef(_) => Ok(Vec::new()),
            ts::Declaration::Main(Main(term)) => {
                let term = term.resolve(state)?;
                Ok(vec![Main(term).into()])
            }
        }
    }
}

impl<'a> Resolver<'a> {
    pub fn exec(&mut self) -> Result<(), Vec<ResolveError>> {
        let mut errors = Vec::new();
        let ts::TopLevel(decls) = self.textual_top;
        for decl in decls {
            let decls = match decl.resolve(self) {
                Ok(decls) => decls,
                Err(e) => {
                    errors.push(e);
                    continue;
                }
            };
            self.top.extend(decls);
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}
