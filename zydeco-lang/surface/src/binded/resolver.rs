use super::{
    err::ResolveError,
    syntax::{Context, TopLevel},
};
use crate::textual::syntax::{self as ts, *};

pub struct Resolver<'a> {
    // references to textual syntax
    pub textual_arena: &'a ts::Context,
    pub textual_top: &'a ts::TopLevel,
    // new binded syntax that is being built
    pub context: Context,
    pub top: TopLevel,
}

pub trait Fold<S, E, U> {
    fn fold(&self, state: &mut S) -> Result<U, E>;
}

impl<T, U> Fold<Resolver<'_>, ResolveError, U> for T
where
    T: Resolve<U>,
{
    fn fold(&self, state: &mut Resolver) -> Result<U, ResolveError> {
        self.resolve(state)
    }
}

pub trait Resolve<T> {
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<T, ResolveError>;
}

/* ---------------------------------- Meta ---------------------------------- */

impl Resolve<bool> for bool {
    fn resolve(&self, _state: &mut Resolver<'_>) -> Result<bool, ResolveError> {
        Ok(*self)
    }
}

impl<T: Resolve<T>> Resolve<Option<T>> for Option<T> {
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Option<T>, ResolveError> {
        match self {
            Some(t) => t.resolve(state).map(Some),
            None => Ok(None),
        }
    }
}

impl<T: Resolve<T>> Resolve<Vec<T>> for Vec<T> {
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Vec<T>, ResolveError> {
        self.iter().map(|t| t.resolve(state)).collect()
    }
}

/* --------------------------------- Pattern -------------------------------- */

impl Resolve<PatternId> for PatternId {
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<PatternId, ResolveError> {
        let pattern =
            &state.textual_arena.patterns[*self].try_map_ref(|pattern| pattern.resolve(state))?;
        Ok(state.context.pattern(pattern.clone()))
    }
}
impl Resolve<Pattern> for Pattern {
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Pattern, ResolveError> {
        match self {
            Pattern::Ann(Annotation { term, ty }) => {
                let term = term.resolve(state)?;
                let ty = ty.resolve(state)?;
                Ok(Annotation { term, ty }.into())
            }
            Pattern::Var(def) => {
                let name = state.textual_arena.defs[*def].clone().inner();
                state.context.lookup.insert(NameRef(Vec::new(), name), *def);
                Ok((*def).into())
            }
            Pattern::Hole(_) => Ok(self.clone()),
        }
    }
}

/* ---------------------------------- Term ---------------------------------- */

impl Resolve<GenBind> for GenBind {
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<GenBind, ResolveError> {
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

impl Resolve<Matcher<TermId>> for Matcher<TermId> {
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Matcher<TermId>, ResolveError> {
        let Matcher { name, binders, tail } = self;
        let name = name.clone();
        let binders = binders.resolve(state)?;
        let tail = tail.resolve(state)?;
        Ok(Matcher { name, binders, tail })
    }
}

impl Resolve<CoMatcher<TermId>> for CoMatcher<TermId> {
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<CoMatcher<TermId>, ResolveError> {
        let CoMatcher { name, binders, tail } = self;
        let name = name.clone();
        let binders = binders.resolve(state)?;
        let tail = tail.resolve(state)?;
        Ok(CoMatcher { name, binders, tail })
    }
}

impl Resolve<TermId> for TermId {
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<TermId, ResolveError> {
        let term = &state.textual_arena.terms[*self].try_map_ref(|term| term.resolve(state))?;
        Ok(state.context.term(term.clone()))
    }
}
impl Resolve<Term<DefId>> for Term<NameRef<VarName>> {
    fn resolve(&self, state: &mut Resolver) -> Result<Term<DefId>, ResolveError> {
        match self {
            Term::Ann(Annotation { term, ty }) => {
                let term = term.resolve(state)?;
                let ty = ty.resolve(state)?;
                Ok(Annotation { term, ty }.into())
            }
            Term::Hole(Hole) => Ok(Hole.into()),
            Term::Var(v) => {
                let def = *state
                    .context
                    .lookup
                    .get(v)
                    .ok_or_else(|| ResolveError::UnboundVar(v.clone()))?;
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
                // let name = name.resolve(state)?;
                let name = name.clone();
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
                // let name = name.resolve(state)?;
                let name = name.clone();
                let args = args.resolve(state)?;
                Ok(Destructor(term, name, args).into())
            }
            Term::Lit(l) => Ok(l.clone().into()),
        }
    }
}

/* -------------------------------- TopLevel -------------------------------- */

impl<'a> Resolver<'a> {
    pub fn exec(&mut self) -> Result<(), Vec<ResolveError>> {
        let errors = Vec::new();
        let ts::TopLevel(decls) = self.textual_top;
        for Modifiers { public: _, external: _, inner: decl } in decls {
            match decl {
                Declaration::Type(TypeDef { head: _, name: _, params: _, arms: _ }) => {}
                Declaration::Define(_) => todo!(),
                Declaration::Module(_) => todo!(),
                Declaration::UseDef(_) => todo!(),
                Declaration::Main(_) => todo!(),
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}
