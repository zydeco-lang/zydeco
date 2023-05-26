use super::{
    err::ResolveError,
    syntax::{Context, TopLevel},
};
use crate::textual::syntax::{self as ts, *};

pub struct Resolver<'a> {
    pub textual_arena: &'a ts::Context,
    pub textual_top: &'a ts::TopLevel,
    pub context: Context,
    pub top: TopLevel,
}

// pub trait Fold<S, E, T> {
//     fn fold(&self, state: &mut S) -> Result<T, E>;
// }

// impl Fold<Resolver<'_>, ResolveError, Term<DefId>> for Term<NameRef<VarName>> {
//     fn fold(&self, state: &mut Resolver) -> Result<Term<DefId>, ResolveError> {
//         todo!()
//     }
// }

pub trait Resolve<T> {
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<T, ResolveError>;
}
impl<T: Resolve<T>> Resolve<Vec<T>> for Vec<T> {
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Vec<T>, ResolveError> {
        self.iter().map(|t| t.resolve(state)).collect()
    }
}

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
                state.context.lookup.insert(NameRef(Vec::new(), name), def.clone());
                Ok(def.clone().into())
            }
            Pattern::Hole(_) => Ok(self.clone()),
        }
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
                let def = state
                    .context
                    .lookup
                    .get(v)
                    .ok_or_else(|| ResolveError::UnboundVar(v.clone()))?
                    .clone();
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
                todo!()
                // let binding = binding.resolve(state)?;
                // let tail = tail.resolve(state)?;
                // Ok(PureBind { binding, tail }.into())
            }
            Term::Ctor(Constructor(name, args)) => {
                // let name = name.resolve(state)?;
                let name = name.clone();
                let args = args.resolve(state)?;
                Ok(Constructor(name, args).into())
            }
            Term::Match(Match { scrut, arms }) => {
                todo!()
            }
            Term::CoMatch(CoMatch { arms }) => {
                todo!()
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

// pub trait Resolve {
//     fn resolve(&self, resolver: &mut Resolver, top: &mut ResolvedTop) -> Result<(), ResolveError>;
// }

// impl<'a> Resolver<'a> {
//     pub fn exec(&mut self, top: &mut ResolvedTop) -> Result<(), Vec<ResolveError>> {
//         let mut errors = Vec::new();
//         let ts::TopLevel(decls) = self.textual_top;
//         for Modifiers { public, external, inner: decl } in decls {
//             match decl {
//                 Declaration::Type(TypeDef { head, name, params, arms }) => {}
//                 Declaration::Define(_) => todo!(),
//                 Declaration::Module(_) => todo!(),
//                 Declaration::UseDef(_) => todo!(),
//                 Declaration::Main(_) => todo!(),
//             }
//         }
//         if errors.is_empty() {
//             Ok(())
//         } else {
//             Err(errors)
//         }
//     }
// }
