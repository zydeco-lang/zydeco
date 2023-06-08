use super::{
    err::ResolveError,
    syntax::{Ctx, Declaration, TopLevel},
};
use crate::textual::syntax::{self as ts, *};

/* -------------------------------- Resolver -------------------------------- */

pub struct Resolver<'a> {
    // references to textual syntax
    pub textual_ctx: &'a ts::Ctx,
    pub textual_top: &'a ts::TopLevel,
    // new binded syntax that is being built
    pub ctx: Ctx,
    pub top: TopLevel,
    // temp
    module_stack: Vec<ModName>,
    lookup_stack: Vec<im::HashMap<NameRef<VarName>, DefId>>,
    span_stack: Vec<Span>,
}

impl Resolver<'_> {
    pub fn new<'a>(textual_ctx: &'a ts::Ctx, textual_top: &'a ts::TopLevel) -> Resolver<'a> {
        Resolver {
            textual_ctx,
            textual_top,
            ctx: Ctx::default(),
            top: TopLevel::default(),
            module_stack: Default::default(),
            lookup_stack: Default::default(),
            span_stack: Default::default(),
        }
    }
    pub fn mod_enter(&mut self, name: &ModName) {
        self.module_stack.push(name.clone());
    }
    pub fn mod_exit(&mut self) {
        self.module_stack.pop();
    }
    /// a "reset point" for the lookup context
    pub fn scope_enter(&mut self) {
        self.lookup_stack.push(self.ctx.lookup.clone());
    }
    /// visit last reset point and consume it
    pub fn scope_exit(&mut self) {
        self.ctx.lookup = self.lookup_stack.pop().unwrap_or_default();
    }
    /// a "reset point" for the span
    pub fn span_enter(&mut self, span: Span) {
        self.span_stack.push(span);
    }
    /// get the current span
    pub fn span(&self) -> Span {
        self.span_stack.last().cloned().unwrap_or(Span::dummy())
    }
    /// discard the last span
    pub fn span_exit(&mut self) {
        self.span_stack.pop();
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
    fn resolve(&self, _state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        Ok(*self)
    }
}

impl Resolve for Hole {
    type Out = Hole;
    type Error = ResolveError;
    fn resolve(&self, _state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        Ok(self.clone())
    }
}

impl Resolve for CtorName {
    type Out = CtorName;
    type Error = ResolveError;
    fn resolve(&self, _state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        Ok(self.clone())
    }
}

impl Resolve for DtorName {
    type Out = DtorName;
    type Error = ResolveError;
    fn resolve(&self, _state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        Ok(self.clone())
    }
}

impl Resolve for TypeArmName {
    type Out = TypeArmName;
    type Error = ResolveError;
    fn resolve(&self, _state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
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
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        let name = state.textual_ctx.defs[*self].clone().inner();
        // Todo: work out module resolution
        state.ctx.lookup.insert(NameRef(Vec::new(), name), *self);
        Ok(*self)
    }
}

impl Resolve for NameRef<VarName> {
    type Out = DefId;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        Ok(*state
            .ctx
            .lookup
            .get(self)
            .ok_or_else(|| ResolveError::UnboundVar(state.span().make(self.clone())))?)
    }
}

/* --------------------------------- Pattern -------------------------------- */

impl Resolve for PatternId {
    type Out = PatternId;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        let sp_pattern = &state.textual_ctx.patterns[*self];
        state.span_enter(sp_pattern.info.clone());
        let pattern = sp_pattern.try_map_ref(|pattern| pattern.resolve(state))?;
        state.span_exit();
        Ok(state.ctx.pattern(self.clone(), pattern))
    }
}
impl Resolve for Pattern {
    type Out = Pattern;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        match self {
            Pattern::Ann(Annotation { term, ty }) => {
                // annotations are resolved before the binders
                let ty = ty.resolve(state)?;
                let term = term.resolve(state)?;
                Ok(Annotation { term, ty }.into())
            }
            Pattern::Var(def) => {
                // binders are inserted into the lookup table
                let def = def.resolve(state)?;
                Ok(Pattern::Var(def))
            }
            Pattern::Hole(h) => Ok(h.resolve(state)?.into()),
        }
    }
}

/* ---------------------------------- Term ---------------------------------- */

struct GenBinder(PatternId);
impl Resolve for GenBinder {
    type Out = PatternId;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        let GenBinder(binder) = self;
        let binder = binder.resolve(state)?;
        match state.ctx.patterns[binder].inner {
            Pattern::Ann(_) => {
                let pat = &state.textual_ctx.patterns[binder];
                let def = pat.inner.get_def_id(state.textual_ctx);
                let name = match def {
                    Some(def) => state.textual_ctx.defs[def].clone(),
                    None => pat.info.make(VarName(String::from("<internal>"))),
                };
                Err(ResolveError::AmbiguousBinderAnnotation(name))
            }
            Pattern::Var(_) | Pattern::Hole(_) => Ok(binder),
        }
    }
}

impl Resolve for GenBind {
    type Out = GenBind;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        let GenBind {
            rec,
            fun,
            binder: binder_old,
            params: params_old,
            ty: ty_old,
            bindee: bindee_old,
        } = self;
        // currently all GenBind params are pi typed definitions
        let rec = rec.resolve(state)?;
        let fun = fun.resolve(state)?;
        let binder;
        let params;
        let ty;
        let bindee;
        if rec {
            // binders are in scope for the bindee
            binder = GenBinder(*binder_old).resolve(state)?;
            state.scope_enter();
            params = params_old.resolve(state)?;
            ty = ty_old.resolve(state)?;
            bindee = bindee_old.resolve(state)?;
            state.scope_exit();
        } else {
            state.scope_enter();
            params = params_old.resolve(state)?;
            ty = ty_old.resolve(state)?;
            bindee = bindee_old.resolve(state)?;
            state.scope_exit();
            // binders are **not** in scope for the bindee
            binder = GenBinder(*binder_old).resolve(state)?;
        };
        Ok(GenBind { rec, fun, binder, params, ty, bindee })
    }
}

impl Resolve for Matcher<TermId> {
    type Out = Matcher<TermId>;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        let Matcher { name, binders, tail } = self;
        state.scope_enter();
        let name = name.resolve(state)?;
        let binders = binders.resolve(state)?;
        let tail = tail.resolve(state)?;
        state.scope_exit();
        Ok(Matcher { name, binders, tail })
    }
}

impl Resolve for CoMatcher<TermId> {
    type Out = CoMatcher<TermId>;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        let CoMatcher { name, binders, tail } = self;
        state.scope_enter();
        let name = name.resolve(state)?;
        let binders = binders.resolve(state)?;
        let tail = tail.resolve(state)?;
        state.scope_exit();
        Ok(CoMatcher { name, binders, tail })
    }
}

impl Resolve for TermId {
    type Out = TermId;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        let sp_term = &state.textual_ctx.terms[*self];
        state.span_enter(sp_term.info.clone());
        let term = sp_term.try_map_ref(|term| term.resolve(state))?;
        state.span_exit();
        Ok(state.ctx.term(self.clone(), term))
    }
}
impl Resolve for Term<NameRef<VarName>> {
    type Out = Term<DefId>;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver) -> Result<Self::Out, Self::Error> {
        match self {
            Term::Ann(Annotation { term, ty }) => {
                let ty = ty.resolve(state)?;
                let term = term.resolve(state)?;
                Ok(Annotation { term, ty }.into())
            }
            Term::Hole(hole) => Ok(hole.resolve(state)?.into()),
            Term::Var(v) => {
                let def = v.resolve(state)?;
                Ok(Term::Var(def))
            }
            Term::Abs(Abstraction(params, term)) => {
                state.scope_enter();
                let params = params.resolve(state)?;
                let term = term.resolve(state)?;
                state.scope_exit();
                Ok(Abstraction(params, term).into())
            }
            Term::App(Application(term, args)) => {
                let term = term.resolve(state)?;
                let args = args.resolve(state)?;
                Ok(Application(term, args).into())
            }
            Term::Rec(Recursion(binder, term)) => {
                state.scope_enter();
                let binder = binder.resolve(state)?;
                let term = term.resolve(state)?;
                state.scope_exit();
                Ok(Recursion(binder, term).into())
            }
            Term::Pi(Pi(params, term)) => {
                state.scope_enter();
                let params = params.resolve(state)?;
                let term = term.resolve(state)?;
                state.scope_exit();
                Ok(Pi(params, term).into())
            }
            Term::Arrow(Arrow(ty_in, ty_out)) => {
                let ty_in = ty_in.resolve(state)?;
                let ty_out = ty_out.resolve(state)?;
                Ok(Arrow(ty_in, ty_out).into())
            }
            Term::Forall(Forall(params, ty)) => {
                state.scope_enter();
                let params = params.resolve(state)?;
                let ty = ty.resolve(state)?;
                state.scope_exit();
                Ok(Forall(params, ty).into())
            }
            Term::Exists(Exists(params, ty)) => {
                state.scope_enter();
                let params = params.resolve(state)?;
                let ty = ty.resolve(state)?;
                state.scope_exit();
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
                state.scope_enter();
                let bindee = bindee.resolve(state)?;
                state.scope_exit();
                let binder = binder.resolve(state)?;
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
    fn resolve(&self, _state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        Ok(self.clone())
    }
}

impl Resolve for TypeArm {
    type Out = TypeArm;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        let TypeArm { name, args, out } = self;
        let name = name.resolve(state)?;
        let args = args.resolve(state)?;
        let out = out.resolve(state)?;
        Ok(TypeArm { name, args, out })
    }
}

impl Resolve for ts::Modifiers<ts::TypeDef> {
    type Out = Vec<Declaration>;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        let Modifiers { public: _, external, inner: ts::TypeDef { head, name, params, arms } } =
            self;
        if *external && arms.is_some() {
            let name = &state.textual_ctx.defs[*name];
            Err(ResolveError::ExternButDefined(name.clone()))?
        } else if !*external && arms.is_none() {
            // peeking ahead to see if this is a definition later
            let def = name;
            let name = state.textual_ctx.defs[*def].clone().inner();
            // Todo: work out module resolution
            if let Some(prev_def) = state.ctx.peeks.insert(NameRef(Vec::new(), name), *def) {
                let name = state.textual_ctx.defs[prev_def].clone();
                Err(ResolveError::DeclaredButNotDefined(name))?
            }
            Ok(Vec::new())
        } else {
            let head = head.resolve(state)?;
            let name = name.resolve(state)?;
            state.scope_enter();
            let params = params.resolve(state)?;
            let arms = arms.resolve(state)?;
            state.scope_exit();
            Ok(vec![TypeDef { head, name, params, arms }.into()])
        }
    }
}

impl Resolve for ts::Modifiers<ts::Define> {
    type Out = Vec<Declaration>;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        let Modifiers { public: _, external, inner: ts::Define(gen) } = self;
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
}

impl Resolve for Modifiers<ts::Declaration> {
    type Out = Vec<Declaration>;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        let Modifiers { public: _, external: _, inner: decl } = self;
        match decl {
            ts::Declaration::Type(type_def) => {
                let decls = self.try_map_ref(|_| Ok(type_def.clone()))?.resolve(state)?;
                Ok(decls)
            }
            ts::Declaration::Define(def) => {
                let decls = self.try_map_ref(|_| Ok(def.clone()))?.resolve(state)?;
                Ok(decls)
            }
            ts::Declaration::Module(Module { name: _, top: None }) => {
                // Todo: work out module resolution
                unimplemented!()
            }
            ts::Declaration::Module(Module { name: NameDef(name), top: Some(ts::TopLevel(top_)) }) => {
                state.mod_enter(name);
                // Todo: work out module resolution
                let mut top = Vec::new();
                for decl in top_ {
                    top.extend(decl.resolve(state)?);
                }
                state.mod_exit();
                Ok(top)
            }
            ts::Declaration::UseDef(_) => {
                // Todo: work out module resolution
                Ok(Vec::new())
            }
            ts::Declaration::Main(Main(term)) => {
                state.scope_enter();
                let term = term.resolve(state)?;
                state.scope_exit();
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
        for def in self.ctx.peeks.values() {
            let name = self.textual_ctx.defs[*def].clone();
            errors.push(ResolveError::DeclaredButNotDefined(name));
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}
