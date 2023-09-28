use im::HashMap;

use super::{
    err::ResolveError,
    syntax::{Ctx, Declaration, PublicDec, TopLevel},
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
    module_stack: Vec<String>,
    lookup_stack_new: Vec<im::HashMap<Vec<String>, im::HashMap<VarName, DefId>>>,
    pub heads: Vec<Vec<String>>,
    pub module_tree: ModuleTree,

    /// all public variables under current scope
    ///  
    /// or just use DefId?
    current_pub_stack: Vec<Vec<PublicDec>>,
    span_stack: Vec<Span>,
}

impl Resolver<'_> {
    pub fn new<'a>(
        textual_ctx: &'a ts::Ctx, textual_top: &'a ts::TopLevel, ctx: Ctx, heads: Vec<Vec<String>>,
        mut outer_module: Vec<String>, module_tree: ModuleTree,
    ) -> Resolver<'a> {
        outer_module.pop();
        Resolver {
            textual_ctx,
            textual_top,
            ctx,
            top: TopLevel::default(),
            module_stack: outer_module,
            lookup_stack_new: Default::default(),
            heads,
            module_tree,
            current_pub_stack: Default::default(),
            span_stack: Default::default(),
        }
    }
    pub fn mod_enter(&mut self, name: &String) {
        if !self.module_stack.is_empty() {
            self.module_tree.get_node_path_mut(&self.module_stack).unwrap().add_child(name.clone());
        }
        self.module_stack.push(name.clone());
    }
    pub fn mod_exit(&mut self) -> HashMap<Vec<String>, HashMap<VarName, DefId>> {
        self.module_stack.pop();
        self.ctx.lookup.clone()
    }
    pub fn get_mod(&mut self) -> Vec<String> {
        self.module_stack.clone()
    }
    pub fn pub_enter(&mut self) {
        self.current_pub_stack.push(self.ctx.current_pub.clone());
        self.ctx.current_pub.clear();
    }
    pub fn pub_exit(&mut self) {
        self.ctx.current_pub = self.current_pub_stack.pop().unwrap_or_default();
    }
    pub fn add_pub_def(&mut self, name: VarName) {
        self.ctx.current_pub.push(PublicDec::Def(name));
    }
    pub fn add_pub_mod(&mut self, name: String) {
        self.ctx.current_pub.push(PublicDec::Module(name));
    }
    pub fn add_pub_use(&mut self, name: NameRef<UseEnum>) {
        self.ctx.current_pub.push(PublicDec::Use(name));
    }
    /// a "reset point" for the lookup context
    pub fn scope_enter(&mut self) {
        self.lookup_stack_new.push(self.ctx.lookup.clone());
    }
    /// visit last reset point and consume it
    pub fn scope_exit(&mut self) {
        self.ctx.lookup = self.lookup_stack_new.pop().unwrap_or_default();
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

    pub fn retain_scope(
        &mut self, path: &Vec<String>, name: &String,
        top_lookup: &HashMap<Vec<String>, HashMap<VarName, DefId>>,
    ) {
        let mut path = path.clone();
        path.push(name.clone());
        if let Some(scope) = top_lookup.get(&path) {
            self.ctx.lookup.insert(path.clone(), scope.clone());
            if let Some(tree) = self.module_tree.get_node_path(&path) {
                for child in &tree.children {
                    self.retain_scope(&path, &child.root.0, top_lookup)
                }
            }
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
        let name = state.textual_ctx.defs[*self].clone();
        if state.ctx.peeks.contains_key(&name) {
            state.ctx.peeks.remove(&name);
        }
        // Todo: work out module resolution
        let cur_module: Vec<String> = Vec::new();
        let var_set = state.ctx.lookup.get(&cur_module);
        match var_set {
            Some(old_map) => {
                // if old_map.get(&name).is_some() {
                //     Err(ResolveError::DefineTwice(state.textual_ctx.spans.defs[*self].make(name)))
                // } else { Question: do b <- ! b;
                let mut new_map = old_map.clone();
                new_map.insert(name.clone(), *self);
                state.ctx.lookup.insert(cur_module, new_map);
                Ok(*self)
                // }
            }
            None => {
                let new_map = im::hashmap! {name => *self};
                state.ctx.lookup.insert(cur_module, new_map);
                Ok(*self)
            }
        }
    }
}

impl Resolve for NameRef<VarName> {
    type Out = DefId;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        let NameRef(mod_path, name) = self;
        if let Some(def_id) = state.ctx.peeks.get(name) {
            return Ok(*def_id);
        }
        let path = mod_path.iter().map(|ModName(s)| s.clone()).collect::<Vec<_>>();
        let default_map: HashMap<VarName, DefId> = HashMap::new();
        let mut found_in_his: Option<DefId> = None;
        for head in state.heads.clone() {
            if let Some(his_scope) = state.ctx.lookup.get(&head) {
                if let Some(def_id) = his_scope.get(name) {
                    found_in_his = Some(def_id.clone());
                    break;
                }
            }
        }
        match found_in_his {
            Some(def_id) => Ok(def_id),
            None => Ok(*state
                .ctx
                .lookup
                .get(&path)
                .unwrap_or(&default_map)
                .get(name)
                .ok_or_else(|| ResolveError::UnboundVar(state.span().make(self.clone())))?),
        }
    }
}

/* --------------------------------- Pattern -------------------------------- */

impl Resolve for PatternId {
    type Out = PatternId;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        let info = &state.textual_ctx.spans[*self];
        state.span_enter(info.clone());
        let pattern = (&state.textual_ctx.patterns[*self]).resolve(state)?;
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
        match state.ctx.patterns[binder] {
            Pattern::Ann(_) => {
                let pat = &state.textual_ctx.patterns[binder];
                let def = pat.get_def_id(state.textual_ctx);
                let name = match def {
                    Some(def) => state.textual_ctx.defs[def].clone(),
                    None => VarName(String::from("<internal>")),
                };
                Err(ResolveError::AmbiguousBinderAnnotation(
                    state.textual_ctx.spans[binder].make(name),
                ))
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
        let info = &state.textual_ctx.spans[*self];
        let term = &state.textual_ctx.terms[*self];
        state.span_enter(info.clone());
        let term = term.resolve(state)?;
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
        let Modifiers { public, external, inner: ts::TypeDef { head, name, params, arms } } = self;
        if *external && arms.is_some() {
            let info = &state.textual_ctx.spans[*name];
            let name = &state.textual_ctx.defs[*name];
            Err(ResolveError::ExternButDefined(info.make(name.clone())))?
        } else if !*external && arms.is_none() {
            // peeking ahead to see if this is a definition later
            let def = name;
            let name = state.textual_ctx.defs[*def].clone();
            if let Some(prev_def) = state.ctx.peeks.insert(name, *def) {
                let info = state.textual_ctx.spans[prev_def].clone();
                let name = state.textual_ctx.defs[prev_def].clone();
                Err(ResolveError::DeclaredButNotDefined(info.make(name)))?
            }
            Ok(Vec::new())
        } else {
            if *public {
                let var_name: &VarName = &state.textual_ctx.defs[*name];
                state.add_pub_def(var_name.clone());
            }
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
        let Modifiers { public, external, inner: ts::Define(gen) } = self;
        let gen = gen.resolve(state)?;
        if *external && gen.bindee.is_some() {
            let info = &state.textual_ctx.spans[gen.binder];
            let pat = &state.textual_ctx.patterns[gen.binder];
            let def = pat.get_def_id(state.textual_ctx);
            let name = match def {
                Some(def) => state.textual_ctx.defs[def].clone(),
                None => VarName(String::from("<internal>")),
            };
            Err(ResolveError::ExternButDefined(info.make(name)))?
        }
        if *public {
            let pat = &state.textual_ctx.patterns[gen.binder];
            let def = pat.get_def_id(state.textual_ctx);
            let var_name = match def {
                Some(def) => state.textual_ctx.defs[def].clone(),
                None => VarName(String::from("<internal>")),
            };
            // abstract above into a function?
            state.add_pub_def(var_name.clone());
        }
        Ok(vec![Define(gen).into()])
    }
}

impl Resolve for UseDef {
    type Out = Vec<PublicDec>;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        let UseDef(path_name) = self;
        let mod_path: Vec<String> =
            path_name.0.clone().iter().map(|ModName(s)| s.clone()).collect();
        let use_enum = &path_name.1;
        match use_enum {
            ts::UseEnum::All(_) => {
                let mut path = state.module_stack.clone();
                while !path.is_empty() {
                    let mut prefix_path = path.clone();
                    prefix_path.extend(mod_path.clone());
                    if let Some(_) = state.ctx.lookup.get(&prefix_path).cloned() {
                        state.heads.push(prefix_path);
                        return Ok(vec![PublicDec::Use(path_name.clone())]);
                    }
                    path.pop();
                }
                Err(ResolveError::ModuleNotFound(state.span().make(NameRef(
                    mod_path.clone().iter().map(|s| ModName(s.clone())).collect(),
                    VarName(String::from("..")),
                ))))
            }
            ts::UseEnum::Cluster(UseCluster(use_defs)) => {
                let mut pub_decls = vec![];
                for UseDef(NameRef(in_path, in_enum)) in use_defs {
                    // Add path/ before all usew_defs
                    let mut new_path = path_name.0.clone();
                    new_path.extend(in_path.clone());
                    pub_decls.extend(UseDef(NameRef(new_path, in_enum.clone())).resolve(state)?);
                }
                Ok(pub_decls)
            }
            ts::UseEnum::Name(name) => {
                let mut path = state.module_stack.clone();
                while !path.is_empty() {
                    let mut prefix_path = path.clone();
                    prefix_path.extend(mod_path.clone());
                    if let Some(mut scope) = state.ctx.lookup.get(&prefix_path).cloned() {
                        if let Some(def_id) = scope.get(name).cloned() {
                            scope.remove(name);
                            state.ctx.lookup.insert(prefix_path, scope);
                            let cur_path: Vec<String> = vec![String::from(".")];
                            let cur_scope =
                                state.ctx.lookup.entry(cur_path).or_insert(HashMap::default());
                            cur_scope.insert(name.clone(), def_id);
                            return Ok(vec![PublicDec::Use(path_name.clone())]);
                        }
                    }
                    path.pop();
                }
                Err(ResolveError::ModuleNotFound(state.span().make(NameRef(
                    mod_path.iter().map(|s| ModName(s.clone())).collect(),
                    name.clone(),
                ))))
            }
            _ => Ok(vec![]),
        }
    }
}

impl Resolve for Modifiers<ts::Declaration> {
    type Out = Vec<Declaration>;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        let Modifiers { public, external: _, inner: decl } = self;
        match decl {
            ts::Declaration::Type(type_def) => {
                let decls = self.try_map_ref(|_| Ok(type_def.clone()))?.resolve(state)?;
                Ok(decls)
            }
            ts::Declaration::Define(def) => {
                let decls = self.try_map_ref(|_| Ok(def.clone()))?.resolve(state)?;
                Ok(decls)
            }
            ts::Declaration::Module(Module { name: NameDef(ModName(name)), top: None }) => {
                if *public {
                    state.add_pub_mod(name.clone())
                }

                Ok(Vec::new())
            }
            ts::Declaration::Module(Module {
                name: NameDef(ModName(name)),
                top: Some(ts::TopLevel(top_)),
            }) => {
                state.mod_enter(name);
                state.pub_enter();
                // When entering a new module, take a snapshot of the lookup table
                state.scope_enter();
                let mut top = Vec::new();
                for decl in top_ {
                    top.extend(decl.resolve(state)?);
                }
                let module_path = state.get_mod(); // the path of the module we are in
                let top_lookup = state.mod_exit(); // the lookup table inside the module
                let default_map: HashMap<VarName, DefId> = HashMap::default();
                let in_modules = top_lookup.get(&Vec::new()).unwrap_or(&default_map).clone(); // the scope of the path of the module
                state.scope_exit();
                // When exiting a new module, abandon the new defs in the module except the ones with "pub"
                let mut var_map = state.ctx.lookup.remove(&module_path).unwrap_or_default();
                for pub_decl in state.ctx.current_pub.clone() {
                    match pub_decl {
                        PublicDec::Def(name) => {
                            var_map.insert(name.clone(), in_modules.get(&name).unwrap().clone());
                        }
                        PublicDec::Module(name) => {
                            state.retain_scope(&module_path, &name, &top_lookup)
                        }
                        PublicDec::Use(name) => {
                            let NameRef(mod_path, use_enum) = name;
                            let path: Vec<String> = mod_path.iter().map(| ModName(s) | s.clone()).collect();
                            match use_enum {
                                UseEnum::Name(name) => {
                                    if let Some(from_scope) = state.ctx.lookup.get(&path) {
                                        if let Some(id) = from_scope.get(&name) {
                                            var_map.insert(name.clone(), id.clone());
                                            // only copy the used variable to the target scope 
                                        }
                                    }
                                },
                                UseEnum::Alias(_) => todo!(),
                                UseEnum::All(_) => {
                                    if let Some(move_scope) = state.ctx.lookup.get(&path) {
                                        var_map.extend(move_scope.clone())
                                        // copy the whole scope to the target scope
                                    }
                                },
                                _ => {}
                            }
                        }
                    }
                }
                state.ctx.lookup.insert(module_path, var_map);
                state.pub_exit();
                if *public {
                    state.add_pub_mod(name.clone())
                }
                Ok(top)
            }
            ts::Declaration::UseDef(use_def) => {
                if *public {
                    let pubs = use_def.resolve(state)?;
                    state.ctx.current_pub.extend(pubs);
                } else {
                    use_def.resolve(state)?;
                }
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
        // Current path?
        if !self.heads.contains(vec![String::from(".")].as_ref()) {
            self.heads.push(vec![String::from(".")]);
        }
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
        // if !self.ctx.current_pub.is_empty() {
        // }
        for def in self.ctx.peeks.values() {
            let info = self.textual_ctx.spans[*def].clone();
            let name = self.textual_ctx.defs[*def].clone();
            errors.push(ResolveError::DeclaredButNotDefined(info.make(name)));
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}
