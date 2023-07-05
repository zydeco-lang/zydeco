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
    module_stack: Vec<ModName>,
    lookup_stack_new: Vec<im::HashMap<Vec<ModName>, im::HashMap<VarName, DefId>>>,
    pub heads: Vec<Vec<ModName>>,

    /// all public variables under current scope
    ///  
    /// or just use DefId?
    current_pub: Vec<PublicDec>,
    span_stack: Vec<Span>,
}

impl Resolver<'_> {
    pub fn new<'a>(
        textual_ctx: &'a ts::Ctx, textual_top: &'a ts::TopLevel, ctx: Ctx, heads: Vec<Vec<ModName>>,
    ) -> Resolver<'a> {
        Resolver {
            textual_ctx,
            textual_top,
            ctx,
            top: TopLevel::default(),
            module_stack: Default::default(),
            lookup_stack_new: Default::default(),
            heads,
            current_pub: Default::default(),
            span_stack: Default::default(),
        }
    }
    pub fn mod_enter(&mut self, name: &ModName) {
        if !self.module_stack.is_empty() {
            self.ctx.cur_module.push(self.module_stack.last().unwrap().clone());
        }
        self.module_stack.push(name.clone());
    }
    pub fn mod_exit(&mut self) -> HashMap<VarName, DefId> {
        println!("mod exit"); //Debug
        debug_print_path(self.get_mod());
        self.ctx.debug_print_lookup(String::from("mod exit"));
        
        let path: Vec<ModName> = Vec::new();
        let default_in_module: HashMap<VarName, DefId> = HashMap::new();
        let in_module = self.ctx.lookup_new.get(&path).unwrap_or(&default_in_module).clone();
        self.module_stack.pop();
        self.ctx.cur_module.pop();
        in_module
    }
    pub fn get_mod(&mut self) -> Vec<ModName> {
        self.module_stack.clone()
    }
    pub fn add_pub_def(&mut self, name: VarName) {
        self.current_pub.push(PublicDec::Def(name));
    }
    pub fn add_pub_mod(&mut self, name: ModName) {
        self.current_pub.push(PublicDec::Module(name));
    }
    // pub fn add_pub_use(&mut self, name: NameRef<UseEnum>) {
    //     self.current_pub.push(PublicDec::Use(name));
    // }
    // TODO: pub use ...
    /// a "reset point" for the lookup context
    pub fn scope_enter(&mut self) {
        self.lookup_stack_new.push(self.ctx.lookup_new.clone());
    }
    /// visit last reset point and consume it
    pub fn scope_exit(&mut self) {
        self.ctx.lookup_new = self.lookup_stack_new.pop().unwrap_or_default();
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

    pub fn debug_cur_pub(&mut self) {
        print!("Under current module: ");
        debug_print_path(self.get_mod().clone());
        for pub_dec in self.current_pub.clone() {
            match pub_dec {
                PublicDec::Module(ModName(mod_name)) => {
                    print!("m({}), ", mod_name);
                }
                PublicDec::Def(var_name) => {
                    print!("v({}), ", var_name);
                }
            }
        }
        print!("\n");
    }
    pub fn debug_cur_use(&mut self, path: Vec<ModName>, use_enum: UseEnum) {
        print!("Current path: ");
        debug_print_path(self.get_mod());
        print!("Use path: ");
        debug_print_path(path);
        match use_enum {
            UseEnum::Name(VarName(name)) => println!("var name: {}\n", name),
            UseEnum::Alias(_) => println!("alias"),
            UseEnum::All(_) => println!("all"),
            UseEnum::Cluster(_) => println!("cluster"),
        }
    }
    pub fn debug_cur_heads(&mut self) {
        println!("Current heads: ");
        for head in self.heads.clone() {
            print!("--");
            debug_print_path(head);
        }
        print!("\n");
    }
}

pub fn debug_print_path(path: Vec<ModName>) {
    for ModName(mod_name) in path {
        print!("{}/", mod_name);
    }
    print!("\n");
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
        // Todo: work out module resolution
        let cur_module: Vec<ModName> = Vec::new();
        let var_set = state.ctx.lookup_new.get(&cur_module);
        match var_set {
            Some(old_map) => {
                // if old_map.get(&name).is_some() {
                //     Err(ResolveError::DefineTwice(state.textual_ctx.spans.defs[*self].make(name)))
                // } else { Question: do b <- ! b;
                let mut new_map = old_map.clone();
                new_map.insert(name.clone(), *self);
                state.ctx.lookup_new.insert(cur_module, new_map);
                Ok(*self)
                // }
            }
            None => {
                let new_map = im::hashmap! {name => *self};
                state.ctx.lookup_new.insert(cur_module, new_map);
                Ok(*self)
            }
        }
    }
}

impl Resolve for NameRef<VarName> {
    type Out = DefId;
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        let NameRef(path, name) = self;
        // println!("Looking for {}", name); //Debug
        // state.ctx.debug_print_lookup(String::from("looking for")); //Debug
        // state.debug_cur_heads(); //Debug
        let default_map: HashMap<VarName, DefId> = HashMap::new();
        let mut found_in_his: Option<DefId> = None;
        for head in state.heads.clone() {
            let his_scope = state.ctx.lookup_new.get(&head);
            if his_scope.is_some() {
                let _def_id = his_scope.unwrap().get(name);
                if _def_id.is_some() {
                    found_in_his = Some(_def_id.unwrap().clone());
                    break;
                }
            }
        }
        match found_in_his {
            Some(def_id) => Ok(def_id),
            None => Ok(*state
                .ctx
                .lookup_new
                .get(path)
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
            // Todo: work out module resolution
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
    type Out = ();
    type Error = ResolveError;
    fn resolve(&self, state: &mut Resolver<'_>) -> Result<Self::Out, Self::Error> {
        let UseDef(path_name) = self;
        let use_enum = &path_name.1;
        match use_enum {
            ts::UseEnum::All(_) => {
                let mut path = state.ctx.cur_module.clone();
                path.extend(path_name.0.clone());
                if state.ctx.lookup_new.get(&path).is_some() && !state.heads.contains(&path) {
                    state.heads.extend(vec![path]);
                    Ok(())
                } else {
                    //TODO: No such module name error
                    Ok(())
                }
            }
            ts::UseEnum::Cluster(UseCluster(use_defs)) => {
                for UseDef(NameRef(in_path, in_enum)) in use_defs {
                    // Add path/ before all usew_defs
                    let mut new_path = path_name.0.clone();
                    new_path.extend(in_path.clone());
                    // change to another function?
                    let _ = UseDef(NameRef(new_path, in_enum.clone())).resolve(state);
                }
                Ok(())
            }
            ts::UseEnum::Name(name) => {
                let mut path = state.ctx.cur_module.clone();
                path.extend(path_name.0.clone());
                let mut _scope = state.ctx.lookup_new.get(&path).cloned();
                // move Path/ key to ./ key
                match _scope {
                    Some(mut scope) => {
                        let _def_id = scope.get(name).cloned();
                        match _def_id {
                            Some(def_id) => {
                                scope.remove(name);
                                state.ctx.lookup_new.insert(path, scope);
                                let cur_path: Vec<ModName> = Vec::new();
                                let default_scope: HashMap<VarName, DefId> = HashMap::default();
                                let mut cur_scope = state
                                    .ctx
                                    .lookup_new
                                    .get(&cur_path)
                                    .unwrap_or(&default_scope)
                                    .clone();
                                cur_scope.insert(name.clone(), def_id);
                                state.ctx.lookup_new.insert(cur_path, cur_scope);
                                Ok(())
                            }
                            None => Err(ResolveError::UnboundVar(
                                state.span().make(NameRef(path, name.clone())),
                            )),
                        }
                    }
                    None => Err(ResolveError::UnboundVar(
                        state.span().make(NameRef(path, name.clone())),
                    )),
                }
            }
            _ => Ok(()),
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
            ts::Declaration::Module(Module { name: NameDef(name), top: None }) => {
                // Todo: work out module resolution
                if *public {
                    state.add_pub_mod(name.clone())
                }

                Ok(Vec::new())
            }
            ts::Declaration::Module(Module {
                name: NameDef(name),
                top: Some(ts::TopLevel(top_)),
            }) => {
                if *public {
                    state.add_pub_mod(name.clone())
                }
                state.mod_enter(name);
                // When entering a new module, take a snapshot of the lookup table
                state.scope_enter();
                // Todo: work out module resolution
                let mut top = Vec::new();
                for decl in top_ {
                    top.extend(decl.resolve(state)?);
                }
                let module_path = state.get_mod();
                let in_modules = state.mod_exit();
                state.ctx.debug_print_lookup(String::from("before scope exit"));
                state.scope_exit();
                state.ctx.debug_print_lookup(String::from("after scope exit"));
                // When exiting a new module, abandon the new defs in the module except the ones with "pub"
                let mut var_map: im::HashMap<VarName, DefId> =
                    if state.ctx.lookup_new.contains_key(&module_path) {
                        state.ctx.lookup_new.get(&module_path).unwrap().clone()
                        // Question: Hardly happen?
                    } else {
                        HashMap::new()
                    };
                state.debug_cur_pub();
                for pub_decl in &state.current_pub {
                    match pub_decl {
                        PublicDec::Def(name) => {
                            var_map.insert(name.clone(), in_modules.get(name).unwrap().clone());
                        }
                        PublicDec::Module(_) => {
                            // Question: useless name?
                            if !state.heads.contains(&module_path) {
                                state.heads.push(module_path.clone());
                            }
                        }
                    }
                }
                state.debug_cur_heads();
                state.ctx.lookup_new.insert(module_path, var_map);
                state.current_pub.clear();
                Ok(top)
            }
            ts::Declaration::UseDef(use_def) => {
                // if *public {
                //     let UseDef(name_ref) = use_def;
                //     state.add_pub_use(name_ref.clone())
                // }
                use_def.resolve(state)?;
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
        if !self.heads.contains(vec![ModName(String::from("."))].as_ref()) {
            self.heads.push(vec![ModName(String::from("."))]);
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
