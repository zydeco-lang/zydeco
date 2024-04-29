use super::{builtins::Builtin, syntax::*};
use crate::{prelude::rc, statics::syntax as ss};
use im::Vector;

impl From<ss::Program> for Program {
    fn from(p: ss::Program) -> Self {
        let ss::Program { module, entry } = p;
        let module = module.inner().into();
        let entry = entry.inner_ref().into();
        Self { module, entry }
    }
}

impl From<ss::Module> for Module {
    fn from(m: ss::Module) -> Self {
        let mut define = Vector::new();
        let std_library = Builtin::std_library();
        for DeclSymbol {
            public: _,
            external: _,
            inner: ss::Define { name: (sym, _ty), def: () },
        } in m.define_ext
        {
            // Note: synthesize the arity from the type?
            let def = std_library
                .get(&sym)
                .expect("no implementation found for the extern term definition")
                .clone();
            define.push_back((sym, def));
        }
        for DeclSymbol { public: _, external: _, inner: ss::Define { name, def } } in m.define {
            define.push_back((name, def.inner_ref().into()));
        }
        Self { name: m.name, define }
    }
}

impl From<&ss::TermValue> for SynVal {
    fn from(v: &ss::TermValue) -> Self {
        match v {
            ss::TermValue::Annotation(Annotation { term: body, ty: _ }) => body.inner_ref().into(),
            ss::TermValue::Var(x) => x.clone().into(),
            ss::TermValue::Thunk(Thunk(e)) => Thunk(rc!(e.inner_ref().into())).into(),
            ss::TermValue::Ctor(Ctor { ctorv: ctor, args }) => {
                let args = args.iter().map(|v| rc!(v.inner_ref().into())).collect();
                Ctor { ctorv: ctor.clone(), args }.into()
            }
            ss::TermValue::Literal(l) => l.clone().into(),
            ss::TermValue::Pack(Pack { ty: _, body }) => body.inner_ref().into(),
        }
    }
}

impl From<&ss::TermComputation> for SynComp {
    fn from(e: &ss::TermComputation) -> Self {
        match e {
            ss::TermComputation::Annotation(Annotation { term: body, ty: _ }) => {
                body.inner_ref().into()
            }
            ss::TermComputation::Abs(Abs { param, body }) => {
                let body = rc!(body.inner_ref().into());
                Abs { param: param.clone(), body }.into()
            }
            ss::TermComputation::App(App { body, arg }) => {
                let body = rc!(body.inner_ref().into());
                let arg = rc!(arg.inner_ref().into());
                App { body, arg }.into()
            }
            ss::TermComputation::Ret(Ret(v)) => Ret(rc!(v.inner_ref().into())).into(),
            ss::TermComputation::Force(Force(v)) => Force(rc!(v.inner_ref().into())).into(),
            ss::TermComputation::Let(Let { var, def, body }) => {
                let def = rc!(def.inner_ref().into());
                let body = rc!(body.inner_ref().into());
                Let { var: var.clone(), def, body }.into()
            }
            ss::TermComputation::Do(Do { var, comp, body }) => {
                let comp = rc!(comp.inner_ref().into());
                let body = rc!(body.inner_ref().into());
                Do { var: var.clone(), comp, body }.into()
            }
            ss::TermComputation::Rec(Rec { var, body }) => {
                let body = rc!(body.inner_ref().into());
                Rec { var: var.clone(), body }.into()
            }
            ss::TermComputation::Match(Match { scrut, arms }) => {
                let scrut = rc!(scrut.inner_ref().into());
                let arms = arms
                    .iter()
                    .map(|Matcher { ctorv: ctor, vars, body }| {
                        let body = rc!(body.inner_ref().into());
                        Matcher { ctorv: ctor.clone(), vars: vars.clone(), body }
                    })
                    .collect();
                Match { scrut, arms }.into()
            }
            ss::TermComputation::Comatch(Comatch { arms }) => {
                let arms = arms
                    .iter()
                    .map(|Comatcher { dtorv: dtor, body }| {
                        let body = rc!(body.inner_ref().into());
                        Comatcher { dtorv: dtor.clone(), body }
                    })
                    .collect();
                Comatch { arms }.into()
            }
            ss::TermComputation::Dtor(Dtor { body, dtorv: dtor }) => {
                let body = rc!(body.inner_ref().into());
                Dtor { body, dtorv: dtor.clone() }.into()
            }
            ss::TermComputation::BeginBlock(BeginBlock { monad, body }) => {
                use crate::lift::MonadTransTerm;
                (&body.inner_ref().lift(monad.inner_ref())).into()
            }
            ss::TermComputation::TyAbsTerm(Abs { param: _, body }) => body.inner_ref().into(),
            ss::TermComputation::TyAppTerm(App { body, arg: _ }) => body.inner_ref().into(),
            ss::TermComputation::MatchPack(MatchPack { scrut, tvar: _, var, body }) => {
                let scrut = rc!(scrut.inner_ref().into());
                let body = rc!(body.inner_ref().into());
                Let { var: var.clone(), def: scrut, body }.into()
            }
        }
    }
}

impl Module {
    pub fn pure(name: Option<String>) -> Self {
        Self { name, define: Vector::new() }
    }
}
