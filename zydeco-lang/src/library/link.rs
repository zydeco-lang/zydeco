use super::{builtins, syntax::*};
use crate::rc;
use crate::statics::syntax as ss;
use indexmap::IndexMap;

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
        let mut define = IndexMap::new();
        let std_library = builtins::std_library();
        for DeclSymbol {
            public: _,
            external: _,
            inner: ss::Define { name: (sym, _ty), def: () },
        } in m.define_ext
        {
            let def = std_library[&sym].clone();
            define.insert(sym, def);
        }
        for DeclSymbol {
            public: _,
            external: _,
            inner: ss::Define { name, def },
        } in m.define
        {
            define.insert(name, def.inner_ref().into());
        }
        Self { name: m.name, define }
    }
}

impl From<&ss::TermValue> for ZVal {
    fn from(v: &ss::TermValue) -> Self {
        match v {
            ss::TermValue::Annotation(Annotation { term: body, ty: _ }) => {
                body.inner_ref().into()
            }
            ss::TermValue::Var(x) => x.clone().into(),
            ss::TermValue::Thunk(Thunk(e)) => {
                Thunk(rc!(e.inner_ref().into())).into()
            }
            ss::TermValue::Ctor(Ctor { ctor, args }) => {
                let args =
                    args.iter().map(|v| rc!(v.inner_ref().into())).collect();
                Ctor { ctor: ctor.clone(), args }.into()
            }
            ss::TermValue::Literal(l) => l.clone().into(),
            ss::TermValue::Pack(Pack { ty: _, body }) => {
                body.inner_ref().into()
            }
        }
    }
}

impl From<&ss::TermComputation> for ZComp {
    fn from(e: &ss::TermComputation) -> Self {
        match e {
            ss::TermComputation::Annotation(Annotation {
                term: body,
                ty: _,
            }) => body.inner_ref().into(),
            ss::TermComputation::Ret(Ret(v)) => {
                Ret(rc!(v.inner_ref().into())).into()
            }
            ss::TermComputation::Force(Force(v)) => {
                Force(rc!(v.inner_ref().into())).into()
            }
            ss::TermComputation::Let(Let { var, def, body }) => {
                let (def, body) =
                    rc!(def.inner_ref().into(), body.inner_ref().into());
                Let { var: var.clone(), def, body }.into()
            }
            ss::TermComputation::Do(Do { var, comp, body }) => {
                let (comp, body) =
                    rc!(comp.inner_ref().into(), body.inner_ref().into());
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
                    .map(|Matcher { ctor, vars, body }| {
                        let body = rc!(body.inner_ref().into());
                        Matcher { ctor: ctor.clone(), vars: vars.clone(), body }
                    })
                    .collect();
                Match { scrut, arms }.into()
            }
            ss::TermComputation::Comatch(Comatch { arms }) => {
                let arms = arms
                    .iter()
                    .map(|Comatcher { dtor, vars, body }| {
                        let body = rc!(body.inner_ref().into());
                        Comatcher {
                            dtor: dtor.clone(),
                            vars: vars.clone(),
                            body,
                        }
                    })
                    .collect();
                Comatch { arms }.into()
            }
            ss::TermComputation::Dtor(Dtor { body, dtor, args }) => {
                let body = rc!(body.inner_ref().into());
                let args = args
                    .iter()
                    .map(|arg| rc!(arg.inner_ref().into()))
                    .collect();
                Dtor { body, dtor: dtor.clone(), args }.into()
            }
            ss::TermComputation::TypAbs(TypAbs { tvar: _, kd: _, body }) => {
                body.inner_ref().into()
            }
            ss::TermComputation::TypApp(TypApp { body, arg: _ }) => {
                body.inner_ref().into()
            }
            ss::TermComputation::MatchPack(MatchPack {
                scrut,
                tvar: _,
                var,
                body,
            }) => {
                let scrut = rc!(scrut.inner_ref().into());
                let body = rc!(body.inner_ref().into());
                Let { var: var.clone(), def: scrut, body }.into()
            }
        }
    }
}
