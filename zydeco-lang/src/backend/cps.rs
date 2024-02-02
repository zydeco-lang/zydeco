use crate::library::syntax::*;
use std::{rc::Rc, vec};
use zydeco_utils::span::Span;

pub trait CpsTransform {
    fn cps_transform(&self) -> Self;
}

impl CpsTransform for SynComp {
    fn cps_transform(&self) -> Self {
        let arg = DtorV::new("arg".into(), Span::dummy());
        let call = DtorV::new("$call".into(), Span::dummy());
        let cont = TermV::new("$cont".into(), Span::dummy());
        match self {
            SynComp::Abs(Abs { param, body }) => {
                Abs { param: param.clone(), body: Rc::new(body.cps_transform()) }.into()
            }
            SynComp::App(App { body, arg }) => {
                App { body: Rc::new(body.cps_transform()), arg: Rc::new(arg.cps_transform()) }
                    .into()
            }
            SynComp::Do(Do { var, comp, body }) => Dtor {
                body: Rc::new(comp.cps_transform()),
                dtorv: call.clone(),
                args: vec![Rc::new(
                    Thunk(Rc::new(
                        Comatch {
                            arms: vec![Comatcher {
                                dtorv: arg.clone(),
                                vars: vec![var.clone()],
                                body: Rc::new(body.cps_transform()),
                            }],
                        }
                        .into(),
                    ))
                    .into(),
                )],
            }
            .into(),
            SynComp::Ret(Ret(val)) => Comatch {
                arms: vec![Comatcher {
                    dtorv: call.clone(),
                    vars: vec![cont.clone()],
                    body: Rc::new(
                        Dtor {
                            body: Rc::new(Force(Rc::new(cont.clone().into())).into()),
                            dtorv: arg.clone(),
                            args: vec![Rc::new(val.cps_transform())],
                        }
                        .into(),
                    ),
                }],
            }
            .into(),
            SynComp::Force(Force(val)) => Force(Rc::new(val.cps_transform())).into(),
            SynComp::Let(Let { var, def, body }) => Let {
                var: var.clone(),
                def: Rc::new(def.cps_transform()),
                body: Rc::new(body.cps_transform()),
            }
            .into(),
            SynComp::Rec(Rec { var, body }) => {
                Rec { var: var.clone(), body: Rc::new(body.cps_transform()) }.into()
            }
            SynComp::Match(Match { scrut, arms }) => Match {
                scrut: Rc::new(scrut.cps_transform()),
                arms: arms
                    .iter()
                    .map(|Matcher { ctorv, vars, body }| Matcher {
                        ctorv: ctorv.clone(),
                        vars: vars.clone(),
                        body: Rc::new(body.cps_transform()),
                    })
                    .collect(),
            }
            .into(),
            SynComp::Comatch(Comatch { arms }) => Comatch {
                arms: arms
                    .iter()
                    .map(|Comatcher { dtorv, vars, body }| Comatcher {
                        dtorv: dtorv.clone(),
                        vars: vars.clone(),
                        body: Rc::new(body.cps_transform()),
                    })
                    .collect(),
            }
            .into(),
            SynComp::Dtor(Dtor { body, dtorv, args }) => Dtor {
                body: Rc::new(body.cps_transform()),
                dtorv: dtorv.clone(),
                args: args.iter().map(|arg| Rc::new(arg.cps_transform())).collect(),
            }
            .into(),
            SynComp::Prim(Prim { arity, body }) => {
                Prim { arity: *arity, body: body.clone() }.into()
            }
        }
    }
}

impl CpsTransform for SynVal {
    fn cps_transform(&self) -> Self {
        match self {
            SynVal::SemValue(_) => unreachable!(),
            SynVal::Ctor(Ctor { ctorv, args }) => Ctor {
                ctorv: ctorv.clone(),
                args: args.iter().map(|arg| Rc::new(arg.cps_transform())).collect(),
            }
            .into(),
            SynVal::Thunk(Thunk(comp)) => Thunk(Rc::new(comp.cps_transform())).into(),
            _ => self.clone(),
        }
    }
}

impl CpsTransform for Module {
    fn cps_transform(&self) -> Self {
        Module {
            name: self.name.clone(),
            define: self
                .define
                .iter()
                .map(|(var, val)| (var.clone(), val.cps_transform()))
                .collect(),
        }
    }
}

impl CpsTransform for Program {
    fn cps_transform(&self) -> Self {
        Program { module: self.module.cps_transform(), entry: self.entry.cps_transform() }
    }
}
