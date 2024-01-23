use crate::library::syntax::*;
use std::{rc::Rc, vec};
use zydeco_utils::span::Span;

fn transform_comp(comp: &SynComp) -> SynComp {
    let arg = DtorV::new("arg".into(), Span::dummy());
    let call = DtorV::new("$call".into(), Span::dummy());
    let cont = TermV::new("$cont".into(), Span::dummy());
    match comp {
        SynComp::Do(Do { var, comp, body }) => Dtor {
            body: Rc::new(transform_comp(comp)),
            dtorv: call.clone(),
            args: vec![Rc::new(
                Thunk(Rc::new(
                    Comatch {
                        arms: vec![Comatcher {
                            dtorv: arg.clone(),
                            vars: vec![var.clone()],
                            body: Rc::new(transform_comp(body)),
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
                        args: vec![Rc::new(transform_val(val))],
                    }
                    .into(),
                ),
            }],
        }
        .into(),
        SynComp::Force(Force(val)) => Force(Rc::new(transform_val(val))).into(),
        SynComp::Let(Let { var, def, body }) => Let {
            var: var.clone(),
            def: Rc::new(transform_val(def)),
            body: Rc::new(transform_comp(body)),
        }
        .into(),
        SynComp::Rec(Rec { var, body }) => {
            Rec { var: var.clone(), body: Rc::new(transform_comp(body)) }.into()
        }
        SynComp::Match(Match { scrut, arms }) => Match {
            scrut: Rc::new(transform_val(scrut)),
            arms: arms
                .iter()
                .map(|Matcher { ctorv, vars, body }| Matcher {
                    ctorv: ctorv.clone(),
                    vars: vars.clone(),
                    body: Rc::new(transform_comp(body)),
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
                    body: Rc::new(transform_comp(body)),
                })
                .collect(),
        }
        .into(),
        SynComp::Dtor(Dtor { body, dtorv, args }) => Dtor {
            body: Rc::new(transform_comp(body)),
            dtorv: dtorv.clone(),
            args: args.iter().map(|arg| Rc::new(transform_val(arg))).collect(),
        }
        .into(),
        SynComp::Prim(Prim { arity, body }) => Prim { arity: *arity, body: body.clone() }.into(),
    }
}

fn transform_val(val: &SynVal) -> SynVal {
    match val {
        SynVal::SemValue(_) => unreachable!(),
        SynVal::Ctor(Ctor { ctorv, args }) => Ctor {
            ctorv: ctorv.clone(),
            args: args.iter().map(|arg| Rc::new(transform_val(arg))).collect(),
        }
        .into(),
        SynVal::Thunk(t) => Thunk(Rc::new(transform_comp(&t.0))).into(),
        _ => val.clone(),
    }
}

fn transform_module(Module { name, define }: &Module) -> Module {
    Module {
        name: name.clone(),
        define: define.iter().map(|(var, val)| (var.clone(), transform_val(val))).collect(),
    }
}

pub fn transform_program(Program { module, entry }: &Program) -> Program {
    Program { module: transform_module(module), entry: transform_comp(entry) }
}
