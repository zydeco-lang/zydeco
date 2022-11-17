use super::env::Env;
use crate::{
    parse::syntax::{Compute, Ctor, Declare, Dtor, Program, VVar, Value},
    utils::ann::AnnT,
};
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct ZProgram<Ann: AnnT> {
    pub decls: Vec<Declare<Ann>>,
    pub comp: ZCompute<Ann>,
    pub ann: Ann,
}

impl<Ann: AnnT> From<Program<Ann>> for ZProgram<Ann> {
    fn from(Program { decls, comp, ann }: Program<Ann>) -> Self {
        ZProgram { decls, comp: (*comp).into(), ann }
    }
}

#[derive(Clone, Debug)]
pub enum ZValue<Ann: AnnT> {
    Var(VVar<Ann>, Ann),
    Thunk(Rc<ZCompute<Ann>>, Option<Env<Ann>>, Ann),
    Ctor(Ctor<Ann>, Vec<Rc<ZValue<Ann>>>, Ann),
    Bool(bool, Ann),
    Int(i64, Ann),
    String(String, Ann),
    Char(char, Ann),
    Triv(Ann),
}

impl<Ann: AnnT> From<Value<Ann>> for ZValue<Ann> {
    fn from(value: Value<Ann>) -> Self {
        match value {
            Value::Var(var, ann) => ZValue::Var(var, ann),
            Value::Thunk(compute, ann) => {
                ZValue::Thunk(Rc::new((*compute).into()), None, ann)
            }
            Value::Ctor(ctor, args, ann) => ZValue::Ctor(
                ctor,
                args.into_iter().map(Into::into).map(Rc::new).collect(),
                ann,
            ),
            Value::Bool(b, ann) => ZValue::Bool(b, ann),
            Value::Int(i, ann) => ZValue::Int(i, ann),
            Value::String(s, ann) => ZValue::String(s, ann),
            Value::Char(s, ann) => ZValue::Char(s, ann),
            Value::Triv(ann) => ZValue::Triv(ann),
        }
    }
}

type Binding<Def, Ann> = (VVar<Ann>, Rc<Def>);

#[derive(Clone, Debug)]
pub enum ZCompute<Ann: AnnT> {
    Let {
        binding: Binding<ZValue<Ann>, Ann>,
        body: Rc<ZCompute<Ann>>,
        ann: Ann,
    },
    Do {
        binding: Binding<ZCompute<Ann>, Ann>,
        body: Rc<ZCompute<Ann>>,
        ann: Ann,
    },
    Force(Rc<ZValue<Ann>>, Ann),
    Return(Rc<ZValue<Ann>>, Ann),
    Lam {
        arg: VVar<Ann>,
        body: Rc<ZCompute<Ann>>,
        ann: Ann,
    },
    Prim {
        arity: u64,
        body: fn(Vec<ZValue<Ann>>) -> ZValue<Ann>,
        ann: Ann,
    },
    Rec {
        arg: VVar<Ann>,
        body: Rc<ZCompute<Ann>>,
        ann: Ann,
    },
    App(Rc<ZCompute<Ann>>, Rc<ZValue<Ann>>, Ann),
    If {
        cond: Rc<ZValue<Ann>>,
        thn: Rc<ZCompute<Ann>>,
        els: Rc<ZCompute<Ann>>,
        ann: Ann,
    },
    Match {
        scrut: Rc<ZValue<Ann>>,
        cases: Vec<(Ctor<Ann>, Vec<VVar<Ann>>, Rc<ZCompute<Ann>>)>,
        ann: Ann,
    },
    CoMatch {
        cases: Vec<(Dtor<Ann>, Vec<VVar<Ann>>, Rc<ZCompute<Ann>>)>,
        ann: Ann,
    },
    CoApp {
        scrut: Rc<ZCompute<Ann>>,
        dtor: Dtor<Ann>,
        args: Vec<Rc<ZValue<Ann>>>,
        ann: Ann,
    },
}

impl<Ann: AnnT> From<Compute<Ann>> for ZCompute<Ann> {
    fn from(compute: Compute<Ann>) -> Self {
        match compute {
            Compute::Let { binding: (name, _, def), body, ann } => {
                ZCompute::Let {
                    binding: (name, Rc::new((*def).into())),
                    body: Rc::new((*body).into()),
                    ann,
                }
            }
            Compute::Do { binding: (name, _, def), body, ann } => {
                ZCompute::Do {
                    binding: (name, Rc::new((*def).into())),
                    body: Rc::new((*body).into()),
                    ann,
                }
            }
            Compute::Force(value, ann) => {
                ZCompute::Force(Rc::new((*value).into()), ann)
            }
            Compute::Return(value, ann) => {
                ZCompute::Return(Rc::new((*value).into()), ann)
            }
            Compute::Lam { arg: (arg, _), body, ann } => {
                ZCompute::Lam { arg, body: Rc::new((*body).into()), ann }
            }
            Compute::Rec { arg: (arg, _), body, ann } => {
                ZCompute::Rec { arg, body: Rc::new((*body).into()), ann }
            }
            Compute::App(f, arg, ann) => {
                ZCompute::App(Rc::new((*f).into()), Rc::new((*arg).into()), ann)
            }
            Compute::If { cond, thn, els, ann } => ZCompute::If {
                cond: Rc::new((*cond).into()),
                thn: Rc::new((*thn).into()),
                els: Rc::new((*els).into()),
                ann,
            },
            Compute::Match { scrut, cases, ann } => ZCompute::Match {
                scrut: Rc::new((*scrut).into()),
                cases: cases
                    .into_iter()
                    .map(|(ctor, vars, body)| {
                        (ctor, vars, Rc::new((*body).into()))
                    })
                    .collect(),
                ann,
            },
            Compute::CoMatch { cases, ann } => ZCompute::CoMatch {
                cases: cases
                    .into_iter()
                    .map(|(dtor, vars, body)| {
                        (dtor, vars, Rc::new((*body).into()))
                    })
                    .collect(),
                ann,
            },
            Compute::CoApp { body: scrut, dtor, args, ann } => {
                ZCompute::CoApp {
                    scrut: Rc::new((*scrut).into()),
                    dtor,
                    args: args
                        .into_iter()
                        .map(Into::into)
                        .map(Rc::new)
                        .collect(),
                    ann,
                }
            }
        }
    }
}
