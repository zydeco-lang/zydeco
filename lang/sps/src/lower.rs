use crate::statics_syntax as ss;
use crate::syntax::*;
use std::rc::Rc;
use zydeco_statics::{surface_syntax::ScopedArena, tyck::syntax::StaticsArena};

pub trait Lower {
    type Arena<'a>;
    type Out;
    fn lower(&self, arena: Self::Arena<'_>) -> Self::Out;
}

pub struct Lowerer {
    pub scoped: ScopedArena,
    pub statics: StaticsArena,
}

impl Lower for ss::DeclId {
    type Arena<'a> = &'a StaticsArena;
    type Out = Option<Declaration>;

    fn lower(&self, statics: Self::Arena<'_>) -> Self::Out {
        let decl = statics.decls[self].clone();
        use ss::Declaration as Decl;
        let decl = match decl {
            | Decl::TAliasBody(_) => None?,
            | Decl::VAliasHead(_) => None?,
            | Decl::VAliasBody(decl) => {
                let ss::VAliasBody { binder, bindee } = decl;
                let binder = binder.lower(statics);
                let bindee = bindee.lower(statics);
                VAliasBody { binder, bindee }.into()
            }
            | Decl::Exec(decl) => {
                let ss::Exec(body) = decl;
                let stack = Rc::new(Current.into());
                let body = body.lower((stack, statics));
                Exec { body }.into()
            }
        };
        Some(decl)
    }
}

impl Lower for ss::VPatId {
    type Arena<'a> = &'a StaticsArena;
    type Out = RcVPat;

    fn lower(&self, statics: Self::Arena<'_>) -> Self::Out {
        let vpat = statics.vpats[self].clone();
        use ss::ValuePattern as VPat;
        let vpat = match vpat {
            | VPat::Hole(Hole) => Hole.into(),
            | VPat::Var(def) => def.into(),
            | VPat::Ctor(vpat) => {
                let Ctor(ctor, arg) = vpat;
                let arg = arg.lower(statics);
                Ctor(ctor, arg).into()
            }
            | VPat::Triv(Triv) => Triv.into(),
            | VPat::VCons(vpat) => {
                let Cons(head, tail) = vpat;
                let head = head.lower(statics);
                let tail = tail.lower(statics);
                Cons(head, tail).into()
            }
            | VPat::TCons(vpat) => {
                let Cons(_, tail) = vpat;
                let tail = tail.lower(statics);
                return tail;
            }
        };
        Rc::new(vpat)
    }
}

impl Lower for ss::ValueId {
    type Arena<'a> = &'a StaticsArena;
    type Out = RcValue;

    fn lower(&self, statics: Self::Arena<'_>) -> Self::Out {
        let value = statics.values[self].clone();
        use ss::Value;
        let value = match value {
            | Value::Hole(Hole) => Hole.into(),
            | Value::Var(def) => def.into(),
            | Value::Thunk(value) => {
                let Thunk(compu) = value;
                let stack = Rc::new(Current.into());
                let body = compu.lower((stack, statics));
                Thunk(body).into()
            }
            | Value::Ctor(value) => {
                let Ctor(ctor, arg) = value;
                let arg = arg.lower(statics);
                Ctor(ctor, arg).into()
            }
            | Value::Triv(Triv) => Triv.into(),
            | Value::VCons(value) => {
                let Cons(head, tail) = value;
                let head = head.lower(statics);
                let tail = tail.lower(statics);
                Cons(head, tail).into()
            }
            | Value::TCons(value) => {
                let Cons(_, tail) = value;
                let tail = tail.lower(statics);
                return tail;
            }
            | Value::Lit(literal) => literal.into(),
        };
        Rc::new(value)
    }
}

impl Lower for ss::CompuId {
    type Arena<'a> = (RcStack, &'a StaticsArena);
    type Out = RcCompu;

    fn lower(&self, (stack, statics): Self::Arena<'_>) -> Self::Out {
        let compu = statics.compus[self].clone();
        use ss::Computation as Compu;
        let compu = match compu {
            | Compu::Hole(Hole) => Hole.into(),
            | Compu::VAbs(compu) => {
                let Abs(binder, body) = compu;
                let binder = binder.lower(statics);
                let bindee = stack;
                let stack = Rc::new(Current.into());
                let body = body.lower((stack, statics));
                LetAbs { binder, bindee, body }.into()
            }
            | Compu::VApp(compu) => {
                let App(body, arg) = compu;
                let arg = arg.lower(statics);
                let stack = Rc::new(Stack::Arg(StackItem { item: arg, next: stack }));
                return body.lower((stack, statics));
            }
            | Compu::TAbs(compu) => {
                let Abs(_, body) = compu;
                return body.lower((stack, statics));
            }
            | Compu::TApp(compu) => {
                let App(body, _) = compu;
                return body.lower((stack, statics));
            }
            | Compu::Fix(_compu) => todo!(),
            | Compu::Force(compu) => {
                let Force(body) = compu;
                let thunk = body.lower(statics);
                Call { thunk, stack }.into()
            }
            | Compu::Ret(compu) => {
                let Return(value) = compu;
                let value = value.lower(statics);
                ReturnKont { stack, value }.into()
            }
            | Compu::Do(compu) => {
                let Bind { binder, bindee, tail } = compu;
                let binder = binder.lower(statics);
                let body = tail.lower((stack, statics));
                let stack = Rc::new(Kont { binder, body }.into());
                return bindee.lower((stack, statics));
            }
            | Compu::Let(compu) => {
                let Let { binder, bindee, tail } = compu;
                let binder = binder.lower(statics);
                let body = tail.lower((stack, statics));
                let stack = Rc::new(Kont { binder, body }.into());
                let value = bindee.lower(statics);
                ReturnKont { stack, value }.into()
            }
            | Compu::Match(compu) => {
                let Match { scrut, arms } = compu;
                let scrut = scrut.lower(statics);
                let arms = arms
                    .iter()
                    .map(|Matcher { binder, tail }| {
                        let binder = binder.lower(statics);
                        let tail = tail.lower((stack.clone(), statics));
                        Matcher { binder, tail }
                    })
                    .collect();
                Match { scrut, arms }.into()
            }
            | Compu::CoMatch(_) => todo!(),
            | Compu::Dtor(_) => todo!(),
        };
        Rc::new(compu)
    }
}
