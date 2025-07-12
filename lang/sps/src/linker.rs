use crate::statics_syntax as ss;
use crate::syntax::*;
use std::rc::Rc;
use zydeco_statics::{surface_syntax::ScopedArena, syntax::StaticsArena};

pub trait Link {
    type Arena<'a>;
    type Out;
    fn link(&self, arena: Self::Arena<'_>) -> Self::Out;
}

pub struct Linker {
    pub scoped: ScopedArena,
    pub statics: StaticsArena,
}

impl Link for ss::DeclId {
    type Arena<'a> = (&'a mut ScopedArena, &'a StaticsArena);
    type Out = Option<Declaration>;

    fn link(&self, (scoped, statics): Self::Arena<'_>) -> Self::Out {
        let decl = statics.decls[self].clone();
        use ss::Declaration as Decl;
        let decl = match decl {
            | Decl::TAliasBody(_) => None?,
            | Decl::VAliasHead(_) => None?,
            | Decl::VAliasBody(decl) => {
                let ss::VAliasBody { binder, bindee } = decl;
                let binder = binder.link(statics);
                let bindee = bindee.link((scoped, statics));
                VAliasBody { binder, bindee }.into()
            }
            | Decl::Exec(decl) => {
                let ss::Exec(body) = decl;
                let zvar = scoped.defs.alloc(VarName("z".to_owned()));
                let stack = Rc::new(zvar.into());
                let body = body.link((stack, scoped, statics));
                let stack = Rc::new(zvar.into());
                Exec { stack, body }.into()
            }
        };
        Some(decl)
    }
}

impl Link for ss::VPatId {
    type Arena<'a> = &'a StaticsArena;
    type Out = RcVPat;

    fn link(&self, statics: Self::Arena<'_>) -> Self::Out {
        let vpat = statics.vpats[self].clone();
        use ss::ValuePattern as VPat;
        let vpat = match vpat {
            | VPat::Hole(Hole) => Hole.into(),
            | VPat::Var(def) => def.into(),
            | VPat::Ctor(vpat) => {
                let Ctor(ctor, arg) = vpat;
                let arg = arg.link(statics);
                Ctor(ctor, arg).into()
            }
            | VPat::Triv(Triv) => Triv.into(),
            | VPat::VCons(vpat) => {
                let Cons(head, tail) = vpat;
                let head = head.link(statics);
                let tail = tail.link(statics);
                Cons(head, tail).into()
            }
            | VPat::TCons(vpat) => {
                let Cons(_, tail) = vpat;
                let tail = tail.link(statics);
                return tail;
            }
        };
        Rc::new(vpat)
    }
}

impl Link for ss::ValueId {
    type Arena<'a> = (&'a mut ScopedArena, &'a StaticsArena);
    type Out = RcValue;

    fn link(&self, (scoped, statics): Self::Arena<'_>) -> Self::Out {
        let value = statics.values[self].clone();
        use ss::Value;
        let value = match value {
            | Value::Hole(Hole) => Hole.into(),
            | Value::Var(def) => def.into(),
            | Value::Thunk(value) => {
                let Thunk(compu) = value;
                let zvar = scoped.defs.alloc(VarName("z".to_owned()));
                let stack = Rc::new(zvar.into());
                let body = compu.link((stack, scoped, statics));
                let stack = Rc::new(zvar.into());
                Proc { stack, body }.into()
            }
            | Value::Ctor(value) => {
                let Ctor(ctor, arg) = value;
                let arg = arg.link((scoped, statics));
                Ctor(ctor, arg).into()
            }
            | Value::Triv(Triv) => Triv.into(),
            | Value::VCons(value) => {
                let Cons(head, tail) = value;
                let head = head.link((scoped, statics));
                let tail = tail.link((scoped, statics));
                Cons(head, tail).into()
            }
            | Value::TCons(value) => {
                let Cons(_, tail) = value;
                let tail = tail.link((scoped, statics));
                return tail;
            }
            | Value::Lit(literal) => literal.into(),
        };
        Rc::new(value)
    }
}

impl Link for ss::CompuId {
    type Arena<'a> = (RcStack, &'a mut ScopedArena, &'a StaticsArena);
    type Out = RcCompu;

    fn link(&self, (stack, scoped, statics): Self::Arena<'_>) -> Self::Out {
        let compu = statics.compus[self].clone();
        use ss::Computation as Compu;
        let compu = match compu {
            | Compu::Hole(Hole) => Hole.into(),
            | Compu::VAbs(compu) => {
                let Abs(binder, body) = compu;
                let binder = binder.link(statics);
                let bindee = stack;
                let zvar = scoped.defs.alloc(VarName("z".to_owned()));
                let stack = Rc::new(zvar.into());
                let body = body.link((stack, scoped, statics));
                let stack = Rc::new(zvar.into());
                LetAbs { binder, stack, bindee, body }.into()
            }
            | Compu::VApp(compu) => {
                let App(body, arg) = compu;
                let arg = arg.link((scoped, statics));
                let stack = Rc::new(Stack::Arg(arg, stack));
                return body.link((stack, scoped, statics));
            }
            | Compu::TAbs(compu) => {
                let Abs(_, body) = compu;
                return body.link((stack, scoped, statics));
            }
            | Compu::TApp(compu) => {
                let App(body, _) = compu;
                return body.link((stack, scoped, statics));
            }
            | Compu::Fix(_compu) => todo!(),
            | Compu::Force(compu) => {
                let Force(body) = compu;
                let thunk = body.link((scoped, statics));
                Call { thunk, stack }.into()
            }
            | Compu::Ret(compu) => {
                let Return(value) = compu;
                let value = value.link((scoped, statics));
                ReturnKont { stack, value }.into()
            }
            | Compu::Do(compu) => {
                let Bind { binder, bindee, tail } = compu;
                let binder = binder.link(statics);
                let body = tail.link((stack, scoped, statics));
                let stack = Rc::new(Kont { binder, body }.into());
                return bindee.link((stack, scoped, statics));
            }
            | Compu::Let(compu) => {
                let Let { binder, bindee, tail } = compu;
                let binder = binder.link(statics);
                let body = tail.link((stack, scoped, statics));
                let stack = Rc::new(Kont { binder, body }.into());
                let value = bindee.link((scoped, statics));
                ReturnKont { stack, value }.into()
            }
            | Compu::Match(compu) => {
                let Match { scrut, arms } = compu;
                let scrut = scrut.link((scoped, statics));
                let arms = arms
                    .iter()
                    .map(|Matcher { binder, tail }| {
                        let binder = binder.link(statics);
                        let tail = tail.link((stack.clone(), scoped, statics));
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
