use crate::{syntax::DynamicsArena, *};
use builtin::BUILTINS;
use std::rc::Rc;
use zydeco_statics::{surface_syntax::ScopedArena, syntax::StaticsArena};
use zydeco_syntax::*;
use zydeco_utils::arena::{ArenaAccess, ArenaSparse};

pub trait Link {
    type Arena<'a>;
    type Out;
    fn link(&self, arena: Self::Arena<'_>) -> Self::Out;
}

pub struct Linker {
    pub scoped: ScopedArena,
    pub statics: StaticsArena,
}

impl Linker {
    pub fn run(self) -> DynamicsArena {
        let Linker { scoped, statics } = self;
        let defs = scoped.defs;
        let mut top = scoped.top;
        let mut invalid = Vec::new();
        let decls = scoped.decls.filter_map_id_mut(|id| match id.link((&statics, &defs)) {
            | Some(decl) => Some(decl),
            | None => {
                invalid.push(id);
                None
            }
        });
        top.release(invalid);
        DynamicsArena { defs, decls, top }
    }
}

impl Link for ss::DeclId {
    type Arena<'a> = (&'a StaticsArena, &'a ArenaSparse<ds::DefId, VarName>);
    type Out = Option<ds::Declaration>;

    fn link(&self, (statics, defs): Self::Arena<'_>) -> Self::Out {
        let decl = statics.decls.get(self)?;
        use ss::Declaration as Decl;
        let decl = match decl {
            | Decl::TAliasBody(_) => None?,
            | Decl::VAliasBody(decl) => {
                let ss::VAliasBody { binder, bindee } = decl;
                let binder = binder.link(statics);
                let bindee = bindee.link(statics);
                ds::VAliasBody { binder, bindee }.into()
            }
            | Decl::VAliasHead(decl) => {
                let ss::VAliasHead { binder, ty: _ } = decl;
                let binder = binder.link(statics);
                use ds::ValuePattern as VPat;
                let def = match binder.as_ref() {
                    | VPat::Var(def) => def,
                    | VPat::Hole(_) | VPat::Ctor(_) | VPat::Triv(_) | VPat::VCons(_) => {
                        unreachable!()
                    }
                };
                let name = defs[def].as_str();
                // println!("builtin: {}", name);
                let bindee = {
                    let prim = BUILTINS[name].to_owned().into();
                    let thunk = Thunk(Rc::new(prim)).into();
                    Rc::new(thunk)
                };
                ds::VAliasBody { binder, bindee }.into()
            }
            | Decl::Exec(exec) => {
                let ss::Exec(comp) = exec;
                let comp = comp.link(statics);
                ds::Exec(comp).into()
            }
        };
        Some(decl)
    }
}

impl Link for ss::VPatId {
    type Arena<'a> = &'a StaticsArena;
    type Out = ds::RcVPat;

    fn link(&self, statics: Self::Arena<'_>) -> Self::Out {
        let vpat = &statics.vpats[self];
        use ss::ValuePattern as VPat;
        let vpat = match vpat {
            | VPat::Hole(_) => Hole.into(),
            | VPat::Var(def) => (*def).into(),
            | VPat::Ctor(Ctor(ctor, pat)) => {
                let ctor = ctor.to_owned();
                let pat = pat.link(statics);
                Ctor(ctor, pat).into()
            }
            | VPat::Triv(Triv) => Triv.into(),
            | VPat::VCons(Cons(a, b)) => {
                let a = a.link(statics);
                let b = b.link(statics);
                Cons(a, b).into()
            }
            | VPat::TCons(Cons(_, pat)) => {
                let pat = pat.link(statics);
                pat.as_ref().to_owned()
            }
        };
        Rc::new(vpat)
    }
}

impl Link for ss::ValueId {
    type Arena<'a> = &'a StaticsArena;
    type Out = ds::RcValue;

    fn link(&self, statics: Self::Arena<'_>) -> Self::Out {
        let value = &statics.values[self];
        use ss::Value;
        let value = match value {
            | Value::Hole(Hole) => Hole.into(),
            | Value::Var(def) => (*def).into(),
            | Value::Thunk(Thunk(body)) => {
                let body = body.link(statics);
                Thunk(body).into()
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                let ctor = ctor.to_owned();
                let body = body.link(statics);
                Ctor(ctor, body).into()
            }
            | Value::Triv(Triv) => Triv.into(),
            | Value::VCons(Cons(a, b)) => {
                let a = a.link(statics);
                let b = b.link(statics);
                Cons(a, b).into()
            }
            | Value::TCons(Cons(_, body)) => {
                let body = body.link(statics);
                body.as_ref().to_owned()
            }
            | Value::Lit(lit) => lit.to_owned().into(),
        };
        Rc::new(value)
    }
}

impl Link for ss::CompuId {
    type Arena<'a> = &'a StaticsArena;
    type Out = ds::RcCompu;

    fn link(&self, statics: Self::Arena<'_>) -> Self::Out {
        let compu = &statics.compus[self];
        use ss::Computation as Compu;
        let compu = match compu {
            | Compu::Hole(Hole) => Hole.into(),
            | Compu::VAbs(Abs(param, body)) => {
                let param = param.link(statics);
                let body = body.link(statics);
                Abs(param, body).into()
            }
            | Compu::VApp(App(body, arg)) => {
                let body = body.link(statics);
                let arg = arg.link(statics);
                App(body, arg).into()
            }
            | Compu::TAbs(Abs(_, body)) => {
                let body = body.link(statics);
                body.as_ref().to_owned()
            }
            | Compu::TApp(App(body, _)) => {
                let body = body.link(statics);
                body.as_ref().to_owned()
            }
            | Compu::Fix(Fix(param, body)) => {
                let param = param.link(statics);
                let body = body.link(statics);
                Fix(param, body).into()
            }
            | Compu::Force(Force(body)) => {
                let body = body.link(statics);
                Force(body).into()
            }
            | Compu::Ret(Return(body)) => {
                let body = body.link(statics);
                Return(body).into()
            }
            | Compu::Do(Bind { binder, bindee, tail }) => {
                let binder = binder.link(statics);
                let bindee = bindee.link(statics);
                let tail = tail.link(statics);
                Bind { binder, bindee, tail }.into()
            }
            | Compu::Let(Let { binder, bindee, tail }) => {
                let binder = binder.link(statics);
                let bindee = bindee.link(statics);
                let tail = tail.link(statics);
                Let { binder, bindee, tail }.into()
            }
            | Compu::Match(Match { scrut, arms }) => {
                let scrut = scrut.link(statics);
                let arms = arms
                    .iter()
                    .map(|Matcher { binder, tail }| {
                        let binder = binder.link(statics);
                        let tail = tail.link(statics);
                        Matcher { binder, tail }
                    })
                    .collect();
                Match { scrut, arms }.into()
            }
            | Compu::CoMatch(CoMatch { arms }) => {
                let arms = arms
                    .iter()
                    .map(|CoMatcher { dtor, tail }| {
                        let dtor = dtor.to_owned();
                        let tail = tail.link(statics);
                        CoMatcher { dtor, tail }
                    })
                    .collect();
                CoMatch { arms }.into()
            }
            | Compu::Dtor(Dtor(body, dtor)) => {
                let body = body.link(statics);
                let dtor = dtor.to_owned();
                Dtor(body, dtor).into()
            }
        };
        Rc::new(compu)
    }
}
