use crate::{builtin::BuiltinRuntime, syntax::DynamicsProgram, *};
use std::{collections::HashSet, rc::Rc, sync::Arc};
use thiserror::Error;
use zydeco_statics::{
    BuiltinPackagePlan, BuiltinPackagePlanError, BuiltinPackageValue, arena::StaticsArena,
    surface_syntax::ScopedArena,
};
use zydeco_syntax::*;

/// Trait for translating statics syntax nodes into dynamic syntax nodes.
pub trait Link {
    type Arena<'a>;
    type Out;
    fn link(&self, arena: Self::Arena<'_>) -> Self::Out;
}

/// Entry point for linking one checked computation root.
pub struct RootLinker {
    pub scoped: ScopedArena,
    pub statics: Arc<StaticsArena>,
    pub root: ss::CompuId,
}

/// Link one checked value as a computation that returns it.
pub struct ValueRootLinker {
    pub scoped: ScopedArena,
    pub statics: Arc<StaticsArena>,
    pub root: ss::ValueId,
}

/// Link a package-dependent root and apply the concrete host Builtin package.
pub struct BuiltinRootLinker {
    pub scoped: ScopedArena,
    pub statics: Arc<StaticsArena>,
    pub root: ss::CompuId,
    pub signature: ss::PackPi,
}

/// Apply host Builtin packages until a package-dependent computation reaches its result.
pub struct BuiltinComputationRootLinker {
    pub scoped: ScopedArena,
    pub statics: Arc<StaticsArena>,
    pub root: ss::CompuId,
    pub signature: ss::PackPi,
}

/// Apply host Builtin packages until a pure package-dependent value reaches its result.
pub struct BuiltinValueRootLinker {
    pub scoped: ScopedArena,
    pub statics: Arc<StaticsArena>,
    pub root: ss::ValueId,
    pub signature: ss::ValuePackPi,
}

#[derive(Clone, Debug, Error)]
pub enum BuiltinPackageError {
    #[error(transparent)]
    Plan(#[from] BuiltinPackagePlanError),
    #[error("host package contracts form a recursive result at type {ty:?}")]
    RecursiveContract { ty: ss::TypeId },
}

/// Materialize the operation values described by a checked Builtin package plan.
struct BuiltinPackageLinker;

impl BuiltinPackageLinker {
    fn link(value: BuiltinPackageValue) -> Result<ds::RcValue, BuiltinPackageError> {
        match value {
            | BuiltinPackageValue::Unit => Ok(Rc::new(ds::Value::Triv(Triv))),
            | BuiltinPackageValue::Operation(role) => Ok(BuiltinRuntime::package_value(role)),
            | BuiltinPackageValue::Product(product) => {
                let values = product
                    .into_values()
                    .into_iter()
                    .map(Self::link)
                    .collect::<Result<Vec<_>, _>>()?;
                let values = ConsN::from_vec(values).expect("a checked product plan is non-empty");
                Ok(Rc::new(ds::Value::VCons(values)))
            }
        }
    }

    fn computation_signature(statics: &StaticsArena, ty: ss::TypeId) -> Option<ss::PackPi> {
        match Self::type_view(statics, ty) {
            | Some(ss::Type::PackPi(signature)) => Some(signature.clone()),
            | _ => None,
        }
    }

    fn value_signature(statics: &StaticsArena, ty: ss::TypeId) -> Option<ss::ValuePackPi> {
        match Self::type_view(statics, ty) {
            | Some(ss::Type::VPackPi(signature)) => Some(signature.clone()),
            | _ => None,
        }
    }

    fn type_view(statics: &StaticsArena, ty: ss::TypeId) -> Option<&ss::Type> {
        statics.normalized_at(ty)
    }
}

impl RootLinker {
    /// Erase static structure and retain one computation as the dynamic root.
    pub fn run(self) -> DynamicsProgram {
        let Self { scoped, statics, root } = self;
        let defs = scoped.defs.rebind::<ds::DynamicsScope>();
        let root = root.link(&statics);
        DynamicsProgram { defs, root }
    }
}

impl ValueRootLinker {
    pub fn run(self) -> DynamicsProgram {
        let Self { scoped, statics, root } = self;
        let defs = scoped.defs.rebind::<ds::DynamicsScope>();
        let value = root.link(&statics);
        let root = Rc::new(ds::Computation::Ret(Return(value)));
        DynamicsProgram { defs, root }
    }
}

impl BuiltinRootLinker {
    pub fn run(self) -> Result<DynamicsProgram, BuiltinPackageError> {
        let Self { scoped, statics, root, signature } = self;
        let plan = BuiltinPackagePlan::for_executable(&statics, &signature)?;
        let package = BuiltinPackageLinker::link(plan.value)?;
        let defs = scoped.defs.rebind::<ds::DynamicsScope>();
        let function = root.link(&statics);
        let root = Rc::new(ds::Computation::VApp(App(function, package)));
        Ok(DynamicsProgram { defs, root })
    }
}

impl BuiltinComputationRootLinker {
    pub fn run(self) -> Result<DynamicsProgram, BuiltinPackageError> {
        let Self { scoped, statics, root, signature } = self;
        let defs = scoped.defs.rebind::<ds::DynamicsScope>();
        let (root, _) = std::iter::successors(Some(signature), |signature| {
            BuiltinPackageLinker::computation_signature(&statics, signature.codomain)
        })
        .try_fold(
            (root.link(&statics), HashSet::new()),
            |(function, mut seen), signature| {
                if !seen.insert(signature.codomain) {
                    return Err(BuiltinPackageError::RecursiveContract { ty: signature.codomain });
                }
                let plan = BuiltinPackagePlan::for_computation(&statics, &signature)?;
                let package = BuiltinPackageLinker::link(plan.value)?;
                Ok::<_, BuiltinPackageError>((
                    Rc::new(ds::Computation::VApp(App(function, package))),
                    seen,
                ))
            },
        )?;
        Ok(DynamicsProgram { defs, root })
    }
}

impl BuiltinValueRootLinker {
    pub fn run(self) -> Result<DynamicsProgram, BuiltinPackageError> {
        let Self { scoped, statics, root, signature } = self;
        let defs = scoped.defs.rebind::<ds::DynamicsScope>();
        let (value, _) = std::iter::successors(Some(signature), |signature| {
            BuiltinPackageLinker::value_signature(&statics, signature.codomain)
        })
        .try_fold(
            (root.link(&statics), HashSet::new()),
            |(function, mut seen), signature| {
                if !seen.insert(signature.codomain) {
                    return Err(BuiltinPackageError::RecursiveContract { ty: signature.codomain });
                }
                let plan = BuiltinPackagePlan::for_value(&statics, &signature)?;
                let package = BuiltinPackageLinker::link(plan.value)?;
                Ok::<_, BuiltinPackageError>((
                    Rc::new(ds::Value::VApp(App(function, package))),
                    seen,
                ))
            },
        )?;
        let root = Rc::new(ds::Computation::Ret(Return(value)));
        Ok(DynamicsProgram { defs, root })
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
            | VPat::Named(Named(_, inner)) => inner.link(statics).as_ref().to_owned(),
            | VPat::Ctor(Ctor(ctor, pat)) => {
                let ctor = ctor.to_owned();
                let pat = pat.link(statics);
                Ctor(ctor, pat).into()
            }
            | VPat::Alias(Alias(patterns)) => {
                let patterns = patterns.iter().map(|pattern| pattern.link(statics)).collect();
                Alias(ds::ConsN::from_vec(patterns).unwrap()).into()
            }
            | VPat::Triv(Triv) => Triv.into(),
            | VPat::VCons(ss::ConsN(items, tail)) => {
                let items = items.iter().map(|item| item.link(statics)).collect();
                let tail = tail.link(statics);
                ds::ConsN(items, tail).into()
            }
            | VPat::SCons(ss::ConsN(_, body)) => {
                let body = body.link(statics);
                body.as_ref().to_owned()
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
            | Value::Named(Named(_, inner)) => inner.link(statics).as_ref().to_owned(),
            | Value::Let(Let { binder, bindee, tail }) => {
                let binder = binder.link(statics);
                let bindee = bindee.link(statics);
                let tail = tail.link(statics);
                Let { binder, bindee, tail }.into()
            }
            | Value::VAbs(Abs(binder, body)) => {
                let binder = binder.link(statics);
                let body = body.link(statics);
                Abs(binder, body).into()
            }
            | Value::VApp(App(function, argument)) => {
                let function = function.link(statics);
                let argument = argument.link(statics);
                App(function, argument).into()
            }
            | Value::TAbs(Abs(_, body)) => {
                let body = body.link(statics);
                body.as_ref().to_owned()
            }
            | Value::TApp(App(body, _)) => {
                let body = body.link(statics);
                body.as_ref().to_owned()
            }
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
            | Value::VCons(ss::ConsN(items, tail)) => {
                let items = items.iter().map(|item| item.link(statics)).collect();
                let tail = tail.link(statics);
                ds::ConsN(items, tail).into()
            }
            | Value::SCons(ss::ConsN(_, body)) => {
                let body = body.link(statics);
                body.as_ref().to_owned()
            }
            | Value::Proj(Proj(head, field)) => {
                let head = head.link(statics);
                return field.target.products.iter().fold(head, |head, projection| {
                    Rc::new(Proj(head, projection.position).into())
                });
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

#[cfg(test)]
mod tests {
    use super::*;
    use zydeco_statics::arena::StaticsScope;
    use zydeco_utils::prelude::IdAllocator;

    #[test]
    fn computation_roots_link_and_evaluate_without_declarations() {
        let mut allocator = IdAllocator::<StaticsScope>::new();
        let value = allocator.alloc();
        let root = allocator.alloc();
        let mut statics = StaticsArena::default();
        statics.values.insert_new(value, ss::Triv.into());
        statics.compus.insert_new(root, ss::Return(value).into());

        let arena =
            RootLinker { scoped: ScopedArena::default(), statics: Arc::new(statics), root }.run();
        let mut input = std::io::empty();
        let mut output = Vec::new();
        let result = ds::Runtime::new(&mut input, &mut output, &[], arena).run();

        assert!(matches!(result, ds::ProgKont::Ret(ds::SemValue::Triv(ss::Triv))));
    }
}
