use crate::{builtin::BuiltinRuntime, syntax::DynamicsProgram, *};
use std::{collections::HashSet, rc::Rc, sync::Arc};
use thiserror::Error;
use zydeco_statics::{
    BuiltinPackagePlan, BuiltinPackagePlanError, BuiltinPackageValue, arena::StaticsArena,
    elaborate::static_values::StaticElaboration, surface_syntax::ScopedArena,
};
use zydeco_syntax::*;
use zydeco_utils::prelude::ArenaAccess;

/// Erase nodes reached from a selected residual root into dynamic syntax.
trait Link {
    type Arena<'a>;
    type Out;
    fn link(&self, arena: Self::Arena<'_>) -> Self::Out;
}

/// Link the recorded residual of one checked source computation root.
pub struct RootLinker {
    pub scoped: Arc<ScopedArena>,
    pub statics: Arc<StaticsArena>,
    pub root: ss::CompuId,
}

/// Link the recorded residual of one checked source value as a computation that returns it.
pub struct ValueRootLinker {
    pub scoped: Arc<ScopedArena>,
    pub statics: Arc<StaticsArena>,
    pub root: ss::ValueId,
}

/// Link a package-dependent root and apply the concrete host Builtin package.
pub struct BuiltinRootLinker {
    pub scoped: Arc<ScopedArena>,
    pub statics: Arc<StaticsArena>,
    pub root: ss::CompuId,
    pub signature: ss::PackPi,
}

/// Apply host Builtin packages until a package-dependent computation reaches its result.
pub struct BuiltinComputationRootLinker {
    pub scoped: Arc<ScopedArena>,
    pub statics: Arc<StaticsArena>,
    pub root: ss::CompuId,
    pub signature: ss::PackPi,
}

/// A missing executable residual or an invalid host Builtin contract.
#[derive(Clone, Debug, Error)]
pub enum LinkError {
    #[error("cannot link source root {root:?} without a matching residual runtime root")]
    MissingResidual { root: ss::TermId },
    #[error(transparent)]
    Plan(#[from] BuiltinPackagePlanError),
    #[error("host package contracts form a recursive result at type {ty:?}")]
    RecursiveContract { ty: ss::TypeId },
}

/// Materialize the operation values described by a checked Builtin package plan.
struct BuiltinPackageLinker;

impl BuiltinPackageLinker {
    fn link(value: BuiltinPackageValue) -> Result<ds::RcValue, LinkError> {
        match value {
            | BuiltinPackageValue::Unit => Ok(Rc::new(ds::Value::Triv(Triv))),
            | BuiltinPackageValue::Operation(role) => Ok(BuiltinRuntime::package_value(role)),
            | BuiltinPackageValue::Product(product) => {
                let values = product.into_iter().map(Self::link).collect::<Result<Vec<_>, _>>()?;
                Ok(Rc::new(ds::Value::VCons(values)))
            }
        }
    }

    fn computation_signature(statics: &StaticsArena, ty: ss::TypeId) -> Option<ss::PackPi> {
        match Self::type_view(statics, ty) {
            | Some(ss::Type::PackPi(signature)) => Some(signature.as_ref().clone()),
            | _ => None,
        }
    }

    fn type_view(statics: &StaticsArena, ty: ss::TypeId) -> Option<&ss::Type> {
        statics.normalized_at(ty)
    }
}

impl RootLinker {
    fn residual(statics: &StaticsArena, root: ss::CompuId) -> Result<ss::CompuId, LinkError> {
        match &statics.static_elaboration {
            | Some(StaticElaboration {
                source: ss::TermAnnId::Compu(source, _),
                residual: Some(ss::TermAnnId::Compu(residual, _)),
            }) if *source == root => Ok(*residual),
            | _ => Err(LinkError::MissingResidual { root: root.into() }),
        }
    }

    /// Erase the source root's residual computation into the dynamic root.
    pub fn run(self) -> Result<DynamicsProgram, LinkError> {
        let Self { scoped, statics, root } = self;
        let root = Self::residual(&statics, root)?;
        let defs = statics.dynamic_definitions(&scoped).rebind::<ds::DynamicsScope>();
        let root = root.link(&statics);
        Ok(DynamicsProgram::new(defs, root))
    }
}

impl ValueRootLinker {
    pub fn run(self) -> Result<DynamicsProgram, LinkError> {
        let Self { scoped, statics, root } = self;
        let root = match &statics.static_elaboration {
            | Some(StaticElaboration {
                source: ss::TermAnnId::Value(source, _),
                residual: Some(ss::TermAnnId::Value(residual, _)),
            }) if *source == root => *residual,
            | _ => return Err(LinkError::MissingResidual { root: root.into() }),
        };
        let defs = statics.dynamic_definitions(&scoped).rebind::<ds::DynamicsScope>();
        let value = root.link(&statics);
        let root = Rc::new(ds::Computation::Ret(Return(value)));
        Ok(DynamicsProgram::new(defs, root))
    }
}

impl BuiltinRootLinker {
    pub fn run(self) -> Result<DynamicsProgram, LinkError> {
        let Self { scoped, statics, root, signature } = self;
        let root = RootLinker::residual(&statics, root)?;
        let plan = BuiltinPackagePlan::for_executable(&statics, &signature)?;
        let package = BuiltinPackageLinker::link(plan.value)?;
        let defs = statics.dynamic_definitions(&scoped).rebind::<ds::DynamicsScope>();
        let function = root.link(&statics);
        let root = Rc::new(ds::Computation::VApp(App(function, package)));
        Ok(DynamicsProgram::new(defs, root))
    }
}

impl BuiltinComputationRootLinker {
    pub fn run(self) -> Result<DynamicsProgram, LinkError> {
        let Self { scoped, statics, root, signature } = self;
        let root = RootLinker::residual(&statics, root)?;
        let defs = statics.dynamic_definitions(&scoped).rebind::<ds::DynamicsScope>();
        let (root, _) = std::iter::successors(Some(signature), |signature| {
            BuiltinPackageLinker::computation_signature(&statics, signature.codomain)
        })
        .try_fold(
            (root.link(&statics), HashSet::new()),
            |(function, mut seen), signature| {
                if !seen.insert(signature.codomain) {
                    return Err(LinkError::RecursiveContract { ty: signature.codomain });
                }
                let plan = BuiltinPackagePlan::for_computation(&statics, &signature)?;
                let package = BuiltinPackageLinker::link(plan.value)?;
                Ok::<_, LinkError>((Rc::new(ds::Computation::VApp(App(function, package))), seen))
            },
        )?;
        Ok(DynamicsProgram::new(defs, root))
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
            | VPat::Lit(literal) => ds::ValuePattern::Lit(literal.clone()),
            | VPat::Alias(Alias(patterns)) => {
                let patterns = patterns.iter().map(|pattern| pattern.link(statics)).collect();
                Alias(ds::ConsN::from_vec(patterns).unwrap()).into()
            }
            | VPat::Triv(Triv) => Triv.into(),
            | VPat::VCons(items) => {
                let items = items.iter().map(|item| item.link(statics)).collect();
                ds::ValuePattern::VCons(items)
            }
            | VPat::SCons(ss::ConsN(_, body)) => {
                let body = body.link(statics);
                body.as_ref().to_owned()
            }
            | VPat::View(_) => unreachable!("static elaboration eliminates views before linking"),
        };
        Rc::new(vpat)
    }
}

impl Link for ss::ValueId {
    type Arena<'a> = &'a StaticsArena;
    type Out = ds::RcValue;

    fn link(&self, statics: Self::Arena<'_>) -> Self::Out {
        if let Some(import) = statics.foreign_imports.get(self).cloned() {
            let primitive = ds::Computation::Foreign(ds::ForeignPrim { import });
            return Rc::new(ds::Value::Thunk(Thunk(Rc::new(primitive))));
        }
        let value = &statics.values[self];
        use ss::Value;
        let value = match value {
            | Value::Hole(Hole) => Hole.into(),
            | Value::Var(def) => (*def).into(),
            | Value::ValAbs(_) | Value::ValApp(_) | Value::Match(_) | Value::IntOp(_) => {
                unreachable!("static value operations must be eliminated before linking")
            }
            | Value::Named(Named(_, inner)) => inner.link(statics).as_ref().to_owned(),
            | Value::Let(Let { binder, bindee, tail }) => {
                let binder = binder.link(statics);
                let bindee = bindee.link(statics);
                let tail = tail.link(statics);
                Let { binder, bindee, tail }.into()
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
            | Value::VCons(items) => {
                let items = items.iter().map(|item| item.link(statics)).collect();
                ds::Value::VCons(items)
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
        let unit = allocator.alloc();
        let ret = allocator.alloc();
        let result = allocator.alloc();
        let vtype = allocator.alloc();
        let ctype = allocator.alloc();
        let ret_kind = allocator.alloc();
        let mut statics = StaticsArena::default();
        statics.values.insert_new(value, ss::Triv.into());
        statics.compus.insert_new(root, ss::Return(value).into());
        statics.kinds_pre.insert_new(vtype, ss::Fillable::Done(ss::VType.into()));
        statics.kinds_pre.insert_new(ctype, ss::Fillable::Done(ss::CType.into()));
        statics.kinds_pre.insert_new(ret_kind, ss::Fillable::Done(Arrow(vtype, ctype).into()));
        statics.types_pre.insert_new(unit, ss::Fillable::Done(ss::Type::Unit(ss::UnitTy)), vtype);
        statics.types_pre.insert_new(ret, ss::Fillable::Done(ss::Type::Ret(ss::RetTy)), ret_kind);
        statics.types_pre.insert_new(
            result,
            ss::Fillable::Done(ss::Type::App(App(ret, unit))),
            ctype,
        );
        statics.annotations_value.insert_new(value, unit);
        statics.annotations_compu.insert_new(root, result);
        statics.static_elaboration = Some(StaticElaboration {
            source: ss::TermAnnId::Compu(root, result),
            residual: Some(ss::TermAnnId::Compu(root, result)),
        });

        let arena = RootLinker {
            scoped: Arc::new(ScopedArena::default()),
            statics: Arc::new(statics),
            root,
        }
        .run()
        .unwrap();
        let mut input = std::io::empty();
        let mut output = Vec::new();
        let result = ds::Runtime::new(&mut input, &mut output, &mut Vec::new(), &[], arena).run();

        assert!(matches!(result, ds::ProgKont::Ret(ds::SemValue::Triv(ss::Triv))));
    }
}
