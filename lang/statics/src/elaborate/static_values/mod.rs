//! Shared static value evaluation for dependent evidence and executable code.
//!
//! The evaluator retains lexical closures and mixed static/runtime structures
//! while composing values. Only representable values are reified into the
//! residual typed program. Source nodes remain available for editor queries;
//! interpretation and lowering enter through the same residual root.
//! Dependent application checking uses a shape-only inspection of this same
//! evaluator, restricted to witness identities visible in the caller's scope.

use crate::{
    Alloc, Env, KontFailure, ResultKont, SkolemScope, TyEnv, TyckError, Tycker, syntax::*,
};
use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};
use zydeco_utils::{err::Errorable, prelude::ArenaAccess};

/// A static requirement that remains at an executable boundary.
#[derive(Clone, Debug)]
pub enum StaticEliminationError {
    UnresolvedApplication { function: ValueId },
    RuntimeValue { value: ValueId, ty: TypeId },
    RuntimeComputation { computation: CompuId, ty: TypeId },
    ReductionLimit { function: ValueId },
}

impl StaticEliminationError {
    pub fn term(&self) -> TermId {
        match self {
            | Self::UnresolvedApplication { function } | Self::ReductionLimit { function } => {
                (*function).into()
            }
            | Self::RuntimeValue { value, .. } => (*value).into(),
            | Self::RuntimeComputation { computation, .. } => (*computation).into(),
        }
    }

    pub fn message(&self) -> &'static str {
        match self {
            | Self::UnresolvedApplication { .. } => {
                "value-function implementation is unavailable to static reduction"
            }
            | Self::RuntimeValue { .. } => "static-only value remains in a runtime payload",
            | Self::RuntimeComputation { .. } => {
                "runtime computation requires a static-only representation"
            }
            | Self::ReductionLimit { .. } => "static value reduction exceeded the compiler limit",
        }
    }
}

/// The original source root and its executable elaboration, when it has one.
/// Unapplied static library values have no residual runtime root.
#[derive(Clone, Debug)]
pub struct StaticElaboration {
    pub source: TermAnnId,
    pub residual: Option<TermAnnId>,
}

/// Static evidence visible to dependent application checking. Runtime data
/// stays opaque, so inspecting a value never enters its computations.
pub(crate) enum StaticShape {
    Opaque,
    Package(Vec<StaticTermId>),
    Product(Vec<StaticShape>),
}

impl StaticShape {
    fn in_scope(self, tycker: &Tycker<'_>, scope: &SkolemScope) -> Self {
        match self {
            | Self::Package(witnesses) => {
                if witnesses.iter().all(|witness| match witness {
                    | StaticTermId::Type(ty) => ty.has_visible_witnesses(tycker, scope),
                    | StaticTermId::Kind(_) => true,
                }) {
                    Self::Package(witnesses)
                } else {
                    Self::Opaque
                }
            }
            | Self::Product(fields) => Self::Product(
                fields.into_iter().map(|field| field.in_scope(tycker, scope)).collect(),
            ),
            | Self::Opaque => Self::Opaque,
        }
    }

    pub(crate) fn witnesses(&self) -> Option<Vec<StaticTermId>> {
        match self {
            | Self::Package(witnesses) => Some(witnesses.clone()),
            | Self::Product(fields) => {
                let witnesses =
                    fields.iter().filter_map(Self::witnesses).flatten().collect::<Vec<_>>();
                (!witnesses.is_empty()).then_some(witnesses)
            }
            | Self::Opaque => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Purpose {
    Inspect,
    Residualize,
}

type TypeAssignments = Arc<[(AbstId, TypeId)]>;

#[derive(Clone, Default)]
struct Environment {
    values: Env<StaticValue>,
    types: TypeAssignments,
}

impl Environment {
    fn with_types(&self, assignments: impl IntoIterator<Item = (AbstId, TypeId)>) -> Self {
        let types = assignments.into_iter().chain(self.types.iter().copied()).collect();
        Self { types, ..self.clone() }
    }
}

#[derive(Clone)]
struct StaticValue(Arc<ValueInfo>);

struct ValueInfo {
    source: ValueId,
    ty: TypeId,
    form: ValueForm,
    /// A runtime binding established in the value's creation scope. Keeping
    /// this alongside known structure preserves sharing through projections.
    shared: Option<ValueId>,
}

#[derive(Clone)]
enum ValueForm {
    Runtime(ValueId),
    Function { binder: ValBinder, body: ValueId, env: Environment },
    Product(Vec<StaticValue>),
    Constructor(CtorName, StaticValue),
    Package(ConsN<StaticTermId, StaticValue>),
    Named(FieldName, StaticValue),
}

impl StaticValue {
    fn shape(&self) -> StaticShape {
        match &self.0.form {
            | ValueForm::Package(ConsN(witnesses, _)) => StaticShape::Package(witnesses.clone()),
            | ValueForm::Product(fields) => {
                StaticShape::Product(fields.iter().map(Self::shape).collect())
            }
            | ValueForm::Named(_, inner) => inner.shape(),
            | _ => StaticShape::Opaque,
        }
    }

    fn with_form(source: ValueId, ty: TypeId, form: ValueForm) -> Self {
        Self(Arc::new(ValueInfo { source, ty, form, shared: None }))
    }

    fn is_runtime(&self) -> bool {
        match &self.0.form {
            | ValueForm::Function { .. } => false,
            | ValueForm::Runtime(_) => true,
            | ValueForm::Product(values) => values.iter().all(Self::is_runtime),
            | ValueForm::Constructor(_, value)
            | ValueForm::Package(ConsN(_, value))
            | ValueForm::Named(_, value) => value.is_runtime(),
        }
    }

    fn unnamed(&self) -> Self {
        match &self.0.form {
            | ValueForm::Named(_, inner) => inner.unnamed(),
            | _ => self.clone(),
        }
    }
}

#[derive(Clone)]
struct Binding {
    binder: VPatId,
    bindee: ValueId,
}

struct MatchFailure<'a> {
    scrutinee: StaticValue,
    arms: &'a [Matcher<VPatId, CompuId>],
    env: Environment,
    source: CompuId,
}

/// Builder for fresh residual terms in the checker's unpublished arena.
pub(crate) struct StaticElaborator<'a, 'db> {
    tycker: &'a mut Tycker<'db>,
    types: HashMap<(TypeId, TypeAssignments), TypeId>,
    representations: HashMap<TypeId, bool>,
    reduction_depth: usize,
    remaining_reductions: usize,
    purpose: Purpose,
    aliases: HashSet<DefId>,
}

mod value;
mod pattern;
mod computation;
mod residual;

impl<'a, 'db> StaticElaborator<'a, 'db> {
    const MAX_REDUCTION_DEPTH: usize = 128;
    const MAX_REDUCTIONS: usize = 65_536;
    pub(crate) fn run(
        tycker: &'a mut Tycker<'db>, root: TermAnnId,
    ) -> ResultKont<StaticElaboration> {
        let mut evaluator = Self {
            tycker,
            types: HashMap::new(),
            representations: HashMap::new(),
            reduction_depth: 0,
            remaining_reductions: Self::MAX_REDUCTIONS,
            purpose: Purpose::Residualize,
            aliases: HashSet::new(),
        };
        let env = Environment::default();
        let residual = match root {
            | TermAnnId::Compu(root, _) => {
                let residual = evaluator.computation(root, &env)?;
                Some(TermAnnId::Compu(
                    residual,
                    evaluator.tycker.statics.annotations_compu[&residual],
                ))
            }
            | TermAnnId::Value(root, _) => {
                let mut bindings = Vec::new();
                let value = evaluator.value(root, &env, &mut bindings)?;
                if value.is_runtime() {
                    let value = evaluator.reify(&value)?;
                    let residual = evaluator.value_bindings(bindings, value);
                    Some(TermAnnId::Value(
                        residual,
                        evaluator.tycker.statics.annotations_value[&residual],
                    ))
                } else {
                    None
                }
            }
            | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Type(_, _) => None,
        };
        Ok(StaticElaboration { source: root, residual })
    }

    pub(crate) fn inspect(tycker: &'a mut Tycker<'db>, value: ValueId) -> StaticShape {
        let errors = tycker.errors.len();
        let scope = tycker.statics.env_value[&value].skolem_scope().clone();
        let mut evaluator = Self {
            tycker,
            types: HashMap::new(),
            representations: HashMap::new(),
            reduction_depth: 0,
            remaining_reductions: Self::MAX_REDUCTIONS,
            purpose: Purpose::Inspect,
            aliases: HashSet::new(),
        };
        // An unresolved type or value means evidence is unavailable. The
        // application checker owns that diagnostic. This evidence query
        // performs substitution, but no unification or execution.
        let result = evaluator.value(value, &Environment::default(), &mut Vec::new());
        evaluator.tycker.errors.truncate(errors);
        result
            .map(|value| value.shape().in_scope(evaluator.tycker, &scope))
            .unwrap_or(StaticShape::Opaque)
    }

    fn fail<T>(&mut self, error: StaticEliminationError) -> ResultKont<T> {
        if self.purpose == Purpose::Inspect {
            return Err(KontFailure);
        }
        self.tycker.err_k(TyckError::StaticElimination(error), std::panic::Location::caller())
    }

    fn ty(&mut self, ty: TypeId, env: &Environment) -> ResultKont<TypeId> {
        if env.types.is_empty() {
            return Ok(ty);
        }
        let key = (ty, env.types.clone());
        if let Some(result) = self.types.get(&key) {
            return Ok(*result);
        }
        let result = ty.subst_absts_k(self.tycker, &env.types)?;
        self.types.insert(key, result);
        Ok(result)
    }

    /// Recursive type aliases can encode self-application without `fix`.
    /// Bound both nesting and total work instead of exhausting the host stack
    /// or silently choosing a runtime representation for such a program.
    fn enter_reduction(&mut self, function: ValueId) -> ResultKont<()> {
        if self.reduction_depth == Self::MAX_REDUCTION_DEPTH || self.remaining_reductions == 0 {
            return self.fail(StaticEliminationError::ReductionLimit { function });
        }
        self.reduction_depth += 1;
        self.remaining_reductions -= 1;
        Ok(())
    }
}
