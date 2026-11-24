//! Shared static value evaluation and demand-driven elaboration into [`zydeco_ir::CoreProgram`].
//!
//! The evaluator retains lexical closures and mixed static/runtime structures
//! while composing values. Representable runtime terms are built directly into
//! [`zydeco_ir::CoreArena`] on demand, keeping front-end arenas untouched for
//! editor queries while ensuring interpretation and lowering consume a
//! self-contained residual IR.

use crate::{Env, ReportedError, Result, SkolemScope, TyEnv, TyckError, Tycker, syntax::*};
use rustc_hash::{FxHashMap, FxHashSet};
use std::{cell::Cell, rc::Rc, sync::Arc};
use zydeco_foundation::{
    dense::{DenseArena, DenseId},
    err::Report,
    prelude::ArenaAccess,
    span::Span,
};
use zydeco_ir::{
    CoreArena, CoreProgram, CoreRoot, DataId as IrDataId, ProductLayout, ProductProjection,
    VCons as IrVCons, syntax as ir,
};

/// A static requirement that remains at an executable boundary.
#[derive(Clone, Debug, thiserror::Error)]
#[error("{}", self.message())]
pub enum StaticEliminationError {
    UnresolvedApplication { function: ValueId },
    UnresolvedMatch { value: ValueId },
    UnresolvedInteger { value: ValueId },
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
            | Self::RuntimeValue { value, .. }
            | Self::UnresolvedMatch { value }
            | Self::UnresolvedInteger { value } => (*value).into(),
            | Self::RuntimeComputation { computation, .. } => (*computation).into(),
        }
    }

    pub fn message(&self) -> &'static str {
        match self {
            | Self::UnresolvedApplication { .. } => {
                "value-function implementation is unavailable to static reduction"
            }
            | Self::RuntimeValue { .. } => "static-only value remains in a runtime payload",
            | Self::UnresolvedMatch { .. } => "value match requires a statically known choice",
            | Self::UnresolvedInteger { .. } => {
                "value integer operation requires statically known operands"
            }
            | Self::RuntimeComputation { .. } => {
                "runtime computation requires a static-only representation"
            }
            | Self::ReductionLimit { .. } => "static value reduction exceeded the compiler limit",
        }
    }
}

/// Static evidence visible to dependent application checking. Runtime data
/// stays opaque, so inspecting a value never enters its computations.
pub(crate) enum StaticShape {
    Opaque,
    Package(Vec<StaticTermId>, Box<StaticShape>),
    Product(Vec<StaticShape>),
}

impl StaticShape {
    fn in_scope(self, tycker: &mut Tycker<'_>, scope: &SkolemScope) -> Self {
        match self {
            | Self::Package(witnesses, body) => {
                if witnesses.iter().all(|witness| match witness {
                    | StaticTermId::Type(ty) => ty.has_visible_witnesses(tycker, scope),
                    | StaticTermId::Kind(_) => true,
                }) {
                    Self::Package(witnesses, Box::new(body.in_scope(tycker, scope)))
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
            | Self::Package(witnesses, _) => Some(witnesses.clone()),
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
    SelectExports,
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

zydeco_foundation::define_idx! {
    struct ShareId;
}

struct ShareSlot {
    owner_scope: usize,
    span: Span,
    value: StaticValue,
    materialized: Option<ir::VarId>,
}

#[derive(Clone, Copy)]
struct CoreBinding {
    binder: ir::VPatId,
    bindee: ir::ValueId,
    span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct EnvKey {
    types: TypeAssignments,
    values: Box<[(DefId, u64)]>,
}

#[derive(Default)]
struct ScopeFrame {
    bindings: Vec<CoreBinding>,
    memo_keys: Vec<(CompuId, Arc<EnvKey>)>,
}

#[derive(Clone)]
struct StaticValue(Rc<ValueInfo>);

struct ValueInfo {
    uid: u64,
    source: ValueId,
    ty: TypeId,
    max_scope: usize,
    form: ValueForm,
    shared: Option<ShareId>,
}

#[derive(Clone)]
enum ValueForm {
    Hole { preallocated: Rc<Cell<Option<ir::ValueId>>> },
    Triv,
    Lit(Literal),
    IntrinsicOp(zydeco_syntax::IntrinsicOp),
    Foreign(zydeco_syntax::CheckedImport),
    RuntimeVar(ir::VarId),
    RuntimeTerm(Rc<Cell<Option<ir::ValueId>>>),
    Thunk { body: CompuId, env: Environment },
    Function { binder: ValBinder, body: ValueId, env: Environment },
    Product(Vec<StaticValue>),
    Constructor { data: Option<DataId>, name: CtorName, payload: StaticValue },
    Package(ConsN<StaticTermId, StaticValue>),
    Named(FieldName, StaticValue),
}

impl StaticValue {
    fn shape(&self) -> StaticShape {
        match &self.0.form {
            | ValueForm::Package(ConsN(witnesses, body)) => {
                StaticShape::Package(witnesses.clone(), Box::new(body.shape()))
            }
            | ValueForm::Product(fields) => {
                StaticShape::Product(fields.iter().map(Self::shape).collect())
            }
            | ValueForm::Named(_, inner) => inner.shape(),
            | _ => StaticShape::Opaque,
        }
    }

    fn is_runtime(&self) -> bool {
        match &self.0.form {
            | ValueForm::Function { .. } => false,
            | ValueForm::Hole { .. }
            | ValueForm::Triv
            | ValueForm::Lit(_)
            | ValueForm::IntrinsicOp(_)
            | ValueForm::Foreign(_)
            | ValueForm::RuntimeVar { .. }
            | ValueForm::RuntimeTerm { .. }
            | ValueForm::Thunk { .. } => true,
            | ValueForm::Product(values) => values.iter().all(Self::is_runtime),
            | ValueForm::Constructor { payload, .. }
            | ValueForm::Package(ConsN(_, payload))
            | ValueForm::Named(_, payload) => payload.is_runtime(),
        }
    }

    fn is_opaque_runtime(&self) -> bool {
        matches!(
            self.0.form,
            ValueForm::Hole { .. }
                | ValueForm::Triv
                | ValueForm::Lit(_)
                | ValueForm::IntrinsicOp(_)
                | ValueForm::Foreign(_)
                | ValueForm::RuntimeVar { .. }
                | ValueForm::RuntimeTerm { .. }
                | ValueForm::Thunk { .. }
        )
    }

    fn unnamed(&self) -> Self {
        match &self.0.form {
            | ValueForm::Named(_, inner) => inner.unnamed(),
            | _ => self.clone(),
        }
    }
}

struct MatchFailure<'a> {
    scrutinee: StaticValue,
    arms: &'a [Matcher<VPatId, CompuId>],
    env: Environment,
    source: CompuId,
}

/// Demand-driven static evaluator emitting self-contained [`CoreProgram`] nodes.
pub(crate) struct StaticElaborator<'a, 'db> {
    tycker: &'a mut Tycker<'db>,
    core: CoreArena,
    protocols: crate::elaborate::protocols::SourceProtocols,
    data_map: FxHashMap<DataId, IrDataId>,
    types: FxHashMap<(TypeId, TypeAssignments), TypeId>,
    representations: FxHashMap<TypeId, bool>,
    unrolled_cache: FxHashMap<TypeId, TypeId>,
    thunk_free_defs: FxHashMap<CompuId, Arc<[DefId]>>,
    value_free_defs: FxHashMap<ValueId, Arc<[DefId]>>,
    memoized_thunks: FxHashMap<(CompuId, Arc<EnvKey>), ir::VarId>,
    shares: DenseArena<ShareId, ShareSlot>,
    scopes: Vec<ScopeFrame>,
    free_vars: FxHashMap<DefId, ir::VarId>,
    next_uid: u64,
    reduction_depth: usize,
    remaining_reductions: usize,
    purpose: Purpose,
    aliases: FxHashSet<DefId>,
    application_site: Option<ValueId>,
}

mod computation;
mod pattern;
mod residual;
mod value;

impl<'a, 'db> StaticElaborator<'a, 'db> {
    const MAX_REDUCTION_DEPTH: usize = 128;
    const MAX_REDUCTIONS: usize = 65_536;

    fn with_purpose(tycker: &'a mut Tycker<'db>, purpose: Purpose) -> Self {
        Self {
            tycker,
            core: CoreArena::default(),
            protocols: crate::elaborate::protocols::SourceProtocols::default(),
            data_map: FxHashMap::default(),
            types: FxHashMap::default(),
            representations: FxHashMap::default(),
            unrolled_cache: FxHashMap::default(),
            thunk_free_defs: FxHashMap::default(),
            value_free_defs: FxHashMap::default(),
            memoized_thunks: FxHashMap::default(),
            shares: DenseArena::default(),
            scopes: Vec::new(),
            free_vars: FxHashMap::default(),
            next_uid: 0,
            reduction_depth: 0,
            remaining_reductions: Self::MAX_REDUCTIONS,
            purpose,
            aliases: FxHashSet::default(),
            application_site: None,
        }
    }

    pub(crate) fn run(tycker: &'a mut Tycker<'db>, root: TermAnnId) -> Result<Option<CoreProgram>> {
        let purpose = if matches!(root, TermAnnId::Value(..)) {
            Purpose::SelectExports
        } else {
            Purpose::Residualize
        };
        Self::run_with_purpose(tycker, root, purpose)
    }

    pub(crate) fn run_export(
        tycker: &'a mut Tycker<'db>, root: CompuId, root_ty: TypeId,
    ) -> Result<CoreProgram> {
        let program = Self::run_with_purpose(
            tycker,
            TermAnnId::Compu(root, root_ty),
            Purpose::SelectExports,
        )?;
        Ok(program.expect("computation root must yield a core program"))
    }

    fn run_with_purpose(
        tycker: &'a mut Tycker<'db>, root: TermAnnId, purpose: Purpose,
    ) -> Result<Option<CoreProgram>> {
        let mut evaluator = Self::with_purpose(tycker, purpose);
        let env = Environment::default();
        let root = match root {
            | TermAnnId::Compu(root, _) => {
                let compu = evaluator.computation(root, &env)?;
                CoreRoot::Compu(compu)
            }
            | TermAnnId::Value(root, _) => {
                let root_scope = evaluator.enter_scope();
                debug_assert_eq!(root_scope, 0);
                let value = evaluator.value(root, &env)?;
                if value.is_runtime() && evaluator.runtime_type(value.0.ty) {
                    let residual = evaluator.reify(&value)?;
                    let frame = evaluator.exit_scope();
                    let wrapped = evaluator.wrap_value_bindings(frame.bindings, residual);
                    CoreRoot::Value(wrapped)
                } else {
                    let _ = evaluator.exit_scope();
                    return Ok(None);
                }
            }
            | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Type(_, _) => {
                return Ok(None);
            }
        };
        evaluator.core.protocols = Arc::new(evaluator.protocols.graph);
        Ok(Some(CoreProgram::new(evaluator.core, root)))
    }

    pub(crate) fn inspect(
        tycker: &'a mut Tycker<'db>, value: ValueId, scope: &SkolemScope,
    ) -> StaticShape {
        let errors = tycker.errors.len();
        let mut evaluator = Self::with_purpose(tycker, Purpose::Inspect);
        evaluator.enter_scope();
        // An unresolved type or value means evidence is unavailable. The
        // application checker owns that diagnostic. This evidence query
        // performs substitution, but no unification or execution.
        let result = evaluator.value(value, &Environment::default());
        let _ = evaluator.exit_scope();
        evaluator.tycker.errors.truncate(errors);
        result
            .map(|value| value.shape().in_scope(evaluator.tycker, scope))
            .unwrap_or(StaticShape::Opaque)
    }

    fn fail<T>(&mut self, error: StaticEliminationError) -> Result<T> {
        if self.purpose == Purpose::Inspect {
            return Err(ReportedError);
        }
        self.tycker.err(TyckError::StaticElimination(error))
    }

    fn ty(&mut self, ty: TypeId, env: &Environment) -> Result<TypeId> {
        if env.types.is_empty() {
            return Ok(ty);
        }
        let key = (ty, env.types.clone());
        if let Some(result) = self.types.get(&key) {
            return Ok(*result);
        }
        let result = ty.subst_absts(self.tycker, &env.types)?;
        self.types.insert(key, result);
        Ok(result)
    }

    /// Recursive type aliases can encode self-application without `fix`.
    /// Bound both nesting and total work instead of exhausting the host stack
    /// or silently choosing a runtime representation for such a program.
    fn enter_reduction(&mut self, function: ValueId) -> Result<()> {
        if self.reduction_depth == Self::MAX_REDUCTION_DEPTH || self.remaining_reductions == 0 {
            return self.fail(StaticEliminationError::ReductionLimit { function });
        }
        self.reduction_depth += 1;
        self.remaining_reductions -= 1;
        Ok(())
    }
}
