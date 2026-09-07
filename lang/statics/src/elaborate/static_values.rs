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

    fn runtime_type(&mut self, ty: TypeId) -> bool {
        if let Some(result) = self.representations.get(&ty) {
            return *result;
        }
        let result = self.runtime_type_inner(ty, &mut HashSet::new());
        self.representations.insert(ty, result);
        result
    }

    fn runtime_type_inner(&self, ty: TypeId, visited: &mut HashSet<TypeId>) -> bool {
        if !visited.insert(ty) {
            return true;
        }
        match self.tycker.statics.normalized_at(ty).cloned() {
            | Some(Type::ValPi(_)) => false,
            | Some(Type::Abst(witness)) => match self.tycker.statics.seals.get(&witness).copied() {
                | Some(body) => self.runtime_type_inner(body, visited),
                | None => true,
            },
            | Some(Type::Prod(Prod(fields))) => {
                fields.into_iter().all(|ty| self.runtime_type_inner(ty, visited))
            }
            | Some(Type::Named(Named(_, body)))
            | Some(Type::Label(Label(_, body)))
            | Some(Type::ManifestKind(ManifestKind { body, .. }))
            | Some(Type::Forall(Forall(_, body))) => self.runtime_type_inner(body, visited),
            | Some(Type::Exists(exists)) => self.runtime_type_inner(exists.body, visited),
            | Some(Type::Arrow(Arrow(domain, codomain))) => {
                self.runtime_type_inner(domain, visited)
                    && self.runtime_type_inner(codomain, visited)
            }
            | Some(Type::PackPi(pi)) => {
                self.runtime_type_inner(pi.domain, visited)
                    && self.runtime_type_inner(pi.codomain, visited)
            }
            | Some(Type::App(App(function, argument))) => {
                self.runtime_type_inner(function, visited)
                    && self.runtime_type_inner(argument, visited)
            }
            | Some(Type::Data(data)) => {
                let arms = self.tycker.statics.datas[&data].clone();
                arms.into_iter().all(|(_, ty)| self.runtime_type_inner(ty, visited))
            }
            | Some(Type::CoData(codata)) => {
                let arms = self.tycker.statics.codatas[&codata].clone();
                arms.into_iter().all(|(_, ty)| self.runtime_type_inner(ty, visited))
            }
            | _ => true,
        }
    }

    fn alloc_value(&mut self, source: ValueId, node: Value, ty: TypeId) -> ValueId {
        let constructor = matches!(node, Value::Ctor(_));
        let value = Alloc::alloc(self.tycker, node, ty, &TyEnv::default());
        if let Some(origin) = self.tycker.statics.terms.source(&source.into()) {
            self.tycker.statics.terms.record(origin, TermId::Value(value));
        }
        if constructor && let Some(hint) = self.tycker.statics.data_hints.get(&source).copied() {
            self.tycker.statics.data_hints.insert_new(value, hint);
        }
        value
    }

    fn alloc_compu(&mut self, source: CompuId, node: Computation, ty: TypeId) -> CompuId {
        let compu = Alloc::alloc(self.tycker, node, ty, &TyEnv::default());
        if let Some(origin) = self.tycker.statics.terms.source(&source.into()) {
            self.tycker.statics.terms.record(origin, TermId::Compu(compu));
        }
        if let Some(hint) = self.tycker.statics.codata_hints.get(&source).copied() {
            self.tycker.statics.codata_hints.insert_new(compu, hint);
        }
        compu
    }

    fn alloc_pattern(&mut self, source: VPatId, node: ValuePattern, ty: TypeId) -> VPatId {
        let constructor = matches!(node, ValuePattern::Ctor(_));
        let pattern = Alloc::alloc(self.tycker, node, ty, &TyEnv::default());
        if let Some(origin) = self.tycker.statics.pats.source(&source.into()) {
            self.tycker.statics.pats.record(origin, PatId::Value(pattern));
        }
        if constructor && let Some(hint) = self.tycker.statics.data_pat_hints.get(&source).copied()
        {
            self.tycker.statics.data_pat_hints.insert_new(pattern, hint);
        }
        pattern
    }

    fn variable(&mut self, source: Option<ValueId>, ty: TypeId) -> (VPatId, StaticValue) {
        let definition =
            Alloc::alloc(self.tycker, VarName("__static_value__".into()), AnnId::Type(ty), &());
        let pattern =
            Alloc::alloc(self.tycker, ValuePattern::Var(definition), ty, &TyEnv::default());
        let value = match source {
            | Some(source) => self.alloc_value(source, Value::Var(definition), ty),
            | None => Alloc::alloc(self.tycker, Value::Var(definition), ty, &TyEnv::default()),
        };
        (pattern, StaticValue::with_form(source.unwrap_or(value), ty, ValueForm::Runtime(value)))
    }

    fn value_bindings(&mut self, bindings: Vec<Binding>, mut tail: ValueId) -> ValueId {
        for Binding { binder, bindee } in bindings.into_iter().rev() {
            let ty = self.tycker.statics.annotations_value[&tail];
            tail = self.alloc_value(tail, Value::Let(Let { binder, bindee, tail }), ty);
        }
        tail
    }

    fn compu_bindings(&mut self, bindings: Vec<Binding>, mut tail: CompuId) -> CompuId {
        for Binding { binder, bindee } in bindings.into_iter().rev() {
            let ty = self.tycker.statics.annotations_compu[&tail];
            tail = self.alloc_compu(tail, Computation::Let(Let { binder, bindee, tail }), ty);
        }
        tail
    }

    fn reify(&mut self, value: &StaticValue) -> ResultKont<ValueId> {
        if self.purpose == Purpose::Inspect {
            return Err(KontFailure);
        }
        if !self.runtime_type(value.0.ty) {
            return self.fail(StaticEliminationError::RuntimeValue {
                value: value.0.source,
                ty: value.0.ty,
            });
        }
        if let Some(shared) = value.0.shared {
            return Ok(shared);
        }
        let node = match &value.0.form {
            | ValueForm::Function { .. } => {
                return self.fail(StaticEliminationError::RuntimeValue {
                    value: value.0.source,
                    ty: value.0.ty,
                });
            }
            | ValueForm::Runtime(runtime) => {
                return Ok(*runtime);
            }
            | ValueForm::Product(fields) => Value::VCons(
                fields.iter().map(|value| self.reify(value)).collect::<ResultKont<_>>()?,
            ),
            | ValueForm::Constructor(name, payload) => {
                Value::Ctor(Ctor(name.clone(), self.reify(payload)?))
            }
            | ValueForm::Package(ConsN(prefix, payload)) => {
                Value::SCons(ConsN(prefix.clone(), self.reify(payload)?))
            }
            | ValueForm::Named(name, payload) => {
                Value::Named(Named(name.clone(), self.reify(payload)?))
            }
        };
        Ok(self.alloc_value(value.0.source, node, value.0.ty))
    }

    fn share(
        &mut self, value: StaticValue, bindings: &mut Vec<Binding>,
    ) -> ResultKont<StaticValue> {
        if self.purpose == Purpose::Inspect
            || value.0.shared.is_some()
            || matches!(value.0.form, ValueForm::Function { .. })
        {
            return Ok(value);
        }
        // Share the fields first, so opening a known product or package uses
        // the same runtime values as materializing the aggregate.
        let form = match &value.0.form {
            | ValueForm::Product(fields) => ValueForm::Product(
                fields
                    .iter()
                    .map(|field| self.share(field.clone(), bindings))
                    .collect::<ResultKont<_>>()?,
            ),
            | ValueForm::Constructor(name, payload) => {
                ValueForm::Constructor(name.clone(), self.share(payload.clone(), bindings)?)
            }
            | ValueForm::Package(ConsN(prefix, payload)) => {
                ValueForm::Package(ConsN(prefix.clone(), self.share(payload.clone(), bindings)?))
            }
            | ValueForm::Named(name, payload) => {
                ValueForm::Named(name.clone(), self.share(payload.clone(), bindings)?)
            }
            | ValueForm::Runtime(_) => value.0.form.clone(),
            | ValueForm::Function { .. } => unreachable!(),
        };
        let value = StaticValue::with_form(value.0.source, value.0.ty, form);
        if value.is_runtime() && self.runtime_type(value.0.ty) {
            let bindee = self.reify(&value)?;
            let (binder, variable) = self.variable(Some(value.0.source), value.0.ty);
            let ValueForm::Runtime(shared) = variable.0.form else { unreachable!() };
            bindings.push(Binding { binder, bindee });
            Ok(StaticValue(Arc::new(ValueInfo {
                source: value.0.source,
                ty: value.0.ty,
                form: value.0.form.clone(),
                shared: Some(shared),
            })))
        } else {
            Ok(value)
        }
    }

    fn value(
        &mut self, source: ValueId, env: &Environment, bindings: &mut Vec<Binding>,
    ) -> ResultKont<StaticValue> {
        let ty = self.ty(self.tycker.statics.annotations_value[&source], env)?;
        if self.tycker.statics.foreign_imports.get(&source).is_some() {
            return Ok(StaticValue::with_form(source, ty, ValueForm::Runtime(source)));
        }
        let form = match self.tycker.statics.values[&source].clone() {
            | Value::Var(definition) => {
                if let Some(value) = env.values.get(&definition) {
                    return Ok(StaticValue(Arc::new(ValueInfo {
                        source,
                        ty: value.0.ty,
                        form: value.0.form.clone(),
                        shared: value.0.shared,
                    })));
                }
                if self.purpose == Purpose::Inspect {
                    return self.inspect_alias(source, definition, ty, env, bindings);
                }
                ValueForm::Runtime(source)
            }
            | Value::Hole(_) | Value::Triv(_) | Value::Lit(_) => ValueForm::Runtime(source),
            | Value::ValAbs(Abs(binder, body)) => {
                ValueForm::Function { binder, body, env: env.clone() }
            }
            | Value::ValApp(App(function, argument)) => {
                let function = self.value(function, env, bindings)?;
                return self.apply(function, argument, env, bindings);
            }
            | Value::Let(Let { binder, bindee, tail }) => {
                let value = self.value(bindee, env, bindings)?;
                let value = self.share(value, bindings)?;
                let mut local = env.clone();
                self.bind(binder, value, &mut local, bindings)?;
                return self.value(tail, &local, bindings);
            }
            | Value::Thunk(Thunk(body)) => {
                if self.purpose == Purpose::Inspect {
                    return Ok(StaticValue::with_form(source, ty, ValueForm::Runtime(source)));
                }
                let body = self.computation(body, env)?;
                ValueForm::Runtime(self.alloc_value(source, Value::Thunk(Thunk(body)), ty))
            }
            | Value::VCons(fields) => ValueForm::Product(
                fields
                    .into_iter()
                    .map(|field| self.value(field, env, bindings))
                    .collect::<ResultKont<_>>()?,
            ),
            | Value::Ctor(Ctor(name, payload)) => {
                ValueForm::Constructor(name, self.value(payload, env, bindings)?)
            }
            | Value::Named(Named(name, payload)) => {
                ValueForm::Named(name, self.value(payload, env, bindings)?)
            }
            | Value::SCons(ConsN(prefix, payload)) => {
                let prefix = prefix
                    .into_iter()
                    .map(|witness| match witness {
                        | StaticTermId::Type(ty) => self.ty(ty, env).map(StaticTermId::Type),
                        | StaticTermId::Kind(_) => Ok(witness),
                    })
                    .collect::<ResultKont<Vec<_>>>()?;
                ValueForm::Package(ConsN(prefix, self.value(payload, env, bindings)?))
            }
            | Value::Proj(Proj(head, field)) => {
                let mut value = self.value(head, env, bindings)?;
                for (index, projection) in field.target.products.iter().enumerate() {
                    if matches!(value.0.form, ValueForm::Runtime(_)) {
                        if self.purpose == Purpose::Inspect {
                            return Ok(StaticValue::with_form(
                                source,
                                ty,
                                ValueForm::Runtime(source),
                            ));
                        }
                        let products = field.target.products[index..]
                            .iter()
                            .map(|step| {
                                Ok(ProductProjection {
                                    product: self.ty(step.product, env)?,
                                    position: step.position,
                                })
                            })
                            .collect::<ResultKont<_>>()?;
                        let head = self.reify(&value)?;
                        let node = Value::Proj(Proj(
                            head,
                            ResolvedField { name: field.name, target: ProjTarget { products } },
                        ));
                        let residual = self.alloc_value(source, node, ty);
                        return Ok(StaticValue::with_form(
                            source,
                            ty,
                            ValueForm::Runtime(residual),
                        ));
                    }
                    let product = self.ty(projection.product, env)?;
                    value = self.project(value, product, projection.position, source, bindings)?;
                }
                let value = value.unnamed();
                if matches!(value.0.form, ValueForm::Runtime(_)) && value.0.ty != ty {
                    if self.purpose == Purpose::Inspect {
                        return Ok(StaticValue::with_form(source, ty, ValueForm::Runtime(source)));
                    }
                    let head = self.reify(&value)?;
                    let node = Value::Proj(Proj(
                        head,
                        ResolvedField {
                            name: field.name,
                            target: ProjTarget { products: Vec::new() },
                        },
                    ));
                    let residual = self.alloc_value(source, node, ty);
                    return Ok(StaticValue::with_form(source, ty, ValueForm::Runtime(residual)));
                }
                return Ok(value);
            }
        };
        Ok(StaticValue::with_form(source, ty, form))
    }

    fn apply(
        &mut self, function: StaticValue, argument: ValArgument, caller: &Environment,
        bindings: &mut Vec<Binding>,
    ) -> ResultKont<StaticValue> {
        self.enter_reduction(function.0.source)?;
        let result = self.apply_inner(function, argument, caller, bindings);
        self.reduction_depth -= 1;
        result
    }

    fn inspect_alias(
        &mut self, source: ValueId, definition: DefId, ty: TypeId, env: &Environment,
        bindings: &mut Vec<Binding>,
    ) -> ResultKont<StaticValue> {
        if !self.aliases.insert(definition) {
            return Err(KontFailure);
        }
        let alias = self.tycker.statics.value_aliases.get(&definition).copied();
        let result = match alias {
            | Some(alias) => self.value(alias, env, bindings),
            | None => Ok(StaticValue::with_form(source, ty, ValueForm::Runtime(source))),
        };
        self.aliases.remove(&definition);
        let value = result?;
        if let Some(witnesses) = self.tycker.statics.package_aliases.get(&definition).cloned() {
            let value = value.unnamed();
            let payload = match &value.0.form {
                | ValueForm::Package(ConsN(_, payload)) => payload.clone(),
                | _ => value.clone(),
            };
            Ok(StaticValue::with_form(source, ty, ValueForm::Package(ConsN(witnesses, payload))))
        } else {
            Ok(value)
        }
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

    fn apply_inner(
        &mut self, function: StaticValue, argument: ValArgument, caller: &Environment,
        bindings: &mut Vec<Binding>,
    ) -> ResultKont<StaticValue> {
        let function = function.unnamed();
        let ValueForm::Function { binder, body, env } = &function.0.form else {
            return self.fail(StaticEliminationError::UnresolvedApplication {
                function: function.0.source,
            });
        };
        let mut local = env.clone();
        match (binder, argument) {
            | (ValBinder::Type(pattern), ValArgument::Type(argument)) => {
                let argument = self.ty(argument, caller)?;
                let classifier = function.0.ty.unroll_k(self.tycker)?;
                let Some(Type::ValPi(pi)) = self.tycker.statics.normalized_at(classifier) else {
                    unreachable!()
                };
                let ValPiBinder::Type(binder) = &pi.binder else { unreachable!() };
                let assignments = std::iter::once((binder.witness, argument))
                    .chain(self.pattern_witness(*pattern).map(|witness| (witness, argument)));
                local = local.with_types(assignments);
            }
            | (ValBinder::Value(pattern), ValArgument::Value(argument)) => {
                let argument = self.value(argument, caller, bindings)?;
                let argument = self.share(argument, bindings)?;
                self.bind(*pattern, argument, &mut local, bindings)?;
            }
            | _ => unreachable!("typed value application follows its binder sort"),
        }
        self.value(*body, &local, bindings)
    }

    fn project(
        &mut self, value: StaticValue, product: TypeId, position: usize, source: ValueId,
        bindings: &mut Vec<Binding>,
    ) -> ResultKont<StaticValue> {
        match &value.0.form {
            | ValueForm::Named(_, inner) | ValueForm::Package(ConsN(_, inner)) => {
                self.project(inner.clone(), product, position, source, bindings)
            }
            | ValueForm::Product(fields) => Ok(fields[position].clone()),
            | ValueForm::Runtime(_) => {
                let product = product.unroll_k(self.tycker)?;
                let Some(Type::Prod(Prod(fields))) =
                    self.tycker.statics.normalized_at(product).cloned()
                else {
                    unreachable!("projection has a product receiver")
                };
                if self.purpose == Purpose::Inspect {
                    return Ok(StaticValue::with_form(
                        source,
                        fields[position],
                        ValueForm::Runtime(source),
                    ));
                }
                let mut patterns = Vec::new();
                let mut selected = None;
                for (index, ty) in fields.into_iter().enumerate() {
                    if index == position {
                        let (pattern, value) = self.variable(Some(source), ty);
                        patterns.push(pattern);
                        selected = Some(value);
                    } else {
                        patterns.push(Alloc::alloc(
                            self.tycker,
                            ValuePattern::Hole(Hole),
                            ty,
                            &TyEnv::default(),
                        ));
                    }
                }
                let binder = Alloc::alloc(
                    self.tycker,
                    ValuePattern::VCons(patterns),
                    product,
                    &TyEnv::default(),
                );
                let bindee = self.reify(&value)?;
                bindings.push(Binding { binder, bindee });
                Ok(selected.unwrap())
            }
            | _ => unreachable!("projection has a product receiver"),
        }
    }

    fn bind(
        &mut self, pattern: VPatId, value: StaticValue, env: &mut Environment,
        bindings: &mut Vec<Binding>,
    ) -> ResultKont<()> {
        match self.tycker.statics.vpats[&pattern].clone() {
            | ValuePattern::Hole(_) | ValuePattern::Triv(_) => {}
            | ValuePattern::Var(definition) => env.values += [(definition, value)],
            | ValuePattern::Named(Named(name, inner)) => {
                let payload = self.open_named(pattern, name, inner, value, env, bindings)?;
                self.bind(inner, payload, env, bindings)?;
            }
            | ValuePattern::Alias(Alias(patterns)) => {
                for pattern in patterns {
                    self.bind(pattern, value.clone(), env, bindings)?;
                }
            }
            | ValuePattern::VCons(patterns) => {
                let product = self.ty(self.tycker.statics.annotations_vpat[&pattern], env)?;
                for (position, pattern) in patterns.into_iter().enumerate() {
                    let field =
                        self.project(value.clone(), product, position, value.0.source, bindings)?;
                    self.bind(pattern, field, env, bindings)?;
                }
            }
            | ValuePattern::SCons(ConsN(prefix, tail)) => {
                let payload = self.open_package(pattern, prefix, tail, value, env, bindings)?;
                self.bind(tail, payload, env, bindings)?;
            }
            | ValuePattern::View(view) => {
                let viewed = self.view(view.function, value, env, bindings)?;
                self.bind(view.pattern, viewed, env, bindings)?;
            }
            | ValuePattern::Ctor(Ctor(name, tail)) => {
                let value = value.unnamed();
                if let ValueForm::Constructor(found, payload) = &value.0.form {
                    assert_eq!(&name, found, "an irrefutable constructor binding must match");
                    self.bind(tail, payload.clone(), env, bindings)?;
                } else {
                    let ty = self.ty(self.tycker.statics.annotations_vpat[&tail], env)?;
                    if self.purpose == Purpose::Inspect {
                        let payload = StaticValue::with_form(
                            value.0.source,
                            ty,
                            ValueForm::Runtime(value.0.source),
                        );
                        return self.bind(tail, payload, env, bindings);
                    }
                    let (payload_pattern, payload) = self.variable(Some(value.0.source), ty);
                    let binder = self.alloc_pattern(
                        pattern,
                        ValuePattern::Ctor(Ctor(name, payload_pattern)),
                        value.0.ty,
                    );
                    let bindee = self.reify(&value)?;
                    bindings.push(Binding { binder, bindee });
                    self.bind(tail, payload, env, bindings)?;
                }
            }
            | ValuePattern::Lit(_) => unreachable!("literal patterns require a match"),
        }
        Ok(())
    }

    fn view(
        &mut self, source: ValueId, value: StaticValue, env: &Environment,
        bindings: &mut Vec<Binding>,
    ) -> ResultKont<StaticValue> {
        let function = self.value(source, env, bindings)?.unnamed();
        let ValueForm::Function { binder: ValBinder::Value(parameter), body, env: closure } =
            &function.0.form
        else {
            return self.fail(StaticEliminationError::UnresolvedApplication { function: source });
        };
        self.enter_reduction(source)?;
        let result = (|| {
            let mut local = closure.clone();
            let value = self.share(value, bindings)?;
            self.bind(*parameter, value, &mut local, bindings)?;
            self.value(*body, &local, bindings)
        })();
        self.reduction_depth -= 1;
        result
    }

    fn open_named(
        &mut self, pattern: VPatId, name: FieldName, tail: VPatId, value: StaticValue,
        env: &Environment, bindings: &mut Vec<Binding>,
    ) -> ResultKont<StaticValue> {
        if let ValueForm::Named(_, payload) = &value.0.form {
            return Ok(payload.clone());
        }
        let ty = self.ty(self.tycker.statics.annotations_vpat[&tail], env)?;
        if self.purpose == Purpose::Inspect {
            return Ok(StaticValue::with_form(
                value.0.source,
                ty,
                ValueForm::Runtime(value.0.source),
            ));
        }
        let (payload_pattern, payload) = self.variable(Some(value.0.source), ty);
        let binder = self.alloc_pattern(
            pattern,
            ValuePattern::Named(Named(name, payload_pattern)),
            value.0.ty,
        );
        let bindee = self.reify(&value)?;
        bindings.push(Binding { binder, bindee });
        Ok(payload)
    }

    fn open_package(
        &mut self, pattern: VPatId, prefix: Vec<StaticPatId>, tail: VPatId, value: StaticValue,
        env: &mut Environment, bindings: &mut Vec<Binding>,
    ) -> ResultKont<StaticValue> {
        let value = value.unnamed();
        match &value.0.form {
            | ValueForm::Package(ConsN(witnesses, payload)) => {
                self.bind_witnesses(pattern, &prefix, witnesses, env)?;
                if self.purpose == Purpose::Inspect
                    && matches!(payload.0.form, ValueForm::Runtime(_))
                {
                    let ty = self.ty(self.tycker.statics.annotations_vpat[&tail], env)?;
                    return Ok(StaticValue::with_form(
                        payload.0.source,
                        ty,
                        payload.0.form.clone(),
                    ));
                }
                Ok(payload.clone())
            }
            | ValueForm::Runtime(_) => {
                let ty = self.ty(self.tycker.statics.annotations_vpat[&tail], env)?;
                if self.purpose == Purpose::Inspect {
                    return Ok(StaticValue::with_form(value.0.source, ty, value.0.form.clone()));
                }
                let (payload_pattern, payload) = self.variable(Some(value.0.source), ty);
                let binder = self.alloc_pattern(
                    pattern,
                    ValuePattern::SCons(ConsN(prefix, payload_pattern)),
                    value.0.ty,
                );
                let bindee = self.reify(&value)?;
                bindings.push(Binding { binder, bindee });
                Ok(payload)
            }
            | _ => unreachable!("package pattern has a package value"),
        }
    }

    fn pattern_witness(&self, pattern: TPatId) -> Option<AbstId> {
        match self.tycker.statics.tpats[&pattern] {
            | TypePattern::Named(Named(_, tail)) => self.pattern_witness(tail),
            | TypePattern::Var(definition) => {
                let AnnId::Type(ty) = self.tycker.statics.annotations_var[&definition] else {
                    return None;
                };
                match self.tycker.statics.normalized_at(ty) {
                    | Some(Type::Abst(witness)) => Some(*witness),
                    | _ => None,
                }
            }
            | TypePattern::Hole(_) => None,
        }
    }

    fn bind_witnesses(
        &mut self, pattern: VPatId, prefix: &[StaticPatId], witnesses: &[StaticTermId],
        env: &mut Environment,
    ) -> ResultKont<()> {
        let mut domain = self.tycker.statics.annotations_vpat[&pattern].unroll_k(self.tycker)?;
        let mut assignments = Vec::new();
        for (pattern, argument) in prefix.iter().zip(witnesses.iter()) {
            match self.tycker.statics.normalized_at(domain).cloned() {
                | Some(Type::Exists(exists)) => {
                    let StaticTermId::Type(argument) = argument else { unreachable!() };
                    assignments.push((exists.binder.witness, *argument));
                    if let StaticPatId::Type(pattern) = pattern
                        && let Some(witness) = self.pattern_witness(*pattern)
                    {
                        assignments.push((witness, *argument));
                    }
                    domain = exists.body;
                }
                | Some(Type::ManifestKind(manifest)) => domain = manifest.body,
                | _ => unreachable!("package witness prefix follows its signature"),
            }
        }
        *env = env.with_types(assignments);
        Ok(())
    }

    fn computation(&mut self, source: CompuId, env: &Environment) -> ResultKont<CompuId> {
        let residual = self.computation_inner(source, env)?;
        let ty = self.tycker.statics.annotations_compu[&residual];
        if !self.runtime_type(ty) {
            return self
                .fail(StaticEliminationError::RuntimeComputation { computation: source, ty });
        }
        // Destructor heads can be arbitrary computations, including lets
        // whose source root disappears during static reduction.
        if let Some(hint) = self.tycker.statics.codata_hints.get(&source).copied() {
            let _ = self.tycker.statics.codata_hints.upsert(residual, hint);
        }
        Ok(residual)
    }

    fn computation_inner(&mut self, source: CompuId, env: &Environment) -> ResultKont<CompuId> {
        let ty = self.ty(self.tycker.statics.annotations_compu[&source], env)?;
        let mut bindings = Vec::new();
        let node = match self.tycker.statics.compus[&source].clone() {
            | Computation::Hole(_) => Computation::Hole(Hole),
            | Computation::Let(Let { binder, bindee, tail }) => {
                let value = self.value(bindee, env, &mut bindings)?;
                let value = self.share(value, &mut bindings)?;
                let tail = self.match_arms(value, &[Matcher { binder, tail }], env, source)?;
                return Ok(self.compu_bindings(bindings, tail));
            }
            | Computation::VAbs(Abs(pattern, body)) => {
                let domain = self.ty(self.tycker.statics.annotations_vpat[&pattern], env)?;
                if !self.runtime_type(domain) {
                    return self.fail(StaticEliminationError::RuntimeComputation {
                        computation: source,
                        ty: domain,
                    });
                }
                let (binder, value) = self.variable(None, domain);
                let body =
                    self.match_arms(value, &[Matcher { binder: pattern, tail: body }], env, body)?;
                let body = self.compu_bindings(bindings, body);
                return Ok(self.alloc_compu(source, Computation::VAbs(Abs(binder, body)), ty));
            }
            | Computation::TAbs(Abs(pattern, body)) => {
                Computation::TAbs(Abs(pattern, self.computation(body, env)?))
            }
            | Computation::TApp(App(function, argument)) => {
                let function = self.computation(function, env)?;
                Computation::TApp(App(function, self.ty(argument, env)?))
            }
            | Computation::VApp(App(function, argument)) => {
                let argument = self.value(argument, env, &mut bindings)?;
                let argument = self.reify(&argument)?;
                Computation::VApp(App(self.computation(function, env)?, argument))
            }
            | Computation::Ret(Return(value)) => {
                let value = self.value(value, env, &mut bindings)?;
                Computation::Ret(Return(self.reify(&value)?))
            }
            | Computation::Force(Force(value)) => {
                let value = self.value(value, env, &mut bindings)?;
                Computation::Force(Force(self.reify(&value)?))
            }
            | Computation::Do(Bind { binder: pattern, bindee, tail }) => {
                let bindee = self.computation(bindee, env)?;
                let domain = self.ty(self.tycker.statics.annotations_vpat[&pattern], env)?;
                let (binder, value) = self.variable(None, domain);
                let tail =
                    self.match_arms(value, &[Matcher { binder: pattern, tail }], env, tail)?;
                let tail = self.compu_bindings(bindings, tail);
                return Ok(self.alloc_compu(
                    source,
                    Computation::Do(Bind { binder, bindee, tail }),
                    ty,
                ));
            }
            | Computation::Fix(Fix(pattern, body)) => {
                let domain = self.ty(self.tycker.statics.annotations_vpat[&pattern], env)?;
                let (binder, value) = self.variable(None, domain);
                let mut local = env.clone();
                self.bind(pattern, value, &mut local, &mut bindings)?;
                let body = self.computation(body, &local)?;
                let body = self.compu_bindings(bindings, body);
                return Ok(self.alloc_compu(source, Computation::Fix(Fix(binder, body)), ty));
            }
            | Computation::CoMatch(CoMatch { arms }) => {
                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        Ok(CoMatcher { dtor: arm.dtor, tail: self.computation(arm.tail, env)? })
                    })
                    .collect::<ResultKont<_>>()?;
                Computation::CoMatch(CoMatch { arms })
            }
            | Computation::Dtor(Dtor(function, name)) => {
                Computation::Dtor(Dtor(self.computation(function, env)?, name))
            }
            | Computation::Match(Match { scrut, arms }) => {
                let value = self.value(scrut, env, &mut bindings)?;
                let value = self.share(value, &mut bindings)?;
                // Keep ordinary runtime case analysis as one multiway match.
                // Expanding each row into a complete tag test duplicates the
                // remaining rows once per alternative during backend lowering.
                if matches!(value.0.form, ValueForm::Runtime(_))
                    && arms.iter().all(|arm| !self.has_view(arm.binder))
                {
                    let scrut = self.reify(&value)?;
                    let arms = arms
                        .into_iter()
                        .map(|arm| {
                            let mut local = env.clone();
                            let binder = self.runtime_pattern(arm.binder, &mut local)?;
                            Ok(Matcher { binder, tail: self.computation(arm.tail, &local)? })
                        })
                        .collect::<ResultKont<_>>()?;
                    let body =
                        self.alloc_compu(source, Computation::Match(Match { scrut, arms }), ty);
                    return Ok(self.compu_bindings(bindings, body));
                }
                let body = self.match_arms(value, &arms, env, source)?;
                return Ok(self.compu_bindings(bindings, body));
            }
        };
        let body = self.alloc_compu(source, node, ty);
        Ok(self.compu_bindings(bindings, body))
    }

    fn has_view(&self, pattern: VPatId) -> bool {
        match &self.tycker.statics.vpats[&pattern] {
            | ValuePattern::View(_) => true,
            | ValuePattern::Named(Named(_, inner))
            | ValuePattern::Ctor(Ctor(_, inner))
            | ValuePattern::SCons(ConsN(_, inner)) => self.has_view(*inner),
            | ValuePattern::Alias(Alias(patterns)) => {
                patterns.iter().any(|pattern| self.has_view(*pattern))
            }
            | ValuePattern::VCons(patterns) => {
                patterns.iter().any(|pattern| self.has_view(*pattern))
            }
            | _ => false,
        }
    }

    fn runtime_pattern(&mut self, pattern: VPatId, env: &mut Environment) -> ResultKont<VPatId> {
        let ty = self.ty(self.tycker.statics.annotations_vpat[&pattern], env)?;
        let node = match self.tycker.statics.vpats[&pattern].clone() {
            | ValuePattern::Var(definition) => {
                let (pattern, value) = self.variable(None, ty);
                env.values += [(definition, value)];
                return Ok(pattern);
            }
            | ValuePattern::Named(Named(name, inner)) => {
                ValuePattern::Named(Named(name, self.runtime_pattern(inner, env)?))
            }
            | ValuePattern::Ctor(Ctor(name, inner)) => {
                ValuePattern::Ctor(Ctor(name, self.runtime_pattern(inner, env)?))
            }
            | ValuePattern::SCons(ConsN(prefix, inner)) => {
                ValuePattern::SCons(ConsN(prefix, self.runtime_pattern(inner, env)?))
            }
            | ValuePattern::VCons(patterns) => ValuePattern::VCons(
                patterns
                    .into_iter()
                    .map(|pattern| self.runtime_pattern(pattern, env))
                    .collect::<ResultKont<_>>()?,
            ),
            | ValuePattern::Alias(Alias(patterns)) => {
                let patterns = patterns
                    .into_iter()
                    .map(|pattern| self.runtime_pattern(pattern, env))
                    .collect::<ResultKont<Vec<_>>>()?;
                ValuePattern::Alias(Alias(ConsN::from_vec(patterns).unwrap()))
            }
            | ValuePattern::View(_) => unreachable!("views require static pattern elaboration"),
            | node => node,
        };
        Ok(self.alloc_pattern(pattern, node, ty))
    }

    fn match_arms(
        &mut self, value: StaticValue, arms: &[Matcher<VPatId, CompuId>], env: &Environment,
        source: CompuId,
    ) -> ResultKont<CompuId> {
        let Some((first, rest)) = arms.split_first() else {
            let ty = self.ty(self.tycker.statics.annotations_compu[&source], env)?;
            let scrut = self.reify(&value)?;
            return Ok(self.alloc_compu(
                source,
                Computation::Match(Match { scrut, arms: Vec::new() }),
                ty,
            ));
        };
        self.match_steps(
            vec![(first.binder, value.clone())],
            first.tail,
            env.clone(),
            &MatchFailure { scrutinee: value, arms: rest, env: env.clone(), source },
        )
    }

    fn match_steps(
        &mut self, mut pending: Vec<(VPatId, StaticValue)>, tail: CompuId, mut env: Environment,
        failure: &MatchFailure<'_>,
    ) -> ResultKont<CompuId> {
        let mut bindings = Vec::new();
        while let Some((pattern, value)) = pending.pop() {
            match self.tycker.statics.vpats[&pattern].clone() {
                | ValuePattern::Alias(Alias(patterns)) => {
                    pending
                        .extend(patterns.into_iter().rev().map(|pattern| (pattern, value.clone())));
                }
                | ValuePattern::Named(Named(name, inner)) => {
                    let payload =
                        self.open_named(pattern, name, inner, value, &env, &mut bindings)?;
                    pending.push((inner, payload));
                }
                | ValuePattern::SCons(ConsN(prefix, inner)) => {
                    let payload =
                        self.open_package(pattern, prefix, inner, value, &mut env, &mut bindings)?;
                    pending.push((inner, payload));
                }
                | ValuePattern::VCons(patterns) => {
                    let product = self.ty(self.tycker.statics.annotations_vpat[&pattern], &env)?;
                    let values = patterns
                        .into_iter()
                        .enumerate()
                        .map(|(position, pattern)| {
                            Ok((
                                pattern,
                                self.project(
                                    value.clone(),
                                    product,
                                    position,
                                    value.0.source,
                                    &mut bindings,
                                )?,
                            ))
                        })
                        .collect::<ResultKont<Vec<_>>>()?;
                    pending.extend(values.into_iter().rev());
                }
                | ValuePattern::Ctor(Ctor(name, payload)) => {
                    if let ValueForm::Constructor(found, inner) = &value.0.form {
                        if &name != found {
                            let body = self.match_arms(
                                failure.scrutinee.clone(),
                                failure.arms,
                                &failure.env,
                                failure.source,
                            )?;
                            return Ok(self.compu_bindings(bindings, body));
                        }
                        pending.push((payload, inner.clone()));
                    } else {
                        let payload_ty =
                            self.ty(self.tycker.statics.annotations_vpat[&payload], &env)?;
                        let (payload_pattern, inner) =
                            self.variable(Some(value.0.source), payload_ty);
                        pending.push((payload, inner));
                        let success = self.match_steps(pending, tail, env, failure)?;
                        let fallback = self.match_arms(
                            failure.scrutinee.clone(),
                            failure.arms,
                            &failure.env,
                            failure.source,
                        )?;
                        let scrut = self.reify(&value)?;
                        let ty = self.tycker.statics.annotations_compu[&success];
                        let data = self.tycker.statics.data_pat_hints[&pattern];
                        let constructors = self.tycker.statics.datas[&data].clone();
                        // Several tags take the same failure continuation.
                        // Name it once so lowering does not duplicate the
                        // remaining pattern rows for every alternative tag.
                        let fallback = if constructors.iter().count() > 2 {
                            let thunk_ty = self.tycker.thk_arg(&TyEnv::default(), ty);
                            let thunk = self.alloc_value(
                                value.0.source,
                                Value::Thunk(Thunk(fallback)),
                                thunk_ty,
                            );
                            let (binder, continuation) = self.variable(None, thunk_ty);
                            bindings.push(Binding { binder, bindee: thunk });
                            let continuation = self.reify(&continuation)?;
                            self.alloc_compu(
                                failure.source,
                                Computation::Force(Force(continuation)),
                                ty,
                            )
                        } else {
                            fallback
                        };
                        let arms = constructors
                            .iter()
                            .map(|(candidate, payload_ty)| {
                                let (payload, tail) = if candidate == &name {
                                    (payload_pattern, success)
                                } else {
                                    (
                                        self.alloc_pattern(
                                            payload,
                                            ValuePattern::Hole(Hole),
                                            *payload_ty,
                                        ),
                                        fallback,
                                    )
                                };
                                let binder = self.alloc_pattern(
                                    pattern,
                                    ValuePattern::Ctor(Ctor(candidate.clone(), payload)),
                                    value.0.ty,
                                );
                                Matcher { binder, tail }
                            })
                            .collect();
                        let body = self.alloc_compu(
                            failure.source,
                            Computation::Match(Match { scrut, arms }),
                            ty,
                        );
                        return Ok(self.compu_bindings(bindings, body));
                    }
                }
                | ValuePattern::Lit(literal) => {
                    let scrut = self.reify(&value)?;
                    let success = self.match_steps(pending, tail, env, failure)?;
                    let fallback = self.match_arms(
                        failure.scrutinee.clone(),
                        failure.arms,
                        &failure.env,
                        failure.source,
                    )?;
                    let binder =
                        self.alloc_pattern(pattern, ValuePattern::Lit(literal), value.0.ty);
                    let hole = self.alloc_pattern(pattern, ValuePattern::Hole(Hole), value.0.ty);
                    let ty = self.tycker.statics.annotations_compu[&success];
                    let body = self.alloc_compu(
                        failure.source,
                        Computation::Match(Match {
                            scrut,
                            arms: vec![
                                Matcher { binder, tail: success },
                                Matcher { binder: hole, tail: fallback },
                            ],
                        }),
                        ty,
                    );
                    return Ok(self.compu_bindings(bindings, body));
                }
                | ValuePattern::View(view) => {
                    let viewed = self.view(view.function, value, &env, &mut bindings)?;
                    pending.push((view.pattern, viewed));
                }
                | _ => self.bind(pattern, value, &mut env, &mut bindings)?,
            }
        }
        let body = self.computation(tail, &env)?;
        Ok(self.compu_bindings(bindings, body))
    }
}
