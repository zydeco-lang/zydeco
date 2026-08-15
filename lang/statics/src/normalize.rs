use super::{syntax::*, *};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use zydeco_utils::arena::ArenaAccess;

/* ------------------------- Existential type scope ------------------------- */

impl TypeId {
    /// Require this type to be well scoped under `scope`.
    ///
    /// Unsolved inference holes inherit the requirement by narrowing the
    /// witnesses their eventual solutions may mention. Solved holes are
    /// checked immediately under that narrowed scope.
    #[track_caller]
    pub fn constrain_to_scope(&self, tycker: &mut Tycker<'_>, scope: &SkolemScope) -> Result<()> {
        let support = TypeSupport::of(*self, tycker)?;
        let mut witnesses = support
            .skolems
            .into_iter()
            .filter(|skolem| !scope.contains(skolem))
            .collect::<Vec<_>>();
        witnesses.sort_unstable();
        if !witnesses.is_empty() {
            return tycker.err(
                TyckError::EscapingExistential { witnesses, result: *self },
                std::panic::Location::caller(),
            );
        }

        let constraints = support
            .fills
            .into_iter()
            .map(|(fill, locally_bound)| {
                let admissible = scope.union(&locally_bound);
                let scope = tycker
                    .statics
                    .fill_scopes
                    .get(&fill)
                    .map(|current| current.intersection(&admissible))
                    .unwrap_or(admissible);
                (fill, scope)
            })
            .collect::<Vec<_>>();
        constraints.into_iter().for_each(|(fill, scope)| {
            let _ = tycker.statics.fill_scopes.upsert(fill, scope);
        });
        Ok(())
    }

    #[track_caller]
    pub fn constrain_to_scope_k(
        &self, tycker: &mut Tycker<'_>, scope: &SkolemScope,
    ) -> ResultKont<()> {
        let result = self.constrain_to_scope(tycker, scope);
        tycker.err_p_to_k(result)
    }
}

#[derive(Default)]
struct TypeSupport {
    skolems: HashSet<AbstId>,
    /// Package witnesses bound at every occurrence of an inference hole.
    ///
    /// A shared hole may occur under different package binders, so the
    /// admissible local scope is the intersection of those occurrences.
    fills: HashMap<FillId, SkolemScope>,
}

impl TypeSupport {
    fn of(root: TypeId, tycker: &Tycker<'_>) -> Result<Self> {
        let mut collector = TypeSupportCollector::new();
        collector.visit(root, tycker)?;
        Ok(collector.support)
    }
}

struct TypeSupportCollector {
    support: TypeSupport,
    bound: HashSet<AbstId>,
    pack_scope: SkolemScope,
    visiting_fills: HashSet<FillId>,
    visiting_vars: HashSet<(DefId, TypeId)>,
    visiting_datas: HashSet<DataId>,
    visiting_codatas: HashSet<CoDataId>,
}

struct InferenceOccurs {
    needle: FillId,
    visiting_kinds: HashSet<KindId>,
    visiting_fills: HashSet<FillId>,
}

impl InferenceOccurs {
    fn new(needle: FillId) -> Self {
        Self { needle, visiting_kinds: HashSet::default(), visiting_fills: HashSet::default() }
    }

    fn in_annotation(&mut self, annotation: AnnId, tycker: &mut Tycker<'_>) -> Result<bool> {
        match annotation {
            | AnnId::Set => Ok(false),
            | AnnId::Kind(kind) => self.in_kind(kind, tycker),
            | AnnId::Type(ty) => Ok(TypeSupport::of(ty, tycker)?.fills.contains_key(&self.needle)),
        }
    }

    fn in_kind(&mut self, kind: KindId, tycker: &mut Tycker<'_>) -> Result<bool> {
        if !self.visiting_kinds.insert(kind) {
            return Ok(false);
        }
        let occurs = match tycker.statics.kinds_pre[&kind].to_owned() {
            | Fillable::Fill(fill) if fill == self.needle => true,
            | Fillable::Fill(fill) if self.visiting_fills.insert(fill) => {
                let occurs = match tycker.statics.solus.get(&fill).copied() {
                    | Some(AnnId::Kind(solution)) => self.in_kind(solution, tycker)?,
                    | Some(AnnId::Set | AnnId::Type(_)) => {
                        return tycker.err(TyckError::SortMismatch, std::panic::Location::caller());
                    }
                    | None => false,
                };
                self.visiting_fills.remove(&fill);
                occurs
            }
            | Fillable::Fill(_) | Fillable::Done(Kind::VType(_) | Kind::CType(_)) => false,
            | Fillable::Done(Kind::Arrow(Arrow(domain, codomain))) => {
                self.in_kind(domain, tycker)? || self.in_kind(codomain, tycker)?
            }
            | Fillable::Done(Kind::Label(Label(_, payload))) => self.in_kind(payload, tycker)?,
        };
        self.visiting_kinds.remove(&kind);
        Ok(occurs)
    }
}

impl TypeSupportCollector {
    fn new() -> Self {
        Self {
            support: TypeSupport::default(),
            bound: HashSet::default(),
            pack_scope: SkolemScope::default(),
            visiting_fills: HashSet::default(),
            visiting_vars: HashSet::default(),
            visiting_datas: HashSet::default(),
            visiting_codatas: HashSet::default(),
        }
    }

    fn visit(&mut self, id: TypeId, tycker: &Tycker<'_>) -> Result<()> {
        match tycker.statics.types_pre[&id].to_owned() {
            | Fillable::Fill(fill) => {
                let local_scope = self.pack_scope.clone();
                self.support
                    .fills
                    .entry(fill)
                    .and_modify(|scope| *scope = scope.intersection(&local_scope))
                    .or_insert(local_scope);
                let solution = tycker.statics.solus.get(&fill).cloned();
                if self.visiting_fills.insert(fill) {
                    let result = match solution {
                        | Some(AnnId::Type(solution)) => self.visit(solution, tycker),
                        | Some(AnnId::Set | AnnId::Kind(_)) => {
                            tycker.err(TyckError::SortMismatch, std::panic::Location::caller())
                        }
                        | None => Ok(()),
                    };
                    self.visiting_fills.remove(&fill);
                    result?;
                }
            }
            | Fillable::Done(ty) => match ty {
                | Type::Var(def) => {
                    let target = tycker.statics.env_type[&id].get(&def).copied();
                    if let Some(AnnId::Type(target)) = target {
                        let key = (def, target);
                        if self.visiting_vars.insert(key) {
                            let result = self.visit(target, tycker);
                            self.visiting_vars.remove(&key);
                            result?;
                        }
                    }
                }
                | Type::Abst(abst) => {
                    if tycker.statics.existential_skolems.get(&abst).is_some()
                        && !self.bound.contains(&abst)
                    {
                        self.support.skolems.insert(abst);
                    }
                }
                | Type::Abs(Abs(_, body)) => self.visit(body, tycker)?,
                | Type::App(App(function, argument)) => {
                    [function, argument].into_iter().try_for_each(|ty| self.visit(ty, tycker))?;
                }
                | Type::Named(Named(_, inner)) | Type::Label(Label(_, inner)) => {
                    self.visit(inner, tycker)?
                }
                | Type::Proj(Proj(head, _)) => self.visit(head, tycker)?,
                | Type::Thk(_)
                | Type::Ret(_)
                | Type::Unit(_)
                | Type::Opaque(_)
                | Type::Primitive(_)
                | Type::OS(_) => {}
                | Type::VArrow(ValueArrow(input, output))
                | Type::Arrow(Arrow(input, output))
                | Type::Prod(Prod(input, output)) => {
                    [input, output].into_iter().try_for_each(|ty| self.visit(ty, tycker))?;
                }
                | Type::VForall(ValueForall(binder, body)) | Type::Forall(Forall(binder, body)) => {
                    let newly_bound = self.bound.insert(binder.witness);
                    let result = self.visit(body, tycker);
                    if newly_bound {
                        self.bound.remove(&binder.witness);
                    }
                    result?;
                }
                | Type::Exists(exists) => {
                    let Exists { binder, mode, body } = *exists;
                    if let ExistsMode::Manifest(definition) = mode {
                        self.visit(definition, tycker)?;
                    }
                    let newly_bound = self.bound.insert(binder.witness);
                    let result = self.visit(body, tycker);
                    if newly_bound {
                        self.bound.remove(&binder.witness);
                    }
                    result?;
                }
                | Type::ManifestKind(ManifestKind { body, .. }) => self.visit(body, tycker)?,
                | Type::VPackPi(pack_pi) => {
                    let ValuePackPi { domain, witnesses, codomain } = *pack_pi;
                    self.visit(domain, tycker)?;
                    let outer_bound = self.bound.clone();
                    let outer_pack_scope = self.pack_scope.clone();
                    self.bound.extend(witnesses.iter().copied());
                    self.pack_scope = self.pack_scope.union(&witnesses.iter().copied().collect());
                    let result = self.visit(codomain, tycker);
                    self.bound = outer_bound;
                    self.pack_scope = outer_pack_scope;
                    result?;
                }
                | Type::PackPi(pack_pi) => {
                    let PackPi { domain, witnesses, codomain } = *pack_pi;
                    self.visit(domain, tycker)?;
                    let outer_bound = self.bound.clone();
                    let outer_pack_scope = self.pack_scope.clone();
                    self.bound.extend(witnesses.iter().copied());
                    self.pack_scope = self.pack_scope.union(&witnesses.iter().copied().collect());
                    let result = self.visit(codomain, tycker);
                    self.bound = outer_bound;
                    self.pack_scope = outer_pack_scope;
                    result?;
                }
                | Type::Data(data) => {
                    if self.visiting_datas.insert(data) {
                        let arms = tycker.statics.datas[&data].clone();
                        let result =
                            arms.into_iter().try_for_each(|(_, arm)| self.visit(arm, tycker));
                        self.visiting_datas.remove(&data);
                        result?;
                    }
                }
                | Type::CoData(codata) => {
                    if self.visiting_codatas.insert(codata) {
                        let arms = tycker.statics.codatas[&codata].clone();
                        let result =
                            arms.into_iter().try_for_each(|(_, arm)| self.visit(arm, tycker));
                        self.visiting_codatas.remove(&codata);
                        result?;
                    }
                }
            },
        }
        Ok(())
    }
}

/* ------------------------------ Substitution ------------------------------ */

impl TypeId {
    pub fn subst_env_k(&self, tycker: &mut Tycker<'_>, env: &TyEnv) -> ResultKont<TypeId> {
        let res = self.subst_env(tycker, env);
        tycker.err_p_to_k(res)
    }
    pub fn subst_env(&self, tycker: &mut Tycker<'_>, env: &TyEnv) -> Result<TypeId> {
        let kd = tycker.statics.annotations_type[self];
        let ty = tycker.statics.types_pre[self].to_owned();
        let ty = match ty {
            // Fixme: should invoke substitution once the type is filled
            | Fillable::Fill(_) => *self,
            | Fillable::Done(ty) => match ty {
                | Type::Var(def) => match env.get(&def) {
                    | Some(ann) => match ann {
                        | AnnId::Set | AnnId::Kind(_) => {
                            tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                        | AnnId::Type(with) => *with,
                    },
                    | None => *self,
                },
                | Type::Abst(_) => *self,
                | Type::Abs(abs) => {
                    let Abs(tpat, ty) = abs;
                    let (def, _) = tpat.try_destruct_def(tycker);
                    if let Some(def) = def
                        && let Some(_with) = env.get(&def)
                    {
                        unreachable!()
                    }
                    let ty_ = ty.subst_env(tycker, env)?;
                    if ty == ty_ { *self } else { Alloc::alloc(tycker, Abs(tpat, ty_), kd, env) }
                }
                | Type::App(app) => {
                    let App(ty1, ty2) = app;
                    let ty1_ = ty1.subst_env(tycker, env)?;
                    let ty2_ = ty2.subst_env(tycker, env)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, App(ty1_, ty2_), kd, env)
                    }
                }
                | Type::Named(named) => {
                    let Named(name, inner) = named;
                    let inner_ = inner.subst_env(tycker, env)?;
                    if inner == inner_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Named(name, inner_), kd, env)
                    }
                }
                | Type::Label(label) => {
                    let Label(name, inner) = label;
                    let inner_ = inner.subst_env(tycker, env)?;
                    if inner == inner_ {
                        *self
                    } else {
                        let target = Alloc::alloc(tycker, Label(name, inner_), kd, env);
                        tycker
                            .statics
                            .builtin_roles
                            .transfer_value(*self, target)
                            .expect("a fresh substituted label cannot have a conflicting role");
                        target
                    }
                }
                | Type::Proj(proj) => {
                    let Proj(head, name) = proj;
                    let head_ = head.subst_env(tycker, env)?;
                    if head == head_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Proj(head_, name), kd, env)
                    }
                }
                | Type::Thk(_)
                | Type::Ret(_)
                | Type::Unit(_)
                | Type::Opaque(_)
                | Type::Primitive(_)
                | Type::OS(_) => *self,
                | Type::Arrow(arr) => {
                    let Arrow(ty1, ty2) = arr;
                    let ty1_ = ty1.subst_env(tycker, env)?;
                    let ty2_ = ty2.subst_env(tycker, env)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Arrow(ty1_, ty2_), kd, env)
                    }
                }
                | Type::VArrow(ValueArrow(ty1, ty2)) => {
                    let ty1_ = ty1.subst_env(tycker, env)?;
                    let ty2_ = ty2.subst_env(tycker, env)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, ValueArrow(ty1_, ty2_), kd, env)
                    }
                }
                | Type::VForall(forall) => {
                    let ValueForall(tpat, ty) = forall;
                    let ty_ = ty.subst_env(tycker, env)?;
                    if ty == ty_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, ValueForall(tpat, ty_), kd, env)
                    }
                }
                | Type::VPackPi(pack_pi) => {
                    let ValuePackPi { domain, witnesses, codomain } = *pack_pi;
                    let domain_ = domain.subst_env(tycker, env)?;
                    let codomain_ = codomain.subst_env(tycker, env)?;
                    if domain == domain_ && codomain == codomain_ {
                        *self
                    } else {
                        Alloc::alloc(
                            tycker,
                            ValuePackPi { domain: domain_, witnesses, codomain: codomain_ },
                            kd,
                            env,
                        )
                    }
                }
                | Type::Forall(forall) => {
                    let Forall(tpat, ty) = forall;
                    let ty_ = ty.subst_env(tycker, env)?;
                    if ty == ty_ { *self } else { Alloc::alloc(tycker, Forall(tpat, ty_), kd, env) }
                }
                | Type::PackPi(pack_pi) => {
                    let PackPi { domain, witnesses, codomain } = *pack_pi;
                    let domain_ = domain.subst_env(tycker, env)?;
                    let codomain_ = codomain.subst_env(tycker, env)?;
                    if domain == domain_ && codomain == codomain_ {
                        *self
                    } else {
                        Alloc::alloc(
                            tycker,
                            PackPi { domain: domain_, witnesses, codomain: codomain_ },
                            kd,
                            env,
                        )
                    }
                }
                | Type::Prod(prod) => {
                    let Prod(ty1, ty2) = prod;
                    let ty1_ = ty1.subst_env(tycker, env)?;
                    let ty2_ = ty2.subst_env(tycker, env)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Prod(ty1_, ty2_), kd, env)
                    }
                }
                | Type::Exists(exists) => {
                    let Exists { binder, mode, body } = *exists;
                    let (mode, definition_changed) = match mode {
                        | ExistsMode::Abstract => (ExistsMode::Abstract, false),
                        | ExistsMode::Manifest(definition) => {
                            let definition_ = definition.subst_env(tycker, env)?;
                            (ExistsMode::Manifest(definition_), definition != definition_)
                        }
                    };
                    let body_ = body.subst_env(tycker, env)?;
                    if !definition_changed && body == body_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Exists { binder, mode, body: body_ }, kd, env)
                    }
                }
                | Type::ManifestKind(manifest) => {
                    let ManifestKind { binder, definition, body } = manifest;
                    let body_ = body.subst_env(tycker, env)?;
                    if body == body_ {
                        *self
                    } else {
                        Alloc::alloc(
                            tycker,
                            ManifestKind { binder, definition, body: body_ },
                            kd,
                            env,
                        )
                    }
                }
                | Type::Data(id) => {
                    let arms = tycker.statics.datas[&id].clone();
                    // let mut unchanged = true;
                    let arms_ = arms
                        .into_iter()
                        .map(|(ctor, ty)| {
                            let ty_ = ty.subst_env(tycker, env)?;
                            // if ty == ty_ {
                            //     Ok((ctor, ty))
                            // } else {
                            //     unchanged = false;
                            //     Ok((ctor, ty_))
                            // }
                            Ok((ctor, ty_))
                        })
                        .collect::<Result<im::Vector<_>>>()?;
                    // if unchanged {
                    //     *self
                    // } else
                    {
                        let id_: DataId = tycker.fresh();
                        tycker.statics.datas.insert_new(id_, Data::new(arms_));
                        Alloc::alloc(tycker, id_, kd, env)
                    }
                }
                | Type::CoData(id) => {
                    let arms = tycker.statics.codatas[&id].clone();
                    // let mut unchanged = true;
                    let arms_ = arms
                        .into_iter()
                        .map(|(dtor, ty)| {
                            let ty_ = ty.subst_env(tycker, env)?;
                            // if ty == ty_ {
                            //     Ok((dtor, ty))
                            // } else {
                            //     unchanged = false;
                            //     Ok((dtor, ty_))
                            // }
                            Ok((dtor, ty_))
                        })
                        .collect::<Result<im::Vector<_>>>()?;
                    // if unchanged {
                    //     *self
                    // } else
                    {
                        let id_: CoDataId = tycker.fresh();
                        tycker.statics.codatas.insert_new(id_, CoData::new(arms_));
                        Alloc::alloc(tycker, id_, kd, env)
                    }
                }
            },
        };
        let kd = tycker.statics.annotations_type[&ty];
        let ty = ty.normalize(tycker, kd)?;
        Ok(ty)
    }
    pub fn subst_k(&self, tycker: &mut Tycker<'_>, var: DefId, with: TypeId) -> ResultKont<TypeId> {
        let res = self.subst(tycker, var, with);
        tycker.err_p_to_k(res)
    }
    pub fn subst(&self, tycker: &mut Tycker<'_>, var: DefId, with: TypeId) -> Result<TypeId> {
        let scope = tycker.statics.env_type[self].skolem_scope().clone();
        let env = TyEnv::from_iter([(var, with.into())]).with_skolem_scope(scope);
        self.subst_env(tycker, &env)
    }
}

impl TypeId {
    pub fn subst_abst_k(
        &self, tycker: &mut Tycker<'_>, assign: (AbstId, TypeId),
    ) -> ResultKont<TypeId> {
        let res = self.subst_abst(tycker, assign);
        tycker.err_p_to_k(res)
    }
    pub fn subst_abst(&self, tycker: &mut Tycker<'_>, assign: (AbstId, TypeId)) -> Result<TypeId> {
        self.subst_absts(tycker, &[assign])
    }
    /// Apply ordered abstract assignments in one structural traversal.
    ///
    /// A replacement receives only the suffix after its own assignment. Binders such as PackPi
    /// filter their witnesses before the sequence reaches either the body or a replacement.
    pub fn subst_absts_k(
        &self, tycker: &mut Tycker<'_>, assignments: &[(AbstId, TypeId)],
    ) -> ResultKont<TypeId> {
        let res = self.subst_absts(tycker, assignments);
        tycker.err_p_to_k(res)
    }
    pub fn subst_absts(
        &self, tycker: &mut Tycker<'_>, assignments: &[(AbstId, TypeId)],
    ) -> Result<TypeId> {
        if assignments.is_empty() {
            return Ok(*self);
        }
        let kd = tycker.statics.annotations_type[self];
        let env = tycker.statics.env_at(*self);
        let ty = match tycker.statics.types_pre[self].to_owned() {
            // Todo: add subst obligation to fills
            | Fillable::Fill(_) => *self,
            | Fillable::Done(ty) => match ty {
                | Type::Var(_) => *self,
                | Type::Abst(abst) => {
                    match assignments.iter().position(|(witness, _)| *witness == abst) {
                        | Some(position) => assignments[position]
                            .1
                            .subst_absts(tycker, &assignments[position + 1..])?,
                        | None => *self,
                    }
                }
                | Type::Abs(abs) => {
                    let Abs(tpat, ty) = abs;
                    let ty_ = ty.subst_absts(tycker, assignments)?;
                    if ty == ty_ { *self } else { Alloc::alloc(tycker, Abs(tpat, ty_), kd, &env) }
                }
                | Type::App(app) => {
                    let App(ty1, ty2) = app;
                    let ty1_ = ty1.subst_absts(tycker, assignments)?;
                    let ty2_ = ty2.subst_absts(tycker, assignments)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, App(ty1_, ty2_), kd, &env)
                    }
                }
                | Type::Named(named) => {
                    let Named(name, inner) = named;
                    let inner_ = inner.subst_absts(tycker, assignments)?;
                    if inner == inner_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Named(name, inner_), kd, &env)
                    }
                }
                | Type::Label(label) => {
                    let Label(name, inner) = label;
                    let inner_ = inner.subst_absts(tycker, assignments)?;
                    if inner == inner_ {
                        *self
                    } else {
                        let target = Alloc::alloc(tycker, Label(name, inner_), kd, &env);
                        tycker
                            .statics
                            .builtin_roles
                            .transfer_value(*self, target)
                            .expect("a fresh substituted label cannot have a conflicting role");
                        target
                    }
                }
                | Type::Proj(proj) => {
                    let Proj(head, name) = proj;
                    let head_ = head.subst_absts(tycker, assignments)?;
                    match tycker.type_filled(&head_)?.to_owned() {
                        | Type::Named(Named(found, inner)) if found == name => inner,
                        | _ if head == head_ => *self,
                        | _ => Alloc::alloc(tycker, Proj(head_, name), kd, &env),
                    }
                }
                | Type::Thk(_)
                | Type::Ret(_)
                | Type::Unit(_)
                | Type::Opaque(_)
                | Type::Primitive(_)
                | Type::OS(_) => *self,
                | Type::Arrow(arr) => {
                    let Arrow(ty1, ty2) = arr;
                    let ty1_ = ty1.subst_absts(tycker, assignments)?;
                    let ty2_ = ty2.subst_absts(tycker, assignments)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Arrow(ty1_, ty2_), kd, &env)
                    }
                }
                | Type::VArrow(ValueArrow(ty1, ty2)) => {
                    let ty1_ = ty1.subst_absts(tycker, assignments)?;
                    let ty2_ = ty2.subst_absts(tycker, assignments)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, ValueArrow(ty1_, ty2_), kd, &env)
                    }
                }
                | Type::VForall(forall) => {
                    let ValueForall(tpat, ty) = forall;
                    let ty_ = ty.subst_absts(tycker, assignments)?;
                    if ty == ty_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, ValueForall(tpat, ty_), kd, &env)
                    }
                }
                | Type::VPackPi(pack_pi) => {
                    let ValuePackPi { domain, witnesses, codomain } = *pack_pi;
                    let domain_ = domain.subst_absts(tycker, assignments)?;
                    let codomain_assignments = assignments
                        .iter()
                        .filter(|(witness, _)| !witnesses.contains(witness))
                        .copied()
                        .collect::<Vec<_>>();
                    let codomain_ = codomain.subst_absts(tycker, &codomain_assignments)?;
                    if domain == domain_ && codomain == codomain_ {
                        *self
                    } else {
                        Alloc::alloc(
                            tycker,
                            ValuePackPi { domain: domain_, witnesses, codomain: codomain_ },
                            kd,
                            &env,
                        )
                    }
                }
                | Type::Forall(forall) => {
                    let Forall(tpat, ty) = forall;
                    let ty_ = ty.subst_absts(tycker, assignments)?;
                    if ty == ty_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Forall(tpat, ty_), kd, &env)
                    }
                }
                | Type::PackPi(pack_pi) => {
                    let PackPi { domain, witnesses, codomain } = *pack_pi;
                    let domain_ = domain.subst_absts(tycker, assignments)?;
                    let codomain_assignments = assignments
                        .iter()
                        .filter(|(witness, _)| !witnesses.contains(witness))
                        .copied()
                        .collect::<Vec<_>>();
                    let codomain_ = codomain.subst_absts(tycker, &codomain_assignments)?;
                    if domain == domain_ && codomain == codomain_ {
                        *self
                    } else {
                        Alloc::alloc(
                            tycker,
                            PackPi { domain: domain_, witnesses, codomain: codomain_ },
                            kd,
                            &env,
                        )
                    }
                }
                | Type::Prod(prod) => {
                    let Prod(ty1, ty2) = prod;
                    let ty1_ = ty1.subst_absts(tycker, assignments)?;
                    let ty2_ = ty2.subst_absts(tycker, assignments)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Prod(ty1_, ty2_), kd, &env)
                    }
                }
                | Type::Exists(exists) => {
                    let Exists { binder, mode, body } = *exists;
                    let (mode, definition_changed) = match mode {
                        | ExistsMode::Abstract => (ExistsMode::Abstract, false),
                        | ExistsMode::Manifest(definition) => {
                            let definition_ = definition.subst_absts(tycker, assignments)?;
                            (ExistsMode::Manifest(definition_), definition != definition_)
                        }
                    };
                    let body_ = body.subst_absts(tycker, assignments)?;
                    if !definition_changed && body == body_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Exists { binder, mode, body: body_ }, kd, &env)
                    }
                }
                | Type::ManifestKind(manifest) => {
                    let ManifestKind { binder, definition, body } = manifest;
                    let body_ = body.subst_absts(tycker, assignments)?;
                    if body == body_ {
                        *self
                    } else {
                        Alloc::alloc(
                            tycker,
                            ManifestKind { binder, definition, body: body_ },
                            kd,
                            &env,
                        )
                    }
                }
                | Type::Data(id) => {
                    let arms = tycker.statics.datas[&id].clone();
                    let mut unchanged = true;
                    let arms_ = arms
                        .into_iter()
                        .map(|(ctor, ty)| {
                            let ty_ = ty.subst_absts(tycker, assignments)?;
                            if ty == ty_ {
                                Ok((ctor, ty))
                            } else {
                                unchanged = false;
                                Ok((ctor, ty_))
                            }
                        })
                        .collect::<Result<im::Vector<_>>>()?;
                    if unchanged {
                        *self
                    } else {
                        let id_: DataId = tycker.fresh();
                        tycker.statics.datas.insert_new(id_, Data::new(arms_));
                        Alloc::alloc(tycker, id_, kd, &env)
                    }
                }
                | Type::CoData(id) => {
                    let arms = tycker.statics.codatas[&id].clone();
                    let mut unchanged = true;
                    let arms_ = arms
                        .into_iter()
                        .map(|(dtor, ty)| {
                            let ty_ = ty.subst_absts(tycker, assignments)?;
                            if ty == ty_ {
                                Ok((dtor, ty))
                            } else {
                                unchanged = false;
                                Ok((dtor, ty_))
                            }
                        })
                        .collect::<Result<im::Vector<_>>>()?;
                    if unchanged {
                        *self
                    } else {
                        let id_: CoDataId = tycker.fresh();
                        tycker.statics.codatas.insert_new(id_, CoData::new(arms_));
                        Alloc::alloc(tycker, id_, kd, &env)
                    }
                }
            },
        };
        let kd = tycker.statics.annotations_type[&ty];
        let ty = ty.normalize(tycker, kd)?;
        Ok(ty)
    }
}

/* --------------------------- Unroll Sealed Types -------------------------- */

impl TypeId {
    pub fn unroll_k(self, tycker: &mut Tycker<'_>) -> ResultKont<TypeId> {
        let res = self.unroll(tycker);
        tycker.err_p_to_k(res)
    }
    pub fn unroll(self, tycker: &mut Tycker<'_>) -> Result<TypeId> {
        let kd = tycker.statics.annotations_type[&self];
        let env = tycker.statics.env_at(self);
        let res = match tycker.type_filled(&self)?.to_owned() {
            | Type::Abst(abst) => {
                match tycker.statics.seals.get(&abst) {
                    | Some(ty) => {
                        ty.unroll(tycker)?
                    }
                    | None => self,
                }
            }
            | Type::App(ty) => {
                // congruence rule
                let App(ty1, ty2) = ty;
                let ty1_ = ty1.unroll(tycker)?;
                if ty1 == ty1_ {
                    self
                } else {
                    let app = Alloc::alloc(tycker, App(ty1_, ty2), kd, &env);
                    app.normalize(tycker, kd)?
                }
            }
            // Todo: figure out if this is correct
            // | Type::Fill(_) // unchanged because terms with unfilled types can't be matched against
            | Type::Var(_) // unchanged because type-variable-typed terms are abstract
            | Type::Abs(_) // unchanged because type-abstration-typed terms are ill-formed
            | Type::Named(_)
            | Type::Label(_)
            | Type::Thk(_)
            | Type::Ret(_)
            | Type::Unit(_)
            | Type::Opaque(_)
            | Type::Primitive(_)
            | Type::OS(_) => self,
            | Type::VArrow(_)
            | Type::VForall(_)
            | Type::VPackPi(_)
            | Type::Arrow(_)
            | Type::Forall(_)
            | Type::PackPi(_)
            | Type::Prod(_)
            | Type::Exists(_)
            | Type::ManifestKind(_) => self,
            | Type::Data(_)
            | Type::CoData(_) => self,
            | Type::Proj(Proj(head, name)) => {
                let head = head.unroll(tycker)?;
                match tycker.type_filled(&head)?.to_owned() {
                    | Type::Named(Named(found, inner)) if found == name => inner.unroll(tycker)?,
                    | _ => {
                        let payload_kind = tycker.statics.annotations_type[&self];
                        Alloc::alloc(tycker, Proj(head, name), payload_kind, &env)
                    }
                }
            }
        };
        Ok(res)
    }
}

/* ------------------------------ Normalization ----------------------------- */

impl TypeId {
    pub fn normalize_k(self, tycker: &mut Tycker<'_>, kd: KindId) -> ResultKont<TypeId> {
        let res = self.normalize(tycker, kd);
        tycker.err_p_to_k(res)
    }
    pub fn normalize(self, tycker: &mut Tycker<'_>, kd: KindId) -> Result<TypeId> {
        let res = match tycker.statics.types_pre[&self].to_owned() {
            | Fillable::Fill(_) => self,
            | Fillable::Done(ty) => match ty {
                | Type::App(app) => {
                    let App(ty1, ty2) = app;
                    let kd2 = tycker.statics.annotations_type[&ty2];
                    let ty2 = ty2.normalize(tycker, kd2)?;
                    ty1.normalize_app(tycker, ty2, kd)?
                }
                // | Type::App(app) => {
                //     let App(ty1, ty2) = app;
                //     let kd2 = tycker.statics.annotations_type[&ty2];
                //     let ty2 = ty2.normalize(tycker, kd2)?;
                //     ty1.normalize_app(tycker, ty2, kd)?
                // }
                // weak head normalization (?)
                // | Type::App(app) => {
                //     let App(ty1, ty2) = app;
                //     ty1.normalize_app_k(tycker, ty2, kd)?
                // }
                | Type::Var(_)
                | Type::Abst(_)
                | Type::Abs(_)
                | Type::Named(_)
                | Type::Label(_)
                | Type::Thk(_)
                | Type::Ret(_)
                | Type::Unit(_)
                | Type::Opaque(_)
                | Type::Primitive(_)
                | Type::OS(_)
                | Type::VArrow(_)
                | Type::VForall(_)
                | Type::VPackPi(_)
                | Type::Arrow(_)
                | Type::Forall(_)
                | Type::PackPi(_)
                | Type::Prod(_)
                | Type::Exists(_)
                | Type::ManifestKind(_)
                | Type::Data(_)
                | Type::CoData(_) => self,
                | Type::Proj(Proj(head, name)) => {
                    let head_kind = tycker.statics.annotations_type[&head];
                    let head = head.normalize(tycker, head_kind)?;
                    match tycker.type_filled(&head)?.to_owned() {
                        | Type::Named(Named(found, inner)) if found == name => {
                            inner.normalize(tycker, kd)?
                        }
                        | _ => {
                            let env = tycker.statics.env_at(self);
                            Alloc::alloc(tycker, Proj(head, name), kd, &env)
                        }
                    }
                }
            },
        };
        Ok(res)
    }
    pub fn normalize_app_k(
        self, tycker: &mut Tycker<'_>, a_ty: TypeId, kd: KindId,
    ) -> ResultKont<TypeId> {
        let res = self.normalize_app(tycker, a_ty, kd);
        tycker.err_p_to_k(res)
    }
    pub fn normalize_app(
        self, tycker: &mut Tycker<'_>, a_ty: TypeId, kd: KindId,
    ) -> Result<TypeId> {
        let env = tycker.statics.env_at(self);
        let res = match tycker.statics.types_pre[&self].to_owned() {
            | Fillable::Fill(_) => self,
            | Fillable::Done(ty) => match ty {
                | Type::Abs(abs) => {
                    // if f_ty is an abstraction, apply it
                    let Abs(binder, body_ty) = abs;
                    let (def, _) = binder.try_destruct_def(tycker);

                    if let Some(def) = def {
                        let argument = binder.bind_argument(tycker, a_ty)?;
                        body_ty.subst(tycker, def, argument)?
                    } else {
                        body_ty
                    }
                }
                | _ => {
                    // else, the app is already normalized
                    Alloc::alloc(tycker, App(self, a_ty), kd, &env)
                }
            },
        };
        Ok(res)
    }
    pub fn normalize_apps_k(
        self, tycker: &mut Tycker<'_>, a_tys: Vec<TypeId>,
    ) -> ResultKont<TypeId> {
        let res = self.normalize_apps(tycker, a_tys);
        tycker.err_p_to_k(res)
    }
    pub fn normalize_apps(self, tycker: &mut Tycker<'_>, a_tys: Vec<TypeId>) -> Result<TypeId> {
        let res = a_tys.into_iter().try_fold(self, |f_ty, a_ty| {
            let abs_kd = tycker.statics.annotations_type[&f_ty];
            let kd = match tycker.kind_filled(&abs_kd)?.to_owned() {
                | Kind::Arrow(Arrow(arg_kd, body_kd)) => {
                    let arg_kd_ = tycker.statics.annotations_type[&a_ty];
                    Lub::lub(arg_kd_, arg_kd, tycker)?;
                    body_kd
                }
                | _ => tycker.err(TyckError::KindMismatch, std::panic::Location::caller())?,
            };
            f_ty.normalize_app(tycker, a_ty, kd)
        })?;
        Ok(res)
    }
}

/* ------------------------- Hole Filling & Solution ------------------------ */

struct InferenceRefinement;

impl InferenceRefinement {
    fn unresolved_type_fill(tycker: &Tycker<'_>, root: TypeId) -> Result<Option<FillId>> {
        let mut current = root;
        let mut visited = HashSet::default();
        loop {
            match tycker.statics.types_pre[&current] {
                | Fillable::Done(_) => return Ok(None),
                | Fillable::Fill(fill) if !visited.insert(fill) => {
                    return tycker
                        .err(TyckError::OccursCheck(fill), std::panic::Location::caller());
                }
                | Fillable::Fill(fill) => match tycker.statics.solus.get(&fill).copied() {
                    | Some(AnnId::Type(solution)) => current = solution,
                    | Some(AnnId::Set | AnnId::Kind(_)) => {
                        return tycker.err(TyckError::SortMismatch, std::panic::Location::caller());
                    }
                    | None => return Ok(Some(fill)),
                },
            }
        }
    }

    fn fresh_type(tycker: &mut Tycker<'_>, parent: FillId, kind: KindId, env: &TyEnv) -> TypeId {
        let site = tycker.statics.fills[&parent];
        let fill = Alloc::alloc(tycker, site, (), &());
        Alloc::alloc(tycker, fill, kind, env)
    }

    fn arrow(tycker: &mut Tycker<'_>, fill: FillId, env: &TyEnv) -> Result<Type> {
        let vtype = Alloc::alloc(tycker, VType, (), &());
        let ctype = Alloc::alloc(tycker, CType, (), &());
        let domain = Self::fresh_type(tycker, fill, vtype, env);
        let codomain = Self::fresh_type(tycker, fill, ctype, env);
        let shape = Alloc::alloc(tycker, Arrow(domain, codomain), ctype, env);
        fill.fill(tycker, shape.into())?;
        Ok(Type::Arrow(Arrow(domain, codomain)))
    }

    fn value_arrow(tycker: &mut Tycker<'_>, fill: FillId, env: &TyEnv) -> Result<Type> {
        let vtype = Alloc::alloc(tycker, VType, (), &());
        let domain = Self::fresh_type(tycker, fill, vtype, env);
        let codomain = Self::fresh_type(tycker, fill, vtype, env);
        let shape = Alloc::alloc(tycker, ValueArrow(domain, codomain), vtype, env);
        fill.fill(tycker, shape.into())?;
        Ok(Type::VArrow(ValueArrow(domain, codomain)))
    }

    fn product(tycker: &mut Tycker<'_>, fill: FillId, env: &TyEnv) -> Result<Type> {
        let vtype = Alloc::alloc(tycker, VType, (), &());
        let head = Self::fresh_type(tycker, fill, vtype, env);
        let tail = Self::fresh_type(tycker, fill, vtype, env);
        let shape = Alloc::alloc(tycker, Prod(head, tail), vtype, env);
        fill.fill(tycker, shape.into())?;
        Ok(Type::Prod(Prod(head, tail)))
    }
}

impl TypeId {
    /// Reveal a solved value type, refining an unresolved metavariable to a
    /// pure value arrow when value application requires that shape.
    #[track_caller]
    pub(crate) fn reveal_or_refine_value_arrow_k(
        self, tycker: &mut Tycker<'_>, env: &TyEnv,
    ) -> ResultKont<Type> {
        let result = (|| {
            let vtype = Alloc::alloc(tycker, VType, (), &());
            let kind = tycker.statics.annotations_type[&self];
            Lub::lub(kind, vtype, tycker)?;
            match InferenceRefinement::unresolved_type_fill(tycker, self)? {
                | Some(fill) => InferenceRefinement::value_arrow(tycker, fill, env),
                | None => tycker.type_filled(&self),
            }
        })();
        tycker.err_p_to_k(result)
    }

    /// Reveal a solved computation type, refining an unresolved metavariable to
    /// a value-to-computation arrow when application requires that shape.
    #[track_caller]
    pub(crate) fn reveal_or_refine_arrow_k(
        self, tycker: &mut Tycker<'_>, env: &TyEnv,
    ) -> ResultKont<Type> {
        let result = (|| {
            let ctype = Alloc::alloc(tycker, CType, (), &());
            let kind = tycker.statics.annotations_type[&self];
            Lub::lub(kind, ctype, tycker)?;
            match InferenceRefinement::unresolved_type_fill(tycker, self)? {
                | Some(fill) => InferenceRefinement::arrow(tycker, fill, env),
                | None => tycker.type_filled(&self),
            }
        })();
        tycker.err_p_to_k(result)
    }

    /// Reveal a solved value type, refining an unresolved metavariable to one
    /// product layer when tuple syntax requires that shape.
    #[track_caller]
    pub(crate) fn reveal_or_refine_product_k(
        self, tycker: &mut Tycker<'_>, env: &TyEnv,
    ) -> ResultKont<Type> {
        let result = (|| {
            let view = match InferenceRefinement::unresolved_type_fill(tycker, self)? {
                | Some(_) => self,
                | None => self.unroll(tycker)?.subst_env(tycker, env)?,
            };
            let vtype = Alloc::alloc(tycker, VType, (), &());
            let kind = tycker.statics.annotations_type[&view];
            Lub::lub(kind, vtype, tycker)?;
            match InferenceRefinement::unresolved_type_fill(tycker, view)? {
                | Some(fill) => InferenceRefinement::product(tycker, fill, env),
                | None => tycker.type_filled(&view),
            }
        })();
        tycker.err_p_to_k(result)
    }

    /// Reveal a product from an expected type already prepared under `env`.
    ///
    /// Direct product structure needs no second recursive substitution. If unrolling exposes a
    /// different representation (for example, through a seal), that newly exposed tree still
    /// receives the environment once.
    #[track_caller]
    pub(crate) fn reveal_or_refine_prepared_product_k(
        self, tycker: &mut Tycker<'_>, env: &TyEnv,
    ) -> ResultKont<Type> {
        let result = (|| {
            let view = match InferenceRefinement::unresolved_type_fill(tycker, self)? {
                | Some(_) => self,
                | None => {
                    let unrolled = self.unroll(tycker)?;
                    if unrolled == self { self } else { unrolled.subst_env(tycker, env)? }
                }
            };
            let vtype = Alloc::alloc(tycker, VType, (), &());
            let kind = tycker.statics.annotations_type[&view];
            Lub::lub(kind, vtype, tycker)?;
            match InferenceRefinement::unresolved_type_fill(tycker, view)? {
                | Some(fill) => InferenceRefinement::product(tycker, fill, env),
                | None => tycker.type_filled(&view),
            }
        })();
        tycker.err_p_to_k(result)
    }

    #[track_caller]
    pub(crate) fn view_product_k(
        self, tycker: &mut Tycker<'_>, env: &TyEnv,
    ) -> ResultKont<Prod<TypeId, TypeId>> {
        match self.reveal_or_refine_product_k(tycker, env)? {
            | Type::Prod(product) => Ok(product),
            | _ => tycker.err_k(
                TyckError::TypeExpected {
                    expected: "a product with enough components".to_string(),
                    found: self,
                },
                std::panic::Location::caller(),
            ),
        }
    }

    #[track_caller]
    pub(crate) fn view_prepared_product_k(
        self, tycker: &mut Tycker<'_>, env: &TyEnv,
    ) -> ResultKont<Prod<TypeId, TypeId>> {
        match self.reveal_or_refine_prepared_product_k(tycker, env)? {
            | Type::Prod(product) => Ok(product),
            | _ => tycker.err_k(
                TyckError::TypeExpected {
                    expected: "a product with enough components".to_string(),
                    found: self,
                },
                std::panic::Location::caller(),
            ),
        }
    }
}

impl FillId {
    pub fn fill_k(&self, tycker: &mut Tycker<'_>, ann: AnnId) -> ResultKont<AnnId> {
        let res = self.fill(tycker, ann);
        tycker.err_p_to_k(res)
    }
    pub fn fill(&self, tycker: &mut Tycker<'_>, candidate: AnnId) -> Result<AnnId> {
        if let Some((head, annotation)) = Self::unresolved_head(tycker, candidate)
            && head == *self
        {
            return Ok(tycker.statics.solus.get(self).copied().unwrap_or(annotation));
        }
        if InferenceOccurs::new(*self).in_annotation(candidate, tycker)? {
            return tycker.err(TyckError::OccursCheck(*self), std::panic::Location::caller());
        }

        let solutions = tycker.statics.solus.clone();
        let scopes = tycker.statics.fill_scopes.clone();
        let result = (|| {
            let current = tycker.statics.solus.get(self).copied();
            let solution =
                current.map_or(Ok(candidate), |current| Lub::lub(current, candidate, tycker))?;
            if !matches!(Self::unresolved_head(tycker, solution), Some((head, _)) if head == *self)
                && InferenceOccurs::new(*self).in_annotation(solution, tycker)?
            {
                return tycker.err(TyckError::OccursCheck(*self), std::panic::Location::caller());
            }
            self.constrain_solution(tycker, solution)?;
            let _ = tycker.statics.solus.upsert(*self, solution);
            Ok(solution)
        })();
        if result.is_err() {
            tycker.statics.solus = solutions;
            tycker.statics.fill_scopes = scopes;
        }
        result
    }

    fn unresolved_head(tycker: &Tycker<'_>, candidate: AnnId) -> Option<(FillId, AnnId)> {
        let mut annotation = candidate;
        let mut visited = HashSet::default();
        loop {
            let fill = match annotation {
                | AnnId::Set => return None,
                | AnnId::Kind(kind) => match tycker.statics.kinds_pre[&kind] {
                    | Fillable::Fill(fill) => fill,
                    | Fillable::Done(_) => return None,
                },
                | AnnId::Type(ty) => match tycker.statics.types_pre[&ty] {
                    | Fillable::Fill(fill) => fill,
                    | Fillable::Done(_) => return None,
                },
            };
            if !visited.insert(fill) {
                return None;
            }
            match tycker.statics.solus.get(&fill).copied() {
                | Some(solution) => annotation = solution,
                | None => return Some((fill, annotation)),
            }
        }
    }

    fn constrain_solution(&self, tycker: &mut Tycker<'_>, solution: AnnId) -> Result<()> {
        match (solution, tycker.statics.fill_scopes.get(self).cloned()) {
            | (AnnId::Type(ty), Some(scope)) => ty.constrain_to_scope(tycker, &scope),
            | (AnnId::Set | AnnId::Kind(_), Some(_))
            | (AnnId::Set | AnnId::Kind(_) | AnnId::Type(_), None) => Ok(()),
        }
    }
}

/// Pass-wide hole resolution after inference has stopped mutating solutions.
#[derive(Default)]
pub(crate) struct HoleResolver {
    types: HashMap<TypeId, TypeId>,
    missing: HashSet<FillId>,
}

impl HoleResolver {
    pub(crate) fn resolve_k(
        &mut self, root: TypeId, tycker: &mut Tycker<'_>,
    ) -> ResultKont<TypeId> {
        let result = self.resolve(root, tycker);
        tycker.err_p_to_k(result)
    }

    fn resolve(&mut self, root: TypeId, tycker: &mut Tycker<'_>) -> Result<TypeId> {
        if let Some(resolved) = self.types.get(&root).copied() {
            return Ok(resolved);
        }
        root.resolve_holes(tycker, self)
    }

    fn remember(&mut self, roots: impl IntoIterator<Item = TypeId>, resolved: TypeId) -> TypeId {
        roots.into_iter().filter(|root| *root != resolved).for_each(|root| {
            self.types.insert(root, resolved);
        });
        self.types.insert(resolved, resolved);
        resolved
    }

    pub(crate) fn into_missing(self) -> Vec<FillId> {
        let mut missing = self.missing.into_iter().collect::<Vec<_>>();
        missing.sort_unstable();
        missing
    }
}

impl TypeId {
    pub fn solution_k(&self, tycker: &mut Tycker<'_>) -> ResultKont<(TypeId, Vec<FillId>)> {
        let res = self.solution(tycker);
        tycker.err_p_to_k(res)
    }
    /// Solve unfilled types as much as possible; returns the final type and the unfilled holes
    pub fn solution(&self, tycker: &mut Tycker<'_>) -> Result<(TypeId, Vec<FillId>)> {
        let mut resolver = HoleResolver::default();
        let resolved = resolver.resolve(*self, tycker)?;
        Ok((resolved, resolver.into_missing()))
    }

    fn resolve_holes(
        &self, tycker: &mut Tycker<'_>, resolver: &mut HoleResolver,
    ) -> Result<TypeId> {
        let root = *self;
        let mut res = *self;
        let mut aliases = Vec::new();
        // recursively lookup unfilled types as much as possible
        while let Fillable::Fill(fill) = tycker.statics.types_pre[&res].to_owned() {
            aliases.push(res);
            let solu = match tycker.statics.solus.get(&fill).cloned() {
                | None => break,
                | Some(AnnId::Type(ty)) => ty,
                | Some(AnnId::Set | AnnId::Kind(_)) => {
                    tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                }
            };
            res = solu;
            if let Some(resolved) = resolver.types.get(&res).copied() {
                return Ok(resolver.remember(aliases.into_iter().chain([root]), resolved));
            }
        }
        let env = tycker.statics.env_at(res);
        let res = match tycker.statics.types_pre[&res].to_owned() {
            | Fillable::Fill(fill) => {
                resolver.missing.insert(fill);
                res
            }
            | Fillable::Done(ty) => match ty {
                | Type::Var(_) | Type::Abst(_) => res,
                | Type::Abs(ty) => {
                    let Abs(tpat, ty) = ty;
                    let tpat_ = tpat;
                    let ty_ = resolver.resolve(ty, tycker)?;
                    if ty == ty_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            Abs(tpat_, ty_),
                            tycker.statics.annotations_type[&res],
                            &env,
                        )
                    }
                }
                | Type::App(ty) => {
                    let App(f_ty, a_ty) = ty;
                    let f_ty_ = resolver.resolve(f_ty, tycker)?;
                    let a_ty_ = resolver.resolve(a_ty, tycker)?;
                    if f_ty == f_ty_ && a_ty == a_ty_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            App(f_ty_, a_ty_),
                            tycker.statics.annotations_type[&res],
                            &env,
                        )
                    }
                }
                | Type::Named(ty) => {
                    let Named(name, inner) = ty;
                    let inner_ = resolver.resolve(inner, tycker)?;
                    if inner == inner_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            Named(name, inner_),
                            tycker.statics.annotations_type[&res],
                            &env,
                        )
                    }
                }
                | Type::Label(ty) => {
                    let Label(name, inner) = ty;
                    let inner_ = resolver.resolve(inner, tycker)?;
                    if inner == inner_ {
                        res
                    } else {
                        let target = Alloc::alloc(
                            tycker,
                            Label(name, inner_),
                            tycker.statics.annotations_type[&res],
                            &env,
                        );
                        tycker
                            .statics
                            .builtin_roles
                            .transfer_value(res, target)
                            .expect("a fresh resolved label cannot have a conflicting role");
                        target
                    }
                }
                | Type::Proj(ty) => {
                    let Proj(head, name) = ty;
                    let head_ = resolver.resolve(head, tycker)?;
                    match tycker.statics.types_pre[&head_].to_owned() {
                        | Fillable::Done(Type::Named(Named(found, inner))) if found == name => {
                            inner
                        }
                        | _ if head == head_ => res,
                        | _ => Alloc::alloc(
                            tycker,
                            Proj(head_, name),
                            tycker.statics.annotations_type[&res],
                            &env,
                        ),
                    }
                }
                | Type::Thk(_)
                | Type::Ret(_)
                | Type::Unit(_)
                | Type::Opaque(_)
                | Type::Primitive(_)
                | Type::OS(_) => res,
                | Type::Arrow(ty) => {
                    let Arrow(ty1, ty2) = ty;
                    let ty1_ = resolver.resolve(ty1, tycker)?;
                    let ty2_ = resolver.resolve(ty2, tycker)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            Arrow(ty1_, ty2_),
                            tycker.statics.annotations_type[&res],
                            &env,
                        )
                    }
                }
                | Type::VArrow(ValueArrow(ty1, ty2)) => {
                    let ty1_ = resolver.resolve(ty1, tycker)?;
                    let ty2_ = resolver.resolve(ty2, tycker)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            ValueArrow(ty1_, ty2_),
                            tycker.statics.annotations_type[&res],
                            &env,
                        )
                    }
                }
                | Type::VForall(ty) => {
                    let ValueForall(tpat, ty) = ty;
                    let tpat_ = tpat;
                    let ty_ = resolver.resolve(ty, tycker)?;
                    if ty == ty_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            ValueForall(tpat_, ty_),
                            tycker.statics.annotations_type[&res],
                            &env,
                        )
                    }
                }
                | Type::VPackPi(pack_pi) => {
                    let ValuePackPi { domain, witnesses, codomain } = *pack_pi;
                    let domain_ = resolver.resolve(domain, tycker)?;
                    let codomain_ = resolver.resolve(codomain, tycker)?;
                    if domain == domain_ && codomain == codomain_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            ValuePackPi { domain: domain_, witnesses, codomain: codomain_ },
                            tycker.statics.annotations_type[&res],
                            &env,
                        )
                    }
                }
                | Type::Forall(ty) => {
                    let Forall(tpat, ty) = ty;
                    let tpat_ = tpat;
                    let ty_ = resolver.resolve(ty, tycker)?;
                    if ty == ty_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            Forall(tpat_, ty_),
                            tycker.statics.annotations_type[&res],
                            &env,
                        )
                    }
                }
                | Type::PackPi(pack_pi) => {
                    let PackPi { domain, witnesses, codomain } = *pack_pi;
                    let domain_ = resolver.resolve(domain, tycker)?;
                    let codomain_ = resolver.resolve(codomain, tycker)?;
                    if domain == domain_ && codomain == codomain_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            PackPi { domain: domain_, witnesses, codomain: codomain_ },
                            tycker.statics.annotations_type[&res],
                            &env,
                        )
                    }
                }
                | Type::Prod(ty) => {
                    let Prod(ty1, ty2) = ty;
                    let ty1_ = resolver.resolve(ty1, tycker)?;
                    let ty2_ = resolver.resolve(ty2, tycker)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            Prod(ty1_, ty2_),
                            tycker.statics.annotations_type[&res],
                            &env,
                        )
                    }
                }
                | Type::Exists(ty) => {
                    let Exists { binder, mode, body } = *ty;
                    let (mode, definition_changed) = match mode {
                        | ExistsMode::Abstract => (ExistsMode::Abstract, false),
                        | ExistsMode::Manifest(definition) => {
                            let definition_ = resolver.resolve(definition, tycker)?;
                            (ExistsMode::Manifest(definition_), definition != definition_)
                        }
                    };
                    let body_ = resolver.resolve(body, tycker)?;
                    if !definition_changed && body == body_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            Exists { binder, mode, body: body_ },
                            tycker.statics.annotations_type[&res],
                            &env,
                        )
                    }
                }
                | Type::ManifestKind(manifest) => {
                    let ManifestKind { binder, definition, body } = manifest;
                    let body_ = resolver.resolve(body, tycker)?;
                    if body == body_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            ManifestKind { binder, definition, body: body_ },
                            tycker.statics.annotations_type[&res],
                            &env,
                        )
                    }
                }
                | Type::Data(data) => {
                    let arms = tycker.statics.datas[&data].clone();
                    let mut unchanged = true;
                    let arms_ = arms
                        .into_iter()
                        .map(|(ctor, ty)| {
                            let ty_ = resolver.resolve(ty, tycker)?;
                            if ty == ty_ {
                                Ok((ctor, ty))
                            } else {
                                unchanged = false;
                                Ok((ctor, ty_))
                            }
                        })
                        .collect::<Result<im::Vector<_>>>()?;
                    if unchanged {
                        res
                    } else {
                        let data: DataId = tycker.fresh();
                        tycker.statics.datas.insert_new(data, Data::new(arms_));
                        Alloc::alloc(tycker, data, tycker.statics.annotations_type[&res], &env)
                    }
                }
                | Type::CoData(codata) => {
                    let arms = tycker.statics.codatas[&codata].clone();
                    let mut unchanged = true;
                    let arms_ = arms
                        .into_iter()
                        .map(|(dtor, ty)| {
                            let ty_ = resolver.resolve(ty, tycker)?;
                            if ty == ty_ {
                                Ok((dtor, ty))
                            } else {
                                unchanged = false;
                                Ok((dtor, ty_))
                            }
                        })
                        .collect::<Result<im::Vector<_>>>()?;
                    if unchanged {
                        res
                    } else {
                        let codata: CoDataId = tycker.fresh();
                        tycker.statics.codatas.insert_new(codata, CoData::new(arms_));
                        Alloc::alloc(tycker, codata, tycker.statics.annotations_type[&res], &env)
                    }
                }
            },
        };
        Ok(resolver.remember(aliases.into_iter().chain([root, res]), res))
    }
}

impl<'a> Tycker<'a> {
    pub fn filling_k<R>(
        &mut self, id: &AnnId, f_set: impl FnOnce(&mut Tycker<'a>) -> Result<R>,
        f_kind: impl FnOnce(&mut Tycker<'a>, Kind) -> Result<R>,
        f_type: impl FnOnce(&mut Tycker<'a>, Type) -> Result<R>,
        f_fill: impl FnOnce(&mut Tycker<'a>, FillId) -> Result<R>,
    ) -> ResultKont<R> {
        let res = self.filling(id, f_set, f_kind, f_type, f_fill);
        self.err_p_to_k(res)
    }
    /// internally resolves unfilled annotations; fails if the annotation has no solution.
    /// only fills the uppermost (or head?) annotation
    pub fn filling<R>(
        &mut self, id: &AnnId, f_set: impl FnOnce(&mut Tycker<'a>) -> Result<R>,
        f_kind: impl FnOnce(&mut Tycker<'a>, Kind) -> Result<R>,
        f_type: impl FnOnce(&mut Tycker<'a>, Type) -> Result<R>,
        f_fill: impl FnOnce(&mut Tycker<'a>, FillId) -> Result<R>,
    ) -> Result<R> {
        match id {
            | AnnId::Set => f_set(self),
            | AnnId::Kind(id) => match self.statics.kinds_pre[id].to_owned() {
                | Fillable::Fill(fill) => match self.statics.solus.get(&fill).cloned() {
                    | Some(AnnId::Kind(kind)) => {
                        self.filling(&kind.into(), f_set, f_kind, f_type, f_fill)
                    }
                    | Some(_) => {
                        self.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | None => f_fill(self, fill),
                },
                | Fillable::Done(kind) => f_kind(self, kind),
            },
            | AnnId::Type(id) => match self.statics.types_pre[id].to_owned() {
                | Fillable::Fill(fill) => match self.statics.solus.get(&fill).cloned() {
                    | Some(AnnId::Type(ty)) => {
                        self.filling(&ty.into(), f_set, f_kind, f_type, f_fill)
                    }
                    | Some(_) => {
                        self.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | None => f_fill(self, fill),
                },
                | Fillable::Done(ty) => f_type(self, ty),
            },
        }
    }

    pub fn kind_filled_k(&mut self, id: &KindId) -> ResultKont<Kind> {
        let res = self.kind_filled(id);
        self.err_p_to_k(res)
    }
    /// internally resolves unfilled kinds; fails if the kind has no solution.
    /// only fills the uppermost (or head?) kind
    pub fn kind_filled(&mut self, id: &KindId) -> Result<Kind> {
        self.filling(
            &id.to_owned().into(),
            |_tycker| unreachable!(),
            |_tycker, kd| Ok(kd),
            |_tycker, _ty| unreachable!(),
            |tycker, fill| {
                tycker.err(TyckError::MissingSolution(vec![fill]), std::panic::Location::caller())
            },
        )
    }

    pub fn type_filled_k(&mut self, id: &TypeId) -> ResultKont<Type> {
        let res = self.type_filled(id);
        self.err_p_to_k(res)
    }
    /// internally resolves unfilled types; fails if the type has no solution.
    /// only fills the uppermost (or head?) type
    pub fn type_filled(&mut self, id: &TypeId) -> Result<Type> {
        self.filling(
            &id.to_owned().into(),
            |_tycker| unreachable!(),
            |_tycker, _kd| unreachable!(),
            |_tycker, ty| Ok(ty),
            |tycker, fill| {
                tycker.err(TyckError::MissingSolution(vec![fill]), std::panic::Location::caller())
            },
        )
    }
}

/* ------------------------------ Normalization ----------------------------- */

/// Pass-wide memoization for filled kind and type normalization.
///
/// A single context is shared by every arena root after inference closes, so
/// overlapping type subgraphs are normalized only once.
#[derive(Default)]
pub(crate) struct FilledNormalizer {
    kinds: HashMap<KindId, KindId>,
    types: HashMap<TypeId, TypeId>,
}

impl FilledNormalizer {
    pub(crate) fn normalize_kind_k(
        &mut self, root: KindId, tycker: &mut Tycker<'_>,
    ) -> ResultKont<()> {
        let result = self.normalize_kind(root, tycker);
        tycker.err_p_to_k(result)
    }

    fn normalize_kind(&mut self, root: KindId, tycker: &mut Tycker<'_>) -> Result<()> {
        let _ = root.filled_norm_id(tycker, self)?;
        Ok(())
    }

    pub(crate) fn normalize_type_k(
        &mut self, root: TypeId, tycker: &mut Tycker<'_>,
    ) -> ResultKont<()> {
        let result = self.normalize_type(root, tycker);
        tycker.err_p_to_k(result)
    }

    fn normalize_type(&mut self, root: TypeId, tycker: &mut Tycker<'_>) -> Result<()> {
        let _ = root.filled_norm_id(tycker, self)?;
        Ok(())
    }
}

impl KindId {
    fn filled_norm_id(
        self, tycker: &mut Tycker<'_>, norm: &mut FilledNormalizer,
    ) -> Result<KindId> {
        if let Some(norm) = norm.kinds.get(&self).cloned() {
            return Ok(norm);
        }
        let res = match tycker.statics.kinds_pre[&self].to_owned() {
            | Fillable::Fill(fill) => match tycker.statics.solus.get(&fill).cloned() {
                | Some(AnnId::Kind(kd)) => kd.filled_norm_id(tycker, norm)?,
                | Some(AnnId::Set | AnnId::Type(_)) => {
                    let _: ResultKont<()> =
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller());
                    self
                }
                | None => {
                    let _: ResultKont<()> = tycker.err_k(
                        TyckError::MissingSolution(vec![fill]),
                        std::panic::Location::caller(),
                    );
                    self
                }
            },
            | Fillable::Done(kind) => match kind {
                | Kind::VType(VType) | Kind::CType(CType) => self,
                | Kind::Arrow(Arrow(from, to)) => {
                    let from_norm = from.filled_norm_id(tycker, norm)?;
                    let to_norm = to.filled_norm_id(tycker, norm)?;
                    if from_norm == from && to_norm == to {
                        self
                    } else {
                        Alloc::alloc(tycker, Arrow(from_norm, to_norm), (), &())
                    }
                }
                | Kind::Label(Label(name, inner)) => {
                    let inner_norm = inner.filled_norm_id(tycker, norm)?;
                    if inner_norm == inner {
                        self
                    } else {
                        Alloc::alloc(tycker, Label(name, inner_norm), (), &())
                    }
                }
            },
        };
        norm.kinds.insert(self, res);
        if self != res {
            norm.kinds.insert(res, res);
        }
        if let Fillable::Done(kind) = tycker.statics.kinds_pre[&res].to_owned() {
            if self == res {
                let _ = tycker.statics.kinds_normalized.upsert(self, kind);
            } else {
                let _ = tycker.statics.kinds_normalized.upsert(self, kind.clone());
                let _ = tycker.statics.kinds_normalized.upsert(res, kind);
            }
        }
        Ok(res)
    }
}

impl TypeId {
    fn filled_norm_id(
        self, tycker: &mut Tycker<'_>, norm: &mut FilledNormalizer,
    ) -> Result<TypeId> {
        if let Some(normalized) = norm.types.get(&self).cloned() {
            return Ok(normalized);
        }
        let kd = tycker.statics.annotations_type[&self];
        let kd_norm = kd.filled_norm_id(tycker, norm)?;
        let env = tycker.statics.env_at(self);
        let res = match tycker.statics.types_pre[&self].to_owned() {
            | Fillable::Fill(fill) => match tycker.statics.solus.get(&fill).cloned() {
                | Some(AnnId::Type(ty)) => ty.filled_norm_id(tycker, norm)?,
                | Some(AnnId::Set | AnnId::Kind(_)) => {
                    let _: ResultKont<()> =
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller());
                    self
                }
                | None => {
                    let _: ResultKont<()> = tycker.err_k(
                        TyckError::MissingSolution(vec![fill]),
                        std::panic::Location::caller(),
                    );
                    self
                }
            },
            | Fillable::Done(ty) => match ty {
                | Type::Var(def) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, def, kd_norm, &env)
                    }
                }
                | Type::Abst(abst) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, abst, kd_norm, &env)
                    }
                }
                | Type::Abs(abs) => {
                    let Abs(tpat, body) = abs;
                    let body_norm = body.filled_norm_id(tycker, norm)?;
                    if body_norm == body && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, Abs(tpat, body_norm), kd_norm, &env)
                    }
                }
                | Type::App(app) => {
                    let App(f_ty, a_ty) = app;
                    let f_norm = f_ty.filled_norm_id(tycker, norm)?;
                    let a_norm = a_ty.filled_norm_id(tycker, norm)?;
                    match tycker.statics.types_pre[&f_norm].to_owned() {
                        | Fillable::Done(Type::Abs(abs)) => {
                            let Abs(tpat, body) = abs;
                            let (def, _) = tpat.try_destruct_def(tycker);
                            let body_subst = if let Some(def) = def {
                                let argument = tpat.bind_argument(tycker, a_norm)?;
                                body.subst(tycker, def, argument)?
                            } else {
                                body
                            };
                            if body_subst == self {
                                self
                            } else {
                                body_subst.filled_norm_id(tycker, norm)?
                            }
                        }
                        | _ => {
                            if f_norm == f_ty && a_norm == a_ty && kd_norm == kd {
                                self
                            } else {
                                Alloc::alloc(tycker, App(f_norm, a_norm), kd_norm, &env)
                            }
                        }
                    }
                }
                | Type::Named(named) => {
                    let Named(name, inner) = named;
                    let inner_norm = inner.filled_norm_id(tycker, norm)?;
                    if inner_norm == inner && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, Named(name, inner_norm), kd_norm, &env)
                    }
                }
                | Type::Label(label) => {
                    let Label(name, inner) = label;
                    let inner_norm = inner.filled_norm_id(tycker, norm)?;
                    if inner_norm == inner && kd_norm == kd {
                        self
                    } else {
                        let target = Alloc::alloc(tycker, Label(name, inner_norm), kd_norm, &env);
                        tycker
                            .statics
                            .builtin_roles
                            .transfer_value(self, target)
                            .expect("a fresh normalized label cannot have a conflicting role");
                        target
                    }
                }
                | Type::Proj(proj) => {
                    let Proj(head, name) = proj;
                    let head_norm = head.filled_norm_id(tycker, norm)?;
                    match tycker.statics.types_pre[&head_norm].to_owned() {
                        | Fillable::Done(Type::Named(Named(found, inner))) if found == name => {
                            inner.filled_norm_id(tycker, norm)?
                        }
                        | _ if head_norm == head && kd_norm == kd => self,
                        | _ => Alloc::alloc(tycker, Proj(head_norm, name), kd_norm, &env),
                    }
                }
                | Type::Thk(ThkTy) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, ThkTy, kd_norm, &env)
                    }
                }
                | Type::Ret(RetTy) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, RetTy, kd_norm, &env)
                    }
                }
                | Type::Unit(UnitTy) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, UnitTy, kd_norm, &env)
                    }
                }
                | Type::Opaque(OpaqueTy) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, OpaqueTy, kd_norm, &env)
                    }
                }
                | Type::Primitive(primitive) => primitive.build(tycker, &env),
                | Type::OS(OSTy) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, OSTy, kd_norm, &env)
                    }
                }
                | Type::Arrow(arr) => {
                    let Arrow(ty1, ty2) = arr;
                    let ty1_norm = ty1.filled_norm_id(tycker, norm)?;
                    let ty2_norm = ty2.filled_norm_id(tycker, norm)?;
                    if ty1_norm == ty1 && ty2_norm == ty2 && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, Arrow(ty1_norm, ty2_norm), kd_norm, &env)
                    }
                }
                | Type::VArrow(ValueArrow(ty1, ty2)) => {
                    let ty1_norm = ty1.filled_norm_id(tycker, norm)?;
                    let ty2_norm = ty2.filled_norm_id(tycker, norm)?;
                    if ty1_norm == ty1 && ty2_norm == ty2 && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, ValueArrow(ty1_norm, ty2_norm), kd_norm, &env)
                    }
                }
                | Type::VForall(forall) => {
                    let ValueForall(abst, body) = forall;
                    let body_norm = body.filled_norm_id(tycker, norm)?;
                    if body_norm == body && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, ValueForall(abst, body_norm), kd_norm, &env)
                    }
                }
                | Type::VPackPi(pack_pi) => {
                    let ValuePackPi { domain, witnesses, codomain } = *pack_pi;
                    let domain_norm = domain.filled_norm_id(tycker, norm)?;
                    let codomain_norm = codomain.filled_norm_id(tycker, norm)?;
                    if domain_norm == domain && codomain_norm == codomain && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(
                            tycker,
                            ValuePackPi { domain: domain_norm, witnesses, codomain: codomain_norm },
                            kd_norm,
                            &env,
                        )
                    }
                }
                | Type::Forall(forall) => {
                    let Forall(abst, body) = forall;
                    let body_norm = body.filled_norm_id(tycker, norm)?;
                    if body_norm == body && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, Forall(abst, body_norm), kd_norm, &env)
                    }
                }
                | Type::PackPi(pack_pi) => {
                    let PackPi { domain, witnesses, codomain } = *pack_pi;
                    let domain_norm = domain.filled_norm_id(tycker, norm)?;
                    let codomain_norm = codomain.filled_norm_id(tycker, norm)?;
                    if domain_norm == domain && codomain_norm == codomain && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(
                            tycker,
                            PackPi { domain: domain_norm, witnesses, codomain: codomain_norm },
                            kd_norm,
                            &env,
                        )
                    }
                }
                | Type::Prod(prod) => {
                    let Prod(ty1, ty2) = prod;
                    let ty1_norm = ty1.filled_norm_id(tycker, norm)?;
                    let ty2_norm = ty2.filled_norm_id(tycker, norm)?;
                    if ty1_norm == ty1 && ty2_norm == ty2 && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, Prod(ty1_norm, ty2_norm), kd_norm, &env)
                    }
                }
                | Type::Exists(exists) => {
                    let Exists { binder, mode, body } = *exists;
                    let (mode, definition_changed) = match mode {
                        | ExistsMode::Abstract => (ExistsMode::Abstract, false),
                        | ExistsMode::Manifest(definition) => {
                            let definition_norm = definition.filled_norm_id(tycker, norm)?;
                            (ExistsMode::Manifest(definition_norm), definition_norm != definition)
                        }
                    };
                    let body_norm = body.filled_norm_id(tycker, norm)?;
                    if !definition_changed && body_norm == body && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(
                            tycker,
                            Exists { binder, mode, body: body_norm },
                            kd_norm,
                            &env,
                        )
                    }
                }
                | Type::ManifestKind(manifest) => {
                    let ManifestKind { binder, definition, body } = manifest;
                    let definition_norm = definition.filled_norm_id(tycker, norm)?;
                    let body_norm = body.filled_norm_id(tycker, norm)?;
                    if definition_norm == definition && body_norm == body && kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(
                            tycker,
                            ManifestKind { binder, definition: definition_norm, body: body_norm },
                            kd_norm,
                            &env,
                        )
                    }
                }
                | Type::Data(data) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, data, kd_norm, &env)
                    }
                }
                | Type::CoData(codata) => {
                    if kd_norm == kd {
                        self
                    } else {
                        Alloc::alloc(tycker, codata, kd_norm, &env)
                    }
                }
            },
        };
        norm.types.insert(self, res);
        if self != res {
            norm.types.insert(res, res);
        }
        // Store the normalized form only when it differs from the pre form:
        // readers fall back to `types_pre` for unchanged nodes, so the arena
        // keeps a delta instead of duplicating every type. Solved fill nodes
        // still store their solution's type, as the pre form is a fill marker.
        if self != res
            && let Fillable::Done(ty) = tycker.statics.types_pre[&res].to_owned()
        {
            let _ = tycker.statics.types_normalized.upsert(self, ty);
        }
        Ok(res)
    }
}
