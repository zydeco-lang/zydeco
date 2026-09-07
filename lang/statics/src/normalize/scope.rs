//! Collect free witnesses and inference holes and enforce existential scope boundaries.

use super::*;

impl TypeId {
    /// Test known existential evidence without changing inference-hole scopes.
    /// Static inspection may expose only witnesses already visible to its caller.
    pub(crate) fn has_visible_witnesses(&self, tycker: &Tycker<'_>, scope: &SkolemScope) -> bool {
        TypeSupport::of(*self, tycker)
            .is_ok_and(|support| support.skolems.iter().all(|witness| scope.contains(witness)))
    }

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
pub(super) struct TypeSupport {
    pub(super) skolems: HashSet<AbstId>,
    /// Package witnesses bound at every occurrence of an inference hole.
    ///
    /// A shared hole may occur under different package binders, so the
    /// admissible local scope is the intersection of those occurrences.
    pub(super) fills: HashMap<FillId, SkolemScope>,
}

impl TypeSupport {
    pub(super) fn of(root: TypeId, tycker: &Tycker<'_>) -> Result<Self> {
        let mut collector = TypeSupportCollector::new();
        collector.visit(root, tycker)?;
        Ok(collector.support)
    }
}

pub(super) struct TypeSupportCollector {
    support: TypeSupport,
    bound: HashSet<AbstId>,
    pack_scope: SkolemScope,
    visiting_fills: HashSet<FillId>,
    visiting_vars: HashSet<(DefId, TypeId)>,
    visiting_datas: HashSet<DataId>,
    visiting_codatas: HashSet<CoDataId>,
}

pub(super) struct InferenceOccurs {
    needle: FillId,
    visiting_kinds: HashSet<KindId>,
    visiting_fills: HashSet<FillId>,
}

impl InferenceOccurs {
    pub(super) fn new(needle: FillId) -> Self {
        Self { needle, visiting_kinds: HashSet::default(), visiting_fills: HashSet::default() }
    }

    pub(super) fn in_annotation(
        &mut self, annotation: AnnId, tycker: &mut Tycker<'_>,
    ) -> Result<bool> {
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
                | Type::Abs(TypeAbstraction { binder, body }) => {
                    let newly_bound = self.bound.insert(binder.witness);
                    let result = self.visit(body, tycker);
                    if newly_bound {
                        self.bound.remove(&binder.witness);
                    }
                    result?;
                }
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
                | Type::ValPi(pi) => {
                    let ValPi { binder, codomain } = *pi;
                    match binder {
                        | ValPiBinder::Type(binder) => {
                            let newly_bound = self.bound.insert(binder.witness);
                            let result = self.visit(codomain, tycker);
                            if newly_bound {
                                self.bound.remove(&binder.witness);
                            }
                            result?;
                        }
                        | ValPiBinder::Value(parameter) => {
                            self.visit(parameter.domain, tycker)?;
                            match parameter.witnesses {
                                | None => self.visit(codomain, tycker)?,
                                | Some(witnesses) => {
                                    let outer_bound = self.bound.clone();
                                    let outer_pack_scope = self.pack_scope.clone();
                                    self.bound.extend(witnesses.iter().copied());
                                    self.pack_scope =
                                        self.pack_scope.union(&witnesses.iter().copied().collect());
                                    let result = self.visit(codomain, tycker);
                                    self.bound = outer_bound;
                                    self.pack_scope = outer_pack_scope;
                                    result?;
                                }
                            }
                        }
                    }
                }
                | Type::Arrow(Arrow(input, output)) => {
                    [input, output].into_iter().try_for_each(|ty| self.visit(ty, tycker))?;
                }
                | Type::Prod(Prod(components)) => {
                    components.into_iter().try_for_each(|ty| self.visit(ty, tycker))?;
                }
                | Type::Forall(Forall(binder, body)) => {
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
