//! Package-dependent arrow (`PackPi`) witness layouts and monadic structure translation.

use super::*;

/// Allocate a computation abstraction whose parameter opens the witnesses
/// bound by a package-dependent arrow.
pub(super) struct PackPiAbstraction {
    pub(super) binder: VPatId,
    pub(super) body: CompuId,
    pub(super) signature: TypeId,
}

impl PackPiAbstraction {
    pub(super) fn build(self, tycker: &mut Tycker<'_>, _env: &TyEnv) -> CompuId {
        Alloc::alloc(tycker, Abs(self.binder, self.body), self.signature, &())
    }
}

/// Allocate an application after its package-dependent result has been
/// instantiated.
pub(super) struct PackPiApplication {
    pub(super) function: CompuId,
    pub(super) argument: ValueId,
    pub(super) result: TypeId,
}

impl PackPiApplication {
    pub(super) fn build(self, tycker: &mut Tycker<'_>, _env: &TyEnv) -> CompuId {
        Alloc::alloc(tycker, App(self.function, self.argument), self.result, &())
    }
}

/// Construct a source package pattern that opens exactly the witnesses bound
/// by a `PackPi`. The pattern is used only as input to monadic translation.
struct PackPiBinderPattern {
    pub(super) signature: PackPi,
}

#[derive(Clone, Copy)]
pub(super) enum PackPiWitnessLayoutEntry {
    KindManifest,
    Abstract(AbstId),
    TypeManifest,
}

pub(super) enum PackagePatternStructure {
    Abstract,
    Manifest,
}

#[derive(Clone)]
pub(super) struct PackPiWitnessLayout(pub(super) Vec<PackPiWitnessLayoutEntry>);

struct PackPiWitnessLayoutState<'a> {
    domain: TypeId,
    witnesses: &'a [AbstId],
    expected: usize,
}

impl<'a> PackPiWitnessLayoutState<'a> {
    fn collect(
        self, tycker: &mut Tycker<'_>, env: &TyEnv,
    ) -> Result<Vec<PackPiWitnessLayoutEntry>> {
        zydeco_foundation::stack::maybe_grow(|| {
            let view = self.domain.unroll(tycker)?.subst_env(tycker, env)?;
            match tycker.type_filled(&view)?.to_owned() {
                | Type::ManifestKind(ManifestKind { body, .. }) => {
                    let tail = Self { domain: body, ..self }.collect(tycker, env)?;
                    Ok(std::iter::once(PackPiWitnessLayoutEntry::KindManifest)
                        .chain(tail)
                        .collect())
                }
                | Type::Exists(exists) => {
                    let Exists { binder, mode, body } = *exists;
                    let (entry, witnesses, payload) = match mode {
                        | ExistsMode::Abstract => {
                            let Some((&witness, witnesses)) = self.witnesses.split_first() else {
                                return self.mismatch(tycker);
                            };
                            let kind = tycker.statics.annotations_abst[witness];
                            (
                                PackPiWitnessLayoutEntry::Abstract(witness),
                                witnesses,
                                Alloc::alloc(tycker, witness, kind, &()),
                            )
                        }
                        | ExistsMode::Manifest(definition) => {
                            (PackPiWitnessLayoutEntry::TypeManifest, self.witnesses, definition)
                        }
                    };
                    let domain = body.subst_abst(tycker, (binder.witness, payload))?;
                    let tail =
                        Self { domain, witnesses, expected: self.expected }.collect(tycker, env)?;
                    Ok(std::iter::once(entry).chain(tail).collect())
                }
                | _ if self.witnesses.is_empty() => Ok(Vec::new()),
                | _ => self.mismatch(tycker),
            }
        })
    }
    fn mismatch<T>(&self, tycker: &mut Tycker<'_>) -> Result<T> {
        tycker.err(TyckError::PackageWitnessArityMismatch {
            expected: self.expected,
            found: self.expected - self.witnesses.len(),
        })
    }
}

impl PackPiWitnessLayout {
    fn new(signature: &PackPi, tycker: &mut Tycker<'_>, env: &TyEnv) -> Result<Self> {
        let witnesses = signature.witnesses.iter().copied().collect::<Vec<_>>();
        let state = PackPiWitnessLayoutState {
            domain: signature.domain,
            witnesses: &witnesses,
            expected: witnesses.len(),
        };
        state.collect(tycker, env).map(Self)
    }

    fn len(&self) -> usize {
        self.0.len()
    }
}

/// Walk a checked binder pattern in lockstep with its domain, assigning the
/// arrow's canonical witnesses to the openings the pattern actually performs.
///
/// A domain walk alone cannot tell a nested package the pattern opens from one
/// it merely stores in a product component; the pattern's package nodes are
/// what consume witnesses, in source order.
struct PackPiPatternLayoutState<'a> {
    witnesses: &'a [AbstId],
    expected: usize,
}

impl<'a> PackPiPatternLayoutState<'a> {
    fn collect(
        &mut self, tycker: &mut Tycker<'_>, env: &TyEnv, pattern: VPatId, domain: TypeId,
    ) -> Result<Vec<PackPiWitnessLayoutEntry>> {
        zydeco_foundation::stack::maybe_grow(|| {
            let view = domain.unroll(tycker)?.subst_env(tycker, env)?;
            match (tycker.statics.vpats[&pattern].to_owned(), tycker.type_filled(&view)?.to_owned())
            {
                | (ValuePattern::Named(Named(_, inner)), Type::Label(Label(_, payload))) => {
                    self.collect(tycker, env, inner, payload)
                }
                | (ValuePattern::SCons(ConsN(witnesses, body)), _) => {
                    let mut entries = Vec::new();
                    let mut domain = view;
                    for witness in witnesses {
                        match (witness, tycker.type_filled(&domain)?.to_owned()) {
                            | (
                                StaticPatId::Kind(_),
                                Type::ManifestKind(ManifestKind { body, .. }),
                            ) => {
                                entries.push(PackPiWitnessLayoutEntry::KindManifest);
                                domain = body;
                            }
                            | (StaticPatId::Type(_), Type::Exists(exists)) => {
                                let Exists { binder, mode, body } = *exists;
                                match mode {
                                    | ExistsMode::Abstract => {
                                        let Some((&witness, rest)) = self.witnesses.split_first()
                                        else {
                                            return self.mismatch(tycker);
                                        };
                                        let kind = tycker.statics.annotations_abst[witness];
                                        let payload = Alloc::alloc(tycker, witness, kind, &());
                                        entries.push(PackPiWitnessLayoutEntry::Abstract(witness));
                                        domain =
                                            body.subst_abst(tycker, (binder.witness, payload))?;
                                        self.witnesses = rest;
                                    }
                                    | ExistsMode::Manifest(definition) => {
                                        entries.push(PackPiWitnessLayoutEntry::TypeManifest);
                                        domain =
                                            body.subst_abst(tycker, (binder.witness, definition))?;
                                    }
                                }
                            }
                            | _ => return self.mismatch(tycker),
                        }
                    }
                    let mut tail = self.collect(tycker, env, body, domain)?;
                    entries.append(&mut tail);
                    Ok(entries)
                }
                | (ValuePattern::VCons(components), Type::Prod(Prod(domains))) => {
                    if components.len() != domains.len() {
                        return self.mismatch(tycker);
                    }
                    let mut entries = Vec::new();
                    for (component, domain) in components.into_iter().zip(domains) {
                        entries.extend(self.collect(tycker, env, component, domain)?);
                    }
                    Ok(entries)
                }
                | (ValuePattern::Alias(Alias(patterns)), _) => {
                    let mut entries = Vec::new();
                    for member in patterns {
                        entries.extend(self.collect(tycker, env, member, domain)?);
                    }
                    Ok(entries)
                }
                | (_, _) => Ok(Vec::new()),
            }
        })
    }
    fn mismatch<T>(&self, tycker: &mut Tycker<'_>) -> Result<T> {
        tycker.err(TyckError::PackageWitnessArityMismatch {
            expected: self.expected,
            found: self.expected - self.witnesses.len(),
        })
    }
}

impl PackPiBinderPattern {
    pub(super) fn build(self, tycker: &mut Tycker<'_>, env: &TyEnv) -> Result<VPatId> {
        let layout = PackPiWitnessLayout::new(&self.signature, tycker, env)?;
        let domain = self.signature.domain;
        let PackPiWitnessLayout(entries) = layout;
        let capacity = entries.len();
        let (body_ty, witness_patterns) = entries.into_iter().enumerate().try_fold(
            (domain, Vec::with_capacity(capacity)),
            |(body_ty, mut witness_patterns),
             (index, entry)|
             -> Result<(TypeId, Vec<StaticPatId>)> {
                let view = body_ty.unroll(tycker)?.subst_env(tycker, env)?;
                match (entry, tycker.type_filled(&view)?.to_owned()) {
                    | (
                        PackPiWitnessLayoutEntry::KindManifest,
                        Type::ManifestKind(ManifestKind { definition, body, .. }),
                    ) => {
                        let witness_def = Alloc::alloc(
                            tycker,
                            VarName::new(format!("pack_kind_{index}")),
                            AnnId::Set,
                            &(),
                        );
                        tycker.kind_bindings.insert(witness_def, definition);
                        let pattern = Alloc::alloc(tycker, witness_def, (), &());
                        witness_patterns.push(StaticPatId::Kind(pattern));
                        Ok((body, witness_patterns))
                    }
                    | (
                        entry @ (PackPiWitnessLayoutEntry::Abstract(_)
                        | PackPiWitnessLayoutEntry::TypeManifest),
                        Type::Exists(exists),
                    ) => {
                        let Exists { binder, mode, body } = *exists;
                        let kind = binder.payload_kind(tycker);
                        let witness_def = Alloc::alloc(
                            tycker,
                            VarName::new(format!("pack_witness_{index}")),
                            kind.into(),
                            &(),
                        );
                        witness_patterns.push(StaticPatId::Type(
                            binder.pattern.rebind_payload(tycker, Some(witness_def)),
                        ));
                        let payload = match (entry, mode) {
                            | (
                                PackPiWitnessLayoutEntry::Abstract(witness),
                                ExistsMode::Abstract,
                            ) => Alloc::alloc(tycker, witness, kind, &()),
                            | (
                                PackPiWitnessLayoutEntry::TypeManifest,
                                ExistsMode::Manifest(definition),
                            ) => definition,
                            | (PackPiWitnessLayoutEntry::Abstract(_), ExistsMode::Manifest(_))
                            | (PackPiWitnessLayoutEntry::TypeManifest, ExistsMode::Abstract) => {
                                unreachable!()
                            }
                            | (PackPiWitnessLayoutEntry::KindManifest, _) => unreachable!(),
                        };
                        let body_ty = body.subst_abst(tycker, (binder.witness, payload))?;
                        Ok((body_ty, witness_patterns))
                    }
                    | _ => unreachable!(),
                }
            },
        )?;

        let body_def = Alloc::alloc(tycker, VarName::new("pack_body"), body_ty.into(), &());
        let body_pattern: VPatId = Alloc::alloc(tycker, body_def, body_ty, &());
        Ok(Alloc::alloc(tycker, ConsN(witness_patterns, body_pattern), domain, &()))
    }
}

/// Translate a package pattern while associating its source `PackPi`
/// witnesses with the fresh witnesses introduced by the translated pattern.
pub(super) struct PackPiPatternTranslation {
    pub(super) pattern: VPatId,
    pub(super) layout: PackPiWitnessLayout,
}

impl PackPiPatternTranslation {
    pub(super) fn new(
        pattern: VPatId, signature: &PackPi, tycker: &mut Tycker<'_>, env: &TyEnv,
    ) -> Result<Self> {
        let witnesses = signature.witnesses.iter().copied().collect::<Vec<_>>();
        let expected = witnesses.len();
        let mut state = PackPiPatternLayoutState { witnesses: &witnesses, expected };
        let layout = state.collect(tycker, env, pattern, signature.domain)?;
        if !state.witnesses.is_empty() {
            return state.mismatch(tycker);
        }
        Ok(Self { pattern, layout: PackPiWitnessLayout(layout) })
    }

    /// Number of physical static fields one product component's pattern opens.
    ///
    /// A deep-selected binder distributes the arrow's witnesses across product
    /// components; each component consumes its subtree's demand in source order.
    fn pattern_witness_demand(tycker: &Tycker<'_>, pattern: VPatId) -> usize {
        match tycker.statics.vpats[&pattern].to_owned() {
            | ValuePattern::SCons(ConsN(prefix, body)) => {
                prefix.len() + Self::pattern_witness_demand(tycker, body)
            }
            | ValuePattern::Named(Named(_, inner)) => Self::pattern_witness_demand(tycker, inner),
            | ValuePattern::VCons(patterns) => {
                patterns.iter().map(|pattern| Self::pattern_witness_demand(tycker, *pattern)).sum()
            }
            | ValuePattern::Alias(Alias(patterns)) => {
                patterns.iter().map(|pattern| Self::pattern_witness_demand(tycker, *pattern)).sum()
            }
            | ValuePattern::Hole(_)
            | ValuePattern::Var(_)
            | ValuePattern::Ctor(_)
            | ValuePattern::Lit(_)
            | ValuePattern::Triv(_)
            | ValuePattern::View(_) => 0,
        }
    }

    pub(super) fn translate(
        self, tycker: &mut Tycker<'_>, env: MonEnv,
    ) -> Result<(MonEnv, VPatId)> {
        if self.layout.0.is_empty() {
            return cs::TermLift { tm: self.pattern }.construct(tycker, env);
        }
        let (env, ty) = cs::TypeOf(self.pattern).construct(tycker, env)?;
        let (env, translated_ty) = cs::TypeLift { ty }.construct(tycker, env)?;
        match tycker.statics.vpats[&self.pattern].to_owned() {
            | ValuePattern::Named(Named(name, inner)) => {
                let (env, inner) =
                    Self { pattern: inner, layout: self.layout }.translate(tycker, env)?;
                let named = Alloc::alloc(tycker, Named(name, inner), translated_ty, &());
                Ok((env, named))
            }
            | ValuePattern::SCons(ConsN(witnesses, body)) => package_pattern_translation(
                tycker,
                env,
                &witnesses,
                Some(&self.layout.0),
                body,
                translated_ty,
            ),
            | ValuePattern::Alias(Alias(patterns)) => {
                // A same-bindee group carries its witnesses in one package member;
                // the remaining members translate without a source layout.
                let mut env = env;
                let mut translated = Vec::new();
                for (index, pattern) in patterns.into_iter().enumerate() {
                    let layout = if index == 0 {
                        self.layout.clone()
                    } else {
                        PackPiWitnessLayout(Vec::new())
                    };
                    let (translated_env, pattern) =
                        Self { pattern, layout }.translate(tycker, env)?;
                    env = translated_env;
                    translated.push(pattern);
                }
                let alias = Alias(ConsN::from_vec(translated).unwrap());
                let pattern = Alloc::alloc(tycker, alias, translated_ty, &());
                Ok((env, pattern))
            }
            | ValuePattern::VCons(components) => {
                let Type::Prod(Prod(_)) = tycker.type_filled(&translated_ty)?.to_owned() else {
                    return tycker.err(TyckError::PackageWitnessArityMismatch {
                        expected: self.layout.len(),
                        found: 0,
                    });
                };
                let mut remaining = self.layout.0.as_slice();
                let mut env = env;
                let mut translated = Vec::new();
                for component in components {
                    let demand = Self::pattern_witness_demand(tycker, component);
                    let (slice, rest) = remaining.split_at(demand);
                    let (component_env, component) =
                        Self { pattern: component, layout: PackPiWitnessLayout(slice.to_vec()) }
                            .translate(tycker, env)?;
                    env = component_env;
                    translated.push(component);
                    remaining = rest;
                }
                if !remaining.is_empty() {
                    return tycker.err(TyckError::PackageWitnessArityMismatch {
                        expected: self.layout.len(),
                        found: self.layout.len() - remaining.len(),
                    });
                }
                let pattern =
                    Alloc::alloc(tycker, ValuePattern::VCons(translated), translated_ty, &());
                Ok((env, pattern))
            }
            | ValuePattern::Hole(_)
            | ValuePattern::Var(_)
            | ValuePattern::Ctor(_)
            | ValuePattern::Lit(_)
            | ValuePattern::Triv(_)
            | ValuePattern::View(_) => tycker.err(TyckError::PackageWitnessArityMismatch {
                expected: self.layout.len(),
                found: 0,
            }),
        }
    }
}

/// The continuation passed to the translated codomain structure.
struct PackPiStructureContinuation {
    function: VPatId,
    pub(super) argument: ValueId,
    codomain: TypeId,
    carrier: VPatId,
}

impl Construct<MonEnv, CompuId> for PackPiStructureContinuation {
    fn construct(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let (env, function) =
            App(Force(cs::Value(self.function)), cs::Value(self.carrier)).construct(tycker, env)?;
        let (env, codomain) = cs::TypeLift { ty: self.codomain }.construct(tycker, env)?;
        let application = PackPiApplication { function, argument: self.argument, result: codomain }
            .build(tycker, &env.ty);
        Ok((env, application))
    }
}

/// The package-dependent function produced by `Str(PackPi)`.
struct PackPiStructureBody {
    pub(super) source: TypeId,
    pub(super) signature: PackPi,
    carrier: AbstId,
    monadic_value: VPatId,
    function: VPatId,
}

impl Construct<MonEnv, CompuId> for PackPiStructureBody {
    fn construct(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let source_pattern =
            PackPiBinderPattern { signature: self.signature.clone() }.build(tycker, &env.ty)?;
        let translation =
            PackPiPatternTranslation::new(source_pattern, &self.signature, tycker, &env.ty)?;
        let (env, binder) = translation.translate(tycker, env)?;
        let argument = binder.reify(tycker);
        let continuation = Abs(cs::Pat("z", self.carrier), {
            let function = self.function;
            let codomain = self.signature.codomain;
            move |carrier| PackPiStructureContinuation { function, argument, codomain, carrier }
        });
        let (env, body) = App(
            App(
                App(cs::Structure { ty: self.signature.codomain }, cs::Ty(self.carrier)),
                cs::Value(self.monadic_value),
            ),
            Thunk(continuation),
        )
        .construct(tycker, env)?;
        let (env, signature) = cs::TypeLift { ty: self.source }.construct(tycker, env)?;
        let abstraction = PackPiAbstraction { binder, body, signature }.build(tycker, &env.ty);
        Ok((env, abstraction))
    }
}

/// Structure translation for a package-dependent computation arrow:
///
/// `Str(PackPi(P; As. B)) Z mz f`
/// `  = fn package => Str(B) Z mz { fn z => ! f z package }`.
///
/// Translating `package` also binds the structures associated with `As`, so
/// `Str(B)` is formed in precisely the environment selected by that package.
pub(super) struct PackPiStructureTranslation {
    pub(super) source: TypeId,
    pub(super) signature: PackPi,
}

impl PackPiStructureTranslation {
    pub(super) fn translate(
        self, tycker: &mut Tycker<'_>, env: MonEnv,
    ) -> Result<(MonEnv, CompuId)> {
        let source = self.source;
        let signature = self.signature;
        Abs(cs::Ty(cs::Pat("Z", VType)), move |_carrier_pattern, carrier| {
            Abs(cs::Pat("mz", cs::Thk(App(env.monad_ty, carrier))), move |monadic_value| {
                let function_ty = cs::Thk(Arrow(carrier, cs::TypeLift { ty: source }));
                Abs(cs::Pat("f", function_ty), move |function| PackPiStructureBody {
                    source,
                    signature,
                    carrier,
                    monadic_value,
                    function,
                })
            })
        })
        .construct(tycker, env)
    }
}
