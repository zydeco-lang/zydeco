//! Implementation of the monadic block via algebra translation.
//!
//! See the following links for details:
//! + Core Idea: [section 5.3 of oopsla25](https://dl.acm.org/doi/10.1145/3720434)
//! + Detailed Implementation: [appendix D of the extended version](https://arxiv.org/abs/2502.15031)

use super::{syntax::*, *};
use crate::*;

pub mod syntax {
    /// signature translation
    #[derive(Clone, Copy)]
    pub struct Signature<T> {
        pub ty: T,
    }
    /// structure translation
    #[derive(Clone)]
    pub struct Structure<T> {
        pub ty: T,
    }
    /// type translation (lift)
    #[derive(Clone, Copy)]
    pub struct TypeLift<T> {
        pub ty: T,
    }
    /// term translation (lift)
    #[derive(Clone)]
    pub struct TermLift<T> {
        pub tm: T,
    }

    /// structure pattern introduction
    ///
    /// need to register the fresh var `S` as a structure of a given (abstract) type `A`;
    /// the optional variable definition `T` can be passed
    pub struct StrPat<S, A, T>(pub S, pub A, pub T);

    /// substitute abstract type from `S` to `T`
    pub struct AbstPat<S, T>(pub S, pub T);

    /// copy a type pattern's named shape around a different payload witness
    pub struct ReboundTypePattern {
        pub source: super::TPatId,
        pub witness: super::AbstId,
    }

    /// reconstruct a complete type argument from a pattern and its payload
    pub struct PatternArgument {
        pub pattern: super::TPatId,
        pub payload: super::AbstId,
    }

    /// construct a forall while retaining an explicit type-pattern shape
    pub struct PatternForall<P, T> {
        pub pattern: P,
        pub witness: super::AbstId,
        pub body: T,
    }
}

mod syntax_impl {
    use super::*;

    // SignatureTrans
    impl<T> MonConstruct<TypeId> for cs::Signature<T>
    where
        T: MonConstruct<TypeId>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
            tycker.guarded(|tycker| {
                let cs::Signature { ty } = self;
                let (env, ty) = ty.mbuild(tycker, env)?;

                // administrative
                tycker.tasks.push_back(TyckTask::SignatureGen(ty.into()));
                signature_translation(tycker, env, ty)
            })
        }
    }

    // StructureTrans
    impl<T> MonConstruct<CompuId> for cs::Structure<T>
    where
        T: MonConstruct<TypeId>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
            tycker.guarded(|tycker| {
                let cs::Structure { ty } = self;
                let (env, ty) = ty.mbuild(tycker, env)?;

                // administrative
                tycker.tasks.push_back(TyckTask::StructureGen(ty.into()));
                structure_translation(tycker, env, ty)
            })
        }
    }

    // TypeLift (type pattern translation)
    impl<T> MonConstruct<TPatId> for cs::TypeLift<T>
    where
        T: MonConstruct<TPatId>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TPatId)> {
            tycker.guarded(|tycker| {
                let cs::TypeLift { ty } = self;
                let (env, ty) = ty.mbuild(tycker, env)?;

                // administrative
                tycker.tasks.push_back(TyckTask::MonadicLiftPat(ty.into()));
                type_pattern_translation(tycker, env, ty)
            })
        }
    }

    // TypeLift
    impl<T> MonConstruct<TypeId> for cs::TypeLift<T>
    where
        T: MonConstruct<TypeId>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
            tycker.guarded(|tycker| {
                let cs::TypeLift { ty } = self;
                let (env, ty) = ty.mbuild(tycker, env)?;

                // administrative
                tycker.tasks.push_back(TyckTask::MonadicLiftTerm(ty.into()));
                type_translation(tycker, env, ty)
            })
        }
    }

    // TermLift (value pattern translation)
    impl<T> MonConstruct<VPatId> for cs::TermLift<T>
    where
        T: MonConstruct<VPatId>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, VPatId)> {
            tycker.guarded(|tycker| {
                let cs::TermLift { tm } = self;
                let (env, tm) = tm.mbuild(tycker, env)?;

                // administrative
                tycker.tasks.push_back(TyckTask::MonadicLiftPat(tm.into()));
                value_pattern_translation(tycker, env, tm)
            })
        }
    }

    // TermLift (value translation)
    impl<T> MonConstruct<ValueId> for cs::TermLift<T>
    where
        T: MonConstruct<ValueId>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, ValueId)> {
            tycker.guarded(|tycker| {
                let cs::TermLift { tm } = self;
                let (env, tm) = tm.mbuild(tycker, env)?;

                // administrative
                tycker.tasks.push_back(TyckTask::MonadicLiftTerm(tm.into()));
                value_translation(tycker, env, tm)
            })
        }
    }

    // TermLift (computation translation)
    impl<T> MonConstruct<CompuId> for cs::TermLift<T>
    where
        T: MonConstruct<CompuId>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
            tycker.guarded(|tycker| {
                let cs::TermLift { tm } = self;
                let (env, tm) = tm.mbuild(tycker, env)?;

                // administrative
                tycker.tasks.push_back(TyckTask::MonadicLiftTerm(tm.into()));
                computation_translation(tycker, env, tm)
            })
        }
    }

    // // Elaboration (value)
    // impl<T> MonConstruct<ValueId> for cs::Elaboration<T>
    // where
    //     T: MonConstruct<ValueId>,
    // {
    //     fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, ValueId)> {
    //         let cs::Elaboration { tm } = self;
    //         let (env, tm) = tm.mbuild(tycker, env)?;
    //         value_monadic_elaboration(tycker, env, tm)
    //     }
    // }

    // // Elaboration (computation)
    // impl<T> MonConstruct<CompuId> for cs::Elaboration<T>
    // where
    //     T: MonConstruct<CompuId>,
    // {
    //     fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
    //         let cs::Elaboration { tm } = self;
    //         let (env, tm) = tm.mbuild(tycker, env)?;
    //         computation_monadic_elaboration(tycker, env, tm)
    //     }
    // }

    // StrPat
    impl<S, A, T> MonConstruct<VPatId> for cs::StrPat<S, A, T>
    where
        S: MonConstruct<VarName>,
        A: MonConstruct<AbstId>,
        T: MonConstruct<Option<DefId>>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, VPatId)> {
            let cs::StrPat(var, abst, tvar) = self;
            let (env, abst) = abst.mbuild(tycker, env)?;
            let (env, ty) = cs::Thk(cs::Signature { ty: cs::Type(abst) }).mbuild(tycker, env)?;
            let (env, var) = var.mbuild(tycker, env)?;
            let def = Alloc::alloc(tycker, var, ty.into(), &());
            let (env, tvar) = tvar.mbuild(tycker, env)?;
            let (env, pattern) = cs::Pat(def, ty).mbuild(tycker, env)?;
            let structure = pattern.reify(tycker);
            let mut env = env;
            env.structure.absts.insert(abst, structure);
            if let Some(tvar) = tvar {
                env.structure.def_map.insert(tvar, abst);
            }
            Ok((env, pattern))
        }
    }

    impl MonConstruct<TypeId> for cs::PatternArgument {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
            let payload = env.subst_abst.get(&self.payload).copied().unwrap_or(self.payload);
            let payload_kind = tycker.statics.annotations_abst[&payload];
            let payload = Alloc::alloc(tycker, payload, payload_kind, &env.ty);
            let argument = self.pattern.introduce_payload(tycker, payload)?;
            Ok((env, argument))
        }
    }

    // AbstPat
    impl<T> MonConstruct<AbstId> for cs::AbstPat<AbstId, T>
    where
        T: MonConstruct<AbstId>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, AbstId)> {
            let cs::AbstPat(old, new) = self;
            let (env, new) = new.mbuild(tycker, env)?;
            tycker.transfer_builtin_role(old, new)?;
            let mut env = env;
            env.subst_abst.insert(old, new);
            Ok((env, new))
        }
    }

    impl MonConstruct<TPatId> for cs::ReboundTypePattern {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TPatId)> {
            use zydeco_utils::arena::ArenaAccess;

            let witness = env.subst_abst.get(&self.witness).copied().unwrap_or(self.witness);
            let payload = tycker.statics.abst_hints.get(&witness).copied();
            let pattern = self.source.rebind_payload(tycker, payload, &env.ty);
            Ok((env, pattern))
        }
    }

    impl<P, T> MonConstruct<TypeId> for cs::PatternForall<P, T>
    where
        P: MonConstruct<TPatId>,
        T: MonConstruct<TypeId>,
    {
        fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
            let (env, pattern) = self.pattern.mbuild(tycker, env)?;
            let witness = env.subst_abst.get(&self.witness).copied().unwrap_or(self.witness);
            let (env, body) = self.body.mbuild(tycker, env)?;
            let (env, ctype) = CType.mbuild(tycker, env)?;
            let binder = TypeBinder { pattern, witness };
            let forall = Alloc::alloc(tycker, Forall(binder, body), ctype, &env.ty);
            Ok((env, forall))
        }
    }
}

/// Signature Translation `Sig_K(T)`
///
/// Given the monad type `M` and the type environment, implement the signature
/// translation from input type `T` to an output computation type `CompuId`
/// which is the type of algebra of `T`. Specifically,
///
/// + `T: VType` -> `Top`
/// + `T: CType` -> `Algebra M T`
/// + `T: K_1 -> K_2` -> `forall X: K_1 . Thk (Sig_K_1(X)) -> Sig_K_2(T X)`
fn signature_translation(tycker: &mut Tycker, env: MonEnv, ty: TypeId) -> Result<(MonEnv, TypeId)> {
    let (env, kd) = cs::TypeOf(ty).mbuild(tycker, env)?;
    let res = match tycker.kind_filled(&kd)?.to_owned() {
        | Kind::VType(VType) => cs::TopTy.mbuild(tycker, env)?,
        | Kind::CType(CType) => cs::Algebra(env.monad_ty, ty).mbuild(tycker, env)?,
        | Kind::Label(Label(name, payload_kind)) => {
            let payload = ty.project_named(tycker, &name, payload_kind)?;
            signature_translation(tycker, env, payload)?
        }
        | Kind::Arrow(Arrow(kd_1, _)) => cs::Forall(cs::Ann(None, kd_1), |abst| {
            let ty_1 = cs::Ann(abst, kd_1);
            Arrow(cs::Thk(cs::Signature { ty: ty_1 }), cs::Signature { ty: App(ty, ty_1) })
        })
        .mbuild(tycker, env)?,
    };
    Ok(res)
}

/// Structure translation for a codata type.
///
/// The source codata directs the recursive structure translations of its
/// destructor signatures, while the translated codata is the interface
/// inhabited by the resulting `comatch`.
struct CoDataStructureTranslation {
    source: TypeId,
    source_codata: CoDataId,
}

impl MonConstruct<CompuId> for CoDataStructureTranslation {
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Self { source, source_codata } = self;
        let source_arms = tycker.statics.codatas[&source_codata].clone();
        let (env, target) = cs::TypeLift { ty: source }.mbuild(tycker, env)?;
        let target_view = target.unroll(tycker)?;
        let Type::CoData(target_codata) = tycker.type_filled(&target_view)?.to_owned() else {
            unreachable!("translated codata type must remain codata")
        };
        let monad_ty = env.monad_ty;

        Abs(cs::Ty(cs::Pat("Z", VType)), move |_tvar, carrier| {
            let monadic_value_ty = cs::Thk(App(monad_ty, carrier));
            Abs(cs::Pat("mz", monadic_value_ty), move |monadic_value: VPatId| {
                let function_ty = cs::Thk(Arrow(carrier, target));
                let source_arms = source_arms.clone();
                Abs(cs::Pat("f", function_ty), move |function: VPatId| {
                    cs::CoMatch(target_codata, move |dtor, _target_signature| {
                        let source_signature = source_arms
                            .get(&dtor)
                            .expect("translated codata must preserve its destructors");
                        let continuation = Abs(cs::Pat("z", carrier), move |value: VPatId| {
                            Dtor(App(Force(cs::Value(function)), cs::Value(value)), dtor)
                        });
                        let structure = cs::Structure { ty: source_signature };
                        App(
                            App(App(structure, cs::Ty(carrier)), cs::Value(monadic_value)),
                            Thunk(continuation),
                        )
                    })
                })
            })
        })
        .mbuild(tycker, env)
    }
}

/// Allocate a computation abstraction whose parameter opens the witnesses
/// bound by a package-dependent arrow.
struct PackPiAbstraction {
    binder: VPatId,
    body: CompuId,
    signature: TypeId,
}

impl PackPiAbstraction {
    fn build(self, tycker: &mut Tycker<'_>, env: &TyEnv) -> CompuId {
        Alloc::alloc(tycker, Abs(self.binder, self.body), self.signature, env)
    }
}

/// Allocate an application after its package-dependent result has been
/// instantiated.
struct PackPiApplication {
    function: CompuId,
    argument: ValueId,
    result: TypeId,
}

impl PackPiApplication {
    fn build(self, tycker: &mut Tycker<'_>, env: &TyEnv) -> CompuId {
        Alloc::alloc(tycker, App(self.function, self.argument), self.result, env)
    }
}

/// Construct a source package pattern that opens exactly the witnesses bound
/// by a `PackPi`. The pattern is used only as input to monadic translation.
struct PackPiBinderPattern {
    signature: PackPi,
}

#[derive(Clone, Copy)]
enum PackPiWitnessLayoutEntry {
    KindManifest,
    Abstract(AbstId),
    TypeManifest,
}

enum PackagePatternStructure {
    Abstract,
    Manifest,
}

#[derive(Clone)]
struct PackPiWitnessLayout(Vec<PackPiWitnessLayoutEntry>);

struct PackPiWitnessLayoutState<'a> {
    domain: TypeId,
    witnesses: &'a [AbstId],
    expected: usize,
}

impl<'a> PackPiWitnessLayoutState<'a> {
    fn collect(
        self, tycker: &mut Tycker<'_>, env: &TyEnv,
    ) -> Result<Vec<PackPiWitnessLayoutEntry>> {
        let view = self.domain.unroll(tycker)?.subst_env(tycker, env)?;
        match tycker.type_filled(&view)?.to_owned() {
            | Type::ManifestKind(ManifestKind { body, .. }) => {
                let tail = Self { domain: body, ..self }.collect(tycker, env)?;
                Ok(std::iter::once(PackPiWitnessLayoutEntry::KindManifest).chain(tail).collect())
            }
            | Type::Exists(Exists { binder, mode, body }) => {
                let (entry, witnesses, payload) = match mode {
                    | ExistsMode::Abstract => {
                        let Some((&witness, witnesses)) = self.witnesses.split_first() else {
                            return self.mismatch(tycker);
                        };
                        let kind = tycker.statics.annotations_abst[&witness];
                        (
                            PackPiWitnessLayoutEntry::Abstract(witness),
                            witnesses,
                            Alloc::alloc(tycker, witness, kind, env),
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
    }

    #[track_caller]
    fn mismatch<T>(&self, tycker: &mut Tycker<'_>) -> Result<T> {
        tycker.err(
            TyckError::PackageWitnessArityMismatch {
                expected: self.expected,
                found: self.expected - self.witnesses.len(),
            },
            std::panic::Location::caller(),
        )
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

impl PackPiBinderPattern {
    fn build(self, tycker: &mut Tycker<'_>, env: &TyEnv) -> Result<VPatId> {
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
                            VarName(format!("pack_kind_{index}")),
                            AnnId::Set,
                            &(),
                        );
                        let pattern_env = env.clone() + [(witness_def, AnnId::Kind(definition))];
                        let pattern = Alloc::alloc(tycker, witness_def, (), &pattern_env);
                        witness_patterns.push(StaticPatId::Kind(pattern));
                        Ok((body, witness_patterns))
                    }
                    | (
                        entry @ (PackPiWitnessLayoutEntry::Abstract(_)
                        | PackPiWitnessLayoutEntry::TypeManifest),
                        Type::Exists(Exists { binder, mode, body }),
                    ) => {
                        let kind = binder.payload_kind(tycker);
                        let witness_def = Alloc::alloc(
                            tycker,
                            VarName(format!("pack_witness_{index}")),
                            kind.into(),
                            &(),
                        );
                        witness_patterns.push(StaticPatId::Type(binder.pattern.rebind_payload(
                            tycker,
                            Some(witness_def),
                            env,
                        )));
                        let payload = match (entry, mode) {
                            | (
                                PackPiWitnessLayoutEntry::Abstract(witness),
                                ExistsMode::Abstract,
                            ) => Alloc::alloc(tycker, witness, kind, env),
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

        let body_def = Alloc::alloc(tycker, VarName("pack_body".to_string()), body_ty.into(), &());
        let body_pattern: VPatId = Alloc::alloc(tycker, body_def, body_ty, env);
        Ok(Alloc::alloc(tycker, ConsN(witness_patterns, body_pattern), domain, env))
    }
}

/// Translate a package pattern while associating its source `PackPi`
/// witnesses with the fresh witnesses introduced by the translated pattern.
struct PackPiPatternTranslation {
    pattern: VPatId,
    layout: PackPiWitnessLayout,
}

impl PackPiPatternTranslation {
    fn new(
        pattern: VPatId, signature: &PackPi, tycker: &mut Tycker<'_>, env: &TyEnv,
    ) -> Result<Self> {
        Ok(Self { pattern, layout: PackPiWitnessLayout::new(signature, tycker, env)? })
    }

    fn translate(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, VPatId)> {
        let (env, ty) = cs::TypeOf(self.pattern).mbuild(tycker, env)?;
        let (env, translated_ty) = cs::TypeLift { ty }.mbuild(tycker, env)?;
        match tycker.statics.vpats[&self.pattern].to_owned() {
            | ValuePattern::Named(Named(name, inner)) => {
                let (env, inner) =
                    Self { pattern: inner, layout: self.layout }.translate(tycker, env)?;
                let named = Alloc::alloc(tycker, Named(name, inner), translated_ty, &env.ty);
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
            | ValuePattern::Hole(_)
            | ValuePattern::Var(_)
            | ValuePattern::Ctor(_)
            | ValuePattern::Triv(_)
            | ValuePattern::VCons(_) => tycker.err(
                TyckError::PackageWitnessArityMismatch { expected: self.layout.len(), found: 0 },
                std::panic::Location::caller(),
            ),
        }
    }
}

/// The continuation passed to the translated codomain structure.
struct PackPiStructureContinuation {
    function: VPatId,
    argument: ValueId,
    codomain: TypeId,
    carrier: VPatId,
}

impl MonConstruct<CompuId> for PackPiStructureContinuation {
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let (env, function) =
            App(Force(cs::Value(self.function)), cs::Value(self.carrier)).mbuild(tycker, env)?;
        let (env, codomain) = cs::TypeLift { ty: self.codomain }.mbuild(tycker, env)?;
        let application = PackPiApplication { function, argument: self.argument, result: codomain }
            .build(tycker, &env.ty);
        Ok((env, application))
    }
}

/// The package-dependent function produced by `Str(PackPi)`.
struct PackPiStructureBody {
    source: TypeId,
    signature: PackPi,
    carrier: AbstId,
    monadic_value: VPatId,
    function: VPatId,
}

impl MonConstruct<CompuId> for PackPiStructureBody {
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
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
        .mbuild(tycker, env)?;
        let (env, signature) = cs::TypeLift { ty: self.source }.mbuild(tycker, env)?;
        let abstraction = PackPiAbstraction { binder, body, signature }.build(tycker, &env.ty);
        Ok((env, abstraction))
    }
}

/// Structure translation for a package-dependent computation arrow:
///
/// `Str(PackPi(P; As. B)) Z mz f`
/// `  = fn package -> Str(B) Z mz { fn z -> ! f z package }`.
///
/// Translating `package` also binds the structures associated with `As`, so
/// `Str(B)` is formed in precisely the environment selected by that package.
struct PackPiStructureTranslation {
    source: TypeId,
    signature: PackPi,
}

impl PackPiStructureTranslation {
    fn translate(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
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
        .mbuild(tycker, env)
    }
}

/// Resolve a recorded structure for a type reference, synthesizing the unique
/// trivial structure when the reference has value kind.
struct ReferencedStructure {
    ty: TypeId,
    structure: Option<ValueId>,
}

impl MonConstruct<CompuId> for ReferencedStructure {
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        if let Some(structure) = self.structure {
            return Force(structure).mbuild(tycker, env);
        }

        let (env, kind) = cs::TypeOf(self.ty).mbuild(tycker, env)?;
        match tycker.kind_filled(&kind)?.to_owned() {
            | Kind::VType(VType) => cs::Top.mbuild(tycker, env),
            | _ => tycker.err(TyckError::MissingStructure(self.ty), std::panic::Location::caller()),
        }
    }
}

/// Structure Translation `Str(T)`
fn structure_translation(
    tycker: &mut Tycker, env: MonEnv, ty: TypeId,
) -> Result<(MonEnv, CompuId)> {
    let monad_impl = env.monad_impl;
    let res = match tycker.type_filled(&ty)? {
        | Type::Var(def) => ReferencedStructure {
            ty,
            structure: env
                .structure
                .def_map
                .get(&def)
                .and_then(|abst| env.structure.absts.get(abst))
                .copied(),
        }
        .mbuild(tycker, env)?,
        | Type::Abst(abst) => {
            let abst = match env.subst_abst.get(&abst).cloned() {
                | Some(new_abst) => new_abst,
                | None => abst,
            };
            ReferencedStructure { ty, structure: env.structure.absts.get(&abst).copied() }
                .mbuild(tycker, env)?
        }
        | Type::Abs(ty) => {
            // input: fn (X : K) -> S
            let Abs(tpat, ty) = ty;
            let svar = {
                let (tvar, _) = tpat.try_destruct_def(tycker);
                match tvar {
                    | Some(tvar) => format!("str_{}", tycker.scoped.defs[&tvar].plain()),
                    | None => "str".to_string(),
                }
            };
            // output: fn (X : K) (str_X : Thk (Sig_K(X))) -> Str(S)
            Abs(cs::Ty(cs::TypeLift { ty: tpat }), move |_tvar, abst| {
                Abs(cs::StrPat(svar, abst, None), move |_str: VPatId| cs::Structure { ty })
            })
            .mbuild(tycker, env)?
        }
        | Type::App(ty) => {
            // input: S_f S_a
            let App(ty_f, ty_a) = ty;
            // output: Str(S_f) Lift(S_a) { Str(S_a) }
            let str_f = cs::Structure { ty: ty_f };
            let ty_a_lift = cs::TypeLift { ty: ty_a };
            let str_a = cs::Structure { ty: ty_a };
            App(App(str_f, cs::Ty(ty_a_lift)), Thunk(str_a)).mbuild(tycker, env)?
        }
        // primitive types are not allowed in monadic blocks
        | Type::Int(_) | Type::Char(_) | Type::String(_) => unreachable!(),
        // unit, product, and existential types have the trivial structure `top`
        // (so should data types)
        | Type::Named(Named(_, inner)) | Type::Proj(Proj(inner, _)) => {
            cs::Structure { ty: inner }.mbuild(tycker, env)?
        }
        | Type::Label(_)
        | Type::Unit(UnitTy)
        | Type::Prod(_)
        | Type::Exists(_)
        | Type::ManifestKind(_)
        | Type::Data(_) => cs::Top.mbuild(tycker, env)?,
        // the thunk type is itself a type constructor,
        // so its structure takes a type and the type's structure as arguments
        | Type::Thk(ThkTy) => {
            // output: fn (X : CType) (_ : Thk (Sig_CType(X))) -> <top>
            Abs(cs::Ty(cs::Pat("_", CType)), move |_tvar, abst| {
                let thk_sig = cs::Thk(cs::Signature { ty: cs::Type(abst) });
                Abs(cs::Pat("_", thk_sig), move |_var| {
                    // <top> = comatch end
                    cs::Top
                })
            })
            .mbuild(tycker, env)?
        }
        // the os type is a primitive computation type and thus not allowed in monadic blocks
        | Type::OS(_) => unreachable!(),
        | Type::Ret(RetTy) => {
            // output: fn (X : VType) (_ : Thk (Sig_VType(X))) -> <monadic_bind>
            Abs(cs::Ty(cs::Pat("_", VType)), |_tvar, abst_x| {
                let thk_sig = cs::Thk(cs::Signature { ty: abst_x });
                Abs(cs::Pat("_", thk_sig), move |_var| {
                    // <monadic_bind> = fn (Z : VType) -> ! monad_impl .bind Z X
                    Abs(cs::Ty(cs::Pat("Z", VType)), move |_tvar, abst_z| {
                        let body = cs::Dtor(Force(monad_impl), ".bind");
                        App(App(body, cs::Ty(abst_z)), cs::Ty(abst_x))
                    })
                })
            })
            .mbuild(tycker, env)?
        }
        | Type::CoData(source_codata) => {
            CoDataStructureTranslation { source: ty, source_codata }.mbuild(tycker, env)?
        }
        | Type::Arrow(ty) => {
            // input: A -> B
            let Arrow(ty_a, ty_b) = ty;
            // output: fn (Z : VType) (mz : Thk (M Z)) (f : Thk (Z -> [A] -> [B])) -> <body>
            Abs(cs::Ty(cs::Pat("Z", VType)), move |_tvar, abst_z| {
                let mz_ty = cs::Thk(App(env.monad_ty, abst_z));
                Abs(cs::Pat("mz", mz_ty), move |mz: VPatId| {
                    let ty_a_ = cs::TypeLift { ty: ty_a };
                    let ty_b_ = cs::TypeLift { ty: ty_b };
                    let f_ty = cs::Thk(Arrow(abst_z, Arrow(ty_a_, ty_b_)));
                    Abs(cs::Pat("f", f_ty), move |f: VPatId| {
                        // <body> = fn (x : [A]) -> Str(B) Z mz { fn (z : Z) -> ! f z x }
                        Abs(cs::Pat("x", ty_a_), move |x: VPatId| {
                            let alg_b = cs::Structure { ty: ty_b };
                            let kont = Abs(cs::Pat("z", abst_z), move |z: VPatId| {
                                App(App(Force(cs::Value(f)), cs::Value(z)), cs::Value(x))
                            });
                            App(App(App(alg_b, cs::Ty(abst_z)), cs::Value(mz)), Thunk(kont))
                        })
                    })
                })
            })
            .mbuild(tycker, env)?
        }
        | Type::Forall(ty_forall) => {
            // input: forall (X : K) . B
            let Forall(binder, ty) = ty_forall;
            let abst = binder.witness;
            let source_pattern = binder.pattern;
            let kd = cs::TypeOf(abst);
            // output: fn (Z : VType) (mz : Thk (M Z)) (f : <f_ty>) -> <body>
            Abs(cs::Ty(cs::Pat("Z", VType)), move |_tvar, abst_z| {
                Abs(cs::Pat("mz", cs::Thk(App(env.monad_ty, abst_z))), move |mz: VPatId| {
                    // construct abstract type X first
                    // substitute the abstract type `abst` with `abst_x`
                    cs::CBind::new(cs::AbstPat(abst, cs::Ann("X", kd)), move |abst_x: AbstId| {
                        // <f_ty> = Thk (Z -> forall (X : K) . Thk (Sig_K(X)) -> [B])
                        let f_ty = cs::Thk(Arrow(
                            abst_z,
                            cs::PatternForall {
                                pattern: cs::ReboundTypePattern {
                                    source: source_pattern,
                                    witness: abst_x,
                                },
                                witness: abst_x,
                                // substitute the abstract type `abst` in the `ty` with `abst_x`
                                body: Arrow(
                                    cs::Thk(cs::Signature { ty: abst_x }),
                                    cs::TypeLift { ty },
                                ),
                            },
                        ));
                        Abs(cs::Pat("f", f_ty), move |f: VPatId| {
                            // <body> = fn (X : K) (str_X : Thk (Sig_K(X))) -> Str(B) Z mz <kont>
                            Abs(
                                cs::Ty((
                                    cs::ReboundTypePattern {
                                        source: source_pattern,
                                        witness: abst_x,
                                    },
                                    abst_x,
                                )),
                                move |_, abst_x: AbstId| {
                                    Abs(cs::StrPat("str_X", abst_x, None), move |str_x: VPatId| {
                                        // <kont> = { fn (z : Z) -> ! f z X str_X }
                                        let kont = Abs(cs::Pat("z", abst_z), move |z: VPatId| {
                                            let f = cs::Value(f);
                                            let z = cs::Value(z);
                                            let str_x = cs::Value(str_x);
                                            let argument = cs::PatternArgument {
                                                pattern: source_pattern,
                                                payload: abst_x,
                                            };
                                            App(App(App(Force(f), z), cs::Ty(argument)), str_x)
                                        });
                                        // substitute `abst` in `ty` with `abst_x`
                                        let str_ = cs::Structure { ty };
                                        App(
                                            App(App(str_, cs::Ty(abst_z)), cs::Value(mz)),
                                            Thunk(kont),
                                        )
                                    })
                                },
                            )
                        })
                    })
                })
            })
            .mbuild(tycker, env)?
        }
        | Type::PackPi(signature) => {
            PackPiStructureTranslation { source: ty, signature }.translate(tycker, env)?
        }
    };
    Ok(res)
}

/// Type Pattern Translation `[TPat]`
fn type_pattern_translation(
    tycker: &mut Tycker, env: MonEnv, tpat: TPatId,
) -> Result<(MonEnv, TPatId)> {
    use TypePattern as TPat;
    let (env, kd) = cs::TypeOf(tpat).mbuild(tycker, env)?;
    let (env, tpat_) = match tycker.statics.tpats[&tpat].to_owned() {
        | TPat::Hole(hole) => cs::Pat(hole, kd).mbuild(tycker, env)?,
        | TPat::Var(def) => cs::Pat(def, kd).mbuild(tycker, env)?,
        | TPat::Named(Named(name, inner)) => {
            let (env, inner) = type_pattern_translation(tycker, env, inner)?;
            let pattern = Alloc::alloc(tycker, Named(name, inner), kd, &env.ty);
            (env, pattern)
        }
    };
    Ok((env, tpat_))
}

fn kind_pattern_translation(
    tycker: &mut Tycker, env: MonEnv, pattern: KPatId, definition: KindId,
) -> Result<(MonEnv, KPatId)> {
    match tycker.statics.kpats[&pattern].to_owned() {
        | KindPattern::Hole(Hole) => {
            let pattern = Alloc::alloc(tycker, Hole, (), &env.ty);
            Ok((env, pattern))
        }
        | KindPattern::Var(source) => {
            use zydeco_surface::scoped::arena::ArenaScoped;

            let variable = tycker.scoped.def(&source);
            let target = Alloc::alloc(tycker, variable, AnnId::Set, &());
            let mut env = env;
            env.subst += [(source, target)];
            env.ty += [(target, AnnId::Kind(definition))];
            let pattern = Alloc::alloc(tycker, target, (), &env.ty);
            Ok((env, pattern))
        }
    }
}

/// Carrier (Type) Translation `[T]`
fn type_translation(tycker: &mut Tycker, env: MonEnv, ty: TypeId) -> Result<(MonEnv, TypeId)> {
    let monad_ty = env.monad_ty;
    let (env, kd) = cs::TypeOf(ty).mbuild(tycker, env)?;
    let (env, res) = match tycker.type_filled(&ty)?.to_owned() {
        | Type::Var(def) => {
            // substitute according to the environment
            let Some(def_) = env.subst.get(&def).cloned() else {
                tycker.err(TyckError::NotInlinable(def), std::panic::Location::caller())?
            };
            let alloc = Alloc::alloc(tycker, def_, kd, &env.ty);
            (env, alloc)
        }
        | Type::Abst(abst) => {
            // only types that are not sealed are allowed here
            use zydeco_utils::arena::ArenaAccess;
            match tycker.statics.seals.get(&abst) {
                | Some(_) => {
                    tycker.err(TyckError::NotInlinableSeal(abst), std::panic::Location::caller())?
                }
                | None => match env.subst_abst.get(&abst).cloned() {
                    | Some(new) => {
                        let alloc = Alloc::alloc(tycker, new, kd, &env.ty);
                        (env, alloc)
                    }
                    | None => {
                        // log::warn!(
                        //     "carrier translation of {} may leak",
                        //     tycker.dump_statics(abst)
                        // );
                        let alloc = Alloc::alloc(tycker, abst, kd, &env.ty);
                        (env, alloc)
                    }
                },
            }
        }
        // | Type::Abst(_abst) => unreachable!(),
        | Type::Abs(ty) => {
            let Abs(tpat, ty) = ty;
            // the environment is bound by type pattern lift
            Abs(cs::TypeLift { ty: tpat }, |_, _, _| cs::TypeLift { ty }).mbuild(tycker, env)?
        }
        | Type::App(ty) => {
            let App(ty_f, ty_a) = ty;
            let ty_f_ = cs::TypeLift { ty: ty_f };
            let ty_a_ = cs::TypeLift { ty: ty_a };
            App(ty_f_, ty_a_).mbuild(tycker, env)?
        }
        | Type::Named(ty) => {
            let Named(name, inner) = ty;
            let (env, inner) = cs::TypeLift { ty: inner }.mbuild(tycker, env)?;
            let named = Alloc::alloc(tycker, Named(name, inner), kd, &env.ty);
            (env, named)
        }
        | Type::Label(ty) => {
            let Label(name, inner) = ty;
            let (env, inner) = cs::TypeLift { ty: inner }.mbuild(tycker, env)?;
            let label = Alloc::alloc(tycker, Label(name, inner), kd, &env.ty);
            (env, label)
        }
        | Type::Proj(ty) => {
            let Proj(head, name) = ty;
            let (env, head) = cs::TypeLift { ty: head }.mbuild(tycker, env)?;
            let projected = head.project_named(tycker, &name, kd)?;
            (env, projected)
        }
        | Type::Thk(ThkTy) => {
            let alloc = Alloc::alloc(tycker, ThkTy, kd, &env.ty);
            (env, alloc)
        }
        // primitive types are not allowed in monadic blocks
        | ty @ (Type::Int(_) | Type::Char(_) | Type::String(_)) => {
            let def = {
                if let Type::Int(_) = ty {
                    tycker.prim.int.get().to_owned()
                } else if let Type::Char(_) = ty {
                    tycker.prim.char.get().to_owned()
                } else if let Type::String(_) = ty {
                    tycker.prim.string.get().to_owned()
                } else {
                    unreachable!()
                }
            };
            tycker.err(TyckError::NotInlinable(def), std::panic::Location::caller())?
        }
        | Type::Data(data) => {
            cs::Data(data, |_ctor, ty| cs::TypeLift { ty }).mbuild(tycker, env)?
        }
        | Type::Unit(UnitTy) => UnitTy.mbuild(tycker, env)?,
        | Type::Prod(ty) => {
            let Prod(ty_1, ty_2) = ty;
            let ty_1_ = cs::TypeLift { ty: ty_1 };
            let ty_2_ = cs::TypeLift { ty: ty_2 };
            Prod(ty_1_, ty_2_).mbuild(tycker, env)?
        }
        | Type::Exists(ty) => {
            let Exists { binder, mode, body } = ty;
            let (env, pattern) = type_pattern_translation(tycker, env, binder.pattern)?;
            let witness = env.subst_abst.get(&binder.witness).copied().unwrap_or(binder.witness);
            let witness_ty = Alloc::alloc(tycker, witness, binder.payload_kind(tycker), &env.ty);
            let (env, mode, structure_ty) = match mode {
                | ExistsMode::Abstract => (env, ExistsMode::Abstract, witness_ty),
                | ExistsMode::Manifest(definition) => {
                    let (env, definition) = cs::TypeLift { ty: definition }.mbuild(tycker, env)?;
                    (env, ExistsMode::Manifest(definition), definition)
                }
            };
            let (env, structure) =
                cs::Thk(cs::Signature { ty: structure_ty }).mbuild(tycker, env)?;
            let (env, body) = cs::TypeLift { ty: body }.mbuild(tycker, env)?;
            let (env, body) = Prod(structure, body).mbuild(tycker, env)?;
            let (env, vtype) = VType.mbuild(tycker, env)?;
            let binder = TypeBinder { pattern, witness };
            let exists = Alloc::alloc(tycker, Exists { binder, mode, body }, vtype, &env.ty);
            (env, exists)
        }
        | Type::ManifestKind(manifest) => {
            let ManifestKind { binder, definition, body } = manifest;
            let (env, body) = cs::TypeLift { ty: body }.mbuild(tycker, env)?;
            let manifest =
                Alloc::alloc(tycker, ManifestKind { binder, definition, body }, kd, &env.ty);
            (env, manifest)
        }
        // os type is also not allowed in monadic blocks
        | Type::OS(_) => unreachable!(),
        // the return type is translated to the provided monad type
        | Type::Ret(RetTy) => (env, monad_ty),
        | Type::CoData(coda) => {
            cs::CoData(coda, |_dtor, ty| cs::TypeLift { ty }).mbuild(tycker, env)?
        }
        | Type::Arrow(ty) => {
            let Arrow(ty_1, ty_2) = ty;
            let ty_1_ = cs::TypeLift { ty: ty_1 };
            let ty_2_ = cs::TypeLift { ty: ty_2 };
            Arrow(ty_1_, ty_2_).mbuild(tycker, env)?
        }
        | Type::Forall(ty) => {
            let Forall(binder, ty) = ty;
            let (env, pattern) = type_pattern_translation(tycker, env, binder.pattern)?;
            let witness = env.subst_abst.get(&binder.witness).copied().unwrap_or(binder.witness);
            let (env, structure) = cs::Thk(cs::Signature { ty: witness }).mbuild(tycker, env)?;
            let (env, body) = cs::TypeLift { ty }.mbuild(tycker, env)?;
            let (env, body) = Arrow(structure, body).mbuild(tycker, env)?;
            let (env, ctype) = CType.mbuild(tycker, env)?;
            let binder = TypeBinder { pattern, witness };
            let forall = Alloc::alloc(tycker, Forall(binder, body), ctype, &env.ty);
            (env, forall)
        }
        | Type::PackPi(pack_pi) => {
            let PackPi { domain, witnesses, codomain } = pack_pi;
            let (env, domain) = cs::TypeLift { ty: domain }.mbuild(tycker, env)?;
            let witnesses =
                witnesses.map(|witness| env.subst_abst.get(&witness).copied().unwrap_or(witness));
            let (env, codomain) = cs::TypeLift { ty: codomain }.mbuild(tycker, env)?;
            let alloc = Alloc::alloc(tycker, PackPi { domain, witnesses, codomain }, kd, &env.ty);
            (env, alloc)
        }
    };
    Ok((env, res))
}

/// Value Pattern Translation `[VPat]`
fn package_pattern_translation(
    tycker: &mut Tycker, env: MonEnv, witnesses: &[StaticPatId],
    source_layout: Option<&[PackPiWitnessLayoutEntry]>, body: VPatId, translated_ty: TypeId,
) -> Result<(MonEnv, VPatId)> {
    let Some((&witness, remaining)) = witnesses.split_first() else {
        if let Some(source_layout) = source_layout
            && !source_layout.is_empty()
        {
            return tycker.err(
                TyckError::PackageWitnessArityMismatch { expected: source_layout.len(), found: 0 },
                std::panic::Location::caller(),
            );
        }
        return cs::TermLift { tm: body }.mbuild(tycker, env);
    };
    let (source_entry, remaining_source_layout) = match source_layout {
        | Some(source_layout) => {
            let Some((&source_entry, remaining)) = source_layout.split_first() else {
                return tycker.err(
                    TyckError::PackageWitnessArityMismatch { expected: witnesses.len(), found: 0 },
                    std::panic::Location::caller(),
                );
            };
            (Some(source_entry), Some(remaining))
        }
        | None => (None, None),
    };

    match witness {
        | StaticPatId::Kind(witness) => {
            let Type::ManifestKind(ManifestKind { definition, body: translated_tail_ty, .. }) =
                tycker.type_filled(&translated_ty)?.to_owned()
            else {
                return tycker.err(
                    TyckError::PackageWitnessArityMismatch { expected: witnesses.len(), found: 0 },
                    std::panic::Location::caller(),
                );
            };
            if !matches!(source_entry, None | Some(PackPiWitnessLayoutEntry::KindManifest)) {
                return tycker.err(
                    TyckError::PackageWitnessArityMismatch { expected: witnesses.len(), found: 0 },
                    std::panic::Location::caller(),
                );
            }
            let (env, witness) = kind_pattern_translation(tycker, env, witness, definition)?;
            let (env, body) = package_pattern_translation(
                tycker,
                env,
                remaining,
                remaining_source_layout,
                body,
                translated_tail_ty,
            )?;
            let package = Alloc::alloc(
                tycker,
                ConsN(vec![StaticPatId::Kind(witness)], body),
                translated_ty,
                &env.ty,
            );
            Ok((env, package))
        }
        | StaticPatId::Type(witness) => {
            let Type::Exists(Exists {
                binder: translated_binder,
                mode: translated_mode,
                body: translated_body_ty,
            }) = tycker.type_filled(&translated_ty)?.to_owned()
            else {
                return tycker.err(
                    TyckError::PackageWitnessArityMismatch { expected: witnesses.len(), found: 0 },
                    std::panic::Location::caller(),
                );
            };
            let domain_abst = translated_binder.witness;
            let (env, witness) = cs::TypeLift { ty: witness }.mbuild(tycker, env)?;
            let (witness_var, _) = witness.try_destruct_def(tycker);
            let mut env = env;
            let (pattern_abst, bound_ty, structure) = match (source_entry, translated_mode) {
                | (
                    Some(PackPiWitnessLayoutEntry::Abstract(source_skolem)),
                    ExistsMode::Abstract,
                ) => {
                    let pattern_abst = Alloc::alloc(tycker, witness, (), &());
                    tycker.transfer_builtin_role(source_skolem, pattern_abst)?;
                    tycker.statics.existential_skolems.ensure(pattern_abst);
                    env.ty = env.ty.with_skolem(pattern_abst);
                    env.subst_abst.insert(source_skolem, pattern_abst);
                    let (env_, abst_ty) = cs::Type(pattern_abst).mbuild(tycker, env)?;
                    env = env_;
                    (pattern_abst, abst_ty, PackagePatternStructure::Abstract)
                }
                | (None, ExistsMode::Abstract) => {
                    let (env_, abst_ty) = cs::Type(domain_abst).mbuild(tycker, env)?;
                    env = env_;
                    (domain_abst, abst_ty, PackagePatternStructure::Abstract)
                }
                | (
                    Some(PackPiWitnessLayoutEntry::TypeManifest),
                    ExistsMode::Manifest(definition),
                )
                | (None, ExistsMode::Manifest(definition)) => {
                    (domain_abst, definition, PackagePatternStructure::Manifest)
                }
                | (
                    Some(
                        PackPiWitnessLayoutEntry::KindManifest
                        | PackPiWitnessLayoutEntry::Abstract(_),
                    ),
                    ExistsMode::Manifest(_),
                )
                | (
                    Some(
                        PackPiWitnessLayoutEntry::KindManifest
                        | PackPiWitnessLayoutEntry::TypeManifest,
                    ),
                    ExistsMode::Abstract,
                ) => {
                    return tycker.err(
                        TyckError::PackageWitnessArityMismatch {
                            expected: witnesses.len(),
                            found: 0,
                        },
                        std::panic::Location::caller(),
                    );
                }
            };
            let translated_body_ty = if pattern_abst == domain_abst {
                translated_body_ty
            } else {
                translated_body_ty.subst_abst(tycker, (domain_abst, bound_ty))?
            };
            let Type::Prod(Prod(structure_ty, translated_tail_ty)) =
                tycker.type_filled(&translated_body_ty)?.to_owned()
            else {
                unreachable!("translated existential body must contain its structure")
            };
            if let Some(witness_var) = witness_var {
                env.ty += [(witness_var, bound_ty.into())];
            }
            let (env, structure) = match structure {
                | PackagePatternStructure::Abstract => {
                    cs::StrPat("str", pattern_abst, witness_var).mbuild(tycker, env)?
                }
                | PackagePatternStructure::Manifest => {
                    cs::Pat("str", structure_ty).mbuild(tycker, env)?
                }
            };
            let (env, body) = package_pattern_translation(
                tycker,
                env,
                remaining,
                remaining_source_layout,
                body,
                translated_tail_ty,
            )?;
            let product =
                Alloc::alloc(tycker, ConsN(vec![structure], body), translated_body_ty, &env.ty);
            let package = Alloc::alloc(
                tycker,
                ConsN(vec![StaticPatId::Type(witness)], product),
                translated_ty,
                &env.ty,
            );
            Ok((env, package))
        }
    }
}

fn package_value_translation(
    tycker: &mut Tycker, env: MonEnv, witnesses: &[StaticTermId], body: ValueId,
    translated_ty: TypeId,
) -> Result<(MonEnv, ValueId)> {
    let Some((&witness, remaining)) = witnesses.split_first() else {
        return cs::TermLift { tm: body }.mbuild(tycker, env);
    };
    match witness {
        | StaticTermId::Kind(witness) => {
            let Type::ManifestKind(ManifestKind { body: translated_tail_ty, .. }) =
                tycker.type_filled(&translated_ty)?.to_owned()
            else {
                unreachable!("translated manifest-kind package field must remain manifest")
            };
            let (env, body) =
                package_value_translation(tycker, env, remaining, body, translated_tail_ty)?;
            let package = Alloc::alloc(
                tycker,
                ConsN(vec![StaticTermId::Kind(witness)], body),
                translated_ty,
                &env.ty,
            );
            Ok((env, package))
        }
        | StaticTermId::Type(witness) => {
            let Some((_abst, translated_body_ty)) = translated_ty.destruct_exists(tycker) else {
                unreachable!("translated type package field must remain existential")
            };
            let Type::Prod(Prod(_structure_ty, translated_tail_ty)) =
                tycker.type_filled(&translated_body_ty)?.to_owned()
            else {
                unreachable!("translated existential body must contain its structure")
            };

            let (env, translated_witness) = cs::TypeLift { ty: witness }.mbuild(tycker, env)?;
            let (env, structure) = cs::Structure { ty: witness }.mbuild(tycker, env)?;
            let (env, structure) = Thunk(structure).mbuild(tycker, env)?;
            let (env, body) =
                package_value_translation(tycker, env, remaining, body, translated_tail_ty)?;
            let product =
                Alloc::alloc(tycker, ConsN(vec![structure], body), translated_body_ty, &env.ty);
            let package = Alloc::alloc(
                tycker,
                ConsN(vec![StaticTermId::Type(translated_witness)], product),
                translated_ty,
                &env.ty,
            );
            Ok((env, package))
        }
    }
}

fn value_pattern_translation(
    tycker: &mut Tycker, env: MonEnv, vpat: VPatId,
) -> Result<(MonEnv, VPatId)> {
    use ValuePattern as VPat;
    let (env, ty) = cs::TypeOf(vpat).mbuild(tycker, env)?;
    let (env, ty_) = cs::TypeLift { ty }.mbuild(tycker, env)?;
    let (env, vpat_) = match tycker.statics.vpats[&vpat].to_owned() {
        | VPat::Hole(hole) => cs::Pat(hole, ty_).mbuild(tycker, env)?,
        | VPat::Var(def) => {
            // create a fresh variable, and track the substitution
            cs::Pat(def, ty_).mbuild(tycker, env)?
        }
        | VPat::Named(vpat) => {
            let Named(name, inner) = vpat;
            let (env, inner) = value_pattern_translation(tycker, env, inner)?;
            let named = Alloc::alloc(tycker, Named(name, inner), ty_, &env.ty);
            (env, named)
        }
        | VPat::Ctor(vpat) => {
            let Ctor(ctor, body) = vpat;
            let body_ = cs::TermLift { tm: body };
            cs::Pat(cs::Ctor(ctor, body_), ty_).mbuild(tycker, env)?
        }
        | VPat::Triv(Triv) => Triv.mbuild(tycker, env)?,
        | VPat::VCons(vpat) => {
            let ConsN(items, tail) = vpat;
            let items = items.into_iter().map(|tm| cs::TermLift { tm }).collect();
            let tail = cs::TermLift { tm: tail };
            ConsN(items, tail).mbuild(tycker, env)?
        }
        | VPat::SCons(vpat) => {
            let ConsN(witnesses, body) = vpat;
            package_pattern_translation(tycker, env, &witnesses, None, body, ty_)?
        }
    };
    Ok((env, vpat_))
}

/// Term Translation (Value) `[V]`
fn value_translation(
    tycker: &mut Tycker, env: MonEnv, value: ValueId,
) -> Result<(MonEnv, ValueId)> {
    let (env, ty) = cs::TypeOf(value).mbuild(tycker, env)?;
    let (env, ty_) = cs::TypeLift { ty }.mbuild(tycker, env)?;
    let (env, res) = match tycker.statics.values[&value].to_owned() {
        | Value::Hole(Hole) => cs::Ann(Hole, ty_).mbuild(tycker, env)?,
        | Value::Lit(literal) => {
            let literal = Alloc::alloc(tycker, literal, ty_, &env.ty);
            (env, literal)
        }
        | Value::Var(def) => {
            // substitute according to the environment
            match env.subst.get(&def).cloned() {
                | Some(def_) => {
                    // the definition is closed in this monadic block
                    let alloc = Alloc::alloc(tycker, def_, ty_, &env.ty);
                    (env, alloc)
                }
                | None => {
                    use zydeco_utils::arena::ArenaAccess;
                    // it should then be global and should be in the inlinables
                    let Some(value) = tycker.statics.inlinables.get(&def).cloned() else {
                        tycker.err(TyckError::NotInlinable(def), std::panic::Location::caller())?
                    };
                    cs::TermLift { tm: value }.mbuild(tycker, env)?
                }
            }
        }
        | Value::Named(value) => {
            let Named(name, inner) = value;
            let (env, inner) = value_translation(tycker, env, inner)?;
            let named = Alloc::alloc(tycker, Named(name, inner), ty_, &env.ty);
            (env, named)
        }
        | Value::Let(value) => {
            let Let { binder, bindee, tail } = value;
            Let {
                binder: cs::TermLift { tm: binder },
                bindee: cs::TermLift { tm: bindee },
                tail: move |_| cs::TermLift { tm: tail },
            }
            .mbuild(tycker, env)?
        }
        | Value::Thunk(value) => {
            let Thunk(body) = value;
            Thunk(cs::TermLift { tm: body }).mbuild(tycker, env)?
        }
        | Value::Ctor(value) => {
            let Ctor(ctor, body) = value;
            let body_ = cs::TermLift { tm: body };
            let ty_ = cs::TypeLift { ty };
            cs::Ann(cs::Ctor(ctor, body_), ty_).mbuild(tycker, env)?
        }
        | Value::Triv(Triv) => Triv.mbuild(tycker, env)?,
        | Value::VCons(value) => {
            let ConsN(items, tail) = value;
            let items = items.into_iter().map(|tm| cs::TermLift { tm }).collect();
            let tail = cs::TermLift { tm: tail };
            ConsN(items, tail).mbuild(tycker, env)?
        }
        | Value::SCons(value) => {
            let ConsN(witnesses, body) = value;
            package_value_translation(tycker, env, &witnesses, body, ty_)?
        }
        | Value::Proj(value) => {
            let Proj(head, field) = value;
            let (env, head) = value_translation(tycker, env, head)?;
            let projected = Alloc::alloc(tycker, Proj(head, field), ty_, &env.ty);
            (env, projected)
        }
    };
    Ok((env, res))
}

/// Term Translation (Computation) `[C]`
fn computation_translation(
    tycker: &mut Tycker, env: MonEnv, compu: CompuId,
) -> Result<(MonEnv, CompuId)> {
    use Computation as Compu;
    let (env, ty) = cs::TypeOf(compu).mbuild(tycker, env)?;

    let (env, res) = match tycker.statics.compus[&compu].to_owned() {
        | Compu::Hole(Hole) => {
            let (env, ty_) = cs::TypeLift { ty }.mbuild(tycker, env)?;
            cs::Ann(Hole, ty_).mbuild(tycker, env)?
        }
        | Compu::VAbs(compu) => {
            let Abs(vpat, compu) = compu;
            match ty.destruct_pack_pi(tycker) {
                | Some(signature) => {
                    let translation =
                        PackPiPatternTranslation::new(vpat, &signature, tycker, &env.ty)?;
                    let (env, vpat) = translation.translate(tycker, env)?;
                    let (env, compu) = cs::TermLift { tm: compu }.mbuild(tycker, env)?;
                    let (env, signature) = cs::TypeLift { ty }.mbuild(tycker, env)?;
                    let abstraction = PackPiAbstraction { binder: vpat, body: compu, signature }
                        .build(tycker, &env.ty);
                    (env, abstraction)
                }
                | None => Abs(cs::TermLift { tm: vpat }, move |_def| cs::TermLift { tm: compu })
                    .mbuild(tycker, env)?,
            }
        }
        | Compu::VApp(compu) => {
            let App(fun, arg) = compu;
            let fun_ty = tycker.statics.annotations_compu[&fun];
            match fun_ty.destruct_pack_pi(tycker) {
                | Some(_) => {
                    let (env, fun) = cs::TermLift { tm: fun }.mbuild(tycker, env)?;
                    let (env, arg) = cs::TermLift { tm: arg }.mbuild(tycker, env)?;
                    let (env, result) = cs::TypeLift { ty }.mbuild(tycker, env)?;
                    let application = PackPiApplication { function: fun, argument: arg, result }
                        .build(tycker, &env.ty);
                    (env, application)
                }
                | None => {
                    let fun_ = cs::TermLift { tm: fun };
                    let arg_ = cs::TermLift { tm: arg };
                    App(fun_, arg_).mbuild(tycker, env)?
                }
            }
        }
        | Compu::TAbs(compu) => {
            let Abs(tpat, compu) = compu;
            let Some((abst, _)) = ty.destruct_forall(tycker) else { unreachable!() };
            Abs(cs::Ty((cs::TypeLift { ty: tpat }, abst)), move |_tpat, abst| {
                Abs(cs::StrPat("str", abst, None), move |_str| cs::TermLift { tm: compu })
            })
            .mbuild(tycker, env)?
        }
        | Compu::TApp(compu) => {
            let App(fun, arg) = compu;
            let fun_ = cs::TermLift { tm: fun };
            let arg_ = cs::TypeLift { ty: arg };
            let str_ = cs::Structure { ty: arg };
            App(App(fun_, cs::Ty(arg_)), Thunk(str_)).mbuild(tycker, env)?
        }
        | Compu::Fix(compu) => {
            let Fix(vpat, compu) = compu;
            Fix(cs::TermLift { tm: vpat }, move |_vpat| cs::TermLift { tm: compu })
                .mbuild(tycker, env)?
        }
        | Compu::Force(compu) => {
            let Force(value) = compu;
            Force(cs::TermLift { tm: value }).mbuild(tycker, env)?
        }
        | Compu::Ret(compu) => {
            let Return(value) = compu;
            App(
                App(
                    cs::Dtor(Force(env.monad_impl), ".return"),
                    cs::Ty(cs::TypeLift { ty: cs::TypeOf(value) }),
                ),
                cs::TermLift { tm: value },
            )
            .mbuild(tycker, env)?
        }
        | Compu::Do(compu) => {
            let Bind { binder, bindee, tail } = compu;
            let str_ = cs::Structure { ty };
            let (env, ret_ty) = cs::TypeOf(bindee).mbuild(tycker, env)?;
            let Some(a_ty) = ret_ty.destruct_ret_app(tycker) else { unreachable!() };
            let a_ty_ = cs::TypeLift { ty: a_ty };
            let bindee_ = cs::TermLift { tm: bindee };
            let kont = Abs(cs::TermLift { tm: binder }, move |_var| cs::TermLift { tm: tail });
            App(App(App(str_, cs::Ty(a_ty_)), Thunk(bindee_)), Thunk(kont)).mbuild(tycker, env)?
        }
        | Compu::Let(compu) => {
            let Let { binder, bindee, tail } = compu;
            let bindee_ = cs::TermLift { tm: bindee };
            let binder_ = cs::TermLift { tm: binder };
            Let { binder: binder_, bindee: bindee_, tail: move |_| cs::TermLift { tm: tail } }
                .mbuild(tycker, env)?
        }
        | Compu::Match(compu) => {
            let Match { scrut, arms } = compu;
            let (env, scrut_) = cs::TermLift { tm: scrut }.mbuild(tycker, env)?;
            let arms_ = arms
                .into_iter()
                .map(|Matcher { binder, tail }| {
                    let (env, binder_) = cs::TermLift { tm: binder }.mbuild(tycker, env.clone())?;
                    let (_env, tail_) = cs::TermLift { tm: tail }.mbuild(tycker, env)?;
                    Ok(Matcher { binder: binder_, tail: tail_ })
                })
                .collect::<Result<Vec<_>>>()?;
            let (env, ty_) = cs::TypeLift { ty }.mbuild(tycker, env)?;
            let alloc = Alloc::alloc(tycker, Match { scrut: scrut_, arms: arms_ }, ty_, &env.ty);
            (env, alloc)
        }
        | Compu::CoMatch(compu) => {
            let (env, ty_) = cs::TypeLift { ty }.mbuild(tycker, env)?;
            let ty_ = ty_.unroll(tycker)?;
            let Type::CoData(coda) = tycker.type_filled(&ty_)? else { unreachable!() };
            let CoMatch { arms } = compu;
            let arms: std::collections::HashMap<_, _> =
                arms.into_iter().map(|CoMatcher { dtor, tail }| (dtor, tail)).collect();
            cs::CoMatch(coda, |dtor, _ty| {
                let tail = arms.get(&dtor).cloned().unwrap();
                cs::TermLift { tm: tail }
            })
            .mbuild(tycker, env)?
        }
        | Compu::Dtor(compu) => {
            let Dtor(compu, dtor) = compu;
            let compu_ = cs::TermLift { tm: compu };
            cs::Dtor(compu_, dtor).mbuild(tycker, env)?
        }
    };

    Ok((env, res))
}
