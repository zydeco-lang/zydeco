//! Extend structure passing to statically eliminated value functions.
//!
//! These are bounded cases of the existing guarded monadic traversal. Binder
//! environments are child scopes; only the rebuilt node leaves the scope.

use super::*;

pub(super) struct ValueFunctionTranslation {
    pub signature: ValPi,
}

impl ValueFunctionTranslation {
    pub fn signature(tycker: &mut Tycker<'_>, ty: TypeId) -> Result<ValPi> {
        let ty = ty.unroll(tycker)?;
        let Type::ValPi(signature) = tycker.type_filled(&ty)?.to_owned() else {
            unreachable!("checked value functions have a ValPi classifier")
        };
        Ok(*signature)
    }

    fn type_binder(
        tycker: &mut Tycker<'_>, env: MonEnv, source: TypeBinder, pattern: TPatId,
    ) -> Result<(MonEnv, TypeBinder)> {
        let (mut env, pattern) = type_pattern_translation(tycker, env, pattern)?;
        let witness = Alloc::alloc(tycker, pattern, (), &());
        tycker.transfer_builtin_role(source.witness, witness)?;
        env.subst_abst.insert_mut(source.witness, witness);
        Ok((env, TypeBinder { pattern, witness }))
    }

    fn value_parameter(domain: TypeId) -> ValPiBinder {
        ValPiBinder::Value(ValueParameter {
            domain,
            witnesses: None,
            witness_projection: PackageWitnessProjection::Ignore,
        })
    }

    fn pi(tycker: &mut Tycker<'_>, env: &MonEnv, binder: ValPiBinder, codomain: TypeId) -> TypeId {
        let kind = VType.build(tycker, &env.ty);
        Alloc::alloc(tycker, ValPi { binder, codomain }, kind, &env.ty)
    }

    pub fn classifier(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let local = env.clone();
        let classifier = match self.signature.binder {
            | ValPiBinder::Type(source) => {
                let pattern = source.pattern;
                let (local, binder) = Self::type_binder(tycker, local, source, pattern)?;
                let (local, domain) =
                    cs::Thk(cs::Signature { ty: binder.witness }).construct(tycker, local)?;
                let (local, codomain) =
                    cs::TypeLift { ty: self.signature.codomain }.construct(tycker, local)?;
                let codomain = Self::pi(tycker, &local, Self::value_parameter(domain), codomain);
                Self::pi(tycker, &local, ValPiBinder::Type(binder), codomain)
            }
            | ValPiBinder::Value(parameter) => {
                let witness_projection = ValueFunctionWitnessRoute {
                    domain: parameter.domain,
                    projection: parameter.witness_projection,
                }
                .translate(tycker, &local.ty)?;
                let (local, domain) =
                    cs::TypeLift { ty: parameter.domain }.construct(tycker, local)?;
                let (local, witnesses) = match parameter.witnesses {
                    | Some(witnesses) => {
                        let (local, fresh) = witnesses.iter().try_fold(
                            (local, Vec::new()),
                            |(mut local, mut witnesses), source| -> Result<_> {
                                let kind = tycker.statics.annotations_abst[source];
                                let name = tycker.statics.abst_hints.get(source).copied();
                                let fresh = Alloc::alloc(tycker, name, kind, &());
                                tycker.transfer_builtin_role(*source, fresh)?;
                                tycker.statics.existential_skolems.ensure(fresh);
                                local.ty = local.ty.with_skolem(fresh);
                                local.subst_abst.insert_mut(*source, fresh);
                                witnesses.push(fresh);
                                Ok((local, witnesses))
                            },
                        )?;
                        (local, Some(PackTelescope::new(fresh[0], fresh[1..].iter().copied())))
                    }
                    | None => (local, None),
                };
                let (local, codomain) =
                    cs::TypeLift { ty: self.signature.codomain }.construct(tycker, local)?;
                Self::pi(
                    tycker,
                    &local,
                    ValPiBinder::Value(ValueParameter { domain, witnesses, witness_projection }),
                    codomain,
                )
            }
        };
        Ok((env, classifier))
    }

    pub fn abstraction(
        self, tycker: &mut Tycker<'_>, env: MonEnv, Abs(pattern, body): Abs<ValBinder, ValueId>,
    ) -> Result<(MonEnv, ValueId)> {
        let local = env.clone();
        let abstraction = match (self.signature.binder, pattern) {
            | (ValPiBinder::Type(source), ValBinder::Type(pattern)) => {
                let (local, binder) = Self::type_binder(tycker, local, source, pattern)?;
                let (local, structure) =
                    cs::StrPat("str", binder.witness, None).construct(tycker, local)?;
                let (local, body) = cs::TermLift { tm: body }.construct(tycker, local)?;
                let (local, codomain) = cs::TypeOf(body).construct(tycker, local)?;
                let (local, expected) =
                    cs::TypeLift { ty: self.signature.codomain }.construct(tycker, local)?;
                Lub::lub(expected, codomain, tycker)?;
                let (local, domain) = cs::TypeOf(structure).construct(tycker, local)?;
                let inner_ty = Self::pi(tycker, &local, Self::value_parameter(domain), codomain);
                let inner = Alloc::alloc(
                    tycker,
                    Abs(ValBinder::Value(structure), body),
                    inner_ty,
                    &local.ty,
                );
                let pattern = binder.pattern;
                let ty = Self::pi(tycker, &local, ValPiBinder::Type(binder), inner_ty);
                Alloc::alloc(tycker, Abs(ValBinder::Type(pattern), inner), ty, &local.ty)
            }
            | (ValPiBinder::Value(parameter), ValBinder::Value(pattern)) => {
                let witness_projection = ValueFunctionWitnessRoute {
                    domain: parameter.domain,
                    projection: parameter.witness_projection,
                }
                .translate(tycker, &local.ty)?;
                let (local, pattern) = if let Some(witnesses) = &parameter.witnesses {
                    let signature = PackPi {
                        domain: parameter.domain,
                        witnesses: witnesses.clone(),
                        codomain: self.signature.codomain,
                    };
                    let translation =
                        PackPiPatternTranslation::new(pattern, &signature, tycker, &local.ty)?;
                    translation.translate(tycker, local)?
                } else {
                    cs::TermLift { tm: pattern }.construct(tycker, local)?
                };
                let witnesses = parameter.witnesses.map(|witnesses| {
                    witnesses
                        .map(|witness| local.subst_abst.get(&witness).copied().unwrap_or(witness))
                });
                let (local, domain) = cs::TypeOf(pattern).construct(tycker, local)?;
                let (local, body) = cs::TermLift { tm: body }.construct(tycker, local)?;
                let (local, codomain) = cs::TypeOf(body).construct(tycker, local)?;
                let (local, expected) =
                    cs::TypeLift { ty: self.signature.codomain }.construct(tycker, local)?;
                Lub::lub(expected, codomain, tycker)?;
                let ty = Self::pi(
                    tycker,
                    &local,
                    ValPiBinder::Value(ValueParameter { domain, witnesses, witness_projection }),
                    codomain,
                );
                Alloc::alloc(tycker, Abs(ValBinder::Value(pattern), body), ty, &local.ty)
            }
            | _ => unreachable!("checked value abstraction follows its classifier binder"),
        };
        Ok((env, abstraction))
    }

    pub fn application(
        tycker: &mut Tycker<'_>, env: MonEnv, function: ValueId, argument: ValArgument,
        result: TypeId,
    ) -> Result<(MonEnv, ValueId)> {
        let (env, function) = cs::TermLift { tm: function }.construct(tycker, env)?;
        let (env, function_ty) = cs::TypeOf(function).construct(tycker, env)?;
        let signature = Self::signature(tycker, function_ty)?;
        let (env, application) = match (signature.binder, argument) {
            | (ValPiBinder::Type(binder), ValArgument::Type(source)) => {
                let (env, argument) = cs::TypeLift { ty: source }.construct(tycker, env)?;
                let (env, argument_kind) = cs::TypeOf(argument).construct(tycker, env)?;
                Lub::lub(binder.domain_kind(tycker), argument_kind, tycker)?;
                let payload = binder.pattern.bind_argument(tycker, argument)?;
                let inner_ty = signature.codomain.subst_abst(tycker, (binder.witness, payload))?;
                let inner = Alloc::alloc(
                    tycker,
                    App(function, ValArgument::Type(argument)),
                    inner_ty,
                    &env.ty,
                );
                let signature = Self::signature(tycker, inner_ty)?;
                let ValPiBinder::Value(parameter) = signature.binder else {
                    unreachable!("translated type application is followed by structure application")
                };
                let (env, structure) =
                    Thunk(cs::Structure { ty: source }).construct(tycker, env)?;
                let (env, structure_ty) = cs::TypeOf(structure).construct(tycker, env)?;
                Lub::lub(parameter.domain, structure_ty, tycker)?;
                Lub::lub(result, signature.codomain, tycker)?;
                let application = Alloc::alloc(
                    tycker,
                    App(inner, ValArgument::Value(structure)),
                    result,
                    &env.ty,
                );
                (env, application)
            }
            | (ValPiBinder::Value(parameter), ValArgument::Value(argument)) => {
                let (env, argument) = cs::TermLift { tm: argument }.construct(tycker, env)?;
                let (env, argument_ty) = cs::TypeOf(argument).construct(tycker, env)?;
                Lub::lub(parameter.domain, argument_ty, tycker)?;
                let application = Alloc::alloc(
                    tycker,
                    App(function, ValArgument::Value(argument)),
                    result,
                    &env.ty,
                );
                (env, application)
            }
            | _ => unreachable!("checked value application follows its classifier binder"),
        };
        Ok((env, application))
    }
}

/// Rebuild witness routes as existential payloads acquire structure fields.
struct ValueFunctionWitnessRoute {
    domain: TypeId,
    projection: PackageWitnessProjection,
}

impl ValueFunctionWitnessRoute {
    fn translate(self, tycker: &mut Tycker<'_>, env: &TyEnv) -> Result<PackageWitnessProjection> {
        use zydeco_utils::fold::Explicit;
        Explicit::run(&mut WitnessRouteFolder { tycker, env }, self)
    }
}

struct WitnessRouteFolder<'t, 'e, 'db> {
    tycker: &'t mut Tycker<'db>,
    env: &'e TyEnv,
}

enum WitnessRouteFrame {
    Package {
        abstracts: usize,
        structure: bool,
    },
    Product {
        remaining: std::vec::IntoIter<ValueFunctionWitnessRoute>,
        translated: Vec<PackageWitnessProjection>,
    },
}

impl WitnessRouteFolder<'_, '_, '_> {
    fn step(&mut self, input: ValueFunctionWitnessRoute) -> Result<zydeco_utils::fold::Step<Self>> {
        use PackageWitnessProjection as Route;
        use zydeco_utils::fold::Step;
        if matches!(input.projection, Route::Ignore | Route::Package { abstracts: 0 }) {
            return Ok(Step::Return(Ok(Route::Ignore)));
        }
        let domain = input.domain.unroll(self.tycker)?.subst_env(self.tycker, self.env)?;
        let node = self.tycker.type_filled(&domain)?.to_owned();
        if let Type::Label(Label(_, domain)) = node {
            return Ok(Step::TailCall(ValueFunctionWitnessRoute { domain, ..input }));
        }
        Ok(match (input.projection, node) {
            | (Route::Product(routes), Type::Prod(Prod(domains))) => {
                assert_eq!(
                    routes.len(),
                    domains.len(),
                    "checked witness route follows its product"
                );
                let mut remaining = routes
                    .into_iter()
                    .zip(domains)
                    .map(|(projection, domain)| ValueFunctionWitnessRoute { domain, projection })
                    .collect::<Vec<_>>()
                    .into_iter();
                match remaining.next() {
                    | Some(input) => Step::Call {
                        input,
                        frame: WitnessRouteFrame::Product { remaining, translated: Vec::new() },
                    },
                    | None => Step::Return(Ok(Route::Ignore)),
                }
            }
            | (Route::Package { abstracts }, Type::ManifestKind(manifest)) => Step::Call {
                input: ValueFunctionWitnessRoute {
                    domain: manifest.body,
                    projection: Route::Package { abstracts },
                },
                frame: WitnessRouteFrame::Package { abstracts: 0, structure: false },
            },
            | (Route::Package { abstracts }, Type::Exists(exists)) => {
                let opened = usize::from(matches!(exists.mode, ExistsMode::Abstract));
                Step::Call {
                    input: ValueFunctionWitnessRoute {
                        domain: exists.body,
                        projection: Route::Package { abstracts: abstracts - opened },
                    },
                    frame: WitnessRouteFrame::Package { abstracts: opened, structure: true },
                }
            }
            | (Route::Unpack { prefix, abstracts, body }, Type::Exists(exists)) => {
                let opened = usize::from(matches!(exists.mode, ExistsMode::Abstract));
                let projection = if prefix == 1 {
                    *body
                } else {
                    Route::Unpack { prefix: prefix - 1, abstracts: abstracts - opened, body }
                };
                Step::Call {
                    input: ValueFunctionWitnessRoute { domain: exists.body, projection },
                    frame: WitnessRouteFrame::Package { abstracts: opened, structure: true },
                }
            }
            | (Route::Unpack { prefix, abstracts, body }, Type::ManifestKind(manifest)) => {
                let projection = if prefix == 1 {
                    *body
                } else {
                    Route::Unpack { prefix: prefix - 1, abstracts, body }
                };
                Step::Call {
                    input: ValueFunctionWitnessRoute { domain: manifest.body, projection },
                    frame: WitnessRouteFrame::Package { abstracts: 0, structure: false },
                }
            }
            | _ => unreachable!("checked witness route follows its package domain"),
        })
    }
}

impl zydeco_utils::fold::Folder for WitnessRouteFolder<'_, '_, '_> {
    type Input = ValueFunctionWitnessRoute;
    type Output = Result<PackageWitnessProjection>;
    type Frame = WitnessRouteFrame;

    fn enter(&mut self, input: Self::Input) -> zydeco_utils::fold::Step<Self> {
        self.step(input).unwrap_or_else(|error| zydeco_utils::fold::Step::Return(Err(error)))
    }

    fn resume(
        &mut self, frame: Self::Frame, child: Self::Output,
    ) -> zydeco_utils::fold::Step<Self> {
        use PackageWitnessProjection as Route;
        use zydeco_utils::fold::Step;
        let child = match child {
            | Ok(child) => child,
            | Err(error) => return Step::Return(Err(error)),
        };
        match frame {
            | WitnessRouteFrame::Package { abstracts, structure } => {
                let body =
                    if structure { Route::product(vec![Route::Ignore, child]) } else { child };
                Step::Return(Ok(Route::unpack(1, abstracts, body)))
            }
            | WitnessRouteFrame::Product { mut remaining, mut translated } => {
                translated.push(child);
                match remaining.next() {
                    | Some(input) => Step::Call {
                        input,
                        frame: WitnessRouteFrame::Product { remaining, translated },
                    },
                    | None => Step::Return(Ok(Route::product(translated))),
                }
            }
        }
    }
}
