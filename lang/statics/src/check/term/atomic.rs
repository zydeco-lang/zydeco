//! Variables, holes, units, and checked literal representations.

use super::*;

impl TermChecker<'_> {
    pub(super) fn check_hole_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Hole, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Hole = term;
            match switch {
                | Switch::Syn => {
                    let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                    let Some(fill) = crate::query::term_hole_syn_judgment(
                        tycker.db,
                        tycker.data,
                        term,
                        tycker.site_occurrence(),
                    ) else {
                        unreachable!("hole judgments are query-produced")
                    };
                    tycker.statics.fills.insert_new(fill, ss::InferenceSite::Term(self.inner));
                    TermAnnId::Hole(fill)
                }
                | Switch::Ana(AnnId::Set) => {
                    // can't deduce kind for now
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                }
                | Switch::Ana(AnnId::Kind(kd)) => {
                    // a type hole, with a specific kind in mind
                    let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                    let input = crate::query::InternedHoleAna::new(
                        tycker.db,
                        crate::query::HoleAnaKind::Type { kd },
                    );
                    let Some(crate::query::HoleAnaOutcome::Type { fill, ty, kd }) =
                        crate::query::hole_ana_judgment(
                            tycker.db,
                            tycker.data,
                            term,
                            input,
                            tycker.site_occurrence(),
                        )
                    else {
                        unreachable!("the kind arm of hole judgments is query-produced")
                    };
                    tycker.statics.fills.insert_new(fill, ss::InferenceSite::Term(self.inner));
                    tycker.statics.types_pre.insert_new(ty, ss::Fillable::Fill(fill), kd);
                    tycker.store_env(ty, &self.info);
                    let scope = self.info.skolem_scope().clone();
                    if let Some(existing) =
                        tycker.statics.fill_scopes.insert_or_get(fill, scope.clone())
                    {
                        tycker
                            .statics
                            .fill_scopes
                            .replace_existing(fill, existing.intersection(&scope));
                    }
                    TermAnnId::Type(ty, kd)
                }
                | Switch::Ana(AnnId::Type(ty)) => {
                    // Preserve typed holes for inspection; execution readiness rejects
                    // any that remain in the residual runtime program.
                    let kd = tycker.statics.type_kind(ty);
                    match tycker.kind_filled_k(&kd)?.to_owned() {
                        | ss::Kind::VType(ss::VType) => {
                            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                            let input = crate::query::InternedHoleAna::new(
                                tycker.db,
                                crate::query::HoleAnaKind::Value { ty },
                            );
                            let Some(crate::query::HoleAnaOutcome::Value { fill, id, value, ann }) =
                                crate::query::hole_ana_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                )
                            else {
                                unreachable!("the value arm of hole judgments is query-produced")
                            };
                            tycker
                                .statics
                                .fills
                                .insert_new(fill, ss::InferenceSite::Term(self.inner));
                            fill.fill_k(tycker, ty.into())?;
                            tycker.statics.fill_hints.insert_new(fill, ());
                            tycker.statics.values.insert_new(id, value);
                            tycker.statics.annotations_value.insert_new(id, ann);
                            tycker.statics.env_value.insert_new(id, self.info.clone());
                            TermAnnId::Value(id, ann)
                        }
                        | ss::Kind::CType(ss::CType) => {
                            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                            let input = crate::query::InternedHoleAna::new(
                                tycker.db,
                                crate::query::HoleAnaKind::Compu { ty },
                            );
                            let Some(crate::query::HoleAnaOutcome::Compu { fill, id, compu, ann }) =
                                crate::query::hole_ana_judgment(
                                    tycker.db,
                                    tycker.data,
                                    term,
                                    input,
                                    tycker.site_occurrence(),
                                )
                            else {
                                unreachable!(
                                    "the computation arm of hole judgments is query-produced"
                                )
                            };
                            tycker
                                .statics
                                .fills
                                .insert_new(fill, ss::InferenceSite::Term(self.inner));
                            fill.fill_k(tycker, ty.into())?;
                            tycker.statics.fill_hints.insert_new(fill, ());
                            tycker.statics.compus.insert_new(id, compu);
                            tycker.statics.annotations_compu.insert_new(id, ann);
                            tycker.statics.env_compu.insert_new(id, self.info.clone());
                            TermAnnId::Compu(id, ann)
                        }
                        | ss::Kind::Arrow(_) | ss::Kind::Label(_) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    }
                }
            }
        })
    }

    pub(super) fn check_var_k<'db>(
        &self, tycker: &mut Tycker<'db>, def: su::DefId, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let annotation =
                tycker.statics.annotations_var.get(&def).copied().unwrap_or_else(|| {
                    panic!(
                        "resolved variable `{}` reached the checker before its binder",
                        tycker.def_name(&def).plain()
                    )
                });
            let ann = {
                match switch {
                    | Switch::Syn => annotation,
                    | Switch::Ana(ana) => Lub::lub_k(annotation, ana, tycker)?,
                }
            };
            match ann {
                | AnnId::Set => {
                    let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                    let env_data = crate::query::EnvData::new(tycker.db, self.info.clone());
                    let annotation = crate::query::InternedAnn::new(tycker.db, ann);
                    let Some(crate::query::VarSynOutcome::Kind { id }) =
                        crate::query::var_syn_judgment(
                            tycker.db,
                            tycker.data,
                            env_data,
                            term,
                            annotation,
                            tycker.site_occurrence(),
                        )
                    else {
                        unreachable!("the set arm of variable judgments is query-produced")
                    };
                    TermAnnId::Kind(id)
                }
                | AnnId::Kind(kd) => match self.info.recursively_get_type(tycker, &def) {
                    | Some(&ann) => {
                        let AnnId::Type(ty) = ann else { unreachable!() };
                        TermAnnId::Type(ty, kd)
                    }
                    | None => {
                        let ty = Alloc::alloc(tycker, def, kd, &self.info);
                        TermAnnId::Type(ty, kd)
                    }
                },
                | AnnId::Type(_) => {
                    let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                    let env_data = crate::query::EnvData::new(tycker.db, self.info.clone());
                    let annotation = crate::query::InternedAnn::new(tycker.db, ann);
                    let Some(crate::query::VarSynOutcome::Value { id, value, ty }) =
                        crate::query::var_syn_judgment(
                            tycker.db,
                            tycker.data,
                            env_data,
                            term,
                            annotation,
                            tycker.site_occurrence(),
                        )
                    else {
                        unreachable!("the type arm of variable judgments is query-produced")
                    };
                    tycker.statics.values.insert_new(id, value);
                    tycker.statics.annotations_value.insert_new(id, ty);
                    tycker.statics.env_value.insert_new(id, self.info.clone());
                    TermAnnId::Value(id, ty)
                }
            }
        })
    }

    pub(super) fn check_triv_k<'db>(
        &self, tycker: &mut Tycker<'db>, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok(match switch {
            | Switch::Syn => {
                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                let Some(outcome) = crate::query::triv_syn_judgment(
                    tycker.db,
                    tycker.data,
                    term,
                    tycker.site_occurrence(),
                ) else {
                    unreachable!("trivial judgments are query-produced")
                };
                let crate::query::TrivSynOutcome { id, value, ty } = outcome;
                tycker.statics.values.insert_new(id, value);
                tycker.statics.annotations_value.insert_new(id, ty);
                tycker.statics.env_value.insert_new(id, self.info.clone());
                TermAnnId::Value(id, ty)
            }
            | Switch::Ana(AnnId::Type(ana)) => {
                let unit = ss::UnitTy.build(tycker, &self.info);
                let ann = Lub::lub_k(unit, ana, tycker)?;
                let triv = Alloc::alloc(tycker, ss::Triv, ann, &self.info);
                TermAnnId::Value(triv, ann)
            }
            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            }
        })
    }

    pub(super) fn check_lit_k<'db>(
        &self, tycker: &mut Tycker<'db>, lit: su::Literal, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok(match switch {
            | Switch::Syn => {
                let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                let Some(outcome) = crate::query::literal_syn_judgment(
                    tycker.db,
                    tycker.data,
                    term,
                    tycker.site_occurrence(),
                ) else {
                    unreachable!("literal judgments are query-produced")
                };
                match outcome {
                    | crate::query::LiteralSynOutcome::Value { id, value, ty } => {
                        tycker.statics.values.insert_new(id, value);
                        tycker.statics.annotations_value.insert_new(id, ty);
                        tycker.statics.env_value.insert_new(id, self.info.clone());
                        TermAnnId::Value(id, ty)
                    }
                    | crate::query::LiteralSynOutcome::Error(error) => {
                        tycker.err_k(error, std::panic::Location::caller())?
                    }
                }
            }
            | Switch::Ana(annotation) => {
                let switch = Switch::Ana(annotation);
                fn literal_type_k(
                    tycker: &mut Tycker<'_>, env: &ss::TyEnv, switch: Switch<AnnId>,
                    primitive: ss::PrimitiveType,
                ) -> ResultKont<ss::TypeId> {
                    let literal_ty = ss::PrimitiveTy(primitive).build(tycker, env);
                    match switch {
                        | Switch::Syn => unreachable!("the synth path is query-produced"),
                        | Switch::Ana(annotation) => {
                            let AnnId::Type(ty) = annotation else {
                                tycker.err_k(
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?
                            };
                            Lub::lub_k(literal_ty, ty, tycker)
                        }
                    }
                }
                use zydeco_syntax::Literal as Lit;
                let (lit, ty) = match lit {
                    | Lit::Integer(i) => {
                        let (ty, integer_type) = match switch {
                            | Switch::Syn => unreachable!("the synth path is query-produced"),
                            | Switch::Ana(AnnId::Type(ty)) => match tycker.primitive_type_of(ty) {
                                | Some(ss::PrimitiveType::Integer(integer_type)) => {
                                    (ty, integer_type)
                                }
                                | Some(_) | None => {
                                    let default = ss::PrimitiveTy(ss::PrimitiveType::Integer(
                                        ss::IntegerType::Int64,
                                    ))
                                    .build(tycker, &self.info);
                                    let ty = Lub::lub_k(default, ty, tycker)?;
                                    (ty, ss::IntegerType::Int64)
                                }
                            },
                            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => tycker
                                .err_k(TyckError::SortMismatch, std::panic::Location::caller())?,
                        };
                        let value = i.value();
                        let Some(i) = i.with_type(integer_type) else {
                            tycker.err_k(
                                TyckError::IntegerLiteralOutOfRange { value, integer_type },
                                std::panic::Location::caller(),
                            )?
                        };
                        (Lit::Integer(i), ty)
                    }
                    | Lit::Float(value) => {
                        let (ty, float_type) = match switch {
                            | Switch::Syn => unreachable!("the synth path is query-produced"),
                            | Switch::Ana(AnnId::Type(ty)) => match tycker.primitive_type_of(ty) {
                                | Some(ss::PrimitiveType::Float(float_type)) => (ty, float_type),
                                | Some(_) | None => {
                                    let default = ss::PrimitiveTy(ss::PrimitiveType::Float(
                                        ss::FloatType::Float64,
                                    ))
                                    .build(tycker, &self.info);
                                    let ty = Lub::lub_k(default, ty, tycker)?;
                                    (ty, ss::FloatType::Float64)
                                }
                            },
                            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => tycker
                                .err_k(TyckError::SortMismatch, std::panic::Location::caller())?,
                        };
                        let original = value;
                        let Some(value) = value.with_type(float_type) else {
                            tycker.err_k(
                                TyckError::FloatLiteralOutOfRange {
                                    value: original.value(),
                                    float_type,
                                },
                                std::panic::Location::caller(),
                            )?
                        };
                        (Lit::Float(value), ty)
                    }
                    | Lit::String(s) => {
                        let ty =
                            literal_type_k(tycker, &self.info, switch, ss::PrimitiveType::String)?;
                        (Lit::String(s), ty)
                    }
                    | Lit::Char(c) => {
                        let ty =
                            literal_type_k(tycker, &self.info, switch, ss::PrimitiveType::Char)?;
                        (Lit::Char(c), ty)
                    }
                };
                let lit = Alloc::alloc(tycker, lit, ty, &self.info);
                TermAnnId::Value(lit, ty)
            }
        })
    }
}
