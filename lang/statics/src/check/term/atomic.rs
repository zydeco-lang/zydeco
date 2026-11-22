//! Variables, holes, units, and checked literal representations.

use super::*;

impl TermChecker<'_> {
    pub(super) fn check_hole<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Hole, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::Hole = term;
            match switch {
                | Switch::Syn => {
                    let fill = Alloc::alloc(tycker, self.inner, (), &());
                    TermAnnId::Hole(fill)
                }
                | Switch::Ana(AnnId::Set) => {
                    // can't deduce kind for now
                    tycker.err(TyckError::SortMismatch)?
                }
                | Switch::Ana(AnnId::Kind(kd)) => {
                    // a type hole, with a specific kind in mind
                    let fill = Alloc::alloc(tycker, self.inner, (), &());
                    let ty = Alloc::alloc(tycker, fill, kd, self.info.skolem_scope());
                    TermAnnId::Type(ty, kd)
                }
                | Switch::Ana(AnnId::Type(ty)) => {
                    // Preserve typed holes for inspection; execution readiness rejects
                    // any that remain in the residual runtime program.
                    let kd = tycker.statics.type_kind(ty);
                    match tycker.kind_filled(&kd)?.to_owned() {
                        | ss::Kind::VType(ss::VType) => {
                            let fill = Alloc::alloc(tycker, self.inner, (), &());
                            fill.fill(tycker, ty.into())?;
                            tycker.statics.fill_hints.ensure(fill);
                            let id = Alloc::alloc(tycker, ss::Value::Hole(ss::Hole), ty, &());
                            TermAnnId::Value(id, ty)
                        }
                        | ss::Kind::CType(ss::CType) => {
                            let fill = Alloc::alloc(tycker, self.inner, (), &());
                            fill.fill(tycker, ty.into())?;
                            tycker.statics.fill_hints.ensure(fill);
                            let id = Alloc::alloc(tycker, ss::Computation::Hole(ss::Hole), ty, &());
                            TermAnnId::Compu(id, ty)
                        }
                        | ss::Kind::Arrow(_) | ss::Kind::Label(_) => {
                            tycker.err(TyckError::SortMismatch)?
                        }
                    }
                }
            }
        })
    }

    pub(super) fn check_var<'db>(
        &self, tycker: &mut Tycker<'db>, def: su::DefId, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
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
                    | Switch::Ana(ana) => Lub::lub(ana, annotation, tycker)?,
                }
            };
            match ann {
                | AnnId::Set => {
                    let AnnId::Kind(kd) = self.info[&def] else {
                        unreachable!("kind-bound variables carry kind annotations")
                    };
                    TermAnnId::Kind(kd)
                }
                | AnnId::Kind(kd) => match self.info.recursively_get_type(tycker, &def) {
                    | Some(&ann) => {
                        let AnnId::Type(ty) = ann else { unreachable!() };
                        TermAnnId::Type(ty, kd)
                    }
                    | None => {
                        let ty = Alloc::alloc(tycker, def, kd, &());
                        TermAnnId::Type(ty, kd)
                    }
                },
                | AnnId::Type(ty) => {
                    let id = Alloc::alloc(tycker, ss::Value::Var(def), ty, &());
                    TermAnnId::Value(id, ty)
                }
            }
        })
    }

    pub(super) fn check_triv<'db>(
        &self, tycker: &mut Tycker<'db>, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok(match switch {
            | Switch::Syn => {
                let ty = tycker.intrinsics.unit;
                let id = Alloc::alloc(tycker, ss::Triv, ty, &());
                TermAnnId::Value(id, ty)
            }
            | Switch::Ana(AnnId::Type(ana)) => {
                let unit = ss::UnitTy.build(tycker, &self.info);
                let ann = Lub::lub(ana, unit, tycker)?;
                let triv = Alloc::alloc(tycker, ss::Triv, ann, &());
                TermAnnId::Value(triv, ann)
            }
            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => tycker.err(TyckError::SortMismatch)?,
        })
    }

    pub(super) fn check_lit<'db>(
        &self, tycker: &mut Tycker<'db>, lit: su::Literal, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok(match switch {
            | Switch::Syn => {
                use zydeco_syntax::{FloatType, IntegerType, Literal, PrimitiveType};
                let (lit, ty) = match lit {
                    | Literal::Integer(i) => {
                        let integer_type = IntegerType::Int;
                        let value = i.value();
                        let Some(i) = i.with_type(integer_type) else {
                            tycker
                                .err(TyckError::IntegerLiteralOutOfRange { value, integer_type })?
                        };
                        (
                            Literal::Integer(i),
                            tycker.intrinsics.primitives[&PrimitiveType::Integer(integer_type)],
                        )
                    }
                    | Literal::Float(value) => {
                        let float_type = FloatType::Float64;
                        let original = value;
                        let Some(value) = value.with_type(float_type) else {
                            tycker.err(TyckError::FloatLiteralOutOfRange {
                                value: original.value(),
                                float_type,
                            })?
                        };
                        (
                            Literal::Float(value),
                            tycker.intrinsics.primitives[&PrimitiveType::Float(float_type)],
                        )
                    }
                    | Literal::String(s) => {
                        (Literal::String(s), tycker.intrinsics.primitives[&PrimitiveType::String])
                    }
                    | Literal::Char(c) => {
                        (Literal::Char(c), tycker.intrinsics.primitives[&PrimitiveType::Char])
                    }
                };
                let id = Alloc::alloc(tycker, ss::Value::Lit(lit), ty, &());
                TermAnnId::Value(id, ty)
            }
            | Switch::Ana(annotation) => {
                let switch = Switch::Ana(annotation);
                fn literal_type(
                    tycker: &mut Tycker<'_>, env: &ss::TyEnv, switch: Switch<AnnId>,
                    primitive: ss::PrimitiveType,
                ) -> Result<ss::TypeId> {
                    let literal_ty = ss::PrimitiveTy(primitive).build(tycker, env);
                    match switch {
                        | Switch::Syn => unreachable!("the synth path is query-produced"),
                        | Switch::Ana(annotation) => {
                            let AnnId::Type(ty) = annotation else {
                                tycker.err(TyckError::SortMismatch)?
                            };
                            Lub::lub(ty, literal_ty, tycker)
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
                                        ss::IntegerType::Int,
                                    ))
                                    .build(tycker, &self.info);
                                    let ty = Lub::lub(ty, default, tycker)?;
                                    (ty, ss::IntegerType::Int)
                                }
                            },
                            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                                tycker.err(TyckError::SortMismatch)?
                            }
                        };
                        let value = i.value();
                        let Some(i) = i.with_type(integer_type) else {
                            tycker
                                .err(TyckError::IntegerLiteralOutOfRange { value, integer_type })?
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
                                    let ty = Lub::lub(ty, default, tycker)?;
                                    (ty, ss::FloatType::Float64)
                                }
                            },
                            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                                tycker.err(TyckError::SortMismatch)?
                            }
                        };
                        let original = value;
                        let Some(value) = value.with_type(float_type) else {
                            tycker.err(TyckError::FloatLiteralOutOfRange {
                                value: original.value(),
                                float_type,
                            })?
                        };
                        (Lit::Float(value), ty)
                    }
                    | Lit::String(s) => {
                        let ty =
                            literal_type(tycker, &self.info, switch, ss::PrimitiveType::String)?;
                        (Lit::String(s), ty)
                    }
                    | Lit::Char(c) => {
                        let ty = literal_type(tycker, &self.info, switch, ss::PrimitiveType::Char)?;
                        (Lit::Char(c), ty)
                    }
                };
                let lit = Alloc::alloc(tycker, lit, ty, &());
                TermAnnId::Value(lit, ty)
            }
        })
    }
}
