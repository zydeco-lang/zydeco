//! Unroll disclosed seals and normalize type applications and projections.

use super::*;

impl TypeId {
    pub fn unroll_k(self, tycker: &mut Tycker<'_>) -> ResultKont<TypeId> {
        let res = self.unroll(tycker);
        tycker.err_p_to_k(res)
    }
    pub fn unroll(self, tycker: &mut Tycker<'_>) -> Result<TypeId> {
        let kd = tycker.statics.type_kind(self);
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
            | Type::Arrow(_)
            | Type::Forall(_)
            | Type::PackPi(_)
            | Type::ValPi(_)
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
                        let payload_kind = tycker.statics.type_kind(self);
                        Alloc::alloc(tycker, Proj(head, name), payload_kind, &env)
                    }
                }
            }
        };
        Ok(res)
    }
}

/* ------------------------------ Normalization ----------------------------- */

#[derive(Clone, Copy)]
struct TypeApplicationStep {
    argument: TypeId,
    result_kind: KindId,
    original: Option<TypeId>,
}

/// A left-associated type-application spine.
///
/// Higher-kinded intermediate applications stay as compact `Type::App` nodes while the checker
/// receives more arguments. Once the result kind is saturated, this spine lets a direct chain of
/// type abstractions compose all abstract assignments and rewrite the body once.
pub(super) struct TypeApplicationSpine {
    function: TypeId,
    steps: Vec<TypeApplicationStep>,
}

impl TypeApplicationSpine {
    pub(super) fn with_application(
        tycker: &Tycker<'_>, mut function: TypeId, argument: TypeId, result_kind: KindId,
    ) -> Self {
        let mut reversed = vec![TypeApplicationStep { argument, result_kind, original: None }];
        while let Fillable::Done(Type::App(App(parent, argument))) =
            tycker.statics.types_pre[&function].to_owned()
        {
            reversed.push(TypeApplicationStep {
                argument,
                result_kind: tycker.statics.type_kind(function),
                original: Some(function),
            });
            function = parent;
        }
        reversed.reverse();
        Self { function, steps: reversed }
    }

    fn from_root(tycker: &Tycker<'_>, root: TypeId, app: App<TypeId, TypeId>) -> Self {
        let App(function, argument) = app;
        Self::with_application(tycker, function, argument, tycker.statics.type_kind(root))
            .with_original(root)
    }

    pub(super) fn with_original(mut self, root: TypeId) -> Self {
        self.steps.last_mut().expect("an application spine is non-empty").original = Some(root);
        self
    }

    fn normalize_components(self, tycker: &mut Tycker<'_>) -> Result<Self> {
        let function_kind = tycker.statics.type_kind(self.function);
        let function = self.function.normalize(tycker, function_kind)?;
        let steps = self
            .steps
            .into_iter()
            .map(|step| {
                let argument_kind = tycker.statics.type_kind(step.argument);
                let argument = step.argument.normalize(tycker, argument_kind)?;
                Ok(TypeApplicationStep { argument, ..step })
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(Self { function, steps })
    }

    pub(super) fn materialize(self, tycker: &mut Tycker<'_>) -> Result<TypeId> {
        if let Some(fused) = self.fuse_nested_abstractions(tycker)? {
            return Ok(fused);
        }

        self.steps.into_iter().try_fold(self.function, |function, step| {
            let env = tycker.statics.env_at(function);
            match tycker.statics.types_pre[&function].to_owned() {
                | Fillable::Done(Type::Abs(TypeAbstraction { binder, body })) => {
                    let argument = binder.pattern.bind_argument(tycker, step.argument)?;
                    body.subst_abst(tycker, (binder.witness, argument))
                }
                | Fillable::Fill(_) => Ok(function),
                | Fillable::Done(_) => {
                    if let Some(original) = step.original
                        && matches!(
                            tycker.statics.types_pre[&original],
                            Fillable::Done(Type::App(App(found_function, found_argument)))
                                if found_function == function && found_argument == step.argument
                        )
                    {
                        Ok(original)
                    } else {
                        Ok(Alloc::alloc(
                            tycker,
                            App(function, step.argument),
                            step.result_kind,
                            &env,
                        ))
                    }
                }
            }
        })
    }

    fn fuse_nested_abstractions(&self, tycker: &mut Tycker<'_>) -> Result<Option<TypeId>> {
        let mut body = self.function;
        let mut assignments = Vec::with_capacity(self.steps.len());
        for step in &self.steps {
            let Fillable::Done(Type::Abs(TypeAbstraction { binder, body: next })) =
                tycker.statics.types_pre[&body].to_owned()
            else {
                return Ok(None);
            };
            let argument = binder.pattern.bind_argument(tycker, step.argument)?;
            assignments.push((binder.witness, argument));
            body = next;
        }
        Ok(Some(body.subst_absts(tycker, &assignments)?))
    }
}

impl TypeId {
    pub fn normalize_k(self, tycker: &mut Tycker<'_>, kd: KindId) -> ResultKont<TypeId> {
        let res = self.normalize(tycker, kd);
        tycker.err_p_to_k(res)
    }
    pub fn normalize(self, tycker: &mut Tycker<'_>, kd: KindId) -> Result<TypeId> {
        let res = match tycker.statics.types_pre[&self].to_owned() {
            | Fillable::Fill(_) => self,
            | Fillable::Done(ty) => match ty {
                | Type::App(app) => TypeApplicationSpine::from_root(tycker, self, app)
                    .normalize_components(tycker)?
                    .materialize(tycker)?,
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
                | Type::ValPi(_)
                | Type::Arrow(_)
                | Type::Forall(_)
                | Type::PackPi(_)
                | Type::Prod(_)
                | Type::Exists(_)
                | Type::ManifestKind(_)
                | Type::Data(_)
                | Type::CoData(_) => self,
                | Type::Proj(Proj(head, name)) => {
                    let head_kind = tycker.statics.type_kind(head);
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
        TypeApplicationSpine::with_application(tycker, self, a_ty, kd)
            .normalize_components(tycker)?
            .materialize(tycker)
    }

    /// Apply one checked type argument, retaining an application while its result is still a type
    /// function. A saturated result materializes the complete left-associated spine at once.
    pub(crate) fn apply_type_argument_k(
        self, tycker: &mut Tycker<'_>, argument: TypeId, result_kind: KindId,
    ) -> ResultKont<TypeId> {
        let result = (|| {
            if matches!(tycker.kind_filled(&result_kind)?, Kind::Arrow(_)) {
                let env = tycker.statics.env_at(self);
                Ok(Alloc::alloc(tycker, App(self, argument), result_kind, &env))
            } else {
                self.normalize_app(tycker, argument, result_kind)
            }
        })();
        tycker.err_p_to_k(result)
    }
    pub fn normalize_apps_k(
        self, tycker: &mut Tycker<'_>, a_tys: Vec<TypeId>,
    ) -> ResultKont<TypeId> {
        let res = self.normalize_apps(tycker, a_tys);
        tycker.err_p_to_k(res)
    }
    pub fn normalize_apps(self, tycker: &mut Tycker<'_>, a_tys: Vec<TypeId>) -> Result<TypeId> {
        let function_kind = tycker.statics.type_kind(self);
        let (_, steps) = a_tys.into_iter().try_fold(
            (function_kind, Vec::new()),
            |(function_kind, mut steps), argument| -> Result<_> {
                let result_kind = match tycker.kind_filled(&function_kind)?.to_owned() {
                    | Kind::Arrow(Arrow(arg_kd, body_kd)) => {
                        let arg_kd_ = tycker.statics.type_kind(argument);
                        Lub::lub(arg_kd_, arg_kd, tycker)?;
                        body_kd
                    }
                    | _ => tycker.err(TyckError::KindMismatch, std::panic::Location::caller())?,
                };
                steps.push(TypeApplicationStep { argument, result_kind, original: None });
                Ok((result_kind, steps))
            },
        )?;
        TypeApplicationSpine { function: self, steps }
            .normalize_components(tycker)?
            .materialize(tycker)
    }
}
