use crate::{syntax::*, *};
use derive_more::From;
use std::collections::HashMap;

/// A type that can be joined with another type, producing their least upper bound.
/// T \/ T ?~~> T'
pub trait Lub<Rhs = Self>: Sized {
    type Out;
    fn lub_k(self, other: Rhs, tycker: &mut Tycker) -> ResultKont<Self::Out> {
        let res = self.lub(other, tycker);
        tycker.err_p_to_k(res)
    }
    /// Override this method to add administrative tasks for error tracking.
    fn lub(self, other: Rhs, tycker: &mut Tycker) -> Result<Self::Out> {
        self.lub_inner(other, tycker)
    }
    fn lub_inner(self, other: Rhs, tycker: &mut Tycker) -> Result<Self::Out>;
}

impl Lub for KindId {
    type Out = KindId;

    fn lub(self, other: Self, tycker: &mut Tycker) -> Result<Self::Out> {
        tycker.guarded(|tycker| {
            // administrative
            tycker.tasks.push_back(TyckTask::Lub(self.into(), other.into()));
            self.lub_inner(other, tycker)
        })
    }
    fn lub_inner(self, other: Self, tycker: &mut Tycker) -> Result<Self::Out> {
        let lhs = tycker.statics.kinds_pre[&self].clone();
        let rhs = tycker.statics.kinds_pre[&other].clone();
        fn fill_kd(tycker: &mut Tycker, fill: FillId, kd: KindId) -> Result<KindId> {
            Ok(fill.fill(tycker, kd.into())?.as_kind())
        }
        let res = match (lhs, rhs) {
            | (_, Fillable::Fill(rhs)) => fill_kd(tycker, rhs, self)?,
            | (Fillable::Fill(lhs), _) => fill_kd(tycker, lhs, other)?,
            | (Fillable::Done(lhs), Fillable::Done(rhs)) => match (lhs, rhs) {
                | (Kind::VType(VType), Kind::VType(VType)) => Alloc::alloc(tycker, VType, (), &()),
                | (Kind::CType(CType), Kind::CType(CType)) => Alloc::alloc(tycker, CType, (), &()),
                | (Kind::Arrow(lhs), Kind::Arrow(rhs)) => {
                    let Arrow(lin, lout) = lhs;
                    let Arrow(rin, rout) = rhs;
                    let kd_in = lin.lub(rin, tycker)?;
                    let kd_out = lout.lub(rout, tycker)?;

                    Alloc::alloc(tycker, Arrow(kd_in, kd_out), (), &())
                }
                | (
                    Kind::Label(Label(lhs_name, lhs_inner)),
                    Kind::Label(Label(rhs_name, rhs_inner)),
                ) => {
                    if lhs_name != rhs_name {
                        tycker.err(
                            TyckError::NamedLabelMismatch {
                                expected: lhs_name.clone(),
                                found: rhs_name,
                            },
                            std::panic::Location::caller(),
                        )?
                    }
                    let inner = lhs_inner.lub(rhs_inner, tycker)?;
                    Alloc::alloc(tycker, Label(lhs_name, inner), (), &())
                }
                | (Kind::VType(_), _)
                | (Kind::CType(_), _)
                | (Kind::Arrow(_), _)
                | (Kind::Label(_), _) => {
                    tycker.err(TyckError::KindMismatch, std::panic::Location::caller())?
                }
            },
        };
        Ok(res)
    }
}

#[derive(From, Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum BinderId {
    Var(DefId),
    Abst(AbstId),
}

#[derive(Clone, Default)]
struct Debruijn {
    level: usize,
    lhs: HashMap<BinderId, usize>,
    rhs: HashMap<BinderId, usize>,
}

impl Debruijn {
    fn new() -> Self {
        Self::default()
    }
    fn insert<T>(mut self, lhs: Option<T>, rhs: Option<T>) -> Self
    where
        T: Into<BinderId>,
    {
        if let Some(lhs) = lhs {
            self.lhs.insert(lhs.into(), self.level);
        }
        if let Some(rhs) = rhs {
            self.rhs.insert(rhs.into(), self.level);
        }
        self.level += 1;
        self
    }
    fn lookup_lhs<T>(&self, lhs: T) -> Option<usize>
    where
        T: Into<BinderId>,
    {
        self.lhs.get(&lhs.into()).cloned()
    }
    fn lookup_rhs<T>(&self, rhs: T) -> Option<usize>
    where
        T: Into<BinderId>,
    {
        self.rhs.get(&rhs.into()).cloned()
    }
    fn lub(self, lhs_id: TypeId, rhs_id: TypeId, tycker: &mut Tycker) -> Result<TypeId> {
        tycker.guarded(|tycker| {
            // administrative
            tycker.tasks.push_back(TyckTask::Lub(lhs_id.into(), rhs_id.into()));
            self.lub_inner(lhs_id, rhs_id, tycker)
        })
    }
    fn lub_inner(self, lhs_id: TypeId, rhs_id: TypeId, tycker: &mut Tycker) -> Result<TypeId> {
        let lhs = tycker.statics.types_pre[&lhs_id].clone();
        let rhs = tycker.statics.types_pre[&rhs_id].clone();
        let env = tycker.statics.env_at(lhs_id);
        fn fill_ty(tycker: &mut Tycker, fill: FillId, ty: TypeId) -> Result<TypeId> {
            Ok(fill.fill(tycker, ty.into())?.as_type())
        }
        let res = match (lhs, rhs) {
            | (_, Fillable::Fill(rhs)) => fill_ty(tycker, rhs, lhs_id)?,
            | (Fillable::Fill(lhs), _) => fill_ty(tycker, lhs, rhs_id)?,
            | (Fillable::Done(lhs), Fillable::Done(rhs)) => match (lhs, rhs) {
                | (Type::Var(lhs), Type::Var(rhs)) => {
                    // Todo: is "checking whether the two variables are the same" the canonical way?
                    if lhs == rhs {
                        lhs_id
                    } else {
                        match (self.lookup_lhs(lhs), self.lookup_rhs(rhs)) {
                            | (Some(l), Some(r)) if l == r => lhs_id,
                            | _ => tycker.err(
                                TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                                std::panic::Location::caller(),
                            )?,
                        }
                    }
                }
                | (Type::Abst(lhs), Type::Abst(rhs)) => {
                    match (self.lookup_lhs(lhs), self.lookup_rhs(rhs)) {
                        | (Some(l), Some(r)) if l == r => lhs_id,
                        | (None, None) if lhs == rhs => lhs_id,
                        | _ => tycker.err(
                            TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                            std::panic::Location::caller(),
                        )?,
                    }
                }
                | (Type::Abst(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (Type::Var(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (
                    Type::Abs(TypeAbstraction { binder: lbinder, body: lbody }),
                    Type::Abs(TypeAbstraction { binder: rbinder, body: rbody }),
                ) => {
                    let lpat = lbinder.pattern;
                    let rpat = rbinder.pattern;
                    let ldomain = tycker.statics.annotations_tpat[&lpat];
                    let rdomain = tycker.statics.annotations_tpat[&rpat];
                    let _domain = Lub::lub(ldomain, rdomain, tycker)?;
                    let (_, lpayload) = lpat.try_destruct_def(tycker);
                    let (_, rpayload) = rpat.try_destruct_def(tycker);
                    let _payload = Lub::lub(lpayload, rpayload, tycker)?;
                    let body = self
                        .insert(Some(lbinder.witness), Some(rbinder.witness))
                        .lub(lbody, rbody, tycker)?;
                    if body == lbody {
                        lhs_id
                    } else {
                        let kd = tycker.statics.annotations_type[&lhs_id];

                        Alloc::alloc(tycker, TypeAbstraction { binder: lbinder, body }, kd, &env)
                    }
                }
                | (Type::Abs(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (Type::App(App(lf, la)), Type::App(App(rf, ra))) => {
                    let f = self.clone().lub(lf, rf, tycker)?;
                    let a = self.lub(la, ra, tycker)?;
                    if f == lf && a == la {
                        lhs_id
                    } else {
                        let kd = tycker.statics.annotations_type[&lhs_id];
                        let app = Alloc::alloc(tycker, App(f, a), kd, &env);
                        app.normalize(tycker, kd)?
                    }
                }
                | (Type::App(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (
                    Type::Named(Named(lhs_name, lhs_inner)),
                    Type::Named(Named(rhs_name, rhs_inner)),
                ) => {
                    if lhs_name != rhs_name {
                        tycker.err(
                            TyckError::NamedLabelMismatch {
                                expected: lhs_name.clone(),
                                found: rhs_name,
                            },
                            std::panic::Location::caller(),
                        )?
                    }
                    let inner = self.lub(lhs_inner, rhs_inner, tycker)?;
                    if inner == lhs_inner {
                        lhs_id
                    } else {
                        let kd = tycker.statics.annotations_type[&lhs_id];
                        Alloc::alloc(tycker, Named(lhs_name, inner), kd, &env)
                    }
                }
                | (Type::Named(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (
                    Type::Label(Label(lhs_name, lhs_inner)),
                    Type::Label(Label(rhs_name, rhs_inner)),
                ) => {
                    if lhs_name != rhs_name {
                        tycker.err(
                            TyckError::NamedLabelMismatch {
                                expected: lhs_name.clone(),
                                found: rhs_name,
                            },
                            std::panic::Location::caller(),
                        )?
                    }
                    let inner = self.lub(lhs_inner, rhs_inner, tycker)?;
                    if inner == lhs_inner {
                        lhs_id
                    } else {
                        let kd = tycker.statics.annotations_type[&lhs_id];
                        Alloc::alloc(tycker, Label(lhs_name, inner), kd, &env)
                    }
                }
                | (Type::Label(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (Type::Proj(Proj(lhs_head, lhs_name)), Type::Proj(Proj(rhs_head, rhs_name))) => {
                    if lhs_name != rhs_name {
                        tycker.err(
                            TyckError::NamedLabelMismatch {
                                expected: lhs_name.clone(),
                                found: rhs_name,
                            },
                            std::panic::Location::caller(),
                        )?
                    }
                    let head = self.lub(lhs_head, rhs_head, tycker)?;
                    if head == lhs_head {
                        lhs_id
                    } else {
                        let kd = tycker.statics.annotations_type[&lhs_id];
                        Alloc::alloc(tycker, Proj(head, lhs_name), kd, &env)
                    }
                }
                | (Type::Proj(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (Type::Thk(ThkTy), Type::Thk(ThkTy)) => lhs_id,
                | (Type::Thk(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (Type::Ret(RetTy), Type::Ret(RetTy)) => lhs_id,
                | (Type::Ret(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (Type::Unit(UnitTy), Type::Unit(UnitTy)) => lhs_id,
                | (Type::Unit(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (Type::Opaque(OpaqueTy), Type::Opaque(OpaqueTy)) => lhs_id,
                | (Type::Opaque(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (Type::Primitive(PrimitiveTy(lhs)), Type::Primitive(PrimitiveTy(rhs)))
                    if lhs == rhs =>
                {
                    lhs_id
                }
                | (Type::Primitive(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (Type::OS(OSTy), Type::OS(OSTy)) => lhs_id,
                | (Type::OS(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (Type::VArrow(ValueArrow(la, lb)), Type::VArrow(ValueArrow(ra, rb))) => {
                    let a = self.clone().lub(la, ra, tycker)?;
                    let b = self.lub(lb, rb, tycker)?;
                    if a == la && b == lb {
                        lhs_id
                    } else {
                        let kd = tycker.statics.annotations_type[&lhs_id];
                        Alloc::alloc(tycker, ValueArrow(a, b), kd, &env)
                    }
                }
                | (Type::VArrow(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (
                    Type::VForall(ValueForall(lbind, lbody)),
                    Type::VForall(ValueForall(rbind, rbody)),
                ) => {
                    let _domain =
                        Lub::lub(lbind.domain_kind(tycker), rbind.domain_kind(tycker), tycker)?;
                    let _payload =
                        Lub::lub(lbind.payload_kind(tycker), rbind.payload_kind(tycker), tycker)?;
                    let body = self
                        .insert(Some(lbind.witness), Some(rbind.witness))
                        .lub(lbody, rbody, tycker)?;
                    if body == lbody {
                        lhs_id
                    } else {
                        let kd = tycker.statics.annotations_type[&lhs_id];
                        Alloc::alloc(tycker, ValueForall(lbind, body), kd, &env)
                    }
                }
                | (Type::VForall(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (Type::VPackPi(lhs), Type::VPackPi(rhs)) => {
                    let ValuePackPi {
                        domain: lhs_domain,
                        witnesses: lhs_witnesses,
                        codomain: lhs_codomain,
                    } = *lhs;
                    let ValuePackPi {
                        domain: rhs_domain,
                        witnesses: rhs_witnesses,
                        codomain: rhs_codomain,
                    } = *rhs;
                    if lhs_witnesses.len() != rhs_witnesses.len() {
                        tycker.err(
                            TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                            std::panic::Location::caller(),
                        )?
                    }
                    let domain = self.clone().lub(lhs_domain, rhs_domain, tycker)?;
                    let body_context = lhs_witnesses
                        .iter()
                        .copied()
                        .zip(rhs_witnesses.iter().copied())
                        .try_fold(self, |context, (lhs, rhs)| -> Result<_> {
                            let lhs_kind = tycker.statics.annotations_abst[&lhs];
                            let rhs_kind = tycker.statics.annotations_abst[&rhs];
                            let _ = Lub::lub(lhs_kind, rhs_kind, tycker)?;
                            Ok(context.insert(Some(lhs), Some(rhs)))
                        })?;
                    let codomain = body_context.lub(lhs_codomain, rhs_codomain, tycker)?;
                    if domain == lhs_domain && codomain == lhs_codomain {
                        lhs_id
                    } else {
                        let kd = tycker.statics.annotations_type[&lhs_id];
                        Alloc::alloc(
                            tycker,
                            ValuePackPi { domain, witnesses: lhs_witnesses, codomain },
                            kd,
                            &env,
                        )
                    }
                }
                | (Type::VPackPi(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (Type::Arrow(Arrow(la, lb)), Type::Arrow(Arrow(ra, rb))) => {
                    let a = self.clone().lub(la, ra, tycker)?;
                    let b = self.lub(lb, rb, tycker)?;
                    if a == la && b == lb {
                        lhs_id
                    } else {
                        let kd = tycker.statics.annotations_type[&lhs_id];

                        Alloc::alloc(tycker, Arrow(a, b), kd, &env)
                    }
                }
                | (Type::Arrow(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (Type::Forall(Forall(lbind, lbody)), Type::Forall(Forall(rbind, rbody))) => {
                    let _domain =
                        Lub::lub(lbind.domain_kind(tycker), rbind.domain_kind(tycker), tycker)?;
                    let _payload =
                        Lub::lub(lbind.payload_kind(tycker), rbind.payload_kind(tycker), tycker)?;
                    let body = self
                        .insert(Some(lbind.witness), Some(rbind.witness))
                        .lub(lbody, rbody, tycker)?;
                    if body == lbody {
                        lhs_id
                    } else {
                        let kd = tycker.statics.annotations_type[&lhs_id];

                        Alloc::alloc(tycker, Forall(lbind, body), kd, &env)
                    }
                }
                | (Type::Forall(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (Type::PackPi(lhs), Type::PackPi(rhs)) => {
                    let PackPi {
                        domain: lhs_domain,
                        witnesses: lhs_witnesses,
                        codomain: lhs_codomain,
                    } = *lhs;
                    let PackPi {
                        domain: rhs_domain,
                        witnesses: rhs_witnesses,
                        codomain: rhs_codomain,
                    } = *rhs;
                    if lhs_witnesses.len() != rhs_witnesses.len() {
                        tycker.err(
                            TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                            std::panic::Location::caller(),
                        )?
                    }
                    let domain = self.clone().lub(lhs_domain, rhs_domain, tycker)?;
                    let body_context = lhs_witnesses
                        .iter()
                        .copied()
                        .zip(rhs_witnesses.iter().copied())
                        .try_fold(self, |context, (lhs, rhs)| -> Result<_> {
                            let lhs_kind = tycker.statics.annotations_abst[&lhs];
                            let rhs_kind = tycker.statics.annotations_abst[&rhs];
                            let _ = Lub::lub(lhs_kind, rhs_kind, tycker)?;
                            Ok(context.insert(Some(lhs), Some(rhs)))
                        })?;
                    let codomain = body_context.lub(lhs_codomain, rhs_codomain, tycker)?;
                    if domain == lhs_domain && codomain == lhs_codomain {
                        lhs_id
                    } else {
                        let kd = tycker.statics.annotations_type[&lhs_id];
                        Alloc::alloc(
                            tycker,
                            PackPi { domain, witnesses: lhs_witnesses, codomain },
                            kd,
                            &env,
                        )
                    }
                }
                | (Type::PackPi(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (Type::Prod(Prod(la, lb)), Type::Prod(Prod(ra, rb))) => {
                    let a = self.clone().lub(la, ra, tycker)?;
                    let b = self.lub(lb, rb, tycker)?;
                    if a == la && b == lb {
                        lhs_id
                    } else {
                        let kd = tycker.statics.annotations_type[&lhs_id];

                        Alloc::alloc(tycker, Prod(a, b), kd, &env)
                    }
                }
                | (Type::Prod(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (Type::Exists(lhs), Type::Exists(rhs)) => {
                    let Exists { binder: lbind, mode: lmode, body: lbody } = *lhs;
                    let Exists { binder: rbind, mode: rmode, body: rbody } = *rhs;
                    let _domain =
                        Lub::lub(lbind.domain_kind(tycker), rbind.domain_kind(tycker), tycker)?;
                    let _payload =
                        Lub::lub(lbind.payload_kind(tycker), rbind.payload_kind(tycker), tycker)?;
                    let (mode, mode_unchanged) = match (lmode, rmode) {
                        | (ExistsMode::Abstract, ExistsMode::Abstract) => {
                            (ExistsMode::Abstract, true)
                        }
                        | (
                            ExistsMode::Manifest(ldefinition),
                            ExistsMode::Manifest(rdefinition),
                        ) => {
                            let definition = self.clone().lub(ldefinition, rdefinition, tycker)?;
                            (ExistsMode::Manifest(definition), definition == ldefinition)
                        }
                        | (ExistsMode::Abstract, ExistsMode::Manifest(_))
                        | (ExistsMode::Manifest(_), ExistsMode::Abstract) => tycker.err(
                            TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                            std::panic::Location::caller(),
                        )?,
                    };
                    let body = self
                        .insert(Some(lbind.witness), Some(rbind.witness))
                        .lub(lbody, rbody, tycker)?;
                    if mode_unchanged && body == lbody {
                        lhs_id
                    } else {
                        let kd = tycker.statics.annotations_type[&lhs_id];

                        Alloc::alloc(tycker, Exists { binder: lbind, mode, body }, kd, &env)
                    }
                }
                | (Type::Exists(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (
                    Type::ManifestKind(ManifestKind {
                        binder: lhs_binder,
                        definition: lhs_definition,
                        body: lhs_body,
                    }),
                    Type::ManifestKind(ManifestKind {
                        binder: _,
                        definition: rhs_definition,
                        body: rhs_body,
                    }),
                ) => {
                    let definition = Lub::lub(lhs_definition, rhs_definition, tycker)?;
                    let body = self.lub(lhs_body, rhs_body, tycker)?;
                    if definition == lhs_definition && body == lhs_body {
                        lhs_id
                    } else {
                        let kind = tycker.statics.annotations_type[&lhs_id];
                        Alloc::alloc(
                            tycker,
                            ManifestKind { binder: lhs_binder, definition, body },
                            kind,
                            &env,
                        )
                    }
                }
                | (Type::ManifestKind(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (Type::Data(lhs), Type::Data(rhs)) => {
                    use std::collections::HashMap;
                    let lhs_query =
                        tycker.statics.datas[&lhs].clone().into_iter().collect::<HashMap<_, _>>();
                    let rhs_query =
                        tycker.statics.datas[&rhs].clone().into_iter().collect::<HashMap<_, _>>();
                    if lhs_query.len() != rhs_query.len() {
                        tycker.err(
                            TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                            std::panic::Location::caller(),
                        )?
                    }
                    for (ctor, lhs_ty) in lhs_query.iter() {
                        let Some(rhs_ty) = rhs_query.get(ctor) else {
                            tycker.err(
                                TyckError::Expressivity("unexpeceted data constructor"),
                                std::panic::Location::caller(),
                            )?
                        };
                        let _ = self.clone().lub(lhs_ty.to_owned(), rhs_ty.to_owned(), tycker)?;
                    }

                    // Fixme: try to make id-equality check work
                    // id-equality check:
                    // if lhs != rhs {
                    //     tycker.err(
                    //         TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    //         std::panic::Location::caller(),
                    //     )?
                    // }

                    lhs_id
                }
                | (Type::Data(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
                | (Type::CoData(lhs), Type::CoData(rhs)) => {
                    use std::collections::HashMap;
                    let lhs_query =
                        tycker.statics.codatas[&lhs].clone().into_iter().collect::<HashMap<_, _>>();
                    let rhs_query =
                        tycker.statics.codatas[&rhs].clone().into_iter().collect::<HashMap<_, _>>();
                    if lhs_query.len() != rhs_query.len() {
                        tycker.err(
                            TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                            std::panic::Location::caller(),
                        )?
                    }
                    for (dtor, lhs_ty) in lhs_query.iter() {
                        let Some(rhs_ty) = rhs_query.get(dtor) else {
                            tycker.err(
                                TyckError::Expressivity("unexpeceted data constructor"),
                                std::panic::Location::caller(),
                            )?
                        };
                        let _ = self.clone().lub(lhs_ty.to_owned(), rhs_ty.to_owned(), tycker)?;
                    }

                    // Fixme: try to make id-equality check work
                    // id-equality check:
                    // if lhs != rhs {
                    //     tycker.err(
                    //         TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    //         std::panic::Location::caller(),
                    //     )?
                    // }

                    lhs_id
                }
                | (Type::CoData(_), _) => tycker.err(
                    TyckError::TypeMismatch { expected: lhs_id, found: rhs_id },
                    std::panic::Location::caller(),
                )?,
            },
        };
        Ok(res)
    }
}

impl Lub for TypeId {
    type Out = TypeId;

    /// We need to remember the definitions introduced by both sides.
    /// We did this by using Debruijn.
    fn lub_inner(self, other: Self, tycker: &mut Tycker) -> Result<Self::Out> {
        Debruijn::new().lub(self, other, tycker)
    }
}

impl Lub for AnnId {
    type Out = AnnId;

    fn lub_inner(self, other: Self, tycker: &mut Tycker) -> Result<Self::Out> {
        let res = match (self, other) {
            | (AnnId::Set, AnnId::Set) => AnnId::Set,
            | (AnnId::Set, _) | (_, AnnId::Set) => {
                tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
            }
            | (AnnId::Kind(lhs), AnnId::Kind(rhs)) => {
                let kd = lhs.lub(rhs, tycker)?;
                kd.into()
            }
            | (AnnId::Kind(_), _) | (_, AnnId::Kind(_)) => {
                tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
            }
            | (AnnId::Type(lhs), AnnId::Type(rhs)) => {
                let ty = lhs.lub(rhs, tycker)?;
                ty.into()
            }
        };
        Ok(res)
    }
}
