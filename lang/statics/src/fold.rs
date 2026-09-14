//! Rebuilding the raw classifier graph. Reduction, solution lookup, and memoization belong to clients.

use crate::*;

/// The binder governing one classifier body; clients choose their substitution policy.
pub enum TypeScope<'a> {
    Abstraction(&'a TypeBinder),
    Universal(&'a TypeBinder),
    Existential(&'a TypeBinder),
    ValueFunction(&'a ValPiBinder),
    PackageFunction(&'a PackTelescope),
}

impl TypeScope<'_> {
    pub fn binds(&self, witness: AbstId) -> bool {
        match self {
            | Self::Abstraction(binder) | Self::Universal(binder) | Self::Existential(binder) => {
                binder.witness == witness
            }
            | Self::ValueFunction(ValPiBinder::Type(binder)) => binder.witness == witness,
            | Self::ValueFunction(ValPiBinder::Value(parameter)) => {
                parameter.witnesses.as_ref().is_some_and(|bound| bound.contains(&witness))
            }
            | Self::PackageFunction(bound) => bound.contains(&witness),
        }
    }
}

/// Local classifier transformation with explicit body and nominal-definition boundaries.
///
/// `fold_children` reads an owned raw `Type`, visits its immediate children in structural order,
/// and allocates only when a child or the supplied kind changed. It does not follow fills,
/// seals, inferred solutions, or normalized views implicitly, and has no identity cache.
pub trait TypeFolder: Sized {
    fn fold_type(&mut self, tycker: &mut Tycker<'_>, source: TypeId) -> Result<TypeId>;

    fn fold_kind(&mut self, _tycker: &mut Tycker<'_>, source: KindId) -> Result<KindId> {
        Ok(source)
    }

    fn fold_body(
        &mut self, tycker: &mut Tycker<'_>, source: TypeId, _scope: TypeScope<'_>,
    ) -> Result<TypeId> {
        self.fold_type(tycker, source)
    }

    fn fold_data(&mut self, tycker: &mut Tycker<'_>, source: DataId) -> Result<DataId> {
        let arms = tycker.statics.datas[&source].clone();
        let mut children = TypeChildren { folder: self, tycker, changed: false };
        let arms = arms
            .into_iter()
            .map(|(name, ty)| Ok((name, children.r#type(ty)?)))
            .collect::<Result<Vec<_>>>()?;
        if !children.changed {
            return Ok(source);
        }
        let target = children.tycker.fresh();
        children.tycker.statics.datas.insert_new(target, Data::new(arms));
        Ok(target)
    }

    fn fold_codata(&mut self, tycker: &mut Tycker<'_>, source: CoDataId) -> Result<CoDataId> {
        let arms = tycker.statics.codatas[&source].clone();
        let mut children = TypeChildren { folder: self, tycker, changed: false };
        let arms = arms
            .into_iter()
            .map(|(name, ty)| Ok((name, children.r#type(ty)?)))
            .collect::<Result<Vec<_>>>()?;
        if !children.changed {
            return Ok(source);
        }
        let target = children.tycker.fresh();
        children.tycker.statics.codatas.insert_new(target, CoData::new(arms));
        Ok(target)
    }

    fn fold_children(
        &mut self, tycker: &mut Tycker<'_>, source: TypeId, node: Type, kind: KindId, env: &TyEnv,
    ) -> Result<TypeId> {
        let mut children = TypeChildren { folder: self, tycker, changed: false };
        let node = children.node(node)?;
        Ok(TypeRebuilder::rebuild(children.tycker, source, node, kind, env, children.changed))
    }
}

/// Allocation policy shared by classifier rewrites, including semantic rules that rebuild locally.
pub struct TypeRebuilder;

impl TypeRebuilder {
    pub fn rebuild(
        tycker: &mut Tycker<'_>, source: TypeId, node: Type, kind: KindId, env: &TyEnv,
        changed: bool,
    ) -> TypeId {
        if !changed && kind == tycker.statics.type_kind(source) {
            return source;
        }
        let label = matches!(node, Type::Label(_));
        let target = Alloc::alloc(tycker, node, kind, env);
        if label {
            tycker
                .statics
                .builtin_roles
                .transfer_value(source, target)
                .expect("a fresh rebuilt label cannot have a conflicting role");
            tycker.statics.member_provenance.transfer(source.into(), target.into());
        }
        target
    }
}

struct TypeChildren<'a, 'db, F> {
    folder: &'a mut F,
    tycker: &'a mut Tycker<'db>,
    changed: bool,
}

impl<F: TypeFolder> TypeChildren<'_, '_, F> {
    fn r#type(&mut self, source: TypeId) -> Result<TypeId> {
        let target = self.folder.fold_type(self.tycker, source)?;
        self.changed |= source != target;
        Ok(target)
    }

    fn kind(&mut self, source: KindId) -> Result<KindId> {
        let target = self.folder.fold_kind(self.tycker, source)?;
        self.changed |= source != target;
        Ok(target)
    }

    fn body(&mut self, source: TypeId, scope: TypeScope<'_>) -> Result<TypeId> {
        let target = self.folder.fold_body(self.tycker, source, scope)?;
        self.changed |= source != target;
        Ok(target)
    }

    fn node(&mut self, node: Type) -> Result<Type> {
        Ok(match node {
            | node @ (Type::Var(_)
            | Type::Abst(_)
            | Type::Thk(_)
            | Type::Ret(_)
            | Type::Unit(_)
            | Type::Opaque(_)
            | Type::Primitive(_)
            | Type::OS(_)) => node,
            | Type::Abs(TypeAbstraction { binder, body }) => {
                let body = self.body(body, TypeScope::Abstraction(&binder))?;
                TypeAbstraction { binder, body }.into()
            }
            | Type::App(App(function, argument)) => {
                App(self.r#type(function)?, self.r#type(argument)?).into()
            }
            | Type::Named(Named(name, body)) => Named(name, self.r#type(body)?).into(),
            | Type::Label(Label(name, body)) => Label(name, self.r#type(body)?).into(),
            | Type::Proj(Proj(head, name)) => Proj(self.r#type(head)?, name).into(),
            | Type::ValPi(pi) => {
                let ValPi { mut binder, codomain } = *pi;
                if let ValPiBinder::Value(parameter) = &mut binder {
                    parameter.domain = self.r#type(parameter.domain)?;
                }
                let codomain = self.body(codomain, TypeScope::ValueFunction(&binder))?;
                ValPi { binder, codomain }.into()
            }
            | Type::Arrow(Arrow(domain, codomain)) => {
                Arrow(self.r#type(domain)?, self.r#type(codomain)?).into()
            }
            | Type::Forall(Forall(binder, body)) => {
                let body = self.body(body, TypeScope::Universal(&binder))?;
                Forall(binder, body).into()
            }
            | Type::PackPi(pi) => {
                let PackPi { domain, witnesses, codomain } = *pi;
                let domain = self.r#type(domain)?;
                let codomain = self.body(codomain, TypeScope::PackageFunction(&witnesses))?;
                PackPi { domain, witnesses, codomain }.into()
            }
            | Type::Prod(Prod(items)) => {
                Prod(items.iter().map(|item| self.r#type(*item)).collect::<Result<Vec<_>>>()?)
                    .into()
            }
            | Type::Exists(exists) => {
                let Exists { binder, mode, body } = *exists;
                let mode = match mode {
                    | ExistsMode::Abstract => ExistsMode::Abstract,
                    | ExistsMode::Manifest(definition) => {
                        ExistsMode::Manifest(self.r#type(definition)?)
                    }
                };
                let body = self.body(body, TypeScope::Existential(&binder))?;
                Exists { binder, mode, body }.into()
            }
            | Type::ManifestKind(ManifestKind { binder, definition, body }) => ManifestKind {
                binder,
                definition: self.kind(definition)?,
                body: self.r#type(body)?,
            }
            .into(),
            | Type::Data(source) => {
                let target = self.folder.fold_data(self.tycker, source)?;
                self.changed |= target != source;
                target.into()
            }
            | Type::CoData(source) => {
                let target = self.folder.fold_codata(self.tycker, source)?;
                self.changed |= target != source;
                target.into()
            }
        })
    }
}
