//! Preserve checked field selection as translated packages acquire structures.

use super::*;

pub(super) struct ValueProjectionTranslation {
    pub head: ValueId,
    pub field: ResolvedField,
    pub result: TypeId,
}

impl ValueProjectionTranslation {
    pub fn translate(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let (env, mut source) = cs::TypeOf(self.head).mbuild(tycker, env)?;
        let (env, head) = cs::TermLift { tm: self.head }.mbuild(tycker, env)?;
        let (env, mut target) = cs::TypeOf(head).mbuild(tycker, env)?;
        let mut source_steps = self.field.target.products.into_iter();
        let mut products = Vec::new();

        // Follow the already resolved source route. The only extra product
        // step selects the payload beside an existential's inserted structure.
        // This linear walk neither searches for fields nor rechecks source terms.
        loop {
            source = source.unroll(tycker)?.subst_env(tycker, &env.ty)?;
            target = target.unroll(tycker)?.subst_env(tycker, &env.ty)?;
            match (tycker.type_filled(&source)?.to_owned(), tycker.type_filled(&target)?.to_owned())
            {
                | (Type::Label(Label(name, source_body)), Type::Label(Label(_, target_body))) => {
                    if name == self.field.name {
                        assert!(
                            source_steps.next().is_none(),
                            "checked field route ends at its label"
                        );
                        Lub::lub(self.result, target_body, tycker)?;
                        break;
                    }
                    source = source_body;
                    target = target_body;
                }
                | (Type::Prod(Prod(source_fields)), Type::Prod(Prod(target_fields))) => {
                    let step = source_steps
                        .next()
                        .expect("checked field route selects a product component");
                    products.push(ProductProjection { product: target, position: step.position });
                    source = source_fields[step.position];
                    target = target_fields[step.position];
                }
                | (Type::ManifestKind(source_package), Type::ManifestKind(target_package)) => {
                    source = source_package.body;
                    target = target_package.body;
                }
                | (Type::Exists(source_package), Type::Exists(target_package)) => {
                    let (
                        ExistsMode::Manifest(source_witness),
                        ExistsMode::Manifest(target_witness),
                    ) = (source_package.mode, target_package.mode)
                    else {
                        unreachable!("term projections only cross manifest package witnesses")
                    };
                    source = source_package
                        .body
                        .subst_abst(tycker, (source_package.binder.witness, source_witness))?;
                    let product = target_package
                        .body
                        .subst_abst(tycker, (target_package.binder.witness, target_witness))?;
                    let Type::Prod(Prod(fields)) = tycker.type_filled(&product)?.to_owned() else {
                        unreachable!("translated existential carries its structure and payload")
                    };
                    products.push(ProductProjection { product, position: 1 });
                    target = fields[1];
                }
                | _ => unreachable!("translated field route follows the checked receiver shape"),
            }
        }
        let field = ResolvedField { name: self.field.name, target: ProjTarget { products } };
        let projected = Alloc::alloc(tycker, Proj(head, field), self.result, &env.ty);
        Ok((env, projected))
    }
}
