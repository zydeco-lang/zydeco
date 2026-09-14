//! Materialize Builtin package plans through the shared folder drivers.

use super::*;
use zydeco_utils::fold::{Folder, Step};

pub(super) struct BuiltinPackageFolder<'lo, 'source> {
    pub lowerer: &'lo mut Lowerer<'source>,
}

pub(super) struct ProductFrame {
    remaining: std::vec::IntoIter<BuiltinPackageValue>,
    fields: Vec<ValueId>,
}

impl Folder for BuiltinPackageFolder<'_, '_> {
    type Input = BuiltinPackageValue;
    type Output = ValueId;
    type Frame = ProductFrame;

    fn enter(&mut self, input: BuiltinPackageValue) -> Step<Self> {
        match input {
            | BuiltinPackageValue::Unit => Step::Return(Triv.build(self.lowerer, None)),
            | BuiltinPackageValue::Operation(role) => {
                Step::Return(ExternalFunction::Host(role).make_function(self.lowerer))
            }
            | BuiltinPackageValue::Product(product) => {
                let fields = Vec::with_capacity(product.len());
                self.product(ProductFrame { remaining: product.into_iter(), fields })
            }
        }
    }

    fn resume(&mut self, mut frame: ProductFrame, child: ValueId) -> Step<Self> {
        frame.fields.push(child);
        self.product(frame)
    }
}

impl BuiltinPackageFolder<'_, '_> {
    fn product(&mut self, mut frame: ProductFrame) -> Step<Self> {
        match frame.remaining.next() {
            | Some(input) => Step::Call { input, frame },
            | None => {
                let layout = ProductLayout { arity: frame.fields.len() };
                Step::Return(VCons::new(frame.fields, layout).build(self.lowerer, None))
            }
        }
    }
}
