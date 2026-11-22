//! Backend-independent interpretation of a checked Builtin package signature.

use zydeco_syntax::BuiltinValueRole;

/// One runtime-relevant component of a checked Builtin package.
#[derive(Clone, Debug)]
pub enum BuiltinPackageValue {
    Unit,
    Operation(BuiltinValueRole),
    Product(Vec<BuiltinPackageValue>),
}

/// Backend-independent instructions for materializing the host Builtin package.
#[derive(Clone, Debug)]
pub struct BuiltinPackagePlan {
    pub value: BuiltinPackageValue,
}
