mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, environment::*, syntax::*};

#[test]
fn type_application_beta_reduces() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let variable = Alloc::alloc(tycker, VarName("X".to_string()), AnnId::Kind(vtype), &());
        let binder: TPatId = Alloc::alloc(tycker, variable, vtype, &env);
        let body: TypeId = Alloc::alloc(tycker, variable, vtype, &env);
        let function_kind: KindId = Alloc::alloc(tycker, Arrow(vtype, vtype), (), &());
        let function = Alloc::alloc(tycker, Abs(binder, body), function_kind, &env);
        let argument = Alloc::alloc(tycker, UnitTy, vtype, &env);
        let application = Alloc::alloc(tycker, App(function, argument), vtype, &env);

        let Ok(normalized) = application.normalize(tycker, vtype) else {
            panic!("type application failed to normalize")
        };
        assert_eq!(normalized, argument);
    });
}
