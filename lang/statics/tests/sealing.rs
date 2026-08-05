mod common;

use common::TestFixture;
use zydeco_statics::{
    Alloc,
    environment::*,
    fmt::{Formatter, SealedTypeEquation},
    syntax::*,
};
use zydeco_syntax::Pretty;

#[test]
fn sealed_abstract_types_unroll_to_their_representations() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let representation = Alloc::alloc(tycker, UnitTy, vtype, &env);
        let sealed: AbstId = Alloc::alloc(tycker, None::<DefId>, vtype, &());
        tycker.statics.seals.insert_new(sealed, representation);
        let abstract_ty = Alloc::alloc(tycker, sealed, vtype, &env);

        let Ok(unrolled) = abstract_ty.unroll(tycker) else {
            panic!("sealed abstract type failed to unroll")
        };
        assert_eq!(unrolled, representation);
    });
}

#[test]
fn sealed_types_pretty_print_as_names_with_separate_equations() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &env);
        let data = tycker.statics.datas.alloc(Data::new([
            (CtorName("+True".to_owned()), unit),
            (CtorName("+False".to_owned()), unit),
        ]));
        let representation = Alloc::alloc(tycker, data, vtype, &env);
        let definition = Alloc::alloc(tycker, VarName("Bool".to_owned()), AnnId::Kind(vtype), &());
        let sealed: AbstId = Alloc::alloc(tycker, definition, vtype, &());
        tycker.statics.seals.insert_new(sealed, representation);
        let formatter = Formatter::new(tycker.scoped, &tycker.statics);

        let mut name = String::new();
        sealed.pretty(&formatter).render_fmt(90, &mut name).unwrap();
        assert_eq!(name, "Bool");

        let mut equation = String::new();
        SealedTypeEquation::new(&tycker.statics, sealed)
            .unwrap()
            .pretty(&formatter)
            .render_fmt(90, &mut equation)
            .unwrap();
        assert_eq!(
            equation,
            concat!(
                "Bool : VType\n",
                "  = data\n",
                "    | +True : Unit\n",
                "    | +False : Unit\n",
                "    end"
            )
        );
    });
}
