mod common;

use common::TestFixture;
use zydeco_statics::{
    Alloc,
    fmt::{Formatter, SealedTypeEquation},
    syntax::*,
};
use zydeco_syntax::Pretty;

#[test]
fn sealed_abstract_types_unroll_to_their_representations() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let representation = Alloc::alloc(tycker, UnitTy, vtype, &());
        let sealed: AbstId = Alloc::alloc(tycker, None::<DefId>, vtype, &());
        tycker.statics.seals.insert_new(sealed, representation);
        let abstract_ty = Alloc::alloc(tycker, sealed, vtype, &());

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
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &());
        let data: DataId = tycker
            .statics
            .datas
            .alloc(Data::new([(CtorName::new("+True"), unit), (CtorName::new("+False"), unit)]));
        let representation = Alloc::alloc(tycker, data, vtype, &());
        let definition = Alloc::alloc(tycker, VarName::new("Bool"), AnnId::Kind(vtype), &());
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
