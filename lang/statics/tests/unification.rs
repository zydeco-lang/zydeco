mod common;

use common::TestFixture;
use zydeco_foundation::prelude::IdAllocator;
use zydeco_statics::{Alloc, Lub, syntax::*};

fn source_site() -> zydeco_statics::surface_syntax::TermId {
    let mut source_ids = IdAllocator::<zydeco_surface::scoped::arena::ScopedScope>::new();
    source_ids.alloc()
}

#[test]
fn structurally_different_primitive_types_do_not_unify() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &());
        let integer = Alloc::alloc(tycker, OpaqueTy, vtype, &());

        assert!(unit.lub(integer, tycker).is_err());
    });
}

#[test]
fn direct_infinite_types_fail_the_occurs_check_without_committing() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let fill: zydeco_statics::syntax::FillId = Alloc::alloc(
            tycker,
            zydeco_statics::syntax::InferenceSite::Term(source_site()),
            (),
            &(),
        );
        let hole = Alloc::alloc(tycker, fill, vtype, &Default::default());
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &());
        let recursive = Alloc::alloc(tycker, Prod(vec![hole, unit]), vtype, &());

        assert!(fill.fill(tycker, recursive.into()).is_err());
        assert!(tycker.statics.solus.get(fill).is_none());
    });
}

#[test]
fn indirect_infinite_types_fail_the_occurs_check_without_committing() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let first: zydeco_statics::syntax::FillId = Alloc::alloc(
            tycker,
            zydeco_statics::syntax::InferenceSite::Term(source_site()),
            (),
            &(),
        );
        let second: zydeco_statics::syntax::FillId = Alloc::alloc(
            tycker,
            zydeco_statics::syntax::InferenceSite::Term(source_site()),
            (),
            &(),
        );
        let first_hole = Alloc::alloc(tycker, first, vtype, &Default::default());
        let second_hole: TypeId = Alloc::alloc(tycker, second, vtype, &Default::default());
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &());
        let recursive = Alloc::alloc(tycker, Prod(vec![first_hole, unit]), vtype, &());

        assert!(first.fill(tycker, second_hole.into()).is_ok());
        assert!(second.fill(tycker, recursive.into()).is_err());
        assert!(tycker.statics.solus.get(second).is_none());
    });
}

#[test]
fn incompatible_constraints_preserve_the_previous_solution() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let fill: zydeco_statics::syntax::FillId = Alloc::alloc(
            tycker,
            zydeco_statics::syntax::InferenceSite::Term(source_site()),
            (),
            &(),
        );
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &());
        let integer = Alloc::alloc(tycker, OpaqueTy, vtype, &());

        assert!(fill.fill(tycker, unit.into()).is_ok());
        assert!(fill.fill(tycker, integer.into()).is_err());
        assert_eq!(tycker.statics.solus.get(fill), Some(&unit.into()));
    });
}

#[test]
fn metavariable_aliases_unify_in_either_order() {
    fn aliased_holes_unify(reverse: bool) {
        TestFixture::run(|tycker| {
            let (vtype, _) = TestFixture::kinds(tycker);
            let first: zydeco_statics::syntax::FillId = Alloc::alloc(
                tycker,
                zydeco_statics::syntax::InferenceSite::Term(source_site()),
                (),
                &(),
            );
            let second: zydeco_statics::syntax::FillId = Alloc::alloc(
                tycker,
                zydeco_statics::syntax::InferenceSite::Term(source_site()),
                (),
                &(),
            );
            let first_hole: TypeId = Alloc::alloc(tycker, first, vtype, &Default::default());
            let second_hole: TypeId = Alloc::alloc(tycker, second, vtype, &Default::default());

            let mut aliases = [(first, second_hole), (second, first_hole)];
            if reverse {
                aliases.reverse();
            }
            for (fill, hole) in aliases {
                assert!(fill.fill(tycker, hole.into()).is_ok());
            }
        });
    }

    aliased_holes_unify(false);
    aliased_holes_unify(true);
}
