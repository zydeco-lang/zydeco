mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, Lub, tyck::syntax::*};
use zydeco_utils::prelude::{ArenaAccess, IdAllocator};

fn source_site() -> zydeco_statics::surface_syntax::TermId {
    let mut source_ids = IdAllocator::<zydeco_surface::bitter::arena::BitterScope>::new();
    source_ids.alloc()
}

#[test]
fn structurally_different_primitive_types_do_not_unify() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &env);
        let integer = Alloc::alloc(tycker, IntTy, vtype, &env);

        assert!(unit.lub(integer, tycker).is_err());
    });
}

#[test]
fn direct_infinite_types_fail_the_occurs_check_without_committing() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let fill = tycker.statics.fills.alloc(source_site().into());
        let hole = Alloc::alloc(tycker, fill, vtype, &env);
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &env);
        let recursive = Alloc::alloc(tycker, Prod(hole, unit), vtype, &env);

        assert!(fill.fill(tycker, recursive.into()).is_err());
        assert!(tycker.statics.solus.get(&fill).is_none());
    });
}

#[test]
fn indirect_infinite_types_fail_the_occurs_check_without_committing() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let first = tycker.statics.fills.alloc(source_site().into());
        let second = tycker.statics.fills.alloc(source_site().into());
        let first_hole = Alloc::alloc(tycker, first, vtype, &env);
        let second_hole: TypeId = Alloc::alloc(tycker, second, vtype, &env);
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &env);
        let recursive = Alloc::alloc(tycker, Prod(first_hole, unit), vtype, &env);

        assert!(first.fill(tycker, second_hole.into()).is_ok());
        assert!(second.fill(tycker, recursive.into()).is_err());
        assert!(tycker.statics.solus.get(&second).is_none());
    });
}

#[test]
fn incompatible_constraints_preserve_the_previous_solution() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let fill = tycker.statics.fills.alloc(source_site().into());
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &env);
        let integer = Alloc::alloc(tycker, IntTy, vtype, &env);

        assert!(fill.fill(tycker, unit.into()).is_ok());
        assert!(fill.fill(tycker, integer.into()).is_err());
        assert_eq!(tycker.statics.solus.get(&fill), Some(&unit.into()));
    });
}

#[test]
fn metavariable_aliases_unify_in_either_order() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let first = tycker.statics.fills.alloc(source_site().into());
        let second = tycker.statics.fills.alloc(source_site().into());
        let first_hole: TypeId = Alloc::alloc(tycker, first, vtype, &env);
        let second_hole: TypeId = Alloc::alloc(tycker, second, vtype, &env);

        assert!(first.fill(tycker, second_hole.into()).is_ok());
        assert!(second.fill(tycker, first_hole.into()).is_ok());
    });
}
