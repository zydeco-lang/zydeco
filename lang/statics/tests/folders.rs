mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, Tycker, environment::*, fold::TypeFolder, syntax::*};

#[derive(Default)]
struct Occurrences(usize);

impl TypeFolder for Occurrences {
    fn fold_type(
        &mut self, tycker: &mut Tycker<'_>, source: TypeId,
    ) -> zydeco_statics::Result<TypeId> {
        self.0 += 1;
        let Fillable::Done(node) = tycker.statics.types_pre[&source].clone() else {
            return Ok(source);
        };
        self.fold_children(
            tycker,
            source,
            node,
            tycker.statics.type_kind(source),
            &tycker.statics.env_at(source),
        )
    }
}

#[test]
fn raw_folder_preserves_identities_and_leaves_visit_policy_to_the_client() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let leaf = Alloc::alloc(tycker, UnitTy, vtype, &env);
        let tail = Alloc::alloc(tycker, Prod(vec![leaf, leaf]), vtype, &env);
        let root = Alloc::alloc(tycker, Prod(vec![tail, tail]), vtype, &env);
        let before = tycker.statics.types_pre.len();
        let mut visits = Occurrences::default();
        assert_eq!(
            visits.fold_type(tycker, root).unwrap_or_else(|_| panic!("valid classifier operation")),
            root
        );
        assert_eq!(visits.0, 7);
        assert_eq!(tycker.statics.types_pre.len(), before);
    });
}

#[test]
fn hole_resolution_reuses_changed_shared_tails_and_collects_distinct_missing_solutions() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let site = InferenceSite::Term(tycker.data.root(tycker.db));
        let solved: FillId = Alloc::alloc(tycker, site, (), &());
        let missing: FillId = Alloc::alloc(tycker, site, (), &());
        let other: FillId = Alloc::alloc(tycker, site, (), &());
        let hole: TypeId = Alloc::alloc(tycker, solved, vtype, &env);
        let missing_ty = Alloc::alloc(tycker, missing, vtype, &env);
        let other_ty = Alloc::alloc(tycker, other, vtype, &env);
        let replacement = Alloc::alloc(tycker, UnitTy, vtype, &env);
        solved
            .fill(tycker, replacement.into())
            .unwrap_or_else(|_| panic!("valid classifier operation"));
        let mut root = Alloc::alloc(tycker, Prod(vec![hole, missing_ty, other_ty]), vtype, &env);
        for _ in 0..12 {
            root = Alloc::alloc(tycker, Prod(vec![root, root]), vtype, &env);
        }
        let before = tycker.statics.types_pre.len();
        let (resolved, unresolved) =
            root.solution(tycker).unwrap_or_else(|_| panic!("valid classifier operation"));
        assert_eq!(tycker.statics.types_pre.len() - before, 13);
        assert_eq!(unresolved.len(), 2);
        assert!(unresolved.contains(&missing) && unresolved.contains(&other));
        let Type::Prod(Prod(children)) =
            tycker.type_filled(&resolved).unwrap_or_else(|_| panic!("valid classifier operation"))
        else {
            panic!("product shape");
        };
        assert_eq!(children[0], children[1]);
        missing
            .fill(tycker, replacement.into())
            .unwrap_or_else(|_| panic!("valid classifier operation"));
        other
            .fill(tycker, replacement.into())
            .unwrap_or_else(|_| panic!("valid classifier operation"));
        assert!(
            root.solution(tycker)
                .unwrap_or_else(|_| panic!("valid classifier operation"))
                .1
                .is_empty()
        );
    });
}
