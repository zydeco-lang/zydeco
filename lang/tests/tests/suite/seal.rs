//! Applicative sealing (`seal term : Sig`): opacity, witness parameterization,
//! nested generative `exists`, elaboration/execution, and static rejections.

use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::{CbvBlock, SourceCase};

#[test]
fn zero_param_seal_unifies_within_scope_and_keeps_representation_opaque() {
    let valid = r#"
let module =
  seal
    (Int, (#mk = { fn (x : Int) => ret x }, #read = { fn (x : Int) => ret x }))
  : exists (T : VType) . (#mk :: Thk (Int -> Ret T)) * (#read :: Thk (T -> Ret Int))
in
let (T1, (/mk = mk1; /read = _)) = module in
let (T2, (/mk = _; /read = read2)) = module in
do v <- ! mk1 42;
do n <- ! read2 v;
! exit 0
"#;
    SourceCase::assert_accepted(SourceCase::check_invariants(valid));
    SourceCase::assert_accepted(SourceCase::lower(valid));
    SourceCase::assert_accepted(SourceCase::run(valid));

    let leak = r#"
let module =
  seal
    (Int, (#item = 42))
  : exists (T : VType) . (#item :: T)
in
let (T, (/item = v)) = module in
let (_ : Int) = v in
! exit 0
"#;
    SourceCase::assert_rejected(SourceCase::check(leak), TyckDiagnosticCode::TypeMismatch);
}

#[test]
fn type_parameterized_seal_unifies_for_equal_args_and_distinguishes_distinct_args() {
    let valid_val_pi = r#"
let mk = val (A : VType) =>
  seal
    (Unit, (#zero = (), #id = { fn _ => ret () }))
  : exists (T : VType) . (#zero :: T) * (#id :: Thk (T -> Ret Unit))
in
let (T1, (/zero = z1; /id = _)) = mk Int in
let (T2, (/zero = _; /id = id2)) = mk Int in
do _ <- ! id2 z1;
! exit 0
"#;
    SourceCase::assert_accepted(SourceCase::check_invariants(valid_val_pi));
    SourceCase::assert_accepted(SourceCase::lower(valid_val_pi));
    SourceCase::assert_accepted(SourceCase::run(valid_val_pi));

    let valid_comp = r#"
let mk = {
  fn (A : VType) =>
    ret (
      seal
        (Unit, (#zero = (), #id = { fn _ => ret () }))
      : exists (T : VType) . (#zero :: T) * (#id :: Thk (T -> Ret Unit))
    )
} in
do (T1, (/zero = z1; /id = _)) <- ! mk Int;
do (T2, (/zero = _; /id = id2)) <- ! mk Int;
do _ <- ! id2 z1;
! exit 0
"#;
    SourceCase::assert_accepted(SourceCase::check_invariants(valid_comp));
    SourceCase::assert_accepted(SourceCase::lower(valid_comp));
    SourceCase::assert_accepted(SourceCase::run(valid_comp));

    // Even when the underlying representation (`Unit`) ignores `A`, distinct
    // static type arguments must yield distinct opaque sealed types.
    let mismatch_val_pi = r#"
let mk = val (A : VType) =>
  seal
    (Unit, (#zero = (), #id = { fn _ => ret () }))
  : exists (T : VType) . (#zero :: T) * (#id :: Thk (T -> Ret Unit))
in
let (T1, (/zero = z_int; /id = _)) = mk Int in
let (T2, (/zero = _; /id = id_char)) = mk Char in
! id_char z_int
"#;
    SourceCase::assert_rejected(
        SourceCase::check(mismatch_val_pi),
        TyckDiagnosticCode::TypeMismatch,
    );

    let mismatch_comp = r#"
let mk = {
  fn (A : VType) =>
    ret (
      seal
        (Unit, (#zero = (), #id = { fn _ => ret () }))
      : exists (T : VType) . (#zero :: T) * (#id :: Thk (T -> Ret Unit))
    )
} in
do (T1, (/zero = z_int; /id = _)) <- ! mk Int;
do (T2, (/zero = _; /id = id_char)) <- ! mk Char;
! id_char z_int
"#;
    SourceCase::assert_rejected(SourceCase::check(mismatch_comp), TyckDiagnosticCode::TypeMismatch);
}

#[test]
fn package_witness_parameterized_seal_tracks_opened_witness_identity() {
    let valid = r#"
let Pkg = exists (= Elem : VType) . (#item :: Elem) in
let mk = val ((/Elem; /item = elem) : Pkg) =>
  seal
    (#Box = Elem, #put = { fn _ => ret elem }, #use = { fn _ => ret () })
  : exists (= Box : VType) . (#put :: Thk (Unit -> Ret Box)) * (#use :: Thk (Box -> Ret Unit))
in
let raw : Pkg = (#Elem = Int, #item = 7) in
let (/Elem; /item = v) = raw in
let (/Box; /put = put1; /use = _) = mk (= Elem, #item = v) in
let (/Box = B2; /put = _; /use = use2) = mk (= Elem, #item = v) in
do b <- ! put1 ();
do _ <- ! use2 b;
! exit 0
"#;
    SourceCase::assert_accepted(SourceCase::check_invariants(valid));
    SourceCase::assert_accepted(SourceCase::lower(valid));
    SourceCase::assert_accepted(SourceCase::run(valid));

    let distinct_openings = r#"
let Pkg = exists (= Elem : VType) . (#item :: Elem) in
let mk = val ((/Elem; /item = elem) : Pkg) =>
  seal
    (#Box = Elem, #put = { fn _ => ret elem }, #use = { fn _ => ret () })
  : exists (= Box : VType) . (#put :: Thk (Unit -> Ret Box)) * (#use :: Thk (Box -> Ret Unit))
in
let raw1 : Pkg = (#Elem = Int, #item = 7) in
let raw2 : Pkg = (#Elem = Int, #item = 8) in
let (/Elem = E1; /item = v1) = raw1 in
let (/Elem = E2; /item = v2) = raw2 in
let (/Box; /put = put1; /use = _) = mk (#Elem = E1, #item = v1) in
let (/Box = B2; /put = _; /use = use2) = mk (#Elem = E2, #item = v2) in
do b <- ! put1 ();
! use2 b
"#;
    SourceCase::assert_rejected(
        SourceCase::check(distinct_openings),
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn nested_exists_inside_seal_remains_generative() {
    let nested_generates_distinct_witnesses = r#"
let Inner = exists (Dyn : VType) . (#item :: Dyn) * (#read :: Thk (Dyn -> Ret Unit)) in
let Outer = exists (Tok : VType) . (#fresh :: Thk (Unit -> Ret Inner)) in
let mod_ =
  seal
    (Unit, (#fresh = { fn _ => ret ((Unit, (#item = (), #read = { fn _ => ret () })) : Inner) }))
  : Outer
in
let (Tok, (/fresh = fresh)) = mod_ in
do (D1, (/item = v1; /read = _)) <- ! fresh ();
do (D2, (/item = _; /read = read2)) <- ! fresh ();
! read2 v1
"#;
    SourceCase::assert_rejected(
        SourceCase::check(nested_generates_distinct_witnesses),
        TyckDiagnosticCode::TypeMismatch,
    );

    let same_outer_nested_works = r#"
let Inner = exists (Dyn : VType) . (#item :: Dyn) * (#read :: Thk (Dyn -> Ret Unit)) in
let Outer = exists (Tok : VType) . (#mk :: Thk (Unit -> Ret Tok)) * (#fresh :: Thk (Tok -> Ret Inner)) in
let mod_ =
  seal
    (
      Unit,
      (
        #mk = { fn _ => ret () },
        #fresh = { fn _ => ret ((Unit, (#item = (), #read = { fn _ => ret () })) : Inner) }
      )
    )
  : Outer
in
let (T1, (/mk = mk1; /fresh = _)) = mod_ in
let (T2, (/mk = _; /fresh = fresh2)) = mod_ in
do tok <- ! mk1 ();
do (Dyn, (/item = v; /read = read)) <- ! fresh2 tok;
do _ <- ! read v;
! exit 0
"#;
    SourceCase::assert_accepted(SourceCase::check_invariants(same_outer_nested_works));
    SourceCase::assert_accepted(SourceCase::lower(same_outer_nested_works));
    SourceCase::assert_accepted(SourceCase::run(same_outer_nested_works));
}

#[test]
fn sealing_private_def_hides_constructors_and_runs_cleanly() {
    let valid = r#"
let counter_mod =
  begin
    def Cnt : VType = data | +Zero : Unit | +Step : Cnt end
  that
    let zero : Cnt = +Zero() in
    let inc : Thk (Cnt -> Ret Cnt) = {
      fn c => ret +Step(c)
    } in
    let is_zero : Thk (Cnt -> Ret Int) = {
      fn c => match c | +Zero(_) => ret 0 | +Step(_) => ret 1 end
    } in
    seal (Cnt, (#zero = zero, #inc = inc, #is_zero = is_zero))
    : exists (S : VType) . (#zero :: S) * (#inc :: Thk (S -> Ret S)) * (#is_zero :: Thk (S -> Ret Int))
  end
in
let (S, (/zero = z; /inc = inc; /is_zero = is_zero)) = counter_mod in
do c1 <- ! inc z;
do n <- ! is_zero c1;
! exit 0
"#;
    SourceCase::assert_accepted(SourceCase::check_invariants(valid));
    SourceCase::assert_accepted(SourceCase::lower(valid));
    SourceCase::assert_accepted(SourceCase::run(valid));

    let leaked_ctor_in_scope = r#"
begin
  def Cnt : VType = data | +Zero : Unit | +Step : Cnt end
that
  let counter_mod =
    seal (Cnt, (#zero = +Zero()))
    : exists (S : VType) . (#zero :: S)
  in
  let (S, (/zero = _)) = counter_mod in
  let (forged : S) = (+Zero() : Cnt) in
  ! exit 0
end
"#;
    SourceCase::assert_rejected(
        SourceCase::check(leaked_ctor_in_scope),
        TyckDiagnosticCode::TypeMismatch,
    );

    let private_ctor_uninferable = r#"
let counter_mod =
  begin
    def Cnt : VType = data | +Zero : Unit | +Step : Cnt end
  that
    seal (Cnt, (#zero = +Zero()))
    : exists (S : VType) . (#zero :: S)
  end
in
let (S, (/zero = _)) = counter_mod in
let (forged : S) = +Zero() in
! exit 0
"#;
    SourceCase::assert_rejected(
        SourceCase::check(private_ctor_uninferable),
        TyckDiagnosticCode::MissingAnnotation,
    );
}

#[test]
fn static_only_repr_behind_seal_still_rejects_runtime_elimination() {
    let static_only = r#"
let static_pkg =
  seal
    (val pi (_ : Unit) . Int, (#mk = val (_ : Unit) => 0))
  : exists (F : VType) . (#mk :: F)
in
let (F, (/mk = f)) = static_pkg in
let use : Thk (F -> Ret Unit) = { fn _ => ret () } in
! exit 0
"#;
    SourceCase::assert_rejected(
        SourceCase::lower(static_only),
        TyckDiagnosticCode::StaticElimination,
    );
}

#[test]
fn seal_rejects_unavailable_witness_bad_signature_escaping_skolem_and_cbv() {
    let unknown_runtime_existential = r#"
let Pkg = exists (Elem : VType) . (#item :: Elem) in
let dyn : Thk (Ret Pkg) = { ret ((Int, (#item = 1)) : Pkg) } in
do p <- ! dyn;
let _ = seal p : Pkg in
! exit 0
"#;
    SourceCase::assert_rejected(
        SourceCase::check(unknown_runtime_existential),
        TyckDiagnosticCode::PackageWitnessesUnavailable,
    );

    let non_existential_sig = r#"
let bad = seal (#x = 1) : (#x :: Int) in
! exit 0
"#;
    SourceCase::assert_rejected(
        SourceCase::check(non_existential_sig),
        TyckDiagnosticCode::TypeExpected,
    );

    let all_manifest_sig = r#"
let bad = seal (Int, (#x = 1)) : exists (T as Int) . (#x :: T) in
! exit 0
"#;
    SourceCase::assert_rejected(
        SourceCase::check(all_manifest_sig),
        TyckDiagnosticCode::TypeExpected,
    );

    let escaping_skolem = r#"
let Pkg = exists (Elem : VType) . (#item :: Elem) in
let outer = {
  let dyn : Pkg = (Int, (#item = 42)) in
  let (X, (/item = xv)) = dyn in
  ret (seal (X, (#item = xv)) : exists (A : VType) . (#item :: A))
} in
! exit 0
"#;
    SourceCase::assert_rejected(
        SourceCase::check(escaping_skolem),
        TyckDiagnosticCode::EscapingExistential,
    );

    let in_cbv = CbvBlock::closed(
        r#"
do _ <- @[cbv] (seal (Unit, (#v = ())) : exists (T : VType) . (#v :: T));
ret ()
"#,
    );
    SourceCase::assert_rejected(SourceCase::check(&in_cbv), TyckDiagnosticCode::Expressivity);
}

#[test]
fn seal_allows_unreduced_payload_calls_and_preserves_witness_opacity_in_val_pi() {
    let unreduced_payload = r#"
let with_param = val (f : val pi (_ : Int) . Int) =>
  seal
    (Int, (#zero = f 0, #read = { fn (x : Int) => ret x }))
  : exists (T : VType) . (#zero :: T) * (#read :: Thk (T -> Ret Int))
in
let (T, (/zero = z; /read = read)) = with_param (val (x : Int) => x) in
do n <- ! read z;
! exit 0
"#;
    SourceCase::assert_accepted(SourceCase::check_invariants(unreduced_payload));
    SourceCase::assert_accepted(SourceCase::lower(unreduced_payload));
    SourceCase::assert_accepted(SourceCase::run(unreduced_payload));

    let direct_manifest_rejected = r#"
let sealed_pkg =
  seal (#T = Int, #item = 42)
  : exists (= T : VType) . (#item :: T)
in
let project = val ((/T; /item = x) : exists (= T : VType) . (#item :: T)) => x in
let _ = project sealed_pkg in
! exit 0
"#;
    SourceCase::assert_rejected(
        SourceCase::check(direct_manifest_rejected),
        TyckDiagnosticCode::TypeMismatch,
    );

    let val_pi_opacity_ok = r#"
let sealed_pkg =
  seal (#T = Int, #item = 42)
  : exists (= T : VType) . (#item :: T)
in
let (/T = SealedT; /item = item) = sealed_pkg in
let project = val ((/T; /item = x) : exists (= T : VType) . (#item :: T)) => x in
let (ok : SealedT) = project (#T = SealedT, #item = item) in
! exit 0
"#;
    SourceCase::assert_accepted(SourceCase::check_invariants(val_pi_opacity_ok));
    SourceCase::assert_accepted(SourceCase::lower(val_pi_opacity_ok));
    SourceCase::assert_accepted(SourceCase::run(val_pi_opacity_ok));

    let val_pi_opacity_leak = r#"
let sealed_pkg =
  seal (#T = Int, #item = 42)
  : exists (= T : VType) . (#item :: T)
in
let (/T = SealedT; /item = item) = sealed_pkg in
let project = val ((/T; /item = x) : exists (= T : VType) . (#item :: T)) => x in
let (leaked : Int) = project (#T = SealedT, #item = item) in
! exit 0
"#;
    SourceCase::assert_rejected(
        SourceCase::check(val_pi_opacity_leak),
        TyckDiagnosticCode::TypeMismatch,
    );
}
