use super::{Ctor, CtorName, IntegerLiteral, Literal, SemValue, Triv};

struct ListFixture;

impl ListFixture {
    fn list(length: i64) -> SemValue {
        let nil = SemValue::Ctor(Ctor(CtorName("Nil".into()), SemValue::Triv(Triv).into()));
        (0..length).fold(nil, |tail, head| {
            let fields = SemValue::VCons(
                [SemValue::Literal(Literal::Integer(IntegerLiteral::Int(head))), tail].into(),
            );
            SemValue::Ctor(Ctor(CtorName("Cons".into()), fields.into()))
        })
    }
}

// https://github.com/zydeco-lang/zydeco/issues/71
// Deep-copying the alternating constructor/product spine made build/walk quadratic.
// Check each layer's storage identity rather than machine-dependent timing ratios.
#[test]
fn cloning_a_constructor_shares_its_payload() {
    for length in [0, 1, 64] {
        let original = ListFixture::list(length);
        let cloned = original.clone();
        let (SemValue::Ctor(Ctor(tag, payload)), SemValue::Ctor(Ctor(cloned_tag, cloned_payload))) =
            (&original, &cloned)
        else {
            panic!("cloning a list must preserve its constructor");
        };
        assert_eq!(tag, cloned_tag);
        assert!(
            std::ptr::eq(payload.as_ref(), cloned_payload.as_ref()),
            "cloning a list of length {length} copied its constructor payload"
        );
    }
}

#[test]
fn cloning_a_product_shares_its_components() {
    for length in [1, 2, 64] {
        let SemValue::Ctor(Ctor(_, payload)) = ListFixture::list(length) else {
            panic!("expected a list constructor");
        };
        // Pattern binding clones the payload separately from the enclosing constructor.
        // Constructor sharing alone must not hide a deep copy at this second layer.
        let cloned = payload.as_ref().clone();
        let (SemValue::VCons(fields), SemValue::VCons(cloned_fields)) = (payload.as_ref(), &cloned)
        else {
            panic!("cloning a Cons payload must preserve its product");
        };
        assert_eq!(fields.len(), 2);
        assert!(
            std::ptr::eq(fields.as_ptr(), cloned_fields.as_ptr()),
            "cloning a list of length {length} copied its product components"
        );
    }
}
