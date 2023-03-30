use super::*;

impl Eqv for () {
    type Ctx = ();
    fn eqv(
        &self, _other: &Self, _ctx: (),
        _f: impl FnOnce() -> Span<TypeCheckError> + Clone,
    ) -> Result<(), Span<TypeCheckError>> {
        Ok(())
    }
}

impl Eqv for Kind {
    type Ctx = ();
    fn eqv(
        &self, other: &Self, _ctx: (),
        f: impl FnOnce() -> Span<TypeCheckError> + Clone,
    ) -> Result<(), Span<TypeCheckError>> {
        bool_test(self == other, f)
    }
}

impl Eqv for Type {
    type Ctx = Ctx;
    fn eqv(
        &self, other: &Self, ctx: Ctx,
        f: impl FnOnce() -> Span<TypeCheckError> + Clone,
    ) -> Result<(), Span<TypeCheckError>> {
        let lhs = &self.synty;
        let rhs = &other.synty;
        // both stuck type variable and type constructor
        bool_test(lhs.tvar == rhs.tvar, f.clone())?;
        // argument length must be equal
        bool_test(lhs.args.len() == rhs.args.len(), f.clone())?;
        // arguments must be equivalent
        for (ty1, ty2) in lhs.args.iter().zip(rhs.args.iter()) {
            ty1.inner_ref().eqv(ty2.inner_ref(), ctx.clone(), f.clone())?;
        }
        Ok(())
    }
}

impl Monoid for Env<TypeV, Type> {
    fn empty() -> Self {
        Self::new()
    }

    fn append(self, ori: Self) -> Self {
        // append on Env is actually composing lazy substitutions, effectively
        //       M [\gamma] [\delta] = M [\delta . \gamma]
        // where we refer to gamma as "original" and delta as "diff" then
        //      new = append(diff, original)
        let mut new = Self::new();
        for (x, ty) in self.clone() {
            if !ori.contains_key(&x) {
                new.insert(x, ty);
            }
        }
        for (x, ty) in ori {
            new.insert(x, ty.subst(self.clone()).unwrap());
        }
        new
    }
}

impl Env<TypeV, Type> {
    pub(super) fn init(
        params: &[(TypeV, Kind)], ty_app_args: &[RcType],
        arity_err: impl FnOnce() -> Span<TypeCheckError>,
    ) -> Result<Self, Span<TypeCheckError>> {
        bool_test(params.len() == ty_app_args.len(), arity_err)?;
        Ok(Env::from_iter(
            params
                .iter()
                .map(|(tvar, _)| tvar.to_owned())
                .zip(ty_app_args.iter().map(|arg| arg.inner_ref().to_owned())),
        ))
    }
}
