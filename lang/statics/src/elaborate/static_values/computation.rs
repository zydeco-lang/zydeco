//! Residualize CBPV computations without executing effects or general recursion.

use super::*;

impl StaticElaborator<'_, '_> {
    pub(super) fn computation(
        &mut self, source: CompuId, env: &Environment,
    ) -> ResultKont<CompuId> {
        let residual = self.computation_inner(source, env)?;
        let ty = self.tycker.statics.annotations_compu[&residual];
        if !self.runtime_type(ty) {
            return self
                .fail(StaticEliminationError::RuntimeComputation { computation: source, ty });
        }
        // Destructor heads can be arbitrary computations, including lets
        // whose source root disappears during static reduction.
        if let Some(hint) = self.tycker.statics.codata_hints.get(&source).copied() {
            let _ = self.tycker.statics.codata_hints.upsert(residual, hint);
        }
        Ok(residual)
    }

    fn computation_inner(&mut self, source: CompuId, env: &Environment) -> ResultKont<CompuId> {
        let ty = self.ty(self.tycker.statics.annotations_compu[&source], env)?;
        let mut bindings = Vec::new();
        let node = match self.tycker.statics.compus[&source].clone() {
            | Computation::Hole(_) => Computation::Hole(Hole),
            | Computation::Let(Let { binder, bindee, tail }) => {
                let value = self.value(bindee, env, &mut bindings)?;
                let value = self.share(value, &mut bindings)?;
                let tail = self.match_arms(value, &[Matcher { binder, tail }], env, source)?;
                return Ok(self.compu_bindings(bindings, tail));
            }
            | Computation::VAbs(Abs(pattern, body)) => {
                let domain = self.ty(self.tycker.statics.annotations_vpat[&pattern], env)?;
                if !self.runtime_type(domain) {
                    return self.fail(StaticEliminationError::RuntimeComputation {
                        computation: source,
                        ty: domain,
                    });
                }
                let (binder, value) = self.variable(None, domain);
                let body =
                    self.match_arms(value, &[Matcher { binder: pattern, tail: body }], env, body)?;
                let body = self.compu_bindings(bindings, body);
                return Ok(self.alloc_compu(source, Computation::VAbs(Abs(binder, body)), ty));
            }
            | Computation::TAbs(Abs(pattern, body)) => {
                Computation::TAbs(Abs(pattern, self.computation(body, env)?))
            }
            | Computation::TApp(App(function, argument)) => {
                let function = self.computation(function, env)?;
                Computation::TApp(App(function, self.ty(argument, env)?))
            }
            | Computation::VApp(App(function, argument)) => {
                let argument = self.value(argument, env, &mut bindings)?;
                let argument = self.reify(&argument)?;
                Computation::VApp(App(self.computation(function, env)?, argument))
            }
            | Computation::Ret(Return(value)) => {
                let value = self.value(value, env, &mut bindings)?;
                Computation::Ret(Return(self.reify(&value)?))
            }
            | Computation::Force(Force(value)) => {
                let value = self.value(value, env, &mut bindings)?;
                Computation::Force(Force(self.reify(&value)?))
            }
            | Computation::Do(Bind { binder: pattern, bindee, tail }) => {
                let bindee = self.computation(bindee, env)?;
                let domain = self.ty(self.tycker.statics.annotations_vpat[&pattern], env)?;
                let (binder, value) = self.variable(None, domain);
                let tail =
                    self.match_arms(value, &[Matcher { binder: pattern, tail }], env, tail)?;
                let tail = self.compu_bindings(bindings, tail);
                return Ok(self.alloc_compu(
                    source,
                    Computation::Do(Bind { binder, bindee, tail }),
                    ty,
                ));
            }
            | Computation::Fix(Fix(pattern, body)) => {
                let domain = self.ty(self.tycker.statics.annotations_vpat[&pattern], env)?;
                let (binder, value) = self.variable(None, domain);
                let mut local = env.clone();
                self.bind(pattern, value, &mut local, &mut bindings)?;
                let body = self.computation(body, &local)?;
                let body = self.compu_bindings(bindings, body);
                return Ok(self.alloc_compu(source, Computation::Fix(Fix(binder, body)), ty));
            }
            | Computation::CoMatch(CoMatch { arms }) => {
                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        Ok(CoMatcher { dtor: arm.dtor, tail: self.computation(arm.tail, env)? })
                    })
                    .collect::<ResultKont<_>>()?;
                Computation::CoMatch(CoMatch { arms })
            }
            | Computation::Dtor(Dtor(function, name)) => {
                Computation::Dtor(Dtor(self.computation(function, env)?, name))
            }
            | Computation::Match(Match { scrut, arms }) => {
                let value = self.value(scrut, env, &mut bindings)?;
                let value = self.share(value, &mut bindings)?;
                // Keep ordinary runtime case analysis as one multiway match.
                // Expanding each row into a complete tag test duplicates the
                // remaining rows once per alternative during backend lowering.
                if matches!(value.0.form, ValueForm::Runtime(_))
                    && arms.iter().all(|arm| !self.has_view(arm.binder))
                {
                    let scrut = self.reify(&value)?;
                    let arms = arms
                        .into_iter()
                        .map(|arm| {
                            let mut local = env.clone();
                            let binder = self.runtime_pattern(arm.binder, &mut local)?;
                            Ok(Matcher { binder, tail: self.computation(arm.tail, &local)? })
                        })
                        .collect::<ResultKont<_>>()?;
                    let body =
                        self.alloc_compu(source, Computation::Match(Match { scrut, arms }), ty);
                    return Ok(self.compu_bindings(bindings, body));
                }
                let body = self.match_arms(value, &arms, env, source)?;
                return Ok(self.compu_bindings(bindings, body));
            }
        };
        let body = self.alloc_compu(source, node, ty);
        Ok(self.compu_bindings(bindings, body))
    }
}
