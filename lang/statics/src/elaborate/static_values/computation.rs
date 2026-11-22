//! Residualize CBPV computations without executing effects or general recursion.

use super::*;

impl StaticElaborator<'_, '_> {
    pub(super) fn computation(
        &mut self, source: CompuId, env: &Environment,
    ) -> Result<ir::CompuId> {
        zydeco_foundation::stack::maybe_grow(|| {
            let _scope = self.enter_scope();
            let result = self.computation_in_scope(source, env);
            let frame = self.exit_scope();
            result.map(|residual| self.wrap_compu_bindings(frame.bindings, residual))
        })
    }

    fn computation_in_scope(&mut self, source: CompuId, env: &Environment) -> Result<ir::CompuId> {
        let ty = self.ty(self.tycker.statics.annotations_compu[&source], env)?;
        if !self.runtime_type(ty) {
            return self
                .fail(StaticEliminationError::RuntimeComputation { computation: source, ty });
        }
        let residual = match self.tycker.statics.compus[&source].clone() {
            | Computation::Hole(_) => {
                let id = self.alloc_core_compu(source, ir::Computation::Hole(Hole), ty, None);
                if self.purpose == Purpose::Residualize && self.reduction_depth == 0 {
                    self.core.retained_holes.push(ir::TermId::Compu(id));
                }
                id
            }
            | Computation::Let(Let { binder, bindee, tail }) => {
                let value = self.value(bindee, env)?;
                let value = self.share(value)?;
                self.match_arms(value, &[Matcher { binder, tail }], env, source)?
            }
            | Computation::VAbs(Abs(pattern, body)) => {
                let domain = self.ty(self.tycker.statics.annotations_vpat[&pattern], env)?;
                if !self.runtime_type(domain) {
                    return self.fail(StaticEliminationError::RuntimeComputation {
                        computation: source,
                        ty: domain,
                    });
                }
                let body_scope = self.enter_scope();
                let name = self.pattern_var_hint(pattern);
                let (binder, value) = self.named_variable(name, Some(pattern), domain, body_scope);
                let body_ir =
                    self.match_arms(value, &[Matcher { binder: pattern, tail: body }], env, source);
                let frame = self.exit_scope();
                let body_ir = self.wrap_compu_bindings(frame.bindings, body_ir?);
                self.alloc_core_compu(source, ir::Computation::VAbs(Abs(binder, body_ir)), ty, None)
            }
            | Computation::TAbs(Abs(_, body)) | Computation::TApp(App(body, _)) => {
                self.computation(body, env)?
            }
            | Computation::VApp(App(function, argument)) => {
                let argument = self.value(argument, env)?;
                let argument = self.reify(&argument)?;
                let function = self.computation(function, env)?;
                self.alloc_core_compu(
                    source,
                    ir::Computation::VApp(App(function, argument)),
                    ty,
                    None,
                )
            }
            | Computation::Ret(Return(value)) => {
                let value = self.value(value, env)?;
                let value = self.reify(&value)?;
                self.alloc_core_compu(source, ir::Computation::Ret(Return(value)), ty, None)
            }
            | Computation::Force(Force(value)) => {
                let value = self.value(value, env)?;
                let value = self.reify(&value)?;
                self.alloc_core_compu(source, ir::Computation::Force(Force(value)), ty, None)
            }
            | Computation::Do(Bind { binder: pattern, bindee, tail }) => {
                let bindee = self.computation(bindee, env)?;
                let domain = self.ty(self.tycker.statics.annotations_vpat[&pattern], env)?;
                let tail_scope = self.enter_scope();
                let name = self.pattern_var_hint(pattern);
                let (binder, value) = self.named_variable(name, Some(pattern), domain, tail_scope);
                let tail_ir =
                    self.match_arms(value, &[Matcher { binder: pattern, tail }], env, source);
                let frame = self.exit_scope();
                let tail_ir = self.wrap_compu_bindings(frame.bindings, tail_ir?);
                self.alloc_core_compu(
                    source,
                    ir::Computation::Do(Bind { binder, bindee, tail: tail_ir }),
                    ty,
                    None,
                )
            }
            | Computation::Fix(Fix(pattern, body)) => {
                let domain = self.ty(self.tycker.statics.annotations_vpat[&pattern], env)?;
                let fix_scope = self.enter_scope();
                let name = self.pattern_var_hint(pattern);
                let (param, value) = self.named_var_only(name, domain, fix_scope);
                let mut local = env.clone();
                let bound = self.bind(pattern, value, &mut local);
                let body_ir = bound.and_then(|()| self.computation(body, &local));
                let frame = self.exit_scope();
                let body_ir = self.wrap_compu_bindings(frame.bindings, body_ir?);
                self.alloc_core_compu(
                    source,
                    ir::Computation::Fix { param, body: body_ir },
                    ty,
                    None,
                )
            }
            | Computation::CoMatch(CoMatch { arms }) => {
                self.comatch_in_scope(source, ty, arms, env)?
            }
            | Computation::Dtor(Dtor(function, name)) => {
                let function_ty = self.ty(self.tycker.statics.annotations_compu[&function], env)?;
                let codata = self
                    .tycker
                    .statics
                    .codata_hints
                    .get(function)
                    .copied()
                    .or_else(|| self.resolve_codata_id(function_ty))
                    .expect("checked destructor head has a codata classifier");
                let tag = self.protocols.tag(&self.tycker.statics, codata, name);
                let body = self.computation(function, env)?;
                self.alloc_core_compu(source, ir::Computation::Dtor { body, tag }, ty, None)
            }
            | Computation::Match(Match { scrut, arms }) => {
                self.computation_match(source, ty, scrut, arms, env)?
            }
        };
        Ok(residual)
    }

    fn comatch_in_scope(
        &mut self, source: CompuId, ty: TypeId, arms: Vec<CoMatcher<DtorName, CompuId>>,
        env: &Environment,
    ) -> Result<ir::CompuId> {
        let codata = self
            .tycker
            .statics
            .codata_hints
            .get(source)
            .copied()
            .or_else(|| self.resolve_codata_id(ty))
            .expect("checked comatch has a codata classifier");
        let arms = arms
            .into_iter()
            .map(|arm| {
                let tag = self.protocols.tag(&self.tycker.statics, codata, arm.dtor);
                let tail = self.computation(arm.tail, env)?;
                Ok(CoMatcher { dtor: tag, tail })
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(self.alloc_core_compu(source, ir::Computation::CoMatch { arms }, ty, Some(codata)))
    }

    fn computation_match(
        &mut self, source: CompuId, ty: TypeId, scrut: ValueId,
        arms: Vec<Matcher<VPatId, CompuId>>, env: &Environment,
    ) -> Result<ir::CompuId> {
        let value = self.value(scrut, env)?;
        let value = self.share(value)?;
        // Keep ordinary runtime case analysis as one multiway match.
        // Expanding each row into a complete tag test duplicates the
        // remaining rows once per alternative during backend lowering.
        if value.is_opaque_runtime() && arms.iter().all(|arm| !self.has_view(arm.binder)) {
            let scrut = self.reify(&value)?;
            let arms = arms
                .into_iter()
                .map(|arm| {
                    let _arm_scope = self.enter_scope();
                    let mut local = env.clone();
                    let binder = self.runtime_pattern(arm.binder, &mut local);
                    let tail = binder.and_then(|binder| {
                        self.computation(arm.tail, &local).map(|tail| (binder, tail))
                    });
                    let frame = self.exit_scope();
                    tail.map(|(binder, tail)| Matcher {
                        binder,
                        tail: self.wrap_compu_bindings(frame.bindings, tail),
                    })
                })
                .collect::<Result<Vec<_>>>()?;
            Ok(self.alloc_core_compu(
                source,
                ir::Computation::Match(Match { scrut, arms }),
                ty,
                None,
            ))
        } else {
            self.match_arms(value, &arms, env, source)
        }
    }
}
