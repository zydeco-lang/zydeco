//! Type-directed elaboration of generalized comatch clauses.

use super::*;
use std::collections::VecDeque;

struct PendingArgument {
    binder: ss::VPatId,
    value: ss::ValueId,
    ty: ss::TypeId,
}

struct ClauseState {
    items: VecDeque<su::CoPatternItem>,
    body: su::TermId,
    env: ss::TyEnv,
    pending: Vec<PendingArgument>,
}

impl ClauseState {
    fn from_source(clause: su::CoPatternClause, env: &ss::TyEnv) -> Self {
        let su::CoPatternClause { spine, tail } = clause;
        Self {
            items: spine.into_items().collect(),
            body: tail,
            env: env.clone(),
            pending: Vec::new(),
        }
    }

    fn next_step(&self) -> CopatternStep {
        match self.items.front() {
            | Some(su::CoPatternItem::Pat(_)) => CopatternStep::Pattern,
            | Some(su::CoPatternItem::Dtor(dtor)) => CopatternStep::Destructor(dtor.clone()),
            | None => CopatternStep::End,
        }
    }

    fn pop(mut self) -> (su::CoPatternItem, Self) {
        let item = self.items.pop_front().expect("a checked copattern step is present");
        (item, self)
    }
}

struct DestructorGroup {
    dtor: ss::DtorName,
    clauses: Vec<ClauseState>,
}

struct FreshArgument {
    definition: ss::DefId,
    binder: ss::VPatId,
    value: ss::ValueId,
    body_env: ss::TyEnv,
}

/// Elaborates one source `comatch` clause matrix against its expected computation type.
pub(super) struct CopatternElaborator {
    source: su::TermId,
    expected: ss::TypeId,
    clauses: Vec<ClauseState>,
    allocation_env: ss::TyEnv,
}

impl CopatternElaborator {
    pub(super) fn new(
        source: su::TermId, syntax: su::CoMatchClauses, expected: ss::TypeId, env: &ss::TyEnv,
    ) -> Self {
        let clauses = syntax
            .clauses
            .into_iter()
            .map(|clause| ClauseState::from_source(clause, env))
            .collect();
        Self { source, expected, clauses, allocation_env: env.clone() }
    }

    fn with_clauses(
        &self, expected: ss::TypeId, clauses: Vec<ClauseState>, env: ss::TyEnv,
    ) -> Self {
        Self { source: self.source, expected, clauses, allocation_env: env }
    }

    pub(super) fn elaborate_k(self, tycker: &mut Tycker<'_>) -> ResultKont<ss::CompuId> {
        let ended = self.clauses.iter().filter(|clause| clause.items.is_empty()).count();
        if ended != 0 {
            if ended == self.clauses.len() {
                return self.finish_clauses_k(tycker);
            }
            return tycker
                .err_k(TyckError::OverlappingCopatternClauses, std::panic::Location::caller());
        }

        let expected_view =
            self.expected.unroll_k(tycker)?.subst_env_k(tycker, &self.allocation_env)?;
        match tycker.type_filled_k(&expected_view)?.to_owned() {
            | ss::Type::CoData(codata) => self.elaborate_codata_k(tycker, codata),
            | ss::Type::Arrow(ss::Arrow(domain, codomain)) => {
                self.elaborate_arrow_k(tycker, domain, codomain)
            }
            | ss::Type::Forall(ss::Forall(binder, body)) => {
                self.elaborate_forall_k(tycker, binder, body)
            }
            | ss::Type::PackPi(signature) => self.elaborate_pack_pi_k(tycker, signature),
            | _ if self.clauses.is_empty() => tycker.err_k(
                TyckError::NonExhaustiveCopattern { expected: self.expected },
                std::panic::Location::caller(),
            ),
            | _ => {
                let found =
                    self.clauses.first().map(ClauseState::next_step).unwrap_or(CopatternStep::End);
                tycker.err_k(
                    TyckError::CopatternStepMismatch { expected: CopatternStepKind::Body, found },
                    std::panic::Location::caller(),
                )
            }
        }
    }

    fn finish_clauses_k(mut self, tycker: &mut Tycker<'_>) -> ResultKont<ss::CompuId> {
        let clauses = std::mem::take(&mut self.clauses);
        let arity = clauses
            .first()
            .map(|clause| clause.pending.len())
            .expect("terminal copattern elaboration has at least one clause");
        if clauses.iter().any(|clause| clause.pending.len() != arity) {
            return tycker
                .err_k(TyckError::OverlappingCopatternClauses, std::panic::Location::caller());
        }
        if arity == 0 {
            if clauses.len() != 1 {
                return tycker
                    .err_k(TyckError::OverlappingCopatternClauses, std::panic::Location::caller());
            }
            let clause =
                clauses.into_iter().next().expect("one terminal copattern clause is present");
            return self.check_body_k(tycker, clause);
        }

        let first = clauses.first().expect("terminal copattern clauses are nonempty");
        let values = first.pending.iter().map(|argument| argument.value).collect::<Vec<_>>();
        let types = first.pending.iter().map(|argument| argument.ty).collect::<Vec<_>>();
        assert!(clauses.iter().all(|clause| {
            clause
                .pending
                .iter()
                .zip(&values)
                .all(|(argument, expected)| argument.value == *expected)
        }));
        let (scrutinee, scrutinee_type) =
            self.combine_values_k(tycker, values, types, &self.allocation_env);
        let arms = clauses
            .into_iter()
            .map(|clause| -> ResultKont<_> {
                let ClauseState { body, env, pending, .. } = clause;
                let patterns = pending.iter().map(|argument| argument.binder).collect::<Vec<_>>();
                let binder =
                    self.combine_patterns(tycker, patterns, scrutinee_type, &self.allocation_env);
                let checked =
                    TyEnvT::new(env, body).tyck_k(tycker, Action::ana(self.expected.into()))?;
                let (tail, _) = checked.try_as_compu(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                Ok(ss::Matcher { binder, tail })
            })
            .collect::<ResultKont<Vec<_>>>()?;
        let computation = Alloc::alloc(
            tycker,
            ss::Match { scrut: scrutinee, arms },
            self.expected,
            &self.allocation_env,
        );
        tycker.statics.copattern_matches.insert_new(computation, ());
        self.record(tycker, computation);
        Ok(computation)
    }

    fn combine_values_k(
        &self, tycker: &mut Tycker<'_>, values: Vec<ss::ValueId>, types: Vec<ss::TypeId>,
        env: &ss::TyEnv,
    ) -> (ss::ValueId, ss::TypeId) {
        assert_eq!(values.len(), types.len());
        let mut values = values.into_iter().rev();
        let mut types = types.into_iter().rev();
        let tail = values.next().expect("a nonempty copattern argument tuple has a tail");
        let tail_type = types.next().expect("a nonempty copattern argument tuple has a type");
        let fields = values.zip(types).collect::<Vec<_>>();
        if fields.is_empty() {
            (tail, tail_type)
        } else {
            let vtype = ss::VType.build(tycker, env);
            let product_type = fields.iter().fold(tail_type, |tail_type, (_, value_type)| {
                Alloc::alloc(tycker, ss::Prod(*value_type, tail_type), vtype, env)
            });
            let items = fields.into_iter().rev().map(|(value, _)| value).collect();
            let tuple = Alloc::alloc(tycker, ss::ConsN(items, tail), product_type, env);
            (tuple, product_type)
        }
    }

    fn combine_patterns(
        &self, tycker: &mut Tycker<'_>, patterns: Vec<ss::VPatId>, product_type: ss::TypeId,
        env: &ss::TyEnv,
    ) -> ss::VPatId {
        let mut patterns = patterns.into_iter().rev();
        let tail = patterns.next().expect("a nonempty copattern pattern tuple has a tail");
        let items = patterns.rev().collect::<Vec<_>>();
        if items.is_empty() {
            tail
        } else {
            Alloc::alloc(tycker, ss::ConsN(items, tail), product_type, env)
        }
    }

    fn check_body_k(
        &self, tycker: &mut Tycker<'_>, clause: ClauseState,
    ) -> ResultKont<ss::CompuId> {
        let checked = TyEnvT::new(clause.env, clause.body)
            .tyck_k(tycker, Action::ana(self.expected.into()))?;
        let (body, _) = checked.try_as_compu(
            tycker,
            TyckError::SortMismatch,
            std::panic::Location::caller(),
        )?;
        Ok(body)
    }

    fn elaborate_codata_k(
        mut self, tycker: &mut Tycker<'_>, codata: ss::CoDataId,
    ) -> ResultKont<ss::CompuId> {
        let clauses = std::mem::take(&mut self.clauses);
        let groups = clauses.into_iter().try_fold(
            Vec::<DestructorGroup>::new(),
            |mut groups, clause| -> ResultKont<_> {
                let found = clause.next_step();
                let (item, clause) = clause.pop();
                let su::CoPatternItem::Dtor(dtor) = item else {
                    return tycker.err_k(
                        TyckError::CopatternStepMismatch {
                            expected: CopatternStepKind::Destructor,
                            found,
                        },
                        std::panic::Location::caller(),
                    );
                };
                match groups.iter_mut().find(|group| group.dtor == dtor) {
                    | Some(group) => group.clauses.push(clause),
                    | None => groups.push(DestructorGroup { dtor, clauses: vec![clause] }),
                }
                Ok(groups)
            },
        )?;

        let declaration = tycker.statics.codatas[&codata].clone();
        let arms = groups
            .into_iter()
            .map(|DestructorGroup { dtor, clauses }| -> ResultKont<_> {
                let Some(arm_type) = declaration.get(&dtor) else {
                    return tycker.err_k(
                        TyckError::UnknownCoDataDestructor(dtor),
                        std::panic::Location::caller(),
                    );
                };
                let tail = self
                    .with_clauses(arm_type, clauses, self.allocation_env.clone())
                    .elaborate_k(tycker)?;
                Ok(ss::CoMatcher { dtor, tail })
            })
            .collect::<ResultKont<Vec<_>>>()?;
        let computation =
            Alloc::alloc(tycker, ss::CoMatch { arms }, self.expected, &self.allocation_env);
        tycker.statics.codata_hints.insert_new(computation, codata);
        self.record(tycker, computation);
        Ok(computation)
    }

    fn elaborate_arrow_k(
        mut self, tycker: &mut Tycker<'_>, domain: ss::TypeId, codomain: ss::TypeId,
    ) -> ResultKont<ss::CompuId> {
        let argument = self.fresh_argument_k(tycker, domain)?;
        let clauses = std::mem::take(&mut self.clauses);
        if clauses.is_empty() {
            let body = Alloc::alloc(
                tycker,
                ss::Match { scrut: argument.value, arms: Vec::new() },
                codomain,
                &argument.body_env,
            );
            tycker.statics.copattern_matches.insert_new(body, ());
            self.record(tycker, body);
            let abstraction = Alloc::alloc(
                tycker,
                ss::Abs(argument.binder, body),
                self.expected,
                &self.allocation_env,
            );
            self.record(tycker, abstraction);
            return Ok(abstraction);
        }
        let clauses = clauses
            .into_iter()
            .map(|clause| -> ResultKont<_> {
                let found = clause.next_step();
                let binding_env = clause.env.clone();
                let (item, mut clause) = clause.pop();
                let su::CoPatternItem::Pat(pattern) = item else {
                    return tycker.err_k(
                        TyckError::CopatternStepMismatch {
                            expected: CopatternStepKind::Pattern,
                            found,
                        },
                        std::panic::Location::caller(),
                    );
                };
                let checked = TyEnvT::new(binding_env.clone(), pattern)
                    .tyck_k(tycker, PatternAction::ana(domain.into()))?;
                let (binder, found_domain) = checked.try_as_value(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                Lub::lub_k(domain, found_domain, tycker)?;
                checked.close_scope_k(tycker, codomain)?;
                clause.env = checked.info + [(argument.definition, domain.into())];
                clause.pending.push(PendingArgument { binder, value: argument.value, ty: domain });
                Ok(clause)
            })
            .collect::<ResultKont<Vec<_>>>()?;
        let body =
            self.with_clauses(codomain, clauses, argument.body_env.clone()).elaborate_k(tycker)?;
        let abstraction = Alloc::alloc(
            tycker,
            ss::Abs(argument.binder, body),
            self.expected,
            &self.allocation_env,
        );
        self.record(tycker, abstraction);
        Ok(abstraction)
    }

    fn fresh_argument_k(
        &self, tycker: &mut Tycker<'_>, domain: ss::TypeId,
    ) -> ResultKont<FreshArgument> {
        let definition = Alloc::alloc(
            tycker,
            ss::VarName("$copattern_argument".to_string()),
            domain.into(),
            &(),
        );
        let binder = Alloc::alloc(tycker, definition, domain, &self.allocation_env);
        let body_env = self.allocation_env.clone() + [(definition, domain.into())];
        let value = Alloc::alloc(tycker, definition, domain, &body_env);
        let domain_view = domain.unroll_k(tycker)?.subst_env_k(tycker, &self.allocation_env)?;
        if let ss::Type::Data(data) = tycker.type_filled_k(&domain_view)?.to_owned() {
            tycker.statics.data_hints.insert_new(value, data);
        }
        Ok(FreshArgument { definition, binder, value, body_env })
    }

    fn elaborate_forall_k(
        mut self, tycker: &mut Tycker<'_>, source_binder: ss::TypeBinder, body: ss::TypeId,
    ) -> ResultKont<ss::CompuId> {
        if self.clauses.is_empty() {
            return tycker.err_k(
                TyckError::NonExhaustiveCopattern { expected: self.expected },
                std::panic::Location::caller(),
            );
        }
        let domain_kind = source_binder.domain_kind(tycker);
        let mut first_binder = None;
        let source_clauses = std::mem::take(&mut self.clauses);
        let clauses = source_clauses
            .into_iter()
            .map(|clause| -> ResultKont<_> {
                let found = clause.next_step();
                let (item, mut clause) = clause.pop();
                let su::CoPatternItem::Pat(pattern) = item else {
                    return tycker.err_k(
                        TyckError::CopatternStepMismatch {
                            expected: CopatternStepKind::Pattern,
                            found,
                        },
                        std::panic::Location::caller(),
                    );
                };
                let checked = TyEnvT::new(clause.env.clone(), pattern)
                    .tyck_k(tycker, PatternAction::ana(domain_kind.into()))?;
                let (binder, _) = checked.try_as_type(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                first_binder.get_or_insert(binder);
                clause.env =
                    self.extend_forall_env_k(tycker, &source_binder, binder, &clause.env)?;
                Ok(clause)
            })
            .collect::<ResultKont<Vec<_>>>()?;
        let binder = first_binder.expect("a nonempty forall copattern has a binder");
        let env = self.extend_forall_env_k(tycker, &source_binder, binder, &self.allocation_env)?;
        let body = self.with_clauses(body, clauses, env).elaborate_k(tycker)?;
        let abstraction =
            Alloc::alloc(tycker, ss::Abs(binder, body), self.expected, &self.allocation_env);
        self.record(tycker, abstraction);
        Ok(abstraction)
    }

    fn extend_forall_env_k(
        &self, tycker: &mut Tycker<'_>, source_binder: &ss::TypeBinder, binder: ss::TPatId,
        env: &ss::TyEnv,
    ) -> ResultKont<ss::TyEnv> {
        let payload_kind = source_binder.payload_kind(tycker);
        let witness = Alloc::alloc(tycker, source_binder.witness, payload_kind, env);
        let argument = source_binder.pattern.introduce_payload(tycker, witness);
        let argument = tycker.err_p_to_k(argument)?;
        Ok(TyEnvT::new(env.clone(), Assign(binder, argument)).tyck_k(tycker, ())?.info)
    }

    fn elaborate_pack_pi_k(
        mut self, tycker: &mut Tycker<'_>, signature: ss::PackPi,
    ) -> ResultKont<ss::CompuId> {
        if self.clauses.is_empty() {
            return tycker.err_k(
                TyckError::NonExhaustiveCopattern { expected: self.expected },
                std::panic::Location::caller(),
            );
        }
        if self.clauses.len() != 1 {
            return tycker
                .err_k(TyckError::MultiplePackPiCopatternClauses, std::panic::Location::caller());
        }
        let clause = self.clauses.pop().expect("one package-dependent copattern clause is present");
        let found = clause.next_step();
        let (item, mut clause) = clause.pop();
        let su::CoPatternItem::Pat(pattern) = item else {
            return tycker.err_k(
                TyckError::CopatternStepMismatch { expected: CopatternStepKind::Pattern, found },
                std::panic::Location::caller(),
            );
        };
        let skolems = TyEnvT::new(
            clause.env.clone(),
            PackPiPatternSkolems { pattern, signature: signature.clone().into() },
        )
        .tyck_k(tycker, ())?;
        let checked = TyEnvT::new(clause.env.clone(), pattern)
            .tyck_k(tycker, PatternAction::ana(signature.domain.into()).with_skolems(skolems))?;
        let (binder, found_domain) = checked.try_as_value(
            tycker,
            TyckError::SortMismatch,
            std::panic::Location::caller(),
        )?;
        Lub::lub_k(signature.domain, found_domain, tycker)?;
        let Some(witnesses) = checked.package_telescope_k(tycker)? else {
            return tycker.err_k(
                TyckError::PackageWitnessArityMismatch {
                    expected: signature.witnesses.len(),
                    found: 0,
                },
                std::panic::Location::caller(),
            );
        };
        if witnesses.len() != signature.witnesses.len() {
            return tycker.err_k(
                TyckError::PackageWitnessArityMismatch {
                    expected: signature.witnesses.len(),
                    found: witnesses.len(),
                },
                std::panic::Location::caller(),
            );
        }
        clause.env = checked.info;
        let env = clause.env.clone();
        let body = self.with_clauses(signature.codomain, vec![clause], env).elaborate_k(tycker)?;
        let abstraction =
            Alloc::alloc(tycker, ss::Abs(binder, body), self.expected, &self.allocation_env);
        tycker.statics.copattern_pack_pi_binders.insert_new(abstraction, binder);
        self.record(tycker, abstraction);
        Ok(abstraction)
    }

    fn record(&self, tycker: &mut Tycker<'_>, computation: ss::CompuId) {
        tycker.statics.terms.ensure(self.source, computation.into());
    }
}
