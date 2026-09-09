use super::*;
use crate::low::{SpsLowError, SpsLowProgram};

#[derive(Default)]
struct Fixture {
    arena: SpsLowArena,
}

impl Fixture {
    fn build<U, S, T>(&mut self, value: U) -> T
    where
        U: Construct<S, T, SpsLowArena>,
    {
        value.build(&mut self.arena, None)
    }

    fn def(&mut self) -> DefId {
        self.arena.admin.fresh_def()
    }

    fn terminal(&mut self) -> CompuId {
        let stack = self.build(Bullet);
        self.build(SHole(stack))
    }

    fn environment(&mut self, arity: usize) -> ValueId {
        if arity == 0 {
            return self.build(Triv);
        }
        let items = (0..arity).map(|_| self.build(Triv)).collect();
        self.build(VCons::new(items, ProductLayout { arity }))
    }

    fn block(&mut self, kind: EntryKind, arity: usize) -> ValueId {
        let label = self.def();
        let environment = if arity == 0 {
            self.build(Triv)
        } else {
            let items = (0..arity).map(|_| self.build(Hole)).collect();
            self.build(VCons::new(items, ProductLayout { arity }))
        };
        let entry = match kind {
            | EntryKind::Closure => EntryParameters::Closure { environment },
            | EntryKind::Continuation => {
                EntryParameters::Continuation { result: self.build(Hole), environment }
            }
        };
        let body = self.terminal();
        self.build(Block { label, entry, body })
    }

    fn closure(&mut self) -> ValueId {
        let code = self.block(EntryKind::Closure, 0);
        let environment = self.build(Triv);
        self.build(ClosurePackage { environment, code })
    }

    fn continuation(&mut self) -> StackId {
        let code = self.block(EntryKind::Continuation, 0);
        let environment: ValueId = self.build(Triv);
        let stack = self.build(Bullet);
        let residual = self.build(Cons(environment, stack));
        self.build(ContinuationPackage { code, residual })
    }

    fn jump(&mut self, target: ValueId, argument: EntryArgument) -> CompuId {
        let stack = self.build(Bullet);
        self.build(Jump { target, argument, stack })
    }

    fn check(self, root: CompuId) -> Result<SpsLowProgram, SpsLowError> {
        SpsLowProgram::try_new(self.arena, root)
    }

    fn rejects(self, root: CompuId, error: EntryContractError) {
        assert_eq!(self.check(root).unwrap_err(), SpsLowError::EntryContract(error));
    }
}

#[test]
fn direct_entries_require_the_matching_transfer_kind() {
    for kind in [EntryKind::Closure, EntryKind::Continuation] {
        let mut f = Fixture::default();
        let target = f.block(kind, 0);
        let environment = f.build(Triv);
        let stack = f.build(Bullet);
        let (argument, stack) = match kind {
            | EntryKind::Closure => (EntryArgument::Closure { environment }, stack),
            | EntryKind::Continuation => {
                let result = f.build(Triv);
                (EntryArgument::Continuation { result }, f.build(Cons(environment, stack)))
            }
        };
        let root = f.build(Jump { target, argument, stack });
        let program = f.check(root).unwrap();
        let (mut arena, root) = program.into_parts();
        let Computation::Jump(jump) = &mut arena.inner.compus[&root] else { unreachable!() };
        jump.argument = match argument {
            | EntryArgument::Closure { environment } => {
                EntryArgument::Continuation { result: environment }
            }
            | EntryArgument::Continuation { result } => {
                EntryArgument::Closure { environment: result }
            }
        };
        let expected = jump.argument.kind();
        Fixture { arena }.rejects(
            root,
            EntryContractError::KindMismatch { site: EntrySite::Jump(root), expected, found: kind },
        );
    }
}

#[test]
fn an_ordinary_value_is_not_bare_code() {
    let mut f = Fixture::default();
    let target = f.build(Triv);
    let environment = f.build(Triv);
    let root = f.jump(target, EntryArgument::Closure { environment });
    f.rejects(root, EntryContractError::UnknownCode { code: target });
}

#[test]
fn closure_packages_check_entry_kind_and_known_environment_arity() {
    for (kind, arity) in
        [(EntryKind::Closure, 1), (EntryKind::Closure, 2), (EntryKind::Continuation, 1)]
    {
        let mut f = Fixture::default();
        let code = f.block(kind, 1);
        let environment = f.environment(arity);
        let package = f.build(ClosurePackage { environment, code });
        let binder = f.build(Hole);
        let tail = f.terminal();
        let root = f.build(LetValue { binder, bindee: package, tail });
        match (kind, arity) {
            | (EntryKind::Closure, 1) => {
                f.check(root).unwrap();
            }
            | (EntryKind::Closure, found) => f.rejects(
                root,
                EntryContractError::EnvironmentLayout {
                    site: EntrySite::Closure(package),
                    expected: 1,
                    found,
                },
            ),
            | (found, _) => f.rejects(
                root,
                EntryContractError::KindMismatch {
                    site: EntrySite::Closure(package),
                    expected: EntryKind::Closure,
                    found,
                },
            ),
        }
    }
}

#[test]
fn continuation_packages_require_their_environment_word() {
    let mut f = Fixture::default();
    let code = f.block(EntryKind::Continuation, 0);
    let residual = f.build(Bullet);
    let stack = f.build(ContinuationPackage { code, residual });
    let root = f.build(SHole(stack));
    f.rejects(
        root,
        EntryContractError::MissingEnvironment {
            site: EntrySite::Continuation(stack),
            stack: residual,
        },
    );
}

#[test]
fn continuation_packages_check_entry_kind_and_known_environment_arity() {
    for (kind, arity) in
        [(EntryKind::Continuation, 1), (EntryKind::Continuation, 2), (EntryKind::Closure, 1)]
    {
        let mut f = Fixture::default();
        let code = f.block(kind, 1);
        let environment = f.environment(arity);
        let ambient = f.build(Bullet);
        let residual = f.build(Cons(environment, ambient));
        let package = f.build(ContinuationPackage { code, residual });
        let root = f.build(SHole(package));
        match (kind, arity) {
            | (EntryKind::Continuation, 1) => {
                f.check(root).unwrap();
            }
            | (EntryKind::Continuation, found) => f.rejects(
                root,
                EntryContractError::EnvironmentLayout {
                    site: EntrySite::Continuation(package),
                    expected: 1,
                    found,
                },
            ),
            | (found, _) => f.rejects(
                root,
                EntryContractError::KindMismatch {
                    site: EntrySite::Continuation(package),
                    expected: EntryKind::Continuation,
                    found,
                },
            ),
        }
    }
}

#[test]
fn direct_jumps_check_the_known_environment_layout() {
    for kind in [EntryKind::Closure, EntryKind::Continuation] {
        for arity in [1, 2] {
            let mut f = Fixture::default();
            let target = f.block(kind, 1);
            let environment = f.environment(arity);
            let stack = f.build(Bullet);
            let (argument, stack) = match kind {
                | EntryKind::Closure => (EntryArgument::Closure { environment }, stack),
                | EntryKind::Continuation => {
                    let result = f.build(Triv);
                    (EntryArgument::Continuation { result }, f.build(Cons(environment, stack)))
                }
            };
            let root = f.build(Jump { target, argument, stack });
            if arity == 1 {
                f.check(root).unwrap();
            } else {
                f.rejects(
                    root,
                    EntryContractError::EnvironmentLayout {
                        site: EntrySite::Jump(root),
                        expected: 1,
                        found: arity,
                    },
                );
            }
        }
    }
}

#[test]
fn indirect_closure_calls_keep_code_and_environment_paired_through_aliases() {
    for crossed in [false, true] {
        let mut f = Fixture::default();
        let [env1, code1, env2, code2, alias_env, alias_code] = std::array::from_fn(|_| f.def());
        let target = f.build(alias_code);
        let environment = f.build(alias_env);
        let jump = f.jump(target, EntryArgument::Closure { environment });
        let fields = vec![f.build(code1), f.build(if crossed { env2 } else { env1 })];
        let bindee = f.build(VCons::new(fields, ProductLayout { arity: 2 }));
        let patterns = vec![f.build(alias_code), f.build(alias_env)];
        let binder = f.build(VCons::new(patterns, ProductLayout { arity: 2 }));
        let ignored = f.build(Hole);
        let binder = f.build(Alias(ConsN(vec![binder], ignored)));
        let body = f.build(LetValue { binder, bindee, tail: jump });
        let package = f.closure();
        let environment = f.build(env2);
        let code = f.build(code2);
        let body = f.build(OpenClosure { package, environment, code, body });
        let package = f.closure();
        let environment = f.build(env1);
        let code = f.build(code1);
        let root = f.build(OpenClosure { package, environment, code, body });
        if crossed {
            f.rejects(
                root,
                EntryContractError::ClosureEnvironment {
                    site: EntrySite::Jump(jump),
                    opening: root,
                },
            );
        } else {
            f.check(root).unwrap();
        }
    }
}

#[test]
fn continuation_calls_keep_the_code_and_restored_stack_from_one_opening() {
    for crossed in [false, true] {
        let mut f = Fixture::default();
        let first = f.def();
        let second = f.def();
        let target = f.build(if crossed { first } else { second });
        let result = f.build(Triv);
        let jump = f.jump(target, EntryArgument::Continuation { result });
        let package = f.continuation();
        let code = f.build(second);
        let body = f.build(OpenContinuation { package, code, body: jump });
        let package = f.continuation();
        let code = f.build(first);
        let root = f.build(OpenContinuation { package, code, body });
        if crossed {
            f.rejects(
                root,
                EntryContractError::ContinuationResidual {
                    site: EntrySite::Jump(jump),
                    opening: root,
                },
            );
        } else {
            f.check(root).unwrap();
        }
    }
}

#[test]
fn continuation_transfers_preserve_the_restored_stack() {
    #[derive(Clone, Copy)]
    enum Change {
        Keep,
        Push,
        Pop,
        PushPop,
    }

    for repack in [false, true] {
        for change in [Change::Keep, Change::Push, Change::Pop, Change::PushPop] {
            let mut f = Fixture::default();
            let original = f.def();
            let residual = f.build(Bullet);
            let residual = if matches!(change, Change::Push) {
                let extra: ValueId = f.build(Triv);
                f.build(Cons(extra, residual))
            } else {
                residual
            };
            let code = f.build(original);
            let (body, site) = if repack {
                let package = f.build(ContinuationPackage { code, residual });
                (f.build(SHole(package)), EntrySite::Continuation(package))
            } else {
                let result = f.build(Triv);
                let jump = f.build(Jump {
                    target: code,
                    argument: EntryArgument::Continuation { result },
                    stack: residual,
                });
                (jump, EntrySite::Jump(jump))
            };
            let body = if matches!(change, Change::Pop | Change::PushPop) {
                let pattern = f.build(Hole);
                let bindee = f.build(Bullet);
                let bindee = if matches!(change, Change::PushPop) {
                    let extra: ValueId = f.build(Triv);
                    f.build(Cons(extra, bindee))
                } else {
                    bindee
                };
                f.build(LetArg { binder: Cons(pattern, Bullet), bindee, tail: body })
            } else {
                body
            };
            let package = f.continuation();
            let code = f.build(original);
            let root = f.build(OpenContinuation { package, code, body });
            if matches!(change, Change::Keep | Change::PushPop) {
                f.check(root).unwrap();
            } else {
                f.rejects(root, EntryContractError::ContinuationResidual { site, opening: root });
            }
        }
    }
}
