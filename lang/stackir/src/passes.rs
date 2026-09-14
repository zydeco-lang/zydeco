//! Built-in high-SPS plans.
//!
//! See `docs/references/compiler.md#built-in-plans-and-phase-boundaries`.

use crate::{
    BranchJoinError, BranchJoinProgram,
    high::{fmt::Pretty, normalize::Normalizer, syntax::DefId},
};
use std::{
    cell::RefCell,
    convert::Infallible,
    fmt,
    io::{self, Write},
    rc::Rc,
    str::FromStr,
    time::Duration,
};
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::scoped::arena::ScopedArena;
use zydeco_utils::pass::{
    CompilerPass, PassFailure, PassInvocation, PassLocation, PassObserver, PassSequence,
};

/// A selectable high-SPS transformation.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum HighSpsPass {
    Normalize,
}

impl HighSpsPass {
    pub const ALL: &'static [Self] = &[Self::Normalize];

    pub fn name(self) -> &'static str {
        match self {
            | Self::Normalize => "normalize",
        }
    }

    pub fn description(self) -> &'static str {
        match self {
            | Self::Normalize => "sharing-preserving local reductions and demand-driven rebuilding",
        }
    }

    fn location(self, index: usize) -> PassLocation {
        PassLocation { path: vec![index], name: self.name().into() }
    }
}

impl fmt::Display for HighSpsPass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

/// An owned selection of built-in high-SPS passes.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub enum HighSpsPlan {
    #[default]
    Default,
    None,
    Custom(Vec<HighSpsPass>),
}

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum HighSpsPlanError {
    #[error("empty high-SPS pass selection; use `none` to disable optional passes")]
    Empty,
    #[error("empty high-SPS pass at position {position}")]
    EmptyPass { position: usize },
    #[error("unknown high-SPS pass `{name}` at position {position}; available pass: normalize")]
    UnknownPass { position: usize, name: String },
    #[error(
        "`{name}` selects a complete plan and cannot occur at position {position} in a pass list"
    )]
    NestedPreset { position: usize, name: String },
}

impl FromStr for HighSpsPlan {
    type Err = HighSpsPlanError;

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        match text.trim() {
            | "" => Err(HighSpsPlanError::Empty),
            | "default" => Ok(Self::Default),
            | "none" => Ok(Self::None),
            | text => text
                .split(',')
                .enumerate()
                .map(|(index, token)| {
                    let position = index + 1;
                    match token.trim() {
                        | "normalize" => Ok(HighSpsPass::Normalize),
                        | "" => Err(HighSpsPlanError::EmptyPass { position }),
                        | name @ ("default" | "none") => {
                            Err(HighSpsPlanError::NestedPreset { position, name: name.into() })
                        }
                        | name => {
                            Err(HighSpsPlanError::UnknownPass { position, name: name.into() })
                        }
                    }
                })
                .collect::<Result<Vec<_>, _>>()
                .map(Self::Custom),
        }
    }
}

impl fmt::Display for HighSpsPlan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::Default => f.write_str("default"),
            | Self::None => f.write_str("none"),
            | Self::Custom(passes) if passes.is_empty() => f.write_str("none"),
            | Self::Custom(passes) => {
                for (index, pass) in passes.iter().enumerate() {
                    if index > 0 {
                        f.write_str(",")?;
                    }
                    write!(f, "{pass}")?;
                }
                Ok(())
            }
        }
    }
}

impl HighSpsPlan {
    pub fn passes(&self) -> &[HighSpsPass] {
        match self {
            | Self::Default => &[HighSpsPass::Normalize],
            | Self::None => &[],
            | Self::Custom(passes) => passes,
        }
    }

    /// Explain the expanded selection together with its required phase boundaries.
    pub fn explain(&self) -> String {
        let stages = self
            .passes()
            .iter()
            .enumerate()
            .map(|(index, pass)| format!("  {}\n", pass.location(index)))
            .collect::<String>();
        format!(
            "high-SPS selection: {self}\n  check closed root (required)\n{stages}  check closed root (required)\n  closure conversion (required)\n"
        )
    }

    /// Instantiate independent occurrences, preserving the selected order.
    pub fn instantiate(&self) -> PassSequence<'static, BranchJoinProgram, Infallible> {
        self.passes().iter().fold(PassSequence::new(), |passes, pass| match pass {
            | HighSpsPass::Normalize => passes.with_pass(Normalizer),
        })
    }

    /// Share a borrowed observer among this plan's distinct pass occurrences.
    pub fn instantiate_observed<'a, O>(
        &self, observer: &'a mut O,
    ) -> PassSequence<'a, BranchJoinProgram, PassFailure<Infallible, O::Error>>
    where
        O: PassObserver<BranchJoinProgram, BranchJoinProgram, Infallible>,
    {
        let observer = Rc::new(RefCell::new(observer));
        self.passes().iter().enumerate().fold(PassSequence::new(), |passes, (index, pass)| {
            match pass {
                | HighSpsPass::Normalize => passes
                    .with_pass(Normalizer.with_observer(pass.location(index), observer.clone())),
            }
        })
    }
}

/// High-SPS trace, verification, and dump options.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct HighSpsInspection {
    pub trace: bool,
    pub verify: bool,
    pub dump: bool,
}

impl HighSpsInspection {
    pub fn enabled(self) -> bool {
        self.trace || self.verify || self.dump
    }
}

#[derive(Debug, thiserror::Error)]
pub enum HighSpsObservationError {
    #[error(transparent)]
    Invalid(#[from] zydeco_surface::diagnostic::Diagnostics<HighSpsInvariant>),
    #[error("could not write high-SPS inspection output: {0}")]
    Output(#[from] io::Error),
}

#[derive(Debug, thiserror::Error)]
pub enum HighSpsInvariant {
    #[error("high-SPS invariant failed: {0}")]
    BranchJoin(BranchJoinError),
    #[error("high-SPS invariant failed: root has free definitions {0:?}")]
    FreeDefinitions(Vec<DefId>),
}

pub type HighSpsFailure = PassFailure<Infallible, HighSpsObservationError>;

/// Render and verify borrowed IR using the current compilation's naming arenas.
pub struct HighSpsObserver<'a, W> {
    pub scoped: &'a ScopedArena,
    pub statics: &'a StaticsArena,
    pub inspection: HighSpsInspection,
    pub output: W,
}

impl<W: Write> HighSpsObserver<'_, W> {
    fn inspect(&mut self, program: &BranchJoinProgram) -> Result<(), HighSpsObservationError> {
        if self.inspection.verify {
            use crate::high::{
                check::BranchJoinValidator,
                traverse::{Together, Traversal},
                variables::Variables,
            };
            let program = program.as_program();
            let mut analyses =
                Together { first: BranchJoinValidator::default(), second: Variables::default() };
            Traversal { arena: &program.arena().inner }.run(program.root().into(), &mut analyses);
            let errors = analyses
                .first
                .errors()
                .iter()
                .cloned()
                .map(HighSpsInvariant::BranchJoin)
                .chain(
                    analyses
                        .second
                        .free_variables(program.root().into())
                        .filter(|free| !free.is_empty())
                        .map(|free| {
                            HighSpsInvariant::FreeDefinitions(free.iter().copied().collect())
                        }),
                )
                .collect();
            if let Some(errors) = zydeco_surface::diagnostic::Diagnostics::with_errors(errors) {
                return Err(errors.into());
            }
        }
        if self.inspection.dump {
            let program = program.as_program();
            let arena = program.arena();
            let formatter = crate::high::fmt::Formatter::new(
                &arena.admin,
                &arena.inner,
                self.scoped,
                self.statics,
            );
            program.pretty(&formatter).render(100, &mut self.output)?;
            writeln!(self.output)?;
        }
        Ok(())
    }
}

impl<W: Write> PassObserver<BranchJoinProgram, BranchJoinProgram, Infallible>
    for HighSpsObserver<'_, W>
{
    type Error = HighSpsObservationError;

    fn before(
        &mut self, invocation: &PassInvocation, input: &BranchJoinProgram,
    ) -> Result<(), Self::Error> {
        if self.inspection.trace || self.inspection.dump {
            writeln!(self.output, "before high-SPS {invocation}")?;
        }
        self.inspect(input)
    }

    fn after(
        &mut self, invocation: &PassInvocation, output: &BranchJoinProgram, elapsed: Duration,
    ) -> Result<(), Self::Error> {
        if self.inspection.trace || self.inspection.dump {
            writeln!(
                self.output,
                "after high-SPS {invocation}: {:.3} ms",
                elapsed.as_secs_f64() * 1000.0
            )?;
        }
        self.inspect(output)
    }
}

#[cfg(test)]
mod tests;
