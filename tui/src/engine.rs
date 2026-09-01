use crate::{diagnostics::DiagnosticText, submission::ExpressionMode};
use std::{collections::HashSet, path::PathBuf};
use thiserror::Error;
use zydeco_dynamics::{
    BuiltinComputationRootLinker, BuiltinPackageError, ProgKont, RootLinker, Runtime,
    ValueRootLinker, fmt::Formatter as DynamicFormatter,
};
use zydeco_session::{
    AnalysisOutcome, CheckedProgram, CompilerSession, ProgramAnalysis, SourceLoadError,
};
use zydeco_statics::{
    TyckObservation,
    arena::StaticsArena,
    fmt::{self as static_fmt, Formatter as StaticFormatter},
    syntax::{Fillable, PackPi, TermAnnId, Type, TypeId},
};
use zydeco_surface::textual::SourceNumber;
use zydeco_syntax::{App, BuiltinRole, BuiltinTypeRole, Meta, Pretty, Ugly};
use zydeco_utils::arena::ArenaAccess;

pub(crate) enum EvaluationOutcome {
    Success(String),
    TypeRejected(String),
    Error(String),
}

pub(crate) struct InstalledInput {
    direct_wrapper: PathBuf,
    returned_wrapper: PathBuf,
}

pub(crate) struct ReplEngine {
    directory: PathBuf,
    builtin: PathBuf,
    session: CompilerSession,
}

impl ReplEngine {
    const INPUT_OBSERVATION: &'static str = "zydeco-tui-input";

    pub(crate) fn new(directory: PathBuf) -> Self {
        let builtin = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../lib/std/builtin.zy");
        Self { directory, builtin, session: CompilerSession::default() }
    }

    pub(crate) fn install(
        &mut self, number: SourceNumber, source: String,
    ) -> Result<InstalledInput, Box<SourceLoadError>> {
        let input = number.overlay_path(&self.directory);
        self.session.set_overlay(&input, source)?;

        let direct_root = self.directory.join(format!(".zydeco-repl-root-{}", number.get()));
        let returned_root =
            self.directory.join(format!(".zydeco-repl-value-root-{}", number.get()));
        let builtin = format!("{:?}", self.builtin.to_string_lossy());
        let direct_wrapper = format!(
            concat!(
                "param (/core = _) : @[import({})] _ in\n",
                "@[debug(\"{}\")] @[import({})] _\n",
            ),
            builtin,
            Self::INPUT_OBSERVATION,
            number.get(),
        );
        let returned_wrapper = format!(
            concat!(
                "param (/core = _) : @[import({})] _ in\n",
                "ret (@[debug(\"{}\")] @[import({})] _)\n",
            ),
            builtin,
            Self::INPUT_OBSERVATION,
            number.get(),
        );
        self.session.set_overlay(&direct_root, direct_wrapper)?;
        self.session.set_overlay(&returned_root, returned_wrapper)?;
        Ok(InstalledInput { direct_wrapper: direct_root, returned_wrapper: returned_root })
    }

    pub(crate) fn evaluate(
        &self, input: &InstalledInput, mode: ExpressionMode,
    ) -> EvaluationOutcome {
        let direct_analysis = match self.session.analyze(&input.direct_wrapper) {
            | Ok(analysis) => analysis,
            | Err(error) => {
                return EvaluationOutcome::Error(DiagnosticText::analysis_error(&error));
            }
        };
        let analysis = if matches!(direct_analysis.outcome(), AnalysisOutcome::Checked { .. }) {
            direct_analysis
        } else {
            let returned_analysis = match self.session.analyze(&input.returned_wrapper) {
                | Ok(analysis) => analysis,
                | Err(error) => {
                    return EvaluationOutcome::Error(DiagnosticText::analysis_error(&error));
                }
            };
            if matches!(returned_analysis.outcome(), AnalysisOutcome::Rejected { .. }) {
                return EvaluationOutcome::TypeRejected(DiagnosticText::rejected(&direct_analysis));
            }
            returned_analysis
        };

        let program = self
            .session
            .checked_program(&analysis)
            .expect("a checked analysis has an owned program");
        let root = Self::input_root(&analysis);
        let observations = Self::observations(&analysis, &program);
        let result = match mode {
            | ExpressionMode::Type => Ok(Self::inspect(&program, root)),
            | ExpressionMode::Evaluate => Self::evaluate_default(program, root),
            | ExpressionMode::Run => Self::run_checked(program, root, true),
        };
        match result {
            | Ok(result) => {
                EvaluationOutcome::Success(Self::with_observations(observations, result))
            }
            | Err(error) => EvaluationOutcome::Error(Self::with_observations(observations, error)),
        }
    }

    fn evaluate_default(program: CheckedProgram, root: TermAnnId) -> Result<String, String> {
        match root {
            | root @ (TermAnnId::Kind(_) | TermAnnId::Type(_, _)) => {
                Ok(Self::inspect(&program, root))
            }
            | TermAnnId::Value(_, _) => Self::run_checked(program, root, false),
            | TermAnnId::Compu(_, _) => Self::run_checked(program, root, false),
            | TermAnnId::Hole(_) => unreachable!("a checked source root cannot remain a hole"),
        }
    }

    fn run_checked(
        program: CheckedProgram, root: TermAnnId, forced: bool,
    ) -> Result<String, String> {
        let classifier = match root {
            | TermAnnId::Value(_, ty) => Some(Self::pretty_in(&program, ty)),
            | TermAnnId::Compu(_, ty) => {
                match Self::evaluation_plan(&program.statics, ty, forced) {
                    | ComputationEvaluationPlan::Return(payload) => {
                        Some(Self::pretty_in(&program, payload))
                    }
                    | ComputationEvaluationPlan::Executable => None,
                    | ComputationEvaluationPlan::HostRequired
                    | ComputationEvaluationPlan::Unsupported
                        if forced =>
                    {
                        return Err(format!(
                            "cannot run a computation of type {} without an argument or host contract",
                            Self::pretty_in(&program, ty),
                        ));
                    }
                    | ComputationEvaluationPlan::HostRequired
                    | ComputationEvaluationPlan::Unsupported => {
                        return Err("this computation is well typed but is not directly runnable"
                            .to_owned());
                    }
                }
            }
            | root @ (TermAnnId::Kind(_) | TermAnnId::Type(_, _)) => {
                return Err(format!(
                    "cannot run {}; use `@[type] expression` to inspect it",
                    Self::inspect(&program, root),
                ));
            }
            | TermAnnId::Hole(_) => unreachable!("a checked source root cannot remain a hole"),
        };
        let dynamics = Self::link_checked(program).map_err(|error| error.to_string())?;
        Self::run_dynamics(dynamics, classifier)
    }

    fn link_checked(
        program: CheckedProgram,
    ) -> Result<zydeco_dynamics::syntax::DynamicsProgram, ReplLinkError> {
        match program.root {
            | TermAnnId::Value(root, _) => {
                Ok(ValueRootLinker { scoped: program.scoped, statics: program.statics, root }.run())
            }
            | TermAnnId::Compu(root, ty) => match Self::computation_plan(&program.statics, ty) {
                | ComputationPlan::Return(_) | ComputationPlan::Executable => {
                    Ok(RootLinker { scoped: program.scoped, statics: program.statics, root }.run())
                }
                | ComputationPlan::Builtin(signature) => BuiltinComputationRootLinker {
                    scoped: program.scoped,
                    statics: program.statics,
                    root,
                    signature,
                }
                .run()
                .map_err(ReplLinkError::from),
                | ComputationPlan::Unsupported => Err(ReplLinkError::UnsupportedRoot),
            },
            | TermAnnId::Kind(_) | TermAnnId::Type(_, _) | TermAnnId::Hole(_) => {
                Err(ReplLinkError::NonDynamicRoot)
            }
        }
    }

    fn run_dynamics(
        dynamics: zydeco_dynamics::syntax::DynamicsProgram, classifier: Option<String>,
    ) -> Result<String, String> {
        let mut input = std::io::empty();
        let mut output = Vec::new();
        let arguments: [String; 0] = [];
        let mut runtime = Runtime::new(&mut input, &mut output, &arguments, dynamics);
        let result = runtime.run();
        let formatter = DynamicFormatter::new(&runtime.program);
        let result = match result {
            | ProgKont::Ret(value) => {
                let value = value.ugly(&formatter);
                classifier.map_or(value.clone(), |classifier| format!("{value} : {classifier}"))
            }
            | ProgKont::ExitCode(code) => format!("Program exited with code {code}"),
            | ProgKont::Dry => unreachable!("the REPL never asks the runtime for a dry run"),
        };
        let output = String::from_utf8_lossy(&output);
        if output.is_empty() { Ok(result) } else { Ok(format!("{}\n{result}", output.trim_end())) }
    }

    fn computation_plan(statics: &StaticsArena, ty: TypeId) -> ComputationPlan {
        match Self::type_view(statics, ty) {
            | Some(Type::App(App(constructor, payload)))
                if matches!(Self::type_view(statics, *constructor), Some(Type::Ret(_))) =>
            {
                ComputationPlan::Return(*payload)
            }
            | Some(Type::PackPi(signature)) => ComputationPlan::Builtin(signature.as_ref().clone()),
            | Some(Type::Abst(witness))
                if statics.builtin_roles.witness(*witness)
                    == Some(BuiltinRole::Type(BuiltinTypeRole::OS)) =>
            {
                ComputationPlan::Executable
            }
            | _ => ComputationPlan::Unsupported,
        }
    }

    fn evaluation_plan(
        statics: &StaticsArena, ty: TypeId, allow_host: bool,
    ) -> ComputationEvaluationPlan {
        let mut current = ty;
        let mut visited = HashSet::new();
        loop {
            if !visited.insert(current) {
                return ComputationEvaluationPlan::Unsupported;
            }
            match Self::computation_plan(statics, current) {
                | ComputationPlan::Return(payload) => {
                    return ComputationEvaluationPlan::Return(payload);
                }
                | ComputationPlan::Executable => {
                    return ComputationEvaluationPlan::Executable;
                }
                | ComputationPlan::Builtin(signature) if allow_host => {
                    current = signature.codomain;
                }
                | ComputationPlan::Builtin(_) => {
                    return ComputationEvaluationPlan::HostRequired;
                }
                | ComputationPlan::Unsupported => {
                    return ComputationEvaluationPlan::Unsupported;
                }
            }
        }
    }

    fn type_view(statics: &StaticsArena, ty: TypeId) -> Option<&Type> {
        if let Some(normalized) = statics.types_normalized.get(&ty) {
            return Some(normalized);
        }
        match statics.types_pre.get(&ty)? {
            | Fillable::Done(ty) => Some(ty),
            | Fillable::Fill(_) => None,
        }
    }

    fn inspect(program: &CheckedProgram, root: TermAnnId) -> String {
        let formatter = StaticFormatter::new(&program.scoped, &program.statics);
        Self::inspect_with(&formatter, root)
    }

    fn input_root(analysis: &ProgramAnalysis) -> TermAnnId {
        analysis
            .observations()
            .iter()
            .rev()
            .find_map(|observation| match observation {
                | TyckObservation::Debug { metadata, result }
                    if metadata.arguments()
                        == [Meta::string(Self::INPUT_OBSERVATION)].as_slice() =>
                {
                    Some(*result)
                }
                | _ => None,
            })
            .expect("the checked REPL wrapper retains its input observation")
    }

    fn inspect_with(formatter: &StaticFormatter<'_>, root: TermAnnId) -> String {
        match root {
            | TermAnnId::Kind(kind) => format!("{} : Set", Self::pretty(formatter, kind)),
            | TermAnnId::Type(ty, kind) => {
                format!("{} : {}", Self::pretty(formatter, ty), Self::pretty(formatter, kind))
            }
            | TermAnnId::Value(value, ty) => {
                format!("{} : {}", Self::pretty(formatter, value), Self::pretty(formatter, ty),)
            }
            | TermAnnId::Compu(computation, ty) => format!(
                "{} : {}",
                Self::pretty(formatter, computation),
                Self::pretty(formatter, ty),
            ),
            | TermAnnId::Hole(fill) => format!("unresolved hole {}", fill.concise()),
        }
    }

    fn observations(analysis: &ProgramAnalysis, program: &CheckedProgram) -> Vec<String> {
        let formatter = StaticFormatter::new(&program.scoped, &program.statics);
        analysis
            .observations()
            .iter()
            .filter_map(|observation| match observation {
                | TyckObservation::Debug { metadata, .. }
                    if metadata.arguments()
                        == [Meta::string(Self::INPUT_OBSERVATION)].as_slice() =>
                {
                    None
                }
                | TyckObservation::Debug { metadata, result } => {
                    Some(format!("@[{}] {}", metadata, Self::inspect_with(&formatter, *result),))
                }
                | TyckObservation::HoleSolution { .. } => None,
            })
            .collect()
    }

    fn with_observations(observations: Vec<String>, result: String) -> String {
        observations.into_iter().chain(std::iter::once(result)).collect::<Vec<_>>().join("\n")
    }

    fn pretty_in<T>(program: &CheckedProgram, item: T) -> String
    where
        T: for<'format> Pretty<'format, static_fmt::Formatter<'format>>,
    {
        Self::pretty(&StaticFormatter::new(&program.scoped, &program.statics), item)
    }

    fn pretty<T>(formatter: &StaticFormatter<'_>, item: T) -> String
    where
        T: for<'format> Pretty<'format, static_fmt::Formatter<'format>>,
    {
        let mut output = String::new();
        item.pretty(formatter).render_fmt(100, &mut output).unwrap();
        output
    }
}

enum ComputationPlan {
    Return(TypeId),
    Builtin(PackPi),
    Executable,
    Unsupported,
}

enum ComputationEvaluationPlan {
    Return(TypeId),
    Executable,
    HostRequired,
    Unsupported,
}

#[derive(Debug, Error)]
enum ReplLinkError {
    #[error(transparent)]
    Builtin(#[from] BuiltinPackageError),
    #[error("checked computation root has no runtime entry contract")]
    UnsupportedRoot,
    #[error("checked source root is not dynamically evaluable")]
    NonDynamicRoot,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_numbered_input_can_import_an_earlier_expression() {
        let directory = tempfile::tempdir().unwrap();
        let mut engine = ReplEngine::new(directory.path().to_path_buf());
        let first = SourceNumber::new(1).unwrap();
        let second = SourceNumber::new(2).unwrap();
        let third = SourceNumber::new(3).unwrap();
        let first_path = engine.install(first, "1".to_owned()).unwrap();
        match engine.evaluate(&first_path, ExpressionMode::Evaluate) {
            | EvaluationOutcome::Success(result) => assert_eq!(result, "1 : Int64"),
            | EvaluationOutcome::TypeRejected(error) | EvaluationOutcome::Error(error) => {
                panic!("first input failed: {error}")
            }
        }

        let second_path = engine.install(second, "@[import(1)] _".to_owned()).unwrap();
        match engine.evaluate(&second_path, ExpressionMode::Evaluate) {
            | EvaluationOutcome::Success(result) => assert_eq!(result, "1 : Int64"),
            | EvaluationOutcome::TypeRejected(error) | EvaluationOutcome::Error(error) => {
                panic!("numbered import failed: {error}")
            }
        }

        let third_path = engine.install(third, "ret (@[import(1)] _)".to_owned()).unwrap();
        match engine.evaluate(&third_path, ExpressionMode::Evaluate) {
            | EvaluationOutcome::Success(result) => assert_eq!(result, "1 : Int64"),
            | EvaluationOutcome::TypeRejected(error) | EvaluationOutcome::Error(error) => {
                panic!("nested numbered import failed: {error}")
            }
        }
    }

    #[test]
    fn type_metadata_checks_without_running() {
        let directory = tempfile::tempdir().unwrap();
        let mut engine = ReplEngine::new(directory.path().to_path_buf());
        let path =
            engine.install(SourceNumber::new(1).unwrap(), "@[type] ret 1".to_owned()).unwrap();

        match engine.evaluate(&path, ExpressionMode::Type) {
            | EvaluationOutcome::Success(result) => {
                assert!(result.contains("Ret Int64"), "{result}")
            }
            | EvaluationOutcome::TypeRejected(error) | EvaluationOutcome::Error(error) => {
                panic!("type command failed: {error}")
            }
        }
    }

    #[test]
    fn returned_values_are_classified_by_the_return_payload() {
        let directory = tempfile::tempdir().unwrap();
        let mut engine = ReplEngine::new(directory.path().to_path_buf());
        let path = engine.install(SourceNumber::new(1).unwrap(), "ret 1".to_owned()).unwrap();

        match engine.evaluate(&path, ExpressionMode::Evaluate) {
            | EvaluationOutcome::Success(result) => assert_eq!(result, "1 : Int64"),
            | EvaluationOutcome::TypeRejected(error) | EvaluationOutcome::Error(error) => {
                panic!("return evaluation failed: {error}")
            }
        }
    }

    #[test]
    fn type_check_failures_have_a_retryable_outcome() {
        let directory = tempfile::tempdir().unwrap();
        let mut engine = ReplEngine::new(directory.path().to_path_buf());
        let path = engine.install(SourceNumber::new(1).unwrap(), "1 2".to_owned()).unwrap();

        match engine.evaluate(&path, ExpressionMode::Evaluate) {
            | EvaluationOutcome::TypeRejected(error) => {
                assert!(!error.is_empty(), "type rejection should include a diagnostic")
            }
            | EvaluationOutcome::Success(result) => {
                panic!("ill-typed input unexpectedly evaluated: {result}")
            }
            | EvaluationOutcome::Error(error) => {
                panic!("ill-typed input was not classified as retryable: {error}")
            }
        }
    }

    #[test]
    fn a_missing_number_is_diagnosed_as_an_input_identity() {
        let directory = tempfile::tempdir().unwrap();
        let mut engine = ReplEngine::new(directory.path().to_path_buf());
        let path =
            engine.install(SourceNumber::new(1).unwrap(), "@[import(2)] _".to_owned()).unwrap();

        match engine.evaluate(&path, ExpressionMode::Evaluate) {
            | EvaluationOutcome::Error(error) => {
                assert!(error.contains("REPL input [2]"), "{error}")
            }
            | EvaluationOutcome::TypeRejected(error) => {
                panic!("missing input unexpectedly reached type checking: {error}")
            }
            | EvaluationOutcome::Success(result) => {
                panic!("missing numbered input unexpectedly evaluated: {result}")
            }
        }
    }

    #[test]
    fn run_mode_supplies_builtin_to_a_declaration_free_program() {
        let directory = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../lib/tests/builtin");
        let mut engine = ReplEngine::new(directory);
        let path = engine
            .install(
                SourceNumber::new(1).unwrap(),
                include_str!("../../lib/tests/builtin/exit.zy").to_owned(),
            )
            .unwrap();

        match engine.evaluate(&path, ExpressionMode::Run) {
            | EvaluationOutcome::Success(result) => {
                assert_eq!(result, "Program exited with code 3")
            }
            | EvaluationOutcome::TypeRejected(error) | EvaluationOutcome::Error(error) => {
                panic!("declaration-free program failed: {error}")
            }
        }
    }
}
