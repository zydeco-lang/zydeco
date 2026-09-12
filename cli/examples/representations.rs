//! Compare static allocation sites for the local representation policies.
//! These counts describe generated code, not executed allocations or peak memory.

use std::path::PathBuf;
use zydeco_assembly::{
    LoweringPipeline,
    representation::{RepresentationPolicy, RepresentationStrategy, Shared, UnboxingOpportunity},
    syntax::{AssemblyProgram, Instruction, Program},
    unbox::LocalUnboxing,
};
use zydeco_cli::{BackendProgram, CommandCompiler};
use zydeco_utils::pass::CompilerPass;

/// A type-level policy experiment that needs no change to the analysis or emitters.
struct FieldLimit<P, const MAX: usize>(P);

impl<P: RepresentationPolicy, const MAX: usize> RepresentationPolicy for FieldLimit<P, MAX> {
    fn unbox(&self, opportunity: UnboxingOpportunity) -> bool {
        opportunity.fields <= MAX && self.0.unbox(opportunity)
    }
}

#[derive(Default)]
struct Measurement {
    cells: usize,
    payload_words: usize,
}

impl Measurement {
    fn collect(program: &AssemblyProgram) -> Self {
        program.arena().programs.iter().fold(Self::default(), |mut measurement, (_, program)| {
            if let Program::Instruction(Instruction::PackProduct(pack), _) = program {
                measurement.cells += 1;
                measurement.payload_words += pack.0.arity;
            }
            measurement
        })
    }

    fn print(
        &self, source: &std::path::Path, policy: &str, unboxing: &LocalUnboxing,
        program: &zydeco_stackir::SpsLowProgram,
    ) {
        let eliminated = unboxing
            .values
            .iter()
            .filter(|id| {
                matches!(
                    program.arena().inner.values[id],
                    zydeco_stackir::low::syntax::Value::VCons(_)
                        | zydeco_stackir::low::syntax::Value::ClosurePackage(_)
                )
            })
            .count();
        println!(
            "{},{policy},{},{},{},{}",
            source.display(),
            self.cells,
            self.payload_words,
            eliminated,
            unboxing.unboxed_vars.len()
        );
    }
}

fn main() {
    std::thread::Builder::new()
        .name("representation-experiment".into())
        .stack_size(32 * 1024 * 1024)
        .spawn(|| {
            if let Err(error) = Experiment::run() {
                eprintln!("{error}");
                std::process::exit(1);
            }
        })
        .expect("start experiment worker")
        .join()
        .expect("representation experiment panicked");
}

struct Experiment;

impl Experiment {
    fn run() -> Result<(), Box<dyn std::error::Error>> {
        let sources = std::env::args_os().skip(1).map(PathBuf::from).collect::<Vec<_>>();
        if sources.is_empty() {
            return Err("supply one or more executable Zydeco source paths".into());
        }
        println!(
            "source,policy,product_cells,product_payload_words,eliminated_cells,expanded_variables"
        );
        let compiler = CommandCompiler::default();
        for source in sources {
            let mut backend = compiler.lower(&source)?;
            for &strategy in RepresentationStrategy::ALL {
                backend = backend.with_representation(strategy);
                let unboxing = LocalUnboxing::with_policy(&backend.sps_low, &strategy);
                Measurement::collect(backend.assembly()).print(
                    &source,
                    strategy.name(),
                    &unboxing,
                    &backend.sps_low,
                );
            }
            Self::measure_static::<1>(&source, &backend);
            Self::measure_static::<4>(&source, &backend);
        }
        Ok(())
    }

    fn measure_static<const MAX: usize>(source: &std::path::Path, backend: &BackendProgram) {
        let assembly = LoweringPipeline::new(&backend.spans, &backend.scoped, &backend.statics)
            .with_representation(FieldLimit::<_, MAX>(Shared))
            .run_infallible(&backend.sps_low);
        let unboxing = LocalUnboxing::with_policy(&backend.sps_low, &FieldLimit::<_, MAX>(Shared));
        Measurement::collect(&assembly).print(
            source,
            &format!("shared-max-{MAX}"),
            &unboxing,
            &backend.sps_low,
        );
    }
}
