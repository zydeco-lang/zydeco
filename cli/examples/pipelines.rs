//! Compare static and dynamic composition on the same checked source.
//!
//! Timing scope: `docs/references/compiler.md#pipeline-examples-and-validation`.

use clap::Parser;
use std::{
    cell::Cell,
    convert::Infallible,
    hint::black_box,
    num::NonZeroUsize,
    path::PathBuf,
    time::{Duration, Instant},
};
use zydeco_cli::CommandCompiler;
use zydeco_session::ExecutableProgram;
use zydeco_stackir::{BranchJoinProgram, BuiltinRootLowerer, high::normalize::Normalizer};
use zydeco_utils::{
    pass::{CompilerPass, PassSequence},
    pipeline,
};

struct CountComputations<'a> {
    result: &'a Cell<usize>,
}

impl CompilerPass<BranchJoinProgram> for CountComputations<'_> {
    type Output = BranchJoinProgram;
    type Error = Infallible;

    fn run(&mut self, input: BranchJoinProgram) -> Result<Self::Output, Self::Error> {
        self.result.set(input.as_program().arena().inner.compus.iter().count());
        Ok(input)
    }
}

#[derive(Default)]
struct Measurement {
    construction: Duration,
    execution: Duration,
    computations: usize,
}

impl Measurement {
    fn collect<P>(
        checked: &ExecutableProgram, count: NonZeroUsize, mut configure: impl FnMut() -> P,
    ) -> Result<Self, Box<dyn std::error::Error>>
    where
        P: CompilerPass<BranchJoinProgram, Output = BranchJoinProgram, Error = Infallible>,
    {
        let mut lower = BuiltinRootLowerer {
            spans: &checked.spans,
            scoped: &checked.scoped,
            statics: &checked.statics,
            signature: checked.signature.clone(),
        };
        (0..count.get()).try_fold(Self::default(), |mut total, _| {
            let input = lower.run(checked.root)?;
            let start = Instant::now();
            let mut passes = black_box(configure());
            total.construction += start.elapsed();
            let start = Instant::now();
            let output = black_box(passes.run_infallible(black_box(input)));
            total.execution += start.elapsed();
            total.computations = output.as_program().arena().inner.compus.iter().count();
            Ok(total)
        })
    }

    fn print(&self, source: &std::path::Path, composition: &str, count: NonZeroUsize) {
        let count = count.get();
        println!(
            "{},{composition},{count},{:.0},{:.0},{}",
            source.display(),
            self.construction.as_nanos() as f64 / count as f64,
            self.execution.as_nanos() as f64 / count as f64,
            self.computations
        );
    }
}

#[derive(Parser)]
struct Experiment {
    /// Number of independently constructed inputs and pipelines per comparison
    #[arg(long, default_value = "20")]
    iterations: NonZeroUsize,
    #[arg(required = true)]
    sources: Vec<PathBuf>,
}

impl Experiment {
    fn run(self) -> Result<(), Box<dyn std::error::Error>> {
        let compiler = CommandCompiler::default();
        println!("source,composition,iterations,construction_ns,execution_ns,computations");
        for source in self.sources {
            let checked = compiler.executable(&source)?;
            // Warm both paths before collecting timings.
            Measurement::collect(&checked, NonZeroUsize::MIN, || pipeline![Normalizer])?;
            Measurement::collect(&checked, NonZeroUsize::MIN, || {
                PassSequence::new().with_pass(Normalizer)
            })?;
            let fixed = Measurement::collect(&checked, self.iterations, || pipeline![Normalizer])?;
            let dynamic = Measurement::collect(&checked, self.iterations, || {
                PassSequence::new().with_pass(Normalizer)
            })?;
            assert_eq!(fixed.computations, dynamic.computations);
            fixed.print(&source, "static", self.iterations);
            dynamic.print(&source, "dynamic", self.iterations);

            let observed = Cell::new(0);
            let custom = Measurement::collect(&checked, NonZeroUsize::MIN, || {
                pipeline![Normalizer, CountComputations { result: &observed }]
            })?;
            assert_eq!(custom.computations, observed.get());
        }
        Ok(())
    }
}

fn main() {
    let experiment = Experiment::parse();
    std::thread::Builder::new()
        .name("pipeline-experiment".into())
        .stack_size(32 * 1024 * 1024)
        .spawn(move || {
            if let Err(error) = experiment.run() {
                eprintln!("{error}");
                std::process::exit(1);
            }
        })
        .expect("start pipeline experiment worker")
        .join()
        .expect("pipeline experiment panicked");
}
