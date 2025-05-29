use super::err::{InterpError, Result};
use crate::prelude::*;
use zydeco_dynamics::{ProgKont, Runtime};

pub struct PackageRuntime {
    pub dynamics: d::DynamicsArena,
}

impl PackageRuntime {
    pub fn run(self) -> ProgKont {
        self.run_with_args(&[])
    }
    pub fn run_with_args(self, args: &[String]) -> ProgKont {
        let PackageRuntime { dynamics } = self;
        let mut input = std::io::stdin().lock();
        let mut output = std::io::stdout();
        
        Runtime::new(&mut input, &mut output, args, dynamics).run()
    }
    pub fn test(self, name: &str, aloud: bool) -> Result<()> {
        let PackageRuntime { dynamics } = self;
        let mut input = std::io::empty();
        let mut output = std::io::sink();
        let kont = Runtime::new(&mut input, &mut output, &[], dynamics).run();

        match kont {
            | ProgKont::ExitCode(0) => {
                if aloud {
                    let mut out = std::io::stdout();
                    use colored::Colorize;
                    use std::io::Write;
                    let _ = writeln!(out, "test {} ... {}", name, "ok".green());
                }
                Ok(())
            }
            | ProgKont::Dry => Ok(()),
            | ProgKont::ExitCode(code) => {
                let err = format!("expected exit code 0, got {}", code);
                Err(InterpError::TestFailed(err))?
            }
            | ProgKont::Ret(v) => {
                let err = format!("expected exit code 0, got a returned value: {:?}", v);
                Err(InterpError::TestFailed(err))?
            }
        }
    }
}
