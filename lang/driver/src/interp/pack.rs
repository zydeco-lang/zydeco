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

        let mut konts = Runtime::new(&mut input, &mut output, args, dynamics).run();

        if konts.len() != 1 {
            panic!("{} entry points executed, expected exactly 1", konts.len());
        }
        konts.pop().unwrap()
    }
    pub fn test(self, name: &str, aloud: bool) -> Result<()> {
        let PackageRuntime { dynamics } = self;
        let mut input = std::io::empty();
        let mut output = std::io::sink();
        let konts = Runtime::new(&mut input, &mut output, &[], dynamics).run();

        for kont in konts {
            match kont {
                | ProgKont::ExitCode(0) => {
                    if aloud {
                        let mut out = std::io::stdout();
                        use colored::Colorize;
                        use std::io::Write;
                        let _ = writeln!(out, "test {} ... {}", name, "ok".green());
                    }
                    continue;
                }
                | ProgKont::Dry => continue,
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

        Ok(())
    }
}
