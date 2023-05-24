#![allow(unused)]
use zydeco_lang::{dynamics::syntax as ds, prelude::*, statics::syntax as ss, zydeco::ZydecoExpr};

pub struct Repl;

impl Repl {
    pub fn launch() -> Result<i32, String> {
        println!("Zydeco v0.2.0");
        let mut zydeco_expr = ZydecoExpr::new();
        loop {
            let mut line = String::new();
            {
                use std::io::Write;
                print!("> ");
                std::io::stdout().flush().unwrap();
                let stdin = std::io::stdin();
                let n = stdin.read_line(&mut line).map_err(|e| e.to_string())?;
                // Ctrl-D to exit
                if n == 0 {
                    break Ok(0);
                }
            }
            let (line, dry) = match Self::preprocess(&mut zydeco_expr, line) {
                Ok(Some(config)) => config,
                Ok(None) => continue,
                Err(e) => {
                    println!("{}", e);
                    continue;
                }
            };
            match Self::run(&mut zydeco_expr, &line, dry) {
                Ok(_) => {}
                Err(e) => {
                    println!("{}", e);
                    continue;
                }
            }
        }
    }
    pub fn preprocess(
        zydeco_expr: &mut ZydecoExpr, mut line: String,
    ) -> Result<Option<(String, bool)>, String> {
        // if nothing's there, return
        if line.trim().is_empty() {
            return Ok(None);
        }
        // check for commands
        let mut dry = false;
        if line.trim_start().starts_with('#') {
            if line.starts_with("#env") {
                // command #env: print all variables in scope
                // Note: with the module system we can have a more
                // fine-grained control to the namespace and print
                // less things
                println!("{}", zydeco_expr.env.fmt());
                return Ok(None);
            } else if line.starts_with("#type") | line.starts_with("#t") | line.starts_with("#dry")
            {
                // command #type: type checks but not run a term
                if let Some((_, term)) = line.split_once(' ') {
                    line = term.to_string();
                    dry = true;
                } else {
                    Err(format!("Missing term"))?
                }
            } else {
                // unknown commands
                Err(format!("Unknown command {}", line.trim()))?
            }
        }
        Ok(Some((line, dry)))
    }
    pub fn run(zydeco_expr: &mut ZydecoExpr, line: &str, dry: bool) -> Result<(), String> {
        // parse and elaborate
        let term = match ZydecoExpr::parse(&line) {
            Err(e) => Err(format!("Parse Error: {}", e))?,
            Ok(term) => match ZydecoExpr::elab(term) {
                Err(e) => Err(format!("Elaboration Error: {}", e))?,
                Ok(term) => term,
            },
        };
        // typecheck and evaluate
        let ty = match zydeco_expr.tyck(term.clone()) {
            Err(e) => Err(format!("Type Error: {}", e))?,
            Ok(ty) => ty,
        };
        if dry || matches!(term.inner_ref(), ss::Term::Value(_)) {
            // Note: not evaluating the value, just printing its type
            println!("{} : {}", term.inner_ref().fmt(), ty.fmt());
            Ok(())
        } else {
            let c = match term.inner_ref() {
                ss::Term::Computation(c) => c,
                _ => unreachable!(),
            };
            // Note: The evaluation will destroy the environment,
            // so we need to save a snapshot of it before we run.
            let snapshot = zydeco_expr.clone();
            let res =
                if let Some(()) = ty.clone().elim_os(zydeco_expr.ctx.clone(), &SpanInfo::dummy()) {
                    let c = ZydecoExpr::link_computation(c);
                    let c = zydeco_expr.eval_os(c, &[]);
                    let ds::ProgKont::ExitCode(i) = c.entry else {
                        unreachable!()
                    };
                    println!("Program exited with code {}", i);
                    Ok(())
                } else if let Some(ty) =
                    ty.clone().elim_ret(zydeco_expr.ctx.clone(), &SpanInfo::dummy())
                {
                    let c = ZydecoExpr::link_computation(c);
                    let c = zydeco_expr.eval_ret_computation(c);
                    let ds::ProgKont::Ret(value) = c else {
                        unreachable!()
                    };
                    println!("{} : {}", value.fmt(), ty.fmt());
                    Ok(())
                } else {
                    let mut s = String::new();
                    s += &format!("Can't run computation of type {}", ty.fmt());
                    s += &format!("Can only run computations of type OS or Ret(a)");
                    Err(s)
                };
            // Note: Restore the environment
            *zydeco_expr = snapshot;
            res
        }
    }
}
