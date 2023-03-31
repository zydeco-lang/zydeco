#![allow(unused)]
use zydeco_lang::{
    dynamics::syntax as ds,
    statics::syntax::{self as ss, SynType},
    utils::{fmt::FmtArgs, span::SpanView},
    zydeco::ZydecoExpr,
};

pub fn launch() -> Result<i32, String> {
    println!("Zydeco v0.0.1");
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
        let term = match ZydecoExpr::parse(&line) {
            Err(e) => {
                println!("Parse Error: {}", e);
                continue;
            }
            Ok(term) => match ZydecoExpr::elab(term) {
                Err(e) => {
                    println!("Elaboration Error: {}", e);
                    continue;
                }
                Ok(term) => term,
            },
        };
        match term.inner_ref() {
            ss::Term::Value(v) => {
                match zydeco_expr.tyck_value(term.span().make(v.clone())) {
                    Err(e) => println!("Type Error: {}", e),
                    Ok(ty) => {
                        let v = ZydecoExpr::link_value(v);
                        let v = zydeco_expr.eval_value(v);
                        println!("{} :: {}", v.fmt(), ty.fmt())
                    }
                }
            }
            ss::Term::Computation(c) => {
                match zydeco_expr.tyck_computation(term.span().make(c.clone()))
                {
                    Err(e) => println!("Type Error: {}", e),
                    Ok(ty) => {
                        let SynType::TypeApp(ty_app) = &ty.synty else {
                            println!("Expected a type application, found {}", ty.fmt());
                            continue;
                        };
                        if let Some(()) = ty_app.elim_os() {
                            let c = ZydecoExpr::link_computation(c);
                            let c = zydeco_expr.eval_os(c, &[]);
                            match c.entry {
                                ds::ProgKont::Ret(value) => {
                                    unreachable!()
                                }
                                ds::ProgKont::ExitCode(i) => {
                                    println!("exited with code {}", i)
                                }
                            }
                        } else if let Some(ty) = ty_app.elim_ret() {
                            let c = ZydecoExpr::link_computation(c);
                            let c = zydeco_expr.eval_ret_computation(c);
                            match c {
                                ds::ProgKont::Ret(value) => {
                                    println!("{} :: {}", value.fmt(), ty.fmt())
                                }
                                ds::ProgKont::ExitCode(i) => {
                                    unreachable!()
                                }
                            }
                        }
                    }
                }
            }
        }
        // Ok(Span { inner: Term::Computation(m), info }) => {
        //     match zydeco::typecheck_computation(&m, &ctx) {
        //         Err(e) => println!("Type Error: {}", e),
        //         Ok(Type { ctor: TCtor::OS, .. }) => {
        //             let mut env = Env::new();
        //             builtins::link_builtin(&mut env);
        //             linker::link(&mut env, &std_decls);
        //             if let Err(e) = zydeco::eval_os_computation(m, env, &[]) {
        //                 println!("Runtime Error: {}", e)
        //             }
        //         }

        //         Ok(Type { ctor: TCtor::Ret, .. })  => {
        //             let mut env = Env::new();
        //             builtins::link_builtin(&mut env);
        //             linker::link(&mut env, &std_decls);
        //             match zydeco::eval_returning_computation(m, env) {
        //                 Err(e) => println!("Runtime Error: {}", e),
        //                 Ok(v) => println!("{}", v)
        //             }
        //         }
        //         Ok(b) => println!("Can't run computation of type {}\nCan only run computations of type OS or Ret(a)", b)

        //     }
        // }
    }
}
