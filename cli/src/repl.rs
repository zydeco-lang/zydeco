#![allow(unused)]
use zydeco_lang::{statics::syntax as ss, zydeco::ZydecoExpr};

pub fn launch() -> Result<(), String> {
    println!("Zydeco v0.0.1");
    let mut zydeco_expr = ZydecoExpr::new();
    loop {
        let mut line = String::new();
        {
            use std::io::Write;
            print!("> ");
            std::io::stdout().flush().unwrap();
            let stdin = std::io::stdin();
            stdin.read_line(&mut line).map_err(|e| e.to_string())?;
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
            ss::Term::Value(v) => todo!(),
            ss::Term::Computation(_) => todo!(),
        }
        // Ok(Span { inner: Term::Value(v), info }) => {
        //     match zydeco::typecheck_value(&v, &ctx) {
        //         Err(e) => println!("Type Error: {}", e),
        //         Ok(a) => {
        //             let sem_v: ZValue = v.into();
        //             println!("{} : {}", sem_v, a)
        //         }
        //     }
        // }
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
