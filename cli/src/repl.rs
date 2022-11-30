use zydeco_lang::{
    dynamics::env::Env,
    dynamics::syntax::ZValue,
    library::builtins,
    library::declarations,
    library::linker,
    parse::syntax::{TCompute, ValOrComp},
    statics::ctx::Ctx,
    zydeco,
};

pub fn launch() -> Result<(), String> {
    println!("Zydeco v0.0.1");
    loop {
        let mut line = String::new();
        {
            use std::io::Write;
            print!("> ");
            std::io::stdout().flush().unwrap();
            let stdin = std::io::stdin();
            stdin.read_line(&mut line).map_err(|e| e.to_string())?;
        }
        match zydeco::parse_exp(&line) {
            Err(e) => println!("Parse Error: {}", e),
            Ok(ValOrComp::Val(v)) => {
                let mut ctx = Ctx::new();
                let std_decls =
                    declarations::std_decls().expect("std library failure");
                declarations::inject_ctx(&mut ctx, &std_decls)
                    .expect("std library failure");
                match zydeco::typecheck_value(&v, &ctx) {
                    Err(e) => println!("Type Error: {}", e),
                    Ok(a) => {
                        let sem_v: ZValue = v.into();
                        println!("{} : {}", sem_v, a)
                    }
                }
            }
            Ok(ValOrComp::Comp(m)) => {
                let mut ctx = Ctx::new();
                let std_decls =
                    declarations::std_decls().expect("std library failure");
                declarations::inject_ctx(&mut ctx, &std_decls)
                    .expect("std library failure");
                match zydeco::typecheck_computation(&m, &ctx) {
                    Err(e) => println!("Type Error: {}", e),
                    Ok(TCompute::OSType) => {
                        let mut env = Env::new();
                        builtins::link_builtin(&mut env);
                        linker::link(&mut env, &std_decls);
                        if let Err(e) = zydeco::eval_os_computation(m, env) {
                            println!("Runtime Error: {}", e)
                        }
                    }

                    Ok(TCompute::Ret(_, _))  => {
                        let mut env = Env::new();
                        builtins::link_builtin(&mut env);
                        linker::link(&mut env, &std_decls);
                        match zydeco::eval_returning_computation(m, env) {
                            Err(e) => println!("Runtime Error: {}", e),
                            Ok(v) => println!("{}", v)
                        }
                    }
                    Ok(b) => println!("Can't run computation of type {}\nCan only run computations of type OS or Ret(a)", b)

                }
            }
        }
    }
}
