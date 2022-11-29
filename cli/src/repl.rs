use zydeco_lang::dynamics::env::Env;
use zydeco_lang::dynamics::syntax::ZValue;
use zydeco_lang::library::builtins;
use zydeco_lang::parse::syntax::{TCompute, ValOrComp};
use zydeco_lang::zydeco;

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
                match zydeco::typecheck_value(&v) {
                    Err(e) => println!("Type Error: {}", e),
                    Ok(a) => {
                        let sem_v: ZValue = v.into();
                        println!("{} : {}", sem_v, a)
                    }
                }
            }
            Ok(ValOrComp::Comp(m)) => {
                match zydeco::typecheck_computation(&m) {
                    Err(e) => println!("Type Error: {}", e),
                    Ok(TCompute::OSType) => {
                        let mut env = Env::new();
                        builtins::link_builtin(&mut env);
                        if let Err(e) = zydeco::eval_os_computation(m, env) {
                            println!("Runtime Error: {}", e)
                        }
                    }

                    Ok(TCompute::Ret(_, _))  => {
                        let mut env = Env::new();
                        builtins::link_builtin(&mut env);
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
