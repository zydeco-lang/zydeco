use crate::{
    dynamics::{
        self,
        syntax::{ZCompute, ZValue},
    },
    lex::token::Tok,
    library::builtins,
    parse::{
        syntax::{Compute, Program, TCompute},
        ZydecoParser,
    },
    statics::tyck::TypeCheck,
    utils::{fmt::FmtDefault, never::Never},
};
use logos::Logos;
use std::panic;

pub struct Zydeco {
    pub title: String,
    pub verbose: bool,
}

impl Zydeco {
    pub fn run(&self, buffer: &str) -> Result<(TCompute<()>, ZValue<()>), ()> {
        let program = self.parse(&buffer)?;
        let ty = self.tyck(&program)?;
        let comp = self.elab(*program.comp)?;
        let zvalue = self.eval(comp)?;
        Ok((ty, zvalue))
    }

    pub fn check(&self, buffer: &str) -> Result<(TCompute<()>, ()), ()> {
        let program = self.parse(&buffer)?;
        let ty = self.tyck(&program)?;
        Ok((ty, ()))
    }

    fn parse(&self, input: &str) -> Result<Program<()>, ()> {
        self.phase("parse", false, || {
            let lexer = Tok::lexer(input)
                .spanned()
                .map(|(tok, range)| (range.start, tok, range.end));
            ZydecoParser::new()
                .parse(input, lexer)
                .map_err(|e| format!("{}", e))
        })
    }

    fn tyck(&self, prog: &Program<()>) -> Result<TCompute<()>, ()> {
        self.phase("check", false, || prog.tyck(&builtins::builtin_ctx()))
    }

    fn elab(&self, comp: Compute<()>) -> Result<ZCompute<()>, ()> {
        self.phase("elab", false, || -> Result<ZCompute<()>, Never> {
            Ok(comp.into())
        })
    }

    fn eval(&self, comp: ZCompute<()>) -> Result<ZValue<()>, ()> {
        self.phase("eval", true, || {
            dynamics::eval::eval(comp, &mut builtins::builtin_runtime())
        })
    }

    fn phase<F, T, E>(&self, name: &'static str, aloud: bool, input: F) -> Result<T, ()>
    where
        F: FnOnce() -> Result<T, E> + std::panic::UnwindSafe,
        T: FmtDefault,
        E: std::fmt::Display,
    {
        if self.verbose {
            println!("=== [{}] <{}>", self.title, name);
        }
        let mut output = String::new();
        panic::set_hook(Box::new(|_| {}));
        let res = panic::catch_unwind(input)
            .or_else(|err| {
                output += &format!("Panic: {:?}", err);
                output += "\n";
                Err(())
            })?
            .and_then(|res| {
                output += &format!("{}", res.fmt());
                output += "\n";
                Ok(res)
            })
            .or_else(|err| {
                output += &format!("Error: {}", err);
                output += "\n";
                Err(())
            });
        if self.verbose || aloud {
            print!("{}", output);
        }
        res
    }
}
