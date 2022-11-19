use crate::{
    dynamics::{
        self,
        syntax::{ZCompute, ZValue},
    },
    lex::token::Tok,
    library::builtins,
    parse::{
        syntax::{Compute, Program, TCompute, TValue, ValOrComp, Value},
        {ExpressionParser, ZydecoParser},
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

#[derive(PartialEq)]
enum Aloud {
    Quiet,
    AloudBody,
    AloudAll,
}
use Aloud::*;

pub fn parse_prog(input: &str) -> Result<Program<()>, String> {
    let lexer = Tok::lexer(input)
        .spanned()
        .map(|(tok, range)| (range.start, tok, range.end));
    ZydecoParser::new().parse(input, lexer).map_err(|e| e.to_string())
}

pub fn parse_exp(input: &str) -> Result<ValOrComp<()>, String> {
    let lexer = Tok::lexer(input)
        .spanned()
        .map(|(tok, range)| (range.start, tok, range.end));
    ExpressionParser::new().parse(input, lexer).map_err(|e| e.to_string())
}

pub fn typecheck_prog(p: &Program<()>) -> Result<(), String> {
    p.tyck(&builtins::builtin_ctx()).map_err(|e| e.to_string())
}

pub fn typecheck_computation(m: &Compute<()>) -> Result<TCompute<()>, String> {
    m.tyck(&builtins::builtin_ctx()).map_err(|e| e.to_string())
}

pub fn typecheck_value(v: &Value<()>) -> Result<TValue<()>, String> {
    v.tyck(&builtins::builtin_ctx()).map_err(|e| e.to_string())
}

pub fn eval_prog(p: Program<()>) -> Result<(), String> {
    eval_os_computation(*p.comp)
}

pub fn eval_os_computation(m: Compute<()>) -> Result<(), String> {
    let sem_comp: ZCompute<()> = m.into();
    dynamics::eval::eval(sem_comp, &mut builtins::builtin_runtime())
        .map_err(|e| e.to_string())?;
    Ok(())
}

pub fn eval_returning_computation(
    m: Compute<()>,
) -> Result<ZValue<()>, String> {
    let sem_comp: ZCompute<()> = m.into();
    dynamics::eval::eval(sem_comp, &mut builtins::builtin_runtime())
        .map_err(|e| e.to_string())
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
        self.phase("parse", Quiet, || {
            let lexer = Tok::lexer(input)
                .spanned()
                .map(|(tok, range)| (range.start, tok, range.end));
            ZydecoParser::new()
                .parse(input, lexer)
                .map_err(|e| format!("{}", e))
        })
    }

    fn tyck(&self, prog: &Program<()>) -> Result<TCompute<()>, ()> {
        self.phase("check", Quiet, || {
            prog.tyck(&builtins::builtin_ctx())
                .map(|_| crate::parse::syntax::TCompute::Os)
        })
    }

    fn elab(&self, comp: Compute<()>) -> Result<ZCompute<()>, ()> {
        self.phase("elab", Quiet, || -> Result<ZCompute<()>, Never> {
            Ok(comp.into())
        })
    }

    fn eval(&self, comp: ZCompute<()>) -> Result<ZValue<()>, ()> {
        self.phase("eval", AloudBody, || {
            dynamics::eval::eval(comp, &mut builtins::builtin_runtime())
        })
    }

    fn phase<F, T, E>(
        &self, name: &'static str, mut aloud: Aloud, input: F,
    ) -> Result<T, ()>
    where
        F: FnOnce() -> Result<T, E> + std::panic::UnwindSafe,
        T: FmtDefault,
        E: std::fmt::Display,
    {
        let mut output = String::new();
        panic::set_hook(Box::new(|_| {}));
        let res = panic::catch_unwind(input)
            .or_else(|err| {
                aloud = AloudAll;
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
                aloud = AloudAll;
                output += &format!("Error: {}", err);
                output += "\n";
                Err(())
            });
        if self.verbose || aloud == AloudAll {
            println!("=== [{}] <{}>", self.title, name);
        }
        if self.verbose || matches!(aloud, AloudAll | AloudBody) {
            print!("{}", output);
        }
        res
    }
}
