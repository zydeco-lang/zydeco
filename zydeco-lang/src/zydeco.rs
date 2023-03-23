use crate::{
    dynamics::{self, Env, Exit, Runtime, ZCompute, ZValue},
    library::{builtins, linker},
    parse::{
        err::ParseError,
        legacy::syntax::{Compute, Program, Type, ValOrComp, Value},
        Lexer, {ExpressionParser, ZydecoParser},
    },
    statics::{Ctx, TypeCheck},
    syntax::span::{FileInfo, SpanHolder},
    utils::never::Never,
};
use std::{path::PathBuf, rc::Rc};

pub struct ZydecoFile {
    pub path: PathBuf,
}

impl ZydecoFile {
    pub fn parse(self) -> Result<Program, String> {
        let source = std::fs::read_to_string(&self.path).unwrap();
        let file_info = FileInfo::new(&source, Rc::new(self.path));
        let p = ZydecoParser::new()
            .parse(&source, Lexer::new(&source))
            .map_err(|e| format!("{}", ParseError(e, &file_info)))?
            .span_map(|span| {
                span.set_info(&file_info);
            });
        Ok(p)
    }
}

pub struct ZydecoExpr {
    pub source: String,
}

impl ZydecoExpr {
    pub fn parse(self) -> Result<ValOrComp, String> {
        ExpressionParser::new()
            .parse(&self.source, Lexer::new(&self.source))
            .map_err(|e| e.to_string())
    }
}

pub fn parse_prog(input: &str) -> Result<Program, String> {
    ZydecoParser::new()
        .parse(&input, Lexer::new(&input))
        .map_err(|e| e.to_string())
}

pub fn parse_exp(input: &str) -> Result<ValOrComp, String> {
    ExpressionParser::new()
        .parse(&input, Lexer::new(&input))
        .map_err(|e| e.to_string())
}

pub fn typecheck_prog(p: &Program, ctx: &Ctx) -> Result<(), String> {
    p.syn(ctx).map_err(|e| e.to_string())
}

pub fn typecheck_computation(m: &Compute, ctx: &Ctx) -> Result<Type, String> {
    m.syn(ctx).map_err(|e| e.to_string())
}

pub fn typecheck_value(v: &Value, ctx: &Ctx) -> Result<Type, String> {
    v.syn(ctx).map_err(|e| e.to_string())
}

pub fn eval_prog(p: Program, args: &[String]) -> Result<Never, String> {
    let mut env = Env::new();
    builtins::link_builtin(&mut env);
    linker::link(&mut env, &p.decls);
    eval_os_computation(*p.comp, env, args)
}

pub fn eval_virtual_prog(
    p: Program, mut env: Env, r: &mut dyn std::io::BufRead,
    w: &mut dyn std::io::Write, args: &[String],
) -> Result<i32, String> {
    linker::link(&mut env, &p.decls);
    eval_virtual_os_computation(*p.comp, env, r, w, args)
}

pub fn elab_prog(p: Program) -> ZCompute {
    (*p.comp).into()
}

pub fn eval_os_sem_computation(
    sem_comp: ZCompute, env: Env, args: &[String],
) -> Result<Never, String> {
    let mut input = std::io::BufReader::new(std::io::stdin());
    let mut output = std::io::stdout();
    let mut runtime = Runtime::new(env, &mut input, &mut output, &args);
    match dynamics::eval(sem_comp, &mut runtime) {
        Err(Exit::ExitCode(exit_code)) => std::process::exit(exit_code),
        Err(Exit::Err(s)) => Err(s),
        Ok(_) => unreachable!(),
    }
}

pub fn eval_os_computation(
    m: Compute, env: Env, args: &[String],
) -> Result<Never, String> {
    eval_os_sem_computation(m.into(), env, args)
}

pub fn eval_virtual_os_computation(
    m: Compute, env: Env, r: &mut dyn std::io::BufRead,
    w: &mut dyn std::io::Write, args: &[String],
) -> Result<i32, String> {
    let mut runtime = Runtime::new(env, r, w, args);
    match dynamics::eval(m.into(), &mut runtime) {
        Err(Exit::Err(s)) => Err(s),
        Err(Exit::ExitCode(exit_code)) => Ok(exit_code),
        Ok(_) => unreachable!(),
    }
}

pub fn eval_returning_computation(
    m: Compute, env: Env,
) -> Result<ZValue, String> {
    let sem_comp: ZCompute = m.into();
    let mut input = std::io::empty();
    let mut output = std::io::sink();
    let mut runtime = Runtime::new(env, &mut input, &mut output, &[]);
    dynamics::eval(sem_comp, &mut runtime).map_err(|e| match e {
        Exit::Err(s) => s,
        Exit::ExitCode(_) => unreachable!(),
    })
}
