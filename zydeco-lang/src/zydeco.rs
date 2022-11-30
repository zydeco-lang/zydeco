use crate::{
    dynamics::{
        self,
        env::Env,
        eval::{Exit, Runtime},
        syntax::{ZCompute, ZValue},
    },
    lex::token::Tok,
    library::{builtins, linker},
    parse::{
        syntax::{Compute, Program, TCompute, TValue, ValOrComp, Value},
        {ExpressionParser, ZydecoParser},
    },
    statics::{ctx::Ctx, tyck::TypeCheck},
    utils::never::Never,
};
use logos::Logos;

pub fn parse_prog(input: &str) -> Result<Program, String> {
    let lexer = Tok::lexer(&input)
        .spanned()
        .map(|(tok, range)| (range.start, tok, range.end));
    ZydecoParser::new().parse(&input, lexer).map_err(|e| e.to_string())
}

pub fn parse_exp(input: &str) -> Result<ValOrComp, String> {
    let lexer = Tok::lexer(&input)
        .spanned()
        .map(|(tok, range)| (range.start, tok, range.end));
    ExpressionParser::new().parse(&input, lexer).map_err(|e| e.to_string())
}

pub fn typecheck_prog(p: &Program, ctx: &Ctx) -> Result<(), String> {
    p.tyck(ctx).map_err(|e| e.to_string())
}

pub fn typecheck_computation(
    m: &Compute, ctx: &Ctx,
) -> Result<TCompute, String> {
    m.tyck(ctx).map_err(|e| e.to_string())
}

pub fn typecheck_value(v: &Value, ctx: &Ctx) -> Result<TValue, String> {
    v.tyck(ctx).map_err(|e| e.to_string())
}

pub fn eval_prog(p: Program) -> Result<Never, String> {
    let mut env = Env::new();
    builtins::link_builtin(&mut env);
    linker::link(&mut env, &p.decls);
    eval_os_computation(*p.comp, env)
}

pub fn eval_virtual_prog(
    p: Program, r: &mut dyn std::io::BufRead, w: &mut dyn std::io::Write,
) -> Result<i32, String> {
    let mut env = Env::new();
    builtins::link_builtin(&mut env);
    linker::link(&mut env, &p.decls);
    eval_virtual_os_computation(*p.comp, env, r, w)
}

pub fn elab_prog(p: Program) -> ZCompute {
    (*p.comp).into()
}

pub fn eval_os_sem_computation(
    sem_comp: ZCompute, env: Env,
) -> Result<Never, String> {
    let mut input = std::io::BufReader::new(std::io::stdin());
    let mut output = std::io::stdout();
    let mut runtime = Runtime::new(env, &mut input, &mut output);
    match dynamics::eval::eval(sem_comp, &mut runtime) {
        Err(Exit::ExitCode(exit_code)) => std::process::exit(exit_code),
        Err(Exit::Err(s)) => Err(s),
        Ok(_) => unreachable!(),
    }
}

pub fn eval_os_computation(m: Compute, env: Env) -> Result<Never, String> {
    eval_os_sem_computation(m.into(), env)
}

pub fn eval_virtual_os_computation(
    m: Compute, env: Env, r: &mut dyn std::io::BufRead,
    w: &mut dyn std::io::Write,
) -> Result<i32, String> {
    let mut runtime = Runtime::new(env, r, w);
    match dynamics::eval::eval(m.into(), &mut runtime) {
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
    let mut runtime = Runtime::new(env, &mut input, &mut output);
    dynamics::eval::eval(sem_comp, &mut runtime).map_err(|e| match e {
        Exit::Err(s) => s,
        Exit::ExitCode(_) => unreachable!(),
    })
}
