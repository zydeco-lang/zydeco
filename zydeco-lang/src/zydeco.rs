use crate::{
    dynamics::{
        self,
        eval::Exit,
        syntax::{ZCompute, ZValue},
    },
    lex::token::Tok,
    library::builtins,
    parse::{
        syntax::{Compute, Program, TCompute, TValue, ValOrComp, Value},
        {ExpressionParser, ZydecoParser},
    },
    statics::tyck::TypeCheck,
    utils::never::Never,
};
use logos::Logos;

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

pub fn eval_prog(p: Program<()>) -> Result<Never, String> {
    eval_os_computation(*p.comp)
}

pub fn eval_virtual_prog(
    p: Program<()>, r: &mut dyn std::io::BufRead, w: &mut dyn std::io::Write,
) -> Result<i32, String> {
    eval_virtual_os_computation(*p.comp, r, w)
}

pub fn elab_prog(p: Program<()>) -> ZCompute {
    (*p.comp).into()
}

pub fn eval_os_sem_computation(sem_comp: ZCompute) -> Result<Never, String> {
    let mut input = std::io::BufReader::new(std::io::stdin());
    let mut output = std::io::stdout();
    match dynamics::eval::eval(
        sem_comp,
        &mut builtins::builtin_runtime(&mut input, &mut output),
    ) {
        Err(Exit::ExitCode(exit_code)) => std::process::exit(exit_code),
        Err(Exit::Err(s)) => Err(s),
        Ok(_) => unreachable!(),
    }
}

pub fn eval_os_computation(m: Compute<()>) -> Result<Never, String> {
    eval_os_sem_computation(m.into())
}

pub fn eval_virtual_os_computation(
    m: Compute<()>, r: &mut dyn std::io::BufRead, w: &mut dyn std::io::Write,
) -> Result<i32, String> {
    match dynamics::eval::eval(m.into(), &mut builtins::builtin_runtime(r, w)) {
        Err(Exit::Err(s)) => Err(s),
        Err(Exit::ExitCode(exit_code)) => Ok(exit_code),
        Ok(_) => unreachable!(),
    }
}

pub fn eval_returning_computation(m: Compute<()>) -> Result<ZValue, String> {
    let sem_comp: ZCompute = m.into();
    let mut input = std::io::empty();
    let mut output = std::io::sink();
    dynamics::eval::eval(
        sem_comp,
        &mut builtins::builtin_runtime(&mut input, &mut output),
    )
    .map_err(|e| match e {
        Exit::Err(s) => s,
        Exit::ExitCode(_) => unreachable!(),
    })
}
