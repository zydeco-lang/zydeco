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

pub fn eval_prog(p: Program<()>) -> Result<(), String> {
    eval_os_computation(*p.comp)
}

pub fn elab_prog(p: Program<()>) -> ZCompute<()> {
    (*p.comp).into()
}

pub fn eval_sem_computation(sem_comp: ZCompute<()>) -> Result<(), String> {
    dynamics::eval::eval(sem_comp, &mut builtins::builtin_runtime())
        .map_err(|e| e.to_string())?;
    Ok(())
}

pub fn eval_os_computation(m: Compute<()>) -> Result<(), String> {
    eval_sem_computation(m.into())
}

pub fn eval_returning_computation(
    m: Compute<()>,
) -> Result<ZValue<()>, String> {
    let sem_comp: ZCompute<()> = m.into();
    dynamics::eval::eval(sem_comp, &mut builtins::builtin_runtime())
        .map_err(|e| e.to_string())
}
