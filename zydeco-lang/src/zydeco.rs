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
use std::fs;

const STDPATH: &str = "zydeco-lang/src/library/std.zydeco";

pub fn preprocess(input: &str) -> String {
    // Original implementation. Doesn't work if executable is run at another folder
    // let mut output = fs::read_to_string(STDPATH).unwrap();

    // Resolve std library at compile time. Probably won't work in the future if
    // there are more library files to include
    let std = include_str!("library/std.zydeco");
    let mut output = std.to_string();
    
    output.push_str(input);
    output
}

pub fn parse_prog(input: &str) -> Result<Program<()>, String> {
    let preprocessed = preprocess(input);
    let lexer = Tok::lexer(&preprocessed)
        .spanned()
        .map(|(tok, range)| (range.start, tok, range.end));
    ZydecoParser::new().parse(&preprocessed, lexer).map_err(|e| e.to_string())
}

pub fn parse_exp(input: &str) -> Result<ValOrComp<()>, String> {
    let preprocessed = preprocess(input);
    let lexer = Tok::lexer(&preprocessed)
        .spanned()
        .map(|(tok, range)| (range.start, tok, range.end));
    ExpressionParser::new()
        .parse(&preprocessed, lexer)
        .map_err(|e| e.to_string())
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
