use crate::{
    dynamics::{
        self,
        eval::{Exit, Runtime},
        syntax::{ZCompute, ZValue},
    },
    lex::token::Tok,
    library::builtins,
    parse::{
        syntax::{Compute, Program, TCompute, TValue, ValOrComp, Value},
        {ExpressionParser, ZydecoParser},
    },
    statics::{ctx::Ctx, tyck::TypeCheck},
    utils::never::Never,
};
use logos::Logos;

pub fn preprocess(input: &str) -> String {
    // Dynamic linking. Doesn't work if executable is running at another folder.
    // Consider shipping std.zydeco with the executable.
    // const STDPATH: &str = "zydeco-lang/src/library/std.zydeco";
    // let mut output = std::fs::read_to_string(STDPATH).unwrap();

    // Static linking. Resolve std library at compile time. Probably won't work in the future if
    // there are more library files to include.
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
    p.tyck(&Ctx::new()).map_err(|e| e.to_string())
}

pub fn typecheck_computation(m: &Compute<()>) -> Result<TCompute<()>, String> {
    m.tyck(&Ctx::new()).map_err(|e| e.to_string())
}

pub fn typecheck_value(v: &Value<()>) -> Result<TValue<()>, String> {
    v.tyck(&Ctx::new()).map_err(|e| e.to_string())
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
    let mut runtime = Runtime::new(&mut input, &mut output);
    builtins::builtin_runtime(&mut runtime);
    match dynamics::eval::eval(sem_comp, &mut runtime) {
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
    let mut runtime = Runtime::new(r, w);
    builtins::builtin_runtime(&mut runtime);
    match dynamics::eval::eval(m.into(), &mut runtime) {
        Err(Exit::Err(s)) => Err(s),
        Err(Exit::ExitCode(exit_code)) => Ok(exit_code),
        Ok(_) => unreachable!(),
    }
}

pub fn eval_returning_computation(m: Compute<()>) -> Result<ZValue, String> {
    let sem_comp: ZCompute = m.into();
    let mut input = std::io::empty();
    let mut output = std::io::sink();
    let mut runtime = Runtime::new(&mut input, &mut output);
    builtins::builtin_runtime(&mut runtime);
    dynamics::eval::eval(sem_comp, &mut runtime).map_err(|e| match e {
        Exit::Err(s) => s,
        Exit::ExitCode(_) => unreachable!(),
    })
}
