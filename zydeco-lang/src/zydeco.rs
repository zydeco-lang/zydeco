use indexmap::IndexMap;

use crate::{
    dynamics::syntax as ds,
    library::syntax as ls,
    parse::{
        err::ParseError,
        parser::{DeclarationVecParser, TermSpanParser, ZydecoParser},
        syntax as ps, Lexer,
    },
    statics::{
        syntax as ss,
        tyck::{Ctx, TypeCheck},
    },
    syntax::{env::Env, DeclSymbol},
    utils::span::{FileInfo, Span, SpanHolder},
};
use std::{path::PathBuf, rc::Rc};

pub struct Zydeco;

impl Zydeco {
    pub fn std() -> Result<Vec<DeclSymbol<ps::Declaration>>, String> {
        // let std_path: PathBuf = "src/library/std.zydeco".into();
        // println!("{}", std::env::current_dir().unwrap().display());
        // let source = std::fs::read_to_string(&std_path).unwrap();
        let source = include_str!("library/std.zydeco");
        let std_path: PathBuf = "zydeco-lang/src/library/std.zydeco".into();
        let file_info = FileInfo::new(&source, Rc::new(std_path));
        let ds = DeclarationVecParser::new()
            .parse(&source, Lexer::new(&source))
            .map_err(|e| format!("{}", ParseError(e, &file_info)))?
            .span_map(|span| {
                span.set_info(&file_info);
            });
        Ok(ds)
    }
}

pub struct ZydecoFile;

impl ZydecoFile {
    pub fn parse(path: PathBuf) -> Result<Span<ps::Module>, String> {
        let source = std::fs::read_to_string(&path).unwrap();
        let file_info = FileInfo::new(&source, Rc::new(path));
        let mut m = ZydecoParser::new()
            .parse(&source, Lexer::new(&source))
            .map_err(|e| format!("{}", ParseError(e, &file_info)))?
            .span_map(|span| {
                span.set_info(&file_info);
            });
        m.inner.declarations = (Zydeco::std()?.into_iter())
            .chain(m.inner.declarations.into_iter())
            .collect();
        Ok(m)
    }
    pub fn elab(m: Span<ps::Module>) -> Result<Span<ss::Module>, String> {
        let mo: ss::Module = m.inner.try_into().unwrap();
        // println!("{}", mo.fmt());
        Ok(m.info.make(mo))
    }
    pub fn tyck(m: Span<ss::Module>) -> Result<(), String> {
        m.syn(Ctx::default()).map_err(|e| format!("{}", e))?;
        Ok(())
    }
    pub fn link(m: ss::Module) -> Result<ls::Module, String> {
        let m: ls::Module = m.into();
        Ok(m)
    }
    pub fn eval_os(
        m: ls::Module, args: &[String],
    ) -> Result<ds::Module, String> {
        let mut input = std::io::stdin().lock();
        let mut output = std::io::stdout();
        let mut runtime = ds::Runtime::new(&mut input, &mut output, args);
        let m = ds::Module::new(m, &mut runtime);
        Ok(m)
    }
    pub fn eval_virtual_os(
        m: ls::Module, r: &mut dyn std::io::BufRead,
        w: &mut dyn std::io::Write, args: &[String],
    ) -> Result<ds::Module, String> {
        let mut runtime = ds::Runtime::new(r, w, args);
        let m = ds::Module::new(m, &mut runtime);
        Ok(m)
    }
}

pub struct ZydecoExpr {
    ctx: Ctx,
    env: Env<ls::TermV, ds::SemVal>,
}

impl ZydecoExpr {
    pub fn new() -> Self {
        // let std = Zydeco::std().unwrap();
        let ctx = Ctx::default();
        let env = Env::new();
        Self { ctx, env }
    }
    pub fn parse(source: &str) -> Result<Span<ps::Term>, String> {
        TermSpanParser::new()
            .parse(source, Lexer::new(source))
            .map_err(|e| e.to_string())
    }
    pub fn elab(val: Span<ps::Term>) -> Result<Span<ss::Term>, String> {
        let v: ss::Term = val.inner.try_into().unwrap();
        Ok(val.info.make(v))
    }
    pub fn tyck_value(&self, val: Span<ss::TermValue>) -> Result<(), String> {
        val.syn(self.ctx.clone()).map_err(|e| format!("{}", e))?;
        Ok(())
    }
    pub fn tyck_computation(
        &self, comp: Span<ss::TermComputation>,
    ) -> Result<(), String> {
        comp.syn(self.ctx.clone()).map_err(|e| format!("{}", e))?;
        Ok(())
    }
    pub fn link_computation(
        &self, comp: &ss::TermComputation,
    ) -> Result<ls::ZComp, String> {
        let comp: ls::ZComp = comp.into();
        Ok(comp)
    }
    pub fn eval_ret_computation(
        &mut self, comp: ls::ZComp,
    ) -> Result<ds::ProgKont, String> {
        let mut input = std::io::empty();
        let mut output = std::io::sink();
        let mut runtime = ds::Runtime::new(&mut input, &mut output, &[]);
        runtime.env = self.env.clone();
        let m = ds::Module::new(
            ls::Module { name: None, define: IndexMap::new(), entry: comp },
            &mut runtime,
        );
        Ok(m.entry)
    }
}

// pub fn eval_prog(p: Program, args: &[String]) -> Result<Never, String> {
//     let mut env = Env::new();
//     builtins::link_builtin(&mut env);
//     linker::link(&mut env, &p.decls);
//     eval_os_computation(*p.comp, env, args)
// }

// pub fn eval_virtual_prog(
//     p: Program, mut env: Env, r: &mut dyn std::io::BufRead,
//     w: &mut dyn std::io::Write, args: &[String],
// ) -> Result<i32, String> {
//     linker::link(&mut env, &p.decls);
//     eval_virtual_os_computation(*p.comp, env, r, w, args)
// }

// pub fn elab_prog(p: Program) -> ZCompute {
//     (*p.comp).into()
// }

// pub fn eval_os_sem_computation(
//     sem_comp: ZCompute, env: Env, args: &[String],
// ) -> Result<Never, String> {
//     let mut input = std::io::BufReader::new(std::io::stdin());
//     let mut output = std::io::stdout();
//     let mut runtime = Runtime::new(env, &mut input, &mut output, &args);
//     match dynamics::eval(sem_comp, &mut runtime) {
//         Err(Exit::ExitCode(exit_code)) => std::process::exit(exit_code),
//         Err(Exit::Err(s)) => Err(s),
//         Ok(_) => unreachable!(),
//     }
// }

// pub fn eval_os_computation(
//     m: Compute, env: Env, args: &[String],
// ) -> Result<Never, String> {
//     eval_os_sem_computation(m.into(), env, args)
// }

// pub fn eval_virtual_os_computation(
//     m: Compute, env: Env, r: &mut dyn std::io::BufRead,
//     w: &mut dyn std::io::Write, args: &[String],
// ) -> Result<i32, String> {
//     let mut runtime = Runtime::new(env, r, w, args);
//     match dynamics::eval(m.into(), &mut runtime) {
//         Err(Exit::Err(s)) => Err(s),
//         Err(Exit::ExitCode(exit_code)) => Ok(exit_code),
//         Ok(_) => unreachable!(),
//     }
// }

// pub fn eval_returning_computation(
//     m: Compute, env: Env,
// ) -> Result<ZValue, String> {
//     let sem_comp: ZCompute = m.into();
//     let mut input = std::io::empty();
//     let mut output = std::io::sink();
//     let mut runtime = Runtime::new(env, &mut input, &mut output, &[]);
//     dynamics::eval(sem_comp, &mut runtime).map_err(|e| match e {
//         Exit::Err(s) => s,
//         Exit::ExitCode(_) => unreachable!(),
//     })
// }
