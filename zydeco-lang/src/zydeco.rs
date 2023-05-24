use crate::{
    dynamics::{eval::Eval, syntax as ds},
    library::syntax as ls,
    parse::{
        err::ParseError,
        lexer::Lexer,
        parser::{TermSpanParser, ZydecoParser},
        syntax as ps,
    },
    prelude::*,
    statics::{syntax as ss, tyck, Ctx, Elaboration, Seal, TypeCheck},
    syntax::Env,
    utils::span::FileInfo,
};
pub use ds::ProgKont;
use std::{path::PathBuf, rc::Rc};

pub struct Zydeco;

impl Zydeco {
    pub fn std() -> Result<Span<ps::TopLevel>, String> {
        let source = include_str!("library/std.zydeco");
        let std_path: PathBuf = "zydeco-lang/src/library/std.zydeco".into();
        let file_info = FileInfo::new(&source, Rc::new(std_path));
        let ds = ZydecoParser::new()
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
    pub fn parse(paths: Vec<PathBuf>) -> Result<Span<ps::TopLevel>, String> {
        let mut top = Zydeco::std()?;
        for path in paths {
            let source = std::fs::read_to_string(&path).map_err(|e| format!("{}", e))?;
            top.inner = top.inner.append(Self::parse_src(&source, path)?.inner);
        }
        Ok(top)
    }
    pub fn parse_src(source: &str, path: PathBuf) -> Result<Span<ps::TopLevel>, String> {
        let file_info = FileInfo::new(source, Rc::new(path));
        let p = ZydecoParser::new()
            .parse(source, Lexer::new(source))
            .map_err(|e| format!("{}", ParseError(e, &file_info)))?
            .span_map(|span| {
                span.set_info(&file_info);
            });
        Ok(p)
    }
    pub fn elab(p: Span<ps::TopLevel>) -> Result<Span<ss::Program>, String> {
        let p = Elaboration::elab(p).map_err(|e| format!("{}", e))?;
        Ok(p)
    }
    pub fn tyck(m: Span<ss::Program>) -> Result<(), String> {
        m.syn(Ctx::default()).map_err(|e| format!("{}", e))?;
        Ok(())
    }
    pub fn link(m: ss::Program) -> Result<ls::Program, String> {
        let m: ls::Program = m.into();
        Ok(m)
    }
    pub fn eval_os(p: ls::Program, args: &[String]) -> ds::Program {
        let mut input = std::io::stdin().lock();
        let mut output = std::io::stdout();
        Self::eval_virtual_os(p, &mut input, &mut output, args)
    }
    pub fn eval_virtual_os(
        p: ls::Program, r: &mut dyn std::io::BufRead, w: &mut dyn std::io::Write, args: &[String],
    ) -> ds::Program {
        let mut runtime = ds::Runtime::new(r, w, args);
        let m = ls::Program::eval(p, &mut runtime);
        m
    }
}

#[derive(Clone)]
pub struct ZydecoExpr {
    pub ctx: Ctx,
    pub env: Env<ls::TermV, ds::SemVal>,
}

impl ZydecoExpr {
    pub fn new() -> Self {
        let std = Zydeco::std().unwrap();
        let std: Span<ss::Module> = Elaboration::elab(std).unwrap();
        let Seal(ctx) = std.syn(Ctx::default()).expect("std import failed");
        let std: ls::Module = std.inner.into();
        let mut input = std::io::empty();
        let mut output = std::io::sink();
        let mut runtime = ds::Runtime::new(&mut input, &mut output, &[]);
        std.eval(&mut runtime);
        Self { ctx, env: runtime.env }
    }
    pub fn parse(source: &str) -> Result<Span<ps::Term>, String> {
        TermSpanParser::new().parse(source, Lexer::new(source)).map_err(|e| e.to_string())
    }
    pub fn elab(val: Span<ps::Term>) -> Result<Span<ss::Term>, String> {
        let v = Elaboration::elab(val).map_err(|e| format!("{}", e))?;
        Ok(v)
    }
    pub fn tyck(&self, t: Span<ss::Term>) -> Result<ss::Type, String> {
        tyck::syn_term(t, self.ctx.clone()).map_err(|e| format!("{}", e))
    }
    pub fn link_value(val: &ss::TermValue) -> ls::SynVal {
        val.into()
    }
    pub fn link_computation(comp: &ss::TermComputation) -> ls::SynComp {
        comp.into()
    }
    pub fn eval_value(&mut self, val: ls::SynVal) -> ds::SemVal {
        let mut input = std::io::empty();
        let mut output = std::io::sink();
        let mut runtime = ds::Runtime::new(&mut input, &mut output, &[]);
        runtime.env = self.env.clone();
        val.eval(&mut runtime)
    }
    pub fn eval_ret_computation(&mut self, comp: ls::SynComp) -> ds::ProgKont {
        let mut input = std::io::empty();
        let mut output = std::io::sink();
        let mut runtime = ds::Runtime::new(&mut input, &mut output, &[]);
        runtime.env = self.env.clone();
        let m = ls::Program::eval(
            ls::Program { module: ls::Module::pure(None), entry: comp },
            &mut runtime,
        );
        self.env = runtime.env;
        m.entry
    }
    pub fn eval_os(&mut self, comp: ls::SynComp, args: &[String]) -> ds::Program {
        let mut input = std::io::stdin().lock();
        let mut output = std::io::stdout();
        let p = ls::Program { module: ls::Module::pure(None), entry: comp };
        let mut runtime = ds::Runtime::new(&mut input, &mut output, args);
        runtime.env = self.env.clone();
        let m = ls::Program::eval(p, &mut runtime);
        self.env = runtime.env;
        m
    }
}
