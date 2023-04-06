use crate::{
    dynamics::{eval::Eval, syntax as ds},
    library::syntax as ls,
    parse::{
        err::ParseError,
        parser::{TermSpanParser, ZydecoModuleParser, ZydecoParser},
        syntax as ps, Lexer,
    },
    statics::{
        syntax as ss,
        tyck::{Ctx, Seal, TypeCheck},
    },
    syntax::env::Env,
    utils::{
        monoid::Monoid,
        span::{FileInfo, Span, SpanHolder},
    },
};
use indexmap::IndexMap;
use std::{path::PathBuf, rc::Rc};

pub struct Zydeco;

impl Zydeco {
    pub fn std() -> Result<Span<ps::Module>, String> {
        let source = include_str!("library/std.zydeco");
        let std_path: PathBuf = "zydeco-lang/src/library/std.zydeco".into();
        let file_info = FileInfo::new(&source, Rc::new(std_path));
        let ds = ZydecoModuleParser::new()
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
    pub fn parse(path: PathBuf) -> Result<Span<ps::Program>, String> {
        let source = std::fs::read_to_string(&path).unwrap();
        Self::parse_src(&source, path)
    }
    pub fn parse_src(
        source: &str, path: PathBuf,
    ) -> Result<Span<ps::Program>, String> {
        let file_info = FileInfo::new(source, Rc::new(path));
        let mut p = ZydecoParser::new()
            .parse(source, Lexer::new(source))
            .map_err(|e| format!("{}", ParseError(e, &file_info)))?
            .span_map(|span| {
                span.set_info(&file_info);
            });
        p.inner.module = Zydeco::std()?.append(p.inner.module);
        Ok(p)
    }
    pub fn elab(p: Span<ps::Program>) -> Result<Span<ss::Program>, String> {
        let pr: ss::Program = p.inner.try_into().unwrap();
        Ok(p.info.make(pr))
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
        p: ls::Program, r: &mut dyn std::io::BufRead,
        w: &mut dyn std::io::Write, args: &[String],
    ) -> ds::Program {
        let mut runtime = ds::Runtime::new(r, w, args);
        let m = ds::Program::run(p, &mut runtime);
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
        let std: Span<ss::Module> =
            std.info.make(std.inner.try_into().unwrap());
        let Seal(ctx) = std.syn(Ctx::default()).expect("std import failed");
        let std: ls::Module = std.inner.into();
        let mut input = std::io::empty();
        let mut output = std::io::sink();
        let mut runtime = ds::Runtime::new(&mut input, &mut output, &[]);
        std.eval(&mut runtime);
        Self { ctx, env: runtime.env }
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
    pub fn tyck_value(
        &self, val: Span<ss::TermValue>,
    ) -> Result<ss::Type, String> {
        val.syn(self.ctx.clone()).map_err(|e| format!("{}", e))
    }
    pub fn tyck_computation(
        &self, comp: Span<ss::TermComputation>,
    ) -> Result<ss::Type, String> {
        comp.syn(self.ctx.clone()).map_err(|e| format!("{}", e))
    }
    pub fn link_value(val: &ss::TermValue) -> ls::ZVal {
        val.into()
    }
    pub fn link_computation(comp: &ss::TermComputation) -> ls::ZComp {
        comp.into()
    }
    pub fn eval_value(&mut self, val: ls::ZVal) -> ds::SemVal {
        let mut input = std::io::empty();
        let mut output = std::io::sink();
        let mut runtime = ds::Runtime::new(&mut input, &mut output, &[]);
        runtime.env = self.env.clone();
        val.eval(&mut runtime)
    }
    pub fn eval_ret_computation(&mut self, comp: ls::ZComp) -> ds::ProgKont {
        let mut input = std::io::empty();
        let mut output = std::io::sink();
        let mut runtime = ds::Runtime::new(&mut input, &mut output, &[]);
        runtime.env = self.env.clone();
        let m = ds::Program::run(
            ls::Program {
                module: ls::Module { name: None, define: IndexMap::new() },
                entry: comp,
            },
            &mut runtime,
        );
        self.env = runtime.env;
        m.entry
    }
    pub fn eval_os(&mut self, comp: ls::ZComp, args: &[String]) -> ds::Program {
        let mut input = std::io::stdin().lock();
        let mut output = std::io::stdout();
        let p = ls::Program {
            module: ls::Module { name: None, define: IndexMap::new() },
            entry: comp,
        };
        let r: &mut dyn std::io::BufRead = &mut input;
        let w: &mut dyn std::io::Write = &mut output;
        let mut runtime = ds::Runtime::new(r, w, args);
        runtime.env = self.env.clone();
        let m = ds::Program::run(p, &mut runtime);
        self.env = runtime.env;
        m
    }
}
