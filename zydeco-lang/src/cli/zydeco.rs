use crate::{
    dynamics::{
        self,
        syntax::{ZCompute, ZValue},
    },
    library::builtins,
    parse::{
        syntax::{Compute, Program, TCompute},
        ZydecoParser,
    },
    statics::tyck::TypeCheck,
    utils::fmt::FmtDefault,
};
use std::panic;

pub struct Zydeco {
    header: Box<dyn Fn(&'static str) -> ()>,
}

enum Void {}

impl std::fmt::Display for Void {
    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {}
    }
}

impl Zydeco {
    pub fn run(
        title: String, buffer: &str,
    ) -> Result<(TCompute<()>, ZValue<()>), ()> {
        let main = Zydeco {
            header: Box::new(move |name| {
                println!("=== [{}] <{}>", title, name)
            }),
        };
        let program = main.parse(&buffer)?;
        let ty = main.tyck(&program)?;
        let comp = main.elab(*program.comp)?;
        let zvalue = main.eval(comp)?;
        Ok((ty, zvalue))
    }

    pub fn check(
        title: String, buffer: &str,
    ) -> Result<(TCompute<()>, ()), ()> {
        let main = Zydeco {
            header: Box::new(move |name| {
                println!("=== [{}] <{}>", title, name)
            }),
        };
        let program = main.parse(&buffer)?;
        let ty = main.tyck(&program)?;
        Ok((ty, ()))
    }

    fn parse(&self, input: &str) -> Result<Program<()>, ()> {
        (self.header)("parse");
        Self::phase(|| ZydecoParser::new().parse(input))
    }

    fn tyck(&self, prog: &Program<()>) -> Result<TCompute<()>, ()> {
        (self.header)("check");
        Self::phase(|| prog.tyck(&builtins::builtin_ctx()))
    }

    fn elab(&self, comp: Compute<()>) -> Result<ZCompute<()>, ()> {
        (self.header)("elab");
        Self::phase(|| -> Result<ZCompute<()>, Void> { Ok(comp.into()) })
    }

    fn eval(&self, comp: ZCompute<()>) -> Result<ZValue<()>, ()> {
        (self.header)("eval");
        Self::phase(|| {
            dynamics::eval::eval(comp, &mut builtins::builtin_runtime())
        })
    }

    fn phase<F, T, E>(input: F) -> Result<T, ()>
    where
        F: FnOnce() -> Result<T, E> + std::panic::UnwindSafe,
        T: FmtDefault,
        E: std::fmt::Display,
    {
        panic::set_hook(Box::new(|_| {}));
        panic::catch_unwind(input)
            .or_else(|err| {
                println!("Panic: {:?}", err);
                Err(())
            })?
            .and_then(|res| {
                println!("{}", res.fmt());
                Ok(res)
            })
            .or_else(|err| {
                println!("Error: {}", err);
                Err(())
            })
    }
}
