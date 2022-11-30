use super::{
    env::*,
    syntax::{ZCompute, ZValue},
};
use std::{
    fmt::Debug,
    io::{BufRead, Write},
    mem::replace,
    rc::Rc,
};

type EvalErr = String;
pub enum Exit {
    ExitCode(i32),
    Err(EvalErr),
}

impl From<EvalErr> for Exit {
    fn from(e: String) -> Self {
        Exit::Err(e)
    }
}

#[derive(Clone)]
enum Frame {
    Kont(Rc<ZCompute>, Env, String),
    Call(Rc<ZValue>),
    Dtor(String, Vec<Rc<ZValue>>),
}

impl Debug for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Frame::Kont(_, _, var) => write!(f, "Kont({})", var),
            Frame::Call(_) => write!(f, "Call"),
            Frame::Dtor(dtor, _) => write!(f, "Dtor({})", dtor),
        }
    }
}

#[derive(Debug, Clone)]
enum Stack {
    Done,
    Frame(Frame, Rc<Stack>),
}

impl Stack {
    fn new() -> Self {
        Stack::Done
    }
}

pub struct Runtime<'rt> {
    stack: Rc<Stack>,
    env: Env,
    input: &'rt mut (dyn BufRead),
    output: &'rt mut (dyn Write),
}

impl<'rt> Runtime<'rt> {
    pub fn new(
        env: Env, input: &'rt mut (dyn BufRead), output: &'rt mut (dyn Write),
    ) -> Self {
        Runtime { stack: Rc::new(Stack::new()), env, input, output }
    }

    fn get(&self, var: &str) -> Result<Rc<ZValue>, EvalErr> {
        self.env.get(var).ok_or_else(|| format!("Variable {} not found", var))
    }

    fn resolve_value(&self, val: Rc<ZValue>) -> Result<Rc<ZValue>, EvalErr> {
        use ZValue::*;
        match val.as_ref() {
            Var(var) => self.get(var),
            Thunk(thunk, None) => {
                let env = self.env.clone().push();
                Ok(Rc::new(Thunk(thunk.clone(), Some(env))))
            }
            Ctor(ctor, args) => {
                let args = args
                    .iter()
                    .map(|arg| self.resolve_value(arg.clone()))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Rc::new(Ctor(ctor.clone(), args)))
            }
            _ => Ok(val),
        }
    }

    fn call(&mut self, arg: Rc<ZValue>) -> Result<(), Exit> {
        let arg = self.resolve_value(arg)?;
        self.stack =
            Rc::new(Stack::Frame(Frame::Call(arg.clone()), self.stack.clone()));
        Ok(())
    }

    fn dtor(&mut self, dtor: String, args: Vec<Rc<ZValue>>) {
        self.stack =
            Rc::new(Stack::Frame(Frame::Dtor(dtor, args), self.stack.clone()));
    }

    fn kont(&mut self, comp: Rc<ZCompute>, var: String) {
        self.push();
        self.stack = Rc::new(Stack::Frame(
            Frame::Kont(comp, self.env.clone(), var),
            self.stack.clone(),
        ));
    }

    fn push(&mut self) {
        let env = replace(&mut self.env, Env::new());
        self.env = env.push();
    }

    pub fn insert(
        &mut self, var: String, val: Rc<ZValue>,
    ) -> Result<(), EvalErr> {
        self.env.insert(var, self.resolve_value(val)?.clone());
        Ok(())
    }

    fn step(&mut self, comp: ZCompute) -> Result<Rc<ZCompute>, Exit> {
        use {ZCompute::*, ZValue::*};
        match comp {
            Let { binding: (var, val), body, .. } => {
                self.insert(var, val)?;
                Ok(body)
            }
            Do { binding: (var, comp), body, .. } => {
                self.kont(body, var);
                Ok(comp)
            }
            Force(val) => {
                if let Thunk(comp, Some(env)) =
                    self.resolve_value(val.clone())?.as_ref()
                {
                    self.env = env.clone();
                    Ok(comp.clone())
                } else {
                    Err(Exit::Err(format!("Force on non-thunk value: {}", val)))
                }
            }
            Return(val) => {
                let val = self.resolve_value(val)?;
                let stack = self.stack.to_owned();
                if let Stack::Frame(Frame::Kont(comp, env, var), prev) =
                    stack.as_ref()
                {
                    self.stack = prev.clone();
                    self.env = env.pop().ok_or_else(|| {
                        Exit::Err(format!("EnvStack is empty"))
                    })?;
                    self.insert(var.clone(), val)?;
                    Ok(comp.clone())
                } else {
                    Err(Exit::Err(format!("Return on non-kont frame: {}", val)))
                }
            }
            Lam { arg: var, body } => {
                let stack = self.stack.to_owned();
                if let Stack::Frame(Frame::Call(arg), prev) = stack.as_ref() {
                    self.stack = prev.clone();
                    self.insert(var, arg.clone())?;
                    Ok(body)
                } else {
                    Err(Exit::Err(format!("Lam on non-call frame")))
                }
            }
            Prim { arity, body, .. } => {
                let mut args = Vec::new();
                for _ in 0..arity {
                    let stack = self.stack.to_owned();
                    if let Stack::Frame(Frame::Call(arg), prev) = stack.as_ref()
                    {
                        self.stack = prev.clone();
                        args.push((**arg).clone());
                    }
                }
                match body(args, self.input, self.output) {
                    Ok(m) => Ok(Rc::new(m)),
                    Err(exit_code) => Err(Exit::ExitCode(exit_code)),
                }
            }
            Rec { arg, body } => {
                self.insert(
                    arg.clone(),
                    Rc::new(Thunk(
                        Rc::new(Rec { arg, body: body.clone() }),
                        Some(self.env.clone()),
                    )),
                )?;
                Ok(body)
            }
            App(f, arg) => {
                self.call(arg.clone())?;
                Ok(f)
            }
            Match { scrut, cases } => {
                if let Ctor(ctor, args) =
                    self.resolve_value(scrut.clone())?.as_ref()
                {
                    let (_, vars, comp) = cases
                        .into_iter()
                        .find(|(pat, ..)| pat == ctor)
                        .ok_or_else(|| {
                            Exit::Err(format!("Ctor {:?} mismatch", ctor))
                        })?;
                    for (var, arg) in vars.iter().zip(args.iter()) {
                        self.insert(var.clone(), arg.clone())?;
                    }
                    Ok(comp)
                } else {
                    Err(Exit::Err(format!(
                        "Match on non-ctor value: {}",
                        scrut.as_ref()
                    )))
                }
            }
            CoMatch { cases } => {
                let stack = self.stack.to_owned();
                if let Stack::Frame(Frame::Dtor(dtor, args), prev) =
                    stack.as_ref()
                {
                    self.stack = prev.clone();
                    let (_, vars, comp) = cases
                        .iter()
                        .find(|(pat, ..)| *pat == *dtor)
                        .ok_or_else(|| Exit::Err(format!("Dtor mismatch")))?;
                    for (var, arg) in vars.iter().zip(args.iter()) {
                        self.insert(var.clone(), arg.clone())?;
                    }
                    Ok(comp.clone())
                } else {
                    Err(Exit::Err(format!("CoMatch on non-dtor frame")))
                }
            }
            CoApp { scrut, dtor, args, .. } => {
                self.dtor(
                    dtor,
                    args.into_iter()
                        .map(|val| self.resolve_value(val))
                        .collect::<Result<_, _>>()?,
                );
                Ok(scrut)
            }
        }
    }

    fn eval(&mut self, mut comp: ZCompute) -> Result<ZValue, Exit> {
        use ZCompute::*;
        const MAX_STEPS: usize = 1000; // TODO: make this programmable too
        let mut steps = 0;
        while steps <= MAX_STEPS {
            match (comp, self.stack.as_ref()) {
                (Return(val), Stack::Done) => {
                    return Ok(self.resolve_value(val)?.as_ref().clone());
                }
                (c, _) => {
                    comp = self.step(c)?.as_ref().clone();
                    // println!("|- {:#?}", self.env);
                    // println!();
                    // println!(":: {:#?}", self.stack);
                    // println!();
                    // println!("|> {:?}", comp);
                    // println!();
                    // println!();
                    // println!();
                }
            }
            steps += 1;
        }
        Err(Exit::Err(format!(
            "My name is megumi! Exceeded max steps: {}",
            MAX_STEPS
        )))
    }
}

pub fn eval(comp: ZCompute, runtime: &mut Runtime) -> Result<ZValue, Exit> {
    runtime.eval(comp)
}
