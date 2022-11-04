use super::syntax::ZValue;
use crate::{parse::syntax::VVar, utils::ann::AnnT};
use std::{collections::HashMap, rc::Rc, fmt::Debug};

pub type EnvMap<Ann> = HashMap<VVar<Ann>, Rc<ZValue<Ann>>>;

#[derive(Clone)]
pub enum EnvStack<Ann: AnnT> {
    Empty,
    Entry(EnvMap<Ann>, Rc<EnvStack<Ann>>),
}

impl<Ann: AnnT> EnvStack<Ann> {
    pub fn new() -> Self {
        EnvStack::Empty
    }

    pub fn get(&self, var: &VVar<Ann>) -> Option<Rc<ZValue<Ann>>> {
        if let EnvStack::Entry(map, prev) = self {
            map.get(var).cloned().or_else(|| prev.get(var))
        } else {
            None
        }
    }
}

#[derive(Clone)]
pub struct Env<Ann: AnnT> {
    pub stack: Rc<EnvStack<Ann>>,
    pub map: EnvMap<Ann>,
}

impl<Ann: AnnT> Env<Ann> {
    pub fn new() -> Self {
        Env { stack: Rc::new(EnvStack::new()), map: HashMap::new() }
    }

    pub fn push(self) -> Self {
        if self.map.len() != 0 {
            Env {
                stack: Rc::new(EnvStack::Entry(self.map, self.stack.clone())),
                map: HashMap::new(),
            }
        } else {
            self
        }
    }

    pub fn pop(&self) -> Option<Self> {
        if let EnvStack::Entry(map, prev) = self.stack.as_ref() {
            Some(Env { stack: prev.clone(), map: map.clone() })
        } else {
            None
        }
    }

    pub fn insert(&mut self, var: VVar<Ann>, val: Rc<ZValue<Ann>>) {
        self.map.insert(var, val);
    }

    pub fn get(&self, var: &VVar<Ann>) -> Option<Rc<ZValue<Ann>>> {
        self.map.get(var).cloned().or_else(|| self.stack.get(var))
    }
}

impl<Ann: AnnT> Debug for Env<Ann> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut v = Vec::new();
        let env = self.clone().push();
        let mut ptr = env.stack.as_ref();
        loop {
            match ptr {
                EnvStack::Empty => break,
                EnvStack::Entry(map, prev) => {
                    v.extend(map);
                    ptr = prev.as_ref();
                }
            }
        }
        if f.alternate() {
            write!(f, "{:#?}", v)
        } else {
            write!(f, "{:?}", v)
        }
    }
}