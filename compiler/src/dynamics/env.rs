use super::syntax::ZValue;
use crate::parse::syntax::VVar;
use std::{collections::HashMap, rc::Rc};

pub type EnvMap<Ann> = HashMap<VVar<Ann>, Rc<ZValue<Ann>>>;

#[derive(Debug, Clone)]
pub enum EnvStack<Ann> {
    Empty,
    Entry(EnvMap<Ann>, Rc<EnvStack<Ann>>),
}

impl<Ann: Clone> EnvStack<Ann> {
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

#[derive(Debug, Clone)]
pub struct Env<Ann> {
    pub stack: Rc<EnvStack<Ann>>,
    pub map: EnvMap<Ann>,
}

impl<Ann: Clone> Env<Ann> {
    pub fn new() -> Self {
        Env { stack: Rc::new(EnvStack::new()), map: HashMap::new() }
    }

    pub fn push(self) -> Self {
        Env {
            stack: Rc::new(EnvStack::Entry(self.map, self.stack.clone())),
            map: HashMap::new(),
        }
    }

    pub fn pop(&self) -> Option<Self> {
        if let EnvStack::Entry(map, prev) = &*self.stack {
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
