use super::syntax::ZValue;
use std::{collections::HashMap, fmt::Debug, rc::Rc};

pub type EnvMap = HashMap<String, Rc<ZValue>>;

#[derive(Clone)]
pub enum EnvStack {
    Empty,
    Entry(EnvMap, Rc<EnvStack>),
}

impl EnvStack {
    pub fn new() -> Self {
        EnvStack::Empty
    }

    pub fn get(&self, var: &str) -> Option<Rc<ZValue>> {
        if let EnvStack::Entry(map, prev) = self {
            map.get(var).cloned().or_else(|| prev.get(var))
        } else {
            None
        }
    }
}

#[derive(Clone)]
pub struct Env {
    pub stack: Rc<EnvStack>,
    pub map: EnvMap,
}

impl Env {
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

    pub fn insert(&mut self, var: String, val: Rc<ZValue>) {
        self.map.insert(var, val);
    }

    pub fn get(&self, var: &str) -> Option<Rc<ZValue>> {
        self.map.get(var).cloned().or_else(|| self.stack.get(var))
    }
}

impl Debug for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut v = Vec::new();
        let env = self.clone().push();
        let mut ptr = env.stack.as_ref();
        loop {
            match ptr {
                EnvStack::Empty => break,
                EnvStack::Entry(map, prev) => {
                    v.extend(
                        map.into_iter().map(|(k, _v)| format!("[{}: ...]", k)),
                        // .map(|(k, v)| format!("[{}: {:?}]", k.name(), v)),
                    );
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
