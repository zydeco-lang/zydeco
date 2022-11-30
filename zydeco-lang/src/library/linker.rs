use crate::{
    dynamics::{env::Env, syntax::ZValue},
    parse::syntax::{Declare, Value::*},
};
use std::rc::Rc;

pub fn link(env: &mut Env, decls: &Vec<Declare>) {
    for decl in decls {
        match decl {
            Declare::Define { name, def: Some(def), .. } => {
                match def.as_ref() {
                    Thunk(def, _) => env.insert(
                        name.name().to_owned(),
                        Rc::new(ZValue::Thunk(
                            Rc::new(def.as_ref().to_owned().into()),
                            Some(Env::new()),
                        )),
                    ),
                    _ => unreachable!(),
                }
            }
            _ => (),
        }
    }
}
