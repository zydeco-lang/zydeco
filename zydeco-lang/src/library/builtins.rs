use super::{impls::*, syntax::*};
use crate::{rc, utils::span::span};
use std::collections::HashMap;

struct Builtin {
    name: &'static str,
    arity: u64,
    behavior: Box<PrimComp>,
}

impl Builtin {
    pub fn new(name: &'static str, arity: u64, behavior: PrimComp) -> Self {
        Builtin { name, arity, behavior: Box::new(behavior) }
    }
    pub fn gen(self) -> (TermV, TermValue) {
        let Builtin { name, arity, behavior } = self;
        (
            TermV::new(name.to_string(), span(0, 0)),
            Thunk(rc!(Prim { arity, body: *behavior }.into())).into(),
        )
    }
}

// To add new builtin functions, provide impl and add declaration to std.zydeco
pub (super) fn std_library() -> HashMap<TermV, TermValue> {
    [
        Builtin::new("add", 2, add),
        Builtin::new("sub", 2, sub),
        Builtin::new("mul", 2, mul),
        Builtin::new("div", 2, div),
        Builtin::new("mod", 2, modulo),
        Builtin::new("int_eq", 2, int_eq),
        Builtin::new("int_lt", 2, int_lt),
        Builtin::new("int_gt", 2, int_gt),
        Builtin::new("str_length", 1, str_length),
        Builtin::new("str_append", 2, str_append),
        Builtin::new("str_eq", 2, str_eq),
        Builtin::new("str_index", 2, str_index),
        Builtin::new("int_to_str", 1, int_to_str),
        Builtin::new("char_to_str", 1, char_to_str),
        Builtin::new("char_to_int", 1, char_to_int),
        Builtin::new("str_to_int", 1, str_to_int),
        Builtin::new("write_str", 2, write_str),
        Builtin::new("read_line", 1, read_line),
        Builtin::new("exit", 1, exit),
        Builtin::new("arg_list", 1, arg_list),
    ]
    .into_iter()
    .map(Builtin::gen)
    .collect()
}

// pub fn link_builtin(env: &mut Env) {
//     for builtin in std_library() {
//         env.insert(
//             builtin.name.to_string(),
//             Rc::new(ZValue::Thunk(
//                 Rc::new({
//                     let arity = builtin.arity;
//                     let body = *builtin.behavior;
//                     ZCompute::Prim { arity, body }
//                 }),
//                 Some(Env::new()),
//             )),
//         );
//     }
// }

// use super::syntax::*;
// use crate::{
//     parse::{
//         err::ParseError, parser::DeclarationVecParser, syntax as ps, Lexer,
//     },
//     statics::{err::TypeCheckError, syntax as ss},
//     utils::span::{span, FileInfo, SpanHolder},
// };
// use indexmap::IndexMap;
// use std::{path::PathBuf, rc::Rc};

// pub fn std_decls() -> Result<IndexMap<TermV, TermValue>, String> {
//     // Static linking. Resolve std library at compile time.
//     // Probably won't work in the future if there are more library files to include.
//     let std = include_str!("std.zydeco");
//     let path_rc = Rc::new(PathBuf::from("zydeco-lang/src/library/std.zydeco"));
//     let file_info = FileInfo::new(&std, path_rc.clone());
//     let declarations = DeclarationVecParser::new()
//         .parse(&std, Lexer::new(&std))
//         .map_err(|e| format!("{}", ParseError(e, &file_info)))?
//         .span_map(|span| {
//             span.set_info(&file_info);
//         });
//     let m: ss::Module = ps::Module {
//         name: Some("Std".into()),
//         declarations,
//         entry: span(0, 0).make(
//             Ret(Box::new(span(0, 0).make(Literal::Int(0).into()))).into(),
//         ),
//     }
//     .try_into()
//     .map_err(|e: TypeCheckError| e.to_string())?;
//     let mut map = IndexMap::new();
//     for DeclSymbol {
//         public: _,
//         external: _,
//         inner: Define { name: (var, _ty), def: () },
//     } in m.define_ext
//     {
//         todo!()
//     }
//     for DeclSymbol { public: _, external: _, inner: Define { name, def } } in
//         m.define
//     {
//         map.insert(name, def.inner_ref().into());
//     }
//     Ok(map)
// }
