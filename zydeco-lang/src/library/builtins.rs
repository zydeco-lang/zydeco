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
