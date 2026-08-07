use crate::syntax::*;
use std::{collections::HashMap, sync::LazyLock};
use zydeco_syntax::BuiltinValueRole;

/// Mapping from builtin names to their primitive implementations.
pub static BUILTINS: LazyLock<HashMap<&'static str, Prim>> = {
    LazyLock::new(|| {
        use crate::impls::*;
        [
            Builtin::new("add", 2, add),
            Builtin::new("sub", 2, sub),
            Builtin::new("mul", 2, mul),
            Builtin::new("div", 2, div),
            Builtin::new("mod", 2, modulo),
            Builtin::new("int_eq_branch", 4, int_eq_branch),
            Builtin::new("int_lt_branch", 4, int_lt_branch),
            Builtin::new("int_gt_branch", 4, int_gt_branch),
            Builtin::new("float_add", 2, float_add),
            Builtin::new("float_sub", 2, float_sub),
            Builtin::new("float_mul", 2, float_mul),
            Builtin::new("float_div", 2, float_div),
            Builtin::new("float_eq_branch", 4, float_eq_branch),
            Builtin::new("float_lt_branch", 4, float_lt_branch),
            Builtin::new("float_gt_branch", 4, float_gt_branch),
            Builtin::new("float_to_str", 1, float_to_str),
            Builtin::new("str_scalar_length", 1, str_scalar_length),
            Builtin::new("str_byte_length", 1, str_byte_length),
            Builtin::new("str_append", 2, str_append),
            Builtin::new("str_split_once_branch", 4, str_split_once_branch),
            Builtin::new("str_split_at_branch", 4, str_split_at_branch),
            Builtin::new("str_eq_branch", 4, str_eq_branch),
            Builtin::new("str_get_branch", 4, str_get_branch),
            Builtin::new("int_to_str", 1, int_to_str),
            Builtin::new("char_to_str", 1, char_to_str),
            Builtin::new("char_codepoint", 1, char_codepoint),
            Builtin::new("char_from_codepoint_branch", 3, char_from_codepoint_branch),
            Builtin::new("str_parse_int_branch", 3, str_parse_int_branch),
            Builtin::new("bytes_empty", 0, bytes_empty),
            Builtin::new("bytes_length", 1, bytes_length),
            Builtin::new("bytes_append", 2, bytes_append),
            Builtin::new("bytes_from_str", 1, bytes_from_str),
            Builtin::new("bytes_to_str_branch", 3, bytes_to_str_branch),
            Builtin::new("stdin", 0, stdin),
            Builtin::new("stdout", 0, stdout),
            Builtin::new("stderr", 0, stderr),
            Builtin::new("io_read", 4, io_read),
            Builtin::new("io_read_line", 4, io_read_line),
            Builtin::new("io_read_all", 3, io_read_all),
            Builtin::new("io_write_all", 4, io_write_all),
            Builtin::new("io_flush", 3, io_flush),
            Builtin::new("io_close_reader", 3, io_close_reader),
            Builtin::new("io_close_writer", 3, io_close_writer),
            Builtin::new("fs_open_reader", 3, fs_open_reader),
            Builtin::new("fs_create_writer", 3, fs_create_writer),
            Builtin::new("fs_append_writer", 3, fs_append_writer),
            Builtin::new("write_str", 2, write_str),
            Builtin::new("write_int", 2, write_int),
            Builtin::new("write_line", 2, write_line),
            Builtin::new("read_line", 1, read_line),
            Builtin::new("read_line_as_int_branch", 2, read_line_as_int_branch),
            Builtin::new("read_till_eof", 1, read_till_eof),
            Builtin::new("arg_fold", 2, arg_fold),
            Builtin::new("random_int", 1, random_int),
            Builtin::new("exit", 1, exit),
        ]
        .into_iter()
        .map(Builtin::generate)
        .collect()
    })
};

/// Metadata used to build a `Prim` entry for the builtin registry.
pub struct Builtin {
    name: &'static str,
    arity: u64,
    behavior: PrimComp,
}

impl Builtin {
    /// Describe a new builtin operation.
    fn new(name: &'static str, arity: u64, behavior: PrimComp) -> Self {
        Builtin { name, arity, behavior }
    }
    /// Convert the builtin metadata into a `Prim` entry for the registry.
    fn generate(self) -> (&'static str, Prim) {
        let Builtin { name, arity, behavior } = self;
        (name, Prim { arity, body: behavior })
    }
    // fn generate(self) -> (&'static str, RcValue) {
    //     let Builtin { name, arity, behavior } = self;
    //     let prim = Prim { arity, body: *behavior }.into();
    //     let thunk = Thunk(Rc::new(prim)).into();
    //     (name, Rc::new(thunk))
    // }
}

/// Typed access to host-operation values used to construct the Builtin
/// package.
pub struct BuiltinRuntime;

impl BuiltinRuntime {
    pub fn package_value(role: BuiltinValueRole) -> Option<RcValue> {
        Self::named_value(role.host_name())
    }

    fn named_value(name: &str) -> Option<RcValue> {
        let primitive: Computation = BUILTINS.get(name)?.to_owned().into();
        Some(std::rc::Rc::new(Thunk(std::rc::Rc::new(primitive)).into()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_builtin_role_has_an_interpreter_package_implementation() {
        let missing = BuiltinValueRole::ALL
            .iter()
            .copied()
            .filter(|role| BuiltinRuntime::package_value(*role).is_none())
            .collect::<Vec<_>>();

        assert!(missing.is_empty(), "missing interpreter Builtin roles: {missing:?}");
    }
}
