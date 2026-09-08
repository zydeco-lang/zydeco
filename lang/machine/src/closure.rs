//! Closure field order shared by ZASM lowering and runtime-created closures.

// Declare the physical record and its compiler-side traversal together. Changing
// the field order updates both the Rust layout and the sequence ZASM packs.
macro_rules! closure_record {
    ($($field:ident => $variant:ident),* $(,)?) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(C)]
        pub struct Closure<W> { $(pub $field: W),* }

        impl<W> Closure<W> {
            pub const WORDS: usize = [$(stringify!($field)),*].len();

            pub fn into_words(self) -> [W; { [$(stringify!($field)),*].len() }] {
                [$(self.$field),*]
            }
        }

        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum ClosureField { $($variant),* }

        impl ClosureField {
            /// Byte displacement in an AMD64 record, independent of the compiler host.
            pub const fn offset(self) -> usize {
                match self {
                    $(Self::$variant => core::mem::offset_of!(Closure<u64>, $field)),*
                }
            }
        }
    };
}

closure_record! { environment => Environment, code => Code }
