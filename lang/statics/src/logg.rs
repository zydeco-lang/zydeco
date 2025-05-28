pub use ::log::*;

use crate::*;

// #[cfg(debug_assertions)]
mod scoped {
    use super::*;
    use zydeco_surface::scoped::fmt::*;

    impl Tycker {
        pub fn dump_scoped<T>(&self, item: T) -> String
        where
            T: for<'f> Ugly<'f, Formatter<'f>>,
        {
            let res = {
                let fmt = Formatter::new(&self.scoped);
                item.ugly(&fmt)
            };
            format!("{}", res)
        }
    }
}

// #[cfg(debug_assertions)]
mod statics {
    use super::*;
    use crate::fmt::*;

    impl Tycker {
        pub fn dump_statics<T>(&self, item: T) -> String
        where
            T: for<'f> Ugly<'f, Formatter<'f>>,
        {
            let res = {
                let fmt = Formatter::new(&self.scoped, &self.statics);
                item.ugly(&fmt)
            };
            format!("{}", res)
        }
    }
}
