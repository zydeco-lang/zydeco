use crate::*;
use std::fmt::Display;

// #[cfg(debug_assertions)]
mod scoped {
    use super::*;
    use zydeco_surface::scoped::fmt::*;

    impl Tycker {
        pub fn log_scoped<S, T>(&self, msg: S, item: T)
        where
            S: Display,
            T: for<'f> Ugly<'f, Formatter<'f>>,
        {
            let res = {
                let fmt = Formatter::new(&self.scoped);
                item.ugly(&fmt)
            };
            ::log::trace!("[{}] {}", msg, res);
        }
    }
}

// #[cfg(debug_assertions)]
mod statics {
    use super::*;
    use crate::fmt::*;

    impl Tycker {
        pub fn log_statics<S, T>(&self, msg: S, item: T)
        where
            S: Display,
            T: for<'f> Ugly<'f, Formatter<'f>>,
        {
            let res = {
                let fmt = Formatter::new(&self.scoped, &self.statics);
                item.ugly(&fmt)
            };
            ::log::trace!("[{}] {}", msg, res);
        }
    }
}
