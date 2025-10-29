//! Dump scoped or statics entities in the type checker.

use crate::*;

mod scoped {
    use super::*;
    use zydeco_surface::scoped::fmt::*;

    impl Tycker {
        pub fn ugly_scoped<T>(&self, item: T) -> String
        where
            T: for<'f> Ugly<'f, Formatter<'f>>,
        {
            let fmt = Formatter::new(&self.scoped);
            let res = item.ugly(&fmt);
            format!("{}", res)
        }

        // pub fn pretty_scoped<T>(&self, item: T) -> String
        // where
        //     T: for<'f> Pretty<'f, Formatter<'f>>,
        // {
        //     let res = {
        //         let fmt = Formatter::new(&self.scoped);
        //         item.pretty(&fmt)
        //     };
        //     format!("{}", res)
        // }
    }
}

mod statics {
    use super::*;
    use tyck::fmt::*;

    impl Tycker {
        pub fn ugly_statics<T>(&self, item: T) -> String
        where
            T: for<'f> Ugly<'f, Formatter<'f>>,
        {
            let fmt = Formatter::new(&self.scoped, &self.statics);
            let res = item.ugly(&fmt);
            format!("{}", res)
        }

        pub fn pretty_statics<T>(&self, item: T) -> String
        where
            T: for<'f> Pretty<'f, Formatter<'f>>,
        {
            let fmt = Formatter::new(&self.scoped, &self.statics);
            let res = item.pretty(&fmt);
            let mut buf = String::new();
            res.render_fmt(100, &mut buf).unwrap();
            buf
        }

        pub fn pretty_statics_nested<T>(&self, item: T, prefix: &str) -> String
        where
            T: for<'f> Pretty<'f, Formatter<'f>>,
        {
            let fmt = Formatter::new(&self.scoped, &self.statics);
            let res = item.pretty(&fmt);
            let mut buf = String::new();
            buf += "\n";
            res.render_fmt(100, &mut buf).unwrap();
            buf.replace("\n", &format!("\n{}", prefix))
        }
    }
}
