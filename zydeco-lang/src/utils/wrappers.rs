#[macro_export]
#[allow(clippy::double_parens)]
macro_rules! rc {
    ($($x:expr),+) => {
        { ($(std::rc::Rc::new($x)),+) }
    };
}
