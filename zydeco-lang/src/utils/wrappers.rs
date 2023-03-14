#[macro_export]
macro_rules! rc {
    ($($x:expr),+) => {
        ($(std::rc::Rc::new($x)),+)
    };
}
