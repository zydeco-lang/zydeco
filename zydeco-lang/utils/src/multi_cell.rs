use std::ops::AddAssign;

pub struct MultiCell<T> {
    cells: Vec<T>,
}

impl<T> MultiCell<T> {
    pub fn new() -> Self {
        Self { cells: Vec::new() }
    }
    pub fn once(&mut self, init: impl FnOnce() -> T) -> &T {
        if self.cells.is_empty() {
            self.cells.push(init())
        }
        self.cells.first().unwrap()
    }
    pub fn is_empty(&self) -> bool {
        self.cells.is_empty()
    }
    pub fn init(&mut self, value: T) -> &T {
        assert!(self.cells.is_empty());
        self.cells.push(value);
        self.cells.first().unwrap()
    }
    pub fn get(&self) -> &T {
        self.cells.first().unwrap()
    }
    pub fn all(&self) -> &[T] {
        &self.cells
    }
    pub fn contains(&self, value: &T) -> bool
    where
        T: PartialEq,
    {
        self.cells.contains(value)
    }
}

impl<T> Default for MultiCell<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> AddAssign<MultiCell<T>> for MultiCell<T> {
    fn add_assign(&mut self, rhs: MultiCell<T>) {
        self.cells.extend(rhs.cells);
    }
}
