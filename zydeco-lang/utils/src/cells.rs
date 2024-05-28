use std::ops::AddAssign;

pub struct SingCell<T> {
    cell: Option<T>,
}

impl<T> SingCell<T> {
    pub fn new() -> Self {
        Self { cell: None }
    }
    pub fn init_or_get(&mut self, init: impl FnOnce() -> T) -> &T {
        if self.cell.is_none() {
            self.cell = Some(init())
        }
        self.cell.as_ref().unwrap()
    }
    pub fn init_or_else<E>(
        &mut self, init: impl FnOnce() -> T, err: impl FnOnce(&T) -> E,
    ) -> Result<&T, E> {
        match &self.cell {
            | None => {
                self.cell = Some(init());
                Ok(self.cell.as_ref().unwrap())
            }
            | Some(t) => Err(err(t)),
        }
    }
    pub fn once_or_else<E>(&self, err: impl FnOnce() -> E) -> Result<&T, E> {
        match &self.cell {
            | None => Err(err()),
            | Some(t) => Ok(t),
        }
    }
    pub fn unwrap(&self) -> &T {
        self.cell.as_ref().unwrap()
    }
}

impl<T> Default for SingCell<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct MultiCell<T> {
    cells: Vec<T>,
}

impl<T> MultiCell<T> {
    pub fn new() -> Self {
        Self { cells: Vec::new() }
    }
    pub fn is_empty(&self) -> bool {
        self.cells.is_empty()
    }
    pub fn extend_one(&mut self, value: T) -> &T {
        self.cells.push(value);
        self.cells.last().unwrap()
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

impl<T> AddAssign for MultiCell<T> {
    fn add_assign(&mut self, rhs: MultiCell<T>) {
        self.cells.extend(rhs.cells);
    }
}
