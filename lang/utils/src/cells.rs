/// An option wrapper that can be set once and then read many times.
///
/// `init` meaning the cell is empty and we're inserting the value
///
/// `get` meaning the cell is not empty and we're reading the value
pub struct SingCell<T> {
    cell: Option<T>,
}

impl<T> SingCell<T> {
    pub fn new() -> Self {
        Self { cell: None }
    }
    /// Initialize the cell if it is empty, and return a reference to the value.
    /// If the cell is not empty, return an error.
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
    /// Get the value if it is not empty, otherwise return an error.
    pub fn get_or_else<E>(&self, err: impl FnOnce() -> E) -> Result<&T, E> {
        match &self.cell {
            | None => Err(err()),
            | Some(t) => Ok(t),
        }
    }
    /// Get the value if it is not empty, otherwise panic.
    pub fn get(&self) -> &T {
        self.cell.as_ref().unwrap()
    }
}

impl<T> Default for SingCell<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Default, Clone)]
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

impl<T> std::ops::AddAssign for MultiCell<T> {
    fn add_assign(&mut self, rhs: MultiCell<T>) {
        self.cells.extend(rhs.cells);
    }
}
