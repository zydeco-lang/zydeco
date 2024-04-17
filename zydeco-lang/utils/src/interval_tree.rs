pub struct IntervalTree<T>
where
    T: std::cmp::Ord,
{
    start: T,
    end: T,
    max: T,
    left: Option<Box<IntervalTree<T>>>,
    right: Option<Box<IntervalTree<T>>>,
}

impl<T> IntervalTree<T>
where
    T: std::cmp::Ord + Copy,
{
    pub fn new(start: T, end: T) -> Self {
        IntervalTree { start, end, max: end, left: None, right: None }
    }

    pub fn insert(&mut self, start: T, end: T) {
        if start < self.start {
            if let Some(left) = &mut self.left {
                left.insert(start, end);
            } else {
                self.left = Some(Box::new(IntervalTree::new(start, end)));
            }
        } else {
            if let Some(right) = &mut self.right {
                right.insert(start, end);
            } else {
                self.right = Some(Box::new(IntervalTree::new(start, end)));
            }
        }
        self.max = std::cmp::max(self.max, end);
    }

    pub fn query(&self, point: T) -> Vec<(T, T)> {
        let mut result = Vec::new();
        if point < self.start {
            if let Some(left) = &self.left {
                result.extend(left.query(point));
            }
        } else if point <= self.end {
            result.push((self.start, self.end));
            if let Some(left) = &self.left {
                result.extend(left.query(point));
            }
            if let Some(right) = &self.right {
                result.extend(right.query(point));
            }
        } else {
            if let Some(right) = &self.right {
                if point <= right.max {
                    result.extend(right.query(point));
                }
            }
        }
        result
    }
}
