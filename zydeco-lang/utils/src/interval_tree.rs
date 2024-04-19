pub struct IntervalTree<I>
where
    I: std::cmp::Ord,
{
    start: I,
    end: I,
    max: I,
    left: Option<Box<IntervalTree<I>>>,
    right: Option<Box<IntervalTree<I>>>,
}

impl<I> IntervalTree<I>
where
    I: std::cmp::Ord + Copy,
{
    pub fn new((start, end): (I, I)) -> Self {
        IntervalTree { start, end, max: end, left: None, right: None }
    }

    pub fn insert(&mut self, (start, end): (I, I)) {
        let range = (start, end);
        if start < self.start {
            if let Some(left) = &mut self.left {
                left.insert(range);
            } else {
                self.left = Some(Box::new(IntervalTree::new(range)));
            }
        } else {
            if let Some(right) = &mut self.right {
                right.insert(range);
            } else {
                self.right = Some(Box::new(IntervalTree::new(range)));
            }
        }
        self.max = std::cmp::max(self.max, end);
    }

    pub fn query(&self, point: I) -> Vec<(I, I)> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn insert() {
        let mut tr = IntervalTree::new((0, 255));
        tr.insert((0, 127));
        tr.insert((0, 63));
        tr.insert((64, 127));
        tr.insert((128, 255));
        {
            let res = tr.query(120).into_iter().collect::<HashSet<_>>();
            let correct = [(0, 255), (0, 127), (64, 127)].into_iter().collect::<HashSet<_>>();
            assert_eq!(res, correct)
        }
        {
            let res = tr.query(64).into_iter().collect::<HashSet<_>>();
            let correct = [(0, 255), (0, 127), (64, 127)].into_iter().collect::<HashSet<_>>();
            assert_eq!(res, correct)
        }
        {
            let res = tr.query(0).into_iter().collect::<HashSet<_>>();
            let correct = [(0, 255), (0, 127), (64, 127)].into_iter().collect::<HashSet<_>>();
            assert_eq!(res, correct)
        }
        {
            let res = tr.query(240).into_iter().collect::<HashSet<_>>();
            let correct = [(0, 255), (128, 255)].into_iter().collect::<HashSet<_>>();
            assert_eq!(res, correct)
        }
    }
}
