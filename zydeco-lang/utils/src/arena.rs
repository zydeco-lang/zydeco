use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

#[derive(Debug)]
pub struct IndexAlloc<Meta>(Meta, usize);
impl<Meta: Copy> Iterator for IndexAlloc<Meta> {
    type Item = (Meta, usize);
    fn next(&mut self) -> Option<Self::Item> {
        let IndexAlloc(meta, ref mut idx) = self;
        let old = *idx;
        *idx += 1;
        Some((*meta, old))
    }
}

pub unsafe trait IndexLike {
    type Meta;
    fn new(meta: Self::Meta, idx: usize) -> Self;
    fn index(&self) -> usize;
}

#[derive(Debug)]
pub struct ArenaDense<Id, T, Meta = usize> {
    allocator: IndexAlloc<Meta>,
    vec: Vec<T>,
    _marker: std::marker::PhantomData<Id>,
}

#[derive(Debug)]
pub struct ArenaSparse<Id, T, Meta = usize> {
    allocator: IndexAlloc<Meta>,
    map: HashMap<usize, T>,
    _marker: std::marker::PhantomData<Id>,
}

#[derive(Debug)]
pub struct ArenaAssoc<Id, T> {
    map: HashMap<Id, T>,
}

pub trait ArenaLike<Id, T, Meta> {
    fn new(allocator: IndexAlloc<Meta>) -> Self;
    fn alloc(&mut self, val: T) -> Id;
}

pub trait ArenaAccess<Id, T, Meta>: Index<Id, Output = T> + IndexMut<Id, Output = T> {
    fn get(&self, id: Id) -> Option<&T>;
    fn get_mut(&mut self, id: Id) -> Option<&mut T>;
}

pub struct GlobalAlloc(IndexAlloc<()>);
impl GlobalAlloc {
    pub fn new() -> Self {
        GlobalAlloc(IndexAlloc((), 0))
    }
    pub fn alloc(&mut self) -> IndexAlloc<usize> {
        IndexAlloc(self.0.next().unwrap().1, 0)
    }
}

mod impls {
    use super::*;

    impl<Id, T, Meta> Index<Id> for ArenaDense<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta>,
    {
        type Output = T;
        fn index(&self, id: Id) -> &Self::Output {
            self.get(id).unwrap()
        }
    }
    impl<Id, T, Meta> IndexMut<Id> for ArenaDense<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta>,
    {
        fn index_mut(&mut self, id: Id) -> &mut Self::Output {
            self.get_mut(id).unwrap()
        }
    }

    impl<Id, T, Meta> ArenaLike<Id, T, Meta> for ArenaDense<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta>,
    {
        fn new(allocator: IndexAlloc<Meta>) -> Self {
            ArenaDense { allocator, vec: Vec::new(), _marker: std::marker::PhantomData }
        }
        fn alloc(&mut self, val: T) -> Id {
            let id = self.allocator.next().unwrap();
            self.vec.push(val);
            IndexLike::new(id.0, id.1)
        }
    }

    impl<Id, T, Meta> ArenaAccess<Id, T, Meta> for ArenaDense<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta>,
    {
        fn get(&self, id: Id) -> Option<&T> {
            self.vec.get(id.index())
        }
        fn get_mut(&mut self, id: Id) -> Option<&mut T> {
            self.vec.get_mut(id.index())
        }
    }

    impl<Id, T, Meta> Index<Id> for ArenaSparse<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta> + Eq + std::hash::Hash,
    {
        type Output = T;
        fn index(&self, id: Id) -> &Self::Output {
            self.get(id).unwrap()
        }
    }

    impl<Id, T, Meta> IndexMut<Id> for ArenaSparse<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta> + Eq + std::hash::Hash,
    {
        fn index_mut(&mut self, id: Id) -> &mut Self::Output {
            self.get_mut(id).unwrap()
        }
    }

    impl<Id, T, Meta> ArenaLike<Id, T, Meta> for ArenaSparse<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta> + Eq + std::hash::Hash,
    {
        fn new(allocator: IndexAlloc<Meta>) -> Self {
            ArenaSparse { allocator, map: HashMap::new(), _marker: std::marker::PhantomData }
        }
        fn alloc(&mut self, val: T) -> Id {
            let id = self.allocator.next().unwrap();
            self.map.insert(id.1, val);
            IndexLike::new(id.0, id.1)
        }
    }

    impl<Id, T, Meta> ArenaAccess<Id, T, Meta> for ArenaSparse<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta> + Eq + std::hash::Hash,
    {
        fn get(&self, id: Id) -> Option<&T> {
            self.map.get(&id.index())
        }
        fn get_mut(&mut self, id: Id) -> Option<&mut T> {
            self.map.get_mut(&id.index())
        }
    }

    impl<Id, T> Index<Id> for ArenaAssoc<Id, T>
    where
        Id: Eq + std::hash::Hash,
    {
        type Output = T;
        fn index(&self, id: Id) -> &Self::Output {
            self.get(id).unwrap()
        }
    }

    impl<Id, T> IndexMut<Id> for ArenaAssoc<Id, T>
    where
        Id: Eq + std::hash::Hash,
    {
        fn index_mut(&mut self, id: Id) -> &mut Self::Output {
            self.get_mut(id).unwrap()
        }
    }

    impl<Id, T> ArenaAccess<Id, T, ()> for ArenaAssoc<Id, T>
    where
        Id: Eq + std::hash::Hash,
    {
        fn get(&self, id: Id) -> Option<&T> {
            self.map.get(&id)
        }
        fn get_mut(&mut self, id: Id) -> Option<&mut T> {
            self.map.get_mut(&id)
        }
    }
}

#[macro_export]
macro_rules! new_key_type {
    ( $(#[$outer:meta])* $vis:vis struct $name:ident; $($rest:tt)* ) => {
        $(#[$outer])*
        #[derive(Copy, Clone, Default,
                    Eq, PartialEq, Ord, PartialOrd,
                    Hash, Debug)]
        $vis struct $name(usize, usize);

        unsafe impl $crate::arena::IndexLike for $name {
            type Meta = usize;
            fn new(meta: Self::Meta, idx: usize) -> Self {
                Self(meta, idx)
            }
            fn index(&self) -> usize {
                self.1
            }
        }

        $crate::new_key_type!($($rest)*);
    };

    () => {}
}

// macro_rules! new_key_type {
//     ( $(#[$outer:meta])* $vis:vis struct $name:ident; $($rest:tt)* ) => {
//         $(#[$outer])*
//         #[derive(Copy, Clone, Default,
//                     Eq, PartialEq, Ord, PartialOrd,
//                     Hash, Debug)]
//         #[repr(transparent)]
//         $vis struct $name($crate::KeyData);

//         impl From<$crate::KeyData> for $name {
//             fn from(k: $crate::KeyData) -> Self {
//                 $name(k)
//             }
//         }

//         unsafe impl $crate::Key for $name {
//             fn data(&self) -> $crate::KeyData {
//                 self.0
//             }
//         }

//         $crate::__serialize_key!($name);

//         $crate::new_key_type!($($rest)*);
//     };

//     () => {}
// }