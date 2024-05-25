use std::hash::Hash;
use zydeco_utils::arena::IndexLike;

pub trait DefPtr: IndexLike<Meta = usize> + Eq + Hash {}
pub trait PatPtr: IndexLike<Meta = usize> + Eq + Hash {}
pub trait CoPatPtr: IndexLike<Meta = usize> + Eq + Hash {}
pub trait TermPtr: IndexLike<Meta = usize> + Eq + Hash {}
pub trait DeclPtr: IndexLike<Meta = usize> + Eq + Hash {}
