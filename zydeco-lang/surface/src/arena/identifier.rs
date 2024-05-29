use std::hash::Hash;
use zydeco_utils::arena::{IndexLike, TrivId};

pub trait DefPtr: IndexLike<Meta = usize> + Eq + Hash {}
pub trait PatPtr: IndexLike<Meta = usize> + Eq + Hash {}
pub trait CoPatPtr: IndexLike<Meta = usize> + Eq + Hash {}
pub trait TermPtr: IndexLike<Meta = usize> + Eq + Hash {}
pub trait DeclPtr: IndexLike<Meta = usize> + Eq + Hash {}

impl DefPtr for TrivId {}
impl PatPtr for TrivId {}
impl CoPatPtr for TrivId {}
impl TermPtr for TrivId {}
impl DeclPtr for TrivId {}
