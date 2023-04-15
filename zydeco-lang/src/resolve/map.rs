use crate::syntax::binder::*;
use slotmap::SlotMap;
use im::HashMap;
use std::ops::{Deref, DerefMut};

pub struct NameMap<'a> {
    pub map: HashMap<NameView<'a>, EntityId>,
}

impl<'a> Deref for NameMap<'a> {
    type Target = HashMap<NameView<'a>, EntityId>;

    fn deref(&self) -> &Self::Target {
        &self.map
    }
}

impl<'a> DerefMut for NameMap<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.map
    }
}

pub struct EntityMap<Sort> {
    pub map: SlotMap<EntityId, Entity<Sort>>,
}

impl<Sort> Deref for EntityMap<Sort> {
    type Target = SlotMap<EntityId, Entity<Sort>>;

    fn deref(&self) -> &Self::Target {
        &self.map
    }
}

impl<Sort> DerefMut for EntityMap<Sort> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.map
    }
}
