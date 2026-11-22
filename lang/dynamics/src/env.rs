//! Persistent 32-way bitmap-indexed radix trie keyed by dense variable IDs.

use std::rc::Rc;
use zydeco_ir::{DenseId, VarId};

const BITS: u32 = 5;
const MASK: u32 = (1 << BITS) - 1;

#[derive(Clone, Debug)]
enum Node<V: Clone> {
    Leaf { key: u32, value: V },
    Branch { bitmap: u32, children: Vec<Rc<Node<V>>> },
}

/// Persistent lexical environment mapping [`VarId`] to runtime [`SemValue`](crate::syntax::SemValue).
///
/// Capturing the environment is O(1) via structural sharing, while updates reuse
/// uniquely owned nodes in place via [`Rc::make_mut`].
#[derive(Clone, Debug, Default)]
pub struct EvalEnv<V: Clone> {
    root: Option<Rc<Node<V>>>,
}

impl<V: Clone> EvalEnv<V> {
    pub fn new() -> Self {
        Self { root: None }
    }

    pub fn get(&self, var: VarId) -> Option<&V> {
        let key = u32::try_from(var.index()).expect("VarId index fits in u32");
        let mut node = self.root.as_deref()?;
        let mut shift = 0u32;
        loop {
            match node {
                | Node::Leaf { key: existing, value } => {
                    return (*existing == key).then_some(value);
                }
                | Node::Branch { bitmap, children } => {
                    let bit = 1u32 << ((key >> shift) & MASK);
                    if (*bitmap & bit) == 0 {
                        return None;
                    }
                    let index = (*bitmap & (bit - 1)).count_ones() as usize;
                    node = &children[index];
                    shift += BITS;
                }
            }
        }
    }

    pub fn insert(&mut self, var: VarId, value: V) {
        let key = u32::try_from(var.index()).expect("VarId index fits in u32");
        let Some(mut cursor) = self.root.as_mut() else {
            self.root = Some(Rc::new(Node::Leaf { key, value }));
            return;
        };
        let mut shift = 0u32;
        loop {
            match &**cursor {
                | Node::Leaf { key: existing, .. } if *existing == key => {
                    let Node::Leaf { value: slot, .. } = Rc::make_mut(cursor) else {
                        unreachable!();
                    };
                    *slot = value;
                    return;
                }
                | Node::Leaf { key: existing, .. } => {
                    let existing = *existing;
                    let old_leaf = Rc::clone(cursor);
                    let new_leaf = Rc::new(Node::Leaf { key, value });
                    *cursor = split_leaves(existing, old_leaf, key, new_leaf, shift);
                    return;
                }
                | Node::Branch { .. } => {
                    let Node::Branch { bitmap, children } = Rc::make_mut(cursor) else {
                        unreachable!();
                    };
                    let bit = 1u32 << ((key >> shift) & MASK);
                    let index = (*bitmap & (bit - 1)).count_ones() as usize;
                    if (*bitmap & bit) == 0 {
                        *bitmap |= bit;
                        children.insert(index, Rc::new(Node::Leaf { key, value }));
                        return;
                    }
                    cursor = &mut children[index];
                    shift += BITS;
                }
            }
        }
    }
}

fn split_leaves<V: Clone>(
    existing_key: u32, old_leaf: Rc<Node<V>>, new_key: u32, new_leaf: Rc<Node<V>>, start_shift: u32,
) -> Rc<Node<V>> {
    let mut shifts = Vec::new();
    let mut shift = start_shift;
    loop {
        let frag_a = (existing_key >> shift) & MASK;
        let frag_b = (new_key >> shift) & MASK;
        if frag_a != frag_b {
            let bit_a = 1u32 << frag_a;
            let bit_b = 1u32 << frag_b;
            let children =
                if frag_a < frag_b { vec![old_leaf, new_leaf] } else { vec![new_leaf, old_leaf] };
            let mut node = Rc::new(Node::Branch { bitmap: bit_a | bit_b, children });
            for prefix_frag in shifts.into_iter().rev() {
                node = Rc::new(Node::Branch { bitmap: 1u32 << prefix_frag, children: vec![node] });
            }
            return node;
        }
        shifts.push(frag_a);
        shift += BITS;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert_shadow_and_clone_preserve_lexical_snapshots() {
        let mut env = EvalEnv::new();
        let a = VarId::from_index(0);
        let b = VarId::from_index(32); // collides at bottom 5 bits (0) with `a`
        let c = VarId::from_index(1024); // collides at two levels (0, 0) with `a`

        env.insert(a, 10);
        env.insert(b, 20);
        let snapshot = env.clone();
        env.insert(a, 99);
        env.insert(c, 30);

        assert_eq!(snapshot.get(a), Some(&10));
        assert_eq!(snapshot.get(b), Some(&20));
        assert_eq!(snapshot.get(c), None);

        assert_eq!(env.get(a), Some(&99));
        assert_eq!(env.get(b), Some(&20));
        assert_eq!(env.get(c), Some(&30));
    }

    #[test]
    fn handles_dense_population_without_collisions_lost() {
        let mut env = EvalEnv::new();
        for i in 0..2_000usize {
            env.insert(VarId::from_index(i), i * 3 + 1);
        }
        let cloned = env.clone();
        env.insert(VarId::from_index(777), 0);
        for i in 0..2_000usize {
            assert_eq!(cloned.get(VarId::from_index(i)), Some(&(i * 3 + 1)));
            let expected = if i == 777 { 0 } else { i * 3 + 1 };
            assert_eq!(env.get(VarId::from_index(i)), Some(&expected));
        }
    }
}
