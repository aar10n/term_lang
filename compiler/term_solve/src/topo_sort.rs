use std::collections::{btree_map::Entry, BTreeMap, BTreeSet};
use std::hash::Hash;

#[derive(Clone, Debug)]
pub(crate) struct Dependency<T> {
    pub num_prec: usize,
    pub succ: BTreeSet<T>,
}

impl<T: Ord + Eq> Dependency<T> {
    fn new() -> Self {
        Self {
            num_prec: 0,
            succ: BTreeSet::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TopologicalSort<T> {
    pub(crate) top: BTreeMap<T, Dependency<T>>,
}

impl<T> Default for TopologicalSort<T> {
    fn default() -> TopologicalSort<T> {
        TopologicalSort {
            top: BTreeMap::new(),
        }
    }
}

impl<T: Clone + Ord + Eq> TopologicalSort<T> {
    pub fn new() -> TopologicalSort<T> {
        Default::default()
    }

    pub fn len(&self) -> usize {
        self.top.len()
    }

    pub fn is_empty(&self) -> bool {
        self.top.is_empty()
    }

    pub fn declare_item(&mut self, item: impl Into<T>) {
        match self.top.entry(item.into()) {
            Entry::Vacant(e) => {
                let dep = Dependency::new();
                e.insert(dep);
            }
            Entry::Occupied(_) => {}
        }
    }

    pub fn add_dependency(&mut self, dependent: impl Into<T>, dependee: impl Into<T>) {
        let prec = dependent.into();
        let succ = dependee.into();
        let recursive = prec == succ;

        match self.top.entry(prec) {
            Entry::Vacant(e) => {
                let mut dep = Dependency::new();
                if !recursive {
                    dep.succ.insert(succ.clone());
                }
                e.insert(dep);
            }
            Entry::Occupied(e) => {
                let mut dep = e.into_mut();
                if !dep.succ.insert(succ.clone()) {
                    return;
                }
            }
        }

        if recursive {
            return;
        }

        match self.top.entry(succ) {
            Entry::Vacant(e) => {
                let mut dep = Dependency::new();
                dep.num_prec = 1;
                e.insert(dep);
            }
            Entry::Occupied(e) => {
                e.into_mut().num_prec += 1;
            }
        }
    }

    pub fn pop(&mut self) -> Vec<T> {
        let mut keys = self
            .top
            .iter()
            .filter(|&(_, v)| v.num_prec == 0)
            .map(|(k, _)| k.clone())
            .collect::<Vec<_>>();
        for k in &keys {
            self.remove(k);
        }
        keys.sort();
        keys
    }

    fn remove(&mut self, prec: &T) -> Option<Dependency<T>> {
        if let Some(ref p) = self.top.remove(prec) {
            for s in &p.succ {
                if let Some(y) = self.top.get_mut(s) {
                    y.num_prec -= 1;
                }
            }
        }
        None
    }
}

impl<T: Clone + Ord + Eq + std::fmt::Display> TopologicalSort<T> {
    pub fn print_debug(&self) {
        println!("TopologicalSort {{");
        for (k, v) in &self.top {
            println!("  {}: (#prec: {}, #succ: {})", k, v.num_prec, v.succ.len());
        }
        println!("}}");
    }
}
