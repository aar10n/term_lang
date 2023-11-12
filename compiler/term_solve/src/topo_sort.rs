use term_print::{PrettyPrint, PrettyString};

use std::collections::{btree_map::Entry, BTreeMap, BTreeSet};
use std::io;

#[derive(Clone, Debug)]
pub struct Dependency<T> {
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
    pub top: BTreeMap<T, Dependency<T>>,
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
                let dep = e.into_mut();
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

    /// Returns a list of items that have no dependencies.
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

    pub fn pop_all(&mut self) -> Vec<T> {
        let mut res = vec![];
        while let mut deps = self.pop() && !deps.is_empty() {
            res.extend(deps);
        }
        res
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

impl<T> From<&BTreeMap<T, BTreeSet<T>>> for TopologicalSort<T>
where
    T: Clone + Ord + Eq,
{
    fn from(map: &BTreeMap<T, BTreeSet<T>>) -> Self {
        let mut topo = Self::new();
        for (k, v) in map.iter() {
            topo.declare_item(k.clone());
            for dep in v {
                topo.add_dependency(dep.clone(), k.clone());
            }
        }
        topo
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

impl<'a, T, Context, Info> PrettyPrint<Context, Info> for TopologicalSort<T>
where
    T: PrettyPrint<Context, Info> + Clone + Ord + Eq,
    Info: Default + Clone,
{
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        _: Info,
    ) -> io::Result<()> {
        for (prec, dep) in &self.top {
            writeln!(
                out,
                "({}) {} | {}",
                dep.num_prec,
                prec.pretty_string(ctx),
                dep.succ
                    .iter()
                    .map(|id| id.pretty_string(ctx))
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
        }
        Ok(())
    }
}
