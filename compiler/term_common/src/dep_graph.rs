use term_print::{PrettyPrint, PrettyString};

use std::collections::{btree_map::Entry, BTreeMap, BTreeSet};
use std::io;

#[derive(Clone, Debug)]
pub struct Dependency<T> {
    /// The number of dependencies this node has.
    pub num_deps: usize,
    /// The set of nodes that depend on this node.
    pub depedents: BTreeSet<T>,
}

impl<T: Ord + Eq> Dependency<T> {
    fn new() -> Self {
        Self {
            num_deps: 0,
            depedents: BTreeSet::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct DependencyGraph<T> {
    pub graph: BTreeMap<T, Dependency<T>>,
}

impl<T> Default for DependencyGraph<T> {
    fn default() -> DependencyGraph<T> {
        DependencyGraph {
            graph: BTreeMap::new(),
        }
    }
}

impl<T: Clone + Ord + Eq> DependencyGraph<T> {
    pub fn new() -> DependencyGraph<T> {
        Default::default()
    }

    pub fn len(&self) -> usize {
        self.graph.len()
    }

    pub fn is_empty(&self) -> bool {
        self.graph.is_empty()
    }

    pub fn add_entry(&mut self, entry: impl Into<T>) {
        match self.graph.entry(entry.into()) {
            Entry::Vacant(e) => {
                let dep = Dependency::new();
                e.insert(dep);
            }
            Entry::Occupied(_) => {}
        }
    }

    pub fn add_dependency(&mut self, entry: impl Into<T>, dependency: impl Into<T>) {
        let entry = entry.into();
        let dependency = dependency.into();
        let recursive = entry == dependency;

        match self.graph.entry(entry) {
            Entry::Vacant(e) => {
                let mut dep = Dependency::new();
                if !recursive {
                    dep.depedents.insert(dependency.clone());
                }
                e.insert(dep);
            }
            Entry::Occupied(e) => {
                let dep = e.into_mut();
                if !dep.depedents.insert(dependency.clone()) {
                    return;
                }
            }
        }

        if recursive {
            return;
        }

        match self.graph.entry(dependency) {
            Entry::Vacant(e) => {
                let mut dep = Dependency::new();
                dep.num_deps = 1;
                e.insert(dep);
            }
            Entry::Occupied(e) => {
                e.into_mut().num_deps += 1;
            }
        }
    }

    /// Returns the next set of items that have no dependencies and removes them from the graph.
    pub fn pop_next(&mut self) -> BTreeSet<T> {
        let set = self
            .graph
            .iter()
            .filter(|&(_, d)| d.num_deps == 0)
            .map(|(e, _)| e.clone())
            .collect::<BTreeSet<_>>();
        for e in &set {
            self.remove(e);
        }
        set
    }

    /// Returns a list of all items ordered such that no item depends on one that comes after it.
    pub fn total_order(&mut self) -> Vec<T> {
        let mut res = vec![];
        while let deps = self.pop_next() && !deps.is_empty() {
            res.extend(deps);
        }
        res.reverse();
        res
    }

    /// Returns a new graph with the given entries removed.
    pub fn removing<I>(self, entries: &I) -> Self
    where
        I: IntoIterator<Item = T>,
        for<'a> &'a I: IntoIterator<Item = &'a T>,
    {
        let mut graph = self;
        for entry in entries.into_iter() {
            graph.remove(&entry);
        }
        graph
    }

    fn remove(&mut self, entry: &T) -> Option<Dependency<T>> {
        if let Some(ref p) = self.graph.remove(entry) {
            for s in &p.depedents {
                if let Some(y) = self.graph.get_mut(s) {
                    y.num_deps -= 1;
                }
            }
        }
        None
    }
}

impl<T> From<&BTreeMap<T, BTreeSet<T>>> for DependencyGraph<T>
where
    T: Clone + Ord + Eq,
{
    fn from(map: &BTreeMap<T, BTreeSet<T>>) -> Self {
        let mut graph = Self::new();
        for (e, deps) in map.iter() {
            graph.add_entry(e.clone()); // in case deps is empty
            for dep in deps {
                graph.add_dependency(e.clone(), dep.clone());
            }
        }
        graph
    }
}

impl<T: Clone + Ord + Eq + std::fmt::Display> DependencyGraph<T> {
    pub fn print_debug(&self) {
        println!("Dependency Graph {{");
        for (k, v) in &self.graph {
            println!(
                "  {}: (num deps: {}, #dependents: {})",
                k,
                v.num_deps,
                v.depedents.len()
            );
        }
        println!("}}");
    }
}

impl<'a, T, Context, Info> PrettyPrint<Context, Info> for DependencyGraph<T>
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
        for (prec, dep) in &self.graph {
            writeln!(
                out,
                "(^{}) {} | {}",
                dep.num_deps,
                prec.pretty_string(ctx),
                dep.depedents
                    .iter()
                    .map(|id| id.pretty_string(ctx))
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
        }
        Ok(())
    }
}
