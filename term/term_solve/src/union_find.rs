use std::collections::BTreeMap;
use std::hash::Hash;

#[derive(Clone, Debug)]
pub struct UnionFind<T: Clone + Ord + Eq> {
    pub(crate) parent: Vec<usize>,
    pub(crate) size: Vec<usize>,
    pub(crate) t_keys: BTreeMap<T, usize>,
    pub(crate) key_ts: BTreeMap<usize, T>,
}

impl<T: Clone + Ord + Eq> UnionFind<T> {
    pub fn new() -> Self {
        Self {
            parent: Vec::new(),
            size: Vec::new(),
            t_keys: BTreeMap::new(),
            key_ts: BTreeMap::new(),
        }
    }

    pub fn insert(&mut self, t: T) {
        if self.t_keys.get(&t).is_some() {
            return;
        }
        self.create_key(t);
    }

    pub fn find(&mut self, t: T) -> T {
        match self.t_keys.get(&t) {
            Some(_) => (),
            None => return t,
        };

        let (_, t) = self.find_or_create(t);
        t
    }

    pub fn find_no_mut(&self, t: T) -> T {
        match self.t_keys.get(&t) {
            Some(_) => (),
            None => return t,
        };
        self.find_no_create(t.clone()).unwrap_or(t)
    }

    pub fn union(&mut self, t1: T, t2: T) -> T {
        let (k1, t1) = self.find_or_create(t1);
        let (k2, t2) = self.find_or_create(t2);
        if k1 == k2 {
            return t1;
        }

        self.parent[k2] = k1;
        self.size[k1] += self.size[k2];
        t1
    }

    pub fn iter_class(&self, p: &T) -> impl Iterator<Item = T> + '_ {
        let k = *self.t_keys.get(p).unwrap();
        self.key_ts
            .iter()
            .filter(move |(_, t)| self.parent[self.t_keys[*t]] == k)
            .map(|(_, t)| t.clone())
    }

    fn find_or_create(&mut self, t: T) -> (usize, T) {
        let k = match self.t_keys.get(&t) {
            Some(k) => *k,
            None => self.create_key(t),
        };

        let p = self.parent[k];
        if p == k {
            (k, self.key_ts[&k].clone())
        } else {
            let t = self.find(self.key_ts[&p].clone());
            self.parent[k] = self.t_keys[&t];
            (self.t_keys[&t], t)
        }
    }

    fn find_no_create(&self, t: T) -> Option<T> {
        let k = match self.t_keys.get(&t) {
            Some(k) => *k,
            None => return None,
        };

        let p = self.parent[k];
        if p == k {
            Some(self.key_ts[&k].clone())
        } else {
            let t = self.find_no_mut(self.key_ts[&p].clone());
            Some(t)
        }
    }

    fn create_key(&mut self, t: T) -> usize {
        let id = self.parent.len();
        self.parent.push(id);
        self.size.push(1);
        self.t_keys.insert(t.clone(), id);
        self.key_ts.insert(id, t);
        id
    }
}
