use term_print::PrettyPrint;

use std::collections::BTreeSet;
use std::io;
use std::iter::{FromIterator, IntoIterator};
use std::ops::{BitOr, BitOrAssign, Deref, DerefMut};

/// A set that preserves insertion order.
#[derive(Clone, Debug)]
pub struct OrderedSet<T> {
    set: BTreeSet<T>,
    order: Vec<T>,
}

impl<T: Clone + Ord> OrderedSet<T> {
    pub fn new() -> Self {
        Self {
            set: BTreeSet::new(),
            order: vec![],
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            set: BTreeSet::new(),
            order: Vec::with_capacity(capacity),
        }
    }

    pub fn iter_ordered(&self) -> impl Iterator<Item = &T> {
        self.order.iter()
    }

    pub fn insert(&mut self, value: T) -> bool {
        if self.set.insert(value.clone()) {
            self.order.push(value);
            true
        } else {
            false
        }
    }

    pub fn remove(&mut self, value: &T) -> bool {
        if self.set.remove(value) {
            self.order.retain(|v| v != value);
            true
        } else {
            false
        }
    }

    pub fn clear(&mut self) {
        self.set.clear();
        self.order.clear();
    }

    pub fn union(&mut self, other: &Self) {
        for value in other.iter() {
            self.insert(value.clone());
        }
    }

    pub fn intersection(&mut self, other: &Self) {
        self.order.retain(|value| other.contains(value));
        self.set = self.order.iter().cloned().collect();
    }

    pub fn difference(&mut self, other: &Self) {
        self.order.retain(|value| !other.contains(value));
        self.set = self.order.iter().cloned().collect();
    }

    pub fn symmetric_difference(&mut self, other: &Self) {
        for value in other.iter() {
            if self.contains(value) {
                self.remove(value);
            } else {
                self.insert(value.clone());
            }
        }
    }

    pub fn to_vec(&self) -> Vec<T> {
        self.order.clone()
    }
}

impl<T: Clone + Ord> Default for OrderedSet<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone + Ord> Deref for OrderedSet<T> {
    type Target = BTreeSet<T>;

    fn deref(&self) -> &Self::Target {
        &self.set
    }
}

impl<T: Clone + Ord> DerefMut for OrderedSet<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.set
    }
}

impl<T: Clone + Ord> BitOr for OrderedSet<T> {
    type Output = Self;

    fn bitor(mut self, other: Self) -> Self::Output {
        self.union(&other);
        self
    }
}

impl<T: Clone + Ord> BitOrAssign for OrderedSet<T> {
    fn bitor_assign(&mut self, other: Self) {
        self.union(&other);
    }
}

impl<T: Clone + Ord> FromIterator<T> for OrderedSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut set = Self::new();
        for value in iter {
            set.insert(value);
        }
        set
    }
}

impl<T: Clone + Ord> IntoIterator for OrderedSet<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.order.into_iter()
    }
}

impl<'a, T, Context, Info> PrettyPrint<Context, Info> for OrderedSet<T>
where
    T: PrettyPrint<Context, Info> + Clone + Ord,
    Info: Default + Clone,
{
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        _: Info,
    ) -> io::Result<()> {
        write!(out, "{{")?;
        for (i, value) in self.order.iter().enumerate() {
            if i > 0 {
                write!(out, ", ")?;
            }
            value.pretty_print(out, ctx, Info::default())?;
        }
        write!(out, "}}")?;
        Ok(())
    }
}
