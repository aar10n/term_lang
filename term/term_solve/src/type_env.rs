use term_core as core;
use term_diag as diag;
use term_print as print;

use core::*;
use diag::{Diagnostic, IntoDiagnostic, IntoError};
use print::PrettyPrint;

use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::ops::{BitOr, Deref, DerefMut};

/// A type environment.
#[derive(Clone, Debug)]
pub struct TypeEnv {
    pub map: HashMap<Expr, TyE>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn from_typings<I>(typings: I) -> Self
    where
        I: IntoIterator<Item = (Expr, TyE)>,
    {
        let mut map = HashMap::new();
        map.extend(typings);
        Self { map }
    }
}

impl Deref for TypeEnv {
    type Target = HashMap<Expr, TyE>;

    fn deref(&self) -> &Self::Target {
        &self.map
    }
}

impl DerefMut for TypeEnv {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.map
    }
}

impl<I> From<I> for TypeEnv
where
    I: IntoIterator<Item = (Expr, TyE)>,
{
    fn from(value: I) -> Self {
        Self::from_typings(value)
    }
}

pub trait TypeLike = Clone + Ord + Eq + Hash + PrettyPrint<core::Context>;

/// A set for type-like objects (mainly Ty's and Ef's)
#[derive(Clone, Debug)]
pub struct TSet<T: TypeLike> {
    pub set: HashSet<T>,
}

impl<T: TypeLike> TSet<T> {
    pub fn new() -> Self {
        Self {
            set: HashSet::new(),
        }
    }

    pub fn into_vec(self) -> Vec<T> {
        self.set.into_iter().collect()
    }
}

impl<T: TypeLike> Default for TSet<T> {
    fn default() -> Self {
        Self {
            set: Default::default(),
        }
    }
}

impl<T: TypeLike> Extend<T> for TSet<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.set.extend(iter);
    }
}

impl<T: TypeLike> FromIterator<T> for TSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self {
            set: iter.into_iter().collect(),
        }
    }
}

impl<T: TypeLike> IntoIterator for TSet<T> {
    type Item = T;
    type IntoIter = std::collections::hash_set::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.set.into_iter()
    }
}

impl<T: TypeLike> Deref for TSet<T> {
    type Target = HashSet<T>;

    fn deref(&self) -> &Self::Target {
        &self.set
    }
}

impl<T: TypeLike> DerefMut for TSet<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.set
    }
}

impl<T: TypeLike> BitOr for TSet<T> {
    type Output = Self;

    fn bitor(mut self, rhs: Self) -> Self::Output {
        self.set.extend(rhs.set);
        self
    }
}

impl<T: TypeLike> BitOr<T> for TSet<T> {
    type Output = Self;

    fn bitor(mut self, rhs: T) -> Self::Output {
        self.set.insert(rhs);
        self
    }
}
