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
    pub stack: Vec<HashMap<Expr, TyE>>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            stack: vec![HashMap::new()],
        }
    }

    pub fn is_empty(&self) -> bool {
        self.stack.is_empty() || self.stack.last().unwrap().is_empty()
    }

    pub fn get(&self, var: &Expr) -> Option<&TyE> {
        for map in self.stack.iter().rev() {
            if let Some(t) = map.get(var) {
                return Some(t);
            }
        }
        None
    }

    pub fn get_mut(&mut self, var: &Expr) -> Option<&mut TyE> {
        for map in self.stack.iter_mut().rev() {
            if let Some(t) = map.get_mut(var) {
                return Some(t);
            }
        }
        None
    }

    pub fn insert(&mut self, var: Expr, t: TyE) {
        self.stack.last_mut().unwrap().insert(var, t);
    }

    pub fn push_empty(&mut self) {
        self.stack.push(HashMap::new());
    }

    pub fn push(&mut self, tys: impl IntoIterator<Item = (Expr, TyE)>) {
        let mut map = HashMap::new();
        map.extend(tys);
        self.stack.push(map);
    }

    pub fn pop(&mut self) -> Self {
        Self {
            stack: vec![self.stack.pop().unwrap()],
        }
    }

    pub fn from_typings(typings: impl IntoIterator<Item = (Expr, TyE)>) -> Self {
        let mut map = HashMap::new();
        map.extend(typings);
        Self { stack: vec![map] }
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
