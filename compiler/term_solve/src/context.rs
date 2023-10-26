use crate::type_env::TypeEnv;
use crate::union_find::UnionFind;
use term_core as core;
use term_print::PrettyString;

use core::{Constraint, Ef, Expr, MonoVarId, Span, Ty, TyE, VarId};

use std::collections::{BTreeSet, HashMap};
use std::ops::{Deref, DerefMut};
use ustr::Ustr;

/// Type context.
#[derive(Debug)]
pub struct TyContext {
    /// Type environment.
    pub typings: TypeEnv,
    /// The set of unsolved type variables.
    pub open: BTreeSet<MonoVarId>,
    /// Working type set.
    pub ty_set: UnionFind<Ty>,
    /// Working effect set.
    pub ef_set: UnionFind<Ef>,
}

impl TyContext {
    pub fn new() -> Self {
        Self {
            typings: TypeEnv::new(),
            open: BTreeSet::default(),
            ty_set: UnionFind::new(),
            ef_set: UnionFind::new(),
        }
    }

    /// Registers a type variable.
    pub fn new_ty_var(&mut self, id: MonoVarId) -> Ty {
        self.ty_set.insert(Ty::Mono(id));
        Ty::Mono(id)
    }

    /// Registers an effect variable.
    pub fn new_ef_var(&mut self, id: MonoVarId) -> Ef {
        self.ef_set.insert(Ef::Mono(id));
        Ef::Mono(id)
    }
}
