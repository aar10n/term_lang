use crate::type_env::{TSet, TypeEnv};
use crate::union_find::UnionFind;
use term_core as core;

use core::{Constraint, Ef, Expr, MonoVarId, Span, Ty, TyE, VarId};

use std::collections::{BTreeSet, HashMap};
use std::ops::{Deref, DerefMut};
use ustr::Ustr;

/// Type solving context.
#[derive(Debug)]
pub struct Context<'a> {
    /// Core context.
    pub ctx: &'a mut core::Context,
    /// Type environments.
    pub env: Vec<TypeEnv>,
    /// The set of unsolved type variables.
    pub open: BTreeSet<MonoVarId>,
    /// Working type set.
    pub ty_set: UnionFind<Ty>,
    /// Working effect set.
    pub ef_set: UnionFind<Ef>,
    /// Relevant source spans.
    pub spans: Vec<Span>,
}

impl<'a> Context<'a> {
    pub fn new(ctx: &'a mut core::Context) -> Self {
        Self {
            ctx,
            env: vec![],
            open: BTreeSet::default(),
            ty_set: UnionFind::new(),
            ef_set: UnionFind::new(),
            spans: Vec::default(),
        }
    }

    pub fn new_mono_var(&mut self) -> MonoVarId {
        self.ctx.ids.next_mono_var_id()
    }

    /// Returns the next unique monotype var and registers it in the type set.
    pub fn new_ty_var(&mut self) -> Ty {
        let id = self.ctx.ids.next_mono_var_id();
        self.ty_set.insert(Ty::Mono(id));
        Ty::Mono(id)
    }

    /// Returns the next unique monotype var and registers it in the effect set.
    pub fn new_ef_var(&mut self) -> Ef {
        let id = self.ctx.ids.next_mono_var_id();
        self.ef_set.insert(Ef::Mono(id));
        Ef::Mono(id)
    }

    /// Pushes a new assumption to the typing environment.
    pub fn push_typing(&mut self, var: Expr, t: TyE) {
        let e = TypeEnv::from([(var, t)]);
        self.env.push(e);
    }

    /// Pushes a new set of assumptions to the typing environment.
    pub fn push_typings(&mut self, typings: impl IntoIterator<Item = (Expr, TyE)>) {
        let e = TypeEnv::from(typings);
        self.env.push(e);
    }

    /// Drops the last pushed typings from the environment.
    pub fn pop_typings(&mut self) -> TypeEnv {
        assert!(!self.env.is_empty());
        self.env.pop().unwrap()
    }

    pub fn resolve_local_var(&self, var_id: VarId) -> Option<(Expr, TyE)> {
        for env in self.env.iter().rev() {
            if let Some(t) = env.get(&Expr::Var(var_id)) {
                return Some((Expr::Var(var_id), t.clone()));
            }
        }
        None
    }
}

impl Deref for Context<'_> {
    type Target = core::Context;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}

impl DerefMut for Context<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ctx
    }
}

impl<'a> From<&'a mut core::Context> for Context<'a> {
    fn from(value: &'a mut core::Context) -> Self {
        Self::new(value)
    }
}

fn nth_monotype_var(mut n: usize) -> Ustr {
    let var = "t";
    let digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"];
    // let var = "𝜏";
    // let digits = ["₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉"];
    let mut s = String::new();
    while n > 0 {
        s.push_str(digits[n % 10]);
        n /= 10;
    }
    let s = s.chars().rev().collect::<String>();
    Ustr::from(&format!("{}{}", var, s))
}
