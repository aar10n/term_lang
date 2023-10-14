use crate::type_env::{TSet, TypeEnv};
use crate::union_find::UnionFind;
use term_core as core;
use term_print::PrettyString;

use core::{Constraint, Ef, Expr, MonoVarId, Span, Ty, TyE, VarId};

use std::collections::{BTreeSet, HashMap};
use std::ops::{Deref, DerefMut};
use ustr::Ustr;

/// Type solving context.
#[derive(Debug)]
pub struct Context<'a> {
    /// Core context.
    pub ctx: &'a mut core::Context,
    /// Type environment.
    pub typings: TypeEnv,
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
            typings: TypeEnv::new(),
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
