use crate::type_env::TypeEnv;
use crate::union_find::UnionFind;
use term_core as core;
use term_print::PrettyString;

use core::{Constraint, Ef, Expr, MonoVarId, Span, Ty, TyE, VarId};

use std::collections::{BTreeSet, HashMap};
use std::ops::{Deref, DerefMut};
use ustr::Ustr;

pub struct Context<'ctx> {
    pub core: &'ctx mut core::Context,
    pub trace: bool,

    pub typ_env: TypeEnv,
    pub ty_set: UnionFind<Ty>,
    pub ef_set: UnionFind<Ef>,
}

impl<'ctx> Context<'ctx> {
    pub fn new(core: &'ctx mut core::Context, trace: bool) -> Self {
        let typ_env = TypeEnv::from(core.typings.clone());
        Self {
            core,
            trace,
            typ_env,
            ty_set: UnionFind::new(),
            ef_set: UnionFind::new(),
        }
    }

    pub fn new_normal(core: &'ctx mut core::Context) -> Self {
        Self::new(core, false)
    }

    pub fn new_tracing(core: &'ctx mut core::Context) -> Self {
        Self::new(core, true)
    }

    pub fn new_ty_var(&mut self) -> Ty {
        let id = self.core.ids.next_mono_var_id();
        self.ty_set.insert(Ty::Mono(id));
        Ty::Mono(id)
    }

    pub fn new_ef_var(&mut self) -> Ef {
        let id = self.core.ids.next_mono_var_id();
        self.ef_set.insert(Ef::Mono(id));
        Ef::Mono(id)
    }
}
