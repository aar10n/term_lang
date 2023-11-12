use crate::*;
use term_common::RcRef;
use term_core as core;

use ast::{Decl, EffectDecl, Expr, Ty};
use core::{
    ClassId, DataConId, DataId, DeclId, EffectId, EffectOpId, HandlerId, Id, InstId, Span, VarId,
};

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::ops::{Deref, DerefMut};
use ustr::{Ustr, UstrMap, UstrSet};

/// An AST context.
#[derive(Debug, Default)]
pub struct Context {
    pub datas: BTreeMap<DataId, RcRef<DataDecl>>,
    pub classes: BTreeMap<ClassId, RcRef<ClassDecl>>,
    pub insts: BTreeMap<InstId, RcRef<ClassInst>>,
    pub effects: BTreeMap<EffectId, RcRef<EffectDecl>>,
    pub handlers: BTreeMap<HandlerId, RcRef<EffectHandler>>,
    pub decls: BTreeMap<DeclId, RcRef<Decl>>,
    pub funcs: BTreeMap<VarId, RcRef<Func>>,

    pub dep_graph: BTreeMap<VarId, BTreeSet<VarId>>,
    pub id_var_ids: BTreeMap<Id, VarId>,
    pub var_decl_ids: BTreeMap<VarId, DeclId>,
    pub method_ids: BTreeSet<(VarId, InstId)>,
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }
}
