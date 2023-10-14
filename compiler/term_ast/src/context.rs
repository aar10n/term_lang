use crate::ast;
use term_common as common;
use term_core as core;

use ast::{EffectDecl, Expr, Ty, VarDecl};
use common::declare_union_id;
use core::{DataConId, DeclId, EffectId, EffectOpId, HandlerId, Id, Span, VarId};

use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use ustr::{Ustr, UstrMap, UstrSet};

/// An AST context.
#[derive(Debug)]
pub struct Context {
    pub ambiguous_names: UstrSet,
    pub decls: BTreeMap<DeclId, Rc<RefCell<VarDecl>>>,
    pub con_var_ids: BTreeMap<DataConId, VarId>,
    pub decl_var_ids: BTreeMap<DeclId, VarId>,
    pub op_var_ids: BTreeMap<EffectOpId, VarId>,
    pub handler_var_ids: BTreeMap<HandlerId, VarId>,
    pub var_decl_ids: BTreeMap<VarId, DeclId>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            decls: BTreeMap::default(),

            ambiguous_names: UstrSet::default(),
            con_var_ids: BTreeMap::default(),
            decl_var_ids: BTreeMap::default(),
            op_var_ids: BTreeMap::default(),
            handler_var_ids: BTreeMap::default(),
            var_decl_ids: BTreeMap::default(),
        }
    }
}
