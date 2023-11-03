use crate::ast;
use term_common as common;
use term_core as core;

use ast::{EffectDecl, Expr, Ty, VarDecl};
use common::declare_union_id;
use core::{DataConId, DeclId, EffectId, EffectOpId, HandlerId, Id, InstId, Span, VarId};

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
    pub id_var_ids: BTreeMap<Id, VarId>,
    pub var_decl_ids: BTreeMap<VarId, DeclId>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            ambiguous_names: UstrSet::default(),
            decls: BTreeMap::default(),
            id_var_ids: BTreeMap::default(),
            var_decl_ids: BTreeMap::default(),
        }
    }
}
