use crate::ast;
use term_common as common;
use term_core as core;

use ast::{EffectDecl, Expr, Ty, VarDecl};
use common::declare_union_id;
use core::{DataConId, DeclId, EffectId, EffectOpId, HandlerId, Id, Span, VarId};

use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use ustr::{Ustr, UstrMap, UstrSet};

/// An AST context.
#[derive(Debug)]
pub struct Context<'a> {
    pub ctx: &'a mut core::Context,
    pub decls: BTreeMap<DeclId, Rc<RefCell<VarDecl>>>,

    pub ambiguous_names: UstrSet,
    pub con_var_ids: BTreeMap<DataConId, VarId>,
    pub decl_var_ids: BTreeMap<DeclId, VarId>,
    pub op_var_ids: BTreeMap<EffectOpId, VarId>,
    pub handler_var_ids: BTreeMap<HandlerId, VarId>,
    pub var_decl_ids: BTreeMap<VarId, DeclId>,
}

impl<'a> Context<'a> {
    pub fn new(ctx: &'a mut core::Context) -> Self {
        Self {
            ctx,
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

impl AsMut<core::Context> for Context<'_> {
    fn as_mut(&mut self) -> &mut core::Context {
        self.ctx
    }
}

impl<'a> From<&'a mut core::Context> for Context<'a> {
    fn from(value: &'a mut core::Context) -> Self {
        Self::new(value)
    }
}
