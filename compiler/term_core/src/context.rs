use crate::core::*;
use term_common::{declare_id_collection, declare_union_id, source::SourceMap, span::Spanned};

use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use ustr::{Ustr, UstrMap, UstrSet};

declare_id_collection! {
    pub Ids {
        ClassId,
        DataId,
        DataConId(DataId),
        EffectId,
        EffectOpId(EffectId),
        DeclId,
        HandlerId,
        InstId,
        MonoVarId,
        PolyVarId,
        VarId,
    }
}

declare_union_id! {
    ParentId {
        Class(ClassId),
        Data(DataId),
        Effect(EffectId),
        Handler(HandlerId),
        Inst(InstId),
    }
}

#[derive(Debug)]
pub struct Context {
    pub ids: Ids,
    pub sources: SourceMap,
    pub builtins: UstrSet,
    pub dep_graph: BTreeMap<VarId, BTreeSet<VarId>>,

    pub global_names: UstrMap<Id>,
    pub global_types: UstrMap<Id>,

    pub classes: BTreeMap<ClassId, Rc<RefCell<Class>>>,
    pub datas: BTreeMap<DataId, Rc<RefCell<Data>>>,
    pub defs: BTreeMap<VarId, Rc<RefCell<Def>>>,
    pub effects: BTreeMap<EffectId, Rc<RefCell<Effect>>>,
    pub handlers: BTreeMap<HandlerId, Rc<RefCell<Handler>>>,
    pub insts: BTreeMap<InstId, Rc<RefCell<Inst>>>,

    /// id -> name and span.
    id_names: BTreeMap<Id, (Ustr, Span)>,
    /// Scoped namespaces tied to a parent (e.g. class methods, effect ops).
    scopes: BTreeMap<ParentId, Scope>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            ids: Ids::default(),
            sources: SourceMap::default(),
            builtins: UstrSet::default(),
            dep_graph: BTreeMap::default(),

            global_names: UstrMap::default(),
            global_types: UstrMap::default(),

            classes: BTreeMap::default(),
            datas: BTreeMap::default(),
            defs: BTreeMap::default(),
            effects: BTreeMap::default(),
            handlers: BTreeMap::default(),
            insts: BTreeMap::default(),

            id_names: BTreeMap::default(),
            scopes: BTreeMap::default(),
        }
    }

    pub fn id_as_str<T: Into<Id>>(&self, id: T) -> &str {
        let id = id.into();
        self.id_names
            .get(&id)
            .map(|(name, _)| name.as_str())
            .unwrap_or("<unknown>")
    }

    pub fn id_as_span<T: Into<Id>>(&self, id: T) -> Option<Span> {
        let id = id.into();
        self.id_names.get(&id).map(|(_, span)| *span)
    }

    pub fn register_id_name<T: Into<Id> + Copy>(&mut self, id: T, name: Ustr, span: Span) {
        self.id_names.insert(id.into(), (name, span));
    }

    pub fn register_builtin(&mut self, name: &str) {
        let name = Ustr::from(name);
        if let Some(existing_id) = self.global_names.get(&name) {
            panic!("builtin `{}` already registered as {:?}", name, existing_id);
        }
        if let Some(existing_func) = self.builtins.get(&name) {
            panic!("builtin `{}` already registered", name);
        }

        self.builtins.insert(name);
    }

    pub fn register_global_name<T: Into<Id> + Copy>(
        &mut self,
        id: T,
        name: Ustr,
        span: Span,
    ) -> Result<(), Id> {
        if let Some(existing_id) = self.global_names.get(&name) {
            return Err(*existing_id);
        }

        self.id_names.insert(id.into(), (name, span));
        self.global_names.insert(name, id.into());
        Ok(())
    }

    pub fn register_global_type<T: Into<Id> + Copy>(
        &mut self,
        id: T,
        name: Ustr,
        span: Span,
    ) -> Result<(), Id> {
        if let Some(existing_id) = self.global_types.get(&name) {
            return Err(*existing_id);
        }

        self.id_names.insert(id.into(), (name, span));
        self.global_types.insert(name, id.into());
        Ok(())
    }

    pub fn register_scoped_name<T: Into<Id> + Copy, U: Into<ParentId> + Copy>(
        &mut self,
        parent_id: U,
        id: T,
        name: Ustr,
        span: Span,
        excl: Exclusivity,
    ) -> Result<(), Id> {
        let id = id.into();
        let parent_id = parent_id.into();

        match excl {
            Exclusivity::Name => {
                if let Some(existing_id) = self.global_names.get(&name) {
                    return Err(*existing_id);
                }
            }
            Exclusivity::Type => {
                if let Some(existing_id) = self.global_types.get(&name) {
                    return Err(*existing_id);
                }
            }
            Exclusivity::None => {}
        }

        if let Some(existing_id) = self
            .scopes
            .get(&parent_id)
            .and_then(|ns| ns.names.get(&name))
        {
            return Err(*existing_id);
        }

        self.id_names.insert(id, (name, span));
        self.scopes
            .entry(parent_id)
            .or_insert_with(|| Scope::new(parent_id.into()))
            .names
            .insert(name, id);
        Ok(())
    }

    pub fn resolve_scoped_name<T: Into<ParentId> + Copy>(
        &self,
        parent_id: T,
        name: &Ustr,
    ) -> Option<Id> {
        let parent_id = parent_id.into();
        self.scopes
            .get(&parent_id)
            .and_then(|ns| ns.names.get(name))
            .copied()
    }
}

pub enum Exclusivity {
    Name,
    Type,
    None,
}

/// A scope for names.
#[derive(Clone, Debug, Default)]
struct Scope {
    pub parent_id: Option<ParentId>,
    pub names: UstrMap<Id>,
}

impl Scope {
    fn new(parent_id: ParentId) -> Self {
        Self {
            parent_id: Some(parent_id),
            names: UstrMap::default(),
        }
    }
}
