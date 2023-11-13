use crate::core::*;
use term_common::{source::SourceMap, span::Spanned};
use term_print::PrettyString;

use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use ustr::{Ustr, UstrMap, UstrSet};

#[derive(Debug, Default)]
pub struct Context {
    pub ids: Ids,
    pub sources: SourceMap,
    pub builtin_names: UstrSet,
    pub builtin_types: UstrSet,

    pub id_names: BTreeMap<Id, (Ustr, Span)>,
    pub global_names: UstrMap<Id>,
    pub global_types: UstrMap<Id>,
    pub name_scopes: BTreeMap<ParentId, NameScope>,

    pub defs: BTreeMap<VarId, Rc<RefCell<Def>>>,
    pub classes: BTreeMap<ClassId, Rc<RefCell<Class>>>,
    pub effects: BTreeMap<EffectId, Rc<RefCell<Effect>>>,
    pub instances: BTreeMap<InstId, BTreeMap<Ustr, VarId>>,
    pub handlers: BTreeMap<HandlerId, BTreeMap<Ustr, VarId>>,

    pub functions: UstrMap<Vec<(VarId, InstId)>>,
    pub signatures: HashMap<TyE, Vec<Ustr>>,
    pub typings: HashMap<Expr, TyE>,
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn id_as_str<T: Into<Id>>(&self, id: T) -> &str {
        let id = id.into();
        self.id_names
            .get(&id)
            .map(|(name, _)| name.as_str())
            .unwrap_or("<unknown>")
    }

    pub fn id_as_ustr<T: Into<Id>>(&self, id: T) -> Ustr {
        let id = id.into();
        self.id_names
            .get(&id)
            .map(|(name, _)| *name)
            .unwrap_or(ustr::ustr("<unknown>"))
    }

    pub fn id_as_span<T: Into<Id>>(&self, id: T) -> Option<Span> {
        let id = id.into();
        self.id_names.get(&id).map(|(_, span)| *span)
    }

    pub fn register_id_name<T: Into<Id> + Copy>(&mut self, id: T, name: Ustr, span: Span) {
        self.id_names.insert(id.into(), (name, span));
    }

    pub fn register_builtin_name(&mut self, name: &str) {
        let name = Ustr::from(name);
        if let Some(existing_id) = self.global_names.get(&name) {
            panic!(
                "`{}` already registered as {}",
                name,
                existing_id.pretty_string(self)
            );
        }

        // declare it internally at first but will be associated with a
        // real declaration site later
        let id = self.ids.next_var_id();
        self.builtin_names.insert(name);
        self.id_names.insert(id.into(), (name, Span::default()));
        self.global_names.insert(name, id.into());
    }

    pub fn register_builtin_type(&mut self, name: &str) {
        let name = Ustr::from(name);
        if let Some(existing_id) = self.global_types.get(&name) {
            panic!(
                "`{}` already registered as {}",
                name,
                existing_id.pretty_string(self)
            );
        }
        if let Some(_) = self.builtin_types.get(&name) {
            panic!("builtin type `{}` already registered", name);
        }

        let id = self.ids.next_data_id();
        self.builtin_types.insert(name);
        self.id_names.insert(id.into(), (name, Span::default()));
        self.global_types.insert(name, id.into());
    }

    pub fn register_global_name<T: Into<Id> + Copy>(
        &mut self,
        id: T,
        name: Ustr,
        span: Span,
    ) -> Result<(), (Span, Id)> {
        if let Some(existing_id) = self.global_names.get(&name) {
            return Err((span, *existing_id));
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
    ) -> Result<(), (Span, Id)> {
        if let Some(existing_id) = self.global_types.get(&name) {
            return Err((span, *existing_id));
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
    ) -> Result<(), (Span, Id)> {
        let id = id.into();
        let parent_id = parent_id.into();

        match excl {
            Exclusivity::Name => {
                if let Some(existing_id) = self.global_names.get(&name) {
                    return Err((span, *existing_id));
                }
            }
            Exclusivity::Type => {
                if let Some(existing_id) = self.global_types.get(&name) {
                    return Err((span, *existing_id));
                }
            }
            Exclusivity::None => {}
        }

        if let Some(existing_id) = self
            .name_scopes
            .get(&parent_id)
            .and_then(|ns| ns.names.get(&name))
        {
            return Err((span, *existing_id));
        }

        self.id_names.insert(id.into(), (name, span));
        self.name_scopes
            .entry(parent_id)
            .or_insert_with(|| NameScope::new(parent_id.into()))
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
        self.name_scopes
            .get(&parent_id)
            .and_then(|ns| ns.names.get(name))
            .copied()
    }

    pub fn get_builtin_name(&self, name: impl AsRef<str>) -> VarId {
        let name = name.as_ref();
        self.global_names
            .get(&Ustr::from(name))
            .copied()
            .unwrap_or_else(|| panic!("builtin name `{}` not found", name))
            .var_id()
    }

    pub fn get_builtin_type(&self, name: impl AsRef<str>) -> DataId {
        let name = name.as_ref();
        self.global_types
            .get(&Ustr::from(name))
            .copied()
            .unwrap_or_else(|| panic!("builtin type `{}` not found", name))
            .data_id()
    }

    pub fn get_primitive_ty(&self, name: impl AsRef<str>) -> TyE {
        let name = name.as_ref();
        if let Some(Id::Data(id)) = self.global_types.get(&Ustr::from(name)) {
            return TyE::pure(Ty::Data(*id, vec![]));
        }
        TyE::pure(Ty::Data(self.get_builtin_type(name), vec![]))
    }
}

pub enum Exclusivity {
    Name,
    Type,
    None,
}

#[derive(Clone, Debug, Default)]
pub struct NameScope {
    pub parent_id: Option<ParentId>,
    pub names: UstrMap<Id>,
}

impl NameScope {
    fn new(parent_id: ParentId) -> Self {
        Self {
            parent_id: Some(parent_id),
            names: UstrMap::default(),
        }
    }
}
