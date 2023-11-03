use term_common::{declare_child_id, declare_id, declare_id_collection, declare_union_id};
pub use term_common::{span::Span, P};

use either::*;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::hash::Hash;
use std::ops::{BitOr, Deref};
use ustr::Ustr;

/// A type class id.
declare_id!(ClassId);
/// A declaration id.
declare_id!(DeclId);
/// A data type id.
declare_id!(DataId);
/// A data constructor id.
declare_child_id!(DataConId, DataId);
/// An effect type id.
declare_id!(EffectId);
/// An effect operation id.
declare_child_id!(EffectOpId, EffectId);
/// Effect handler id.
declare_id!(HandlerId);
/// Class instance id.
declare_id!(InstId);
/// A poly type variable id.
declare_id!(PolyVarId);
/// A mono type variable id.
declare_id!(MonoVarId);
/// A variable id.
declare_id!(VarId);

/// Top level id.
declare_union_id!(Id {
    Class(ClassId),
    Decl(DeclId),
    Data(DataId),
    DataCon(DataConId),
    Effect(EffectId),
    EffectOp(EffectOpId),
    Handler(HandlerId),
    Inst(InstId),
    MonoVar(MonoVarId),
    PolyVar(PolyVarId),
    Var(VarId),
});

declare_id_collection! {
    pub Ids {
        ClassId,
        DeclId,
        DataId,
        DataConId(DataId),
        EffectId,
        EffectOpId(EffectId),
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

/// A type + effect + constraints.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyE {
    pub ty: Ty,
    pub ef: Ef,
    pub cs: Vec<Constraint>,
}

impl TyE {
    // Constructors
    //

    pub fn new(ty: Ty, ef: Ef, cs: Vec<Constraint>) -> Self {
        Self { ty, ef, cs }
    }

    pub fn simple(ty: Ty, ef: Ef) -> Self {
        Self::new(ty, ef, vec![])
    }

    pub fn pure(ty: Ty) -> Self {
        Self {
            ty,
            ef: Ef::Pure,
            cs: vec![],
        }
    }

    pub fn infer() -> Self {
        Self {
            ty: Ty::Infer,
            ef: Ef::Infer,
            cs: vec![],
        }
    }

    pub fn func(a: TyE, b: TyE) -> Self {
        let (rt, f) = b.split_ef();
        Self {
            ty: Ty::Func(a.into(), rt.into()),
            ef: f,
            cs: vec![],
        }
    }

    pub fn nary_func(ts: impl IntoIterator<Item = TyE>, rt: TyE) -> Self {
        let ts = ts.into_iter().collect::<Vec<_>>();
        if ts.is_empty() {
            Self::func(TyE::pure(Ty::Unit), rt.into())
        } else {
            ts.into_iter().fold(rt, |f_t, t| TyE::func(t, f_t))
        }
    }

    pub fn record(fs: impl IntoIterator<Item = (Ustr, TyE)>) -> Self {
        let fs = fs.into_iter().collect::<BTreeMap<_, _>>();
        Self::new(Ty::Record(fs.clone()), Ef::Pure, vec![])
    }

    // Predicates
    //

    pub fn is_monomorphic(&self) -> bool {
        self.ty.is_monomorphic() && self.ef.is_monomorphic()
    }

    pub fn is_polymorphic(&self) -> bool {
        !self.is_monomorphic()
    }

    pub fn is_concrete(&self) -> bool {
        self.ty.is_concrete() && self.ef.is_concrete()
    }

    pub fn has_effect(&self) -> bool {
        !self.ef.is_pure()
    }

    // Accessors
    //

    pub fn ret_ty(&self) -> Option<&TyE> {
        self.ty.return_ty()
    }

    // Modifiers
    //

    pub fn with_ef(self, ef: Ef) -> Self {
        Self::new(self.ty, self.ef | ef, self.cs)
    }

    pub fn with_cs(self, iter: impl IntoIterator<Item = Constraint>) -> Self {
        let mut cs = self.cs;
        cs.extend(iter);
        Self::new(self.ty, self.ef, cs)
    }

    pub fn split_ef(self) -> (TyE, Ef) {
        (TyE::new(self.ty, Ef::Pure, self.cs), self.ef)
    }

    pub fn into_tuple(self) -> (Ty, Ef, Vec<Constraint>) {
        (self.ty, self.ef, self.cs)
    }
}

impl Deref for TyE {
    type Target = Ty;

    fn deref(&self) -> &Self::Target {
        &self.ty
    }
}

/// A type.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ty {
    /// Inferrable type.
    Infer,
    /// The never type.
    Never,
    /// The unit type.
    Unit,
    /// Symbolic type.
    Sym(Ustr),
    /// Mono type variable.
    Mono(MonoVarId),
    /// Poly type variable.
    Poly(PolyVarId),
    /// A data type.
    Data(DataId, Vec<TyE>),
    /// Function type (A -> B).
    Func(P<TyE>, P<TyE>),
    /// Record type ({a: A, b: B}).
    Record(BTreeMap<Ustr, TyE>),
    /// Effectful type (t ~ e).
    Effectful(P<Ty>, Ef),
}

impl Ty {
    // Constructors
    //

    pub fn sym(s: impl AsRef<str>) -> Self {
        Self::Sym(Ustr::from(s.as_ref()))
    }

    pub fn func(a: TyE, b: TyE) -> Self {
        Self::Func(a.into(), b.into())
    }

    pub fn nary_func(ts: impl IntoIterator<Item = TyE>, rt: TyE) -> Self {
        let ts = ts.into_iter().collect::<Vec<_>>();
        if ts.is_empty() {
            Self::func(TyE::pure(Ty::Unit), rt.into())
        } else {
            let (t, f, _) = ts
                .into_iter()
                .fold(rt, |f_t, t| TyE::func(t, f_t))
                .into_tuple();
            Ty::Effectful(t.into(), f)
        }
    }

    pub fn record(fs: impl IntoIterator<Item = (Ustr, TyE)>) -> Self {
        Self::Record(fs.into_iter().collect())
    }

    // Predicates
    //

    pub fn is_mono(&self) -> bool {
        matches!(self, Self::Mono(_))
    }

    pub fn is_monomorphic(&self) -> bool {
        match self {
            Self::Infer | Self::Poly(_) => false,
            Self::Data(_, ts) => ts.iter().all(|t| t.is_monomorphic()),
            Self::Func(box a, box b) => a.is_monomorphic() && b.is_monomorphic(),
            Self::Record(fs) => fs.values().all(|t| t.is_monomorphic()),
            Self::Effectful(box t, _) => t.is_monomorphic(),
            _ => true,
        }
    }

    pub fn is_concrete(&self) -> bool {
        match self {
            Self::Infer | Self::Mono(_) | Self::Poly(_) => false,
            Self::Data(_, ts) => ts.iter().all(|t| t.is_concrete()),
            Self::Func(box a, box b) => a.is_concrete() && b.is_concrete(),
            Self::Record(fs) => fs.values().all(|t| t.is_concrete()),
            Self::Effectful(box t, _) => t.is_concrete(),
            _ => true,
        }
    }

    pub fn has_effect(&self) -> bool {
        match self {
            Self::Data(_, ts) => ts.iter().any(|t| t.has_effect()),
            Self::Func(a, b) => a.has_effect() || b.has_effect(),
            Self::Record(fs) => fs.values().any(|t| t.has_effect()),
            Self::Effectful(_, _) => true,
            _ => false,
        }
    }

    // Accessors
    //

    pub fn return_ty(&self) -> Option<&TyE> {
        match self {
            Self::Func(_, b) => Some(b),
            _ => None,
        }
    }
}

impl Into<TyE> for Ty {
    fn into(self) -> TyE {
        TyE::pure(self)
    }
}

/// An effect.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ef {
    /// Inferrable effect.
    Infer,
    /// Pure (empty) effect
    Pure,
    /// Mono type variable.
    Mono(MonoVarId),
    /// Poly type variable.
    Poly(PolyVarId),
    /// An effect type.
    Effect(EffectId, Vec<TyE>),
    /// Effect union (A | B). invariant: len >= 2
    Union(BTreeSet<Ef>),
}

impl Ef {
    pub fn union(fs: impl IntoIterator<Item = Ef>) -> Self {
        let fs = fs.into_iter().collect::<Vec<_>>();
        if fs.is_empty() {
            Self::Pure
        } else if fs.len() == 1 {
            fs.into_iter().next().unwrap()
        } else {
            Self::Union(fs.into_iter().collect())
        }
    }

    pub fn is_pure(&self) -> bool {
        matches!(self, Self::Pure)
    }

    pub fn is_monomorphic(&self) -> bool {
        match self {
            Self::Infer | Self::Poly(_) => false,
            Self::Effect(_, ts) => ts.iter().all(|t| t.is_monomorphic()),
            Self::Union(fs) => fs.iter().all(|f| f.is_monomorphic()),
            _ => true,
        }
    }

    pub fn is_concrete(&self) -> bool {
        match self {
            Self::Infer | Self::Mono(_) | Self::Poly(_) => false,
            Self::Effect(_, ts) => ts.iter().all(|t| t.is_concrete()),
            Self::Union(fs) => fs.iter().all(|f| f.is_concrete()),
            _ => true,
        }
    }

    pub fn into_set(self) -> BTreeSet<Ef> {
        match self {
            Self::Infer | Self::Pure => BTreeSet::default(),
            Self::Union(fs) => fs.into_iter().flat_map(|f| f.into_set()).collect(),
            f => vec![f].into_iter().collect(),
        }
    }

    pub fn with_ef(self, f: Ef) -> Ef {
        let mut fs = self.into_set();
        fs.insert(f);
        Ef::from(fs)
    }
}

impl BitOr for Ef {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        self.with_ef(rhs)
    }
}

impl From<BTreeSet<Ef>> for Ef {
    fn from(mut set: BTreeSet<Ef>) -> Self {
        set.remove(&Self::Pure);
        if set.is_empty() {
            Self::Pure
        } else if set.len() == 1 {
            set.into_iter().next().unwrap()
        } else {
            Self::Union(set.into_iter().collect())
        }
    }
}

/// A type constraint.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constraint {
    /// Empty constraint.
    Empty,
    /// An equality constraint.
    Eq(P<TyE>, P<TyE>),
    /// Type class constraint.
    Class(ClassId, Vec<TyE>),
}

//
//

/// An generalized expression.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    /// Type.
    Type(P<TyE>),
    /// Literal.
    Lit(Lit),
    /// Symbol.
    Sym(Ustr),
    /// Variable.
    Var(VarId),

    /// Application (a b).
    Apply(P<Expr>, P<Expr>),
    /// Lambda abstraction (λx.a).
    Lambda(P<Expr>, P<Expr>),
    /// Case expression.
    Case(P<Expr>, Vec<Alt>),
    /// Handle expression.
    Handle(P<Expr>, Option<Vec<EfAlt>>),
    /// Do expression.
    Do(Vec<Expr>),
    /// Let binding (let \[x = a]... in b).
    Let(Vec<Bind>, Option<P<Expr>>),
    /// Record expression.
    Record(BTreeMap<Ustr, Expr>),
    /// Record select.
    RecSel(P<Expr>, Ustr),

    /// Source span info.
    Span(Span, P<Expr>),
}

impl Expr {
    // Constructors
    //

    pub fn unit() -> Self {
        Self::Lit(Lit::Unit)
    }

    pub fn ty(t: TyE) -> Self {
        Self::Type(t.into())
    }

    pub fn apply(a: Expr, b: Expr) -> Self {
        Self::Apply(a.into(), b.into())
    }

    pub fn apply_n(f: Expr, args: impl IntoIterator<Item = Expr>) -> Self {
        let args = args.into_iter().collect::<Vec<_>>();
        assert!(!args.is_empty());
        args.into_iter().fold(f, Self::apply)
    }

    pub fn lambda(p: Expr, b: Expr) -> Self {
        Self::Lambda(p.into(), b.into())
    }

    pub fn lambda_n(ps: impl IntoIterator<Item = Expr>, b: Expr) -> Self {
        let ps = ps.into_iter().collect::<Vec<_>>();
        assert!(!ps.is_empty());
        ps.into_iter().rev().fold(b, |a, b| Self::lambda(b, a))
    }

    pub fn record(fs: impl IntoIterator<Item = (Ustr, Expr)>) -> Self {
        Self::Record(fs.into_iter().collect())
    }

    // Predicates
    //

    pub fn is(e: &Self, pred: impl Fn(&Self) -> bool) -> bool {
        pred(e.inner())
    }

    pub fn is_sym(&self) -> bool {
        Self::is(self, |e| matches!(e, Self::Sym(_)))
    }

    pub fn is_var(&self) -> bool {
        Self::is(self, |e| matches!(e, Self::Var(_)))
    }

    pub fn is_app(&self) -> bool {
        Self::is(self, |e| matches!(e, Self::Apply(..)))
    }

    pub fn is_lam(&self) -> bool {
        Self::is(self, |e| matches!(e, Self::Lambda(..)))
    }

    // Accessors
    //

    pub fn inner(&self) -> &Self {
        match self {
            Expr::Span(_, ref inner) => inner,
            _ => self,
        }
    }

    pub fn inner_mut(&mut self) -> &mut Self {
        match self {
            Expr::Span(_, ref mut inner) => inner,
            _ => self,
        }
    }

    // Modifiers
    //

    pub fn with_span(self, span: Span) -> Self {
        let (e, _) = self.into_inner();
        Self::Span(span, e.into())
    }

    pub fn unwrap_inner(self) -> Self {
        match self {
            Expr::Span(_, box inner) => inner,
            _ => self,
        }
    }

    pub fn into_inner(self) -> (Self, Span) {
        match self {
            Expr::Span(s, box inner) => (inner, s),
            _ => (self, Span::default()),
        }
    }

    pub fn uncurry_apply(self) -> (Expr, Vec<Expr>) {
        let Self::Apply(box a, box b) = self.unwrap_inner() else {
            panic!("uncurry_apply: not an application");
        };

        let mut f = a;
        let mut es = vec![b];
        while f.is_app() {
            let Self::Apply(box a, box b) = f.unwrap_inner() else {
                unreachable!()
            };

            f = a;
            es.push(b);
        }
        (f, es)
    }

    pub fn uncurry_lambda(self) -> (Vec<Expr>, Expr) {
        let Self::Lambda(box a, box b) = self.unwrap_inner() else {
            panic!("uncurry_lambda: not a lambda");
        };

        let mut ps = vec![a];
        let mut e = b;
        while e.is_lam() {
            let Self::Lambda(box a, box b) = e.unwrap_inner() else {
                unreachable!()
            };

            ps.push(a);
            e = b;
        }
        ps.reverse();
        (ps, e)
    }
}

impl Hash for Expr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Type(t) => {
                state.write_u8(0);
                t.hash(state);
            }
            Self::Lit(l) => {
                state.write_u8(1);
                l.hash(state);
            }
            Self::Sym(s) => {
                state.write_u8(2);
                s.hash(state);
            }
            Self::Var(v) => {
                state.write_u8(3);
                v.hash(state);
            }
            Self::Apply(a, b) => {
                state.write_u8(4);
                a.hash(state);
                b.hash(state);
            }
            Self::Lambda(a, b) => {
                state.write_u8(5);
                a.hash(state);
                b.hash(state);
            }
            Self::Case(a, bs) => {
                state.write_u8(6);
                a.hash(state);
                bs.hash(state);
            }
            Self::Handle(a, bs) => {
                state.write_u8(7);
                a.hash(state);
                bs.hash(state);
            }
            Self::Do(es) => {
                state.write_u8(8);
                es.hash(state);
            }
            Self::Let(bs, b) => {
                state.write_u8(9);
                bs.hash(state);
                b.hash(state);
            }
            Self::Record(fs) => {
                state.write_u8(10);
                fs.hash(state);
            }
            Self::RecSel(a, b) => {
                state.write_u8(11);
                a.hash(state);
                b.hash(state);
            }
            Self::Span(_, e) => {
                e.hash(state);
            }
        }
    }
}

pub type Alt = (Expr /* pattern */, Expr);
pub type EfAlt = (Ef, Expr);

/// A name binding.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Bind {
    /// A non-recursive binding (x = a).
    NonRec(P<Expr>, P<Expr>),
    /// A recursive binding (f = a).
    Rec(VarId, P<Expr>),
}

/// A literal.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Lit {
    /// Unit literal.
    Unit,
    /// Boolean.
    Bool(bool),
    /// Integer.
    Int(u64),
    /// Float.
    Float(u64 /*f64*/),
    /// Character.
    Char(char),
    /// Symbolic constant.
    Symbol(Ustr),
}

impl Lit {
    pub fn as_ty(&self) -> Ty {
        match self {
            Self::Unit => Ty::Unit,
            Self::Bool(_) => Ty::sym("Bool"),
            Self::Int(_) => Ty::sym("Int"),
            Self::Float(_) => Ty::sym("Float"),
            Self::Char(_) => Ty::sym("Char"),
            Self::Symbol(s) => Ty::Sym(*s),
        }
    }
}

//
//
//

/// A type class.
#[derive(Clone, Debug)]
pub struct Class {
    /// Class id.
    pub id: ClassId,
    /// Class constraints.
    pub cs: Vec<Constraint>,
    /// Class declarations.
    pub decls: BTreeMap<Ustr, TyE>,
    /// Class instances.
    pub insts: Vec<VarId>,
}

impl Class {
    pub fn new(id: ClassId, cs: Vec<Constraint>, decls: BTreeMap<Ustr, TyE>) -> Self {
        Self {
            id,
            cs,
            decls,
            insts: vec![],
        }
    }
}

/// A definition.
#[derive(Clone, Debug)]
pub struct Def {
    /// Variable id.
    pub id: VarId,
    /// Variable type.
    pub ty: TyE,
    /// Variable body.
    pub body: Expr,
    /// Builtin definition.
    pub builtin: bool,
}

impl Def {
    pub fn new(id: VarId, ty: TyE, body: Expr) -> Self {
        Self {
            id,
            ty,
            body,
            builtin: false,
        }
    }

    pub fn new_builtin(id: VarId, ty: TyE) -> Self {
        Self {
            id,
            ty,
            body: Expr::Var(id),
            builtin: true,
        }
    }
}

/// An effect type.
#[derive(Clone, Debug)]
pub struct Effect {
    /// Effect id.
    pub id: EffectId,
    /// Side effects.
    pub side_efs: Vec<Ef>,
    /// Effect operations.
    pub ops: BTreeMap<Ustr, TyE>,
    /// Effect handler type.
    pub handler_ty: TyE,
    /// Effect handlers.
    pub handlers: Vec<VarId>,
    /// Default handler.
    pub default: Option<VarId>,
}

impl Effect {
    pub fn new(id: EffectId, side_efs: Vec<Ef>, ops: BTreeMap<Ustr, TyE>, handler_ty: TyE) -> Self {
        Self {
            id,
            side_efs,
            ops,
            handler_ty,
            handlers: vec![],
            default: None,
        }
    }
}
