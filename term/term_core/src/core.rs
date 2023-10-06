use term_common::{declare_child_id, declare_id, declare_union_id};
pub use term_common::{span::Span, P};

use std::collections::{BTreeMap, HashSet};
use std::ops::BitOr;
use ustr::Ustr;

/// A type class id.
declare_id!(ClassId);
/// A data type id.
declare_id!(DataId);
/// A data constructor id.
declare_child_id!(DataConId, DataId);
/// An effect type id.
declare_id!(EffectId);
/// An effect operation id.
declare_child_id!(EffectOpId, EffectId);
/// A declaration id.
declare_id!(DeclId);
/// An effect handler id.
declare_id!(HandlerId);
/// A class instance id.
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
    Data(DataId),
    DataCon(DataConId),
    Effect(EffectId),
    EffectOp(EffectOpId),
    Decl(DeclId),
    Handler(HandlerId),
    Inst(InstId),
    MonoVar(MonoVarId),
    PolyVar(PolyVarId),
    Var(VarId),
});

/// An generalized expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expr {
    /// Wildcard.
    Wildcard,
    /// Literal.
    Lit(Lit),
    /// Symbol.
    Sym(Ustr),
    /// Variable.
    Var(VarId),
    /// Effect constructor.
    EfCon(EffectId, Vec<TyE>),
    /// Type
    Type(P<TyE>),

    /// Application (a b).
    Apply(P<Expr>, P<Expr>),
    /// Lambda abstraction (λx.a).
    Lambda(VarId, P<Expr>),
    /// Let binding (x = a in b).
    Let(Bind, Option<P<Expr>>),
    /// Case expression.
    Case(P<Expr>, Vec<(Expr, Expr)>),
    /// Do expression.
    Do(Vec<Expr>),
}

impl Expr {
    pub fn unit() -> Self {
        Self::Lit(Lit::Unit)
    }

    pub fn is_var(&self) -> bool {
        matches!(self, Self::Var(_))
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
    pub fn new(ty: Ty, ef: Ef, cs: Vec<Constraint>) -> Self {
        Self { ty, ef, cs }
    }

    pub fn pure(ty: Ty) -> Self {
        Self {
            ty,
            ef: Ef::Pure,
            cs: vec![],
        }
    }

    pub fn unit(ef: Ef) -> Self {
        Self {
            ty: Ty::Unit,
            ef,
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

    pub fn infer_ef(ty: Ty) -> Self {
        Self {
            ty,
            ef: Ef::Infer,
            cs: vec![],
        }
    }

    pub fn is_concrete(&self) -> bool {
        self.ty.is_concrete() && self.ef.is_concrete()
    }

    pub fn has_effect(&self) -> bool {
        !self.ef.is_pure()
    }

    pub fn into_tuple(self) -> (Ty, Ef, Vec<Constraint>) {
        (self.ty, self.ef, self.cs)
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
    /// Symbolic constant.
    Symbol(Ustr),
    /// Mono type variable.
    Mono(MonoVarId),
    /// Poly type variable.
    Poly(PolyVarId),
    /// A data type.
    Data(DataId, Vec<TyE>),
    /// Function type (A -> B).
    Func(P<TyE>, P<TyE>),
    /// Sum type (A + B). invariant: len >= 2
    Sum(Vec<TyE>),
    /// Record type ({a: A, b: B}). invariant: len >= 1
    Record(BTreeMap<Ustr, TyE>),
    /// Effectful type (t ~ e).
    Effectful(P<Ty>, Ef),
}

impl Ty {
    pub fn is_const(&self) -> bool {
        matches!(self, Self::Unit | Self::Symbol(_))
    }

    pub fn is_concrete(&self) -> bool {
        match self {
            Self::Infer | Self::Mono(_) | Self::Poly(_) => false,
            Self::Func(box a, box b) => a.is_concrete() && b.is_concrete(),
            Self::Sum(ts) => ts.iter().all(|t| t.is_concrete()),
            Self::Record(fs) => fs.values().all(|t| t.is_concrete()),
            Self::Effectful(box t, _) => t.is_concrete(),
            _ => true,
        }
    }

    pub fn has_effect(&self) -> bool {
        match self {
            Self::Func(a, b) => a.has_effect() || b.has_effect(),
            Self::Sum(ts) => ts.iter().any(|t| t.has_effect()),
            Self::Record(fs) => fs.values().any(|t| t.has_effect()),
            Self::Effectful(_, _) => true,
            _ => false,
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
    Union(Vec<Ef>),
}

impl Ef {
    pub fn is_pure(&self) -> bool {
        matches!(self, Self::Pure)
    }

    pub fn is_concrete(&self) -> bool {
        match self {
            Self::Infer | Self::Mono(_) | Self::Poly(_) => false,
            Self::Effect(_, ts) => ts.iter().all(|t| t.is_concrete()),
            Self::Union(fs) => fs.iter().all(|f| f.is_concrete()),
            _ => true,
        }
    }

    pub fn into_hashset(self) -> HashSet<Ef> {
        match self {
            Self::Pure => HashSet::default(),
            Self::Union(fs) => fs.into_iter().flat_map(|f| f.into_hashset()).collect(),
            f => vec![f].into_iter().collect(),
        }
    }
}

impl BitOr for Ef {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Pure, f) | (f, Self::Pure) => f,
            (Self::Union(mut ts), f) | (f, Self::Union(mut ts)) => {
                ts.push(f);
                Self::Union(ts)
            }
            (lhs, rhs) => Self::Union(vec![lhs, rhs]),
        }
    }
}

impl From<HashSet<Ef>> for Ef {
    fn from(mut set: HashSet<Ef>) -> Self {
        if set.is_empty() {
            Self::Pure
        } else if set.len() == 1 {
            set.into_iter().next().unwrap()
        } else {
            Self::Union(set.into_iter().collect())
        }
    }
}

/// A name binding.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Bind {
    /// A non-recursive binding (x = a).
    NonRec(P<Expr>, P<Expr>),
    /// A recursive binding (f = a).
    Rec(VarId, P<Expr>),
}

/// A type constraint.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constraint {
    /// Empty constraint.
    Empty,
    /// An equality constraint.
    Eq(P<TyE>, P<TyE>),
    /// A type class constraint.
    Class(ClassId, Vec<TyE>),
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
            Self::Bool(_) => Ty::Symbol(Ustr::from("Bool")),
            Self::Int(_) => Ty::Symbol(Ustr::from("Int")),
            Self::Float(_) => Ty::Symbol(Ustr::from("Float")),
            Self::Char(_) => Ty::Symbol(Ustr::from("Char")),
            Self::Symbol(s) => Ty::Symbol(*s),
        }
    }
}

//
//
//

/// A definition.
#[derive(Clone, Debug)]
pub struct Def {
    /// The variable id.
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

/// A data type.
#[derive(Clone, Debug)]
pub struct Data {
    /// The data type id.
    pub id: DataId,
    /// Data type parameters.
    pub params: Vec<PolyVarId>,
    /// Constraints
    pub constraints: Vec<Constraint>,
    /// Data type constructors.
    pub cons: Vec<DataCon>,
}

/// A data constructor.
#[derive(Clone, Debug)]
pub struct DataCon {
    /// The data constructor id.
    pub id: DataConId,
    /// The constructor function var id.
    pub var_id: VarId,
    /// Data constructor fields.
    pub fields: Vec<TyE>,
}

/// An effect type.
#[derive(Clone, Debug)]
pub struct Effect {
    /// The effect type id.
    pub id: EffectId,
    /// Effect type parameters.
    pub params: Vec<PolyVarId>,
    /// Constraints
    pub constraints: Vec<Constraint>,
    /// Effect type operations.
    pub ops: Vec<EffectOp>,
    /// Effect handlers.
    pub handlers: Vec<HandlerId>,
    /// Default handler.
    pub default: Option<HandlerId>,
}

/// An effect operation.
#[derive(Clone, Debug)]
pub struct EffectOp {
    /// The operation id.
    pub id: EffectOpId,
    /// Effect operation type.
    pub ty: TyE,
}

/// An effect handler.
#[derive(Clone, Debug)]
pub struct Handler {
    /// The handler id.
    pub id: HandlerId,
    /// Type parameters.
    pub params: Vec<TyE>,
    /// Constraints.
    pub constraints: Vec<Constraint>,
    /// Handler operations.
    pub ops: Vec<VarId>,
}

/// A type class.
#[derive(Clone, Debug)]
pub struct Class {
    /// The class id.
    pub id: ClassId,
    /// Class type parameters.
    pub params: Vec<PolyVarId>,
    /// Constraints.
    pub constraints: Vec<Constraint>,
    /// Class methods.
    pub methods: Vec<Method>,
    /// Instances of class.
    pub insts: Vec<InstId>,
}

/// A type class method.
#[derive(Clone, Debug)]
pub struct Method {
    /// The method id.
    pub id: DeclId,
    /// Method type.
    pub ty: TyE,
}

/// A type class instance.
#[derive(Clone, Debug)]
pub struct Inst {
    /// The instance id.
    pub id: InstId,
    /// Type parameters.
    pub params: Vec<TyE>,
    /// Constraints.
    pub constraints: Vec<Constraint>,
    /// Instance methods.
    pub methods: Vec<VarId>,
}
