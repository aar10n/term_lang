use crate::{node, Context};
use term_common as common;
use term_core as core;

use common::{declare_id, declare_union_id, impl_spanned};
pub use common::{
    span::{Span, Spanned},
    P,
};
use core::{DeclId, EffectId, EffectOpId, Id, InstId, VarId};
pub use node::Node;

pub use either::{Either, Either::*};
use std::ops::Deref;
use ustr::{ustr, Ustr};

/// A module.
#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub items: Vec<Item>,
}

/// An item in a module.
pub type Item = Node<ItemKind>;

#[derive(Clone, Debug, PartialEq)]
pub enum ItemKind {
    /// A meta command.
    Command(Ustr, String /*args*/),
    /// A data type declaration.
    DataDecl(P<DataDecl>),
    /// An effect type declaration.
    EffectDecl(P<EffectDecl>),
    /// An effect handler.
    EffectHandler(P<EffectHandler>),
    /// A type class declaration.
    ClassDecl(P<ClassDecl>),
    /// A type class instance declaration.
    ClassInst(P<ClassInst>),
    /// A variable type declaration.
    VarDecl(Either<VarDecl, DeclId>),
    /// An expression.
    Expr(P<Expr>),
}

/// A type.
pub type Ty = Node<TyKind>;

impl Ty {
    pub fn infer() -> Self {
        Self::new(TyKind::Infer, Span::default())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TyKind {
    /// A type that can be inferred.
    Infer,
    /// The never type.
    Never,
    /// The unit type.
    Unit,
    /// An integer type.
    Int,
    /// A floating point type.
    Float,
    /// A boolean type.
    Bool,
    /// A character type.
    Char,
    /// A string type.
    String,
    /// A type name or instantiation.
    Name(Ident),
    /// A type instantiation.
    Inst(Ident, Vec<P<Ty>>),
    /// A function type.
    Func(P<Ty>, P<Ty>),
    /// A list type.
    List(P<Ty>),
    /// A tuple type.
    Tuple(Vec<P<Ty>>),
    /// A record type.
    Record(Vec<(Ident, P<Ty>)>),
    /// An effectful type (t ~ e).
    Effect(P<Ty>, P<Ef>),
    /// Forall quantifier.
    Forall(TyParams, P<Ty>),
}

/// An effect.
pub type Ef = Node<EfKind>;

#[derive(Clone, Debug, PartialEq)]
pub enum EfKind {
    /// Inferrable effect.
    Infer,
    /// The pure (non-) effect.
    Pure,
    /// Effect reference.
    Name(Ident, Vec<P<Ty>>),
    /// Effect union (A + B).
    Union(Vec<P<Ef>>),
}

impl EfKind {
    pub fn pure() -> Self {
        Self::Pure
    }

    pub fn is_pure(&self) -> bool {
        matches!(self, Self::Pure)
    }
}

/// A data type declaration.
#[derive(Clone, Debug, PartialEq)]
pub struct DataDecl {
    /// Data type name.
    pub name: Ident,
    /// Type parameters.
    pub ty_params: TyParams,
    /// Data constructors.
    pub cons: Vec<DataConDecl>,
    /// Declaration span.
    span: Span,
}

impl DataDecl {
    pub fn new(name: Ident, ty_params: TyParams, cons: Vec<DataConDecl>) -> Self {
        Self {
            name,
            ty_params,
            cons,
            span: Span::default(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DataConDecl {
    /// Constructor name.
    pub name: Ident,
    /// Constructor fields.
    pub fields: Vec<P<Ty>>,
    /// Constructor span.
    span: Span,
}

impl DataConDecl {
    pub fn new(name: Ident, fields: Vec<P<Ty>>) -> Self {
        Self {
            name,
            fields,
            span: Span::default(),
        }
    }
}

/// An effect type declaration.
#[derive(Clone, Debug, PartialEq)]
pub struct EffectDecl {
    /// Effect type name.
    pub name: Ident,
    /// Type parameters.
    pub ty_params: TyParams,
    /// Side effects.
    pub side_efs: Vec<P<Ef>>,
    /// Effect operations.
    pub ops: Vec<EffectOpDecl>,
    /// Effect type span.
    span: Span,
}

impl EffectDecl {
    pub fn new(
        name: Ident,
        ty_params: TyParams,
        side_efs: Vec<P<Ef>>,
        ops: Vec<EffectOpDecl>,
    ) -> Self {
        Self {
            name,
            ty_params,
            side_efs,
            ops,
            span: Span::default(),
        }
    }
}

/// An effect operation declation.
#[derive(Clone, Debug, PartialEq)]
pub struct EffectOpDecl {
    /// Operation name.
    pub name: Ident,
    /// Operation type signature.
    pub ty: P<Ty>,
    /// Operation span.
    span: Span,
}

impl EffectOpDecl {
    pub fn new(name: Ident, ty: P<Ty>) -> Self {
        Self {
            name,
            ty,
            span: Span::default(),
        }
    }
}

/// A named effect handler.
#[derive(Clone, Debug, PartialEq)]
pub struct EffectHandler {
    /// Handler name.
    pub name: Ident,
    /// Effect name.
    pub effect: Ident,
    /// Type arguments.
    pub ty_args: TyArgs,
    /// Handler operations.
    pub ops: Vec<EffectOpImpl>,
    /// Handler is the default handler.
    pub default: bool,
    /// Handler span.
    span: Span,
}

impl EffectHandler {
    pub fn new(
        name: Ident,
        effect: Ident,
        ty_args: TyArgs,
        ops: Vec<EffectOpImpl>,
        default: bool,
    ) -> Self {
        Self {
            name,
            effect,
            ty_args,
            ops,
            default,
            span: Span::default(),
        }
    }
}

/// An effect operation implementation.
#[derive(Clone, Debug, PartialEq)]
pub struct EffectOpImpl {
    /// Associated effect op id.
    pub op_id: Option<EffectOpId>,
    /// Operation name.
    pub name: Ident,
    /// Operation parameters.
    pub params: Vec<P<Pat>>,
    /// Operation body.
    pub expr: P<Expr>,
    /// Operation span.
    span: Span,
}

impl EffectOpImpl {
    pub fn new(name: Ident, params: Vec<P<Pat>>, expr: P<Expr>) -> Self {
        Self {
            op_id: None,
            name,
            params,
            expr,
            span: Span::default(),
        }
    }
}

/// A type class declaration.
#[derive(Clone, Debug, PartialEq)]
pub struct ClassDecl {
    /// Class name.
    pub name: Ident,
    /// Type parameters.
    pub ty_params: TyParams,
    /// Class members.
    pub members: Vec<MemberDecl>,
    /// Class span.
    span: Span,
}

impl ClassDecl {
    pub fn new(name: Ident, ty_params: TyParams, members: Vec<MemberDecl>) -> Self {
        Self {
            name,
            ty_params,
            members,
            span: Span::default(),
        }
    }
}

/// A class member declaration.
pub type MemberDecl = Node<MemberDeclKind>;

#[derive(Clone, Debug, PartialEq)]
pub enum MemberDeclKind {
    /// An associated type.
    AssocTy(Ident),
    /// A method declaration.
    Method(MethodDecl),
}

/// A type class method declaration.
#[derive(Clone, Debug, PartialEq)]
pub struct MethodDecl {
    /// Method name.
    pub name: Ident,
    /// Type parameters.
    pub ty_params: TyParams,
    /// Function type.
    pub ty: P<Ty>,
    /// Instance span.
    span: Span,
}

impl MethodDecl {
    pub fn new(name: Ident, ty_params: TyParams, ty: P<Ty>) -> Self {
        Self {
            name,
            ty_params,
            ty,
            span: Span::default(),
        }
    }
}

/// A type class instance.
#[derive(Clone, Debug, PartialEq)]
pub struct ClassInst {
    /// Associated inst id.
    pub id: Option<InstId>,
    /// Type class.
    pub class: Ident,
    /// Type arguments.
    pub ty_args: TyArgs,
    /// Instance members.
    pub members: Vec<MemberImpl>,
    /// Instance span.
    span: Span,
}

impl ClassInst {
    pub fn new(class: Ident, ty_args: TyArgs, members: Vec<MemberImpl>) -> Self {
        Self {
            id: None,
            class,
            ty_args,
            members,
            span: Span::default(),
        }
    }
}

/// A type class member implementation.
pub type MemberImpl = Node<MemberImplKind>;

#[derive(Clone, Debug, PartialEq)]
pub enum MemberImplKind {
    /// Associated type definition.
    AssocTy(Ident, P<Ty>),
    /// A method implementation.
    Method(MethodImpl),
}

/// A type class method implementation.
#[derive(Clone, Debug, PartialEq)]
pub struct MethodImpl {
    /// Function name.
    pub name: Ident,
    /// Resolved class method decl id.
    pub method_id: Option<DeclId>,
    /// Function parameters.
    pub params: Vec<P<Pat>>,
    /// Impl expression.
    pub expr: P<Expr>,
    /// Instance span.
    span: Span,
}

impl MethodImpl {
    pub fn new(name: Ident, params: Vec<P<Pat>>, expr: P<Expr>) -> Self {
        Self {
            name,
            method_id: None,
            params,
            expr,
            span: Span::default(),
        }
    }
}

/// A variable declaration.
#[derive(Clone, Debug, PartialEq)]
pub struct VarDecl {
    /// Variable name.
    pub name: Ident,
    /// Variable type.
    pub ty: P<Ty>,
    /// Variable span.
    span: Span,
}

impl VarDecl {
    pub fn new(name: Ident, ty: P<Ty>) -> Self {
        Self {
            name,
            ty,
            span: Span::default(),
        }
    }
}

/// A constrained list of type parameters.
#[derive(Clone, Debug, PartialEq)]
pub struct TyParams {
    /// Parameter names.
    pub params: Vec<Ident>,
    /// Additional constraints.
    pub constraints: Vec<Constraint>,
    /// Type parameters span.
    span: Span,
}

impl TyParams {
    pub fn new(params: Vec<Ident>, constraints: Vec<Constraint>) -> Self {
        Self {
            params,
            constraints,
            span: Span::default(),
        }
    }
}

impl Default for TyParams {
    fn default() -> Self {
        Self {
            params: Vec::new(),
            constraints: Vec::new(),
            span: Span::default(),
        }
    }
}

/// A full or partially specified type parameters list.
#[derive(Clone, Debug, PartialEq)]
pub struct TyArgs {
    /// All arguments.
    pub args: Vec<P<Ty>>,
    /// Unspecified params in the args.
    pub params: Vec<Ident>,
    /// Additional constraints.
    pub constraints: Vec<Constraint>,
    /// Type parameters span.
    span: Span,
}

impl TyArgs {
    pub fn new(args: Vec<P<Ty>>, params: Vec<Ident>, constraints: Vec<Constraint>) -> Self {
        Self {
            args,
            params,
            constraints,
            span: Span::default(),
        }
    }
}

impl Default for TyArgs {
    fn default() -> Self {
        Self::new(vec![], vec![], vec![])
    }
}

/// A type constraint.
pub type Constraint = Node<ConstraintKind>;

#[derive(Clone, Debug, PartialEq)]
pub enum ConstraintKind {
    /// Type class constraint.
    Class(Ident, Vec<P<Ty>>),
}

/// An expression.
pub type Expr = Node<ExprKind>;

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    /// Expression application.
    Apply(P<Expr>, P<Expr>),
    /// A binary operation.
    Binary(BinOp, P<Expr>, P<Expr>),
    /// A unary operation.
    Unary(UnOp, P<Expr>),
    /// Case expression.
    Case(Case),
    /// Handle expresion.
    Handle(Handle),
    /// Do expression.
    Do(Do),
    /// If expression.
    If(If),
    /// A function binding.
    Func(Func),
    /// An anonymous function.
    Lambda(Lambda),
    /// A variable binding.
    Var(Var),
    /// A list expression.
    List(Vec<P<Expr>>),
    /// A tuple expression.
    Tuple(Vec<P<Expr>>),
    /// A record expression.
    Record(Vec<(Ident, P<Expr>)>),
    /// A literal value.
    Lit(Lit),
    /// An identifier.
    Ident(Ident),
}

impl ExprKind {
    pub fn unit() -> Self {
        ExprKind::Lit(Lit::new(LitKind::Unit, Span::default()))
    }
}

/// A pattern.
pub type Pat = Node<PatKind>;

impl Pat {
    pub fn is_var(&self) -> bool {
        matches!(self.kind, PatKind::Ident(_))
    }

    pub fn collect_vars(&self, vars: &mut Vec<VarId>) {
        use PatKind::*;
        match &self.kind {
            DataCon(_, ps) => ps.iter().for_each(|p| p.collect_vars(vars)),
            Tuple(ps) => ps.iter().for_each(|p| p.collect_vars(vars)),
            List(ps) => ps.iter().for_each(|p| p.collect_vars(vars)),
            Cons(x, xs) => {
                x.collect_vars(vars);
                xs.collect_vars(vars);
            }
            Ident(ident) => {
                if let Some(id) = ident.id {
                    vars.push(id.var_id());
                }
            }
            _ => {}
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum PatKind {
    /// The wildcard pattern.
    Wildcard,
    /// The unit pattern.
    Unit,
    /// A data constructor pattern.
    DataCon(Ident, Vec<P<Pat>>),
    /// A tuple pattern.
    Tuple(Vec<P<Pat>>),
    /// A list pattern.
    List(Vec<P<Pat>>),
    /// A record pattern.
    Record(Vec<(Ident, P<Pat>)>),
    /// A cons pattern.
    Cons(P<Pat>, P<Pat>),
    /// A literal pattern.
    Lit(Lit),
    /// An identifier binding.
    Ident(Ident),
}

/// A block expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Do {
    /// Block expressions.
    pub exprs: Vec<P<Expr>>,
    /// Block span.
    span: Span,
}

impl Do {
    pub fn new(exprs: Vec<P<Expr>>) -> Self {
        Self {
            exprs,
            span: Span::default(),
        }
    }
}

/// A case expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Case {
    /// Case expression.
    pub expr: P<Expr>,
    /// Case alternatives.
    pub alts: Vec<CaseAlt>,
    /// Case span.
    span: Span,
}

impl Case {
    pub fn new(expr: P<Expr>, alts: Vec<CaseAlt>) -> Self {
        Self {
            expr,
            alts,
            span: Span::default(),
        }
    }
}

/// A case alternative.
#[derive(Clone, Debug, PartialEq)]
pub struct CaseAlt {
    /// Alternative pattern.
    pub pat: P<Pat>,
    /// Alternative expression.
    pub expr: P<Expr>,
    /// Alternative span.
    span: Span,
}

impl CaseAlt {
    pub fn new(pat: P<Pat>, expr: P<Expr>) -> Self {
        Self {
            pat,
            expr,
            span: Span::default(),
        }
    }
}

/// A handle expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Handle {
    /// Handle expression.
    pub expr: P<Expr>,
    /// Handle alternatives.
    pub alts: Vec<HandleAlt>,
    /// Case span.
    span: Span,
}

impl Handle {
    pub fn new(expr: P<Expr>, alts: Vec<HandleAlt>) -> Self {
        Self {
            expr,
            alts,
            span: Span::default(),
        }
    }
}

/// A handle alternative.
#[derive(Clone, Debug, PartialEq)]
pub struct HandleAlt {
    /// Alternative effect.
    pub ef: P<Ef>,
    /// Alternative expression.
    pub expr: P<Expr>,
    /// Alternative span.
    span: Span,
}

impl HandleAlt {
    pub fn new(ef: P<Ef>, expr: P<Expr>) -> Self {
        Self {
            ef,
            expr,
            span: Span::default(),
        }
    }
}

/// An if expression.
#[derive(Clone, Debug, PartialEq)]
pub struct If {
    /// Condition expression.
    pub cond: P<Expr>,
    /// Then expression.
    pub then: P<Expr>,
    /// Else expression.
    pub else_: P<Expr>,
    /// If span.
    span: Span,
}

impl If {
    pub fn new(cond: P<Expr>, then: P<Expr>, else_: P<Expr>) -> Self {
        Self {
            cond,
            then,
            else_,
            span: Span::default(),
        }
    }
}

/// A function expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Func {
    /// Function name.
    pub name: Ident,
    /// Function parameters.
    pub params: Vec<P<Pat>>,
    /// Function body.
    pub body: P<Expr>,
    /// Function span.
    span: Span,
}

impl Func {
    pub fn new(name: Ident, params: Vec<P<Pat>>, body: P<Expr>) -> Self {
        Self {
            name,
            params,
            body,
            span: Span::default(),
        }
    }
}

/// A lambda expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Lambda {
    /// Lambda parameters.
    pub params: Vec<P<Pat>>,
    /// Lambda body.
    pub body: P<Expr>,
    /// Lambda span.
    span: Span,
}

impl Lambda {
    pub fn new(params: Vec<P<Pat>>, body: P<Expr>) -> Self {
        Self {
            params,
            body,
            span: Span::default(),
        }
    }
}

/// A var expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Var {
    /// Var pattern.
    pub pat: P<Pat>,
    /// Var expr.
    pub expr: P<Expr>,
    /// Var span.
    span: Span,
}

impl Var {
    pub fn new(pat: P<Pat>, expr: P<Expr>) -> Self {
        Self {
            pat,
            expr,
            span: Span::default(),
        }
    }
}

/// A literal value.
pub type Lit = Node<LitKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum LitKind {
    Unit,
    Bool(bool),
    Int(u64),
    Float(f64),
    Char(char),
    String(String),
}

/// An identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ident {
    /// Raw identifier.
    pub raw: Ustr,
    /// Associated id.
    pub id: Option<Id>,
    /// Identifier span.
    span: Span,
}

impl From<&str> for Ident {
    fn from(value: &str) -> Self {
        Self {
            raw: Ustr::from(value),
            id: None,
            span: Span::default(),
        }
    }
}

impl Deref for Ident {
    type Target = Ustr;

    fn deref(&self) -> &Self::Target {
        &self.raw
    }
}

/// A binary operator.
#[rustfmt::skip]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add, Sub, Mul, Div, Mod,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or,
}

impl BinOp {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
            Self::Eq => "==",
            Self::Ne => "!=",
            Self::Lt => "<",
            Self::Le => "<=",
            Self::Gt => ">",
            Self::Ge => ">=",
            Self::And => "&&",
            Self::Or => "||",
        }
    }
}

/// A unary operator.
#[rustfmt::skip]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Effect,
    Pos, Neg, 
    Not,
}

impl UnOp {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Effect => "~",
            Self::Pos => "+",
            Self::Neg => "-",
            Self::Not => "!",
        }
    }
}

impl_spanned!(DataDecl);
impl_spanned!(DataConDecl);
impl_spanned!(EffectDecl);
impl_spanned!(EffectOpDecl);
impl_spanned!(EffectHandler);
impl_spanned!(EffectOpImpl);
impl_spanned!(ClassDecl);
impl_spanned!(MethodDecl);
impl_spanned!(ClassInst);
impl_spanned!(MethodImpl);
impl_spanned!(VarDecl);
impl_spanned!(TyParams);
impl_spanned!(Do);
impl_spanned!(Case);
impl_spanned!(CaseAlt);
impl_spanned!(Handle);
impl_spanned!(HandleAlt);
impl_spanned!(If);
impl_spanned!(Func);
impl_spanned!(Lambda);
impl_spanned!(Var);
impl_spanned!(Ident);
