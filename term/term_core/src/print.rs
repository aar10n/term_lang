use crate::core::*;
use crate::Context;

use term_print::ansi::{
    chars::{
        ARROW, COMMA, COMMA_SEP, EQUALS, LAMBDA, LBRAC, LPARN, PERIOD, PIPE_SEP, PLUS_SEP, QMARK,
        QUOTE, RBRAC, RPARN, TILDE,
    },
    ATTR, BLUE, BOLD, CYAN, DELIM, GREEN, ITALIC, KEYWORD, MAGENTA, PUNCT, RED, RESET, TAG, TITLE,
    UNDERLINE,
};
use term_print::write_bulleted_sep;
use term_print::write_indented;
use term_print::{
    format_list, write_bulleted, write_delimited_list_if_many, write_list, PrettyPrint,
    PrettyString, TABWIDTH,
};

use std::cell::RefCell;
use std::collections::{BTreeMap, HashSet};
use std::{fs::write, io};
use ustr::Ustr;

impl PrettyPrint<Context> for Expr {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        write!(out, "{tab}")?;
        match self {
            Expr::Type(ty) => ty.pretty_print(out, ctx, 0),
            Expr::Wildcard => write!(out, "{PUNCT}_{RESET}"),
            Expr::Lit(l) => l.pretty_print(out, ctx, 0),
            Expr::Sym(n) => write!(out, "{BLUE}{}{RESET}", n),
            Expr::Var(id) => write!(out, "{}", id.pretty_string(ctx)),

            Expr::Apply(a, b) => {
                write!(out, "{LPARN}")?;
                a.pretty_print(out, ctx, 0)?;
                write!(out, " ")?;
                b.pretty_print(out, ctx, 0)?;
                write!(out, "{RPARN}")?;
                Ok(())
            }
            Expr::Lambda(id, t) => {
                write!(out, "{LAMBDA}{}{PERIOD}", id.pretty_string(ctx))?;
                t.pretty_print(out, ctx, 0)
            }
            Expr::Let(b, e) => {
                b.pretty_print(out, ctx, 0)?;
                if let Some(e) = e {
                    writeln!(out, " {KEYWORD}in{RESET} ")?;
                    write_indented(out, ctx, e, level + 1, true)?;
                }
                Ok(())
            }
            Expr::Case(e, bs) => {
                write!(out, "{KEYWORD}case{RESET} ")?;
                write_indented(out, ctx, e, 0, false)?;
                writeln!(out, " {KEYWORD}of{RESET} ")?;
                write_bulleted_sep(out, ctx, bs, level + 1, "|")
            }
            Expr::Handle(e, bs) => {
                write!(out, "{KEYWORD}handle{RESET} ")?;
                write_indented(out, ctx, e, 0, false)?;
                writeln!(out, " {KEYWORD}with{RESET} ")?;
                write_bulleted_sep(out, ctx, bs, level + 1, "|")
            }
            Expr::Do(es) => {
                writeln!(out, "{KEYWORD}do{RESET}")?;
                write_bulleted(out, ctx, es, level + 1)
            }

            Expr::Span(_, box e) => e.pretty_print(out, ctx, level),
        }
    }
}

impl PrettyPrint<Context> for TyE {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        self.ty.pretty_print(out, ctx, Default::default())?;
        if !self.ef.is_pure() {
            write!(out, " {TILDE} ");
            self.ef.pretty_print(out, ctx, level)?;
        }
        Ok(())
    }
}

impl PrettyPrint<Context, RefCell<BTreeMap<PolyVarId, Ustr>>> for Ty {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        params: RefCell<BTreeMap<PolyVarId, Ustr>>,
    ) -> io::Result<()> {
        match self {
            Ty::Infer => write!(out, "{PUNCT}?{RESET}"),
            Ty::Never => write!(out, "{BLUE}never{RESET}"),
            Ty::Unit => write!(out, "{LPARN}{RPARN}"),
            Ty::Symbol(s) => write!(out, "{BLUE}{}{RESET}", s.as_str()),
            Ty::Mono(id) => write!(out, "{GREEN}{}{RESET}", id),
            Ty::Poly(id) => {
                let mut params = params.borrow_mut();
                if let Some(name) = params.get(id) {
                    write!(out, "{MAGENTA}{}{RESET}", name)
                } else {
                    let name = index_to_ident(params.len());
                    params.insert(*id, name);
                    write!(out, "{MAGENTA}{}{RESET}", name)
                }
            }
            Ty::Data(id, ts) => {
                write!(out, "{BLUE}{}{RESET}", ctx.id_as_str(*id))?;
                write_args_list(out, ctx, ts, QUOTE, " ")
            }
            Ty::Func(a, b) => {
                a.pretty_print(out, ctx, 0)?;
                write!(out, " {ARROW} ")?;
                b.pretty_print(out, ctx, 0)
            }
            Ty::Sum(ts) => write_list(out, ctx, PLUS_SEP, ts),
            Ty::Record(fields) => {
                write!(out, "{LBRAC}")?;
                let mut fields = fields.iter().peekable();
                while let Some((name, ty)) = fields.next() {
                    write!(out, "{BLUE}{}{RESET} {PUNCT}:{RESET} ", name.as_ref())?;
                    ty.pretty_print(out, ctx, 0)?;
                    if fields.peek().is_some() {
                        write!(out, "{PUNCT},{RESET} ")?;
                    }
                }
                write!(out, "{RBRAC}")
            }
            Ty::Effectful(t, f) => {
                t.pretty_print(out, ctx, Default::default());
                if !f.is_pure() {
                    write!(out, " {TILDE} ")?;
                    f.pretty_print(out, ctx, 0)?;
                }
                Ok(())
            }
        }
    }
}

impl PrettyPrint<Context> for Ef {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        write!(out, "{tab}")?;
        match self {
            Ef::Infer => write!(out, "{PUNCT}_{RESET}"),
            Ef::Pure => write!(out, "{PUNCT}pure{RESET}"),
            Ef::Mono(id) => write!(out, "{GREEN}{}{RESET}", id),
            Ef::Poly(id) => write!(out, "{MAGENTA}{}{RESET}", id),
            Ef::Effect(id, ts) => {
                id.pretty_print(out, ctx, 0)?;
                write_args_list(out, ctx, ts, QUOTE, " ")
            }
            Ef::Union(ts) => write_list(out, ctx, PIPE_SEP, ts),
        }
    }
}

impl PrettyPrint<Context> for (Expr, Expr) {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        write!(out, "{tab}")?;
        self.0.pretty_print(out, ctx, 0)?;
        write!(out, " {PUNCT}->{RESET} ")?;
        self.1.pretty_print(out, ctx, 0)
    }
}

impl PrettyPrint<Context> for (Ef, Expr) {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        write!(out, "{tab}")?;
        self.0.pretty_print(out, ctx, 0)?;
        write!(out, " {PUNCT}->{RESET} ")?;
        self.1.pretty_print(out, ctx, 0)
    }
}

impl PrettyPrint<Context> for Bind {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        match self {
            Bind::NonRec(p, e) => {
                write!(out, "{PUNCT}let{RESET} ")?;
                p.pretty_print(out, ctx, level)?;
                write!(out, " {PUNCT}={RESET} ")?;
                e.pretty_print(out, ctx, level)
            }
            Bind::Rec(n, e) => {
                write!(out, "{PUNCT}let{RESET} ")?;
                write!(out, "{BLUE}{}{RESET}", ctx.id_as_str(*n))?;
                write!(out, " {PUNCT}={RESET} ")?;
                e.pretty_print(out, ctx, level)
            }
        }
    }
}

impl PrettyPrint<Context> for Constraint {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        match self {
            Constraint::Empty => Ok(()),
            Constraint::Eq(a, b) => {
                a.pretty_print(out, ctx, level)?;
                write!(out, " {EQUALS} ")?;
                b.pretty_print(out, ctx, level)
            }
            Constraint::Class(id, tys) => {
                write!(out, "{BLUE}{}{RESET}", ctx.id_as_str(*id));
                write_args_list(out, ctx, tys, QUOTE, " ")
            }
        }
    }
}

impl PrettyPrint<Context> for Lit {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        match self {
            Lit::Unit => write!(out, "{LPARN}{RPARN}"),
            Lit::Bool(b) => write!(out, "{MAGENTA}{}{RESET}", b),
            Lit::Int(i) => write!(out, "{CYAN}{}{RESET}", i),
            Lit::Float(f) => write!(out, "{CYAN}{}{RESET}", f),
            Lit::Char(c) => write!(out, "{GREEN}{}{RESET}", c.escape_default()),
            Lit::Symbol(s) => write!(out, "{BLUE}'{}{RESET}", s),
        }
    }
}

//
//

impl PrettyPrint<Context> for Def {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        let ttab = format!("{tab}{TABWIDTH}");
        let id = id_to_string(ctx, self.id, true);
        writeln!(out, "{tab}{TAG}Def{RESET}")?;
        writeln!(out, "{ttab}{ATTR}id:{RESET} {}", id)?;
        writeln!(
            out,
            "{ttab}{ATTR}type:{RESET} {}",
            self.ty.pretty_string(ctx),
        )?;

        if self.builtin {
            writeln!(out, "{ttab}{ATTR}body:{RESET} {BOLD}<builtin>{RESET}")?;
        } else {
            writeln!(out, "{ttab}{ATTR}body:{RESET}")?;
            self.body.pretty_print(out, ctx, level + 2)?;
        }
        Ok(())
    }
}

impl PrettyPrint<Context> for Data {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        let ttab = format!("{tab}{TABWIDTH}");
        let id = id_to_string(ctx, self.id, true);
        writeln!(out, "{tab}{TAG}Data{RESET}")?;
        writeln!(out, "{ttab}{ATTR}id:{RESET} {}", id)?;
        if !self.params.is_empty() {
            write!(out, "{ttab}{ATTR}params:{RESET} ")?;
            write_list(out, ctx, ", ", &self.params)?;
            writeln!(out)?;
        }
        if !self.constraints.is_empty() {
            write!(out, "{ttab}{ATTR}constraints:{RESET} ")?;
            write_list(out, ctx, ", ", &self.constraints)?;
            writeln!(out)?;
        }
        writeln!(out, "{ttab}{ATTR}cons:{RESET}")?;
        write_bulleted(out, ctx, &self.cons, level + 2)
    }
}

impl PrettyPrint<Context> for DataCon {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        let ttab = format!("{tab}{TABWIDTH}");
        let id = id_to_string(ctx, self.id, true);
        writeln!(out, "{tab}{TAG}DataCon{RESET}")?;
        writeln!(out, "{ttab}{ATTR}id:{RESET} {}", id)?;
        if !self.fields.is_empty() {
            write!(out, "{ttab}{ATTR}fields:{RESET} ")?;
            write_list(out, ctx, ", ", &self.fields)?;
            writeln!(out)?;
        }
        Ok(())
    }
}

impl PrettyPrint<Context> for Effect {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        let ttab = format!("{tab}{TABWIDTH}");
        writeln!(out, "{tab}{TAG}Effect{RESET}")?;
        writeln!(out, "{ttab}{ATTR}id:{RESET} {}", self.id.pretty_string(ctx))?;
        if !self.params.is_empty() {
            write!(out, "{ttab}{ATTR}params:{RESET} ")?;
            write_list(out, ctx, " ", &self.params)?;
            writeln!(out)?;
        }
        if !self.constraints.is_empty() {
            write!(out, "{ttab}{ATTR}constraints:{RESET} ")?;
            write_list(out, ctx, ", ", &self.constraints)?;
            writeln!(out)?;
        }
        writeln!(out, "{ttab}{ATTR}ops:{RESET}")?;
        write_bulleted(out, ctx, &self.ops, level + 2)
    }
}

impl PrettyPrint<Context> for EffectOp {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        let ttab = format!("{tab}{TABWIDTH}");
        let id = id_to_string(ctx, self.id, true);
        writeln!(out, "{tab}{TAG}EffectOp{RESET}")?;
        writeln!(out, "{ttab}{ATTR}id:{RESET} {}", id)?;
        writeln!(out, "{ttab}{ATTR}ty:{RESET} {}", self.ty.pretty_string(ctx))
    }
}

impl PrettyPrint<Context> for Handler {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        info: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(info);
        let ttab = format!("{tab}{TABWIDTH}");
        let id = id_to_string(ctx, self.id, true);
        writeln!(out, "{tab}{TAG}Handler{RESET}")?;
        writeln!(out, "{ttab}{ATTR}id:{RESET} {}", id)?;
        writeln!(
            out,
            "{ttab}{ATTR}effect:{RESET} {}",
            self.effect_id.pretty_string(ctx)
        )?;
        if !self.params.is_empty() {
            write!(out, "{ttab}{ATTR}params:{RESET} ")?;
            write_list(out, ctx, " ", &self.params)?;
            writeln!(out)?;
        }
        if !self.constraints.is_empty() {
            write!(out, "{ttab}{ATTR}constraints:{RESET} ")?;
            write_list(out, ctx, PIPE_SEP, &self.constraints)?;
            writeln!(out)?;
        }
        writeln!(out, "{ttab}{ATTR}ops:{RESET}")?;
        for (op_id, var_id) in self.ops.iter() {
            writeln!(
                out,
                "{ttab}{tab}{DELIM}-{RESET} {} {PUNCT}=>{RESET} {}",
                Id::from(*op_id).pretty_string(ctx),
                Id::from(*var_id).pretty_string(ctx),
            )?;
        }
        Ok(())
    }
}

impl PrettyPrint<Context> for Class {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        info: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(info);
        let ttab = format!("{tab}{TABWIDTH}");
        let id = id_to_string(ctx, self.id, true);
        writeln!(out, "{tab}{TAG}Class{RESET}")?;
        writeln!(out, "{ttab}{ATTR}id:{RESET} {}", id)?;
        if !self.params.is_empty() {
            write!(out, "{ttab}{ATTR}params:{RESET} ")?;
            write_list(out, ctx, " ", &self.params)?;
            writeln!(out)?;
        }
        if !self.constraints.is_empty() {
            write!(out, "{ttab}{ATTR}constraints:{RESET} ")?;
            write_list(out, ctx, PIPE_SEP, &self.constraints)?;
            writeln!(out)?;
        }
        writeln!(out, "{ttab}{ATTR}methods:{RESET}")?;
        write_bulleted(out, ctx, &self.methods, info + 2)
    }
}

impl PrettyPrint<Context> for Method {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        info: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(info);
        let ttab = format!("{tab}{TABWIDTH}");
        let id = id_to_string(ctx, self.id, true);
        writeln!(out, "{tab}{TAG}Method{RESET}")?;
        writeln!(out, "{ttab}{ATTR}id:{RESET} {}", id)?;
        writeln!(out, "{ttab}{ATTR}ty:{RESET} {}", self.ty.pretty_string(ctx))
    }
}

impl PrettyPrint<Context> for Inst {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        let ttab = format!("{tab}{TABWIDTH}");
        let id = id_to_string(ctx, self.id, true);
        // let methods = self.methods.values().collect::<Vec<_>>();
        writeln!(out, "{tab}{TAG}Inst{RESET}")?;
        writeln!(out, "{ttab}{ATTR}id:{RESET} {}", id)?;
        if !self.params.is_empty() {
            write!(out, "{ttab}{ATTR}params:{RESET} ")?;
            write_list(out, ctx, " ", &self.params)?;
            writeln!(out)?;
        }
        if !self.constraints.is_empty() {
            write!(out, "{ttab}{ATTR}constraints:{RESET} ")?;
            write_list(out, ctx, PIPE_SEP, &self.constraints)?;
            writeln!(out)?;
        }
        writeln!(out, "{ttab}{ATTR}methods:{RESET}")?;
        write_bulleted(out, ctx, &self.methods, level + 2)
    }
}

impl PrettyPrint<Context> for Id {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        write!(out, "{}", id_to_string(ctx, *self, false))
    }
}

//

impl<Ctx> PrettyPrint<Ctx> for Context {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        _: &Ctx,
        _: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH;
        writeln!(out, "{:-^1$}", "", 80)?;
        if !self.classes.is_empty() {
            let classes = self.classes.values().collect::<Vec<_>>();
            writeln!(out, "{TITLE}Classes{RESET}")?;
            write_bulleted(out, self, &classes, 0)?;
        }
        if !self.datas.is_empty() {
            let datatys = self.datas.values().collect::<Vec<_>>();
            writeln!(out, "{TITLE}Data Types{RESET}")?;
            write_bulleted(out, self, &datatys, 0)?;
        }
        if !self.effects.is_empty() {
            let effects = self.effects.values().collect::<Vec<_>>();
            writeln!(out, "{TITLE}Effects{RESET}")?;
            write_bulleted(out, self, &effects, 0)?;
        }
        if !self.defs.is_empty() {
            let defs = self.defs.values().collect::<Vec<_>>();
            writeln!(out, "{TITLE}Definitions{RESET}")?;
            write_bulleted(out, self, &defs, 0)?;
        }
        if !self.handlers.is_empty() {
            let handlers = self.handlers.values().collect::<Vec<_>>();
            writeln!(out, "{TITLE}Handlers{RESET}")?;
            write_bulleted(out, self, &handlers, 0)?;
        }
        if !self.insts.is_empty() {
            let insts = self.insts.values().collect::<Vec<_>>();
            writeln!(out, "{TITLE}Instances{RESET}")?;
            write_bulleted(out, self, &insts, 0)?;
        }
        writeln!(out, "{:-^1$}", "", 80)?;

        Ok(())
    }
}

//

macro_rules! impl_pretty_print_for_id {
    ($id:ident) => {
        impl PrettyPrint<Context> for $id {
            fn pretty_print<Output: io::Write>(
                &self,
                out: &mut Output,
                ctx: &Context,
                info: usize,
            ) -> io::Result<()> {
                write!(out, "{CYAN}{}{RESET}", ctx.id_as_str(*self))
            }
        }
    };
}

impl_pretty_print_for_id!(ClassId);
impl_pretty_print_for_id!(DataId);
impl_pretty_print_for_id!(DataConId);
impl_pretty_print_for_id!(EffectId);
impl_pretty_print_for_id!(DeclId);
impl_pretty_print_for_id!(HandlerId);
impl_pretty_print_for_id!(InstId);
// impl_pretty_print_for_id!(MonoVarId);
impl_pretty_print_for_id!(PolyVarId);
impl_pretty_print_for_id!(VarId);

fn write_args_list<I, T, S>(
    out: &mut impl io::Write,
    ctx: &Context,
    args: &I,
    first_sep: S,
    arg_sep: S,
) -> io::Result<()>
where
    T: PrettyPrint<Context>,
    S: AsRef<str>,
    I: IntoIterator<Item = T>,
    for<'a> &'a I: IntoIterator<Item = &'a T>,
{
    if args.into_iter().peekable().peek().is_some() {
        write!(out, "{}", first_sep.as_ref())?;
    }
    write_delimited_list_if_many(out, ctx, args, " ", LPARN, RPARN)
}

fn id_to_string<I>(ctx: &Context, id: I, is_decl: bool) -> String
where
    I: Into<Id> + Copy,
{
    let (raw, color) = id_to_color(id.into());
    if is_decl {
        format!("{color}{}<{}>{RESET}", ctx.id_as_str(id), raw)
    } else {
        format!("{color}{}<{}>{RESET}", ctx.id_as_str(id), raw)
    }
}

fn id_to_color(id: Id) -> (usize, &'static str) {
    match id {
        Id::Class(id) => (id.raw, CYAN),
        Id::Data(id) => (id.raw, CYAN),
        Id::DataCon(id) => (id.raw, ""),
        Id::Effect(id) => (id.raw, CYAN),
        Id::EffectOp(id) => (id.raw, RED),
        Id::Decl(id) => (id.raw, ""),
        Id::Handler(id) => (id.raw, BLUE),
        Id::Inst(id) => (id.raw, BLUE),
        Id::MonoVar(id) => (id.raw, GREEN),
        Id::PolyVar(id) => (id.raw, MAGENTA),
        Id::Var(id) => (id.raw, ""),
    }
}

/// maps integer to a lowercase identifier `a`, `b`, `c`, ...
fn index_to_ident(i: usize) -> Ustr {
    Ustr::from(&((b'a' + (i % 26) as u8) as char).to_string())
}
