use crate::core::*;
use crate::Context;

use term_print::ansi::{
    chars::{
        ARROW, COLON, COMMA, COMMA_SEP, EQUALS, LAMBDA, LBRAC, LBRACE, LPARN, PERIOD, PIPE_SEP,
        PLUS_SEP, QMARK, QUOTE, RBRAC, RBRACE, RPARN, TILDE,
    },
    ATTR, BLUE, BOLD, CYAN, DELIM, GREEN, ITALIC, KEYWORD, MAGENTA, PUNCT, RED, RESET, TAG, TITLE,
    UNDERLINE,
};
use term_print::{
    format_list, write_bulleted, write_bulleted_sep, write_delimited_list_if_many, write_indented,
    write_list, Mapped, PrettyPrint, PrettyString, TABWIDTH,
};

use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::{fs::write, io};
use ustr::Ustr;

impl PrettyPrint<Context> for TyE {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let params = RefCell::new(BTreeMap::new());
        if self.ef.is_pure() {
            self.ty.pretty_print(out, ctx, params.clone())?;
        } else {
            self.ty.pretty_print(out, ctx, params.clone())?;
            write!(out, " {TILDE} ");
            self.ef.pretty_print(out, ctx, params.clone())?;
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
            Ty::Sym(s) => write!(out, "{BLUE}{}{RESET}", s.as_str()),
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
                if matches!(a.ty, Ty::Func(_, _)) {
                    write!(out, "{LPARN}")?;
                    a.pretty_print(out, ctx, 0)?;
                    write!(out, "{RPARN}")?;
                } else {
                    a.pretty_print(out, ctx, 0)?;
                }

                write!(out, " {ARROW} ")?;
                b.pretty_print(out, ctx, 0)
            }
            Ty::Record(fields) => {
                write!(out, "{LBRACE}")?;
                let mut fields = fields.iter().peekable();
                while let Some((name, ty)) = fields.next() {
                    write!(out, "{BLUE}{}{RESET} {PUNCT}:{RESET} ", name.as_ref())?;
                    ty.pretty_print(out, ctx, 0)?;
                    if fields.peek().is_some() {
                        write!(out, "{PUNCT},{RESET} ")?;
                    }
                }
                write!(out, "{RBRACE}")
            }
            Ty::Effectful(t, f) => {
                if f.is_pure() {
                    t.pretty_print(out, ctx, Default::default());
                } else {
                    write!(out, "{LPARN}")?;
                    t.pretty_print(out, ctx, Default::default());
                    write!(out, " {TILDE} ")?;
                    f.pretty_print(out, ctx, Default::default())?;
                    write!(out, "{RPARN}")?;
                }
                Ok(())
            }
        }
    }
}

impl PrettyPrint<Context, RefCell<BTreeMap<PolyVarId, Ustr>>> for Ef {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        params: RefCell<BTreeMap<PolyVarId, Ustr>>,
    ) -> io::Result<()> {
        match self {
            Ef::Infer => write!(out, "{PUNCT}?{RESET}"),
            Ef::Pure => write!(out, "{PUNCT}pure{RESET}"),
            Ef::Mono(id) => write!(out, "{GREEN}{}{RESET}", id),
            Ef::Poly(id) => {
                write!(out, "{MAGENTA}{}{RESET}", id.raw)
                // let mut params = params.borrow_mut();
                // if let Some(name) = params.get(id) {
                //     write!(out, "{MAGENTA}{}{RESET}", name)
                // } else {
                //     let name = index_to_ident(params.len());
                //     params.insert(*id, name);
                //     write!(out, "{MAGENTA}{}{RESET}", name)
                // }
            }
            Ef::Effect(id, ts) => {
                id.pretty_print(out, ctx, 0)?;
                write_args_list(out, ctx, ts, QUOTE, " ")
            }
            Ef::Union(fs) => {
                let fs = fs.iter().filter(|f| !f.is_pure()).collect::<Vec<_>>();
                write_list(out, ctx, PIPE_SEP, &fs)
            }
        }
    }
}

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
            Expr::Lit(l) => l.pretty_print(out, ctx, 0),
            Expr::Sym(n) => write!(out, "{BLUE}`{}`{RESET}", n),
            Expr::Var(id) => write!(out, "{}", id.pretty_string(ctx)),

            Expr::Apply(a, b) => {
                write!(out, "{LPARN}")?;
                a.pretty_print(out, ctx, 0)?;
                write!(out, " ")?;
                b.pretty_print(out, ctx, 0)?;
                write!(out, "{RPARN}")?;
                Ok(())
            }
            Expr::Lambda(p, t) => {
                write!(out, "{LAMBDA}{}{PERIOD} ", p.pretty_string(ctx))?;
                t.pretty_print(out, ctx, 0)
            }
            Expr::Case(e, alts) => {
                write!(out, "{KEYWORD}case{RESET} ")?;
                write_indented(out, ctx, e, level, false)?;
                writeln!(out, "{tab}{KEYWORD}of{RESET} ")?;
                write_indented(
                    out,
                    ctx,
                    &Mapped::new(alts, |ctx: &_, alt: &Alt| -> String {
                        format!("{PIPE_SEP}{}\n", alt.pretty_string(ctx).trim_end())
                    }),
                    level + 1,
                    true,
                )
            }
            Expr::Handle(e, None) => {
                write!(out, "{KEYWORD}handle default{RESET} ")?;
                write_indented(out, ctx, e, level, false)?;
                Ok(())
            }
            Expr::Handle(e, Some(alts)) => {
                writeln!(out, "{KEYWORD}handle{RESET}")?;
                write_indented(out, ctx, e, level + 1, false)?;
                writeln!(out, "{tab}{KEYWORD}with{RESET}")?;
                write_indented(
                    out,
                    ctx,
                    &Mapped::new(alts, |ctx: &_, alt: &EfAlt| -> String {
                        format!("{PIPE_SEP}{}\n", alt.pretty_string(ctx).trim_end())
                    }),
                    level + 1,
                    true,
                )
            }
            Expr::Do(es) => {
                writeln!(out, "{KEYWORD}do{RESET}")?;
                for e in es {
                    e.pretty_print(out, ctx, level + 1)?;
                    writeln!(out)?;
                }
                Ok(())
            }
            Expr::Let(bs, e) => {
                write_list(out, ctx, COMMA_SEP, bs)?;
                if let Some(e) = e {
                    writeln!(out, " {KEYWORD}in{RESET} ")?;
                    write_indented(out, ctx, e, level + 1, true)?;
                }
                Ok(())
            }
            Expr::Record(fs) => {
                write!(out, "{LBRACE}")?;
                let mut fs = fs.iter().peekable();
                while let Some((name, e)) = fs.next() {
                    write!(out, "{BLUE}{}{RESET} {PUNCT}:{RESET} ", name.as_ref())?;
                    e.pretty_print(out, ctx, 0)?;
                    if fs.peek().is_some() {
                        write!(out, "{PUNCT},{RESET} ")?;
                    }
                }
                write!(out, "{RBRACE}")
            }
            Expr::RecSel(e, n) => {
                write!(out, "{}{PERIOD}{}", e.pretty_string(ctx), n)
            }

            Expr::Span(_, box e) => e.pretty_print(out, ctx, level),
        }
    }
}

impl PrettyPrint<Context> for Alt {
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

impl PrettyPrint<Context> for EfAlt {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        write!(out, "{tab}")?;
        self.0.pretty_print(out, ctx, Default::default())?;
        write!(out, " {PUNCT}~>{RESET} ")?;
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
                write!(
                    out,
                    "{KEYWORD}let{RESET} {} {EQUALS} ",
                    p.pretty_string(ctx)
                )?;
                e.pretty_print(out, ctx, 0)
            }
            Bind::Rec(n, e) => {
                write!(
                    out,
                    "{KEYWORD}let rec{RESET} {} {EQUALS} ",
                    n.pretty_string(ctx)
                )?;
                e.pretty_print(out, ctx, 0)
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

impl PrettyPrint<Context> for Class {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH;
        let ttab = format!("{tab}{TABWIDTH}");
        writeln!(out, "{:-^1$}", "", 80)?;
        writeln!(out, "{TITLE}Class{RESET}")?;
        writeln!(out, "{tab}{ATTR}id:{RESET} {}", self.id.pretty_string(ctx))?;
        if !self.cs.is_empty() {
            writeln!(
                out,
                "{tab}{ATTR}constraints:{RESET} {}",
                format_list(ctx, ", ", &self.cs)
            )?;
        }
        if !self.decls.is_empty() {
            writeln!(out, "{tab}{ATTR}decls:{RESET}")?;
            for (name, ty) in &self.decls {
                writeln!(
                    out,
                    "{ttab}{BLUE}{}{RESET} {COLON} {}",
                    name.as_ref(),
                    ty.pretty_string(ctx),
                )?;
            }
        }
        if !self.insts.is_empty() {
            writeln!(out, "{tab}{ATTR}insts:{RESET}")?;
            write_bulleted(out, ctx, &self.insts, level + 1)?;
        }
        writeln!(out, "{:-^1$}", "", 80)?;
        Ok(())
    }
}

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
            write_indented(out, ctx, &self.body, level + 1, true)?;
        }
        Ok(())
    }
}

impl PrettyPrint<Context> for Effect {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        info: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH;
        let ttab = format!("{tab}{TABWIDTH}");
        writeln!(out, "{:-^1$}", "", 80)?;
        writeln!(out, "{TITLE}Effect{RESET}")?;
        writeln!(out, "{tab}{ATTR}id:{RESET} {}", self.id.pretty_string(ctx))?;
        if !self.ops.is_empty() {
            writeln!(out, "{tab}{ATTR}ops:{RESET}")?;
            for (name, ty) in &self.ops {
                writeln!(
                    out,
                    "{ttab}{BLUE}{}{RESET} {COLON} {}",
                    name.as_ref(),
                    ty.pretty_string(ctx),
                )?;
            }
        }
        if let Some(default) = self.default {
            writeln!(
                out,
                "{tab}{ATTR}default:{RESET} {}",
                default.pretty_string(ctx)
            )?;
        }
        writeln!(out, "{:-^1$}", "", 80)?;
        Ok(())
    }
}

impl PrettyPrint<Context> for HashMap<Expr, TyE> {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        for (e, ty) in self {
            writeln!(
                out,
                "{tab}{} {ARROW} {}",
                e.pretty_string(ctx),
                ty.pretty_string(ctx)
            )?;
        }
        Ok(())
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
        if !self.defs.is_empty() {
            let defs = self.defs.values().collect::<Vec<_>>();
            writeln!(out, "{TITLE}Definitions{RESET}")?;
            write_bulleted(out, self, &defs, 0)?;
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
impl_pretty_print_for_id!(DeclId);
impl_pretty_print_for_id!(DataId);
impl_pretty_print_for_id!(DataConId);
impl_pretty_print_for_id!(EffectId);
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
        Id::Decl(id) => (id.raw, ""),
        Id::Data(id) => (id.raw, CYAN),
        Id::DataCon(id) => (id.raw, ""),
        Id::Effect(id) => (id.raw, CYAN),
        Id::EffectOp(id) => (id.raw, RED),
        Id::Handler(id) => (id.raw, MAGENTA),
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
