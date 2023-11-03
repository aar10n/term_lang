use crate::{ast::*, Context};
use term_core::Id;
use term_print::ansi::{
    chars::{
        COLON, COMMA_SEP, LBRAC, LBRACE, LPARN, PERIOD, PIPE, PIPE_SEP, PLUS_SEP, QUOTE, RBRAC,
        RBRACE, RPARN, TILDE,
    },
    ATTR, BLUE, BOLD, CYAN, DELIM, GREEN, MAGENTA, PUNCT, RED, RESET, TAG, UNDERLINE,
};
use term_print::{
    format_list, write_bulleted, write_list, PrettyPrint, PrettyString, DELIM_COMMA, TABWIDTH,
};

use std::io;

impl<T: PrettyPrint<Context>> PrettyPrint<Context> for Node<T> {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        self.kind.pretty_print(out, ctx, level)
    }
}

impl PrettyPrint<Context> for Module {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        writeln!(out, "{tab}{TAG}Module{RESET}")?;
        self.items.pretty_print(out, ctx, level + 1)
    }
}

impl PrettyPrint<Context> for ItemKind {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        match self {
            ItemKind::Command(..) => {
                writeln!(out, "{tab}{TAG}Command{RESET}")?;
            }
            ItemKind::DataDecl(data_decl) => {
                writeln!(out, "{tab}{TAG}DataDecl{RESET}")?;
                data_decl.pretty_print(out, ctx, level + 1)?;
            }
            ItemKind::EffectDecl(effect) => {
                writeln!(out, "{tab}{TAG}EffectDecl{RESET}")?;
                effect.pretty_print(out, ctx, level + 1)?
            }
            ItemKind::EffectHandler(effect_handler) => {
                writeln!(out, "{tab}{TAG}EffectHandler{RESET}")?;
                effect_handler.pretty_print(out, ctx, level + 1)?;
            }
            ItemKind::ClassDecl(class_decl) => {
                writeln!(out, "{tab}{TAG}ClassDecl{RESET}")?;
                class_decl.pretty_print(out, ctx, level + 1)?;
            }
            ItemKind::ClassInst(class_inst) => {
                writeln!(out, "{tab}{TAG}ClassInst{RESET}")?;
                class_inst.pretty_print(out, ctx, level + 1)?;
            }
            ItemKind::VarDecl(decl) => {
                writeln!(out, "{tab}{TAG}VarDecl{RESET}")?;
                match decl {
                    Left(decl) => decl.pretty_print(out, ctx, level + 1)?,
                    Right(id) => ctx.decls[id].borrow().pretty_print(out, ctx, level + 1)?,
                };
            }
            ItemKind::Expr(expr) => {
                writeln!(out, "{tab}{TAG}Expr{RESET}")?;
                expr.pretty_print(out, ctx, level + 1)?;
            }
        }
        Ok(())
    }
}

impl PrettyPrint<Context> for TyKind {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        write!(out, "{tab}")?;
        match self {
            TyKind::Infer => write!(out, "{BLUE}?{RESET}"),
            TyKind::Never => write!(out, "{BLUE}never{RESET}"),
            TyKind::Unit => write!(out, "{BLUE}(){RESET}"),
            TyKind::Int => write!(out, "{BOLD}{BLUE}Int{RESET}"),
            TyKind::Float => write!(out, "{BOLD}{BLUE}Float{RESET}"),
            TyKind::Bool => write!(out, "{BOLD}{BLUE}Bool{RESET}"),
            TyKind::Char => write!(out, "{BOLD}{BLUE}Char{RESET}"),
            TyKind::String => write!(out, "{BLUE}String{RESET}"),
            TyKind::Name(n) => write!(out, "{}", n.pretty_string(ctx)),
            TyKind::Inst(n, ts) => {
                write!(out, "{}", n.pretty_string(ctx))?;
                match &ts[..] {
                    [] => {}
                    [x] => {
                        write!(out, "{QUOTE}")?;
                        x.pretty_print(out, ctx, 0)?;
                    }
                    _ => {
                        write!(out, "{LPARN}")?;
                        ts.pretty_print(out, ctx, 0)?;
                        write!(out, "{RPARN}")?;
                    }
                }
                Ok(())
            }
            TyKind::Func(a, b) => {
                a.pretty_print(out, ctx, 0)?;
                write!(out, " {PUNCT}->{RESET} ")?;
                match b.kind {
                    TyKind::Effect(..) => {
                        write!(out, "{LPARN}")?;
                        b.pretty_print(out, ctx, 0)?;
                        write!(out, "{RPARN}")
                    }
                    _ => b.pretty_print(out, ctx, 0),
                }
            }
            TyKind::List(a) => {
                write!(out, "{LBRAC}")?;
                a.pretty_print(out, ctx, 0)?;
                write!(out, "{RBRAC}")
            }
            TyKind::Tuple(a) => {
                write!(out, "{LPARN}")?;
                a.pretty_print(out, ctx, 0)?;
                write!(out, "{RPARN}")
            }
            TyKind::Record(fs) => {
                write!(out, "{LBRACE}")?;
                for (i, (n, ty)) in fs.iter().enumerate() {
                    if i > 0 {
                        write!(out, " ")?;
                    }
                    write!(out, "{} {COLON} ", n.pretty_string(ctx))?;
                    ty.pretty_print(out, ctx, 0)?;
                }
                write!(out, "{RBRACE}")
            }
            TyKind::Effect(ty, ef) => {
                if ef.is_pure() {
                    ty.pretty_print(out, ctx, 0)
                } else {
                    write!(
                        out,
                        "{} {TILDE} {}",
                        ty.pretty_string(ctx),
                        ef.pretty_string(ctx)
                    )
                }
            }
            TyKind::Forall(ps, ty) => {
                let b = ps.params.len() > 1;
                write!(out, "{PUNCT}∀{RESET}{}", if b { LBRAC } else { "" })?;
                write_list(out, ctx, DELIM_COMMA, &ps.params)?;
                write!(out, "{}", if b { RBRAC } else { "" });

                if !ps.constraints.is_empty() {
                    write!(
                        out,
                        " {PIPE}{}{PIPE}{PERIOD}",
                        format_list(ctx, DELIM_COMMA, &ps.constraints)
                    )?;
                } else {
                    write!(out, "{PERIOD}");
                }

                match ty.kind {
                    TyKind::Forall(..) => {
                        write!(out, "{LPARN}")?;
                        ty.pretty_print(out, ctx, 0)?;
                        write!(out, "{RPARN}")
                    }
                    _ => ty.pretty_print(out, ctx, 0),
                }
            }
        }
    }
}

impl PrettyPrint<Context> for EfKind {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        _: usize,
    ) -> io::Result<()> {
        match self {
            EfKind::Infer => write!(out, "{BLUE}_{RESET}"),
            EfKind::Pure => write!(out, "{BLUE}(){RESET}"),
            EfKind::Name(n, ps) => {
                write!(out, "{}", n.pretty_string(ctx))?;
                if ps.len() > 1 {
                    write!(out, "{QUOTE}{LPARN}")?;
                    ps.pretty_print(out, ctx, 0)?;
                    write!(out, "{RPARN}")?;
                } else if !ps.is_empty() {
                    write!(out, "{QUOTE}")?;
                    ps.pretty_print(out, ctx, 0)?;
                }
                Ok(())
            }
            EfKind::Union(fs) => write_list(out, ctx, PIPE_SEP, fs),
        }
    }
}

impl PrettyPrint<Context> for ClassDecl {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        writeln!(
            out,
            "{tab}{ATTR}name:{RESET} {}",
            self.name.pretty_string(ctx)
        )?;
        self.ty_params.pretty_print(out, ctx, level)?;
        writeln!(out, "{tab}{ATTR}methods:{RESET}")?;
        write_bulleted(out, ctx, &self.members, level + 1)?;
        Ok(())
    }
}

impl PrettyPrint<Context> for MemberDeclKind {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        match self {
            MemberDeclKind::AssocTy(name) => {
                writeln!(out, "{tab}{TAG}Type{RESET} {}", name.pretty_string(ctx))
            }
            MemberDeclKind::Method(method) => {
                writeln!(out, "{tab}{TAG}Method{RESET}")?;
                method.pretty_print(out, ctx, level + 1)
            }
        }
    }
}

impl PrettyPrint<Context> for MethodDecl {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        writeln!(out, "{tab}{}", self.name.pretty_string(ctx))?;
        self.ty_params.pretty_print(out, ctx, level)?;
        writeln!(
            out,
            "{tab}{ATTR}type:{RESET} {}",
            self.ty.pretty_string(ctx)
        )
    }
}

impl PrettyPrint<Context> for ClassInst {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        writeln!(
            out,
            "{tab}{ATTR}class:{RESET} {}",
            self.class.pretty_string(ctx)
        )?;
        self.ty_args.pretty_print(out, ctx, level)?;
        writeln!(out, "{tab}{ATTR}methods:{RESET}")?;
        write_bulleted(out, ctx, &self.members, level + 1)?;
        Ok(())
    }
}

impl PrettyPrint<Context> for MemberImplKind {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        let ttab = format!("{}{}", tab, TABWIDTH);
        match self {
            MemberImplKind::AssocTy(name, ty) => {
                writeln!(out, "{tab}{TAG}Type{RESET}")?;
                writeln!(out, "{ttab}{ATTR}name:{RESET} {}", name.pretty_string(ctx))?;
                writeln!(out, "{ttab}{ATTR}type:{RESET} {}", ty.pretty_string(ctx))
            }
            MemberImplKind::Method(method) => {
                writeln!(out, "{tab}{TAG}Method{RESET}")?;
                method.pretty_print(out, ctx, level + 1)
            }
        }
    }
}

impl PrettyPrint<Context> for MethodImpl {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        writeln!(out, "{} ", self.name.pretty_string(ctx))?;
        if !self.params.is_empty() {
            writeln!(
                out,
                "{tab}{TABWIDTH}{ATTR}params:{RESET} {}",
                format_list(ctx, DELIM_COMMA, &self.params)
            )?;
            writeln!(out, "{tab}{TABWIDTH}{ATTR}body:{RESET}",)?;
        } else {
            writeln!(out, "{tab}{TABWIDTH}{ATTR}expr:{RESET}",)?;
        }
        self.expr.pretty_print(out, ctx, level + 2)?;
        Ok(())
    }
}

impl PrettyPrint<Context> for DataDecl {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        writeln!(
            out,
            "{tab}{ATTR}name:{RESET} {}",
            self.name.pretty_string(ctx)
        )?;
        self.ty_params.pretty_print(out, ctx, level)?;
        writeln!(out, "{tab}{ATTR}constructors:{RESET}")?;
        write_bulleted(out, ctx, &self.cons, level + 1)?;
        Ok(())
    }
}

impl PrettyPrint<Context> for DataConDecl {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        _: usize,
    ) -> io::Result<()> {
        let var_id = ctx.id_var_ids[&self.name.id.unwrap()];
        write!(out, "{}<{}>", self.name.pretty_string(ctx), var_id)?;

        if !self.fields.is_empty() {
            write!(out, " {LPARN}")?;
            for (i, ty) in self.fields.iter().enumerate() {
                if i > 0 {
                    write!(out, " ")?;
                }
                ty.pretty_print(out, ctx, 0)?;
            }
            write!(out, "{RPARN}")?;
        }
        Ok(())
    }
}

impl PrettyPrint<Context> for EffectDecl {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        writeln!(
            out,
            "{tab}{ATTR}name:{RESET} {}",
            self.name.pretty_string(ctx)
        )?;
        self.ty_params.pretty_print(out, ctx, level)?;
        if !self.side_efs.is_empty() {
            writeln!(
                out,
                "{tab}{ATTR}effects:{RESET} {}",
                format_list(ctx, DELIM_COMMA, &self.side_efs)
            )?;
        }
        writeln!(out, "{tab}{ATTR}ops:{RESET}")?;
        write_bulleted(out, ctx, &self.ops, level + 1)?;
        Ok(())
    }
}

impl PrettyPrint<Context> for EffectOpDecl {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        _: usize,
    ) -> io::Result<()> {
        write!(out, "{} {COLON} ", self.name.pretty_string(ctx))?;
        self.ty.pretty_print(out, ctx, 0)?;
        Ok(())
    }
}

impl PrettyPrint<Context> for EffectHandler {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        write!(
            out,
            "{tab}{ATTR}name:{RESET} {}",
            self.name.pretty_string(ctx)
        )?;
        if self.default {
            writeln!(out, " {BOLD}{MAGENTA}[default]{RESET}");
        } else {
            writeln!(out, "");
        }
        self.ty_args.pretty_print(out, ctx, level)?;
        writeln!(out, "{tab}{ATTR}ops:{RESET}")?;
        write_bulleted(out, ctx, &self.ops, level + 1)?;
        Ok(())
    }
}

impl PrettyPrint<Context> for EffectOpImpl {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        writeln!(out, "{}", self.name.pretty_string(ctx))?;
        if !self.params.is_empty() {
            writeln!(
                out,
                "{tab}{TABWIDTH}{ATTR}params:{RESET} {}",
                format_list(ctx, DELIM_COMMA, &self.params)
            )?;
            writeln!(out, "{tab}{TABWIDTH}{ATTR}body:{RESET}",)?;
        } else {
            writeln!(out, "{tab}{TABWIDTH}{ATTR}expr:{RESET}",)?;
        }
        self.expr.pretty_print(out, ctx, level + 1)?;
        Ok(())
    }
}

impl PrettyPrint<Context> for VarDecl {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        writeln!(
            out,
            "{tab}{} {COLON} {}",
            self.name.pretty_string(ctx),
            self.ty.pretty_string(ctx)
        );
        // if let Some(decl_id) = self.name.id.and_then(|id| id.as_decl()) {
        //     if let Some(var_id) = ctx.decl_var_ids.get(&decl_id) {
        //         writeln!(
        //             out,
        //             "{tab}{TABWIDTH}{ATTR}var_id:{RESET} {}<{}>",
        //             var_id.pretty_string(&mut term_core::Context::new()),
        //             var_id.raw
        //         )?;
        //     }
        // }

        Ok(())
    }
}

impl PrettyPrint<Context> for TyParams {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        if !self.params.is_empty() {
            writeln!(
                out,
                "{tab}{ATTR}params:{RESET} {}",
                format_list(ctx, DELIM_COMMA, &self.params)
            )?;
        }
        if !self.constraints.is_empty() {
            writeln!(
                out,
                "{tab}{ATTR}constraints:{RESET} {}",
                format_list(ctx, DELIM_COMMA, &self.constraints)
            )?;
        }
        Ok(())
    }
}

impl PrettyPrint<Context> for TyArgs {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        if !self.args.is_empty() {
            writeln!(
                out,
                "{tab}{ATTR}args:{RESET} {}",
                format_list(ctx, DELIM_COMMA, &self.args)
            )?;
        }
        if !self.params.is_empty() {
            writeln!(
                out,
                "{tab}{ATTR}params:{RESET} {}",
                format_list(ctx, DELIM_COMMA, &self.params)
            )?;
        }
        if !self.constraints.is_empty() {
            writeln!(
                out,
                "{tab}{ATTR}constraints:{RESET} {}",
                format_list(ctx, DELIM_COMMA, &self.constraints)
            )?;
        }
        Ok(())
    }
}

impl PrettyPrint<Context> for ConstraintKind {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        _: usize,
    ) -> io::Result<()> {
        match self {
            ConstraintKind::Class(name, args) => {
                write!(out, "{}{QUOTE}", name.pretty_string(ctx))?;
                if let [x] = &args[..] {
                    x.pretty_print(out, ctx, 0)?;
                } else {
                    write!(out, "{LPARN}")?;
                    args.pretty_print(out, ctx, 0)?;
                    write!(out, "{RPARN}")?;
                }
                Ok(())
            }
            _ => todo!(),
        }
    }
}

// impl<> PrettyPrint<Context> for Expr {
//     fn pretty_print<Output: io::Write>(
//         &self,
//         out: &mut Output,
//         ctx: &Context,
//         level: usize,
//     ) -> io::Result<()> {
//         self.kind.pretty_print(out, ctx, level)
//     }
// }

impl PrettyPrint<Context> for ExprKind {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        let ttab = format!("{}{}", tab, TABWIDTH);
        match self {
            ExprKind::Apply(x, y) => {
                writeln!(out, "{tab}{TAG}Apply{RESET}")?;
                x.pretty_print(out, ctx, level + 1);
                y.pretty_print(out, ctx, level + 1);
            }
            ExprKind::Binary(op, x, y) => {
                writeln!(out, "{tab}{TAG}Binary{RESET} {:?}", op)?;
                x.pretty_print(out, ctx, level + 1);
                y.pretty_print(out, ctx, level + 1);
            }
            ExprKind::Unary(op, x) => {
                writeln!(out, "{tab}{TAG}Unary{RESET} {:?}", op)?;
                x.pretty_print(out, ctx, level + 1);
            }
            ExprKind::Case(case) => {
                writeln!(out, "{tab}{TAG}Case{RESET}")?;
                case.pretty_print(out, ctx, level + 1);
            }
            ExprKind::Handle(handle) => {
                writeln!(out, "{tab}{TAG}Handle{RESET}")?;
                handle.pretty_print(out, ctx, level + 1);
            }
            ExprKind::Do(block) => {
                writeln!(out, "{tab}{TAG}Block{RESET}")?;
                block.pretty_print(out, ctx, level + 1);
            }
            ExprKind::If(if_) => {
                writeln!(out, "{tab}{TAG}If{RESET}")?;
                if_.pretty_print(out, ctx, level + 1);
            }
            ExprKind::Func(func) => {
                writeln!(out, "{tab}{TAG}Func{RESET}")?;
                func.pretty_print(out, ctx, level + 1);
            }
            ExprKind::Lambda(lambda) => {
                writeln!(out, "{tab}{TAG}Lambda{RESET}")?;
                lambda.pretty_print(out, ctx, level + 1);
            }
            ExprKind::Var(var) => {
                writeln!(out, "{tab}{TAG}Var{RESET}")?;
                var.pretty_print(out, ctx, level + 1);
            }
            ExprKind::List(exprs) => {
                writeln!(out, "{tab}{TAG}List{RESET}")?;
                write_bulleted(out, ctx, exprs, level + 1)?;
            }
            ExprKind::Tuple(exprs) => {
                writeln!(out, "{tab}{TAG}Tuple{RESET}")?;
                write_bulleted(out, ctx, exprs, level + 1)?;
            }
            ExprKind::Record(fields) => {
                writeln!(out, "{tab}{TAG}Record{RESET}")?;
                for (name, expr) in fields {
                    writeln!(out, "{ttab}{}:", name.pretty_string(ctx))?;
                    expr.pretty_print(out, ctx, level + 2)?;
                }
            }
            ExprKind::Lit(lit) => {
                writeln!(out, "{tab}{TAG}Lit{RESET} {}", lit.pretty_string(ctx))?;
            }
            ExprKind::Ident(name) => {
                writeln!(out, "{tab}{TAG}Ident{RESET} {}", name.pretty_string(ctx))?;
            }
        }
        Ok(())
    }
}

impl PrettyPrint<Context> for PatKind {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        _: usize,
    ) -> io::Result<()> {
        match self {
            PatKind::Wildcard => write!(out, "{PUNCT}_{RESET}"),
            PatKind::Unit => write!(out, "{LPARN}{RPARN}"),
            PatKind::DataCon(name, pats) => {
                if !pats.is_empty() {
                    write!(out, "{}", name.pretty_string(ctx))?;
                    write!(out, " {LPARN}")?;
                    write_list(out, ctx, " ", pats)?;
                    write!(out, "{RPARN}")?;
                }
                Ok(())
            }
            PatKind::Tuple(pats) => {
                write!(out, "{LPARN}")?;
                pats.pretty_print(out, ctx, 0)?;
                write!(out, "{RPARN}")
            }
            PatKind::List(pats) => {
                write!(out, "{LBRAC}")?;
                pats.pretty_print(out, ctx, 0)?;
                write!(out, "{RBRAC}")
            }
            PatKind::Record(fields) => {
                write!(out, "{LBRACE}")?;
                for (i, (n, pat)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(out, " ")?;
                    }
                    write!(out, "{} {COLON} ", n.pretty_string(ctx))?;
                    pat.pretty_print(out, ctx, 0)?;
                }
                write!(out, "{RBRACE}")
            }
            PatKind::Cons(head, tail) => {
                head.pretty_print(out, ctx, 0)?;
                write!(out, " {PUNCT}:{RESET} ")?;
                tail.pretty_print(out, ctx, 0)
            }
            PatKind::Ident(name) => write!(out, "{}", name.pretty_string(ctx)),
            PatKind::Lit(lit) => lit.pretty_print(out, ctx, 0),
        }
    }
}

impl PrettyPrint<Context> for Case {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        writeln!(out, "{tab}{ATTR}expr:{RESET}")?;
        self.expr.pretty_print(out, ctx, level + 1)?;
        writeln!(out, "{tab}{ATTR}alts:{RESET}")?;
        write_bulleted(out, ctx, &self.alts, level + 1)?;
        Ok(())
    }
}

impl PrettyPrint<Context> for CaseAlt {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        writeln!(
            out,
            "{tab}{ATTR}pat:{RESET} {}",
            self.pat.pretty_string(ctx)
        )?;
        writeln!(out, "{tab}{ATTR}expr:{RESET}")?;
        self.expr.pretty_print(out, ctx, level + 1)
    }
}

impl PrettyPrint<Context> for Handle {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        writeln!(out, "{tab}{ATTR}expr:{RESET}")?;
        self.expr.pretty_print(out, ctx, level + 1)?;
        writeln!(out, "{tab}{ATTR}alts:{RESET}")?;
        write_bulleted(out, ctx, &self.alts, level + 1)?;
        Ok(())
    }
}

impl PrettyPrint<Context> for HandleAlt {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        writeln!(out, "{tab}{ATTR}ef:{RESET} {}", self.ef.pretty_string(ctx))?;
        writeln!(out, "{tab}{ATTR}expr:{RESET}")?;
        self.expr.pretty_print(out, ctx, level + 1)
    }
}

impl PrettyPrint<Context> for Do {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        level: usize,
    ) -> io::Result<()> {
        write_bulleted(out, ctx, &self.exprs, level)
    }
}

impl PrettyPrint<Context> for If {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        _: usize,
    ) -> io::Result<()> {
        writeln!(out, "{ATTR}cond:{RESET}")?;
        self.cond.pretty_print(out, ctx, 0)?;
        writeln!(out, "{ATTR}then:{RESET}")?;
        self.then.pretty_print(out, ctx, 0)?;
        writeln!(out, "{ATTR}else:{RESET}")?;
        self.else_.pretty_print(out, ctx, 0)
    }
}

impl PrettyPrint<Context> for Func {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        info: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(info);
        writeln!(
            out,
            "{tab}{ATTR}params:{RESET} {}",
            format_list(ctx, DELIM_COMMA, &self.params)
        )?;
        if let Some(ty) = &self.ty {
            writeln!(out, "{tab}{ATTR}type:{RESET} {}", ty.pretty_string(ctx))?;
        }
        writeln!(out, "{tab}{ATTR}body:{RESET}")?;
        self.body.pretty_print(out, ctx, info + 1)
    }
}

impl PrettyPrint<Context> for Lambda {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        info: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(info);
        writeln!(
            out,
            "{tab}{ATTR}params:{RESET} {}",
            format_list(ctx, DELIM_COMMA, &self.params)
        )?;
        if let Some(ty) = &self.ty {
            writeln!(out, "{tab}{ATTR}type:{RESET} {}", ty.pretty_string(ctx))?;
        }
        writeln!(out, "{tab}{ATTR}body:{RESET}")?;
        self.body.pretty_print(out, ctx, info + 1)
    }
}

impl PrettyPrint<Context> for Var {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        info: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(info);
        if let Some(ty) = &self.ty {
            writeln!(out, "{tab}{ATTR}type:{RESET} {}", ty.pretty_string(ctx))?;
        }
        writeln!(out, "{tab}{ATTR}expr:{RESET}")?;
        self.expr.pretty_print(out, ctx, info + 1)
    }
}

impl PrettyPrint<Context> for LitKind {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        _: usize,
    ) -> io::Result<()> {
        match self {
            LitKind::Unit => writeln!(out, "{CYAN}(){RESET}"),
            LitKind::Int(n) => write!(out, "{CYAN}{}{RESET}", n),
            LitKind::Float(n) => write!(out, "{CYAN}{}{RESET}", n),
            LitKind::Bool(b) => write!(out, "{MAGENTA}{}{RESET}", b),
            LitKind::Char(c) => write!(out, "{GREEN}'{}'{RESET}", c.escape_debug()),
            LitKind::String(s) => write!(out, "{GREEN}\"{}\"{RESET}", s.escape_debug()),
        }
    }
}

impl PrettyPrint<Context> for Ident {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context,
        _: usize,
    ) -> io::Result<()> {
        if let Some(id) = self.id {
            let (raw, color) = id_to_color(id);
            write!(out, "{color}{}<{}>{RESET}", self.as_ref(), raw)
        } else {
            write!(out, "{}", self.as_ref())
        }
    }
}

fn id_to_color(id: Id) -> (usize, &'static str) {
    match id {
        Id::Class(id) => (id.raw, CYAN),
        Id::Data(id) => (id.raw, CYAN),
        Id::DataCon(id) => (id.raw, RED),
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
