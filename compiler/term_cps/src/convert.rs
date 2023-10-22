use term_core as core;
use term_diag as diag;
use term_print as print;

use core::{Bind, Context, Expr};
use print::{PrettyPrint, PrettyString};
use ustr::ustr;

pub fn convert(ctx: &mut Context, expr: Expr, c: &dyn Fn(&mut Context, Expr) -> Expr) -> Expr {
    use Bind::*;
    use Expr::*;
    match expr {
        Type(_) | Lit(_) | Sym(_) | Var(_) => c(ctx, expr),

        Record(_) => todo!(),
        Apply(_, _) => todo!(),
        Lambda(_, _) => todo!(),
        Let(_, _) => todo!(),
        Case(_, _) => todo!(),
        Handle(_, _) => todo!(),
        Do(_) => todo!(),
        Span(_, _) => todo!(),
    }
}
