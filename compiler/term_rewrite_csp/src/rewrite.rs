use term_core as core;
use term_diag as diag;
use term_print as print;

use core::{Context, Expr};
use print::{PrettyPrint, PrettyString};

pub fn rewrite_csp(ctx: &mut Context, expr: Expr) -> Expr {
    use Expr::*;
    match expr {
        Type(_) => todo!(),

        Lit(_) => todo!(),
        Sym(_) => todo!(),
        Var(_) => todo!(),
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
