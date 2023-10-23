use term_core as core;
use term_diag as diag;
use term_print as print;

use core::{Bind, Context, Expr};
use print::{PrettyPrint, PrettyString};
use ustr::ustr;

pub fn convert(ctx: &mut Context, expr: Expr, c: &dyn Fn(&mut Context, Expr) -> Expr) -> Expr {
    use Bind::*;
    use Expr::*;
    todo!()
}
