#![allow(unused)]
#![feature(box_patterns)]
pub mod convert;

use term_core as core;
use term_diag as diag;
use term_print as print;

use core::{Context, Expr};

pub fn transform(ctx: &mut Context, expr: Expr) -> Expr {
    convert::convert(ctx, expr, &|ctx, expr| expr)
}
