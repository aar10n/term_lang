use crate::type_env::TypeEnv;
use crate::{topo_sort::TopologicalSort, union_find::UnionFind, Context};
use term_core as core;
use term_print as print;

use print::{PrettyPrint, PrettyString, TABWIDTH};
use std::{fs::write, io};

impl PrettyPrint<core::Context> for TypeEnv {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &core::Context,
        level: usize,
    ) -> io::Result<()> {
        let tab = TABWIDTH.repeat(level);
        for map in self.stack.iter().rev() {
            for (var, ty) in map {
                writeln!(
                    out,
                    "{tab}{}: {}",
                    var.pretty_string(ctx),
                    ty.pretty_string(ctx),
                )?;
            }
        }
        Ok(())
    }
}

impl<'a, T, Info> PrettyPrint<Context<'a>, Info> for UnionFind<T>
where
    T: PrettyPrint<core::Context, Info> + Clone + Ord,
    Info: Default + Clone,
{
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Context<'a>,
        level: Info,
    ) -> io::Result<()> {
        for key in 0..self.parent.len() {
            let key = &self.key_ts[&key];
            let ty = self.find_no_mut(key.clone());
            if ty == *key {
                continue;
            }
            writeln!(
                out,
                "{}: {}",
                key.pretty_string(ctx.core),
                ty.pretty_string(ctx.core)
            )?;
        }

        Ok(())
    }
}

impl<'a, T, Info> PrettyPrint<core::Context, Info> for TopologicalSort<T>
where
    T: PrettyPrint<core::Context, Info> + Clone + Ord + Eq,
    Info: Default + Clone,
{
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &core::Context,
        level: Info,
    ) -> io::Result<()> {
        for (prec, dep) in &self.top {
            writeln!(
                out,
                "({}) {} | {}",
                dep.num_prec,
                prec.pretty_string(ctx),
                dep.succ
                    .iter()
                    .map(|id| id.pretty_string(ctx))
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
        }
        Ok(())
    }
}
