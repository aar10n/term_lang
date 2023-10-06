use crate::{union_find::UnionFind, Context};
use term_core as core;
use term_print as print;

use print::{PrettyPrint, PrettyString, TABWIDTH};
use std::{fs::write, io};

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
            writeln!(out, "{}: {}", key.pretty_string(ctx), ty.pretty_string(ctx))?;
        }

        Ok(())
    }
}
