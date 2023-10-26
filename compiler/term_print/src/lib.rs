pub mod ansi;

use ansi::{StripAnsi, DELIM, RESET};
use const_format::concatcp;

use std::cell::RefCell;
use std::io;
use std::rc::Rc;

pub const TABWIDTH: &str = "  ";
pub const DELIM_COMMA: &str = concatcp!(DELIM, ", ", RESET);

pub trait PrettyPrint<Ctx, Info: Clone = usize> {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        info: Info,
    ) -> io::Result<()>;
}

impl<T: PrettyPrint<Ctx, Info>, Ctx, Info: Clone> PrettyPrint<Ctx, Info> for Vec<T> {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        info: Info,
    ) -> io::Result<()> {
        for item in self {
            item.pretty_print(out, ctx, info.clone())?;
        }
        Ok(())
    }
}

impl<T: PrettyPrint<Ctx, Info>, Ctx, Info: Clone> PrettyPrint<Ctx, Info> for Option<T> {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        info: Info,
    ) -> io::Result<()> {
        if let Some(item) = self {
            item.pretty_print(out, ctx, info)?;
        }
        Ok(())
    }
}

impl<T: PrettyPrint<Ctx, Info>, Ctx, Info: Clone> PrettyPrint<Ctx, Info> for Box<T> {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        info: Info,
    ) -> io::Result<()> {
        (**self).pretty_print(out, ctx, info)
    }
}

impl<T: PrettyPrint<Ctx, Info>, Ctx, Info: Clone> PrettyPrint<Ctx, Info> for &T {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        info: Info,
    ) -> io::Result<()> {
        (**self).pretty_print(out, ctx, info)
    }
}

impl<T: PrettyPrint<Ctx, Info>, Ctx, Info: Clone> PrettyPrint<Ctx, Info> for Rc<T> {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        info: Info,
    ) -> io::Result<()> {
        (**self).pretty_print(out, ctx, info)
    }
}

impl<T: PrettyPrint<Ctx, Info>, Ctx, Info: Clone> PrettyPrint<Ctx, Info> for RefCell<T> {
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        info: Info,
    ) -> io::Result<()> {
        self.borrow().pretty_print(out, ctx, info)
    }
}

//
//

pub trait PrettyString<Ctx, Info: Clone>: PrettyPrint<Ctx, Info> {
    fn pretty_string(&self, ctx: &Ctx) -> String;
    fn plain_string(&self, ctx: &Ctx) -> String;
    fn print_stdout(&self, ctx: &Ctx) -> io::Result<()>;
}

impl<T: PrettyPrint<Ctx, Info>, Ctx, Info: Default + Clone> PrettyString<Ctx, Info> for T {
    fn pretty_string(&self, ctx: &Ctx) -> String {
        let mut buf = Vec::new();
        self.pretty_print(&mut buf, ctx, Info::default()).unwrap();
        String::from_utf8(buf).unwrap()
    }

    fn plain_string(&self, ctx: &Ctx) -> String {
        let mut buf = Vec::new();
        self.pretty_print(&mut StripAnsi::new(&mut buf), ctx, Info::default())
            .unwrap();
        String::from_utf8(buf).unwrap()
    }

    fn print_stdout(&self, ctx: &Ctx) -> io::Result<()> {
        self.pretty_print(&mut io::stdout(), ctx, Info::default())
    }
}

/// A wrapper for writers that indents the start of newlines with the given indent level.
pub struct IndentWriter<'a, Output: io::Write> {
    out: &'a mut Output,
    level: usize,
    newline: bool,
}

impl<'a, Output: io::Write> IndentWriter<'a, Output> {
    pub fn new(out: &'a mut Output, level: usize) -> Self {
        Self {
            out,
            level,
            newline: true,
        }
    }
}

impl<'a, Output: io::Write> io::Write for IndentWriter<'a, Output> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let tab = TABWIDTH.repeat(self.level);
        let mut n = 0;
        for &b in buf {
            if self.newline {
                self.out.write_all(tab.as_bytes())?;
                self.newline = false;
            }
            if b == b'\n' {
                self.newline = true;
            }
            self.out.write_all(&[b])?;
            n += 1;
        }
        Ok(n)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.out.flush()
    }
}

//
// Formatting helpers

pub fn pretty_format<T: PrettyString<Ctx, Info>, Ctx, Info: Clone + Default>(
    ctx: &Ctx,
    item: &T,
) -> io::Result<String> {
    Ok(item.pretty_string(ctx))
}

pub fn pretty_print<T: PrettyString<Ctx, Info>, Ctx, Info: Clone + Default>(
    ctx: &Ctx,
    item: &T,
) -> io::Result<()> {
    item.print_stdout(ctx)
}

pub fn write_list<C, I, O, S, T, Info>(out: &mut O, ctx: &C, sep: S, items: &I) -> io::Result<()>
where
    O: io::Write,
    S: AsRef<str>,
    T: PrettyPrint<C, Info>,
    Info: Clone + Default,
    for<'a> &'a I: IntoIterator<Item = &'a T>,
{
    let mut items = items.into_iter().peekable();
    while let Some(arg) = items.next() {
        arg.pretty_print(out, ctx, Info::default())?;
        if items.peek().is_some() {
            write!(out, "{}", sep.as_ref())?;
        }
    }
    Ok(())
}

pub fn format_list<C, I, S, T, Info>(ctx: &C, sep: S, items: &I) -> String
where
    S: AsRef<str>,
    T: PrettyPrint<C, Info>,
    Info: Clone + Default,
    for<'a> &'a I: IntoIterator<Item = &'a T>,
{
    let mut buf = Vec::new();
    write_list(&mut buf, ctx, sep, items).unwrap();
    String::from_utf8(buf).unwrap()
}

pub fn write_delimited_list<C, I, O, S, T, Info>(
    out: &mut O,
    ctx: &C,
    items: &I,
    sep: S,
    l_delim: S,
    r_delim: S,
) -> io::Result<()>
where
    O: io::Write,
    T: PrettyPrint<C, Info>,
    S: AsRef<str>,
    Info: Clone + Default,
    for<'a> &'a I: IntoIterator<Item = &'a T>,
{
    write!(out, "{}", l_delim.as_ref())?;
    write_list(out, ctx, sep, items)?;
    write!(out, "{}", r_delim.as_ref())?;
    Ok(())
}

pub fn write_delimited_list_if_many<C, I, O, S, T, Info>(
    out: &mut O,
    ctx: &C,
    items: &I,
    sep: S,
    l_delim: S,
    r_delim: S,
) -> io::Result<()>
where
    O: io::Write,
    T: PrettyString<C, Info>,
    S: AsRef<str>,
    Info: Clone + Default,
    for<'a> &'a I: IntoIterator<Item = &'a T>,
{
    let n = items.into_iter().count();
    if n == 0 {
        return Ok(());
    } else if n == 1 {
        return write_list(out, ctx, sep, items);
    }

    write!(out, "{}", l_delim.as_ref())?;
    let mut items = items.into_iter().peekable();
    while let Some(item) = items.next() {
        write!(out, "{}", item.pretty_string(ctx).trim())?;
        if items.peek().is_some() {
            write!(out, "{}", sep.as_ref())?;
        }
    }
    write!(out, "{}", r_delim.as_ref())?;
    Ok(())
}

pub fn write_bulleted_sep<T: PrettyPrint<Ctx, usize>, Output: io::Write, Ctx, I>(
    out: &mut Output,
    ctx: &Ctx,
    items: &I,
    level: usize,
    sep: impl AsRef<str>,
) -> io::Result<()>
where
    I: IntoIterator<Item = T>,
    for<'a> &'a I: IntoIterator<Item = &'a T>,
{
    let tab = TABWIDTH.repeat(level);
    for item in items {
        let mut buf = Vec::new();
        item.pretty_print(&mut buf, ctx, level + 1)?;
        let item = String::from_utf8_lossy(&buf).trim().to_owned();
        writeln!(out, "{tab}{DELIM}{}{RESET} {}", sep.as_ref(), item)?;
    }
    Ok(())
}

pub fn write_bulleted<T: PrettyPrint<Ctx, usize>, Output: io::Write, Ctx, I>(
    out: &mut Output,
    ctx: &Ctx,
    items: &I,
    level: usize,
) -> io::Result<()>
where
    I: IntoIterator<Item = T>,
    for<'a> &'a I: IntoIterator<Item = &'a T>,
{
    write_bulleted_sep(out, ctx, items, level, "-")
}

pub fn format_bulleted<T: PrettyPrint<Ctx, usize>, Ctx, I>(
    ctx: &Ctx,
    items: &I,
    level: usize,
) -> String
where
    I: IntoIterator<Item = T>,
    for<'a> &'a I: IntoIterator<Item = &'a T>,
{
    let mut buf = Vec::new();
    write_bulleted(&mut buf, ctx, items, level).unwrap();
    String::from_utf8(buf).unwrap()
}

/// Writes the given item to the given output, prefixing each line of output
/// with the given indentation level. The fifth argument specifies whether
/// the first line should be indented.
pub fn write_indented<T: PrettyPrint<Ctx, usize>, Output: io::Write, Ctx>(
    out: &mut Output,
    ctx: &Ctx,
    t: &T,
    level: usize,
    first: bool,
) -> io::Result<()> {
    let tab = TABWIDTH.repeat(level);

    let s = t.pretty_string(ctx);
    let ls = s.split("\n").into_iter().collect::<Vec<_>>();
    let (x, xs) = ls.split_first().unwrap();
    if first {
        writeln!(out, "{tab}{}", x.trim_matches('\n'))?;
    } else {
        writeln!(out, "{}", x.trim_matches('\n'))?;
    }
    for x in xs {
        let x = x.trim_matches('\n');
        if !x.is_empty() {
            writeln!(out, "{tab}{}", x)?;
        }
    }
    Ok(())
}

pub struct Joined<'a, I> {
    pub sep: String,
    pub items: &'a I,
}

impl<'a, I> Joined<'a, I> {
    pub fn new<S: AsRef<str>>(sep: S, items: &'a I) -> Self {
        Self {
            sep: sep.as_ref().to_owned(),
            items,
        }
    }
}

impl<'a, I, T: PrettyPrint<Ctx, Info>, Ctx, Info: Clone + Default> PrettyPrint<Ctx, Info>
    for Joined<'a, I>
where
    for<'b> &'b I: IntoIterator<Item = &'b T>,
{
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        _: Info,
    ) -> io::Result<()> {
        for (i, item) in self.items.into_iter().enumerate() {
            if i > 0 {
                write!(out, "{}", self.sep)?;
            }
            item.pretty_print(out, ctx, Info::default())?;
        }
        Ok(())
    }
}

pub struct Mapped<'a, I, F> {
    pub items: &'a I,
    pub f: F,
}

impl<'a, I, F> Mapped<'a, I, F> {
    pub fn new(items: &'a I, f: F) -> Self {
        Self { items, f }
    }
}

impl<'a, I, F, T: PrettyPrint<Ctx, Info>, Ctx, Info: Clone + Default> PrettyPrint<Ctx, Info>
    for Mapped<'a, I, F>
where
    for<'b> &'b I: IntoIterator<Item = &'b T>,
    F: Fn(&Ctx, &T) -> String,
{
    fn pretty_print<Output: io::Write>(
        &self,
        out: &mut Output,
        ctx: &Ctx,
        _: Info,
    ) -> io::Result<()> {
        for item in self.items.into_iter() {
            write!(out, "{}", (self.f)(ctx, item))?;
        }
        Ok(())
    }
}
