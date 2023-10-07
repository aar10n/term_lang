use const_format::concatcp;
use std::io::Write;

pub const RED: &str = "\x1B[31m";
pub const GREEN: &str = "\x1B[32m";
pub const YELLOW: &str = "\x1B[33m";
pub const BLUE: &str = "\x1B[34m";
pub const MAGENTA: &str = "\x1B[35m";
pub const CYAN: &str = "\x1B[36m";
pub const BOLD: &str = "\x1B[1m";
pub const ITALIC: &str = "\x1B[3m";
pub const UNDERLINE: &str = "\x1B[4m";
pub const RESET: &str = "\x1B[0m";

pub const TITLE: &str = concatcp!(MAGENTA, BOLD, UNDERLINE);
pub const TAG: &str = concatcp!(BLUE, BOLD);
pub const ATTR: &str = concatcp!(YELLOW);
pub const PUNCT: &str = concatcp!(CYAN);
pub const DELIM: &str = concatcp!(CYAN);
pub const KEYWORD: &str = concatcp!(GREEN);

pub mod chars {
    use super::*;
    pub const LBRAC: &str = concatcp!(DELIM, "[", RESET);
    pub const RBRAC: &str = concatcp!(DELIM, "]", RESET);
    pub const LBRACE: &str = concatcp!(DELIM, "{", RESET);
    pub const RBRACE: &str = concatcp!(DELIM, "}", RESET);
    pub const LPARN: &str = concatcp!(DELIM, "(", RESET);
    pub const RPARN: &str = concatcp!(DELIM, ")", RESET);
    pub const PIPE: &str = concatcp!(DELIM, "|", RESET);

    pub const EQUALS: &str = concatcp!(PUNCT, "+", RESET);
    pub const PLUS: &str = concatcp!(PUNCT, "+", RESET);
    pub const PERIOD: &str = concatcp!(PUNCT, ".", RESET);
    pub const COMMA: &str = concatcp!(PUNCT, ",", RESET);
    pub const COLON: &str = concatcp!(PUNCT, ":", RESET);
    pub const LAMBDA: &str = concatcp!(PUNCT, "λ", RESET);
    pub const QUOTE: &str = concatcp!(PUNCT, "'", RESET);
    pub const QMARK: &str = concatcp!(PUNCT, "?", RESET);
    pub const ARROW: &str = concatcp!(PUNCT, "->", RESET);
    pub const TILDE: &str = concatcp!(PUNCT, "~", RESET);

    pub const COMMA_SEP: &str = concatcp!(COMMA, " ");
    pub const PLUS_SEP: &str = concatcp!(" ", PLUS, " ");
    pub const PIPE_SEP: &str = concatcp!(" ", PIPE, " ");
}

/// A wrapper for writers that strips ANSI escape codes.
pub struct StripAnsi<'a, W: Write + ?Sized> {
    writer: &'a mut W,
}

impl<'a, W: Write + ?Sized> StripAnsi<'a, W> {
    pub fn new(writer: &'a mut W) -> Self {
        Self { writer }
    }
}

impl<'a, W: Write + ?Sized> Write for StripAnsi<'a, W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let mut i = 0;
        let mut j = 0;
        while i < buf.len() {
            if buf[i] == b'\x1B' {
                if i > j {
                    self.writer.write(&buf[j..i])?;
                }
                while i < buf.len() && buf[i] != b'm' {
                    i += 1;
                }
                j = i + 1;
            }
            i += 1;
        }
        if i > j {
            self.writer.write(&buf[j..i])?;
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.writer.flush()
    }
}
