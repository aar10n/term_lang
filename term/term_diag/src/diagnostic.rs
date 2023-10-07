use term_common::ansi::{BLUE, MAGENTA, RED, RESET, YELLOW};
use term_common::source::{SourceFile, SourceMap, Span};

use std::io;

pub trait FormatFn<Output: io::Write> = FnOnce(&mut Output, &SourceMap) -> io::Result<()>;

// /// A diagnostic.
// #[derive(Debug, Clone)]
// pub struct DiagnosticBuilder {
//     pub level: Level,
//     pub message: String,
//     pub span: Span,
//     pub span_note: Option<String>,
//     pub notes: Vec<(String, Span)>,
// }

/// A diagnostic.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub level: Level,
    pub message: String,
    pub span: Span,
    pub span_note: Option<String>,
    pub notes: Vec<(String, Span)>,
}

impl Diagnostic {
    pub fn new(level: Level, message: String, span: Span) -> Self {
        Self {
            level,
            message,
            span,
            span_note: None,
            notes: Vec::new(),
        }
    }

    pub fn error<S: ToString>(message: S, span: Span) -> Self {
        Self::new(Level::Error, message.to_string(), span)
    }

    pub fn is_error(&self) -> bool {
        self.level == Level::Error
    }

    pub fn is_warning(&self) -> bool {
        self.level == Level::Warning
    }

    pub fn with_inline_note<S: ToString>(mut self, message: S) -> Self {
        self.span_note = Some(message.to_string());
        self
    }

    pub fn with_note<S: ToString>(mut self, message: S, span: Span) -> Self {
        self.notes.push((message.to_string(), span));
        self
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    /// Renders the diagnostic to the output stream.
    pub fn render<Output: io::Write>(
        &self,
        out: &mut Output,
        source_map: &SourceMap,
    ) -> io::Result<()> {
        if self.span == Span::default() {
            match self.level {
                Level::Error => writeln!(out, "{RED}error: {}{RESET}", self.message)?,
                Level::Warning => writeln!(out, "{YELLOW}warning: {}{RESET}", self.message)?,
            }
            return Ok(());
        }

        let source = source_map.get(self.span.source_id).unwrap();
        let (line, col) = source.line_and_column(self.span.start);

        match self.level {
            Level::Error => {
                writeln!(out, "{RED}error: {}{RESET}", self.message)?;
                writeln!(out, "  {}:{}:{}", source.name(), line, col)?;

                let marker = Some((self.marker_style(), col, RED));
                format_code_snippet(out, &self.span, source, marker)
            }
            Level::Warning => {
                writeln!(out, "{YELLOW}warning: {}{RESET}", self.message)?;
                writeln!(out, "  {}:{}:{}", source.name(), line, col)?;

                let marker = Some((self.marker_style(), col, YELLOW));
                format_code_snippet(out, &self.span, source, marker)
            }
        }?;

        for (message, span) in &self.notes {
            let marker = Some((MarkerStyle::SpanWithNote('^', message), span.start, MAGENTA));
            format_code_snippet(out, span, source_map.get(span.source_id).unwrap(), marker)?;
        }
        // writeln!(out)
        Ok(())
    }

    fn marker_style(&self) -> MarkerStyle<'_> {
        if let Some(ref inline_note) = self.span_note {
            if self.span.len() <= 1 {
                MarkerStyle::SingleWithNote('^', inline_note)
            } else {
                MarkerStyle::SpanWithNote('^', inline_note)
            }
        } else {
            MarkerStyle::Span('^')
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    Error,
    Warning,
}

//
//

enum MarkerStyle<'a> {
    // /// A single character marking the start of a span.
    // ///
    // /// Example:
    // ///     one two three
    // ///     ^
    // Single(char),
    /// A single character with an inline note.
    ///
    /// Example:
    ///    one two three
    ///   ^ note
    SingleWithNote(char, &'a str),

    /// A character repeated for the length of the span.
    ///
    /// Example:
    ///    one two three
    ///    ^^^^^^^^^^^^^
    Span(char),

    /// A character repeated for the length of the span with an inline note.
    ///
    /// Example:
    ///   one two three
    ///  ^^^^^^^^^^^^^ note
    SpanWithNote(char, &'a str),
}

fn format_code_snippet<Output: io::Write>(
    out: &mut Output,
    span: &Span,
    source: &SourceFile,
    marker: Option<(MarkerStyle, usize /*col*/, &str /* color */)>,
) -> io::Result<()> {
    let margin = 4;
    let sep = format!("{BLUE}|{RESET}");

    let lines = source.lines_for_span(span);
    writeln!(out, " {:>margin$} {sep}", "")?;
    for (i, line) in lines.iter() {
        writeln!(out, " {:>margin$} {sep} {}", i, line)?;
    }

    if let Some((style, col, color)) = marker {
        if lines.len() > 1 {
            return Ok(());
        }

        let marker = match style {
            // MarkerStyle::Single(c) => format!("{}{}", color, c),
            MarkerStyle::SingleWithNote(c, note) => format!("{}{} {}", color, c, note),
            MarkerStyle::Span(c) => {
                let mut marker = String::new();
                marker.push_str(color);
                for _ in 0..span.len() {
                    marker.push(c);
                }
                marker
            }
            MarkerStyle::SpanWithNote(c, note) => {
                let mut marker = String::new();
                marker.push_str(color);
                for _ in 0..span.len() {
                    marker.push(c);
                }
                marker.push_str(" ");
                marker.push_str(note);
                marker
            }
        };
        writeln!(
            out,
            " {:>margin$} {sep} {:>col$}{}{RESET}",
            "",
            "",
            marker,
            col = col - 1
        )?;
    }
    Ok(())
}
