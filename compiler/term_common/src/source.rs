use crate::id::declare_id;
pub use crate::span::Span;

declare_id!(SourceId);

/// A source file.
#[derive(Debug, Clone)]
pub struct SourceFile {
    /// The source id.
    id: SourceId,
    /// The name of the source file.
    name: String,
    /// The raw source code.
    source: String,
    /// The start and length of each line in the source code.
    lines: Vec<(usize, usize)>,
}

impl SourceFile {
    pub fn new(id: SourceId, name: String, source: String) -> Self {
        let lines = source
            .lines()
            .scan(0, |acc, line| {
                let len = line.len();
                let start = *acc;
                *acc += len + 1;
                Some((start, len))
            })
            .collect();

        Self {
            id,
            name,
            source,
            lines,
        }
    }

    /// Returns the source id.
    pub fn id(&self) -> SourceId {
        self.id
    }

    /// Returns the source file name.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns the raw source code.
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Returns the line and column number for the given offset (0-based).
    pub fn line_and_column(&self, offset: usize) -> (usize, usize) {
        self.lines
            .iter()
            .enumerate()
            .find(|(_, (start, len))| offset >= *start && offset <= *start + *len)
            .map(|(i, (start, _))| (i + 1, offset - start + 1))
            .unwrap_or((1, 1))
    }

    /// Returns the string for the given line number (1-based).
    pub fn line_str(&self, line: usize) -> Option<&str> {
        if line == 0 || line > self.lines.len() {
            return None;
        }

        self.lines.get(line - 1).map(|(start, len)| {
            let end = start + len;
            &self.source[*start..end]
        })
    }

    /// Returns the line number/string pairs for all lines in the given span.
    pub fn lines_for_span(&self, span: &Span) -> Vec<(usize, &str)> {
        let (start_line, _) = self.line_and_column(span.start);
        let (end_line, _) = self.line_and_column(span.end);
        (start_line..=end_line)
            .map(|line| (line, self.line_str(line).unwrap()))
            .collect()
    }
}

/// A collection of source programs.
#[derive(Debug, Clone)]
pub struct SourceMap {
    sources: Vec<SourceFile>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self { sources: vec![] }
    }

    pub fn len(&self) -> usize {
        self.sources.len()
    }

    pub fn get(&self, id: SourceId) -> Option<&SourceFile> {
        self.sources.get(id.raw)
    }

    pub fn add(&mut self, name: String, source: String) -> SourceId {
        let id = SourceId::new(self.sources.len());
        self.sources.push(SourceFile::new(id, name, source));
        id
    }

    pub fn first(&self) -> Option<&SourceFile> {
        self.sources.first()
    }
}

impl Default for SourceMap {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: AsRef<str>> From<T> for SourceMap {
    fn from(s: T) -> Self {
        let mut sm = SourceMap::new();
        sm.add("".to_string(), s.as_ref().to_string());
        sm
    }
}
