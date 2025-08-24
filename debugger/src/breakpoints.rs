use std::collections::{BTreeSet, HashMap};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Breakpoint {
    pub id: usize,
    pub source: String,
    pub line: usize,
}

#[derive(Debug, Default, Clone)]
pub struct Breakpoints {
    breakpoints: BTreeSet<Breakpoint>,
    next_breakpoint: usize,
}

impl Breakpoints {
    pub fn add(&mut self, chunk: impl Into<String>, line: usize) -> usize {
        let chunk = chunk.into();
        let id = self.next_breakpoint;
        self.next_breakpoint += 1;
        self.breakpoints.insert(Breakpoint {
            id,
            source: chunk,
            line,
        });
        id
    }

    pub fn remove(&mut self, chunk: &str, line: usize) {
        self.breakpoints
            .retain(|breakpoint| !(breakpoint.source == chunk && breakpoint.line == line))
    }

    pub fn list(&self) -> HashMap<String, Vec<usize>> {
        let sources: BTreeSet<&str> = self
            .breakpoints
            .iter()
            .map(|breakpoint| breakpoint.source.as_str())
            .collect();
        sources
            .into_iter()
            .map(|source| {
                (
                    source.to_owned(),
                    self.breakpoints
                        .iter()
                        .filter(|breakpoint| breakpoint.source == source)
                        .map(|breakpoint| breakpoint.line)
                        .collect(),
                )
            })
            .collect()
    }

    pub fn matches(&self, chunk: &str, line: usize) -> Vec<usize> {
        self.breakpoints
            .iter()
            .filter(|breakpoint| breakpoint.source == chunk && breakpoint.line == line)
            .map(|breakpoint| breakpoint.id)
            .collect()
    }
}
