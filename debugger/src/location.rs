use piccolo::compiler::FunctionRef;

#[derive(Debug, Clone)]
pub struct Location {
    chunk_name: String,
    function_ref: FunctionRef<String>,
    line: usize,
}

impl Location {
    pub fn new(chunk_name: String, function_ref: FunctionRef<String>, line: usize) -> Self {
        Self {
            chunk_name,
            function_ref,
            line,
        }
    }

    pub fn chunk(&self) -> &str {
        &self.chunk_name
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn function_ref(&self) -> &FunctionRef<String> {
        &self.function_ref
    }
}

impl PartialEq for Location {
    fn eq(&self, other: &Self) -> bool {
        self.chunk_name == other.chunk_name && self.line == other.line
    }
}

impl Eq for Location {}
