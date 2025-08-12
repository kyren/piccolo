use super::file::IoFile;
use gc_arena::Collect;
use std::cell::RefCell;

#[derive(Collect, Clone)]
#[collect(no_drop)]
pub struct IoState {
    input: RefCell<IoFile>,
    output: RefCell<IoFile>,
}

impl IoState {
    pub fn new() -> Self {
        Self {
            input: RefCell::new(IoFile::stdin()),
            output: RefCell::new(IoFile::stdout()),
        }
    }
    pub fn replace_input(&self, input: IoFile) {
        self.input.replace(input);
    }
    pub fn replace_output(&self, output: IoFile) {
        self.output.replace(output);
    }
    pub fn input(&self) -> IoFile {
        self.input.borrow().clone()
    }
    pub fn output(&self) -> IoFile {
        self.output.borrow().clone()
    }
}
