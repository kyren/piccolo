use alloc::{boxed::Box, rc::Rc};

use hashbrown::HashSet;

pub trait StringInterner {
    type String: AsRef<[u8]> + Clone;

    fn intern(&mut self, s: &[u8]) -> Self::String;
}

impl<'a, S: StringInterner> StringInterner for &'a mut S {
    type String = S::String;

    fn intern(&mut self, s: &[u8]) -> Self::String {
        S::intern(self, s)
    }
}

#[derive(Default)]
pub struct BasicInterner(HashSet<Rc<[u8]>>);

impl StringInterner for BasicInterner {
    type String = Rc<[u8]>;

    fn intern(&mut self, string: &[u8]) -> Self::String {
        if let Some(s) = self.0.get(string) {
            s.clone()
        } else {
            let s = Rc::from(Box::from(string));
            self.0.insert(Rc::clone(&s));
            s
        }
    }
}
