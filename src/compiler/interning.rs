use std::{cell::RefCell, rc::Rc};

use rustc_hash::FxHashSet;

pub trait StringInterner {
    type String: AsRef<[u8]> + Clone;

    fn intern(&self, s: &[u8]) -> Self::String;
}

impl<'a, S: StringInterner> StringInterner for &'a S {
    type String = S::String;

    fn intern(&self, s: &[u8]) -> Self::String {
        S::intern(self, s)
    }
}

#[derive(Default)]
pub struct BasicInterner(RefCell<FxHashSet<Rc<[u8]>>>);

impl StringInterner for BasicInterner {
    type String = Rc<[u8]>;

    fn intern(&self, string: &[u8]) -> Self::String {
        let mut set = self.0.borrow_mut();
        if let Some(s) = set.get(string) {
            s.clone()
        } else {
            let s = Rc::from(Box::from(string));
            set.insert(Rc::clone(&s));
            s
        }
    }
}
