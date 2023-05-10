pub trait StringInterner {
    type String: AsRef<[u8]> + Clone;

    fn intern(&self, s: &[u8]) -> Self::String;
}

pub struct BoxInterner;

impl StringInterner for BoxInterner {
    type String = Box<[u8]>;

    fn intern(&self, s: &[u8]) -> Self::String {
        Box::from(s)
    }
}
