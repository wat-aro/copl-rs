#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NatSource<'a> {
    pub raw: &'a str,
}

impl<'a> NatSource<'a> {
    pub fn new(raw: &'a str) -> Self {
        Self { raw }
    }
}
