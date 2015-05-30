use super::Context;
use super::ContextItem;

pub struct ContextIterator<'cx, 'input:'cx> {
    next: Option<&'cx Context<'input>>
}

impl<'cx,'input> ContextIterator<'cx,'input> {
    pub fn new(next: Option<&'cx Context<'input>>) -> ContextIterator<'cx,'input> {
        ContextIterator { next: next }
    }
}

impl<'cx, 'input> Iterator for ContextIterator<'cx, 'input> {
    type Item = &'cx ContextItem<'input>;

    fn next(&mut self) -> Option<&'cx ContextItem<'input>> {
        match self.next {
            None => None,
            Some(cx) => {
                self.next = cx.next.as_ref();
                Some(&cx.item)
            }
        }
    }
}
