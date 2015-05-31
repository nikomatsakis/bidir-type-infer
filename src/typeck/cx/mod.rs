use ast;
use std::fmt::{Debug, Error, Formatter};
use std::ops::Deref;
use std::rc::Rc;

pub mod iter;
#[cfg(test)] mod test;

#[derive(Clone, Eq, PartialEq)]
pub struct Context<'input> {
    data: Rc<ContextData<'input>>
}

#[derive(Debug, Eq, PartialEq)]
pub struct ContextData<'input> {
    pub item: ContextItem<'input>,
    pub next: Option<Context<'input>>
}

impl<'input> Deref for Context<'input> {
    type Target = ContextData<'input>;
    fn deref(&self) -> &ContextData<'input> {
        &*self.data
    }
}

impl<'input> Debug for Context<'input> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        try!(write!(fmt, "["));
        let mut first = true;
        for item in self.iter() {
            if !first { try!(write!(fmt, ",")); }
            try!(write!(fmt, "{:?}", item));
            first = false;
        }
        try!(write!(fmt, "]"));
        Ok(())
    }
}

#[derive(Clone, Eq, PartialEq)]
pub enum ContextItem<'input> {
    /// the type is in scope
    TypeDecl(ast::Id<'input>),

    /// variable with name `id` has type `ty`
    VarType(/*id*/ ast::Id<'input>, /*ty*/ ast::Type<'input>),

    /// existential variable is in scope
    ExistentialDecl(ast::ExistentialId, Option<ast::Type<'input>>),

    /// marker for an existential variable
    Marker(ast::ExistentialId),
}

impl<'input> Debug for ContextItem<'input> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            ContextItem::TypeDecl(ref id) => write!(fmt, "{:?}", id),
            ContextItem::VarType(ref id, ref ty) => write!(fmt, "{:?}:{:?}", id, ty),
            ContextItem::ExistentialDecl(ref id, None) => write!(fmt, "{:?}", id),
            ContextItem::ExistentialDecl(ref id, Some(ref ty)) => write!(fmt, "{:?}={:?}", id, ty),
            ContextItem::Marker(ref id) => write!(fmt, ">{:?}", id),
        }
    }
}

impl<'input> Context<'input> {
    pub fn new(item: ContextItem<'input>, next: Option<Context<'input>>) -> Context<'input> {
        Context { data: Rc::new(ContextData {
            item: item,
            next: next
        })}
    }

    pub fn root(item: ContextItem<'input>) -> Context<'input> {
        Context::new(item, None)
    }

    /// adds a new item to the context
    pub fn add(&self, item: ContextItem<'input>) -> Context<'input> {
        Context::new(item, Some(self.clone()))
    }

    /// walks the items in the context, from most recently added to the end
    pub fn iter<'cx>(&'cx self) -> iter::ContextIterator<'cx, 'input> {
        iter::ContextIterator::new(Some(self))
    }
}

///////////////////////////////////////////////////////////////////////////
// ContextIterator

