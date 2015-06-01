use ast;
use std::fmt::{Debug, Error, Formatter};
use std::ops::Deref;
use std::rc::Rc;

#[cfg(test)] mod test;

#[derive(Clone, Eq, PartialEq)]
pub struct Context<'input> {
    pub items: Vec<ContextItem<'input>>
}

impl<'input> Debug for Context<'input> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        let mut items: Vec<_> =
            self.items
                .iter()
                .map(|item| format!("{:?}", item))
                .collect();
        write!(fmt, "[{}]", items.connect(", "))
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
    pub fn new() -> Context<'input> {
        Context { items: vec![] }
    }

    pub fn add(mut self, item: ContextItem<'input>) -> Context<'input> {
        self.items.push(item);
        self
    }
}

///////////////////////////////////////////////////////////////////////////
// ContextIterator

