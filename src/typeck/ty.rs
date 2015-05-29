use ast;
use std::rc::Rc;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Type<'input> {
    kind: Rc<TypeKind<'input>>
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeKind<'input> {
    Unit,
    Variable(ast::Id<'input>),
    Existential(u32),
    ForAll(ast::Id<'input>, Type<'input>),
    Arrow(Type<'input>, Type<'input>),
}

impl<'input> Type<'input> {
    pub fn new(kind: TypeKind<'input>) -> Type<'input> {
        Type { kind: Rc::new(kind) }
    }

    pub fn kind(&self) -> &TypeKind<'input> {
        &self.kind
    }
}
