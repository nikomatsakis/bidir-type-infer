use std::rc::Rc;
use rusty_peg::Symbol;

///////////////////////////////////////////////////////////////////////////
// Identifiers

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Id<'input> {
    text: &'input str
}

impl<'input> Id<'input> {
    pub fn new(text: &'input str) -> Id<'input> {
        Id { text: text }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ExistentialId(pub u32);

///////////////////////////////////////////////////////////////////////////
// Terms

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Term<'input> {
    kind: Rc<TermKind<'input>>
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TermKind<'input> {
    Var(Id<'input>),
    Unit,
    Lambda(Id<'input>, Term<'input>),
    Call(Term<'input>, Term<'input>),
    Ascription(Term<'input>, Type<'input>),
}

impl<'input> Term<'input> {
    pub fn new(kind: TermKind<'input>) -> Term<'input> {
        Term { kind: Rc::new(kind) }
    }

    pub fn kind(&self) -> &TermKind<'input> {
        &self.kind
    }
}

///////////////////////////////////////////////////////////////////////////
// Types

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Type<'input> {
    kind: Rc<TypeKind<'input>>
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeKind<'input> {
    Var(Id<'input>),
    Unit,
    Existential(ExistentialId),
    ForAll(Id<'input>, Type<'input>),
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
