use std::rc::Rc;
use rusty_peg::Symbol;

mod parser;
#[cfg(test)] mod test;

pub fn parse_term(input: &str) -> Term {
    let mut parser = parser::Grammar::new(());
    parser::TERM.parse_complete(&mut parser, input).unwrap()
}

pub fn parse_type(input: &str) -> Type {
    let mut parser = parser::Grammar::new(());
    parser::TYPE.parse_complete(&mut parser, input).unwrap()
}

pub fn parse_id(input: &str) -> Id {
    let mut parser = parser::Grammar::new(());
    parser::IDENTIFIER.parse_complete(&mut parser, input).unwrap()
}

///////////////////////////////////////////////////////////////////////////
// Identifiers

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Id<'input> {
    text: &'input str
}

impl<'input> Id<'input> {
    pub fn new(text: &'input str) -> Id<'input> {
        Id { text: text }
    }
}

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
