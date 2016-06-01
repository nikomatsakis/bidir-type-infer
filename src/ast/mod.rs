use std::fmt::{Debug, Error, Formatter};
use std::rc::Rc;

#[cfg(test)]
mod test;

///////////////////////////////////////////////////////////////////////////
// Identifiers

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Id<'input> {
    text: &'input str
}

impl<'input> Id<'input> {
    pub fn new(text: &'input str) -> Id<'input> {
        Id { text: text }
    }
}

impl<'input> Debug for Id<'input> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        write!(fmt, "{}", self.text)
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct ExistentialId(pub u32);

impl Debug for ExistentialId {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        write!(fmt, "${}", self.0)
    }
}

///////////////////////////////////////////////////////////////////////////
// Terms

#[derive(Clone, Eq, PartialEq)]
pub struct Term<'input> {
    kind: Rc<TermKind<'input>>
}

#[derive(Clone, Eq, PartialEq)]
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

impl<'input> Debug for Term<'input> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        write!(fmt, "{:?}", self.kind())
    }
}

impl<'input> Debug for TermKind<'input> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            TermKind::Var(ref id) => write!(fmt, "{:?}", id),
            TermKind::Unit => write!(fmt, "()"),
            TermKind::Lambda(ref id, ref term) => write!(fmt, "(\\{:?}.{:?})", id, term),
            TermKind::Call(ref f, ref a) => write!(fmt, "({:?} {:?})", f, a),
            TermKind::Ascription(ref f, ref t) => write!(fmt, "({:?}:{:?})", f, t),
        }
    }
}

///////////////////////////////////////////////////////////////////////////
// Types

#[derive(Clone, Eq, PartialEq)]
pub struct Type<'input> {
    kind: Rc<TypeKind<'input>>
}

#[derive(Clone, Eq, PartialEq)]
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

    pub fn references(&self, id: ExistentialId) -> bool {
        match *self.kind {
            TypeKind::Var(_) |
            TypeKind::Unit =>
                false,
            TypeKind::Existential(id1) =>
                id == id1,
            TypeKind::ForAll(_, ref ty) =>
                ty.references(id),
            TypeKind::Arrow(ref a, ref b) =>
                a.references(id) || b.references(id),
        }
    }

    // [a/$a] ty
    pub fn instantiate(&self, from: Id<'input>, to: ExistentialId) -> Type<'input> {
        match *self.kind {
            TypeKind::Var(id) if id == from => {
                Type::new(TypeKind::Existential(to))
            }
            TypeKind::Var(_) |
            TypeKind::Unit => {
                self.clone()
            }
            TypeKind::Existential(id) => {
                assert!(id != to); // existential we are subst'ing in should have been fresh
                self.clone()
            }
            TypeKind::ForAll(id, ref ty) => {
                if id == from {
                    self.clone()
                } else {
                    Type::new(TypeKind::ForAll(id, ty.instantiate(from, to)))
                }
            }
            TypeKind::Arrow(ref a, ref b) => {
                Type::new(TypeKind::Arrow(a.instantiate(from, to), b.instantiate(from, to)))
            }
        }
    }
}

impl<'input> Debug for Type<'input> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        write!(fmt, "{:?}", self.kind())
    }
}

impl<'input> Debug for TypeKind<'input> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            TypeKind::Var(ref id) => write!(fmt, "{:?}", id),
            TypeKind::Unit => write!(fmt, "()"),
            TypeKind::Existential(ref id) => write!(fmt, "{:?}", id),
            TypeKind::ForAll(ref id, ref ty) => write!(fmt, "(forall {:?}.{:?})", id, ty),
            TypeKind::Arrow(ref f, ref t) => write!(fmt, "({:?} -> {:?})", f, t),
        }
    }
}

