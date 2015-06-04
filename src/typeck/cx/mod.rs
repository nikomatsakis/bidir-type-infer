use ast::{self, ExistentialId, Type, TypeKind};
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
    ///////////////////////////////////////////////////////////////////////////
    // Construction

    pub fn new() -> Context<'input>
    {
        Context { items: vec![] }
    }

    pub fn add(mut self, item: ContextItem<'input>) -> Context<'input>
    {
        self.items.push(item);
        self
    }

    pub fn with<R,F>(&mut self, item: ContextItem<'input>, body: F) -> R
        where F: FnOnce(&mut Context<'input>) -> R
    {
        self.items.push(item);
        let r = body(self);
        self.items.pop();
        r
    }

    ///////////////////////////////////////////////////////////////////////////
    // Queries

    pub fn any<F>(&self, predicate: F) -> bool
        where F: FnMut(&ContextItem<'input>) -> bool
    {
        self.items.iter().any(predicate)
    }

    pub fn contains(&self, item: ContextItem<'input>) -> bool
    {
        self.any(|i| *i == item)
    }

    pub fn lookup(&self, id: ExistentialId) -> Option<Type<'input>>
    {
        self.items.iter()
                  .filter_map(|item| match *item {
                      ContextItem::ExistentialDecl(id1, ref v) if id == id1 => Some(v.clone()),
                      _ => None
                  })
                  .next()
                  .unwrap() // assumes that `id` is in scope
    }

    ///////////////////////////////////////////////////////////////////////////
    // Rules

    pub fn type_wf(&mut self, ty: &Type<'input>) -> bool
    {
        match *ty.kind() {
            TypeKind::Var(id) => {
                self.contains(ContextItem::TypeDecl(id))
            }
            TypeKind::Unit => {
                true
            }
            TypeKind::Existential(id) => {
                self.any(|item| match *item {
                    ContextItem::ExistentialDecl(id1, _) => id == id1,
                    _ => false
                })
            }
            TypeKind::ForAll(id, ref ty) => {
                self.with(ContextItem::TypeDecl(id), |this| this.type_wf(ty))
            }
            TypeKind::Arrow(ref a, ref b) => {
                self.type_wf(a) && self.type_wf(b)
            }
        }
    }

    pub fn subst(&self, ty: &Type<'input>) -> Type<'input> {
        match *ty.kind() {
            TypeKind::Var(_) |
            TypeKind::Unit => {
                ty.clone()
            }
            TypeKind::Existential(id) => {
                match self.lookup(id) {
                    Some(u) => u,
                    None => ty.clone()
                }
            }
            TypeKind::ForAll(id, ref ty) => {
                Type::new(TypeKind::ForAll(id, self.subst(ty)))
            }
            TypeKind::Arrow(ref a, ref b) => {
                Type::new(TypeKind::Arrow(self.subst(a), self.subst(b)))
            }
        }
    }
}
