use ast::{self, ExistentialId, Id, Term, TermKind, Type, TypeKind};
use std::cmp;
use std::fmt::{Debug, Error, Formatter};
use std::ops::Deref;
use std::rc::Rc;

#[cfg(test)] mod test;

#[derive(Clone, Eq, PartialEq)]
pub struct Context<'input> {
    pub items: Vec<ContextItem<'input>>,
    pub existentials: u32,
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

pub struct TypeError(pub String);
pub type TypeResult<T> = Result<T, TypeError>;

impl<'input> Context<'input> {
    ///////////////////////////////////////////////////////////////////////////
    // Construction

    pub fn new() -> Context<'input>
    {
        Context { items: vec![], existentials: 0 }
    }

    pub fn add(mut self, item: ContextItem<'input>) -> Context<'input>
    {
        // make sure we track the largest id in our environment
        match item {
            ContextItem::ExistentialDecl(id, _) => {
                self.existentials = cmp::max(id.0+1, self.existentials);
            }
            _ => { }
        }

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

    pub fn lookup_existential(&self, id: ExistentialId) -> Option<Type<'input>>
    {
        self.items.iter()
                  .filter_map(|item| match *item {
                      ContextItem::ExistentialDecl(id1, ref v) if id == id1 => Some(v.clone()),
                      _ => None
                  })
                  .next()
                  .unwrap() // assumes that `id` is in scope
    }

    pub fn lookup_var(&self, id: Id<'input>) -> Option<Type<'input>>
    {
        self.items.iter()
                  .filter_map(|item| match *item {
                      ContextItem::VarType(id1, ref v) if id == id1 => Some(v.clone()),
                      _ => None
                  })
                  .next()
    }

    ///////////////////////////////////////////////////////////////////////////
    // Rules

    // In the paper:
    //
    //    Cx |- A <: B -| Cx'
    //
    // but here we use mutability. If true is returned, then `self` is
    // mutated in place and the subtyping holds, else `cx` is
    // unmodified.
    pub fn subtype(&mut self, a_ty: &Type<'input>, b_ty: &Type<'input>) -> TypeResult<()> {
        match (a_ty.kind(), b_ty.kind()) {
            // Var
            (&TypeKind::Var(a_id), &TypeKind::Var(b_id)) if a_id == b_id => {
                self.type_wf(a_ty)
            }

            // Unit
            (&TypeKind::Unit, &TypeKind::Unit) => {
                Ok(())
            }

            // Exvar
            (&TypeKind::Existential(a_id), &TypeKind::Existential(b_id)) if a_id == b_id => {
                self.type_wf(a_ty)
            }

            // <: ->
            (&TypeKind::Arrow(ref a_in, ref a_out), &TypeKind::Arrow(ref b_in, ref b_out)) => {
                self.try(|this| {
                    this.subtype(b_in, a_in)?;
                    let a_out1 = this.subst(a_out);
                    let b_out1 = this.subst(b_out);
                    this.subtype(&a_out1, &b_out1)
                })
            }

            // <: ForAll L
            (&TypeKind::ForAll(a_id, ref a_ty), _) => {
                self.try(|this| {
                    let beta = this.fresh_existential();
                    this.items.push(ContextItem::Marker(beta));
                    this.items.push(ContextItem::ExistentialDecl(beta, None));

                    let a_ty1 = a_ty.instantiate(a_id, beta);
                    this.subtype(&a_ty1, b_ty)?;
                    this.pop_marker(beta)
                })
            }

            // <: ForAll R
            (_, &TypeKind::ForAll(b_id, ref b_quantified_ty)) => {
                self.try(|this| {
                    this.items.push(ContextItem::TypeDecl(b_id));
                    this.subtype(a_ty, b_quantified_ty)?;
                    this.pop_type_decl(b_id)
                })
            }

            // <: InstantiateL
            (&TypeKind::Existential(a_id), _) => {
                self.try(|this| {
                    this.type_wf(a_ty)?;
                    assert!(!b_ty.references(a_id));
                    this.instantiate_left(a_id, b_ty)
                })
            }

            // <: InstantiateR
            (_, &TypeKind::Existential(b_id)) => {
                self.try(|this| {
                    this.type_wf(b_ty)?;
                    assert!(!a_ty.references(b_id));
                    this.instantiate_right(a_ty, b_id)
                })
            }

            _ => Err(TypeError(format!("subtype: no match")))
        }
    }

    pub fn instantiate_left(&mut self, alpha: ExistentialId, b_ty: &Type<'input>) -> TypeResult<()> {
        match *b_ty.kind() {
            TypeKind::Var(b_id) => {
                if let Some((a_index, None)) = self.find_existential_decl(alpha) {
                    // we need to check that the decl of b_id precedes
                    // alpha for cases like `$1 <: (forall x. x)`,
                    // which we want to fail
                    if !self.items[..a_index].contains(&ContextItem::TypeDecl(b_id)) {
                        Err(TypeError(format!("no such existential `{:?}`", b_id)))
                    } else {
                        self.assign(a_index, alpha, b_ty)
                    }
                } else {
                    // TODO wat
                    Err(TypeError(format!("no or invalid existential decl for `{:?}`", alpha)))
                }
            }
            TypeKind::Unit => { // InstLSolve
                if let Some((a_index, None)) = self.find_existential_decl(alpha) {
                    self.assign(a_index, alpha, b_ty)
                } else {
                    // TODO wat
                    Err(TypeError(format!("no or invalid existential decl for `{:?}`", alpha)))
                }
            }
            TypeKind::Existential(b_id) => { // InstLReach
                self.unify(alpha, b_id)
            }
            TypeKind::ForAll(id, ref ty) => { // InstLAllR
                self.try(|this| {
                    this.items.push(ContextItem::TypeDecl(id));
                    this.instantiate_left(alpha, ty)?;
                    this.pop_type_decl(id)
                })
            }
            TypeKind::Arrow(ref domain_ty, ref range_ty) => { // InstLArr
                match self.find_existential_decl(alpha) {
                    Some((alpha_index, None)) => {
                        self.try(|this| {
                            let domain_id = this.fresh_existential();
                            let range_id = this.fresh_existential();

                            // hmm, maybe a vec wasn't the best choice :)
                            this.assign(
                                alpha_index,
                                alpha,
                                &Type::new(TypeKind::Arrow(
                                    Type::new(TypeKind::Existential(domain_id)),
                                    Type::new(TypeKind::Existential(range_id)))));
                            this.items.insert(
                                alpha_index,
                                ContextItem::ExistentialDecl(
                                    domain_id,
                                    None));
                            this.items.insert(
                                alpha_index,
                                ContextItem::ExistentialDecl(
                                    range_id,
                                    None));

                            this.instantiate_right(domain_ty, domain_id)?;

                            let range_ty = this.subst(range_ty);
                            this.instantiate_left(range_id, &range_ty)
                        })
                    }
                    _ => {
                        Err(TypeError(format!("instantiate_left({:?}, {:?})", alpha, b_ty)))
                    }
                }
            }
        }
    }

    pub fn instantiate_right(&mut self, a_ty: &Type<'input>, alpha: ExistentialId) -> TypeResult<()> {
        match *a_ty.kind() {
            TypeKind::Var(a_id) => {
                if let Some((b_index, None)) = self.find_existential_decl(alpha) {
                    // we need to check that the decl of a_id precedes
                    // alpha for cases like `$1 <: (forall x. x)`,
                    // which we want to fail
                    if !self.items[..b_index].contains(&ContextItem::TypeDecl(a_id)) {
                        Err(TypeError(format!("instantiate_right({:?}, {:?}) -- ordering", a_ty, alpha)))
                    } else {
                        self.assign(b_index, alpha, a_ty)
                    }
                } else {
                    Err(TypeError(format!("instantiate_right({:?}, {:?})", a_ty, alpha)))
                }
            }
            TypeKind::Unit => { // InstRSolve
                if let Some((b_index, None)) = self.find_existential_decl(alpha) {
                    self.assign(b_index, alpha, a_ty)
                } else {
                    Err(TypeError(format!("instantiate_right({:?}, {:?})", a_ty, alpha)))
                }
            }
            TypeKind::Existential(b_id) => {
                self.unify(b_id, alpha)
            }
            TypeKind::ForAll(a_id, ref a_subty) => {
                self.try(|this| {
                    let beta = this.fresh_existential();
                    this.items.push(ContextItem::Marker(beta));
                    this.items.push(ContextItem::ExistentialDecl(beta, None));

                    let a_subty1 = a_subty.instantiate(a_id, beta);
                    this.instantiate_right(&a_subty1, alpha)?;
                    this.pop_marker(beta)
                })
            }
            TypeKind::Arrow(ref domain_ty, ref range_ty) => {
                match self.find_existential_decl(alpha) {
                    Some((alpha_index, None)) => {
                        self.try(|this| {
                            let domain_id = this.fresh_existential();
                            let range_id = this.fresh_existential();

                            // hmm, maybe a vec wasn't the best choice :)
                            this.assign(
                                alpha_index,
                                alpha,
                                &Type::new(TypeKind::Arrow(
                                    Type::new(TypeKind::Existential(domain_id)),
                                    Type::new(TypeKind::Existential(range_id)))));
                            this.items.insert(
                                alpha_index,
                                ContextItem::ExistentialDecl(domain_id, None));
                            this.items.insert(
                                alpha_index,
                                ContextItem::ExistentialDecl(range_id, None));

                            this.instantiate_left(domain_id, domain_ty)?;

                            let range_ty = this.subst(range_ty);
                            this.instantiate_right(&range_ty, range_id)
                        })
                    }
                    _ => {
                        Err(TypeError(format!("instantiate_right({:?}, {:?})", a_ty, alpha)))
                    }
                }
            }
        }
    }

    pub fn type_wf(&mut self, ty: &Type<'input>) -> TypeResult<()>
    {
        match *ty.kind() {
            TypeKind::Var(id) => {
                if self.contains(ContextItem::TypeDecl(id)) {
                    Ok(())
                } else {
                    Err(TypeError(format!("type_wf({:?}) -- no var decl", ty)))
                }
            }
            TypeKind::Unit => {
                Ok(())
            }
            TypeKind::Existential(id) => {
                if self.find_existential_decl(id).is_some() {
                    Ok(())
                } else {
                    Err(TypeError(format!("type_wf({:?}) -- no existential decl", ty)))
                }
            }
            TypeKind::ForAll(id, ref ty) => {
                self.with(ContextItem::TypeDecl(id), |this| this.type_wf(ty))
            }
            TypeKind::Arrow(ref a, ref b) => {
                self.type_wf(a)?;
                self.type_wf(b)
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
                match self.lookup_existential(id) {
                    Some(u) => self.subst(&u),
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

    pub fn try<F, T>(&mut self, op: F) -> TypeResult<T>
        where F: FnOnce(&mut Context<'input>) -> TypeResult<T>
    {
        let preserved: Context<'input> = self.clone();
        match op(self) {
            Ok(v) => Ok(v),
            Err(e) => {
                *self = preserved;
                Err(e)
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // Mutating methods
    //
    // These should generally be used inside of a `try` closure, unless
    // there are no further conditions to check.

    pub fn fresh_existential(&mut self) -> ExistentialId {
        let id = self.existentials;
        self.existentials += 1;
        ExistentialId(id)
    }

    pub fn pop_marker(&mut self, existential: ExistentialId) -> TypeResult<()> {
        while let Some(item) = self.items.pop() {
            match item {
                ContextItem::Marker(id) if id == existential => { return Ok(()); }
                _ => { }
            }
        }

        assert!(false, "marker for {:?} not found", existential);
        Err(TypeError(format!("pop_marker({:?})", existential)))
    }

    pub fn pop_type_decl(&mut self, id: Id<'input>) -> TypeResult<()> {
        while let Some(item) = self.items.pop() {
            match item {
                ContextItem::TypeDecl(id1) if id1 == id => { return Ok(()); }
                _ => { }
            }
        }

        assert!(false, "type decl for {:?} not found", id);
        Err(TypeError(format!("pop_type_decl({:?})", id)))
    }

    pub fn unify(&mut self, a: ExistentialId, b: ExistentialId) -> TypeResult<()> {
        match (self.find_existential_decl(a), self.find_existential_decl(b)) {
            (Some((a_index, None)), Some((b_index, None))) if a_index < b_index =>
                // InstLReach
                self.assign(b_index, b, &Type::new(TypeKind::Existential(a))),
            (Some((a_index, None)), Some((b_index, None))) if a_index >= b_index =>
                // InstLSolve
                self.assign(a_index, a, &Type::new(TypeKind::Existential(b))),
            _ =>
                Err(TypeError(format!("unify({:?}, {:?})", a, b)))
        }
    }

    pub fn assign(&mut self, index: usize, id: ExistentialId, ty: &Type<'input>) -> TypeResult<()> {
        match &mut self.items[index] {
            &mut ContextItem::ExistentialDecl(id1, ref mut v) => {
                assert_eq!(id, id1);
                assert!(v.is_none());
                *v = Some(ty.clone());
                Ok(())
            }
            _ => {
                assert!(false);
                Err(TypeError(format!("assign({:?}, {:?}, {:?})", index, id, ty)))
            }
        }
    }

    pub fn find_existential_decl(&self, id: ExistentialId)
                                 -> Option<(usize, Option<Type<'input>>)> {
        self.items.iter()
                  .enumerate()
                  .filter_map(|(index, item)| match *item {
                      ContextItem::ExistentialDecl(id1, ref fv) if id == id1 => {
                          Some((index, fv.clone()))
                      }
                      _ => None
                  })
                  .next()
    }
}
