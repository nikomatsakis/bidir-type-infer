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

    pub fn prefix(&self, length: usize) -> Context<'input> {
        Context {
            items: self.items[..length].to_owned(),
            existentials: self.existentials,
        }
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
    //    Cx |- e => A -| Cx'
    //
    // but here we use mutability. If ok is returned, then `self` is
    // mutated in place and the subtyping holds, else `cx` is
    // unmodified.
    //pub fn term_type(&mut self, e_term: &Term<'input>) -> TypeResult<Type<'input>> {
    //    match e_term.kind() {
    //        &TermKind::Var(id) => { // Var
    //            match self.lookup_var(id) {
    //                Some(t) => Ok(t),
    //                None => Err(TypeError(format!("term_type({:?}) -- no such var", e_term)))
    //            }
    //        }
    //
    //        &TermKind::Ascription(ref term, ref ty) => { // Anno
    //            self.ascribe_term(term, ty)?;
    //            Ok(ty.clone())
    //        }
    //
    //        &TermKind::Unit => { // 1|=>
    //            Ok(Type::new(TypeKind::Unit))
    //        }
    //
    //        &TermKind::Lambda(ref x, ref body) => { // ->|=>
    //            unimplemented!()
    //        }
    //
    //        &TermKind::Call(ref func, ref arg) => { // ->E
    //            unimplemented!()
    //        }
    //    }
    //}

    // In the paper:
    //
    //    Cx |- e <= A -| Cx'
    //
    // but here we use mutability. If ok is returned, then `self` is
    // mutated in place and the subtyping holds, else `cx` is
    // unmodified.
    //pub fn ascribe_term(&mut self, e_term: &Term<'input>, a_ty: &Type<'input>) -> TypeResult<()> {
    //    match e_term.kind() {
    //        &
    //    }
    //}

    // In the paper:
    //
    //    Cx |- A <: B -| Cx'
    //
    // but here we use mutability. If ok is returned, then `self` is
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
            (&TypeKind::ForAll(alpha_id, ref a_ty), _) => {
                self.try(|this| {
                    let alpha_hat = this.fresh_existential();
                    this.items.push(ContextItem::Marker(alpha_hat));
                    this.items.push(ContextItem::ExistentialDecl(alpha_hat, None));

                    let a_ty = a_ty.instantiate(alpha_id, alpha_hat);
                    this.subtype(&a_ty, b_ty)?;
                    this.pop_marker(alpha_hat)
                })
            }

            // <: ForAll R
            (_, &TypeKind::ForAll(alpha_id, ref b_ty)) => {
                self.try(|this| {
                    this.items.push(ContextItem::TypeDecl(alpha_id));
                    this.subtype(a_ty, b_ty)?;
                    this.pop_type_decl(alpha_id)
                })
            }

            // <: InstantiateL or <: InstantiateR combined with
            // InstLReach or InstRReach
            (&TypeKind::Existential(a_id), &TypeKind::Existential(b_id)) => {
                let a_index = self.find_unbound_existential(a_id)?;
                let b_index = self.find_unbound_existential(b_id)?;
                if a_index < b_index { // will use InstLReach
                    self.instantiate_left(a_id, b_ty)
                } else { // will use InstRReach
                    self.instantiate_right(a_ty, b_id)
                }
            }

            // <: InstantiateL
            (&TypeKind::Existential(a_id), _) => {
                self.try(|this| {
                    if !b_ty.references(a_id) {
                        this.type_wf(a_ty)?;
                        this.instantiate_left(a_id, b_ty)
                    } else {
                        Err(TypeError(format!("subtype({:?}, {:?}) -- cycle on {:?}", a_ty, b_ty, a_id)))
                    }
                })
            }

            // <: InstantiateR
            (_, &TypeKind::Existential(b_id)) => {
                self.try(|this| {
                    if !a_ty.references(b_id) {
                        this.type_wf(b_ty)?;
                        this.instantiate_right(a_ty, b_id)
                    } else {
                        Err(TypeError(format!("subtype({:?}, {:?}) -- cycle on {:?}", a_ty, b_ty, b_id)))
                    }
                })
            }

            _ => Err(TypeError(format!("subtype: no match")))
        }
    }

    pub fn instantiate_left(&mut self, alpha_id: ExistentialId, b_ty: &Type<'input>) -> TypeResult<()> {
        // all rules require Cx[alpha]
        let alpha_index = self.find_unbound_existential(alpha_id)?;

        // InstLReach
        if let &TypeKind::Existential(beta_id) = b_ty.kind() {
            let beta_index = self.find_unbound_existential(beta_id)?;
            if alpha_index < beta_index {
                return self.assign(beta_index, beta_id, &Type::new(TypeKind::Existential(alpha_id)));
            }
        }

        match *b_ty.kind() {
            TypeKind::Existential(_) | TypeKind::Var(_) | TypeKind::Unit => { // InstLSolve
                self.prefix(alpha_index).type_wf(b_ty)?;
                self.assign(alpha_index, alpha_id, b_ty)
            }
            TypeKind::ForAll(id, ref ty) => { // InstLAllR
                self.try(|this| {
                    this.items.push(ContextItem::TypeDecl(id));
                    this.instantiate_left(alpha_id, ty)?;
                    this.pop_type_decl(id)
                })
            }
            TypeKind::Arrow(ref domain_ty, ref range_ty) => { // InstLArr
                self.try(|this| {
                    let domain_id = this.fresh_existential();
                    let range_id = this.fresh_existential();

                    // insert decl of `domain_id`, `range_id` before
                    // `alpha_id`, so that we can assign `domain_id ->
                    // range_id` to `alpha`; the need for this
                    // suggests a vec isn't really the best overall
                    // choice, but whatever :)
                    this.assign(
                        alpha_index,
                        alpha_id,
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
        }
    }

    pub fn instantiate_right(&mut self, a_ty: &Type<'input>, alpha_id: ExistentialId) -> TypeResult<()> {
        // all rules requires Cx[alpha]
        let alpha_index = self.find_unbound_existential(alpha_id)?;

        // InstRReach
        if let &TypeKind::Existential(beta_id) = a_ty.kind() {
            let beta_index = self.find_unbound_existential(beta_id)?;
            if alpha_index < beta_index {
                return self.assign(beta_index, beta_id, &Type::new(TypeKind::Existential(alpha_id)));
            }
        }

        match *a_ty.kind() {
            TypeKind::Existential(_) | TypeKind::Var(_) | TypeKind::Unit => { // InstRSolve
                self.prefix(alpha_index).type_wf(a_ty)?;
                self.assign(alpha_index, alpha_id, a_ty)
            }
            TypeKind::ForAll(beta_id, ref b_ty) => { // InstRAllL
                self.try(|this| {
                    let beta_hat_id = this.fresh_existential();
                    this.items.push(ContextItem::Marker(beta_hat_id));
                    this.items.push(ContextItem::ExistentialDecl(beta_hat_id, None));

                    let b_ty = b_ty.instantiate(beta_id, beta_hat_id);
                    this.instantiate_right(&b_ty, alpha_id)?;
                    this.pop_marker(beta_hat_id)
                })
            }
            TypeKind::Arrow(ref domain_ty, ref range_ty) => { // InstRArr
                self.try(|this| {
                    let domain_id = this.fresh_existential();
                    let range_id = this.fresh_existential();

                    this.assign(
                        alpha_index,
                        alpha_id,
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

    pub fn find_unbound_existential(&self, alpha: ExistentialId)
                                    -> TypeResult<usize> {
        match self.find_existential_decl(alpha) {
            Some((idx, None)) => Ok(idx),
            Some((idx, Some(_))) => Err(TypeError(format!("find_unbound_existential({:?}) -- already bound", alpha))),
            None => Err(TypeError(format!("find_unbound_existential({:?}) -- not in scope", alpha))),
        }
    }
}
