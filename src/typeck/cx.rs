use ast;
use super::ty;

pub struct Context<'input> {
    items: Vec<ContextItem<'input>>,
}

pub enum ContextItem<'input> {
    /// the type is in scope
    TypeDecl(ast::Id<'input>),

    /// variable with name `id` has type `ty`
    VarType(/*id*/ ast::Id<'input>, /*ty*/ ty::Type<'input>),

    /// existential variable is in scope
    ExistentialDecl(u32, Option<ty::Type<'input>>),

    /// marker for an existential variable
    Marker(u32),
}

