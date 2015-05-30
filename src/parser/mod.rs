use ast::{ExistentialId, Id, Term, TermKind, Type, TypeKind};
use rusty_peg::Symbol;
use std::str::FromStr;
use typeck::cx;

#[cfg(test)] mod test;

rusty_peg! {
    parser Grammar<'input> {
        ///////////////////////////////////////////////////////////////////////////
        // AST TERMS

        TERM: Term<'input> =
            fold(<lhs:NON_CALL_TERM>,
                 (<rhs:NON_CALL_TERM>) => Term::new(TermKind::Call(lhs, rhs)));

        NON_CALL_TERM: Term<'input> =
            (VARIABLE_TERM / UNIT_TERM / LAMBDA_TERM / ASCRIPTION_TERM / PAREN_TERM);

        VARIABLE_TERM: Term<'input> =
            (<id:IDENTIFIER>) => Term::new(TermKind::Var(id));

        UNIT_TERM: Term<'input> =
            ("(", ")") => Term::new(TermKind::Unit);

        LAMBDA_TERM: Term<'input> =
            ("\\", <id:IDENTIFIER>, ".", <t:TERM>) => Term::new(TermKind::Lambda(id, t));

        ASCRIPTION_TERM: Term<'input> =
            ("(", <t:TERM>, ":", <ty:TYPE>, ")") => Term::new(TermKind::Ascription(t, ty));

        PAREN_TERM: Term<'input> =
            ("(", <t:TERM>, ")") => t;

        ///////////////////////////////////////////////////////////////////////////
        // AST TYPES

        TYPE: Type<'input> =
            (ARROW_TYPE / NON_ARROW_TYPE);

        ARROW_TYPE: Type<'input> =
            (<l:NON_ARROW_TYPE>, "->", <t:TYPE>) => Type::new(TypeKind::Arrow(l, t));

        NON_ARROW_TYPE: Type<'input> =
            (UNIT_TYPE / VAR_TYPE / FORALL_TYPE / PAREN_TYPE);

        UNIT_TYPE: Type<'input> =
            ("()") => Type::new(TypeKind::Unit);

        FORALL_TYPE: Type<'input> =
            ("forall", <id:IDENTIFIER>, ".", <ty:TYPE>) => Type::new(TypeKind::ForAll(id, ty));

        VAR_TYPE: Type<'input> =
            (<id:IDENTIFIER>) => Type::new(TypeKind::Var(id));

        PAREN_TYPE: Type<'input> =
            ("(", <t:TYPE>, ")") => t;

        ///////////////////////////////////////////////////////////////////////////
        // TYPECK CONTEXTS

        CX: cx::Context<'input> =
            fold(<lhs:CX_ROOT>, (",", <rhs:CX_ITEM>) => lhs.add(rhs));

        CX_ROOT: cx::Context<'input> =
            (<i:CX_ITEM>) => cx::Context::root(i);

        CX_ITEM: cx::ContextItem<'input> =
            (CX_VAR_TYPE / CX_TYPE_DECL / CX_EXISTENTIAL_DECL1 / CX_EXISTENTIAL_DECL2 / CX_MARKER);

        CX_VAR_TYPE: cx::ContextItem<'input> =
            (<i:IDENTIFIER>, ":", <t:TYPE>) => cx::ContextItem::VarType(i, t);

        CX_TYPE_DECL: cx::ContextItem<'input> =
            (<i:IDENTIFIER>) => cx::ContextItem::TypeDecl(i);

        CX_EXISTENTIAL_DECL1: cx::ContextItem<'input> =
            (<i:EXISTENTIAL>, ":", <t:TYPE>) => cx::ContextItem::ExistentialDecl(i, Some(t));

        CX_EXISTENTIAL_DECL2: cx::ContextItem<'input> =
            (<i:EXISTENTIAL>) => cx::ContextItem::ExistentialDecl(i, None);

        CX_MARKER: cx::ContextItem<'input> =
            (">", <i:EXISTENTIAL>) => cx::ContextItem::Marker(i);

        ///////////////////////////////////////////////////////////////////////////
        // IDENTIFIERS

        IDENTIFIER: Id<'input> =
            (<i:IDENTIFIER_RE>) => Id::new(i);

        IDENTIFIER_RE: &'input str =
            regex(r"[a-zA-Z_][a-zA-Z0-9_]*") - ["forall"];

        EXISTENTIAL: ExistentialId =
            (<s:EXISTENTIAL_RE>) => ExistentialId(u32::from_str(&s[1..]).unwrap());

        EXISTENTIAL_RE: &'input str =
            regex(r"$[0-9]+");
    }
}

pub fn parse_term(input: &str) -> Term {
    let mut parser = Grammar::new(());
    TERM.parse_complete(&mut parser, input).unwrap()
}

pub fn parse_type(input: &str) -> Type {
    let mut parser = Grammar::new(());
    TYPE.parse_complete(&mut parser, input).unwrap()
}

pub fn parse_id(input: &str) -> Id {
    let mut parser = Grammar::new(());
    IDENTIFIER.parse_complete(&mut parser, input).unwrap()
}
