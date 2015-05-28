use super::*;

use std::rc::Rc; // bug in rusty-peg

rusty_peg! {
    parser Grammar<'input> {
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
            ("(", <t:TERM>, ":", <ty:TYPE>, ")") => Term::new(TermKind::Typed(t, ty));

        PAREN_TERM: Term<'input> =
            ("(", <t:TERM>, ")") => t;

        TYPE: Type<'input> =
            (ARROW_TYPE / NON_ARROW_TYPE);

        ARROW_TYPE: Type<'input> =
            (<l:NON_ARROW_TYPE>, "->", <t:TYPE>) => Type::new(TypeKind::Arrow(l, t));

        NON_ARROW_TYPE: Type<'input> =
            (UNIT_TYPE / VAR_TYPE / FORALL_TYPE / PAREN_TYPE);

        UNIT_TYPE: Type<'input> =
            ("1") => Type::new(TypeKind::Unit);

        FORALL_TYPE: Type<'input> =
            ("forall", <id:IDENTIFIER>, ".", <ty:TYPE>) => Type::new(TypeKind::ForAll(id, ty));

        VAR_TYPE: Type<'input> =
            (<id:IDENTIFIER>) => Type::new(TypeKind::Var(id));

        PAREN_TYPE: Type<'input> =
            ("(", <t:TYPE>, ")") => t;

        IDENTIFIER: Id<'input> =
            (<i:IDENTIFIER_RE>) => Id::new(i);

        IDENTIFIER_RE: &'input str =
            regex(r"[a-zA-Z_][a-zA-Z0-9_]*") - ["forall"];
    }
}
