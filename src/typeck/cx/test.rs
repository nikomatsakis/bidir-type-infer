use super::*;

use ast::ExistentialId;
use parser;

#[test]
fn cx_iter() {
    let mut cx = parser::parse_cx("X");
    assert!(cx.type_wf(&parser::parse_type("X")).is_ok());
    assert!(cx.type_wf(&parser::parse_type("Y")).is_err());
}

#[test]
fn cx_subst() {
    let mut cx = parser::parse_cx("X,Y,$1=Y");

    assert_eq!(cx.subst(&parser::parse_type("X")),
               parser::parse_type("X"));

    assert_eq!(cx.subst(&parser::parse_type("Y")),
               parser::parse_type("Y"));

    assert_eq!(cx.subst(&parser::parse_type("$1")),
               parser::parse_type("Y"));

    assert_eq!(cx.subst(&parser::parse_type("()")),
               parser::parse_type("()"));

    assert_eq!(cx.subst(&parser::parse_type("X -> (Y -> $1)")),
               parser::parse_type("X -> (Y -> Y)"));
}

#[test]
fn cx_subst_arrow() {
    let mut cx = parser::parse_cx("X,Y,$1=X,$2=Y");

    assert_eq!(cx.subst(&parser::parse_type("$1 -> ($2 -> $1)")),
               parser::parse_type("X -> (Y -> X)"));
}

#[test]
fn cx_subst_chain() {
    let mut cx = parser::parse_cx("X,$1=X,$2=$1");

    assert_eq!(cx.subst(&parser::parse_type("$2")),
               parser::parse_type("X"));
}

#[test]
fn subtype_different_ids() {
    let mut cx = parser::parse_cx("A,B,C");
    let mut ty1 = &parser::parse_type("B");
    let mut ty2 = &parser::parse_type("C");
    assert!(cx.subtype(ty1, ty2).is_err());
}

#[test]
fn subtype_same_id() {
    let mut cx = parser::parse_cx("A,B,C");
    let mut ty1 = &parser::parse_type("B");
    assert!(cx.subtype(ty1, ty1).is_ok());
}

#[test]
fn subtype_arrow_id() {
    let mut cx = parser::parse_cx("A,B,C");
    let mut ty1 = &parser::parse_type("A -> B");
    let mut ty2 = &parser::parse_type("A -> C");
    assert!(cx.subtype(ty1, ty1).is_ok());
    assert!(cx.subtype(ty1, ty2).is_err());
}

#[test]
fn subtype_forall_r() {
    let mut cx = parser::parse_cx("A,B,C");
    let mut ty1 = &parser::parse_type("A");
    let mut ty2 = &parser::parse_type("forall x. x");
    assert!(cx.subtype(ty1, ty2).is_err());
}

#[test]
fn subtype_inst_left_two_vars_1() {
    // instantiation order decides which becomes a pointer to which here:
    let mut cx = parser::parse_cx("$1,$2");
    let mut ty1 = parser::parse_type("$1");
    let mut ty2 = parser::parse_type("$2");

    assert!(cx.instantiate_left(ExistentialId(1), &ty2).is_ok());
    assert_eq!(cx.subst(&ty1), ty1);
    assert_eq!(cx.subst(&ty2), ty1);
}

#[test]
fn subtype_inst_left_two_vars_2() {
    // instantiation order decides which becomes a pointer to which here:
    let mut cx = parser::parse_cx("$2,$1");
    let mut ty1 = parser::parse_type("$1");
    let mut ty2 = parser::parse_type("$2");

    assert!(cx.instantiate_left(ExistentialId(1), &ty2).is_ok());
    assert_eq!(cx.subst(&ty1), ty2);
    assert_eq!(cx.subst(&ty2), ty2);
}

#[test]
fn subtype_inst_left_existential_forall() {
    let mut cx = parser::parse_cx("$1");
    let mut ty1 = parser::parse_type("$1");
    let mut ty2 = parser::parse_type("forall x. x");

    // type variables in this system cannot be instantiated with
    // higher-ranked values
    assert!(cx.instantiate_left(ExistentialId(1), &ty2).is_err());
}

#[test]
fn subtype_inst_left_existential_unit() {
    let mut cx = parser::parse_cx("$1");
    let mut ty1 = parser::parse_type("$1");
    let mut ty2 = parser::parse_type("()");

    assert!(cx.instantiate_left(ExistentialId(1), &ty2).is_ok());
    assert_eq!(cx.subst(&ty1), ty2);
}

#[test]
fn subtype_inst_left_existential_var_bad() {
    // X must be in scope before $1 is declared

    let mut cx = parser::parse_cx("$1,X");
    let mut ty1 = parser::parse_type("$1");
    let mut ty2 = parser::parse_type("X");

    assert!(cx.instantiate_left(ExistentialId(1), &ty2).is_err());
}

#[test]
fn subtype_inst_left_existential_var_ok() {
    // X must be in scope before $1 is declared

    let mut cx = parser::parse_cx("X,$1");
    let mut ty1 = parser::parse_type("$1");
    let mut ty2 = parser::parse_type("X");

    assert!(cx.instantiate_left(ExistentialId(1), &ty2).is_ok());
    assert_eq!(cx.subst(&ty1), ty2);
}

#[test]
fn subtype_inst_left_arrow_ok() {
    // X must be in scope before $1 is declared

    let mut cx = parser::parse_cx("X,Y,$1");
    let mut ty1 = parser::parse_type("$1");
    let mut ty2 = parser::parse_type("X -> Y");

    assert!(cx.instantiate_left(ExistentialId(1), &ty2).is_ok());
    assert_eq!(cx.subst(&ty1), ty2);
}
