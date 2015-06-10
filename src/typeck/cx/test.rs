use super::*;

use parser;

#[test]
fn cx_iter() {
    let mut cx = parser::parse_cx("X");
    assert!(cx.type_wf(&parser::parse_type("X")));
    assert!(!cx.type_wf(&parser::parse_type("Y")));
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
fn subtype_different_ids() {
    let mut cx = parser::parse_cx("A,B,C");
    let mut ty1 = &parser::parse_type("B");
    let mut ty2 = &parser::parse_type("C");
    assert!(!cx.subtype(ty1, ty2));
}

#[test]
fn subtype_same_id() {
    let mut cx = parser::parse_cx("A,B,C");
    let mut ty1 = &parser::parse_type("B");
    assert!(cx.subtype(ty1, ty1));
}

#[test]
fn subtype_arrow_id() {
    let mut cx = parser::parse_cx("A,B,C");
    let mut ty1 = &parser::parse_type("A -> B");
    let mut ty2 = &parser::parse_type("A -> C");
    assert!(cx.subtype(ty1, ty1));
    assert!(!cx.subtype(ty1, ty2));
}

#[test]
fn subtype_forall_r() {
    let mut cx = parser::parse_cx("A,B,C");
    let mut ty1 = &parser::parse_type("A");
    let mut ty2 = &parser::parse_type("forall x. x");
    assert!(!cx.subtype(ty1, ty2));
}
