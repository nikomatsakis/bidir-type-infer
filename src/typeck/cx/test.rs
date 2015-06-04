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
