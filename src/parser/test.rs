use super::*;
use ast::*;

#[test]
pub fn parse_left_leaning_call() {
    let term1 = parse_term("f x y z");
    let term2 = parse_term("((f x) y) z");
    let term3 = parse_term("f (x (y z))");
    assert_eq!(term1, term2);
    assert!(term1 != term3);
}

#[test]
pub fn parse_unit_term() {
    let term1 = parse_term("()");
    assert_eq!(term1.kind(), &TermKind::Unit);
}

#[test]
pub fn parse_unit_ascribe() {
    let term1 = parse_term("(x: Y)");
    assert_eq!(
        format!("{:?}", term1),
        "(x:Y)");
}

#[test]
pub fn parse_right_leaning_type() {
    let type1 = parse_type("X -> Y -> Z");
    let type2 = parse_type("X -> (Y -> Z)");
    let type3 = parse_type("(X -> Y) -> Z");
    assert_eq!(type1, type2);
    assert!(type1 != type3);
}

#[test]
pub fn parse_unit_type() {
    let type1 = parse_type("()");
    assert_eq!(type1.kind(), &TypeKind::Unit);
}

#[test]
pub fn parse_context() {
    let cx1 = parse_cx("X,>$1,$1,$2=Z");
    assert_eq!(
        format!("{:?}", cx1),
        "[X, >$1, $1, $2=Z]");
}
