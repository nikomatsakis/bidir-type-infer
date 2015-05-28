use super::*;

#[test]
pub fn parse_left_leaning_call() {
    let term1 = parse_term("f x y z");
    let term2 = parse_term("((f x) y) z");
    let term3 = parse_term("f (x (y z))");
    assert_eq!(term1, term2);
    assert!(term1 != term3);
}

#[test]
pub fn parse_right_leaning_type() {
    let type1 = parse_type("X -> Y -> Z");
    let type2 = parse_type("X -> (Y -> Z)");
    let type3 = parse_type("(X -> Y) -> Z");
    assert_eq!(type1, type2);
    assert!(type1 != type3);
}
