use super::*;
use parser;

#[test]
fn instantiate1() {
    let ty1 = parser::parse_type("forall a. B");
    let ty2 = parser::parse_type("forall a. $22");
    assert_eq!(ty1.instantiate(Id::new("B"), ExistentialId(22)), ty2);
}

#[test]
fn instantiate_captured() {
    let ty1 = parser::parse_type("forall B. B");
    assert_eq!(ty1.instantiate(Id::new("B"), ExistentialId(22)), ty1);
}

#[test]
fn instantiate_arrow() {
    let ty1 = parser::parse_type("(B -> C) -> (C -> B)");
    let ty2 = parser::parse_type("($22 -> C) -> (C -> $22)");
    let ty3 = parser::parse_type("(B -> $22) -> ($22 -> B)");
    assert_eq!(ty1.instantiate(Id::new("B"), ExistentialId(22)), ty2);
    assert_eq!(ty1.instantiate(Id::new("C"), ExistentialId(22)), ty3);
}
