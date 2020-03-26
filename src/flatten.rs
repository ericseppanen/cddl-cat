//! Tools for converting a [`cddl::ast`] (syntax tree) into an [`ivt`].
//!
//! This module is called "flatten" because its goal is to flatten syntax
//! tree detail that's not useful for validation.
//!
//! For example, this CDDL description:
//! ```text
//! name_type = tstr
//! name_group = (name: name_type)
//! object = { name_group }
//! ```
//! is functionally identical to this "flattened" one:
//! ```text
//! object = { name: tstr }
//! ```
//!

use crate::ivt::*;
use crate::util::ValidateError;
use cddl::ast::{self, CDDL};
use cddl::parser::cddl_from_str;
use std::collections::BTreeMap;

pub type FlattenResult<T> = std::result::Result<T, ValidateError>;

pub type RulesByName = BTreeMap<String, Node>;

pub fn flatten_from_str(cddl_input: &str) -> FlattenResult<RulesByName> {
    let cddl = cddl_from_str(cddl_input).map_err(|_e| {
        // FIXME: don't throw away the original error
        ValidateError::Oops("cddl parse error".to_string())
    })?;
    flatten(&cddl)
}

pub fn flatten(ast: &CDDL) -> FlattenResult<RulesByName> {
    println!("{:#?}", ast);

    let mut rules: RulesByName = ast.rules.iter().map(|rule| flatten_rule(rule)).collect();

    Ok(rules)
}

fn flatten_rule(rule: &ast::Rule) -> (String, Node) {
    match rule {
        ast::Rule::Type { rule, .. } => flatten_typerule(rule),
        _ => unimplemented!(),
    }
}

fn flatten_typerule(typerule: &ast::TypeRule) -> (String, Node) {
    // FIXME: handle generic_param
    // FIXME: handle is_type_choice_alternate
    println!("flatten_typerule {:#?}", typerule);
    let rhs = flatten_type(&typerule.value);
    (typerule.name.ident.clone(), rhs)
}

fn flatten_type(ty: &ast::Type) -> Node {
    // FIXME: len > 1 means we should emit a Choice instead.
    assert!(ty.type_choices.len() == 1);
    let ty1 = &ty.type_choices[0];
    flatten_type1(ty1)
}

fn flatten_type1(ty1: &ast::Type1) -> Node {
    // FIXME: handle range & control operators.
    flatten_type2(&ty1.type2)
}

fn flatten_type2(ty2: &ast::Type2) -> Node {
    use ast::Type2;
    match ty2 {
        // FIXME: this casting is gross.
        Type2::UintValue { value, .. } => Node::Literal(Literal::Int(*value as i128)),
        _ => unimplemented!(),
    }
}

#[test]
fn flatten_literal_int() {
    let cddl_input = r#"thing = 1"#;
    let result = flatten_from_str(cddl_input).unwrap();
    let result = format!("{:?}", result);
    assert_eq!(result, r#"{"thing": Literal(Int(1))}"#);
}
