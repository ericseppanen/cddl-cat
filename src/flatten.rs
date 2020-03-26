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
use std::sync::Arc;

pub type FlattenResult<T> = std::result::Result<T, ValidateError>;

pub type MutateResult = std::result::Result<(), ValidateError>;

pub type RulesByName = BTreeMap<String, ArcNode>;

pub fn flatten_from_str(cddl_input: &str) -> FlattenResult<RulesByName> {
    let cddl = cddl_from_str(cddl_input).map_err(|e| {
        // FIXME: don't throw away the original error
        let msg = format!("cddl parse error {}", e);
        ValidateError::Oops(msg)
    })?;
    flatten(&cddl)
}

pub fn flatten(ast: &CDDL) -> FlattenResult<RulesByName> {
    println!("{:#?}", ast);

    // This first pass generates a tree of Nodes from the AST.
    let mut rules: RulesByName = ast.rules.iter().map(|rule| flatten_rule(rule)).collect();
    // This second pass substitutes Weak references for by-name rule references.
    replace_rule_refs(&mut rules);
    Ok(rules)
}

// Descend recursively into a tree of Nodes, possibly making changes along the way.
fn mutate_node_tree<F>(node: &mut Node, func: &F)
where
    F: FnMut(&Node) -> MutateResult,
{
    match node {
        Node::Literal(_) => (),     // leaf node
        Node::PreludeType(_) => (), // leaf node
        Node::Rule(_) => (),        // leaf node
        Node::Choice(c) => {
            for mut option in &mut c.options {
                let mut_option = Arc::get_mut(&mut option).unwrap();
                mutate_node_tree(mut_option, func);
            }
        }
        Node::Map(m) => {
            for mut kv in &mut m.members {
                let mut_key = Arc::get_mut(&mut kv.key).unwrap();
                mutate_node_tree(mut_key, func);
                let mut_value = Arc::get_mut(&mut kv.value).unwrap();
                mutate_node_tree(mut_value, func);
            }
        }
        //Node::ArrayRecord(a) => ___,
        //Node::ArrayVec(a) => ___,
        _ => unimplemented!(),
    }
}

fn replace_rule_refs(rules: &mut RulesByName) {
    for (rule_name, mut arcnode) in rules.iter_mut() {
        let mut root = Arc::get_mut(arcnode).unwrap();
        mutate_node_tree(&mut root, &|node| {
            match node {
                Node::Rule(rule_ref) => {
                    println!("hello {}", rule_ref.name);
                    // FIXME: add graceful handling of nonexistent rule name
                    let real_ref = rules.get(&rule_ref.name).unwrap();
                    rule_ref.upgrade(real_ref);
                }
                _ => (),
            }
            Ok(())
        })
    }
}

// Given a Node containing a Rule reference, replace that Node with
// a reference to the actual rule tree
fn replace_node(dest: &mut Node, src: Node) {
    *dest = src;
}

fn flatten_rule(rule: &ast::Rule) -> (String, ArcNode) {
    let (name, node) = match rule {
        ast::Rule::Type { rule, .. } => flatten_typerule(rule),
        _ => unimplemented!(),
    };
    (name, Arc::new(node))
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
        Type2::TextValue { value, .. } => Node::Literal(Literal::Text(value.clone())),
        Type2::Typename { ident, .. } => flatten_typename(&ident.ident),
        Type2::Map { group, .. } => flatten_map(&group),
        _ => unimplemented!(),
    }
}

fn flatten_typename(name: &str) -> Node {
    match name {
        "int" => Node::PreludeType(PreludeType::Int),
        "tstr" => Node::PreludeType(PreludeType::Tstr),
        // FIXME: lots more prelude types to handle...
        // FIXME: this could be a group name, maybe other things?
        n => Node::Rule(Rule::new(name)),
    }
}

fn flatten_map(group: &ast::Group) -> Node {
    println!("flatten_map {:#?}", group);
    // FIXME: len > 1 means we should emit a Choice instead.
    assert!(group.group_choices.len() == 1);
    let grpchoice = &group.group_choices[0];
    let nodes: Vec<KeyValue> = grpchoice
        .group_entries
        .iter()
        .map(|ge_tuple| {
            let group_entry = &ge_tuple.0;
            flatten_groupentry(group_entry)
        })
        .collect();
    Node::Map(Map { members: nodes })
}

fn flatten_groupentry(group_entry: &ast::GroupEntry) -> KeyValue {
    use ast::GroupEntry;
    // FIXME: does this need different behavior for maps vs arrays(record or vector)?
    match group_entry {
        GroupEntry::ValueMemberKey { ge, .. } => flatten_vmke(ge),
        GroupEntry::TypeGroupname { .. } => unimplemented!(),
        GroupEntry::InlineGroup { .. } => unimplemented!(),
    }
}

fn flatten_vmke(vmke: &ast::ValueMemberKeyEntry) -> KeyValue {
    // FIXME: handle occurrence
    let member_key = vmke.member_key.as_ref().unwrap(); // FIXME: may be None for arrays
    let key = flatten_memberkey(&member_key);
    let value = flatten_type(&vmke.entry_type);
    KeyValue::new(key, value)
}

fn flatten_memberkey(memberkey: &ast::MemberKey) -> Node {
    use ast::MemberKey;
    match memberkey {
        MemberKey::Bareword { ident, .. } => {
            // A "bareword" is just a literal string used in the context
            // of a map key.
            let name = ident.ident.clone();
            Node::Literal(Literal::Text(name))
        }
        // FIXME: handle cut
        MemberKey::Type1 { t1, .. } => flatten_type1(t1.as_ref()),
        _ => unimplemented!(),
    }
}

#[test]
fn test_flatten_literal_int() {
    let cddl_input = r#"thing = 1"#;
    let result = flatten_from_str(cddl_input).unwrap();
    let result = format!("{:?}", result);
    assert_eq!(result, r#"{"thing": Literal(Int(1))}"#);
}

#[test]
fn test_flatten_literal_tstr() {
    let cddl_input = r#"thing = "abc""#;
    let result = flatten_from_str(cddl_input).unwrap();
    let result = format!("{:?}", result);
    assert_eq!(result, r#"{"thing": Literal(Text("abc"))}"#);
}

#[test]
fn test_flatten_prelude_reference() {
    let cddl_input = r#"thing = int"#;
    let result = flatten_from_str(cddl_input).unwrap();
    let result = format!("{:?}", result);
    assert_eq!(result, r#"{"thing": PreludeType(Int)}"#);
}

#[test]
fn test_flatten_type_reference() {
    let cddl_input = r#"thing = foo"#;
    let result = flatten_from_str(cddl_input).unwrap();
    let result = format!("{:?}", result);
    assert_eq!(result, r#"{"thing": Rule(Rule { name: "foo" })}"#);
}

#[test]
fn test_flatten_map() {
    // A map containing a bareword key
    let cddl_input = r#"thing = { foo: tstr }"#;
    let result = flatten_from_str(cddl_input).unwrap();
    let result = format!("{:?}", result);
    let expected = concat!(
        r#"{"thing": Map(Map { members: [KeyValue "#,
        r#"{ key: Literal(Text("foo")), value: PreludeType(Tstr) }] })}"#
    );
    assert_eq!(result, expected);

    // A map containing a prelude type key.
    // Note: CDDL syntax requires type keys to use "=>" not ":", otherwise
    // it will assume a bareword key is being used.
    let cddl_input = r#"thing = { tstr => tstr }"#;
    let result = flatten_from_str(cddl_input).unwrap();
    let result = format!("{:?}", result);
    let expected = concat!(
        r#"{"thing": Map(Map { members: [KeyValue "#,
        r#"{ key: PreludeType(Tstr), value: PreludeType(Tstr) }] })}"#
    );
    assert_eq!(result, expected);

    // A map key name alias
    let cddl_input = r#"foo = "bar" thing = { foo => tstr }"#;
    let result = flatten_from_str(cddl_input).unwrap();
    let result = format!("{:?}", result);
    let expected = concat!(
        r#"{"foo": Literal(Text("bar")), "#,
        r#""thing": Map(Map { members: [KeyValue "#,
        r#"{ key: Rule(Rule { name: "foo" }), value: PreludeType(Tstr) }] })}"#
    );
    // FIXME: is Rule the right output?  What if "abc" was a group name?
    assert_eq!(result, expected);
}
