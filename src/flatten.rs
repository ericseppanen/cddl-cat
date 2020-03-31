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
    // This first pass generates a tree of Nodes from the AST.
    let rules: RulesByName = ast.rules.iter().map(|rule| flatten_rule(rule)).collect();
    // This second pass adds Weak references for by-name rule references.
    replace_rule_refs(&rules)?;
    Ok(rules)
}

// Descend recursively into a tree of Nodes, running a function against each.
fn mutate_node_tree<F>(node: &Node, func: &mut F) -> MutateResult
where
    F: FnMut(&Node) -> MutateResult,
{
    // Apply the function first, then recurse.
    func(node)?;
    match node {
        Node::Literal(_) => (),     // leaf node
        Node::PreludeType(_) => (), // leaf node
        Node::Rule(_) => (),        // leaf node
        Node::Group(g) => {
            for member in &g.members {
                mutate_node_tree(member.as_ref(), func)?;
            }
        }
        Node::Choice(c) => {
            for option in &c.options {
                mutate_node_tree(option.as_ref(), func)?;
            }
        }
        Node::Map(m) => {
            for member in &m.members {
                mutate_node_tree(member.as_ref(), func)?;
            }
        }
        Node::KeyValue(kv) => {
            mutate_node_tree(kv.key.as_ref(), func)?;
            mutate_node_tree(kv.value.as_ref(), func)?;
        }
        Node::Array(a) => {
            for member in &a.members {
                mutate_node_tree(member.as_ref(), func)?;
            }
        }
        Node::Occur(o) => {
            mutate_node_tree(o.node.as_ref(), func)?;
        }
    }
    Ok(())
}

fn replace_rule_refs(rules: &RulesByName) -> MutateResult {
    for root in rules.values() {
        mutate_node_tree(root, &mut |node| {
            match node {
                Node::Rule(rule_ref) => {
                    // FIXME: add graceful handling of nonexistent rule name
                    let real_ref = rules.get(&rule_ref.name);
                    if real_ref.is_none() {
                        panic!("tried to access nonexistent rule '{}'", &rule_ref.name);
                    }
                    let real_ref = real_ref.unwrap();
                    rule_ref.upgrade(real_ref);
                }
                _ => (),
            }
            Ok(())
        })?;
    }
    Ok(())
}

/// flatten an ast::Rule to an ivt::Node
///
/// Returns (name, node) where the name is the name of the rule (which may
/// be referenced in other places.
fn flatten_rule(rule: &ast::Rule) -> (String, ArcNode) {
    let (name, node) = match rule {
        ast::Rule::Type { rule, .. } => flatten_typerule(rule),
        ast::Rule::Group { rule, .. } => flatten_grouprule(rule),
    };
    (name, Arc::new(node))
}

// returns (name, node) just like flatten_rule()
fn flatten_typerule(typerule: &ast::TypeRule) -> (String, Node) {
    // FIXME: handle generic_param
    // FIXME: handle is_type_choice_alternate
    let node = flatten_type(&typerule.value);
    (typerule.name.ident.clone(), node)
}

// returns (name, node) just like flatten_rule()
// FIXME: should groups and rules be kept separate?
fn flatten_grouprule(grouprule: &ast::GroupRule) -> (String, Node) {
    // FIXME: handle generic_param
    // FIXME: handle is_type_choice_alternate
    let node = flatten_groupentry(&grouprule.entry);
    (grouprule.name.ident.clone(), node)
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
        Type2::FloatValue { value, .. } => Node::Literal(Literal::Float(*value)),
        Type2::Typename { ident, .. } => flatten_typename(&ident.ident),
        Type2::Map { group, .. } => flatten_map(&group),
        Type2::Array { group, .. } => flatten_array(&group),
        _ => unimplemented!(),
    }
}

fn flatten_typename(name: &str) -> Node {
    match name {
        "any" => Node::PreludeType(PreludeType::Any),
        "bool" => Node::PreludeType(PreludeType::Bool),
        "false" => Node::Literal(Literal::Bool(false)),
        "true" => Node::Literal(Literal::Bool(true)),
        "int" => Node::PreludeType(PreludeType::Int),
        "uint" => Node::PreludeType(PreludeType::Uint),
        "float" => Node::PreludeType(PreludeType::Float),
        "tstr" => Node::PreludeType(PreludeType::Tstr),
        // FIXME: lots more prelude types to handle...
        // FIXME: this could be a group name, maybe other things?
        _ => Node::Rule(Rule::new(name)),
    }
}

/// Flatten a group into a Map.
fn flatten_map(group: &ast::Group) -> Node {
    let kvs = flatten_group(group);
    Node::Map(Map { members: kvs })
}

/// Flatten a group into a Map.
fn flatten_array(group: &ast::Group) -> Node {
    let kvs = flatten_group(group);
    Node::Array(Array { members: kvs })
}

// FIXME: special handling for GroupRule vs Map vs Array?
fn flatten_group(group: &ast::Group) -> VecNode {
    // FIXME: len > 1 means we should emit a Choice instead.
    assert!(group.group_choices.len() == 1);
    let grpchoice = &group.group_choices[0];
    let kvs: VecNode = grpchoice
        .group_entries
        .iter()
        .map(|ge_tuple| {
            let group_entry = &ge_tuple.0;
            Arc::new(flatten_groupentry(group_entry))
        })
        .collect();
    kvs
}

fn flatten_groupentry(group_entry: &ast::GroupEntry) -> Node {
    use ast::GroupEntry;

    match group_entry {
        GroupEntry::ValueMemberKey { ge, .. } => flatten_vmke(ge),
        GroupEntry::TypeGroupname { ge, .. } => flatten_tge(ge),
        GroupEntry::InlineGroup { group, occur, .. } => {
            // FIXME: if nodes has len(1), just return a single node.
            let nodes = flatten_group(group);
            let node = Node::Group(Group { members: nodes });
            occur_wrap(&occur, node)
        }
    }
}

/// Convert ast::Occur to ivt::OccurLimit
impl From<&ast::Occur> for OccurLimit {
    fn from(occur: &ast::Occur) -> OccurLimit {
        match occur {
            ast::Occur::Optional(_) => OccurLimit::Optional,
            ast::Occur::ZeroOrMore(_) => OccurLimit::ZeroOrMore,
            ast::Occur::OneOrMore(_) => OccurLimit::OneOrMore,
            ast::Occur::Exact { lower, upper, .. } => {
                let lower: usize = match lower {
                    Some(n) => *n,
                    None => 0,
                };
                let upper: usize = match upper {
                    Some(n) => *n,
                    None => usize::MAX,
                };
                OccurLimit::Numbered(lower, upper)
            }
        }
    }
}

/// If the ast::Occur is Some, wrap the given Node in an ivt::Occur.
///
/// This is an adapter between the way the `ast` does occurences (extra
/// metadata attached to certain data structures) and the way `ivt` does them
/// (wrapping the Node in another Node).
///
/// If the occur argument is None, return the original node.
///
fn occur_wrap(occur: &Option<ast::Occur>, node: Node) -> Node {
    match &occur {
        Some(o) => Node::Occur(Occur::new(o.into(), node)),
        None => node,
    }
}

fn flatten_tge(tge: &ast::TypeGroupnameEntry) -> Node {
    // The incoming TypeGroupnameEntry can mean multiple things.  It carries
    // an Identifier, which could refer to:
    // - a prelude type
    // - a user-defined type rule
    // - a user-defined group

    // FIXME: handle generic_arg

    let node = flatten_typename(&tge.name.ident);
    occur_wrap(&tge.occur, node)
}

fn flatten_vmke(vmke: &ast::ValueMemberKeyEntry) -> Node {
    let member_key = vmke.member_key.as_ref().unwrap(); // FIXME: may be None for arrays
    let key = flatten_memberkey(&member_key);
    let value = flatten_type(&vmke.entry_type);
    let node = Node::KeyValue(KeyValue::new(key, value));
    occur_wrap(&vmke.occur, node)
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
#[ignore] // FIXME: choking on dangling type reference
fn test_flatten_type_reference() {
    let cddl_input = r#"thing = foo"#;
    let result = flatten_from_str(cddl_input).unwrap();
    let result = format!("{:?}", result);
    assert_eq!(result, r#"{"thing": Rule(Rule { name: "foo!" })}"#);
}

#[test]
fn test_flatten_map() {
    // A map containing a bareword key
    let cddl_input = r#"thing = { foo: tstr }"#;
    let result = flatten_from_str(cddl_input).unwrap();
    let result = format!("{:?}", result);
    let expected = concat!(
        r#"{"thing": Map(Map { members: [KeyValue(KeyValue(Literal(Text("foo")), PreludeType(Tstr)))] })}"#
    );
    assert_eq!(result, expected);

    // A map containing a prelude type key.
    // Note: CDDL syntax requires type keys to use "=>" not ":", otherwise
    // it will assume a bareword key is being used.
    let cddl_input = r#"thing = { tstr => tstr }"#;
    let result = flatten_from_str(cddl_input).unwrap();
    let result = format!("{:?}", result);
    let expected = concat!(
        r#"{"thing": Map(Map { members: ["#,
        r#"KeyValue(KeyValue(PreludeType(Tstr), PreludeType(Tstr)))] })}"#
    );
    assert_eq!(result, expected);

    // A map key name alias
    let cddl_input = r#"foo = "bar" thing = { foo => tstr }"#;
    let result = flatten_from_str(cddl_input).unwrap();
    let result = format!("{:?}", result);
    let expected = concat!(
        r#"{"foo": Literal(Text("bar")), "thing": Map(Map { members: ["#,
        r#"KeyValue(KeyValue(Rule(Rule { name: "foo!" }), PreludeType(Tstr)))] })}"#
    );
    // FIXME: is Rule the right output?  What if "abc" was a group name?
    assert_eq!(result, expected);
}
