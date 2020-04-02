//! Tools for converting a [`cddl::ast`] (syntax tree) into an [`ivt`].
//!
//! This module is called "flatten" because its goal is to flatten syntax
//! tree detail that's not useful for validation.
//!
//! [`ivt`]: crate::ivt

use crate::ivt::*;
use crate::util::ValidateError;
use cddl::ast::{self, CDDL};
use cddl::parser::cddl_from_str;
use hex;

pub type FlattenResult<T> = std::result::Result<T, ValidateError>;

pub type MutateResult = std::result::Result<(), ValidateError>;

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
    Ok(rules)
}

/// flatten an ast::Rule to an ivt::Node
///
/// Returns (name, node) where the name is the name of the rule (which may
/// be referenced in other places.
fn flatten_rule(rule: &ast::Rule) -> (String, Node) {
    let (name, node) = match rule {
        ast::Rule::Type { rule, .. } => flatten_typerule(rule),
        ast::Rule::Group { rule, .. } => flatten_grouprule(rule),
    };
    (name, node)
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
    let mut options: Vec<Node> = ty
        .type_choices
        .iter()
        .map(|type1| flatten_type1(type1))
        .collect();

    match options.len() {
        0 => panic!("flatten type with 0 options"),
        1 => options.drain(..).next().unwrap(), // FIXME: awkward
        _ => Node::Choice(Choice { options }),
    }
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
        Type2::B16ByteString { value, .. } => {
            // FIXME: need to return a Result from this function;
            // panicking is not a good strategy.
            // Maybe this belongs in the CDDL parser instead?
            let bytes = hex::decode(value).unwrap_or_else(|e| panic!("bad hex literal: {}", e));
            Node::Literal(Literal::Bytes(bytes))
        }
        Type2::ParenthesizedType { pt, .. } => flatten_type(pt),
        _ => panic!("flatten_type2 unimplemented {:#?}", ty2),
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
        "bstr" => Node::PreludeType(PreludeType::Bstr),
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

// Returns an ivt::Group node, or a vector of other nodes.
fn flatten_group(group: &ast::Group) -> VecNode {
    if group.group_choices.len() == 1 {
        let groupchoice = &group.group_choices[0];
        flatten_groupchoice(groupchoice)
    } else {
        // Emit a Choice node, containing a vector of Group nodes.
        let options: VecNode = group
            .group_choices
            .iter()
            .map(|gc| {
                let inner_members = flatten_groupchoice(gc);
                Node::Group(Group {
                    members: inner_members,
                })
            })
            .collect();
        vec![Node::Choice(Choice { options })]
    }
}

fn flatten_groupchoice(groupchoice: &ast::GroupChoice) -> VecNode {
    let kvs: VecNode = groupchoice
        .group_entries
        .iter()
        .map(|ge_tuple| {
            let group_entry = &ge_tuple.0;
            flatten_groupentry(group_entry)
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

// FIXME: it seems weird to me that cddl::ast::MemberKey::Value includes a
// cddl::token::Value, when literals in other places are presented using
// cddl::ast::Type2 .  Is that a deliberate choice, or a parser bug?
fn translate_token_value(v: &cddl::token::Value) -> Node {
    use cddl::token::Value;
    match v {
        Value::INT(i) => Node::Literal(Literal::Int(*i as i128)),
        Value::UINT(u) => Node::Literal(Literal::Int(*u as i128)),
        Value::FLOAT(f) => Node::Literal(Literal::Float(*f)),
        Value::TEXT(s) => Node::Literal(Literal::Text(s.clone())),
        Value::BYTE(b) => {
            use cddl::token::ByteValue;
            match b {
                ByteValue::B16(encoded) => {
                    // FIXME: return an error on bad hex decode.
                    let bytes =
                        hex::decode(encoded).unwrap_or_else(|e| panic!("bad hex literal: {}", e));
                    Node::Literal(Literal::Bytes(bytes))
                }
                // FIXME: UTF8 and Base64 variants
                _ => panic!("unhandled token::ByteValue"),
            }
        }
    }
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
        MemberKey::Value { value, .. } => translate_token_value(value),
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
    assert_eq!(result, r#"{"thing": Rule(Rule { name: "foo" })}"#);
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
        r#"KeyValue(KeyValue(Rule(Rule { name: "foo" }), PreludeType(Tstr)))] })}"#
    );
    // FIXME: is Rule the right output?  What if "abc" was a group name?
    assert_eq!(result, expected);
}
