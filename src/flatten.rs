//! Tools for converting a [`cddl::ast`] (syntax tree) into an [`ivt`].
//!
//! This module is called "flatten" because its goal is to flatten syntax
//! tree detail that's not useful for validation.
//!
//! [`ivt`]: crate::ivt

use crate::ivt::*;
use crate::util::{make_oops, ValidateError};
use cddl::ast::{self, CDDL};
use cddl::parser::cddl_from_str;
use hex;
use std::collections::BTreeMap;
use std::convert::TryInto;

// The type used to return a {name: rule} tree
type RulesByName = BTreeMap<String, Node>;

/// The result of a flatten operation.
pub type FlattenResult<T> = std::result::Result<T, ValidateError>;

/// Convert a CDDL schema in UTF-8 form into a (name, rules) map.
pub fn flatten_from_str(cddl_input: &str) -> FlattenResult<RulesByName> {
    let cddl = cddl_from_str(cddl_input).map_err(|e| {
        // FIXME: don't throw away the original error
        let msg = format!("cddl parse error {}", e);
        ValidateError::Oops(msg)
    })?;
    flatten(&cddl)
}

/// Convert an already-parsed cddl AST into a (name, rules) map.
pub fn flatten(ast: &CDDL) -> FlattenResult<RulesByName> {
    // This first pass generates a tree of Nodes from the AST.
    ast.rules.iter().map(|rule| flatten_rule(rule)).collect()
}

/// flatten an ast::Rule to an ivt::Node
///
/// Returns (name, node) where the name is the name of the rule (which may
/// be referenced in other places.
fn flatten_rule(rule: &ast::Rule) -> FlattenResult<(String, Node)> {
    let (name, node) = match rule {
        ast::Rule::Type { rule, .. } => flatten_typerule(rule)?,
        ast::Rule::Group { rule, .. } => flatten_grouprule(rule)?,
    };
    Ok((name, node))
}

// returns (name, node) just like flatten_rule()
fn flatten_typerule(typerule: &ast::TypeRule) -> FlattenResult<(String, Node)> {
    // FIXME: handle generic_param
    // FIXME: handle is_type_choice_alternate
    let node = flatten_type(&typerule.value)?;
    Ok((typerule.name.ident.clone(), node))
}

// returns (name, node) just like flatten_rule()
// FIXME: should groups and rules be kept separate?
fn flatten_grouprule(grouprule: &ast::GroupRule) -> FlattenResult<(String, Node)> {
    // FIXME: handle generic_param
    // FIXME: handle is_type_choice_alternate
    let node = flatten_groupentry(&grouprule.entry)?;
    Ok((grouprule.name.ident.clone(), node))
}

fn flatten_type(ty: &ast::Type) -> FlattenResult<Node> {
    let options: FlattenResult<Vec<Node>> = ty
        .type_choices
        .iter()
        .map(|type1| flatten_type1(type1))
        .collect();
    let options = options?;

    match options.len() {
        0 => make_oops("flatten type with 0 options"),
        1 => Ok(options.into_iter().next().unwrap()),
        _ => Ok(Node::Choice(Choice { options })),
    }
}

fn flatten_type1(ty1: &ast::Type1) -> FlattenResult<Node> {
    // FIXME: handle range & control operators.
    flatten_type2(&ty1.type2)
}

fn flatten_type2(ty2: &ast::Type2) -> FlattenResult<Node> {
    use ast::Type2;
    match ty2 {
        Type2::UintValue { value, .. } => {
            let value = num_to_i128(*value)?;
            Ok(literal_int(value))
        }
        Type2::TextValue { value, .. } => Ok(literal_text(value)),
        Type2::FloatValue { value, .. } => Ok(literal_float(*value)),
        Type2::Typename { ident, .. } => flatten_typename(&ident.ident),
        Type2::Map { group, .. } => flatten_map(&group),
        Type2::Array { group, .. } => flatten_array(&group),
        Type2::B16ByteString { value, .. } => {
            // Maybe hex decode belongs in the CDDL parser instead?
            Ok(literal_bytes(bytes_from_hex(value)?))
        }
        Type2::ParenthesizedType { pt, .. } => flatten_type(pt),
        _ => make_oops("unimplemented Type2 variant"),
    }
}

fn flatten_typename(name: &str) -> FlattenResult<Node> {
    let result = match name {
        "any" => Node::PreludeType(PreludeType::Any),
        "bool" => Node::PreludeType(PreludeType::Bool),
        "false" => literal_bool(false),
        "true" => literal_bool(true),
        "int" => Node::PreludeType(PreludeType::Int),
        "uint" => Node::PreludeType(PreludeType::Uint),
        "float" => Node::PreludeType(PreludeType::Float),
        "tstr" => Node::PreludeType(PreludeType::Tstr),
        "bstr" => Node::PreludeType(PreludeType::Bstr),
        // FIXME: lots more prelude types to handle...
        // FIXME: this could be a group name, maybe other things?
        _ => Node::Rule(Rule::new(name)),
    };
    Ok(result)
}

/// Flatten a group into a Map.
fn flatten_map(group: &ast::Group) -> FlattenResult<Node> {
    let kvs = flatten_group(group)?;
    Ok(Node::Map(Map { members: kvs }))
}

/// Flatten a group into a Map.
fn flatten_array(group: &ast::Group) -> FlattenResult<Node> {
    let kvs = flatten_group(group)?;
    Ok(Node::Array(Array { members: kvs }))
}

// Returns an ivt::Group node, or a vector of other nodes.
fn flatten_group(group: &ast::Group) -> FlattenResult<Vec<Node>> {
    if group.group_choices.len() == 1 {
        let groupchoice = &group.group_choices[0];
        flatten_groupchoice(groupchoice)
    } else {
        // Emit a Choice node, containing a vector of Group nodes.
        let options: FlattenResult<Vec<Node>> = group
            .group_choices
            .iter()
            .map(|gc| {
                let inner_members = flatten_groupchoice(gc)?;
                Ok(Node::Group(Group {
                    members: inner_members,
                }))
            })
            .collect();
        let options = options?;
        Ok(vec![Node::Choice(Choice { options })])
    }
}

fn flatten_groupchoice(groupchoice: &ast::GroupChoice) -> FlattenResult<Vec<Node>> {
    let kvs: FlattenResult<Vec<Node>> = groupchoice
        .group_entries
        .iter()
        .map(|ge_tuple| {
            let group_entry = &ge_tuple.0;
            flatten_groupentry(group_entry)
        })
        .collect();
    kvs
}

fn flatten_groupentry(group_entry: &ast::GroupEntry) -> FlattenResult<Node> {
    use ast::GroupEntry;

    match group_entry {
        GroupEntry::ValueMemberKey { ge, .. } => flatten_vmke(ge),
        GroupEntry::TypeGroupname { ge, .. } => flatten_tge(ge),
        GroupEntry::InlineGroup { group, occur, .. } => {
            // FIXME: if nodes has len(1), just return a single node.
            let nodes = flatten_group(group)?;
            let node = Node::Group(Group { members: nodes });
            Ok(occur_wrap(&occur, node))
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

fn flatten_tge(tge: &ast::TypeGroupnameEntry) -> FlattenResult<Node> {
    // The incoming TypeGroupnameEntry can mean multiple things.  It carries
    // an Identifier, which could refer to:
    // - a prelude type
    // - a user-defined type rule
    // - a user-defined group

    // FIXME: handle generic_arg

    let node = flatten_typename(&tge.name.ident)?;
    Ok(occur_wrap(&tge.occur, node))
}

fn flatten_vmke(vmke: &ast::ValueMemberKeyEntry) -> FlattenResult<Node> {
    let member_key = vmke.member_key.as_ref().unwrap(); // FIXME: may be None for arrays
    let key = flatten_memberkey(&member_key)?;
    let value = flatten_type(&vmke.entry_type)?;
    let node = Node::KeyValue(KeyValue::new(key, value));
    Ok(occur_wrap(&vmke.occur, node))
}

// hex::decode, remapping the error.
// FIXME: maybe implementing From<FromHexError> for ValidateError would be better.
fn bytes_from_hex<T: AsRef<[u8]>>(data: T) -> FlattenResult<Vec<u8>> {
    hex::decode(data).map_err(|_| ValidateError::Oops("bad hex literal".to_string()))
}

// try_into(), remapping the error.
fn num_to_i128<T: TryInto<i128>>(n: T) -> FlattenResult<i128> {
    // This error doesn't seem possible, since isize and usize should always
    // fit into an i128.
    n.try_into()
        .map_err(|_| ValidateError::Oops("try_into<i128> failed".to_string()))
}

// FIXME: it seems weird to me that cddl::ast::MemberKey::Value includes a
// cddl::token::Value, when literals in other places are presented using
// cddl::ast::Type2 .  Is that a deliberate choice, or a parser bug?
fn translate_token_value(v: &cddl::token::Value) -> FlattenResult<Node> {
    use cddl::token::Value;
    let result = match v {
        Value::INT(i) => {
            let i = num_to_i128(*i)?;
            literal_int(i)
        }
        Value::UINT(u) => {
            let u = num_to_i128(*u)?;
            literal_int(u)
        }
        Value::FLOAT(f) => literal_float(*f),
        Value::TEXT(s) => literal_text(s),
        Value::BYTE(b) => {
            use cddl::token::ByteValue;
            match b {
                ByteValue::B16(encoded) => literal_bytes(bytes_from_hex(encoded)?),
                // FIXME: UTF8 and Base64 variants
                _ => return make_oops("unhandled token::ByteValue"),
            }
        }
    };
    Ok(result)
}

fn flatten_memberkey(memberkey: &ast::MemberKey) -> FlattenResult<Node> {
    use ast::MemberKey;
    match memberkey {
        MemberKey::Bareword { ident, .. } => {
            // A "bareword" is just a literal string used in the context
            // of a map key.
            let name = ident.ident.clone();
            Ok(literal_text(name))
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
