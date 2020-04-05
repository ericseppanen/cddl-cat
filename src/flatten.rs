//! Tools for converting the [`ast`] (syntax tree) into an [`ivt`]
//! (intermediate validation tree).
//!
//! This module is called "flatten" because its goal is to flatten syntax
//! tree detail that's not useful for validation.
//!
//! [`ivt`]: crate::ivt
//! [`ast`]: crate::ast

use crate::ivt::*;
use crate::util::{make_oops, ValidateError};
use crate::ast::{self, parse_cddl};
use std::collections::BTreeMap;
use std::convert::TryInto;

// The type used to return a {name: rule} tree
type RulesByName = BTreeMap<String, Node>;

/// The result of a flatten operation.
pub type FlattenResult<T> = std::result::Result<T, ValidateError>;

/// Convert a CDDL schema in UTF-8 form into a (name, rules) map.
pub fn flatten_from_str(cddl_input: &str) -> FlattenResult<RulesByName> {
    let cddl = parse_cddl(cddl_input).map_err(|e| {
        let msg = format!("cddl parse error: {}", e);
        ValidateError::Oops(msg)
    })?;
    flatten(&cddl)
}

/// Convert an already-parsed cddl AST into a (name, rules) map.
pub fn flatten(cddl: &ast::Cddl) -> FlattenResult<RulesByName> {
    // This first pass generates a tree of Nodes from the AST.
    cddl.rules.iter().map(|rule| flatten_rule(rule)).collect()
}

/// flatten an ast::Rule to an ivt::Node
///
/// Returns (name, node) where the name is the name of the rule (which may
/// be referenced in other places.
fn flatten_rule(rule: &ast::Rule) -> FlattenResult<(String, Node)> {
    use ast::RuleVal;
    let node = match &rule.val {
        RuleVal::AssignType(t) => flatten_type(&t)?,
        RuleVal::AssignGroup(g) => flatten_groupentry(&g)?,
    };
    Ok((rule.name.clone(), node))
}

fn flatten_type(ty: &ast::Type) -> FlattenResult<Node> {
    let options: FlattenResult<Vec<Node>> = ty.0
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
    flatten_type2(ty1)
}

fn flatten_value(value: &ast::Value) -> FlattenResult<Node> {
    use ast::Value;
    match value {
        Value::Text(s) => Ok(literal_text(s)),
        Value::Uint(u) => {
            let value = num_to_i128(*u)?;
            Ok(literal_int(value))
        }
        Value::Nint(n) => {
            let value = num_to_i128(*n)?;
            Ok(literal_int(value))
        }
        Value::Float(f) => Ok(literal_float(*f)),
        Value::Bytes(b) => Ok(literal_bytes(b.clone())),
    }
}

fn flatten_type2(ty2: &ast::Type2) -> FlattenResult<Node> {
    use ast::Type2;
    match ty2 {
        Type2::Value(v) => flatten_value(v),
        Type2::Typename(s) => flatten_typename(&s),
        Type2::Parethesized(t) => flatten_type(t),
        Type2::Map(g) => flatten_map(&g),
        Type2::Array(g) => flatten_array(&g),
        //_ => make_oops("unimplemented Type2 variant"),
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
    println!("XXX flatten_array {:#?}", group);
    let kvs = flatten_group(group)?;
    Ok(Node::Array(Array { members: kvs }))
}

// Returns an ivt::Group node, or a vector of other nodes.
fn flatten_group(group: &ast::Group) -> FlattenResult<Vec<Node>> {
    let group_choices = &group.0;
    if group_choices.len() == 1 {
        let groupchoice = &group_choices[0];
        flatten_groupchoice(groupchoice)
    } else {
        // Emit a Choice node, containing a vector of Group nodes.
        let options: FlattenResult<Vec<Node>> = group_choices
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

fn flatten_groupchoice(groupchoice: &ast::GrpChoice) -> FlattenResult<Vec<Node>> {
    let group_entries = &groupchoice.0;
    let kvs: FlattenResult<Vec<Node>> = group_entries
        .iter()
        .map(|group_entry| {
            flatten_groupentry(group_entry)
        })
        .collect();
    kvs
}

fn flatten_groupentry(group_entry: &ast::GrpEnt) -> FlattenResult<Node> {
    let node = flatten_groupentry_val(&group_entry.val)?;
    Ok(occur_wrap(&group_entry.occur, node))
}

fn flatten_groupentry_val(gev: &ast::GrpEntVal) -> FlattenResult<Node> {
    use ast::GrpEntVal;

    match gev {
        GrpEntVal::Member(m) => flatten_member(m),
        GrpEntVal::Groupname(s) => flatten_typename(s),
        GrpEntVal::Parenthesized(g) => {
            let nodes = flatten_group(g)?;
            Ok(Node::Group(Group { members: nodes }))
        }
    }
}

/* Don't delete this yet; I would like to remove usize::MAX from the parser.
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
*/

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
        Some(o) => Node::Occur(Occur::new(o.clone(), node)),
        None => node,
    }
}

fn flatten_member(member: &ast::Member) -> FlattenResult<Node> {
    match &member.key {
        Some(key) => {
            let key = flatten_memberkey(&key)?;
            let value = flatten_type(&member.value)?;
            Ok(Node::KeyValue(KeyValue::new(key, value)))
        }
        None => flatten_type(&member.value)
    }
}

// try_into(), remapping the error.
fn num_to_i128<T: TryInto<i128>>(n: T) -> FlattenResult<i128> {
    // This error doesn't seem possible, since isize and usize should always
    // fit into an i128.
    n.try_into()
        .map_err(|_| ValidateError::Oops("try_into<i128> failed".to_string()))
}

fn flatten_memberkey(memberkey: &ast::MemberKey) -> FlattenResult<Node> {
    use ast::MemberKeyVal;

    // FIXME: handle cut

    let memberkey_val = &memberkey.val;
    match memberkey_val {
        MemberKeyVal::Bareword(s) => {
            // A "bareword" is a literal string that appears without quote
            // marks.  Treat it just like we would a literal with quotes.
            Ok(literal_text(s.clone()))
        }
        MemberKeyVal::Type1(t1) => flatten_type1(&t1),
        MemberKeyVal::Value(v) => flatten_value(&v),
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
