//! Tools for converting the [`ast`] (syntax tree) into an [`ivt`]
//! (intermediate validation tree).
//!
//! This module is called "flatten" because its goal is to flatten syntax
//! tree detail that's not useful for validation.
//!
//! [`ivt`]: crate::ivt
//! [`ast`]: crate::ast

use crate::ast;
use crate::ivt::*;
use crate::parser::{parse_cddl, slice_parse_cddl};
use crate::util::ValidateError;
use std::collections::BTreeMap;
use std::convert::TryInto;

// The type used to return a {name: rule} tree
type RulesByName = BTreeMap<String, Node>;

type RulesWithStrings = BTreeMap<String, (Node, String)>;

/// The result of a flatten operation.
pub type FlattenResult<T> = std::result::Result<T, ValidateError>;

/// Convert a CDDL schema in UTF-8 form into a `(name, rule)` map.
pub fn flatten_from_str(cddl_input: &str) -> FlattenResult<RulesByName> {
    let cddl = parse_cddl(cddl_input).map_err(ValidateError::ParseError)?;
    flatten(&cddl)
}

/// Convert a CDDL schema in UTF-8 form into a `(name, (rule, rule-string))` map.
///
/// This works the same as `flatten_from_str`, but preserves a copy of the original
/// CDDL text alongside the IVT.
pub fn slice_flatten_from_str(cddl_input: &str) -> FlattenResult<RulesWithStrings> {
    let cddl = slice_parse_cddl(cddl_input).map_err(ValidateError::ParseError)?;
    slice_flatten(&cddl)
}

/// Convert an already-parsed cddl AST into a `(name, rules)` map.
pub fn flatten(cddl: &ast::Cddl) -> FlattenResult<RulesByName> {
    // This first pass generates a tree of Nodes from the AST.
    cddl.rules.iter().map(|rule| flatten_rule(rule)).collect()
}

/// Convert an already-parsed cddl AST into a `(name, (rule, rule-string))` map.
///
/// This works the same as `flatten`, but preserves a copy of the original
/// CDDL text alongside the IVT.
pub fn slice_flatten(cddl: &ast::CddlSlice) -> FlattenResult<RulesWithStrings> {
    // This first pass generates a tree of Nodes from the AST.
    cddl.rules
        .iter()
        .map(|(rule, s)| {
            let (name, flat) = flatten_rule(rule)?;
            // key = name, value = (Node, copy of cddl text slice)
            Ok((name, (flat, s.clone())))
        })
        .collect()
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
    let options: FlattenResult<Vec<Node>> = ty.0.iter().map(|type1| flatten_type1(type1)).collect();
    let options = options?;

    match options.len() {
        0 => Err(ValidateError::Unsupported(
            "flatten type with 0 options".into(),
        )),
        1 => Ok(options.into_iter().next().unwrap()),
        _ => Ok(Node::Choice(Choice { options })),
    }
}

fn flatten_type1(ty1: &ast::Type1) -> FlattenResult<Node> {
    use ast::Type1::*;
    match ty1 {
        Simple(ty2) => flatten_type2(ty2),
        Range(r) => flatten_range(r),
        Control(_) => Err(ValidateError::Unsupported("control operator".into())),
    }
}

// The only way a range start or end can be specified is with a literal
// value, or with a typename.  We will accept either of those, and throw
// an error otherwise.  Let the validator worry about whether a typename
// resolves to a literal.
fn range_point(point: &ast::Type2) -> FlattenResult<Node> {
    let node = match point {
        ast::Type2::Value(v) => flatten_value(v),
        ast::Type2::Typename(t) => flatten_typename(t),
        _ => Err(ValidateError::Structural(
            "bad type on range operator".into(),
        )),
    }?;
    // Check that the node that came back is a Rule or Literal.
    // Anything else (i.e. PreludeType) should cause an error.
    match node {
        Node::Rule(_) | Node::Literal(_) => Ok(node),
        _ => Err(ValidateError::Structural(
            "bad type on range operator".into(),
        )),
    }
}

fn flatten_range(range: &ast::TypeRange) -> FlattenResult<Node> {
    let start = range_point(&range.start)?;
    let end = range_point(&range.end)?;

    Ok(Node::Range(Range {
        start: start.into(),
        end: end.into(),
        inclusive: range.inclusive,
    }))
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
        Type2::Unwrap(r) => Ok(Node::Unwrap(Rule::new(r))),
    }
}

fn flatten_typename(name: &str) -> FlattenResult<Node> {
    let unsupported = |s: &str| -> FlattenResult<Node> {
        let msg = format!("prelude type '{}'", s);
        Err(ValidateError::Unsupported(msg))
    };

    let result = match name {
        "any" => Node::PreludeType(PreludeType::Any),
        "nil" | "null" => Node::PreludeType(PreludeType::Nil),
        "bool" => Node::PreludeType(PreludeType::Bool),
        "false" => literal_bool(false),
        "true" => literal_bool(true),
        "int" => Node::PreludeType(PreludeType::Int),
        "uint" => Node::PreludeType(PreludeType::Uint),
        "nint" => Node::PreludeType(PreludeType::Nint),
        "float" => Node::PreludeType(PreludeType::Float),
        "tstr" | "text" => Node::PreludeType(PreludeType::Tstr),
        "bstr" | "bytes" => Node::PreludeType(PreludeType::Bstr),

        // FIXME: need to store the "additional information" bits that will
        // preserve the expected floating-point size.  For now, just pretend
        // that all floats are the same.
        "float16" | "float32" | "float64" | "float16-32" | "float32-64" => {
            Node::PreludeType(PreludeType::Float)
        }

        // The remaining prelude types are specified using CBOR (major, ai)
        // pairs.  These types can only be used with CBOR (not JSON).

        // FIXME: preserve more information about these types so that a JSON
        // validator knows to reject them, and a CBOR validator has some chance
        // at further validation.

        // CBOR types that are stored as "tstr":
        // tdate = #6.0(tstr)
        // uri = #6.32(tstr)
        // b64url = #6.33(tstr)
        // b64legacy = #6.34(tstr)
        // regexp = #6.35(tstr)
        // mime-message = #6.36(tstr)
        "tdate" | "uri" | "b64url" | "b64legacy" | "regexp" | "mime-message" => {
            Node::PreludeType(PreludeType::Tstr)
        }

        // CBOR types that are stored as "bstr":
        // biguint = #6.2(bstr)
        // bignint = #6.3(bstr)
        // encoded-cbor = #6.24(bstr)
        "biguint" | "bignint" | "encoded-cbor" => Node::PreludeType(PreludeType::Bstr),

        // CBOR types that are stored as "any":
        // eb64url = #6.21(any)
        // eb64legacy = #6.22(any)
        // eb16 = #6.23(any)
        // cbor-any = #6.55799(any)
        "eb64url" | "eb64legacy" | "eb16" | "cbor-any" => Node::PreludeType(PreludeType::Any),

        // CBOR types that are stored as "number":
        // time = #6.1(number)
        "time" => return unsupported(name),

        // CBOR types that are choices of other types:
        // bigint = biguint / bignint
        // integer = int / bigint
        // unsigned = uint / biguint
        // number = int / float
        "bigint" | "integer" | "unsigned" | "number" => return unsupported(name),

        // Other miscellaneous prelude types:
        // decfrac = #6.4([e10: int, m: integer])
        // bigfloat = #6.5([e2: int, m: integer])
        // undefined = #7.23
        "decfrac" | "bigfloat" | "undefined" => return unsupported(name),

        // We failed to find this string in the standard prelude, so we will
        // assume it's a rule or group identifier.  No further validation is
        // done at this time.
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
        .map(|group_entry| flatten_groupentry(group_entry))
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
            let cut = key.cut;
            let key = flatten_memberkey(&key)?;
            let value = flatten_type(&member.value)?;
            Ok(Node::KeyValue(KeyValue::new(key, value, cut)))
        }
        None => flatten_type(&member.value),
    }
}

// try_into(), remapping the error.
fn num_to_i128<T>(n: T) -> FlattenResult<i128>
where
    T: TryInto<i128> + std::fmt::Display + Copy,
{
    // This error doesn't seem possible, since isize and usize should always
    // fit into an i128.
    n.try_into().map_err(|_| {
        let msg = format!("integer conversion failed: {}", n);
        ValidateError::Structural(msg)
    })
}

fn flatten_memberkey(memberkey: &ast::MemberKey) -> FlattenResult<Node> {
    use ast::MemberKeyVal;

    match &memberkey.val {
        MemberKeyVal::Bareword(s) => {
            // A "bareword" is a literal string that appears without quote
            // marks.  Treat it just like we would a literal with quotes.
            Ok(literal_text(s.clone()))
        }
        MemberKeyVal::Type1(t1) => flatten_type1(&t1),
        MemberKeyVal::Value(v) => flatten_value(&v),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        assert_eq!(result, expected);
    }
}
