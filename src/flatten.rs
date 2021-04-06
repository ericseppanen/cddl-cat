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
use std::convert::TryInto;

/// The result of a flatten operation.
pub type FlattenResult<T> = std::result::Result<T, ValidateError>;

/// Convert a CDDL schema in UTF-8 form into a structured rule set.
pub fn flatten_from_str(cddl_input: &str) -> FlattenResult<RulesByName> {
    let cddl = parse_cddl(cddl_input).map_err(ValidateError::ParseError)?;
    flatten(&cddl)
}

/// Convert a CDDL schema in UTF-8 form into a structured rule set, preserving the CDDL text.
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
fn flatten_rule(rule: &ast::Rule) -> FlattenResult<(String, RuleDef)> {
    use ast::RuleVal;
    let node = match &rule.val {
        RuleVal::AssignType(t) => flatten_type(&t)?,
        RuleVal::AssignGroup(g) => flatten_groupentry(&g)?,
    };
    let ruledef = RuleDef {
        generic_parms: rule.generic_parms.clone(),
        node,
    };
    Ok((rule.name.clone(), ruledef))
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
        Control(ctl) => flatten_control(ctl),
    }
}

// We only support the following control operators:
//
// <target> .size <integer literal>
//
//     The only allowed targets are bstr, tstr, and unsigned integers.
//
// TODO:
// .bits
// .regexp
// .cbor .cborseq
// .within .and
// .lt .le .gt .ge .eq .ne. default
// According to RFC 8610 3.8, new control operators may arrive later.
//
fn flatten_control(ctl: &ast::TypeControl) -> FlattenResult<Node> {
    let ctl_result = match &ctl.op[..] {
        "size" => {
            let target = flatten_type2(&ctl.target)?;
            let size = flatten_type2(&ctl.arg)?;

            // The only allowed limit types are:
            // A positive literal integer
            // A named rule (which should resolve to a literal integer)
            match size {
                Node::Literal(Literal::Int(_)) => {}
                Node::Rule(_) => {}
                _ => return Err(ValidateError::Unsupported(".size limit type".into())),
            };

            Control::Size(CtlOpSize {
                target: Box::new(target),
                size: Box::new(size),
            })
        }
        _ => return Err(ValidateError::Unsupported("control operator".into())),
    };

    Ok(Node::Control(ctl_result))
}

// The only way a range start or end can be specified is with a literal
// value, or with a typename.  We will accept either of those, and throw
// an error otherwise.  Let the validator worry about whether a typename
// resolves to a literal.
fn range_point(point: &ast::Type2) -> FlattenResult<Node> {
    let node = match point {
        ast::Type2::Value(v) => flatten_value(v),
        ast::Type2::Typename(t) => flatten_name_generic(t),
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
        Type2::Typename(s) => flatten_name_generic(s),
        Type2::Parethesized(t) => flatten_type(t),
        Type2::Map(g) => flatten_map(&g),
        Type2::Array(g) => flatten_array(&g),
        Type2::Unwrap(r) => Ok(Node::Unwrap(flatten_rule_generic(r)?)),
        Type2::ChoiceifyInline(g) => flatten_choiceify_inline(&g),
        Type2::Choiceify(r) => flatten_choiceify(r),
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
        _ => Node::Rule(Rule::new_name(name)),
    };
    Ok(result)
}

// Similar to flatten_name_generic, but if a prelude type is detected,
// it returns an error.  This is for the "unwrap" and "choiceify" operators,
// which can only be used on group references, not prelude types.
//
// This code doesn't validate that the rule name is actually a group; that
// will happen later.
fn flatten_rule_generic(name_generic: &ast::NameGeneric) -> FlattenResult<Rule> {
    let result = flatten_name_generic(name_generic);
    match result {
        Ok(Node::Rule(r)) => Ok(r),
        _ => Err(ValidateError::GenericError),
    }
}

fn flatten_name_generic(name_generic: &ast::NameGeneric) -> FlattenResult<Node> {
    // Flatten the name
    let mut node = flatten_typename(&name_generic.name)?;
    match node {
        Node::Rule(ref mut r) => {
            // Add the args to the rule.
            for arg in &name_generic.generic_args {
                // Need to flatten each individual generic arg.
                let arg_node = flatten_type1(&arg)?;
                r.generic_args.push(arg_node);
            }
        }
        _ => {
            // If the name resolves to a prelude type, and there are generic args,
            // return an error.
            if !name_generic.generic_args.is_empty() {
                return Err(ValidateError::GenericError);
            }
        }
    }
    Ok(node)
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

fn flatten_choiceify(name: &ast::NameGeneric) -> FlattenResult<Node> {
    let rule = flatten_rule_generic(name)?;
    Ok(Node::Choiceify(rule))
}

fn flatten_choiceify_inline(group: &ast::Group) -> FlattenResult<Node> {
    let kvs = flatten_group(group)?;
    Ok(Node::ChoiceifyInline(Array { members: kvs }))
}

// Useful utilities for testing the flatten code.
#[cfg(test)]
#[macro_use]
mod test_utils {
    use super::*;
    use std::collections::BTreeMap;

    // Given a string, generate a rule reference.
    impl From<&str> for Rule {
        fn from(s: &str) -> Self {
            Rule {
                name: s.to_string(),
                generic_args: vec![],
            }
        }
    }

    // Given a string, generate a Node::Rule
    impl From<&str> for Node {
        fn from(s: &str) -> Self {
            Node::Rule(Rule::from(s))
        }
    }

    // Given a Node (with no generic parameters), generate a RuleDef.
    impl From<Node> for RuleDef {
        fn from(n: Node) -> Self {
            RuleDef {
                generic_parms: Vec::default(),
                node: n,
            }
        }
    }

    // Given a list of names and Nodes, build a rules map.
    // Note: This requires Node instead of Into<Node> because we want to allow
    // multiple types of Node, which must be done by the caller.
    pub fn make_rules(mut list: Vec<(&str, Node)>) -> RulesByName {
        list.drain(..)
            .map(|(s, n)| (s.to_string(), RuleDef::from(n)))
            .collect()
    }

    // Given a single name/Node pair, build a rules map.
    pub fn make_rule<T: Into<Node>>(name: &str, node: T) -> RulesByName {
        let node: Node = node.into();
        let mut result = BTreeMap::new();
        result.insert(name.to_string(), RuleDef::from(node));
        result
    }

    // Given a single name/Node pair, build a rules map.
    pub fn make_generic_rule(name: &str, generic_parms: &[&str], node: Node) -> RulesByName {
        let mut result = BTreeMap::new();
        let generic_parms: Vec<String> = generic_parms.iter().map(|s| String::from(*s)).collect();
        result.insert(
            name.to_string(),
            RuleDef {
                generic_parms,
                node,
            },
        );
        result
    }

    // A trait for generating literals.
    pub trait CreateLiteral {
        fn literal(self) -> Node;
    }

    // Create a literal string.
    impl CreateLiteral for &str {
        fn literal(self) -> Node {
            Node::Literal(Literal::Text(self.to_string()))
        }
    }

    // Create a literal integer.
    // This will work for both signed and unsigned rust literals.
    impl CreateLiteral for i64 {
        fn literal(self) -> Node {
            Node::Literal(Literal::Int(self.into()))
        }
    }

    // Shorthand for the tstr prelude type
    pub fn tstr() -> Node {
        Node::PreludeType(PreludeType::Tstr)
    }

    // Shorthand for creating a new map.
    pub fn make_map() -> Map {
        Map {
            members: Vec::new(),
        }
    }

    // Shorthand for creating a new array.
    pub fn make_array() -> Array {
        Array {
            members: Vec::new(),
        }
    }

    // A trait for appending something (to a Map or Array)
    // since it returns Self, we can chain multiple calls.
    pub trait Append {
        fn append<T: Into<Node>>(self, t: T) -> Self;
    }

    impl Append for Map {
        fn append<T: Into<Node>>(mut self, t: T) -> Self {
            let node: Node = t.into();
            self.members.push(node);
            self
        }
    }

    impl Append for Array {
        fn append<T: Into<Node>>(mut self, t: T) -> Self {
            let node: Node = t.into();
            self.members.push(node);
            self
        }
    }

    // Shorthand for storing a Map inside a Node.
    impl From<Map> for Node {
        fn from(m: Map) -> Self {
            Node::Map(m)
        }
    }

    // Shorthand for storing an Array inside a Node.
    impl From<Array> for Node {
        fn from(a: Array) -> Self {
            Node::Array(a)
        }
    }

    // Shorthand for storing a KeyValue inside a Node.
    impl From<KeyValue> for Node {
        fn from(kv: KeyValue) -> Self {
            Node::KeyValue(kv)
        }
    }

    // Shorthand for storing a Rule inside a Node.
    impl From<Rule> for Node {
        fn from(r: Rule) -> Self {
            Node::Rule(r)
        }
    }

    // Shorthand for storing a Rule inside a Node.
    impl From<Control> for Node {
        fn from(r: Control) -> Self {
            Node::Control(r)
        }
    }

    #[derive(Copy, Clone)]
    pub enum KvCut {
        Cut,
        NoCut,
    }
    pub use KvCut::*;

    impl From<KvCut> for bool {
        fn from(c: KvCut) -> bool {
            match c {
                Cut => true,
                NoCut => false,
            }
        }
    }

    // Shorthand for creating a key-value pair, with explicit cut setting.
    pub fn kv(k: Node, v: Node, cut: KvCut) -> KeyValue {
        KeyValue {
            key: Box::new(k),
            value: Box::new(v),
            cut: cut.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::test_utils::*;
    use super::*;

    #[test]
    fn test_flatten_literal_int() {
        let cddl_input = r#"thing = 1"#;
        let result = flatten_from_str(cddl_input).unwrap();
        let expected = make_rule("thing", 1.literal());
        assert_eq!(result, expected);

        let cddl_input = r#"thing = -1"#;
        let result = flatten_from_str(cddl_input).unwrap();
        let expected = make_rule("thing", (-1i64).literal());
        assert_eq!(result, expected);
    }

    #[test]
    fn test_flatten_literal_tstr() {
        let cddl_input = r#"thing = "abc""#;
        let result = flatten_from_str(cddl_input).unwrap();
        let expected = make_rule("thing", "abc".literal());
        assert_eq!(result, expected);
    }

    #[test]
    fn test_flatten_prelude_reference() {
        let cddl_input = r#"thing = int"#;
        let result = flatten_from_str(cddl_input).unwrap();
        let expected = make_rule("thing", Node::PreludeType(PreludeType::Int));
        assert_eq!(result, expected);
    }

    #[test]
    fn test_flatten_type_reference() {
        let cddl_input = r#"thing = foo"#;
        let result = flatten_from_str(cddl_input).unwrap();
        assert_eq!(result, make_rule("thing", Rule::from("foo")));
    }

    #[test]
    fn test_flatten_map() {
        // A map containing a bareword key
        let cddl_input = r#"thing = { foo: tstr }"#;
        let result = flatten_from_str(cddl_input).unwrap();
        let expected = make_rule("thing", make_map().append(kv("foo".literal(), tstr(), Cut)));
        assert_eq!(result, expected);

        // A map containing a prelude type key.
        // Note: CDDL syntax requires type keys to use "=>" not ":", otherwise
        // it will assume a bareword key is being used.
        let cddl_input = r#"thing = { tstr => tstr }"#;
        let result = flatten_from_str(cddl_input).unwrap();
        let expected = make_rule("thing", make_map().append(kv(tstr(), tstr(), NoCut)));
        assert_eq!(result, expected);

        // A map key name alias
        let cddl_input = r#"foo = "bar" thing = { foo => tstr }"#;
        let result = flatten_from_str(cddl_input).unwrap();
        let expected = make_rules(vec![
            ("foo", "bar".literal()),
            (
                "thing",
                make_map().append(kv("foo".into(), tstr(), NoCut)).into(),
            ),
        ]);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_flatten_generic() {
        let cddl_input = "message<t, v> = [t, v]";
        let result = flatten_from_str(cddl_input).unwrap();
        let expected = make_generic_rule(
            "message",
            &["t", "v"],
            make_array()
                .append(Rule::from("t"))
                .append(Rule::from("v"))
                .into(),
        );
        assert_eq!(result, expected);
    }

    #[test]
    fn test_control_op() {
        let cddl_input = "four_bytes = tstr .size 4";
        let result = flatten_from_str(cddl_input).unwrap();
        let expected = make_rule(
            "four_bytes",
            Control::Size(CtlOpSize {
                target: Box::new(tstr()),
                size: Box::new(4.literal()),
            }),
        );
        assert_eq!(result, expected);
    }
}
