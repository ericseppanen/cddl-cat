//! This module defines the Intermediate Validation Tree.
//!
//! It contains a simplified representation of a CDDL rule, flattened to only
//! include the parts that are necessary for validation.
//!
//! This module doesn't know anything about validating specific types (e.g.
//! CBOR or JSON), but it helps make writing those validators easier.

use crate::ast;
use std::collections::BTreeMap;
use std::fmt;
use strum_macros::{Display, IntoStaticStr};

/// The definition of a CDDL rule.
///
/// Each rule has a name, some (optional) generic parameters, and a
/// definition `Node`.
#[derive(Clone, Debug, PartialEq)]
pub struct RuleDef {
    /// Optional generic parameters.
    pub generic_parms: Vec<String>,
    /// The Node representing the rule definition.
    pub node: Node,
}

/// A set of CDDL rules.
///
/// Each rule has a name, some (optional) generic parameters, and a
/// definition `Node`.
pub type RulesByName = BTreeMap<String, RuleDef>;

/// A set of CDDL rules.
///
/// Each rule has a name, some (optional) generic parameters, and a
/// definition `Node`.
///
/// `RulesWithStrings` is exactly like `RulesByName`, except that it
/// preserves the original CDDL text for the rule, to assist in debugging.
pub type RulesWithStrings = BTreeMap<String, (RuleDef, String)>;

/// One of the types named in the CDDL prelude.
///
/// The following types are defined in [RFC8610 appendix D]:
/// `any`, `bool`, `int`, `uint`, `float`, `tstr`, `bstr`.
/// There are more that aren't supported by this crate yet.
///
/// [RFC8610 appendix D]: https://tools.ietf.org/html/rfc8610#appendix-D
#[derive(Debug, Copy, Clone, PartialEq, Eq, Display)]
#[allow(missing_docs)]
pub enum PreludeType {
    /// Any type or embedded data structure
    Any,
    /// Nil aka null: nothing.
    Nil,
    /// A boolean value: true or false
    Bool,
    /// A positive or negative integer
    Int,
    /// An integer >= 0
    Uint,
    /// An integer < 0
    Nint,
    /// A floating-point value
    Float,
    /// A text string
    Tstr,
    /// A byte string
    Bstr,
}

/// A literal value, e.g. `7`, `1.3`, or ``"foo"``.
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub enum Literal {
    Bool(bool),
    Int(i128),
    Float(f64),
    Text(String),
    Bytes(Vec<u8>),
    // TODO: nil?
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::Int(i) => write!(f, "{}", i),
            // FIXME: it's annoying that floating point values can omit the
            // decimal, which can be confused for an integer.
            Literal::Float(fl) => write!(f, "{}", fl),
            Literal::Text(s) => write!(f, "\"{}\"", s),
            Literal::Bytes(_) => write!(f, "LiteralBytes"),
        }
    }
}

/// A shortcut for `Node::Literal(Literal::Bool(b))`
pub fn literal_bool(b: bool) -> Node {
    Node::Literal(Literal::Bool(b))
}

/// A shortcut for `Node::Literal(Literal::Int(i))`
///
/// This doesn't work for isize and usize, unfortunately.
pub fn literal_int<T: Into<i128>>(i: T) -> Node {
    Node::Literal(Literal::Int(i.into()))
}

/// A shortcut for `Node::Literal(Literal::Float(f))`
pub fn literal_float<T: Into<f64>>(f: T) -> Node {
    Node::Literal(Literal::Float(f.into()))
}

/// A shortcut for `Node::Literal(Literal::Text(t))`
pub fn literal_text<T: Into<String>>(s: T) -> Node {
    Node::Literal(Literal::Text(s.into()))
}

/// A shortcut for `Node::Literal(Literal::Bytes(b))`
pub fn literal_bytes<T: Into<Vec<u8>>>(b: T) -> Node {
    Node::Literal(Literal::Bytes(b.into()))
}

/// A rule reference, linked to a dispatch object for later resolution.
///
/// Resolving the rule reference is handled by the validation context.
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub struct Rule {
    pub name: String,
    pub generic_args: Vec<Node>,
}

impl Rule {
    // Create a new rule reference by name
    #[doc(hidden)] // Only pub for integration tests
    pub fn new(name: &str, generic_args: Vec<Node>) -> Rule {
        Rule {
            name: name.to_string(),
            generic_args,
        }
    }

    #[doc(hidden)] // Only pub for integration tests
    pub fn new_name(name: &str) -> Rule {
        Rule {
            name: name.to_string(),
            generic_args: Vec::new(),
        }
    }
}

/// A Choice validates if any one of a set of options validates.
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub struct Choice {
    pub options: Vec<Node>,
}

/// A key-value pair; key and value can be anything (types, arrays, maps, etc.)
///
/// "Cut" means a match on this key will prevent any later keys from matching.
///
/// For example, a map containing `"optional-key": "hello"`
/// would be permitted to match the following:
///
/// ```text
/// extensible-map-example = {
///     ? "optional-key" => int,
///     * tstr => any
/// }
/// ```
/// If we add the cut symbol `^`, the same map would not match:
///
/// ```text
/// extensible-map-example = {
///     ? "optional-key" ^ => int,
///     * tstr => any
/// }
/// ```
///
/// Note: CDDL map members that use `:` always use cut semantics.
///
/// See RFC8610 3.5.4 for more discussion.
///
#[derive(Clone, PartialEq)]
#[allow(missing_docs)]
pub struct KeyValue {
    pub key: Box<Node>,
    pub value: Box<Node>,
    pub cut: IsCut,
}

pub(crate) type IsCut = bool;

impl KeyValue {
    #[doc(hidden)] // Only pub for integration tests
    pub fn new(key: Node, value: Node, cut: IsCut) -> KeyValue {
        KeyValue {
            key: Box::new(key),
            value: Box::new(value),
            cut,
        }
    }
}

impl fmt::Display for KeyValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.key, self.value)
    }
}

// Implement Debug by hand so we can format it like a map.
impl fmt::Debug for KeyValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut formatter = f.debug_tuple("KeyValue");
        formatter.field(&self.key).field(&self.value).finish()
    }
}

/// Specify a CDDL occurrence's limits.
///
/// An "occurrence" in CDDL specifies how many times a value should repeat
/// (in an array) or flag optional map keys.  [RFC8610] specifies the following
/// occurrence symbols:
/// ```text
/// "?" Optional
/// "*" Zero or more
/// "+" One or more
/// n*m Between n and m, inclusive (n and m are both optional)
/// ```
///
/// [RFC8610]: https://tools.ietf.org/html/rfc8610

// Re-use the Occur limit type that the parser uses
pub type OccurLimit = ast::Occur;

/// Occurences specify how many times a value can appear.
///
/// This implementation wraps the Node that the occurrence applies to.

#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub struct Occur {
    pub limit: OccurLimit,
    pub node: Box<Node>,
}

impl Occur {
    /// Creates a new Occur from one of the CDDL occurrence chars ?*+
    pub fn new(limit: OccurLimit, node: Node) -> Occur {
        Occur {
            limit,
            node: Box::new(node),
        }
    }

    /// Get the CDDL symbol for this occurrence.
    ///
    /// Returns `?`, `*`, `+`, or `n*m`
    pub fn symbol(&self) -> String {
        match self.limit {
            OccurLimit::Optional => "?".into(),
            OccurLimit::ZeroOrMore => "*".into(),
            OccurLimit::OneOrMore => "+".into(),
            OccurLimit::Numbered(n, m) => match (n, m) {
                (0, std::usize::MAX) => format!("{}*", n),
                (_, _) => format!("{}*{}", n, m),
            },
        }
    }

    /// Return the lower and upper limits on this occurrence
    ///
    /// Occurrences can always be represented by an inclusive [lower, upper]
    /// count limit.
    ///
    /// required         => [1, 1]
    /// optional "?"     => [0, 1]
    /// zero-or-more "*" => [0, MAX]
    /// one-or-more "+"  => [1, MAX]
    pub fn limits(&self) -> (usize, usize) {
        match self.limit {
            OccurLimit::Optional => (0, 1),
            OccurLimit::ZeroOrMore => (0, usize::MAX),
            OccurLimit::OneOrMore => (1, usize::MAX),
            OccurLimit::Numbered(n, m) => (n, m),
        }
    }
}

impl fmt::Display for Occur {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.symbol(), self.node)
    }
}

/// A map containing key-value pairs.
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub struct Map {
    pub members: Vec<Node>,
}

/// A context-free group of key-value pairs.
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub struct Group {
    pub members: Vec<Node>,
}

/// An array is a list of types in a specific order.
///
/// Arrays are expected to take the form of "records" or "vectors".
///
/// A "vector" array is expected to have an arbitrary-length list of a single
/// type, e.g. zero-or-more integers:
/// ```text
/// [ * int ]
/// ```
/// The type in a vector could be something complex, like a group, choice, or
/// another array or map.
///
/// A "record" array is a sequence of different values, each with a specific
/// type.  It has similar semantics to a rust tuple, though it could also
/// theoretically be used to serialize a struct.
///
/// CDDL syntax allows certain nonsensical or ambiguous arrays, for example:
/// ```text
/// thing = [ * mygroup ]
/// mygroup = ( a = tstr, b = int)
/// ```
/// or
/// ```text
/// thing = [ * "a" = int, * "b" = int ]
/// ```
///
/// CDDL arrays may be composed of key-value pairs, but the keys are solely
/// for information/debugging; they are ignored for validation purposes.
///
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub struct Array {
    pub members: Vec<Node>,
}

/// A range of numbers.
///
/// Ranges can be defined as inclusive (`..`) or exclusive (`...`).
///
/// CDDL only defines ranges between two integers or between two floating
/// point values.  A lower bound that exceeds the upper bound is valid CDDL,
/// but behaves as an empty set.
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub struct Range {
    pub start: Box<Node>,
    pub end: Box<Node>,
    pub inclusive: bool,
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = if self.inclusive { ".." } else { "..." };
        write!(f, "{}{}{}", self.start, op, self.end)
    }
}

/// Control Operators
///
/// A control operator constrains a type by adding an additional condition
/// that must be met. For example, "tstr .size 10" permits only strings of
/// 10 bytes or less.  See RFC 8610 section 3.8 for details.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq)]
pub enum Control {
    /// Limit the size in bytes.
    Size(CtlOpSize),
    /// Apply a regular expression to a text string.
    Regexp(CtlOpRegexp),
    /// Validate a nested CBOR bytestring
    Cbor(CtlOpCbor),
}

/// Control Operator `.size`
///
/// `.size` is defined in RFC 8610 3.8.1.
/// It sets an upper limit, measured in bytes.
///
/// For example, "tstr .size 10" permits only strings of
/// 10 bytes or less.  See RFC 8610 section 3.8 for details.
#[derive(Debug, Clone, PartialEq)]
pub struct CtlOpSize {
    /// The type that is size-constrained.
    ///
    /// Only certain types are permitted.  RFC 8610 defines `.size` for
    /// `tstr`, `bstr`, and unsigned integers.
    pub target: Box<Node>,
    /// The size limit, in bytes.
    pub size: Box<Node>,
}

/// Control Operator `.regexp`
///
/// `.regexp` is defined in RFC 8610 3.8.3.
///
#[derive(Debug, Clone)]
pub struct CtlOpRegexp {
    /// The regular expression, in compiled form.
    pub(crate) re: regex::Regex,
}

impl PartialEq for CtlOpRegexp {
    fn eq(&self, other: &Self) -> bool {
        // We only need to compare the string form,
        // not the compiled form.
        self.re.as_str() == other.re.as_str()
    }
}

/// Control Operator `.cbor`
///
/// `.cbor` is defined in RFC 8610 3.8.4
///
/// A ".cbor" control on a byte string indicates that the byte string
/// carries a CBOR-encoded data item.  Decoded, the data item matches the
/// type given as the right-hand-side argument.
#[derive(Debug, Clone, PartialEq)]
pub struct CtlOpCbor {
    /// The nested node to satisfy
    pub(crate) node: Box<Node>,
}

/// Any node in the Intermediate Validation Tree.
#[derive(Debug, Clone, PartialEq, IntoStaticStr)]
#[allow(missing_docs)]
pub enum Node {
    Literal(Literal),
    PreludeType(PreludeType),
    Rule(Rule),
    Choice(Choice),
    Map(Map),
    Array(Array),
    Group(Group),
    KeyValue(KeyValue),
    Occur(Occur),
    Unwrap(Rule),
    Range(Range),
    Control(Control),
    Choiceify(Rule),
    ChoiceifyInline(Array),
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Literal(l) => write!(f, "{}", l),
            Node::PreludeType(p) => write!(f, "{}", p),
            Node::KeyValue(kv) => write!(f, "{}", kv),
            _ => {
                let variant: &str = self.into();
                write!(f, "{}", variant)
            }
        }
    }
}
