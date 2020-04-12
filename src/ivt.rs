//! This module defines the Intermediate Validation Tree.
//!
//! It contains a simplified representation of a CDDL rule, flattened to only
//! include the parts that are necessary for validation.
//!
//! This module doesn't know anything about validating specific types (e.g.
//! CBOR or JSON), but it helps make writing those validators easier.

use crate::ast;
use std::fmt;
use strum_macros::{Display, IntoStaticStr};

/// One of the types named in the CDDL prelude.
///
/// The following types are defined in [RFC8610 appendix D]:
/// `any`, `bool`, `int`, `uint`, `float`, `tstr`, `bstr`.
/// There are more that aren't supported by this crate yet.
///
/// [RFC8610 appendix D]: https://tools.ietf.org/html/rfc8610#appendix-D
#[derive(Debug, Copy, Clone, Display)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Rule {
    pub name: String,
    // Resolving the rule reference is handled by the validation context.
}

impl Rule {
    // Create a new rule reference by name
    #[doc(hidden)] // Only pub for integration tests
    pub fn new(name: &str) -> Rule {
        Rule {
            name: name.to_string(),
        }
    }
}

/// A Choice validates if any one of a set of options validates.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Choice {
    pub options: Vec<Node>,
}

/// A key-value pair; key and value can be anything (types, arrays, maps, etc.)
#[derive(Clone)]
#[allow(missing_docs)]
pub struct KeyValue {
    pub key: Box<Node>,
    pub value: Box<Node>,
}

impl KeyValue {
    #[doc(hidden)] // Only pub for integration tests
    pub fn new(key: Node, value: Node) -> KeyValue {
        KeyValue {
            key: Box::new(key),
            value: Box::new(value),
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

#[derive(Debug, Clone)]
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
            OccurLimit::ZeroOrMore => (0, std::usize::MAX),
            OccurLimit::OneOrMore => (1, std::usize::MAX),
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
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Map {
    pub members: Vec<Node>,
}

/// A context-free group of key-value pairs.
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Array {
    pub members: Vec<Node>,
}

#[derive(Debug, Clone)]
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

/// Any node in the Intermediate Validation Tree.
#[derive(Debug, Clone, IntoStaticStr)]
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
