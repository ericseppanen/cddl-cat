//! This module defines the Intermediate Validation Tree.
//!
//! It contains a simplified representation of a CDDL rule, flattened to only
//! include the parts that are necessary for validation.
//!
//! This module doesn't know anything about validating specific types (e.g.
//! CBOR or JSON), but it helps make writing those validators easier.

use std::collections::BTreeMap;
use std::fmt;

pub type RulesByName = BTreeMap<String, Node>;

// Some useful type shortcuts
pub type VecNode = Vec<Node>;

/// One of the types named in the CDDL prelude.
#[derive(Debug, Copy, Clone)]
pub enum PreludeType {
    Any, // Not really a prelude type, but it's easy to handle here.
    Bool,
    Int,
    Uint,
    Float,
    Tstr,
    Bstr,
}

/// A literal value, e.g. `7`, `1.3`, or ``"foo"``.
#[derive(Debug, Clone)]
pub enum Literal {
    Bool(bool),
    Int(i128),
    Float(f64),
    Text(String),
    Bytes(Vec<u8>),
    // TODO: nil?
}

/// A rule reference, linked to a dispatch object for later resolution
#[derive(Debug, Clone)]
pub struct Rule {
    pub name: String,
    // Resolving the rule reference is handled by the validation context.
}

impl Rule {
    // Create a new rule reference by name
    pub fn new(name: &str) -> Rule {
        Rule {
            name: name.to_string(),
        }
    }
}

/// A Choice validates if any one of a set of options validates.
#[derive(Debug, Clone)]
pub struct Choice {
    pub options: VecNode,
}

/// A key-value pair; key and value can be anything (types, arrays, maps, etc.)
#[derive(Clone)]
pub struct KeyValue {
    pub key: Box<Node>,
    pub value: Box<Node>,
}

impl KeyValue {
    pub fn new(key: Node, value: Node) -> KeyValue {
        KeyValue {
            key: Box::new(key),
            value: Box::new(value),
        }
    }
}

// Implement Debug by hand so we can format it like a map.
impl fmt::Debug for KeyValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut formatter = f.debug_tuple("KeyValue");
        formatter.field(&self.key).field(&self.value).finish()
    }
}

#[derive(Debug, Clone)]
pub enum OccurLimit {
    Optional,
    ZeroOrMore,
    OneOrMore,
    Numbered(usize, usize),
}

/// Occurences specify how many times a value can appear.
///
#[derive(Debug, Clone)]
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

/// A map containing key-value pairs.
#[derive(Debug, Clone)]
pub struct Map {
    pub members: VecNode,
}

/// A context-free group of key-value pairs.
#[derive(Debug, Clone)]
pub struct Group {
    pub members: VecNode,
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
pub struct Array {
    pub members: VecNode,
}

/// Any node in the Intermediate Validation Tree.
#[derive(Debug, Clone)]
pub enum Node {
    Literal(Literal),
    PreludeType(PreludeType),
    Rule(Rule), // FIXME: NameRef
    Choice(Choice),
    Map(Map),
    Array(Array),
    Group(Group), // FIXME: NameRef
    KeyValue(KeyValue),
    Occur(Occur),
}
