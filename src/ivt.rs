//! This module defines the Intermediate Validation Tree.
//!
//! It contains a simplified representation of a CDDL rule, flattened to only
//! include the parts that are necessary for validation.
//!
//! This module doesn't know anything about validating specific types (e.g.
//! CBOR or JSON), but it helps make writing those validators easier.

use crate::util::*;
use std::fmt;
use std::sync::{Arc, Mutex, Weak};

/// A trait that allows recursive validation of an AST.
pub trait Validate<T> {
    fn validate(&self, node: &Node) -> TempResult<T>;
}

// Some useful type shortcuts
pub type ArcNode = Arc<Node>;
pub type VecNode = Vec<ArcNode>;

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

/// A rule reference, with interior mutability.
pub struct Rule {
    pub name: String,
    // The actual rule reference is stored in a Mutex so that we can
    // mutate it later, swapping out by-name references for Arc references.
    // We use a Weak reference so we don't accidentally create reference
    // cycles that leak memory.
    pub node_ref: Mutex<Option<Weak<Node>>>,
}

// Implement Debug by hand so we can give the Mutex special treatment.
impl fmt::Debug for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let bang;
        let my_name = match self.is_upgraded() {
            true => {
                bang = format!("{}!", self.name);
                &bang
            }
            false => &self.name,
        };
        f.debug_struct("Rule").field("name", my_name).finish()
    }
}

// Implement Clone by hand with special handling for the Mutex
impl Clone for Rule {
    fn clone(&self) -> Rule {
        let name = self.name.clone();
        let guard = self.node_ref.lock().unwrap();
        let node_ref = Mutex::new(guard.clone());
        Rule { name, node_ref }
    }
}

impl Rule {
    // Create a new rule reference by name
    pub fn new(name: &str) -> Rule {
        let name = name.to_string();
        let node_ref = Mutex::new(None);
        Rule { name, node_ref }
    }
    // Upgrade a rule reference to a real Arc reference
    pub fn upgrade(&self, node: &ArcNode) {
        let mut guard = self.node_ref.lock().unwrap();
        assert!(guard.is_none());
        guard.replace(Arc::downgrade(node));
    }

    pub fn is_upgraded(&self) -> bool {
        let guard = self.node_ref.lock().unwrap();
        guard.is_some()
    }

    pub fn get_ref(&self) -> Option<ArcNode> {
        let guard = self.node_ref.lock().unwrap();
        let weak_ref = guard.as_ref()?; // handles Option::None
        weak_ref.upgrade()
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
    pub key: ArcNode,
    pub value: ArcNode,
}

impl KeyValue {
    pub fn new(key: Node, value: Node) -> KeyValue {
        let key = Arc::new(key);
        let value = Arc::new(value);
        KeyValue { key, value }
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
    pub node: ArcNode,
}

impl Occur {
    /// Creates a new Occur from one of the CDDL occurrence chars ?*+
    pub fn new(limit: OccurLimit, n: Node) -> Occur {
        let node = Arc::new(n);
        Occur {limit, node}
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

// This is just a convenience function, that reverses Node and Value, because
// it's more intuitive to write node.validate(value) than value.validate(node).
impl Node {
    pub fn validatex<T, V: Validate<T>>(&self, value: &V) -> TempResult<T> {
        value.validate(self)
    }
}
