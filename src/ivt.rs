//! This module defines the Intermediate Validation Tree.
//!
//! It contains a simplified representation of a CDDL rule, flattened to only
//! include the parts that are necessary for validation.
//!
//! This module doesn't know anything about validating specific types (e.g.
//! CBOR or JSON), but it helps make writing those validators easier.

use crate::util::*;
use std::sync::Arc;

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
    Int,
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
    // TODO: byte string literals, nil?
}

/// A rule reference, by name.

// FIXME: this is an awkward type; I don't want it to exist in the final IVT,
// but I can't think of any way to not require it, at least temporarily, while
// assembling the tree.  Actual validation code should never see one of these,
// because it should have been replaced by a reference to another Node.
#[derive(Debug, Clone)]
pub struct Rule {
    pub name: String,
}

/// A Choice validates if any one of a set of options validates.
#[derive(Debug, Clone)]
pub struct Choice {
    pub options: VecNode,
}

/// A key-value pair; key and value can be anything (types, arrays, maps, etc.)
#[derive(Debug, Clone)]
pub struct KeyValue {
    pub key: ArcNode,
    pub value: ArcNode,
}

/// A map containing key-value pairs.
#[derive(Debug, Clone)]
pub struct Map {
    pub members: Vec<KeyValue>,
}

/// An array with "record" semantics: a list of types in a specific order.
///
/// It has similar semantics to a rust tuple, though it could also be used
/// to serialize a struct.
/// It contains key-value pairs, but the keys are solely for debugging;
/// they are ignored for validation purposes.
#[derive(Debug, Clone)]
pub struct ArrayRecord {
    pub elements: Vec<KeyValue>,
}

/// An array with "vector" semantics: a homogenous list of elements, all of the
/// same type.
#[derive(Debug, Clone)]
pub struct ArrayVec {
    // TODO: handle occurrences
    pub element: ArcNode,
}

/// Any node in the Intermediate Validation Tree.
#[derive(Debug, Clone)]
pub enum Node {
    Literal(Literal),
    PreludeType(PreludeType),
    Rule(Rule),
    Choice(Choice),
    Map(Map),
    ArrayRecord(ArrayRecord),
    ArrayVec(ArrayVec),
}


// This is just a convenience function, that reverses Node and Value, because
// it's more intuitive to write node.validate(value) than value.validate(node).
impl Node {
    pub fn validatex<T, V: Validate<T>>(&self, value: &V) -> TempResult<T> {
        value.validate(self)
    }
}
