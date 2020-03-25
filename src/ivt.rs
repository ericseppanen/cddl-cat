use std::error;
use std::fmt;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum ValidateError {
    Oops(String),
}

impl fmt::Display for ValidateError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ValidateError::Oops(msg) => write!(f, "Oops! {}", msg),
        }
    }
}

// Standard boilerplate, required so other errors can wrap this one.
impl error::Error for ValidateError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

pub type ValidateResult = std::result::Result<(), ValidateError>;

pub trait Validate {
    fn validate(&self, node: &Node) -> ValidateResult;
}

// Some useful type shortcuts
pub type ArcNode = Arc<Node>;
pub type VecNode = Vec<ArcNode>;

// One of the types named in the CDDL prelude
#[derive(Debug, Copy, Clone)]
pub enum PreludeType {
    Int,
    Tstr,
    Bstr,
}

// A literal value, e.g. 7, 1.3, or "foo"
#[derive(Debug, Copy, Clone)]
pub enum Literal {
    Bool(bool),
    Int(i128),
    Float(f64),
    // TODO: byte string literals, nil?
}

// A rule reference, by name.
// FIXME: how can I replace these with actual owning references (e.g. Arc<Node>)?
// I think what happens is that the Rule node just gets replaced with an ArcNode that
// shares ownership of the RHS of the rule (which can be a type, array, map, etc.)
#[derive(Debug, Clone)]
pub struct Rule {
    pub name: String,
}

// Something that validates if it matches any of the options.
#[derive(Debug, Clone)]
pub struct Choice {
    pub options: VecNode,
}

// Intermediate Key-value pair; key and value can be anything (types, arrays, maps, etc.)
#[derive(Debug, Clone)]
pub struct KeyValue {
    pub key: ArcNode,
    pub value: ArcNode,
}

#[derive(Debug, Clone)]
pub struct Map {
    pub members: Vec<KeyValue>,
}

#[derive(Debug, Clone)]
pub struct ArrayRecord {
    pub elements: Vec<KeyValue>,
}

#[derive(Debug, Clone)]
pub struct ArrayVec {
    // TODO: handle occurrences
    pub element: ArcNode,
}

// Any node in the Intermediate Validation Tree
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

// FIXME: is this a good idea?  It's handy for unit testing...
impl From<PreludeType> for Node {
    fn from(item: PreludeType) -> Node {
        Node::PreludeType(item)
    }
}

// This is just a convenience function, that reverses Node and Value, because
// it's more intuitive to write node.validate(value) than value.validate(node).
impl Node {
    fn validatex<T: Validate>(&self, value: &T) -> ValidateResult {
        value.validate(self)
    }
}

pub fn validate_choice<T: Validate>(choice: &Choice, value: &T) -> ValidateResult {
    // A choice validates if any of the options validates
    for node in &choice.options {
        let node: &Node = node.as_ref();
        value.validate(node)?;
    }
    Ok(())
}
