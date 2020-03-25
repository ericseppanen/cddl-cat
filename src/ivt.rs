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

pub trait Validate<T> {
    fn validate(&self, value: &T) -> ValidateResult;
}

pub trait Pogo<N> {
    fn pogo(&self, node: &N) -> ValidateResult;
}

//type DynVal<T> = Box<dyn Validate<T>>;

// Some useful type shortcuts
pub type ArcNode = Arc<Node>;
pub type VecNode = Vec<ArcNode>;

// One of the types named in the CDDL prelude
pub enum PreludeType {
    Int,
    Tstr,
    Bstr,
}

// A rule reference, by name.
// FIXME: how can I replace these with actual owning references (e.g. Arc<Node>)?
// I think what happens is that the Rule node just gets replaced with an ArcNode that
// shares ownership of the RHS of the rule (which can be a type, array, map, etc.)
pub struct Rule {
    pub name: String,
}

// Something that validates if it matches any of the options.
pub struct Choice {
    pub options: VecNode,
}

// Intermediate Key-value pair; key and value can be anything (types, arrays, maps, etc.)
pub struct KeyValue {
    pub key: ArcNode,
    pub value: ArcNode,
}

pub struct Map {
    pub members: Vec<KeyValue>,
}

pub struct ArrayRecord {
    pub elements: Vec<KeyValue>,
}

pub struct ArrayVec {
    // TODO: handle occurrences
    pub element: ArcNode,
}

// Any node in the Intermediate Validation Tree
pub enum Node {
    PreludeType(PreludeType),
    Rule(Rule),
    Choice(Choice),
    KeyValue(KeyValue),
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

pub fn validate_choice<T>(choice: &Choice, value: &T) -> ValidateResult
where
    T: Pogo<Node>,
{
    // A choice validates if any of the options validates
    for node in &choice.options {
        let node: &Node = node.as_ref();
        // This generates a compiler error "expected ___, found type parameter `T`"
        // It would be a lot more graceful if this had worked.
        // Instead, we have this Pogo workaround.
        //node.validate(value)?;
        value.pogo(node)?;
    }
    Ok(())
}

// FIXME: I can't figure out how to implement this logic.
// I can kind of see how it's impossible to dispatch the inner call
// to validate() because we can't know at compile-time what function
// to call, or even if one exists.
/*
impl<T> Validate<T> for Choice {
    fn validate(&self, value: &T) -> ValidateResult {
        // A choice validates if any of the options validates
        for node in self.options {
            node.validate::<T>(value)?;
        }
        Ok(())
    }
}
*/
