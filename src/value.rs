//! This module declares a generic Value enum for use with validation.

use std::collections::BTreeMap;
use std::fmt;

use float_ord::FloatOrd;

/// `Value` represents all the types of data we can validate.
///
/// To validate a new type of data, write implementations of the `From`
/// trait for that type.  See the [`cbor`] module for an example.
///
/// [`cbor`]: crate::cbor
///
#[derive(Clone, Eq, Ord, PartialEq, PartialOrd)]
#[allow(missing_docs)]
pub enum Value {
    Null,
    Bool(bool),
    Integer(i128),
    Float(FloatOrd<f64>),
    Bytes(Vec<u8>),
    Text(String),
    Array(Vec<Value>),
    Map(BTreeMap<Value, Value>),
}

// FloatOrd doesn't implement Debug, so we have to do all the work by hand.
impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Null => write!(f, "Null"),
            Value::Bool(x) => x.fmt(f),
            Value::Integer(x) => x.fmt(f),
            Value::Float(x) => x.0.fmt(f),
            Value::Bytes(x) => x.fmt(f),
            Value::Text(x) => x.fmt(f),
            Value::Array(x) => x.fmt(f),
            Value::Map(x) => x.fmt(f),
        }
    }
}

// Only exists so implementers don't need to use/see float_ord::FloatOrd
impl Value {
    pub(crate) fn from_float<F: Into<f64>>(f: F) -> Value {
        Value::Float(FloatOrd(f.into()))
    }
}
