//! This module implements validation from [`serde_json::Value`].
//!
//! # Examples
//!
//! ```
//! use cddl_cat::validate_json_str;
//!
//! let cddl_input = "person = {name: tstr, age: int}";
//! let json_str = r#"{ "name": "Bob", "age": 43 }"#;
//!
//! validate_json_str("person", cddl_input, &json_str).unwrap();
//! ```
//!

#![cfg(feature = "serde_json")]

use crate::context::{BasicContext, Context};
use crate::flatten::flatten_from_str;
use crate::ivt::Node;
use crate::util::{ValidateError, ValidateResult};
use crate::validate::validate;
use crate::value::Value;
use serde_json::Value as JSON_Value;
use std::collections::BTreeMap;
use std::convert::TryFrom;

// Convert JSON `Value`s to the local `Value` type that the validate code
// uses.

impl TryFrom<&JSON_Value> for Value {
    type Error = ValidateError;

    fn try_from(value: &JSON_Value) -> Result<Self, Self::Error> {
        let result = match value {
            JSON_Value::Null => Value::Null,
            JSON_Value::Bool(b) => Value::Bool(*b),
            JSON_Value::Number(num) => {
                if let Some(u) = num.as_u64() {
                    Value::Integer(u as i128)
                } else if let Some(i) = num.as_i64() {
                    Value::Integer(i as i128)
                } else if let Some(f) = num.as_f64() {
                    Value::from_float(f)
                } else {
                    return Err(ValidateError::ValueError(
                        "JSON Value::Number conversion failure".into(),
                    ));
                }
            }
            JSON_Value::String(t) => Value::Text(t.clone()),
            JSON_Value::Array(a) => {
                let array: Result<_, _> = a.iter().map(Value::try_from).collect();
                Value::Array(array?)
            }
            JSON_Value::Object(m) => {
                type MapTree = BTreeMap<Value, Value>;
                let map: Result<MapTree, _> = m
                    .iter()
                    .map(|(k, v)| {
                        // An iterator returning a 2-tuple can be used as (key, value)
                        // when building a new map.
                        Ok((Value::Text(k.clone()), Value::try_from(v)?))
                    })
                    .collect();
                Value::Map(map?)
            }
        };
        Ok(result)
    }
}

#[test]
fn test_json_number_behavior() {
    // Ensures that our JSON decoder tracks number types precisely, and
    // doesn't, say, allow floating-point values to become integers.
    // serde_json does sometimes permit as_f64 to work on integers, which is
    // why try_from has to test u64, then i64, then f64.

    let json_value: JSON_Value = serde_json::from_str("1").unwrap();
    assert!(json_value.as_u64().is_some());

    let json_value: JSON_Value = serde_json::from_str("-1").unwrap();
    assert!(json_value.as_u64().is_none());
    assert!(json_value.as_i64().is_some());

    let json_value: JSON_Value = serde_json::from_str("1.0").unwrap();
    assert!(json_value.as_u64().is_none());
    assert!(json_value.as_i64().is_none());
    assert!(json_value.as_f64().is_some());
}

// A variant that consumes the JSON Value.
impl TryFrom<JSON_Value> for Value {
    type Error = ValidateError;

    fn try_from(value: JSON_Value) -> Result<Self, Self::Error> {
        Value::try_from(&value)
    }
}

/// Validate already-parsed JSON data against an already-parsed CDDL schema.
pub fn validate_json(node: &Node, value: &JSON_Value, ctx: &dyn Context) -> ValidateResult {
    let value = Value::try_from(value)?;
    validate(&value, node, ctx)
}

/// Validate JSON-encoded data against a specified rule in a UTF-8 CDDL schema.
pub fn validate_json_str(name: &str, cddl: &str, json: &str) -> ValidateResult {
    // Parse the CDDL text and flatten it into IVT form.
    let flat_cddl = flatten_from_str(cddl)?;
    let ctx = BasicContext::new(flat_cddl);

    // Find the rule name that was requested
    let rule_node: &Node = ctx
        .rules
        .get(name)
        .ok_or_else(|| ValidateError::MissingRule(name.into()))?;

    // Deserialize the JSON bytes
    let json_value: JSON_Value =
        serde_json::from_str(json).map_err(|e| ValidateError::ValueError(format!("{}", e)))?;

    // Convert the JSON tree into a Value tree for validation
    let value = Value::try_from(json_value)?;
    validate(&value, rule_node, &ctx)
}
