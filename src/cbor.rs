//! This module implements conversions from [`serde_cbor::Value`].
//!
//! Validation of data requires converting data to a generic type [`Value`].

use crate::context::{BasicContext, Context};
use crate::flatten::flatten_from_str;
use crate::ivt::Node;
use crate::util::{ValidateError, ValidateResult};
use crate::validate::validate;
use crate::value::Value;
use serde_cbor::Value as CBOR_Value;

// These conversions seem obvious and pointless, but over time they may
// diverge.  However, CDDL and CBOR were designed to work with one another, so
// it's not surprising that they map almost perfectly.
impl From<&CBOR_Value> for Value {
    fn from(value: &CBOR_Value) -> Value {
        match value {
            CBOR_Value::Null => Value::Null,
            CBOR_Value::Bool(b) => Value::Bool(*b),
            CBOR_Value::Integer(i) => Value::Integer(*i),
            CBOR_Value::Float(f) => Value::from_float(*f),
            CBOR_Value::Bytes(b) => Value::Bytes(b.clone()),
            CBOR_Value::Text(t) => Value::Text(t.clone()),
            CBOR_Value::Array(a) => {
                let array = a.iter().map(|v| Value::from(v)).collect();
                Value::Array(array)
            }
            CBOR_Value::Map(m) => {
                let map = m
                    .iter()
                    .map(|(k, v)| (Value::from(k), Value::from(v)))
                    .collect();
                Value::Map(map)
            }
            _ => panic!("can't handle hidden cbor Value"),
        }
    }
}

// A variant that consumes the CBOR Value.
impl From<CBOR_Value> for Value {
    fn from(value: CBOR_Value) -> Value {
        Value::from(&value)
    }
}

/// Validate already-parsed CBOR data against an already-parsed CDDL schema.
pub fn validate_cbor(node: &Node, value: &CBOR_Value, ctx: &dyn Context) -> ValidateResult {
    let value = Value::from(value);
    validate(&value, node, ctx)
}

/// Validate CBOR-encoded data against a specified rule in a UTF-8 CDDL schema.
pub fn validate_cbor_bytes(name: &str, cddl: &str, cbor: &[u8]) -> ValidateResult {
    // Parse the CDDL text and flatten it into IVT form.
    let flat_cddl = flatten_from_str(cddl)?;
    let ctx = BasicContext::new(flat_cddl);

    // Find the rule name that was requested
    let rule_node: &Node = ctx.rules.get(name).ok_or_else(|| {
        let msg = format!("rule/group lookup failure: {}", name);
        ValidateError::Oops(msg)
    })?;

    // Deserialize the CBOR bytes
    let cbor_value: CBOR_Value = serde_cbor::from_slice(cbor).map_err(|e| {
        let msg = format!("cbor parsing failed: {}", e);
        ValidateError::Oops(msg)
    })?;

    // Convert the CBOR tree into a Value tree for validation
    let value = Value::from(cbor_value);
    validate(&value, rule_node, &ctx)
}
