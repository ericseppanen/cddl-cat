//! This module implements conversions from [`serde_cbor::Value`].
//!
//! Validation of data requires converting data to a generic type [`Value`]

use crate::context::{Context, BasicContext};
use crate::value::Value;
use crate::util::{ValidateError, ValidateResult};
use crate::flatten::flatten_from_str;
use crate::ivt::Node;
use crate::validate::validate;
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
            _ => panic!("can't handle hidden cbor Value")
        }
    }
}

// A variant that consumes the CBOR Value.
impl From<CBOR_Value> for Value {
    fn from(value: CBOR_Value) -> Value {
        Value::from(&value)
    }
}

pub fn validate_cbor(value: &CBOR_Value, node: &Node, ctx: &dyn Context) -> ValidateResult {
    let value = Value::from(value);
    validate(&value, node, ctx)
}

/// Validate CBOR-encoded data against a specified rule in a text CDDL schema.
pub fn validate_cbor_cddl_named(name: &str, cddl: &str, cbor: &[u8]) -> ValidateResult {
    // Parse the CDDL text and flatten it into IVT form.
    let flat_cddl = flatten_from_str(cddl)?;
    let ctx = BasicContext::new(flat_cddl);

    let rule_node: &Node = ctx.rules.get(name).ok_or_else(|| {
        let msg = format!("rule/group lookup failure: {}", name);
        ValidateError::Oops(msg)
    })?;

    let cbor_value: CBOR_Value = serde_cbor::from_slice(cbor).map_err(|e| {
        let msg = format!("cbor parsing failed: {}", e);
        ValidateError::Oops(msg)
    })?;
    let value = Value::from(cbor_value);

    validate(&value, rule_node, &ctx)
}

// Validate CBOR against a CDDL description.
pub fn validate_cbor_cddl(cddl: &str, cbor: &[u8]) -> ValidateResult {
    // Parse the CDDL text and flatten it into IVT form.
    let flat_cddl = flatten_from_str(cddl)?;
    let ctx = BasicContext::new(flat_cddl);

    let cbor_value: CBOR_Value = serde_cbor::from_slice(cbor).map_err(|e| {
        let msg = format!("cbor parsing failed: {}", e);
        ValidateError::Oops(msg)
    })?;
    let value = Value::from(cbor_value);

    // FIXME: We stored rules in a BTreeMap, which caused us to lose access to their original
    // ordering!
    // For now, just grab the first rule we find.  We'll be wrong some of the time,
    // but we'll fix that in a moment.
    let rule_node: &Node = ctx.rules.values().next().unwrap();

    validate(&value, rule_node, &ctx)
}
