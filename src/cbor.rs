//! This module implements validation from [`serde_cbor::Value`].
//!
//! # Examples
//!
//! ```
//! use cddl_cat::validate_cbor_bytes;
//! use serde::Serialize;
//!
//! #[derive(Serialize)]
//! struct PersonStruct {
//!     name: String,
//!     age: u32,
//! }
//!
//! let input = PersonStruct {
//!     name: "Bob".to_string(),
//!     age: 43,
//! };
//! let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
//! let cddl_input = "person = {name: tstr, age: int}";
//!
//! validate_cbor_bytes("person", cddl_input, &cbor_bytes).unwrap();
//! ```
//!
//! If the caller wants to reuse the parsed CDDL IVT, replace
//! `validate_cbor_bytes(...)` with:
//! ```
//! # use cddl_cat::validate_cbor_bytes;
//! use cddl_cat::{cbor::validate_cbor, context::BasicContext, flatten::flatten_from_str};
//! # use serde::Serialize;
//! #
//! # #[derive(Serialize)]
//! # struct PersonStruct {
//! #     name: String,
//! #     age: u32,
//! # }
//! #
//! # let input = PersonStruct {
//! #     name: "Bob".to_string(),
//! #     age: 43,
//! # };
//! # let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
//! # let cddl_input = "person = {name: tstr, age: int}";
//!
//! // Parse the CDDL text and flatten it into IVT form.
//! let flat_cddl = flatten_from_str(cddl_input).unwrap();
//! // Create a Context object to store the IVT
//! let ctx = BasicContext::new(flat_cddl);
//! // Look up the Rule we want to validate.
//! let rule_def = &ctx.rules.get("person").unwrap();
//! // Deserialize the CBOR bytes
//! let cbor_value = serde_cbor::from_slice(&cbor_bytes).unwrap();
//! // Perform the validation.
//! validate_cbor(&rule_def, &cbor_value, &ctx).unwrap();
//! ```

#![cfg(feature = "serde_cbor")]

use crate::context::{BasicContext, LookupContext};
use crate::flatten::flatten_from_str;
use crate::ivt::RuleDef;
use crate::util::{ValidateError, ValidateResult};
use crate::validate::do_validate;
use crate::value::Value;
use serde_cbor::Value as CBOR_Value;
use std::collections::BTreeMap;
use std::convert::TryFrom;

// These conversions seem obvious and pointless, but over time they may
// diverge.  However, CDDL and CBOR were designed to work with one another, so
// it's not surprising that they map almost perfectly.

impl TryFrom<&CBOR_Value> for Value {
    type Error = ValidateError;

    fn try_from(value: &CBOR_Value) -> Result<Self, Self::Error> {
        let result = match value {
            CBOR_Value::Null => Value::Null,
            CBOR_Value::Bool(b) => Value::Bool(*b),
            CBOR_Value::Integer(i) => Value::Integer(*i),
            CBOR_Value::Float(f) => Value::from_float(*f),
            CBOR_Value::Bytes(b) => Value::Bytes(b.clone()),
            CBOR_Value::Text(t) => Value::Text(t.clone()),
            CBOR_Value::Array(a) => {
                let array: Result<_, _> = a.iter().map(Value::try_from).collect();
                Value::Array(array?)
            }
            CBOR_Value::Map(m) => {
                type MapTree = BTreeMap<Value, Value>;
                let map: Result<MapTree, _> = m
                    .iter()
                    .map(|(k, v)| {
                        // An iterator returning a 2-tuple can be used as (key, value)
                        // when building a new map.
                        Ok((Value::try_from(k)?, Value::try_from(v)?))
                    })
                    .collect();
                Value::Map(map?)
            }
            _ => {
                // cbor::Value has a few hidden internal variants.  We should
                // never see them, but return an error if we do.
                return Err(ValidateError::ValueError(
                    "can't handle hidden cbor Value".into(),
                ));
            }
        };
        Ok(result)
    }
}

// A variant that consumes the CBOR Value.
impl TryFrom<CBOR_Value> for Value {
    type Error = ValidateError;

    fn try_from(value: CBOR_Value) -> Result<Self, Self::Error> {
        Value::try_from(&value)
    }
}

/// Validate already-parsed CBOR data against an already-parsed CDDL schema.
pub fn validate_cbor(
    rule_def: &RuleDef,
    value: &CBOR_Value,
    ctx: &dyn LookupContext,
) -> ValidateResult {
    let value = Value::try_from(value)?;
    do_validate(&value, rule_def, ctx)
}

/// Validate CBOR-encoded data against a specified rule in a UTF-8 CDDL schema.
pub fn validate_cbor_bytes(name: &str, cddl: &str, cbor: &[u8]) -> ValidateResult {
    // Parse the CDDL text and flatten it into IVT form.
    let flat_cddl = flatten_from_str(cddl)?;
    let ctx = BasicContext::new(flat_cddl);

    // Find the rule name that was requested
    let rule_def: &RuleDef = ctx
        .rules
        .get(name)
        .ok_or_else(|| ValidateError::MissingRule(name.into()))?;

    // Deserialize the CBOR bytes
    let cbor_value: CBOR_Value =
        serde_cbor::from_slice(cbor).map_err(|e| ValidateError::ValueError(format!("{}", e)))?;

    // Convert the CBOR tree into a Value tree for validation
    let value = Value::try_from(cbor_value)?;
    do_validate(&value, rule_def, &ctx)
}
