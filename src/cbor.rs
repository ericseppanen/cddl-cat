//! This module contains code to validate CBOR data.
//!
//! More precisely, it validates `serde_cbor::Value` trees.

use crate::flatten::flatten_from_str;
use crate::generic;
use crate::ivt::*;
use crate::util::*;
use serde_cbor::Value;
use std::cell::RefCell;
use std::collections::BTreeMap; // used in serde_cbor Value::Map

type ValueMap = BTreeMap<Value, Value>;

/// This struct allows us to maintain a map that is consumed during validation.
struct WorkingMap {
    map: RefCell<ValueMap>,
}

impl WorkingMap {
    /// Makes a copy of an existing map's table.
    fn new(value_map: &ValueMap) -> WorkingMap {
        WorkingMap {
            map: RefCell::new(value_map.clone()),
        }
    }
}

/// The main entry point for validating CBOR data against an IVT.
pub fn validate_cbor(node: &Node, value: &Value) -> ValidateResult {
    value.validate(node)
}

// Validate CBOR against a CDDL description.
pub fn validate_cbor_cddl(cddl: &str, cbor: &[u8]) -> ValidateResult {
    // Parse the CDDL text and flatten it into IVT form.
    let flat_cddl = flatten_from_str(cddl)?;
    let cbor_value: Value = serde_cbor::from_slice(cbor).map_err(|e| {
        let msg = format!("cbor parsing failed: {}", e);
        ValidateError::Oops(msg)
    })?;

    // FIXME: allow selection of which CDDL rule to validate against
    // FIXME: We stored rules in a BTreeMap, which caused us to lose access to their original
    // ordering!
    // For now, just grab the first rule we find.  We'll be wrong some of the time,
    // but we'll fix that in a moment.
    let rule_node: &Node = flat_cddl.values().next().unwrap();

    validate_cbor(rule_node, &cbor_value)
}

impl Validate<()> for Value {
    // This is the main validation dispatch function.
    // It tries to match a Node and a Value, recursing as needed.
    fn validate(&self, node: &Node) -> ValidateResult {
        let value = self;
        match node {
            Node::Literal(l) => validate_literal(l, value),
            Node::PreludeType(p) => validate_prelude_type(p, value),
            Node::Choice(c) => generic::validate_choice(c, value),
            Node::Map(m) => validate_map(m, value),
            Node::ArrayRecord(_) => unimplemented!(),
            Node::ArrayVec(_) => unimplemented!(),
            Node::Rule(r) => generic::validate_rule(r, value),
        }
    }
}

impl Validate<Value> for WorkingMap {
    // Dispatch for handling Map key types.  This is specialized because
    // some keys (literal values) can be found with a fast search, while
    // others may require a linear search.
    fn validate(&self, node: &Node) -> TempResult<Value> {
        let value_map = self;
        match node {
            Node::Literal(l) => map_search_literal(l, value_map),
            _ => map_search(node, value_map),
        }
    }
}

/// Create a `Value` from a `Literal`.
impl From<&Literal> for Value {
    fn from(l: &Literal) -> Value {
        match l {
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Int(i) => Value::Integer(*i),
            Literal::Float(f) => Value::Float(*f),
            Literal::Text(t) => Value::Text(t.clone()),
        }
    }
}

fn validate_literal(literal: &Literal, value: &Value) -> ValidateResult {
    if *value == Value::from(literal) {
        return Ok(());
    }
    make_oops("failed validate_literal")
}

fn map_search_literal(literal: &Literal, working_map: &WorkingMap) -> TempResult<Value> {
    // Find a key in the working map, looking for a match.
    // If we find one, remove that key.
    let mut working_mut = working_map.map.borrow_mut();
    let search_key = Value::from(literal);
    match working_mut.remove(&search_key) {
        Some(val) => {
            // We found the key, and removed it from the working map.
            // This means validation was successful.
            Ok(val)
        }
        None => {
            // We didn't find the key; return an error
            make_oops("failed map_search_literal")
        }
    }
}

fn map_search(node: &Node, working_map: &WorkingMap) -> TempResult<Value> {
    // Iterate over each key in the working map, looking for a match.
    // If we find one, remove that key.
    // This is less efficient than map_search_literal.
    // Note: we remove the key before learning whether the key's value
    // has matched.  So we can't support non-"cut" semantics like:
    // { * tstr => int, * tstr => tstr }
    // See rfc8610 section 3.5.4 for a longer explanation of "cuts".

    let mut working_mut = working_map.map.borrow_mut();
    for key in working_mut.keys() {
        let attempt = key.validate(node);
        match attempt {
            Ok(_) => {
                // This key matches the node.  Remove the key and return success.
                // Some juggling is required to satisfy the borrow checker.
                let key2 = key.clone();
                drop(key);
                let val = working_mut.remove(&key2).unwrap();
                return Ok(val);
            }
            Err(_) => {
                // This key didn't match, but maybe another one will
            }
        }
    }
    // We searched all the keys without finding a match.  Validation fails.
    make_oops("failed map_search")
}

fn validate_prelude_type(ty: &PreludeType, value: &Value) -> ValidateResult {
    match (ty, value) {
        (PreludeType::Int, Value::Integer(_)) => Ok(()),
        (PreludeType::Int, _) => make_oops("bad int"),
        (PreludeType::Tstr, Value::Text(_)) => Ok(()),
        (PreludeType::Tstr, _) => make_oops("bad tstr"),
        _ => unimplemented!(),
    }
}

fn validate_map(m: &Map, value: &Value) -> ValidateResult {
    match value {
        Value::Map(vm) => validate_map_part2(m, vm),
        _ => make_oops("expected map, found not-a-map"),
    }
}

fn validate_map_part2(m: &Map, value_map: &ValueMap) -> ValidateResult {
    // Strategy for validating a map:
    // 1. We assume that the code that constructed the IVT Map placed the keys
    //    in matching order (specific heads first, more general types at the end)
    //    so that we consume IVT Map keys in order without worrying about non-
    //    deterministic results.
    // 2. Make a mutable working copy of the Value::Map contents
    // 3. Iterate over the IVT Map, searching the Value::Map for a matching key.
    // 4. If a match is found, remove the key-value pair from our working copy.
    // 5. Validate the key's corresponding value.
    // 6. TODO: If the key can consume multiple values, repeat the search for this key.
    // 7. TODO: If the key is not found and the key is optional, continue to the next key.
    // 8. TODO: If the key is not found and the key is not optional, return an error.

    let working_map = WorkingMap::new(value_map);

    for kv in &m.members {
        let key_node = kv.key.as_ref();
        // Validating a key has the side-effect of removing that key from
        // the working map.
        // If we fail to validate a key, exit now with an error.
        // TODO: handle occurrences
        let extracted_val = working_map.validate(key_node)?;
        let val_node = kv.value.as_ref();
        // If we fail to validate a value, exit with an error.
        extracted_val.validate(val_node)?;
    }
    if working_map.map.into_inner().is_empty() {
        Ok(())
    } else {
        // If the working map isn't empty, that means we had some extra values
        // that didn't match anything.
        make_oops("dangling map values")
    }
}
