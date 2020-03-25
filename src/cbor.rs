use crate::ivt::*;
use serde_cbor::{self, Value};
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

fn make_oops(msg: &str) -> ValidateResult {
    Err(ValidateError::Oops(msg.into()))
}

impl Validate for Value {
    // This is the main validation dispatch function.
    // It tries to match a Node and a Value, recursing as needed.
    fn validate(&self, node: &Node) -> ValidateResult {
        let value = self;
        match node {
            Node::Literal(_) => unimplemented!(),
            Node::PreludeType(p) => validate_prelude_type(p, value),
            Node::Choice(c) => validate_choice(c, value),
            Node::Map(m) => validate_map(m, value),
            Node::ArrayRecord(_) => unimplemented!(),
            Node::ArrayVec(_) => unimplemented!(),
            Node::Rule(_) => unreachable!(), // Should never happen
        }
    }
}

impl Validate for WorkingMap {
    // Dispatch for handling Map key types.  This is specialized because
    // some keys (literal values) can be found with a fast search, while
    // others may require a linear search.
    fn validate(&self, node: &Node) -> ValidateResult {
        let value_map = self;
        match node {
            Node::Literal(l) => map_search_literal(l, value_map),
            _ => map_search(node, value_map),
        }
    }
}

impl From<&Literal> for Value {
    fn from(l: &Literal) -> Value {
        match l {
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Int(i) => Value::Integer(*i),
            Literal::Float(f) => Value::Float(*f),
        }
    }
}

fn map_search_literal(literal: &Literal, working_map: &WorkingMap) -> ValidateResult {
    // Find a key in the working map, looking for a match.
    // If we find one, remove that key.
    let mut working_mut = working_map.map.borrow_mut();
    let search_key = Value::from(literal);
    match working_mut.remove(&search_key) {
        Some(_) => {
            // We found the key, and removed it from the working map.
            // This means validation was successful.
            Ok(())
        },
        None => {
            // We didn't find the key; return an error
            make_oops("failed map_search_literal")
        }
    }
}

fn map_search(node: &Node, working_map: &WorkingMap) -> ValidateResult {
    // Iterate over each key in the working map, looking for a match.
    // If we find one, remove that key.
    // This is less efficient than map_search_literal.
    let mut working_mut = working_map.map.borrow_mut();
    for key in working_mut.keys() {
        let attempt = key.validate(node);
        match attempt {
            Ok(_) => {
                // This key matches the node.  Remove the key and return success.
                // Some juggling is required to satisfy the borrow checker.
                let key2 = key.clone();
                drop(key);
                working_mut.remove(&key2);
                return Ok(());
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
        _ => unimplemented!(),
    }
}

fn validate_map(m: &Map, value: &Value) -> ValidateResult {
    match value {
        Value::Map(vm) => validate_map_part2(m, vm),
        _ => make_oops("expected map, found not-a-map"),
    }
}

fn exact_key_search(target: &Literal, value_map: &ValueMap) -> bool {
    false
}

fn validate_map_part2(m: &Map, value_map: &ValueMap) -> ValidateResult {
    // Strategy for validating a map:
    // 1. We assume that the code that constructed the IVT Map placed the keys
    //    in matching order (specific heads first, more general types at the end)
    //    so that we consume IVT Map keys in order without worrying about non-
    //    deterministic results.
    // 2. Make a mutable working copy of the Value::Map contents
    // 3. Iterate over the IVT Map, searching the Value::Map for a matching key.
    // 4. If a matching key is found, validate the key's corresponding value.
    // 5. If a match is found, remove the key-value pair from our working copy.
    // 6. If the key can consume multiple values, repeat the search for this key.
    // 7. If the key is not found and the key is optional, continue to the next key.
    // 8. If the key is not found and the key is not optional, return an error.

    let mut working_map = WorkingMap::new(value_map);

    for kv in &m.members {
        let key = kv.key.as_ref();
        working_map.validate(key)?;
        //let value = kv.value;
        // Validate the key and then the value

        //kv.key, kv.value
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn validate_int() {
        let node: Node = PreludeType::Int.into();
        let value = Value::Integer(7);
        node.validate(&value).unwrap();

        let value = Value::Text("abc".into());
        node.validate(&value).unwrap_err();
    }
}
