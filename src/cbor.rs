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
use std::collections::VecDeque;

type ValueMap = BTreeMap<Value, Value>;

/// This struct allows us to maintain a map that is consumed during validation.
struct WorkingMap {
    // This only uses RefCell because Rust doesn't allow traits to be generic
    // over mutable-ness.  We want to implement the Validate trait on this
    // struct, but we know it's going to be mutable.  So instead of having
    // a separate ValidateMut trait we accept that this struct will always
    // be treated as immutable and we use interior mutability to actually
    // change things.
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

/// This struct allows us to maintain a copy of an array that is consumed
/// during validation.
struct WorkingArray {
    array: RefCell<VecDeque<Value>>,
}

impl WorkingArray {
    /// Makes a copy of an existing map's table.
    fn new(array: &Vec<Value>) -> WorkingArray {
        let deque: VecDeque<Value> = array.iter().cloned().collect();
        WorkingArray {
            array: RefCell::new(deque),
        }
    }
}

/// The main entry point for validating CBOR data against an IVT.
pub fn validate_cbor(node: &Node, value: &Value) -> ValidateResult {
    value.validate(node)
}

pub fn validate_cbor_cddl_named(name: &str, cddl: &str, cbor: &[u8]) -> ValidateResult {
    // Parse the CDDL text and flatten it into IVT form.
    let flat_cddl = flatten_from_str(cddl)?;

    let rule_node: &Node = flat_cddl.get(name).ok_or_else(|| {
        let msg = format!("rule/group lookup failure: {}", name);
        ValidateError::Oops(msg)
    })?;

    let cbor_value: Value = serde_cbor::from_slice(cbor).map_err(|e| {
        let msg = format!("cbor parsing failed: {}", e);
        ValidateError::Oops(msg)
    })?;

    validate_cbor(rule_node, &cbor_value)
}

// Validate CBOR against a CDDL description.
pub fn validate_cbor_cddl(cddl: &str, cbor: &[u8]) -> ValidateResult {
    // Parse the CDDL text and flatten it into IVT form.
    let flat_cddl = flatten_from_str(cddl)?;
    let cbor_value: Value = serde_cbor::from_slice(cbor).map_err(|e| {
        let msg = format!("cbor parsing failed: {}", e);
        ValidateError::Oops(msg)
    })?;

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
            Node::Array(a) => validate_array(a, value),
            Node::Rule(r) => generic::validate_rule(r, value),
            Node::Group(g) => {
                panic!("validate group {:?}", g);
            }
            Node::KeyValue(_) => unimplemented!(), // FIXME: can this even happen?
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
        (PreludeType::Any, _) => Ok(()),
        (PreludeType::Bool, Value::Bool(_)) => Ok(()),
        (PreludeType::Bool, _) => make_oops("bad int"),
        (PreludeType::Int, Value::Integer(_)) => Ok(()),
        (PreludeType::Int, _) => make_oops("bad int"),
        (PreludeType::Uint, Value::Integer(x)) if *x >= 0 => Ok(()),
        (PreludeType::Uint, _) => make_oops("bad uint"),
        (PreludeType::Tstr, Value::Text(_)) => Ok(()),
        (PreludeType::Tstr, _) => make_oops("bad tstr"),
        _ => unimplemented!(),
    }
}

// FIXME: should this be combined with Map handling?
fn validate_array(ar: &Array, value: &Value) -> ValidateResult {
    match value {
        Value::Array(a) => validate_array_part2(ar, a),
        _ => make_oops("expected array, found not-array"),
    }
}

fn validate_array_part2(ar: &Array, value_array: &Vec<Value>) -> ValidateResult {
    // Strategy for validating an array:
    // 1. We assume that the code that constructed the IVT Array placed the
    //    members in matching order (literals first, more general types at the
    //    end) so that we consume IVT Array members in order without worrying
    //    about non-deterministic results.
    // 2. Make a mutable working copy of the Value::Array contents
    // 3. Iterate over the IVT Array, searching the working copy for a
    //    matching key.
    // 4. If a match is found, remove the value from our working copy.
    // 6. If the IVT member can consume multiple values, repeat the search for
    //    this key.
    // 7. If a match is not found and the member is optional (or we've already
    //    consumed an acceptable number of keys), continue to the next IVT
    //    member.
    // 8. If the member is not found and we haven't consumed the expected
    //    number of values, return an error.

    let mut working_array = WorkingArray::new(value_array);

    for member in &ar.members {
        validate_array_member(member, &mut working_array)?;
    }
    if working_array.array.into_inner().is_empty() {
        Ok(())
    } else {
        // If the working map isn't empty, that means we had some extra values
        // that didn't match anything.
        make_oops("dangling array values")
    }
}

fn validate_array_member(member: &ArcNode, working_array: &mut WorkingArray) -> ValidateResult {
    match member.as_ref() {
        // FIXME: does it make sense for this to destructure & dispatch
        // each Node type here?  Is there any way to make this generic?
        Node::KeyValue(kv) => {
            // The key is ignored.  Validate the value only.
            // FIXME: should we try to use the key to provide a more
            // useful error message?
            validate_array_value(&kv.value, working_array)
        }
        Node::Rule(r) => {
            // FIXME: This seems like a gross hack.  We need to dereference
            // the rule here, because if we drop to the bottom and call
            // validate_array_value() then we lose our ability to "see
            // through" KeyValue and Group nodes while remembering that we are
            // in an array context (with a particular working copy).
            // BUG: Choice nodes will have the same problem.
            let next_node = &r.get_ref().unwrap();
            validate_array_member(next_node, working_array)
        }
        Node::Group(g) => {
            // Recurse into each member of the group.
            for group_member in &g.members {
                // Exit early if we hit an error.
                validate_array_member(group_member, working_array)?;
            }
            // All group members validated Ok.
            Ok(())
        }
        m => validate_array_value(m, working_array),
    }
}

/// Validate a key-value pair against a mutable working array.
fn validate_array_value(node: &Node, working_array: &mut WorkingArray) -> ValidateResult {
    let mut count: usize = 0;

    // FIXME: oops, we were previously consuming occur from the KeyValue type.
    // We need to plumb that through somehow.
    let occur = Occur { lower: 1, upper: 1 };

    loop {
        // Try to match this node against the head of the working array.
        // A successful match has the side-effect of removing that value from
        // the working array.
        // If we fail to validate a node, exit now with an error.

        match working_array.array.borrow().front() {
            Some(val) => match val.validate(node) {
                Ok(_) => (),
                Err(_) => break,
            },
            None => break,
        }

        // We had a successful match; remove the matched value.
        working_array.array.borrow_mut().pop_front();
        count += 1;

        if count >= occur.upper {
            // Stop matching; we've consumed the maximum number of this key.
            break;
        }
    }
    if count < occur.lower {
        return make_oops(&format!("failure to find array value"));
    }
    Ok(())
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
    //    in matching order (literals first, more general types at the end) so
    //    that we consume IVT Map keys in order without worrying about non-
    //    deterministic results.
    // 2. Make a mutable working copy of the Value::Map contents
    // 3. Iterate over the IVT Map, searching the Value::Map for a matching key.
    // 4. If a match is found, remove the key-value pair from our working copy.
    // 5. Validate the key's corresponding value.
    // 6. If the key can consume multiple values, repeat the search for this key.
    // 7. If the key is not found and the key is optional (or we've already consumed
    //    an acceptable number of keys), continue to the next key.
    // 8. If the key is not found and we haven't consumed the expected number of
    //    keys, return an error.

    let mut working_map = WorkingMap::new(value_map);

    for member in &m.members {
        validate_map_member(member, &mut working_map)?;
    }
    if working_map.map.into_inner().is_empty() {
        Ok(())
    } else {
        // If the working map isn't empty, that means we had some extra values
        // that didn't match anything.
        make_oops("dangling map values")
    }
}

fn validate_map_member(member: &ArcNode, working_map: &mut WorkingMap) -> ValidateResult {
    match member.as_ref() {
        // FIXME: does it make sense for this to destructure & dispatch
        // each Node type here?  Is there any way to make this generic?
        Node::KeyValue(kv) => validate_map_keyvalue(kv, working_map),
        Node::Rule(r) => {
            // NOTE!  validate() on a WorkingMap returns a Value (the result of a key-value lookup)
            //
            // So we can't use generic::validate() here.  We need to punch down
            // a level into the rule and match again.
            let next_node = &r.get_ref().unwrap();
            validate_map_member(next_node, working_map)
        }
        Node::Group(g) => {
            // Recurse into each member of the group.
            for group_member in &g.members {
                // Exit early if we hit an error.
                validate_map_member(group_member, working_map)?;
            }
            // All group members validated Ok.
            Ok(())
        }
        _ => panic!("unhandled map member {:?}", member),
    }
}

/// Validate a key-value pair against a mutable working map.
fn validate_map_keyvalue(kv: &KeyValue, working_map: &mut WorkingMap) -> ValidateResult {
    let key_node = kv.key.as_ref();
    let val_node = kv.value.as_ref();
    let mut count: usize = 0;

    loop {
        // Walk the working map, trying to match this key.
        // A successful key match has the side-effect of removing that key from
        // the working map (returning the value to us.)
        // If we fail to validate a key, exit now with an error.
        let extracted_val;
        match working_map.validate(key_node) {
            Ok(v) => extracted_val = v,
            Err(_) => break,
        }
        // If we fail to validate a value, exit with an error.
        match extracted_val.validate(val_node) {
            Ok(_) => {
                count += 1;
                if count >= kv.occur.upper {
                    // Stop matching; we've consumed the maximum number of this key.
                    break;
                }
            }
            Err(_) => break,
        }
    }
    if count < kv.occur.lower {
        return make_oops(&format!("failure to find key-value pair"));
    }
    Ok(())
}
