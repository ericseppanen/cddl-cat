//! This module contains code to validate serialized data.
//!
//! More precisely, it validates data that can be represented by [`Value`] trees.

use crate::context::Context;
use crate::ivt::*;
use crate::util::{mismatch, ValidateError, ValidateResult};
use crate::value::Value;
use std::collections::BTreeMap; // used in Value::Map
use std::collections::VecDeque;
use std::mem::discriminant;

type ValueMap = BTreeMap<Value, Value>;

// A Result that returns some temporary value.
type TempResult<T> = Result<T, ValidateError>;

/// This struct allows us to maintain a map that is consumed during validation.
struct WorkingMap {
    map: ValueMap,
}

impl WorkingMap {
    /// Makes a copy of an existing map's table.
    fn new(value_map: &ValueMap) -> WorkingMap {
        WorkingMap {
            map: value_map.clone(),
        }
    }
}

/// This struct allows us to maintain a copy of an array that is consumed
/// during validation.
#[derive(Debug)]
struct WorkingArray {
    // The elements in the Value Array
    array: VecDeque<Value>,
    // A stack of lists; each list contains maybe-discarded elements.
    snaps: VecDeque<VecDeque<Value>>,
}

impl WorkingArray {
    /// Makes a copy of an existing map's table.
    fn new(array: &[Value]) -> WorkingArray {
        let deque: VecDeque<Value> = array.iter().cloned().collect();
        WorkingArray {
            array: deque,
            snaps: VecDeque::new(),
        }
    }

    // When we start speculatively matching array elements (e.g. in a Choice
    // or Occur), we may fail the match partway through, and need to rewind
    // to the most recent snapshot.
    //
    // It's possible for nested snapshots to exist; for example if we have a
    // group-of-choices nested inside a group-of-choices.
    //
    // If one array is nested inside another, the inner array will get its
    // own WorkingArray so snapshots aren't necessary in that case.
    fn snapshot(&mut self) {
        let new_snap: VecDeque<Value> = VecDeque::new();
        self.snaps.push_back(new_snap);
    }

    // Restore the array to the point when we last called snapshot()
    fn rewind(&mut self) {
        // If validate code is implemented correctly, then unwrap() should
        // never panic.
        let mut top_snap = self.snaps.pop_back().unwrap();
        // drain the elements in LIFO order, and push them back into
        // the working array.
        for element in top_snap.drain(..).rev() {
            self.array.push_front(element);
        }
    }

    // We completed a match, so we can retire the most recent snapshot.
    fn commit(&mut self) {
        // If validate code is implemented correctly, then unwrap() should
        // never panic.
        // This throws away the value that was popped; those values were
        // successfully matched and are no longer needed.
        self.snaps.pop_back().unwrap();
    }

    // Peek at the front of the working array.
    fn peek_front(&self) -> Option<&Value>{
        self.array.front()
    }

    // Remove an element from the working array.
    // If there is an active snapshot, stash the element there until we're
    // certain we've matched the entire group.
    fn pop_front(&mut self) {
        // If validate code is implemented correctly, then unwrap() should
        // never panic (we've already peeked at this value in order to match
        // it.)
        let element = self.array.pop_front().unwrap();
        match self.snaps.back_mut() {
            Some(snap) => {
                snap.push_back(element);
            }
            None => {
                // Nothing to do if there's no snapshot;
                // just discard the element.
            }
        }
    }
}

// Prevent warnings if both serde_cbor and serde_json are disabled.
#[allow(dead_code)]

// This is the main validation dispatch function.
// It tries to match a Node and a Value, recursing as needed.
pub(crate) fn validate(value: &Value, node: &Node, ctx: &dyn Context) -> ValidateResult {
    match node {
        Node::Literal(l) => validate_literal(l, value),
        Node::PreludeType(p) => validate_prelude_type(*p, value),
        Node::Choice(c) => validate_choice(c, value, ctx),
        Node::Map(m) => validate_map(m, value, ctx),
        Node::Array(a) => validate_array(a, value, ctx),
        Node::Rule(r) => validate_rule(r, value, ctx),
        Node::Group(g) => validate_standalone_group(g, value, ctx),
        Node::KeyValue(_) => Err(ValidateError::Structural("unexpected KeyValue".into())),
        Node::Occur(_) => Err(ValidateError::Structural("unexpected Occur".into())),
        Node::Unwrap(_) => Err(ValidateError::Structural("unexpected Unwrap".into())),
        Node::Range(r) => validate_range(r, value, ctx),
    }
}

// Perform map key search.
// Some keys (literal values) can be found with a fast search, while
// others may require a linear search.
fn validate_workingmap(
    value_map: &mut WorkingMap,
    node: &Node,
    ctx: &dyn Context,
) -> TempResult<Value> {
    match node {
        Node::Literal(l) => map_search_literal(l, value_map),
        _ => map_search(node, value_map, ctx),
    }
}

/// Validate a `Choice` containing an arbitrary number of "option" nodes.
///
/// If any of the options matches, this validation is successful.
pub fn validate_choice(choice: &Choice, value: &Value, ctx: &dyn Context) -> ValidateResult {
    for node in &choice.options {
        if let Ok(()) = validate(value, node, ctx) {
            return Ok(());
        }
    }
    let expected = format!("choice of {}", choice.options.len());
    Err(mismatch(expected))
}

/// Validate a `Rule` reference
///
/// This just falls through to the referenced `Node`.
pub fn validate_rule(rule: &Rule, value: &Value, ctx: &dyn Context) -> ValidateResult {
    let node = ctx.lookup_rule(&rule.name)?;
    validate(value, &node, ctx)
}

/// Create a `Value` from a `Literal`.
impl From<&Literal> for Value {
    fn from(l: &Literal) -> Value {
        match l {
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Int(i) => Value::Integer(*i),
            Literal::Float(f) => Value::from_float(*f),
            Literal::Text(t) => Value::Text(t.clone()),
            Literal::Bytes(b) => Value::Bytes(b.clone()),
        }
    }
}

fn validate_literal(literal: &Literal, value: &Value) -> ValidateResult {
    if *value == Value::from(literal) {
        return Ok(());
    }
    Err(mismatch(format!("{}", literal)))
}

fn map_search_literal(literal: &Literal, working_map: &mut WorkingMap) -> TempResult<Value> {
    // Find a key in the working map, looking for a match.
    // If we find one, remove that key.
    let search_key = Value::from(literal);
    match working_map.map.remove(&search_key) {
        Some(val) => {
            // We found the key, and removed it from the working map.
            // This means validation was successful.
            Ok(val)
        }
        None => {
            // We didn't find the key; return an error
            Err(mismatch(format!("map{{{}}}", literal)))
        }
    }
}

fn map_search(node: &Node, working_map: &mut WorkingMap, ctx: &dyn Context) -> TempResult<Value> {
    // Iterate over each key in the working map, looking for a match.
    // If we find one, remove that key.
    // This is less efficient than map_search_literal.
    // Note: we remove the key before learning whether the key's value
    // has matched.  So we can't support non-"cut" semantics like:
    // { * tstr => int, * tstr => tstr }
    // See rfc8610 section 3.5.4 for a longer explanation of "cuts".

    for key in working_map.map.keys() {
        let attempt = validate(key, node, ctx);
        if attempt.is_ok() {
            // This key matches the node.  Remove the key and return success.
            // Some juggling is required to satisfy the borrow checker.
            let key2: Value = key.clone();
            let val = working_map.map.remove(&key2).unwrap();
            return Ok(val);
        }
    }
    // We searched all the keys without finding a match.  Validation fails.
    // FIXME: node could expand to a giant string.  Need some kind of helper fn?
    Err(mismatch(format!("map{{{}}}", node)))
}

// Note `ty` is passed by value because clippy says it's only 1 byte.
fn validate_prelude_type(ty: PreludeType, value: &Value) -> ValidateResult {
    match (ty, value) {
        (PreludeType::Any, _) => Ok(()),
        (PreludeType::Nil, Value::Null) => Ok(()),
        (PreludeType::Nil, _) => Err(mismatch("nil")),
        (PreludeType::Bool, Value::Bool(_)) => Ok(()),
        (PreludeType::Bool, _) => Err(mismatch("bool")),
        (PreludeType::Int, Value::Integer(_)) => Ok(()),
        (PreludeType::Int, _) => Err(mismatch("int")),
        (PreludeType::Uint, Value::Integer(x)) if *x >= 0 => Ok(()),
        (PreludeType::Uint, _) => Err(mismatch("uint")),
        (PreludeType::Nint, Value::Integer(x)) if *x < 0 => Ok(()),
        (PreludeType::Nint, _) => Err(mismatch("nint")),
        (PreludeType::Float, Value::Float(_)) => Ok(()),
        (PreludeType::Float, _) => Err(mismatch("float")),
        (PreludeType::Tstr, Value::Text(_)) => Ok(()),
        (PreludeType::Tstr, _) => Err(mismatch("tstr")),
        (PreludeType::Bstr, Value::Bytes(_)) => Ok(()),
        (PreludeType::Bstr, _) => Err(mismatch("bstr")),
    }
}

// FIXME: should this be combined with Map handling?
fn validate_array(ar: &Array, value: &Value, ctx: &dyn Context) -> ValidateResult {
    match value {
        Value::Array(a) => validate_array_part2(ar, a, ctx),
        _ => Err(mismatch("array")),
    }
}

fn validate_array_part2(ar: &Array, value_array: &[Value], ctx: &dyn Context) -> ValidateResult {
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
        validate_array_member(member, &mut working_array, ctx)?;
    }
    if working_array.array.is_empty() {
        Ok(())
    } else {
        // If the working map isn't empty, that means we had some extra values
        // that didn't match anything.
        // FIXME: Should this be a unique error type?
        Err(mismatch("shorter array"))
    }
}

fn validate_array_member(
    member: &Node,
    working_array: &mut WorkingArray,
    ctx: &dyn Context,
) -> ValidateResult {
    match member {
        // FIXME: does it make sense for this to destructure & dispatch
        // each Node type here?  Is there any way to make this generic?
        Node::Occur(o) => validate_array_occur(o, working_array, ctx),
        Node::KeyValue(kv) => {
            // The key is ignored.  Validate the value only.
            // FIXME: should we try to use the key to provide a more
            // useful error message?
            validate_array_value(&kv.value, working_array, ctx)
        }
        Node::Rule(r) => {
            // FIXME: This seems like a gross hack.  We need to dereference
            // the rule here, because if we drop to the bottom and call
            // validate_array_value() then we lose our ability to "see
            // through" KeyValue and Group nodes while remembering that we are
            // in an array context (with a particular working copy).
            // BUG: Choice nodes will have the same problem.
            let next_node = ctx.lookup_rule(&r.name)?;
            validate_array_member(next_node, working_array, ctx)
        }
        Node::Unwrap(r) => {
            // Like Rule, we are dereferencing the Rule by hand here so that
            // we can "see through" to the underlying data without forgetting
            // we were in an array context.
            let node = ctx.lookup_rule(&r.name)?;
            validate_array_unwrap(node, working_array, ctx)
        }
        Node::Choice(c) => {
            // We need to explore each of the possible choices.
            // We can't use validate_array_value() because we'll lose our
            // array context.
            for option in &c.options {
                if let Ok(()) = validate_array_member(option, working_array, ctx) {
                    return Ok(());
                }
            }
            // None of the choices worked.
            let expected = format!("choice of {}", c.options.len());
            Err(mismatch(expected))
        }
        Node::Group(g) => {
            // As we call validate_array_member, we don't know how many items
            // it might speculatively pop from the list.  So we'll take a snapshot
            // now and commit our changes if we match successfully (and roll them
            // back if it fails).
            working_array.snapshot();

            // Recurse into each member of the group.
            for group_member in &g.members {
                match validate_array_member(group_member, working_array, ctx) {
                    Ok(_) => {
                        // So far so good...
                    }
                    Err(e) => {
                        // Since we failed to validate the entire group, rewind to our most
                        // recent snapshot.  This may put values back into the array,
                        // so they can be matched by whatever we try next (or trigger
                        // an error if they aren't consumed by anything).
                        working_array.rewind();
                        return Err(e);
                    }
                }
            }
            // All group members validated Ok.
            working_array.commit();
            Ok(())
        }
        m => validate_array_value(m, working_array, ctx),
    }
}

fn validate_array_unwrap(
    node: &Node,
    working_array: &mut WorkingArray,
    ctx: &dyn Context,
) -> ValidateResult {
    // After traversing an unwrap from inside an array, the next node must be an
    // Array node too.
    match node {
        Node::Array(a) => {
            // Recurse into each member of the unwrapped array.
            for member in &a.members {
                validate_array_member(member, working_array, ctx)?;
            }
            // All array members validated Ok.
            Ok(())
        }
        _ => Err(mismatch("unwrap array")),
    }
}

/// Validate an occurrence against a mutable working array.
// FIXME: this is pretty similar to validate_map_occur; maybe they can be combined?
fn validate_array_occur(
    occur: &Occur,
    working_array: &mut WorkingArray,
    ctx: &dyn Context,
) -> ValidateResult {
    let (lower_limit, upper_limit) = occur.limits();
    let mut count: usize = 0;

    loop {
        match validate_array_member(&occur.node, working_array, ctx) {
            Ok(_) => (),
            Err(_) => break,
        }
        count += 1;
        if count >= upper_limit {
            // Stop matching; we've consumed the maximum number of this key.
            break;
        }
    }
    if count < lower_limit {
        return Err(mismatch(format!("more array element [{}]", occur)));
    }
    Ok(())
}

/// Validate some node against a mutable working array.
fn validate_array_value(
    node: &Node,
    working_array: &mut WorkingArray,
    ctx: &dyn Context,
) -> ValidateResult {
    match working_array.peek_front() {
        Some(val) => {
            validate(val, node, ctx)?;
            // We had a successful match; remove the matched value.
            working_array.pop_front();
            Ok(())
        }
        None => Err(mismatch(format!("array element {}", node))),
    }
}

fn validate_map(m: &Map, value: &Value, ctx: &dyn Context) -> ValidateResult {
    match value {
        Value::Map(vm) => validate_map_part2(m, vm, ctx),
        _ => Err(mismatch("map")),
    }
}

fn validate_map_part2(m: &Map, value_map: &ValueMap, ctx: &dyn Context) -> ValidateResult {
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
        validate_map_member(member, &mut working_map, ctx)?;
    }
    if working_map.map.is_empty() {
        Ok(())
    } else {
        // If the working map isn't empty, that means we had some extra values
        // that didn't match anything.
        Err(mismatch("shorter map"))
    }
}

fn validate_map_member(
    member: &Node,
    working_map: &mut WorkingMap,
    ctx: &dyn Context,
) -> ValidateResult {
    match member {
        // FIXME: does it make sense for this to destructure & dispatch
        // each Node type here?  Is there any way to make this generic?
        Node::Occur(o) => validate_map_occur(o, working_map, ctx),
        Node::KeyValue(kv) => validate_map_keyvalue(kv, working_map, ctx),
        Node::Rule(r) => {
            // We can't use the generic validate() here; we would forget that
            // we were in a map context.  We need to punch down a level into
            // the rule and match again.
            let next_node = ctx.lookup_rule(&r.name)?;
            validate_map_member(next_node, working_map, ctx)
        }
        Node::Unwrap(r) => {
            // Like Rule, we are dereferencing the Rule by hand here so that
            // we can "see through" to the underlying data without forgetting
            // we were in a map context.
            let node = ctx.lookup_rule(&r.name)?;
            validate_map_unwrap(node, working_map, ctx)
        }
        Node::Group(g) => {
            // Recurse into each member of the group.
            for group_member in &g.members {
                // Exit early if we hit an error.
                validate_map_member(group_member, working_map, ctx)?;
            }
            // All group members validated Ok.
            Ok(())
        }
        Node::Choice(c) => {
            // We need to explore each of the possible choices.
            // FIXME: need to checkpoint the WorkingMap before trying each
            // option, because we may fail the choice and need to back out
            // any changes.  Need a unit test to test this behavior.
            // Recurse into each member of the group.
            for option in &c.options {
                if let Ok(()) = validate_map_member(option, working_map, ctx) {
                    return Ok(());
                }
            }
            // None of the choices worked.
            let expected = format!("choice of {}", c.options.len());
            Err(mismatch(expected))
        }
        // I don't think any of these are possible using CDDL grammar.
        Node::Literal(_) => Err(ValidateError::Structural("literal map member".into())),
        Node::PreludeType(_) => Err(ValidateError::Structural("prelude type map member".into())),
        Node::Map(_) => Err(ValidateError::Structural("map as map member".into())),
        Node::Array(_) => Err(ValidateError::Structural("array as map member".into())),
        Node::Range(_) => Err(ValidateError::Structural("range as map member".into())),
    }
}

fn validate_map_unwrap(
    node: &Node,
    working_map: &mut WorkingMap,
    ctx: &dyn Context,
) -> ValidateResult {
    // After traversing an unwrap from inside a map, the next node must be a
    // Map node too.
    match node {
        Node::Map(m) => {
            // Recurse into each member of the unwrapped array.
            for member in &m.members {
                validate_map_member(member, working_map, ctx)?;
            }
            // All array members validated Ok.
            Ok(())
        }
        _ => Err(mismatch("unwrap map")),
    }
}

/// Validate an occurrence against a mutable working map.
fn validate_map_occur(
    occur: &Occur,
    working_map: &mut WorkingMap,
    ctx: &dyn Context,
) -> ValidateResult {
    let (lower_limit, upper_limit) = occur.limits();
    let mut count: usize = 0;

    loop {
        match validate_map_member(&occur.node, working_map, ctx) {
            Ok(_) => (),
            Err(_) => break,
        }
        count += 1;
        if count >= upper_limit {
            // Stop matching; we've consumed the maximum number of this key.
            break;
        }
    }
    if count < lower_limit {
        // Read this format string as "{{" then "{}" then "}}"
        // The first and last print a single brace; the value is in the
        // middle, e.g "{foo}".
        return Err(mismatch(format!("map{{{}}}]", occur)));
    }
    Ok(())
}

/// Validate a key-value pair against a mutable working map.
fn validate_map_keyvalue(
    kv: &KeyValue,
    working_map: &mut WorkingMap,
    ctx: &dyn Context,
) -> ValidateResult {
    let key_node = &kv.key;
    let val_node = &kv.value;

    // This is using WorkingMap::Validate, which returns a Value.
    // A successful key match has the side-effect of removing that key from
    // the working map (and returning its value to us.)
    // If we fail to validate a key, exit now with an error.
    let extracted_val: Value = validate_workingmap(working_map, key_node, ctx)?;

    // Match the value that was returned.
    validate(&extracted_val, val_node, ctx)
}

fn validate_standalone_group(g: &Group, value: &Value, ctx: &dyn Context) -> ValidateResult {
    // Since we're not in an array or map context, it's not clear how we should
    // validate a group containing multiple elements.  If we see one, return an
    // error.
    match g.members.len() {
        1 => {
            // Since our group has length 1, validate against that single element.
            validate(value, &g.members[0], ctx)
        }
        _ => Err(ValidateError::Unsupported("standalone group".into())),
    }
}

fn deref_range_rule(node: &Node, ctx: &dyn Context) -> TempResult<Literal> {
    match node {
        Node::Literal(l) => Ok(l.clone()),
        Node::Rule(r) => deref_range_rule(ctx.lookup_rule(&r.name)?, ctx),
        _ => Err(ValidateError::Structural(
            "confusing type on range operator".into(),
        )),
    }
}

// Returns true if value is within range
fn check_range<T: PartialOrd>(start: T, end: T, value: T, inclusive: bool) -> bool {
    if value < start {
        return false;
    }
    if inclusive {
        value <= end
    } else {
        value < end
    }
}

fn validate_range(range: &Range, value: &Value, ctx: &dyn Context) -> ValidateResult {
    // first dereference rules on start and end, if necessary.
    let start = deref_range_rule(&range.start, ctx)?;
    let end = deref_range_rule(&range.end, ctx)?;

    match (&start, &end, &value) {
        (Literal::Int(i1), Literal::Int(i2), Value::Integer(v)) => {
            if check_range(i1, i2, v, range.inclusive) {
                Ok(())
            } else {
                Err(mismatch(format!("{}", range)))
            }
        }
        (Literal::Float(f1), Literal::Float(f2), Value::Float(v)) => {
            if check_range(f1, f2, &v.0, range.inclusive) {
                Ok(())
            } else {
                Err(mismatch(format!("{}", range)))
            }
        }
        _ => {
            if discriminant(&start) == discriminant(&end) {
                // The range types were the same, so this is just a mismatch.
                Err(mismatch(format!("{}", range)))
            } else {
                // The range types didn't agree; return an error that points the
                // finger at the CDDL instead.
                Err(ValidateError::Structural(
                    "mismatched types on range operator".into(),
                ))
            }
        }
    }
}
