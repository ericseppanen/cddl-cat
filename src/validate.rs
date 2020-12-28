//! This module contains code to validate serialized data.
//!
//! More precisely, it validates data that can be represented by [`Value`] trees.

use crate::context::LookupContext;
use crate::ivt::*;
use crate::util::{mismatch, ValidateError, ValidateResult};
use crate::value::Value;
use std::collections::BTreeMap; // used in Value::Map
use std::collections::HashMap;
use std::collections::VecDeque;
use std::convert::TryInto;
use std::mem::discriminant;

// A map from generic parameter name to the type being used here.
#[derive(Clone, Debug)]
struct GenericMap<'a> {
    map: HashMap<String, &'a Node>,
    // Because we captured this map at a previous time, we may need to look up
    // generic types from that previous context.  We carry a copy of that
    // Context with us to do those lookups.
    past_ctx: Option<&'a Context<'a>>,
}

// At the top level of a validation, we won't have any past_ctx
// because there cannot be any generic parameter map.
impl Default for GenericMap<'_> {
    fn default() -> Self {
        Self {
            map: HashMap::default(),
            past_ctx: None,
        }
    }
}

#[derive(Clone)]
struct Context<'a> {
    lookup: &'a dyn LookupContext,
    generic_map: GenericMap<'a>,
    depth: u32,
}

impl std::fmt::Debug for Context<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Context")
            .field("generic_map", &self.generic_map)
            .finish()
    }
}

struct NodeContext<'a> {
    node: &'a Node,
    ctx: Context<'a>,
}

impl<'a> Context<'a> {
    // Set a maximum depth, to avoid infinite recursion in the case of
    // circular rule references.
    const MAX_DEPTH: u32 = 50;

    fn inc_depth(&self) -> TempResult<u32> {
        if self.depth >= Self::MAX_DEPTH {
            Err(ValidateError::Structural("hit recursion limit".into()))
        } else {
            Ok(self.depth + 1)
        }
    }

    // `lookup_rule` will first do a search for a local generic parameter name
    // first, followed by a lookup for a rule by that name.

    // To give an example:
    //     IP = [u8, u8, u8, u8]
    //     PORT = u16
    //     socket = [IP, PORT]
    //     conn<IP> = [name, socket, IP] # This is the generic parm IP not the rule IP.
    //
    // While validating "conn" we have a Context with a generic mapping for "IP".
    // As soon as we traverse into "socket", we need for there to be a new context,
    // where no "IP" exists (so we'll do the rule lookup instead).
    //
    fn lookup_rule(&'a self, rule: &'a Rule) -> TempResult<NodeContext<'a>> {
        // First, check to see if the "rule name" is actually a generic parameter.
        // TODO: this would be a lot easier if this were pre-processed by the flattener
        // so that "rule lookup" and "generic type lookup" were two separate Node variants.
        let generic_node: Option<&'a Node> = self.generic_map.map.get(&rule.name).cloned();
        if let Some(node) = generic_node {
            // If we stored a past_ctx along with the generic args, then pass that
            // context along with the node that is substituting for this generic
            // parameter name.  Otherwise, return a new blank Context.
            let ctx = match self.generic_map.past_ctx {
                Some(ctx) => ctx.clone(),
                None => self.blank()?,
            };
            return Ok(NodeContext { node, ctx });
        }

        let rule_def: &RuleDef = self.lookup.lookup_rule(&rule.name)?;

        // Create a new context containing a new generic parameter map.
        let ctx = self.derive(rule_def, rule)?;

        Ok(NodeContext {
            node: &rule_def.node,
            ctx,
        })
    }

    // Create a new blank Context (with no parameter map)
    fn blank(&self) -> TempResult<Context<'a>> {
        Ok(Context {
            lookup: self.lookup,
            generic_map: GenericMap::default(),
            depth: self.inc_depth()?,
        })
    }

    // Create a new Context, with optional generic parameter map.
    fn derive(&'a self, rule_def: &'a RuleDef, rule: &'a Rule) -> TempResult<Context<'a>> {
        let parms_len = rule_def.generic_parms.len();
        let args_len = rule.generic_args.len();
        if parms_len != args_len {
            // Wrong number of generic arguments
            return Err(ValidateError::GenericError);
        }

        // Zip the two Vecs:
        // 1. rule_def.generic_parms
        // 2. rule.generic_args
        // and put them into a HashMap so we can do lookups from parm -> arg.
        let map: HashMap<String, &'a Node> = rule_def
            .generic_parms
            .iter()
            .cloned()
            .zip(rule.generic_args.iter())
            .collect();

        let generic_map = GenericMap {
            map,
            past_ctx: Some(self),
        };

        Ok(Context {
            lookup: self.lookup,
            generic_map,
            depth: self.inc_depth()?,
        })
    }
}

pub(crate) fn do_validate(
    value: &Value,
    rule_def: &RuleDef,
    ctx: &dyn LookupContext,
) -> ValidateResult {
    // If the rule_def passed in requires generic parameters, we should
    // return an error, because we don't have any way to specify them.
    if !rule_def.generic_parms.is_empty() {
        return Err(ValidateError::GenericError);
    }

    let ctx = Context {
        lookup: ctx,
        generic_map: GenericMap::default(),
        depth: 0,
    };
    let node = &rule_def.node;
    validate(value, node, &ctx)
}

type ValueMap = BTreeMap<Value, Value>;

// A Result that returns some temporary value.
type TempResult<T> = Result<T, ValidateError>;

/// This struct allows us to maintain a map that is consumed during validation.
struct WorkingMap {
    map: ValueMap,
    // A stack of lists; each list contains maybe-discarded elements,
    // in (key, value) form.
    snaps: VecDeque<VecDeque<(Value, Value)>>,
}

impl WorkingMap {
    /// Makes a copy of an existing map's table.
    fn new(value_map: &ValueMap) -> WorkingMap {
        WorkingMap {
            map: value_map.clone(),
            snaps: VecDeque::new(),
        }
    }

    // When we start speculatively matching map elements (e.g. in a Choice
    // or Occur containing groups), we may fail the match partway through, and
    // need to rewind to the most recent snapshot.
    //
    // It's possible for nested snapshots to exist; for example if we have a
    // group-of-choices nested inside a group-of-choices.
    //
    // If one array is nested inside another, the inner array will get its
    // own WorkingArray so snapshots aren't necessary in that case.
    fn snapshot(&mut self) {
        self.snaps.push_back(VecDeque::new());
    }

    // Restore the map to the point when we last called snapshot()
    fn rewind(&mut self) {
        // If validate code is implemented correctly, then unwrap() should
        // never panic.
        let mut top_snap = self.snaps.pop_back().unwrap();
        // Drain the elements (order not important), and insert them back into
        // the working map.
        self.map.extend(top_snap.drain(..));
    }

    // We completed a match, so we can retire the most recent snapshot.
    fn commit(&mut self) {
        // If validate code is implemented correctly, then unwrap() should
        // never panic.
        // This throws away the list that was popped; those values were
        // successfully matched and are no longer needed.
        self.snaps.pop_back().unwrap();
    }

    // Peek at the value correspending to a given key (if any)
    fn peek_at(&self, key: &Value) -> Option<&Value> {
        self.map.get(key)
    }

    // Remove a value from the working map.
    // If there is an active snapshot, stash the key/value pair there until
    // we're certain we've matched the entire group.
    fn remove(&mut self, key: &Value) {
        // If validate code is implemented correctly, then unwrap() should
        // never panic (we've already peeked at this value in order to match
        // it.)
        let value = self.map.remove(&key).unwrap();
        // If there is a current snapshot, preserve this element
        // for later rewind.
        if let Some(snap) = self.snaps.back_mut() {
            snap.push_back((key.clone(), value));
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
    // or Occur containing groups), we may fail the match partway through, and
    // need to rewind to the most recent snapshot.
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
        // This throws away the list that was popped; those values were
        // successfully matched and are no longer needed.
        self.snaps.pop_back().unwrap();
    }

    // Peek at the front of the working array.
    fn peek_front(&self) -> Option<&Value> {
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
        // If there is a current snapshot, preserve this element
        // for later rewind.
        if let Some(snap) = self.snaps.back_mut() {
            snap.push_back(element);
        }
    }
}

// Prevent warnings if both serde_cbor and serde_json are disabled.
#[allow(dead_code)]

// This is the main validation dispatch function.
// It tries to match a Node and a Value, recursing as needed.
fn validate(value: &Value, node: &Node, ctx: &Context) -> ValidateResult {
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
        Node::Control(ctl) => validate_control(ctl, value, ctx),
    }
}

// Perform map key search.
// Some keys (literal values) can be found with a fast search, while
// others may require a linear search.
fn validate_map_key<'a>(
    value_map: &'a mut WorkingMap,
    node: &Node,
    ctx: &Context,
) -> TempResult<(Value, &'a Value)> {
    match node {
        Node::Literal(l) => map_search_literal(l, value_map),
        _ => map_search(node, value_map, ctx),
    }
}

/// Validate a `Choice` containing an arbitrary number of "option" nodes.
///
/// If any of the options matches, this validation is successful.
fn validate_choice(choice: &Choice, value: &Value, ctx: &Context) -> ValidateResult {
    for node in &choice.options {
        match validate(value, node, ctx) {
            Ok(()) => {
                return Ok(());
            }
            Err(e) => {
                // Only fail if the error is considered fatal.
                // Otherwise, we'll keep trying other options.
                if e.is_fatal() {
                    return Err(e);
                }
            }
        }
    }
    let expected = format!("choice of {}", choice.options.len());
    Err(mismatch(expected))
}

/// Validate a `Rule` reference
///
/// Seek out the right `Node` and `Context`, and recurse.
fn validate_rule(rule: &Rule, value: &Value, ctx: &Context) -> ValidateResult {
    let answer = ctx.lookup_rule(&rule)?;
    validate(value, &answer.node, &answer.ctx)
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

// Find a specific key in the map and return that key plus a reference to its value.
fn map_search_literal<'a>(
    literal: &Literal,
    working_map: &'a mut WorkingMap,
) -> TempResult<(Value, &'a Value)> {
    let search_key = Value::from(literal);
    match working_map.peek_at(&search_key) {
        Some(val) => Ok((search_key, val)),
        None => {
            // We didn't find the key; return an error
            Err(mismatch(format!("map{{{}}}", literal)))
        }
    }
}

// Iterate over each key in the working map, looking for a match.
// If we find a match, return a copy of the key, and a reference to the value.
// This is less efficient than map_search_literal.
fn map_search<'a>(
    node: &Node,
    working_map: &'a mut WorkingMap,
    ctx: &Context,
) -> TempResult<(Value, &'a Value)> {
    for (key, value) in working_map.map.iter() {
        let attempt = validate(key, node, ctx);
        if attempt.is_ok() {
            return Ok((key.clone(), value));
        }
    }
    // We searched all the keys without finding a match.  Validation fails.
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
fn validate_array(ar: &Array, value: &Value, ctx: &Context) -> ValidateResult {
    match value {
        Value::Array(a) => validate_array_part2(ar, a, ctx),
        _ => Err(mismatch("array")),
    }
}

fn validate_array_part2(ar: &Array, value_array: &[Value], ctx: &Context) -> ValidateResult {
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
    ctx: &Context,
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

            let answer = ctx.lookup_rule(r)?;
            validate_array_member(&answer.node, working_array, &answer.ctx)
        }
        Node::Unwrap(r) => {
            // Like Rule, we are dereferencing the Rule by hand here so that
            // we can "see through" to the underlying data without forgetting
            // we were in an array context.
            let answer = ctx.lookup_rule(r)?;
            validate_array_unwrap(&answer.node, working_array, &answer.ctx)
        }
        Node::Choice(c) => {
            // We need to explore each of the possible choices.
            // We can't use validate_array_value() because we'll lose our
            // array context.
            for option in &c.options {
                match validate_array_member(option, working_array, ctx) {
                    Ok(()) => {
                        return Ok(());
                    }
                    Err(e) => {
                        // Only fail if the error is considered fatal.
                        // Otherwise, we'll keep trying other options.
                        if e.is_fatal() {
                            return Err(e);
                        }
                    }
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
    ctx: &Context,
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
    ctx: &Context,
) -> ValidateResult {
    let (lower_limit, upper_limit) = occur.limits();
    let mut count: usize = 0;

    loop {
        match validate_array_member(&occur.node, working_array, ctx) {
            Ok(_) => (),
            Err(e) => {
                if e.is_mismatch() {
                    // Stop trying to match this occurrence.
                    break;
                }
                // The error is something serious (e.g. MissingRule or
                // Unsupported).  We should fail now and propagate that
                // error upward.
                return Err(e);
            }
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
    ctx: &Context,
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

fn validate_map(m: &Map, value: &Value, ctx: &Context) -> ValidateResult {
    match value {
        Value::Map(vm) => validate_map_part2(m, vm, ctx),
        _ => Err(mismatch("map")),
    }
}

fn validate_map_part2(m: &Map, value_map: &ValueMap, ctx: &Context) -> ValidateResult {
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
        validate_map_member(member, &mut working_map, ctx).map_err(|e| {
            // If a MapCut error pops out here, change it to a Mismatch, so that
            // it can't cause trouble in nested maps.
            e.erase_mapcut()
        })?;
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
    ctx: &Context,
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
            let answer = ctx.lookup_rule(r)?;
            validate_map_member(&answer.node, working_map, &answer.ctx)
        }
        Node::Unwrap(r) => {
            // Like Rule, we are dereferencing the Rule by hand here so that
            // we can "see through" to the underlying data without forgetting
            // we were in a map context.
            let answer = ctx.lookup_rule(r)?;
            validate_map_unwrap(&answer.node, working_map, &answer.ctx)
        }
        Node::Group(g) => {
            // As we call validate_array_member, we don't know how many items
            // it might speculatively pop from the list.  So we'll take a
            // snapshot now and commit our changes if we match successfully
            // (and roll them back if it fails).
            working_map.snapshot();

            // Recurse into each member of the group.
            for group_member in &g.members {
                match validate_map_member(group_member, working_map, ctx) {
                    Ok(_) => {
                        // So far so good...
                    }
                    Err(e) => {
                        // Since we failed to validate the entire group,
                        // rewind to our most recent snapshot.  This may put
                        // values back into the map, so they can be matched by
                        // whatever we try next (or trigger an error if they
                        // aren't consumed by anything).
                        working_map.rewind();

                        // Also forget any MapCut errors, so that a sibling
                        // group may succeed where we failed.
                        return Err(e.erase_mapcut());
                    }
                }
            }
            // All group members validated Ok.
            working_map.commit();
            Ok(())
        }
        Node::Choice(c) => {
            // We need to explore each of the possible choices.
            for option in &c.options {
                match validate_map_member(option, working_map, ctx) {
                    Ok(()) => {
                        return Ok(());
                    }
                    Err(e) => {
                        // We can keep trying other options as long as the
                        // error is a Mismatch, not a MapCut or something
                        // fatal.
                        if !e.is_mismatch() {
                            return Err(e);
                        }
                    }
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
        Node::Control(_) => Err(ValidateError::Structural("control op as map member".into())),
    }
}

fn validate_map_unwrap(node: &Node, working_map: &mut WorkingMap, ctx: &Context) -> ValidateResult {
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
    ctx: &Context,
) -> ValidateResult {
    let (lower_limit, upper_limit) = occur.limits();
    let mut count: usize = 0;

    loop {
        match validate_map_member(&occur.node, working_map, ctx) {
            Ok(_) => (),
            Err(e) => {
                if e.is_mismatch() {
                    // Stop trying to match this occurrence.
                    break;
                }
                // Either we got a MapCut error, or it's something even more
                // serious (e.g. MissingRule or Unsupported).  We should fail
                // now and propagate that error upward.
                return Err(e);
            }
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
    ctx: &Context,
) -> ValidateResult {
    // CDDL syntax reminder:
    //   a => b   ; non-cut
    //   a ^ => b ; cut
    //   a: b     ; cut
    //
    // If we're using "cut" semantics, a partial match (key matches + value
    // mismatch) should force validation failure for the entire map.  We
    // signal this to our caller with a MapCut error.
    // If we're using "non-cut" semantics, a partial match will leave the
    // key-value pair in place, in the hope it may match something else.

    let key_node = &kv.key;
    let val_node = &kv.value;
    let cut = kv.cut;

    // If we fail to validate a key, exit now with an error.
    let (working_key, working_val) = validate_map_key(working_map, key_node, ctx)?;

    // Match the value that was returned.
    match validate(working_val, val_node, ctx) {
        Ok(()) => {
            working_map.remove(&working_key);
            Ok(())
        }
        Err(e) => {
            match (cut, e) {
                (true, ValidateError::Mismatch(m)) => {
                    // If "cut" semantics are in force, then rewrite Mismatch errors.
                    // This allows special handling when nested inside Occur nodes.
                    Err(ValidateError::MapCut(m))
                }
                (_, x) => Err(x),
            }
        }
    }
}

fn validate_standalone_group(g: &Group, value: &Value, ctx: &Context) -> ValidateResult {
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

fn deref_range_rule(node: &Node, ctx: &Context) -> TempResult<Literal> {
    match node {
        Node::Literal(l) => Ok(l.clone()),
        Node::Rule(r) => {
            let answer = ctx.lookup_rule(r)?;
            deref_range_rule(&answer.node, &answer.ctx)
        }
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

fn validate_range(range: &Range, value: &Value, ctx: &Context) -> ValidateResult {
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

// Follow a chain of Rule references until we reach a non-Rule node.
fn chase_rules<'a, F, R>(node: &'a Node, ctx: &'a Context<'a>, f: F) -> TempResult<R>
where
    F: Fn(&Node) -> TempResult<R>,
    R: 'static,
{
    if let Node::Rule(rule) = node {
        let answer = ctx.lookup_rule(&rule)?;
        chase_rules(answer.node, &answer.ctx, f)
    } else {
        f(node)
    }
}

fn validate_control(ctl: &Control, value: &Value, ctx: &Context) -> ValidateResult {
    match ctl {
        Control::Size(ctl_size) => validate_control_size(ctl_size, value, ctx),
    }
}

fn validate_control_size(ctl: &CtlOpSize, value: &Value, ctx: &Context) -> ValidateResult {
    // Follow the chain of rules references until we wind up with a non-Rule node.
    //let target = chase_rules(&ctl.target, ctx)?;
    let size: u64 = chase_rules(&ctl.size, ctx, |size_node| {
        // Compute the size in bytes
        match size_node {
            Node::Literal(Literal::Int(i)) => (*i).try_into().map_err(|n| {
                let msg = format!("bad .size limit {}", n);
                ValidateError::Structural(msg)
            }),
            _ => {
                let msg = format!("bad .size argument type ({})", size_node);
                Err(ValidateError::Structural(msg))
            }
        }
    })?;

    chase_rules(&ctl.target, ctx, |target_node| {
        // Ensure that the target node evaluates to some type that is
        // compatible with the .size operator, and then validate the size limit.
        match target_node {
            Node::PreludeType(PreludeType::Uint) => validate_size_uint(size, value),
            Node::PreludeType(PreludeType::Tstr) => validate_size_tstr(size, value),
            Node::PreludeType(PreludeType::Bstr) => validate_size_bstr(size, value),
            _ => {
                let msg = format!("bad .size target type ({})", target_node);

                Err(ValidateError::Structural(msg))
            }
        }
    })
}

fn validate_size_uint(size: u64, value: &Value) -> ValidateResult {
    match value {
        Value::Integer(x) => {
            if *x < 0 {
                Err(mismatch(".size on negative integer"))
            } else if size >= 16 {
                // If the size is 16 or larger, the i128 value will always fit.
                Ok(())
            } else {
                let mask = (1i128 << (size * 8)) - 1;
                if *x & !mask == 0 {
                    Ok(())
                } else {
                    Err(mismatch("uint over .size limit"))
                }
            }
        }
        _ => Err(mismatch("uint")),
    }
}

// Check the size of a text string.
fn validate_size_tstr(size: u64, value: &Value) -> ValidateResult {
    match value {
        Value::Text(s) => {
            let size: usize = size.try_into().unwrap_or(usize::MAX);
            if s.len() > size {
                Err(mismatch("tstr over .size limit"))
            } else {
                Ok(())
            }
        }
        _ => Err(mismatch("tstr")),
    }
}

// Check the size of a byte string.
fn validate_size_bstr(size: u64, value: &Value) -> ValidateResult {
    match value {
        Value::Bytes(b) => {
            let size: usize = size.try_into().unwrap_or(usize::MAX);
            if b.len() > size {
                Err(mismatch("bstr over .size limit"))
            } else {
                Ok(())
            }
        }
        _ => Err(mismatch("bstr")),
    }
}
