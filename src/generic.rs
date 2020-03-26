//! This module contains generic validation helper functions.

use crate::ivt::{Rule, Choice, Validate, Node};
use crate::util::*;


/// Validate a `Choice` containing an arbitrary number of "option" nodes.
///
/// If any of the options matches, this validation is successful.
pub fn validate_choice<T, V: Validate<T>>(choice: &Choice, value: &V) -> TempResult<T> {
    for node in &choice.options {
        let node: &Node = node.as_ref();
        if let Ok(result) = value.validate(node) {
            return Ok(result);
        }
    }
    make_oops("choice failed")
}

/// Validate a `Rule` reference
///
/// This just falls through to the referenced `Node`.
pub fn validate_rule<T, V: Validate<T>>(rule: &Rule, value: &V) -> TempResult<T> {
    let node = rule.get_ref().unwrap();
    value.validate(&node)
}
