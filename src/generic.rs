use crate::ivt::*;
use crate::util::*;

/// This module contains generic validation helper functions.

/// Validate a `Choice` containing an arbitrary number of "option" nodes.
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
