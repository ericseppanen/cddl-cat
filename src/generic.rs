//! This module contains generic validation helper functions.

use crate::ivt::{Choice, Context, Rule, Validate};
use crate::util::*;

/// Validate a `Choice` containing an arbitrary number of "option" nodes.
///
/// If any of the options matches, this validation is successful.
pub fn validate_choice<T, V: Validate<T>>(
    choice: &Choice,
    value: &V,
    ctx: &Context,
) -> TempResult<T> {
    for node in &choice.options {
        if let Ok(result) = value.validate(node, ctx) {
            return Ok(result);
        }
    }
    make_oops("choice failed")
}

/// Validate a `Rule` reference
///
/// This just falls through to the referenced `Node`.
pub fn validate_rule<T, V: Validate<T>>(rule: &Rule, value: &V, ctx: &Context) -> TempResult<T> {
    let node = ctx.lookup_rule(&rule.name);
    value.validate(&node, ctx)
}
