//! This module defines the LookupContext trait.
//!
//! A [`LookupContext`] is used to specify runtime behavior for validation.
//! When a validation needs to resolve a rule reference, it will ask the
//! `LookupContext` to perform the name resolution.
//!

use crate::ivt::{RuleDef, RulesByName};
use crate::util::ValidateError;

// The Node reference lives as long as the LookupContext does.
type LookupResult<'a> = Result<&'a RuleDef, ValidateError>;

/// A LookupContext contains any external information required for validation.
///
/// Right now, that only includes a function that understands how to resolve
/// a name reference to an [`ivt::Rule`].
///
/// [`ivt::Rule`]: crate::ivt::Rule

pub trait LookupContext {
    /// Lookup a rule by name.
    fn lookup_rule<'a>(&'a self, name: &str) -> LookupResult<'a>;
}

/// A simple context that owns a set of rules and can lookup rules by name.
#[allow(missing_docs)]
pub struct BasicContext {
    pub rules: RulesByName,
}

impl BasicContext {
    /// Create a new BasicContext from a rules map.
    pub fn new(rules: RulesByName) -> BasicContext {
        BasicContext { rules }
    }
}

impl LookupContext for BasicContext {
    fn lookup_rule<'a>(&'a self, name: &str) -> LookupResult<'a> {
        match self.rules.get(name) {
            Some(rule_def) => Ok(rule_def),
            None => Err(ValidateError::MissingRule(name.into())),
        }
    }
}

#[doc(hidden)] // Only pub for integration tests
#[allow(missing_docs)]
pub mod tests {
    use super::*;

    /// A `LookupContext` that fails all rule lookups
    pub struct DummyContext;

    impl LookupContext for DummyContext {
        fn lookup_rule<'a>(&'a self, name: &str) -> LookupResult<'a> {
            Err(ValidateError::MissingRule(name.into()))
        }
    }
}
