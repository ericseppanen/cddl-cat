//! This module defines the Context trait.
//!
//! A [`Context`] is used to specify runtime behavior for validation.
//! When a validation needs to resolve a rule reference, it will ask the
//! `Context` to perform the name resolution.
//!

use crate::ivt::Node;
use crate::util::ValidateError;
use std::collections::BTreeMap;

/// This type is used in BasicContext to perform rule lookups.
pub type RulesByName = BTreeMap<String, Node>;

// The Node reference lives as long as the Context does.
type LookupResult<'a> = Result<&'a Node, ValidateError>;

/// A Context contains any external information required for validation.
///
/// Right now, that only includes a function that understands how to resolve
/// a name reference to an [`ivt::Rule`].
///
/// [`ivt::Rule`]: crate::ivt::Rule

pub trait Context {
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

impl Context for BasicContext {
    fn lookup_rule<'a>(&'a self, name: &str) -> LookupResult<'a> {
        match self.rules.get(name) {
            Some(node) => Ok(node),
            None => Err(ValidateError::MissingRule(name.into())),
        }
    }
}

#[doc(hidden)] // Only pub for integration tests
#[allow(missing_docs)]
pub mod tests {
    use super::{Context, LookupResult, ValidateError};

    /// A [Context] that fails all rule lookups
    pub struct DummyContext {}

    impl DummyContext {
        #![allow(clippy::new_without_default)]
        pub fn new() -> DummyContext {
            DummyContext {}
        }
    }

    impl Context for DummyContext {
        fn lookup_rule<'a>(&'a self, name: &str) -> LookupResult<'a> {
            Err(ValidateError::MissingRule(name.into()))
        }
    }
}
