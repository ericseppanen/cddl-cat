//! This module defines error and result types.
//!

use crate::parser;
use std::error;
use std::fmt;
use std::result::Result;

/// A basic error type that contains a string.
#[allow(missing_docs)]
#[rustversion::attr(since(1.40), non_exhaustive)]
#[derive(Debug, PartialEq)]
pub enum ValidateError {
    /// An error during CDDL parsing.
    ParseError(parser::ParseError),
    /// A logical error in the CDDL structure.
    Structural(String),
    /// A data mismatch during validation.
    Mismatch(Mismatch),
    /// A CDDL rule lookup failed.
    MissingRule(String),
    /// A CDDL feature that is unsupported.
    Unsupported(String),
    /// A data value that can't be validated by CDDL.
    ValueError(String),
}

/// A data mismatch during validation.
///
/// If the CDDL specified an `int` and the data contained a string, this is
/// the error that would result.
#[derive(Debug, PartialEq)]
pub struct Mismatch {
    expected: String,
}

/// Shortcut for creating mismatch errors.
#[doc(hidden)]
pub fn mismatch<E: Into<String>>(expected: E) -> ValidateError {
    ValidateError::Mismatch(Mismatch {
        expected: expected.into(),
    })
}

impl fmt::Display for ValidateError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ValidateError::*;
        match self {
            ParseError(p) => p.fmt(f),
            Structural(msg) => write!(f, "Structural({})", msg),
            Mismatch(mismatch) => write!(f, "Mismatch(expected {})", mismatch.expected),
            MissingRule(rule) => write!(f, "MissingRule({})", rule),
            Unsupported(msg) => write!(f, "Unsupported {}", msg),
            ValueError(msg) => write!(f, "ValueError({})", msg),
        }
    }
}

// Standard boilerplate, required so other errors can wrap this one.
impl error::Error for ValidateError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

/// A validation that doesn't return anything.
pub type ValidateResult = Result<(), ValidateError>;
