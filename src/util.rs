//! This module defines error and result types.
//!

use crate::parser;
use std::result::Result;
use thiserror::Error;

/// A basic error type that contains a string.
#[allow(missing_docs)]
#[non_exhaustive]
#[derive(Debug, PartialEq, Eq, Error)]
pub enum ValidateError {
    /// An error during CDDL parsing.
    #[error(transparent)]
    ParseError(#[from] parser::ParseError),
    /// A logical error in the CDDL structure.
    #[error("Structural({0})")]
    Structural(String),
    /// A data mismatch during validation.
    // The difference between Mismatch and MapCut is that they trigger
    // slightly different internal behavior; to a human reader they mean
    // the same thing so we will Display them the same way.
    #[error("Mismatch(expected {})", .0.expected)]
    Mismatch(Mismatch),
    /// A map key-value cut error.
    #[error("Mismatch(expected {})", .0.expected)]
    MapCut(Mismatch),
    /// A CDDL rule lookup failed.
    #[error("MissingRule({0})")]
    MissingRule(String),
    /// A CDDL feature that is unsupported.
    #[error("Unsupported {0}")]
    Unsupported(String),
    /// A data value that can't be validated by CDDL.
    #[error("ValueError({0})")]
    ValueError(String),
    /// A generic type parameter was used incorrectly.
    #[error("GenericError")]
    GenericError,
}

impl ValidateError {
    /// Identify whether this error is fatal
    ///
    /// A "fatal" error is one that should fail the entire validation, even if
    /// it occurs inside a choice or occurrence that might otherwise succeed.
    pub(crate) fn is_fatal(&self) -> bool {
        !matches!(self, ValidateError::Mismatch(_) | ValidateError::MapCut(_))
    }

    /// Convert a MapCut error to a Mismatch error; otherwise return the original error.
    pub(crate) fn erase_mapcut(self) -> ValidateError {
        match self {
            ValidateError::MapCut(m) => ValidateError::Mismatch(m),
            _ => self,
        }
    }

    pub(crate) fn is_mismatch(&self) -> bool {
        matches!(self, ValidateError::Mismatch(_))
    }
}

/// A data mismatch during validation.
///
/// If the CDDL specified an `int` and the data contained a string, this is
/// the error that would result.
#[derive(Debug, PartialEq, Eq)]
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

/// A validation that doesn't return anything.
pub type ValidateResult = Result<(), ValidateError>;

// Some utility functions that are helpful when testing whether the right
// error was returned.
#[doc(hidden)]
pub trait ErrorMatch {
    fn err_mismatch(&self);
    fn err_missing_rule(&self);
    fn err_generic(&self);
    fn err_parse(&self);
    fn err_structural(&self);
}

impl ErrorMatch for ValidateResult {
    #[track_caller]
    fn err_mismatch(&self) {
        match self {
            Err(ValidateError::Mismatch(_)) => (),
            _ => panic!("expected Mismatch, got {:?}", self),
        }
    }

    #[track_caller]
    fn err_missing_rule(&self) {
        match self {
            Err(ValidateError::MissingRule(_)) => (),
            _ => panic!("expected MissingRule, got {:?}", self),
        }
    }

    #[track_caller]
    fn err_generic(&self) {
        match self {
            Err(ValidateError::GenericError) => (),
            _ => panic!("expected GenericError, got {:?}", self),
        }
    }

    #[track_caller]
    fn err_parse(&self) {
        match self {
            Err(ValidateError::ParseError(_)) => (),
            _ => panic!("expected ParseError, got {:?}", self),
        }
    }

    #[track_caller]
    fn err_structural(&self) {
        match self {
            Err(ValidateError::Structural(_)) => (),
            _ => panic!("expected Structural, got {:?}", self),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_extras() {
        let e: ValidateError = mismatch("");
        assert!(!e.is_fatal());

        let e = ValidateError::Structural("".into());
        assert!(e.is_fatal());
    }
}
