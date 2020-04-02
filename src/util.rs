//! This module defines error and result types.
//!

use std::error;
use std::fmt;
use std::result::Result;

/// A basic error type that contains a string.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum ValidateError {
    Oops(String),
}

impl fmt::Display for ValidateError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ValidateError::Oops(msg) => write!(f, "Oops! {}", msg),
        }
    }
}

// Standard boilerplate, required so other errors can wrap this one.
impl error::Error for ValidateError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

/// A placeholder error generator.
///
/// This crate doesn't generate useful errors yet.  This is a simple substitute until
/// it does.
pub(crate) fn make_oops<T>(msg: &str) -> Result<T, ValidateError> {
    Err(ValidateError::Oops(msg.into()))
}

/// A validation that doesn't return anything.
pub type ValidateResult = Result<(), ValidateError>;
