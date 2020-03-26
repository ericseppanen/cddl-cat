//! This module defines error and result types.
//!

use std::error;
use std::fmt;

/// A basic error type that contains a string.
#[derive(Debug, Clone)]
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
pub fn make_oops<T>(msg: &str) -> TempResult<T> {
    Err(ValidateError::Oops(msg.into()))
}

/// A Result that returns some temporary value.
///
/// This is used when validating a map key and the map value should be returned.
pub type TempResult<T> = std::result::Result<T, ValidateError>;

/// A validation that doesn't return anything.
pub type ValidateResult = TempResult<()>;