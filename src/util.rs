use std::error;
use std::fmt;

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

pub fn make_oops<T>(msg: &str) -> TempResult<T> {
    Err(ValidateError::Oops(msg.into()))
}

/// A generic validation step that might need to return an intermediate value
/// This is used when validating a map key and the map value should be returned.
pub type TempResult<T> = std::result::Result<T, ValidateError>;
/// A validation that doesn't return anything.
pub type ValidateResult = TempResult<()>;
