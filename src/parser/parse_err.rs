//! Parser error types and related utilities
//!

use nom::error::FromExternalError;
use std::borrow::Cow;
use thiserror::Error;

/// The "kind" of error generated during CDDL parsing.
#[non_exhaustive]
#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind {
    /// An integer didn't parse correctly.
    MalformedInteger,
    /// A floating-point number didn't parse correctly.
    MalformedFloat,
    /// A hex literal didn't parse correctly.
    MalformedHex,
    /// A malformed text string
    MalformedText,
    /// A malformed base64 byte string
    MalformedBase64,
    /// A nonspecific parsing error.
    Unparseable,
}

/// An error that occurred during CDDL parsing.
#[derive(Debug, Error)]
// thiserror will generate a Display implementation.
#[error("{kind:?}({ctx})")]
pub struct ParseError {
    /// The "kind" of error generated during CDDL parsing.
    pub kind: ErrorKind,
    /// A snippet of text from the CDDL input that may be the cause of the error.
    pub ctx: String,
}

// Convert a temporary error into an owned 'static error.
impl From<CowParseError<'_>> for ParseError {
    fn from(err: CowParseError<'_>) -> Self {
        ParseError {
            kind: err.kind,
            // Create an owned String from the Cow<'_, str>
            ctx: err.ctx.into(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct CowParseError<'a> {
    /// The "kind" of error generated during CDDL parsing.
    pub kind: ErrorKind,
    /// A snippet of text from the CDDL input that may be the cause of the error.
    ///
    /// This may contain either a borrowed 'str or an owned String. This is useful
    /// because many transient errors are generated during parsing and thrown away,
    /// and there's no point allocating memory for them until we are done parsing.
    pub ctx: Cow<'a, str>,
}

// Convert a bounded lifetime ParseError into a ParseError<'static>.

impl<I, E> FromExternalError<I, E> for CowParseError<'_> {
    fn from_external_error(_input: I, _kind: nom::error::ErrorKind, _e: E) -> Self {
        CowParseError {
            kind: ErrorKind::Unparseable,
            ctx: "nom-error".into(),
        }
    }
}

pub(crate) fn parse_error<'a, S: Into<Cow<'a, str>>>(kind: ErrorKind, ctx: S) -> CowParseError<'a> {
    CowParseError {
        kind,
        ctx: ctx.into(),
    }
}

// Used when calling all_consuming() at the end of the parsing process.
impl From<nom::Err<CowParseError<'_>>> for ParseError {
    fn from(e: nom::Err<CowParseError>) -> ParseError {
        match e {
            nom::Err::Incomplete(_) => parse_error(ErrorKind::Unparseable, "Incomplete"),
            nom::Err::Error(pe) => pe,
            nom::Err::Failure(pe) => pe,
        }
        .into()
    }
}

// FIXME: the name collision here makes the code hard to read
impl<'a, I: Into<Cow<'a, str>>> nom::error::ParseError<I> for CowParseError<'a> {
    fn from_error_kind(input: I, _kind: nom::error::ErrorKind) -> Self {
        parse_error(ErrorKind::Unparseable, input)
    }

    fn append(_input: I, _kind: nom::error::ErrorKind, other: Self) -> Self {
        // FIXME: It's not obvious what I should do here, or
        // when a proper implementation will be necessary...
        other
    }
}
