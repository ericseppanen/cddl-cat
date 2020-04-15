//! `cddl-cat` is a library for validating encoded data against a CDDL
//! document that describes the expected structure of the data.
//!
//! **Note:** This library is fairly new, and may still contain significant
//! bugs, ommissions, or unstable interfaces.
//!
//! CDDL is a text document described by [RFC8610] that describes data
//! structures.  CDDL is not tied to any specific serialization or encoding
//! method; it can be used to validate data that is in [CBOR] or JSON format.
//!
//! The goal of this library is to make CBOR or JSON data easy to validate
//! against a CDDL schema description.
//!
//! `cddl-cat` supports Rust 1.37 and later.
//!
//! # Implementation Details
//!
//! - Supports CBOR and JSON encodings, controlled by the `serde_cbor` and
//!   `serde_json` features.
//!
//! - An "Intermediate Validation Tree" ([`ivt`]) is constructed from the CDDL
//!   AST; this removes some of the CDDL syntax detail resulting in a
//!   simplified tree that can be more easily validated.
//!   The IVT is constructed almost entirely of [`Node`] elements, allowing
//!   recursive validation.
//!
//! - Validation is performed by first translating the incoming data into
//!   a generic form, so most of the validation code is completely agnostic
//!   to the serialization format.
//!
//! - Validation code uses a [`Context`] object to perform all rule lookups.
//!   This will allow stacking CDDL documents or building CDDL libraries that
//!   can be used by other CDDL schemas.  In the future the validation process
//!   itself may be customized by changing the `Context` configuration.
//!
//! # Examples
//!
//! This example validates JSON-encoded data against a CDDL schema:
//!
//! ```
//! # #[cfg(feature = "serde_json")]
//! use cddl_cat::validate_json_str;
//!
//! let cddl_input = "person = {name: tstr, age: int}";
//! let json_str = r#"{ "name": "Bob", "age": 43 }"#;
//!
//! # #[cfg(feature = "serde_json")]
//! validate_json_str("person", cddl_input, &json_str).unwrap();
//! ```
//!
//! If the JSON data doesn't have the expected structure, an error will
//! result:
//! ```
//! # #[cfg(feature = "serde_json")]
//! use cddl_cat::validate_json_str;
//!
//! let cddl_input = "person = {name: tstr, age: int}";
//! let json_str = r#"{ "name": "Bob", "age": "forty three" }"#;
//!
//! # #[cfg(feature = "serde_json")]
//! assert!(validate_json_str("person", cddl_input, &json_str).is_err());
//! ```
//!
//! A similar example, verifying CBOR-encoded data against a CDDL schema:
//! ```
//! # #[cfg(feature = "serde_cbor")]
//! use cddl_cat::validate_cbor_bytes;
//! use serde::Serialize;
//!
//! #[derive(Serialize)]
//! struct PersonStruct {
//!     name: String,
//!     age: u32,
//! }
//!
//! let input = PersonStruct {
//!     name: "Bob".to_string(),
//!     age: 43,
//! };
//! # #[cfg(feature = "serde_cbor")]
//! let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
//! let cddl_input = "person = {name: tstr, age: int}";
//! # #[cfg(feature = "serde_cbor")]
//! validate_cbor_bytes("person", cddl_input, &cbor_bytes).unwrap();
//! ```
//! Supported prelude types:
//! - `any`, `uint`, `nint`, `int`, `bstr`, `bytes`, `tstr`, `text`
//! - `float`, `float16`, `float32`, `float64`, `float16-32`, `float32-64` \
//! Note: float sizes are not validated.
//!
//! Supported CDDL features:
//! - Basic prelude types (integers, floats, bool, nil, text strings, byte strings)
//! - Literal int, float, bool, UTF-8 text strings
//! - Byte strings in UTF-8 or hex
//! - Arrays and maps
//! - Rule lookups by name
//! - Groups
//! - Choices (using `/` or `//` syntax)
//! - Occurrences (`?`, `*`, `+`, or `m*n`)
//! - Ranges (e.g. `1..7` or `1...8`)
//! - Unwrapping (`~`)
//! - Map keys with cut syntax (`^ =>`)
//!
//! Unimplemented CDDL features:
//! - Generics
//! - Extend type with `/=`
//! - Extend group with `//=`
//! - Type sockets with `$`
//! - Group sockets with `$$`
//! - Control operators, e.g. `.size`, `.bits`, ...
//! - Group enumeration with `&`
//! - Tagged data with `#`
//! - Literal integers with `0x` or `0b`
//! - Hexfloat
//! - Base64 bytestring literals (`b64'...'`)
//! - Prelude types that invoke CBOR tags (e.g. `tdate` or `biguint`)
//!
//! [`Node`]: ivt::Node
//! [`Context`]: context::Context
//! [RFC8610]: https://tools.ietf.org/html/rfc8610
//! [CBOR]: https://cbor.io/

#![warn(missing_docs)]
#![forbid(unsafe_code)]

pub mod ast;
pub mod context;
pub mod flatten;
pub mod ivt;
pub mod parser;
pub mod util;
pub use util::{ValidateError, ValidateResult};
pub(crate) mod validate;
pub mod value;

#[cfg(feature = "serde_cbor")]
pub mod cbor;
#[cfg(feature = "serde_cbor")]
pub use cbor::{validate_cbor, validate_cbor_bytes};

#[cfg(feature = "serde_json")]
pub mod json;
#[cfg(feature = "serde_json")]
pub use json::{validate_json, validate_json_str};

pub use parser::parse_cddl;
