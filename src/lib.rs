//! This is a library for validating data structures against a CDDL document.
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
//! Some of the ways this library differs from other implementations:
//!
//! - An "Intermediate Validation Tree" ([`ivt`]) is constructed from the CDDL
//!   AST; this removes some of the CDDL syntax detail resulting in a
//!   simplified tree that can be more easily validated.
//!   The IVT is constructed almost entirely of [`Node`] elements, allowing
//!   recursive validation.
//!
//! - Validation is performed by first translating the incoming data into
//!   a generic tree form, so the bulk of the validation code is completely
//!   agnostic to the serialization format.
//!
//! An example, validating CBOR-encoded data against a CDDL schema:
//! ```
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
//! let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
//! let cddl_input = "thing = {name: tstr, age: int}";
//! validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();
//! ```
//! Supported prelude types:
//! - `any`, `uint`, `nint`, `int`, `bstr`, `bytes`, `tstr`, `text`
//! - `float`, `float16`, `float32`, `float64`, `float16-32`, `float32-64` \
//! Note: float sizes are not validated.
//!
//! Unimplemented features:
//! - Generics
//! - Non-cut map keys
//! - Extend type with `/=`
//! - Extend group with `//=`
//! - Type sockets with `$`
//! - Group sockets with `$$`
//! - Range operators `..`, `...`
//! - Control operators, e.g. `.size`, `.bits`, ...
//! - Group enumeration with `&`
//! - Tagged data with `#`
//! - Literal integers with `0x` or `0b`
//! - Hexfloat
//! - Base64 bytestring literals (`b64'...'`)
//! - Prelude types that invoke CBOR tags (e.g. `tdate` or `biguint`)
//!
//! [`Node`]: ivt::Node
//! [RFC8610]: https://tools.ietf.org/html/rfc8610
//! [CBOR]: https://cbor.io/

#![warn(missing_docs)]

pub mod ast;
pub mod cbor;
pub mod context;
pub mod flatten;
pub mod ivt;
pub mod parser;
pub mod util;
pub(crate) mod validate;
pub mod value;

pub use cbor::{validate_cbor, validate_cbor_bytes};
pub use parser::parse_cddl;
