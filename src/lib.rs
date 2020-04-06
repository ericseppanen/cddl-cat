//! This is a library for validating data structures against a CDDL document.
//!
//! **Note:** This library is fairly new, and may still contain significant
//! bugs, ommissions, or unstable interfaces.
//!
//! The goal of this library is to make individual encodings (like CBOR or
//! JSON) easy to validate.
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
//!
//! [`Node`]: ivt::Node

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
