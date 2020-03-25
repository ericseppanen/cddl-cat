//! **This crate is experimental.**
//!
//! This is a library for validating data structures against a CDDL document.
//!
//! This crate is a playground for testing ways to improve the validation
//! capabilities of the [`cddl`] crate.
//!
//! The goal of this library is to make individual encodings (like CBOR or
//! JSON) easier to validate, using the following tools:
//!
//! 1. An "Intermediate Validation Tree" (IVT) is constructed from the CDDL
//!    AST; this removes some of the CDDL syntax detail (groups, barewords,
//!    sockets, generics), resulting in a simplified tree that can be more
//!    easily validated.
//!
//! 2. The IVT is constructed almost entirely of [`Node`] elements, allowing
//!    recursive validation using the [`Validate`] trait.
//!
//! 3. Validation helper functions that are fully generic (like recursive
//!    validation of [`Choice`] types) can be written once and used by all
//!    data formats.
//!
//! 4. Validation is context-free because the tree is built of [`Arc`]
//!    shared references; global lookups into other rules is transparent.
//!
//! [`Node`]: ivt::Node
//! [`Validate`]: ivt::Validate
//! [`Choice`]: ivt::Choice
//! [`Arc`]: std::sync::Arc

pub mod cbor;
pub mod generic;
pub mod ivt;
pub mod util;

pub use cbor::validate_cbor;
