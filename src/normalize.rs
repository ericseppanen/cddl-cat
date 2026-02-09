//! This module contains code to normalize data into desired formats

use std::convert::TryInto;

use crate::ivt::{Literal, Node, Range};
use crate::ValidateError;

/// Normalize .size control into Range format.
pub fn normalize_size_range(size_node: &Node) -> Result<Range, ValidateError> {
    match size_node {
        Node::Literal(Literal::Int(i)) => {
            let _: u64 = (*i)
                .try_into()
                .map_err(|_| ValidateError::Structural(format!("bad .size limit {}", i)))?;

            Ok(Range {
                start: Box::new(Node::Literal(Literal::Int(*i))),
                end: Box::new(Node::Literal(Literal::Int(*i))),
                inclusive: true,
            })
        }
        Node::Range(r) => {
            let start_i = match r.start.as_ref() {
                Node::Literal(Literal::Int(i)) => *i,
                other => {
                    return Err(ValidateError::Structural(format!(
                        "bad .size range start type ({})",
                        other
                    )))
                }
            };
            let end_i = match r.end.as_ref() {
                Node::Literal(Literal::Int(i)) => *i,
                other => {
                    return Err(ValidateError::Structural(format!(
                        "bad .size range end type ({})",
                        other
                    )))
                }
            };
            let _: u64 = start_i
                .try_into()
                .map_err(|_| ValidateError::Structural(format!("bad .size limit {}", start_i)))?;
            let _: u64 = end_i
                .try_into()
                .map_err(|_| ValidateError::Structural(format!("bad .size limit {}", end_i)))?;

            Ok(r.clone())
        }
        _ => Err(ValidateError::Structural(format!(
            "bad .size argument type ({})",
            size_node
        ))),
    }
}
