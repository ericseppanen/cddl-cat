use serde_cbor::{self, Value};
use crate::ivt::*;

fn make_oops(msg: &str) -> ValidateResult {
    Err(ValidateError::Oops(msg.into()))
}

impl Validate<Value> for Node {
    fn validate(&self, value: &Value) -> ValidateResult {
        match self {
            Node::PreludeType(p) => validate_prelude_type(p, value),
            Node::Choice(c) => validate_choice(c, value),
            _ => unimplemented!()
        }
    }
}

// This is a strange hack, to allow generic validation functions to live
// in the "ivt" module while still calling back here.
// I really wish I could find a way around this.
impl Pogo<Node> for Value {
    fn pogo(&self, node: &Node) -> ValidateResult {
        node.validate(self)
    }
}


fn validate_prelude_type(ty: &PreludeType, value: &Value) -> ValidateResult {
    match (ty, value) {
        (PreludeType::Int, Value::Integer(_)) => Ok(()),
        (PreludeType::Int, _) => make_oops("bad int"),
        _ => unimplemented!(),
    }
}



#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn validate_int() {
        let node: Node = PreludeType::Int.into();
        let value = Value::Integer(7);
        node.validate(&value).unwrap();

        let value = Value::Text("abc".into());
        node.validate(&value).unwrap_err();
    }
}
