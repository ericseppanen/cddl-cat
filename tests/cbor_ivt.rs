use cddl_validator::ivt::*;
use serde::ser::Serialize;
use serde_cbor::Value;
use std::collections::HashMap;
use std::sync::Arc;

// Some Node structs to test with
static PRELUDE_INT: &Node = &Node::PreludeType(PreludeType::Int);
static PRELUDE_TSTR: &Node = &Node::PreludeType(PreludeType::Tstr);
static LITERAL_7: &Node = &Node::Literal(Literal::Int(7));

// Create a Value instance from anything that's serializable
fn gen_value<T: Serialize>(t: T) -> Value {
    serde_cbor::value::to_value(t).unwrap()
}

#[test]
fn validate_prelude_int() {
    let node = PRELUDE_INT;
    gen_value(7).validate(node).unwrap();
    gen_value("abc").validate(node).unwrap_err();
}

#[test]
fn validate_prelude_text() {
    let node = PRELUDE_TSTR;
    gen_value("abc").validate(node).unwrap();
    gen_value(7).validate(node).unwrap_err();
}

#[test]
fn validate_literal_int() {
    let node = LITERAL_7;
    gen_value(7).validate(node).unwrap();
    gen_value(8).validate(node).unwrap_err();
}

#[test]
fn validate_literal_text() {
    let node = &Node::Literal(Literal::Text("abc".to_string()));
    Value::Text("abc".to_string()).validate(node).unwrap();
    Value::Integer(8).validate(node).unwrap_err();
}

#[test]
fn validate_choice() {
    let options = vec![1, 2, 3];
    let options = options
        .iter()
        .map(|n| Arc::new(Node::Literal(Literal::Int(*n))))
        .collect();
    let node = &Node::Choice(Choice { options });

    gen_value(1).validate(node).unwrap();
    gen_value(2).validate(node).unwrap();
    gen_value(3).validate(node).unwrap();
    gen_value(4).validate(node).unwrap_err();
    gen_value("abc").validate(node).unwrap_err();
}

#[test]
fn validate_map() {
    let kv_template = [
        ("Alice", PreludeType::Int),
        ("Bob", PreludeType::Int),
        ("Carol", PreludeType::Int),
    ];
    let kv_vec: VecNode = kv_template
        .iter()
        .map(|kv| {
            let key = kv.0;
            let key = Node::Literal(Literal::Text(key.to_string()));
            let value = kv.1;
            let value = Node::PreludeType(value);
            let occur = Occur { lower: 1, upper: 1 };
            Arc::new(Node::KeyValue(KeyValue::new(key, value, occur)))
        })
        .collect();
    let node = &Node::Map(Map { members: kv_vec });

    let value_template = [("Alice", 42), ("Bob", 43), ("Carol", 44)];
    let value: HashMap<&str, u32> = value_template.iter().cloned().collect();
    gen_value(value).validate(node).unwrap();

    // Missing the "Bob" key
    let value_template = [("Alice", 42), ("Carol", 44)];
    let value: HashMap<&str, u32> = value_template.iter().cloned().collect();
    let mut value = gen_value(value);
    value.validate(node).unwrap_err();

    // Add the "Bob" key with the wrong type.
    match &mut value {
        Value::Map(m) => {
            let key = gen_value("Bob");
            let value = gen_value("forty three");
            m.insert(key, value);
        }
        _ => unreachable!(),
    }
    value.validate(node).unwrap_err();

    // Has an extra "David" key
    let value_template = [("Alice", 42), ("Bob", 43), ("Carol", 44), ("David", 45)];
    let value: HashMap<&str, u32> = value_template.iter().cloned().collect();
    gen_value(value).validate(node).unwrap_err();

    // Attempt to match against a different (non-Map) Value type
    Value::Integer(1).validate(node).unwrap_err();
}

#[test]
fn validate_rule_ref() {
    let tmp_rule = Rule::new("seven");
    let arc = Arc::new(LITERAL_7.clone());
    tmp_rule.upgrade(&arc);
    let node2 = &Node::Rule(tmp_rule);
    gen_value(7).validate(node2).unwrap();
    gen_value(8).validate(node2).unwrap_err();
}
