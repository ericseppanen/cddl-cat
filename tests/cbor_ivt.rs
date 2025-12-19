#![cfg(feature = "ciborium")]

use cddl_cat::cbor::validate_cbor;
use cddl_cat::context::{tests::DummyContext, BasicContext};
use cddl_cat::ivt::*;
use cddl_cat::util::ValidateResult;
use ciborium::value::Integer;
use ciborium::Value;
use serde::ser::Serialize;
use std::collections::HashMap;

// Some Node structs to test with
static PRELUDE_INT: &Node = &Node::PreludeType(PreludeType::Int);
static PRELUDE_TSTR: &Node = &Node::PreludeType(PreludeType::Tstr);
static LITERAL_7: &Node = &Node::Literal(Literal::Int(7));

// Create a Value instance from anything that's serializable
fn gen_value<T: Serialize>(value: T) -> Value {
    // hack: serialize to a Vec and then deserialize to a Value
    let mut serialized = Vec::new();
    ciborium::into_writer(&value, &mut serialized).unwrap();
    ciborium::from_reader(serialized.as_slice()).unwrap()
}

trait TestValidate {
    fn test_validate(&self, node: &Node) -> ValidateResult;
}

impl TestValidate for Value {
    // Create a validation context and perform validation
    fn test_validate(&self, node: &Node) -> ValidateResult {
        // We don't need to do any Rule lookups, so an empty Context will do.
        let ctx = DummyContext;
        let rule_def = RuleDef {
            node: node.clone(),
            generic_parms: Vec::new(),
        };
        validate_cbor(&rule_def, self, &ctx)
    }
}

#[test]
fn validate_prelude_int() {
    let node = PRELUDE_INT;
    gen_value(7).test_validate(node).unwrap();
    gen_value("abc").test_validate(node).unwrap_err();
}

#[test]
fn validate_prelude_text() {
    let node = PRELUDE_TSTR;
    gen_value("abc").test_validate(node).unwrap();
    gen_value(7).test_validate(node).unwrap_err();
}

#[test]
fn validate_literal_int() {
    let node = LITERAL_7;
    gen_value(7).test_validate(node).unwrap();
    gen_value(8).test_validate(node).unwrap_err();
}

#[test]
fn validate_literal_text() {
    let node = &literal_text("abc");
    Value::Text("abc".to_string()).test_validate(node).unwrap();
    Value::Integer(Integer::from(8))
        .test_validate(node)
        .unwrap_err();
}

#[test]
fn validate_choice() {
    let options = [1, 2, 3];
    let options = options.iter().map(|n| literal_int(*n)).collect();
    let node = &Node::Choice(Choice { options });

    gen_value(1).test_validate(node).unwrap();
    gen_value(2).test_validate(node).unwrap();
    gen_value(3).test_validate(node).unwrap();
    gen_value(4).test_validate(node).unwrap_err();
    gen_value("abc").test_validate(node).unwrap_err();
}

#[test]
fn validate_map() {
    let kv_template = [
        ("Alice", PreludeType::Int),
        ("Bob", PreludeType::Int),
        ("Carol", PreludeType::Int),
    ];
    let kv_vec: Vec<Node> = kv_template
        .iter()
        .map(|kv| {
            let key = literal_text(kv.0);
            let value = Node::PreludeType(kv.1);
            let cut = true;
            Node::KeyValue(KeyValue::new(key, value, cut))
        })
        .collect();
    let node = &Node::Map(Map { members: kv_vec });

    let value_template = [("Alice", 42), ("Bob", 43), ("Carol", 44)];
    let value: HashMap<&str, u32> = value_template.iter().cloned().collect();
    gen_value(value).test_validate(node).unwrap();

    // Missing the "Bob" key
    let value_template = [("Alice", 42), ("Carol", 44)];
    let value: HashMap<&str, u32> = value_template.iter().cloned().collect();
    let mut value = gen_value(value);
    value.test_validate(node).unwrap_err();

    // Add the "Bob" key with the wrong type.
    match &mut value {
        Value::Map(m) => {
            let key = gen_value("Bob");
            let value = gen_value("forty three");
            m.push((key, value));
        }
        _ => unreachable!(),
    }
    value.test_validate(node).unwrap_err();

    // Has an extra "David" key
    let value_template = [("Alice", 42), ("Bob", 43), ("Carol", 44), ("David", 45)];
    let value: HashMap<&str, u32> = value_template.iter().cloned().collect();
    gen_value(value).test_validate(node).unwrap_err();

    // Attempt to match against a different (non-Map) Value type
    Value::Integer(Integer::from(1))
        .test_validate(node)
        .unwrap_err();
}

#[test]
fn validate_rule_ref() {
    let mut rules = RulesByName::new();
    rules.insert(
        "seven".to_string(),
        RuleDef {
            generic_parms: Vec::new(),
            node: LITERAL_7.clone(),
        },
    );

    let ctx = BasicContext::new(rules);
    let node = Node::Rule(Rule::new_name("seven"));
    let rule_def = RuleDef {
        node,
        generic_parms: Vec::new(),
    };

    validate_cbor(&rule_def, &gen_value(7), &ctx).unwrap();
    validate_cbor(&rule_def, &gen_value(8), &ctx).unwrap_err();
}
