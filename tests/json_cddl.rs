#![cfg(feature = "serde_json")]

use cddl_cat::json::validate_json_str;
use cddl_cat::util::ErrorMatch;
use serde::{Deserialize, Serialize};

#[test]
fn validate_json_null() {
    let cddl_input = r#"thing = nil"#;
    validate_json_str("thing", cddl_input, "null").unwrap();
    validate_json_str("thing", cddl_input, "0").unwrap_err();
    validate_json_str("thing", cddl_input, "false").unwrap_err();
}

#[test]
fn validate_json_bool() {
    let cddl_input = r#"thing = true"#;
    validate_json_str("thing", cddl_input, "true").unwrap();
    validate_json_str("thing", cddl_input, "false").unwrap_err();
    validate_json_str("thing", cddl_input, "null").unwrap_err();
}

#[test]
fn validate_json_float() {
    let cddl_input = r#"thing = 0.0"#;
    validate_json_str("thing", cddl_input, "0.0").unwrap();
    validate_json_str("thing", cddl_input, "1.0").unwrap_err();

    let cddl_input = r#"thing = float"#;
    validate_json_str("thing", cddl_input, "1.0").unwrap();
    validate_json_str("thing", cddl_input, "1e5").unwrap();
    validate_json_str("thing", cddl_input, "1e300").unwrap();

    let cddl_input = r#"thing = float16"#;
    validate_json_str("thing", cddl_input, "1.0").unwrap();

    // "Too small" floats should not cause a validation error.
    // JSON doesn't preserve the original size.
    let cddl_input = r#"thing = float32"#;
    validate_json_str("thing", cddl_input, "1.0").unwrap();
    validate_json_str("thing", cddl_input, "1e5").unwrap();

    let cddl_input = r#"thing = float64"#;
    validate_json_str("thing", cddl_input, "1.0").unwrap();
    validate_json_str("thing", cddl_input, "1e300").unwrap();

    // TODO: check that large floats don't validate against a smaller size.
    // We could try converting f64 to f23 and back to see if it changes.
}

#[test]
fn validate_json_choice() {
    let cddl_input = r#"thing = 23 / 24"#;
    validate_json_str("thing", cddl_input, "23").unwrap();
    validate_json_str("thing", cddl_input, "24").unwrap();

    let cddl_input = r#"thing = (foo // bar) foo = (int / float) bar = tstr"#;
    validate_json_str("thing", cddl_input, "23").unwrap();
    validate_json_str("thing", cddl_input, "1.0").unwrap();
    validate_json_str("thing", cddl_input, r#""JSON""#).unwrap();
    validate_json_str("thing", cddl_input, "true").unwrap_err();
}

#[test]
fn validate_json_integer() {
    let cddl_input = r#"thing = 1"#;
    validate_json_str("thing", cddl_input, "null").unwrap_err();
    validate_json_str("thing", cddl_input, "1.0").unwrap_err();
    validate_json_str("thing", cddl_input, "true").unwrap_err();
    let cddl_input = r#"thing = int"#;
    validate_json_str("thing", cddl_input, "0").unwrap();
    validate_json_str("thing", cddl_input, "24").unwrap();
    validate_json_str("thing", cddl_input, "-1000").unwrap();
    validate_json_str("thing", cddl_input, "1.0").unwrap_err();
    let cddl_input = r#"thing = uint"#;
    validate_json_str("thing", cddl_input, "0").unwrap();
    validate_json_str("thing", cddl_input, "24").unwrap();
    validate_json_str("thing", cddl_input, "-1000").unwrap_err();
    let cddl_input = r#"thing = nint"#;
    validate_json_str("thing", cddl_input, "-1000").unwrap();
    validate_json_str("thing", cddl_input, "0").unwrap_err();
    validate_json_str("thing", cddl_input, "24").unwrap_err();
}

#[test]
fn validate_json_textstring() {
    // "tstr" and "text" mean the same thing.
    for cddl_input in [r#"thing = tstr"#, r#"thing = text"#].iter() {
        validate_json_str("thing", cddl_input, r#""""#).unwrap();
        validate_json_str("thing", cddl_input, r#""JSON""#).unwrap();
        validate_json_str("thing", cddl_input, r#""水""#).unwrap();
    }
}

#[test]
fn validate_json_array() {
    let cddl_input = r#"thing = []"#;
    validate_json_str("thing", cddl_input, "[]").unwrap();
    validate_json_str("thing", cddl_input, "null").unwrap_err();
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap_err();

    let cddl_input = r#"thing = [1, 2, 3]"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap();
}

// These data structures exist so that we can serialize some more complex
// beyond the RFC examples.
#[derive(Debug, Serialize, Deserialize)]
struct PersonStruct {
    name: String,
    age: u32,
}

#[derive(Debug, Serialize, Deserialize)]
struct PersonTuple(String, u32);

#[derive(Debug, Serialize, Deserialize)]
struct BackwardsTuple(u32, String);

#[derive(Debug, Serialize, Deserialize)]
struct LongTuple(String, u32, u32);

#[derive(Debug, Serialize, Deserialize)]
struct ShortTuple(String);

#[derive(Debug, Serialize, Deserialize)]
struct KitchenSink(String, u32, f64, bool);

#[test]
fn validate_json_homogenous_array() {
    let cddl_input = r#"thing = [* int]"#; // zero or more
    validate_json_str("thing", cddl_input, "[]").unwrap();
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap();
    let cddl_input = r#"thing = [+ int]"#; // one or more
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap();
    validate_json_str("thing", cddl_input, "[]").unwrap_err();
    let cddl_input = r#"thing = [? int]"#; // zero or one
    validate_json_str("thing", cddl_input, "[]").unwrap();
    let json_str = serde_json::to_string(&[42]).unwrap();
    validate_json_str("thing", cddl_input, &json_str).unwrap();
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap_err();

    let cddl_input = r#"thing = [* tstr]"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap_err();

    // Alias type.  Note the rule we want to validate must come first.
    let cddl_input = r#"thing = [* zipcode]  zipcode = int"#;
    validate_json_str("thing", cddl_input, "[]").unwrap();
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap();
}

#[test]
fn validate_json_array_groups() {
    let cddl_input = r#"thing = [int, (int, int)]"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap();

    let cddl_input = r#"thing = [(int, int, int)]"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap();

    // Consume values in groups of one, an arbitrary number of times.
    let cddl_input = r#"thing = [* (int)]"#;
    validate_json_str("thing", cddl_input, "[]").unwrap();
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap();

    // Consume values in groups of three, an arbitrary number of times.
    let cddl_input = r#"thing = [* (int, int, int)]"#;
    validate_json_str("thing", cddl_input, "[]").unwrap();
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap();

    // Consume values in groups of two, an arbitrary number of times.
    let cddl_input = r#"thing = [* (int, int)]"#;
    validate_json_str("thing", cddl_input, "[]").unwrap();
    // Shouldn't match because three doesn't go into two evenly.
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap_err();

    let cddl_input = r#"thing = [a: int, b: int, bar] bar = (c: int)"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap();

    let cddl_input = r#"thing = [a: int, (bar)] bar = (b: int, c: int)"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap();

    // This is incorrectly constructed, because this is a key-value with
    // a group name where the value should be.
    let cddl_input = r#"thing = [a: int, b: bar] bar = (b: int, c: int)"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap_err();
}

#[test]
fn validate_json_array_unwrap() {
    // unwrap something into the head of an array
    let cddl_input = r#"header = [a: int, b: int] thing = [~header c: int]"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap();
    validate_json_str("thing", cddl_input, "[]").unwrap_err();
    // unwrap something into the tail of an array
    let cddl_input = r#"footer = [a: int, b: int] thing = [c: int ~footer]"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap();

    // unwrap something into the middle of an array
    let cddl_input = r#"middle = [int] thing = [a: int, ~middle, c: int]"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap();

    // add an extra rule redirection while unwrapping
    let cddl_input = r#"foo = int middle = [foo] thing = [a: int, ~middle, c: int]"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap();

    // Fail if we find too few items.
    let cddl_input = r#"header = [a: int] thing = [~header, c: int]"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap_err();
    let cddl_input = r#"footer = [a: int] thing = [c: int, ~footer]"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap_err();

    // Fail if we don't find enough matching items while unwrapping.
    let cddl_input = r#"footer = [a: int, b: int] thing = [c: int, d: int, ~footer]"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap_err();

    // Fail if the unwrapped name doesn't resolve.
    let cddl_input = r#"thing = [c: int ~footer]"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap_err();

    // Unwrapping a map into an array isn't allowed.
    let cddl_input = r#"header = {a: int, b: int} thing = [~header c: int]"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap_err();
}

#[test]
fn validate_json_array_record() {
    let cddl_input = r#"thing = [a: int, b: int, c: int]"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap();
    validate_json_str("thing", cddl_input, "[]").unwrap_err();

    let cddl_input = r#"thing = [a: int, b: int, c: foo] foo = int"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap();

    let cddl_input = r#"thing = [int, int, int]"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap();
    validate_json_str("thing", cddl_input, "[]").unwrap_err();

    let cddl_input = r#"thing = [a: tstr, b: int]"#;

    let input = PersonTuple("Alice".to_string(), 42);
    let json_str = serde_json::to_string(&input).unwrap();
    validate_json_str("thing", cddl_input, &json_str).unwrap();

    let input = BackwardsTuple(43, "Carol".to_string());
    let json_str = serde_json::to_string(&input).unwrap();
    validate_json_str("thing", cddl_input, &json_str).unwrap_err();

    let input = LongTuple("David".to_string(), 44, 45);
    let json_str = serde_json::to_string(&input).unwrap();
    validate_json_str("thing", cddl_input, &json_str).unwrap_err();

    let input = ShortTuple("Eve".to_string());
    let json_str = serde_json::to_string(&input).unwrap();
    validate_json_str("thing", cddl_input, &json_str).unwrap_err();

    let cddl_input = r#"thing = [a: tstr, b: uint, c: float, d: bool]"#;

    let input = KitchenSink("xyz".to_string(), 17, 9.9, false);
    let json_str = serde_json::to_string(&input).unwrap();
    validate_json_str("thing", cddl_input, &json_str).unwrap();

    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap_err();
}

#[test]
fn validate_json_map_unwrap() {
    let input = PersonStruct {
        name: "Bob".to_string(),
        age: 43,
    };
    let json_str = serde_json::to_string(&input).unwrap();
    let cddl_input = r#"thing = {name: tstr, ~agroup} agroup = {age: int}"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap();

    // Unwrapping an array into a map isn't allowed.
    let cddl_input = r#"thing = {name: tstr, ~agroup} agroup = [age: int]"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap_err();
}

#[test]
fn validate_json_map_group() {
    let input = PersonStruct {
        name: "Bob".to_string(),
        age: 43,
    };
    let json_str = serde_json::to_string(&input).unwrap();
    let cddl_input = r#"thing = {name: tstr, agroup} agroup = (age: int)"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap();

    let cddl_input = r#"thing = {agroup} agroup = (age: int, name: tstr)"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap();

    let cddl_input = r#"thing = {((agroup))} agroup = (age: int, name: tstr)"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap();

    let cddl_input = r#"thing = {agroup empty} agroup = (age: int, name: tstr) empty = ()"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap();

    let cddl_input =
        r#"thing = {agroup maybe} agroup = (age: int, name: tstr) maybe = (? minor: bool)"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap();

    let cddl_input = r#"thing = {name: tstr, agroup} agroup = (wrong: int)"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap_err();

    let cddl_input = r#"thing = {name: tstr, agroup} agroup = (age: bool)"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap_err();
}

#[test]
fn validate_json_map() {
    let input = PersonStruct {
        name: "Bob".to_string(),
        age: 43,
    };
    let json_str = serde_json::to_string(&input).unwrap();
    let cddl_input = r#"thing = {name: tstr, age: int}"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap();

    let cddl_input = r#"thing = {name: tstr, ? age: int}"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap();

    // Ensure that keys are optional if the occurrence is "?" or "*"
    // and required if the occurrence is "+"
    let cddl_input = r#"thing = {name: tstr, age: int, ? minor: bool}"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap();
    let cddl_input = r#"thing = {name: tstr, age: int, * minor: bool}"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap();
    let cddl_input = r#"thing = {name: tstr, age: int, + minor: bool}"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap_err();

    let cddl_input = r#"thing = {name: tstr, age: tstr}"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap_err();

    let cddl_input = r#"thing = {name: tstr}"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap_err();

    // "* keytype => valuetype" is the expected syntax for collecting
    // any remaining key/value pairs of the expected type.
    let cddl_input = r#"thing = {* tstr => any}"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap();
    let cddl_input = r#"thing = {name: tstr, * tstr => any}"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap();
    let cddl_input = r#"thing = {name: tstr, age: int, * tstr => any}"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap();
    let cddl_input = r#"thing = {+ tstr => any}"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap();

    // Should fail because the JSON input has two entries that can't be
    // collected because the key type doesn't match.
    let cddl_input = r#"thing = {* int => any}"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap_err();

    let cddl_input = r#"thing = {name: tstr, age: int, minor: bool}"#;
    validate_json_str("thing", cddl_input, &json_str).unwrap_err();

    let cddl_input = r#"thing = {x: int, y: int, z: int}"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap_err();
}

#[test]
fn validate_json_map_cut() {
    let json_str = r#"{ "foo": "not-an-int" }"#;

    // This uses non-cut semantics: the "foo" key matches, but because the value
    // doesn't match we allow "foo" to match the second member instead.
    let cddl = r#"
        thing = {
            ? "foo" => int,  ; non-cut is the default for "=>"
            tstr => tstr,
        }"#;
    validate_json_str("thing", cddl, &json_str).unwrap();

    // This uses cut semantics: the "foo" key matches, but because the value
    // doesn't match we prevent it from matching any later rules.
    let cddl = r#"
        thing = {
            ? "foo" ^ => int,  ; cut is indicated by "^"
            tstr => tstr,
        }"#;
    let err = validate_json_str("thing", cddl, &json_str).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected int)");

    // Only "=>" can ever be non-cut.  Members using ":" always get
    // cut semantics.
    let cddl = r#"
        thing = {
            ? "foo": int,  ; cut is implied by ":"
            tstr => tstr,
        }"#;
    validate_json_str("thing", cddl, &json_str).unwrap_err();

    // Just a sanity check to ensure that non-cut matches work.
    let json_str = r#"{ "foo": 17, "bar": "baz" }"#;
    let cddl = r#"
        thing = {
            ? "foo" => int,
            tstr => tstr,
        }"#;
    validate_json_str("thing", cddl, &json_str).unwrap();

    // Same as the previous, but with the catch-all statement first.
    let cddl = r#"
        thing = {
            tstr => tstr,
            ? "foo" => int,
        }"#;
    validate_json_str("thing", cddl, &json_str).unwrap();

    // It's not really possible to enforce cut semantics on choices, because
    // in a map, choices between key-value pairs are represented as groups.
    // We want the "cut" to end at the group boundary, so that things like
    // this can work:
    //
    // palette_entry = (color: tstr, position: int)
    // rgba = (color: int, alpha: int)
    // { palette_entry // rgba }
    // Each map is unambiguous by itself; we shouldn't fail the second group
    // because the first group happened to use "color" to mean a different
    // thing.
    //
    // Make sure this decision sticks, at least until we change that policy.
    let cddl = r#"thing = { "foo": int // tstr => tstr }"#;
    let json_str = r#"{ "foo": "not-int" }"#;
    validate_json_str("thing", cddl, &json_str).unwrap();

    // This example should fail; the cut semantics should cause the non-
    // matching key-value pair to be ignored from further match consideration.
    // Depending on the order we inspect the map, we risk validating this JSON
    // because the "zzz" failure may only terminate the occurrence; we need to
    // ensure it fails the validation of the entire map.
    let json_str = r#"{ "aaa": 17, "zzz": "baz" }"#;
    let cddl = r#"thing = {* tstr ^ => int }"#;
    let err = validate_json_str("thing", cddl, &json_str).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected int)");
}

#[derive(Debug, Serialize)]
struct StreetNumber {
    street: String,
    number: u32,
    name: String,
    zip_code: u32,
}

#[derive(Debug, Serialize)]
struct POBox {
    po_box: u32,
    name: String,
    zip_code: u32,
}

#[derive(Debug, Serialize)]
struct Pickup {
    per_pickup: bool,
}

#[test]
fn validate_choice_example() {
    // This is an example from RFC8610 2.2.2
    // The only modification from the RFC example is to substitute "_" for "-" in barewords,
    // for compatibility with serde_json.
    let cddl_input = r#"
        address = { delivery }

        delivery = (
        street: tstr, ? number: uint, city //
        po_box: uint, city //
        per_pickup: true )

        city = (
        name: tstr, zip_code: uint
        )"#;

    let input = POBox {
        po_box: 101,
        name: "San Francisco".to_string(),
        zip_code: 94103,
    };
    let json_str = serde_json::to_string(&input).unwrap();
    validate_json_str("address", cddl_input, &json_str).unwrap();

    let input = StreetNumber {
        street: "Eleventh St.".to_string(),
        number: 375,
        name: "San Francisco".to_string(),
        zip_code: 94103,
    };
    let json_str = serde_json::to_string(&input).unwrap();
    validate_json_str("address", cddl_input, &json_str).unwrap();

    let json_str = r#"{
        "street": "Eleventh St.",
        "name": "San Francisco",
        "zip_code": 94103
    }"#;
    validate_json_str("address", cddl_input, &json_str).unwrap();

    let input = Pickup { per_pickup: true };
    let json_str = serde_json::to_string(&input).unwrap();
    validate_json_str("address", cddl_input, &json_str).unwrap();
}

#[test]
fn json_generic_basic() {
    let cddl_input = r#"identity<T> = T  thing = identity<int>"#;
    validate_json_str("thing", cddl_input, "0").unwrap();
    validate_json_str("thing", cddl_input, r#""abc""#).err_mismatch();
    validate_json_str("identity", cddl_input, "0").err_generic();

    let cddl_input = r#"double<T> = (T, T) thing = [int, double<int>]"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").unwrap();
    validate_json_str("thing", cddl_input, "[1.0, 2, 3]").err_mismatch();
    validate_json_str("thing", cddl_input, "[1, 2, 3.0]").err_mismatch();

    let cddl_input = "message<t, v> = [t, v]  thing = message<tstr, int>";
    validate_json_str("thing", cddl_input, r#"["JSON", 123]"#).unwrap();
    validate_json_str("thing", cddl_input, r#"[123, "JSON"]"#).err_mismatch();

    let cddl_input = r#"identity<T> = T   thing = [identity<(int)>]"#;
    validate_json_str("thing", cddl_input, "[1]").unwrap();

    let cddl_input = r#"identity<T> = (T)   thing = [identity<(int)>]"#;
    validate_json_str("thing", cddl_input, "[1]").unwrap();

    let cddl_input = r#"double<T> = (T, T)   thing = [double<[int, int]>]"#;
    validate_json_str("thing", cddl_input, "[[1, 2], [3, 4]]").unwrap();

    let cddl_input = r#"double<T> = (T, T)   identity<I> = I   thing = [double<identity<int>>]"#;
    validate_json_str("thing", cddl_input, "[1, 2]").unwrap();
}

#[test]
fn json_generic_map() {
    let cddl_input = "identity<T> = T  thing = {identity<tstr> => identity<int>}";
    validate_json_str("thing", cddl_input, r#" { "abc": 0 } "#).unwrap();

    // The "key:value" syntax only allows barewords or values; it doesn't allow
    // generic arguments.
    let cddl_input = "identity<T> = T  thing = {identity<tstr>: identity<int>}";
    validate_json_str("thing", cddl_input, r#" { "abc": 0 } "#).err_parse();
}

#[test]
fn json_generic_occurrence() {
    let cddl_input = r#"one_or_more<T> = (+ T)  thing = [one_or_more<int>]"#;
    validate_json_str("thing", cddl_input, "[4]").unwrap();
    validate_json_str("thing", cddl_input, "[4, 5, 6]").unwrap();
    validate_json_str("thing", cddl_input, "[]").err_mismatch();

    let cddl_input = r#"three_to_five<T> = (3*5 T)  thing = [three_to_five<tstr>]"#;
    validate_json_str("thing", cddl_input, r#"["one", "two"]"#).err_mismatch();
    validate_json_str("thing", cddl_input, r#"["one", "two", "three"]"#).unwrap();
    validate_json_str("thing", cddl_input, r#"["one", "two", "three", "four"]"#).unwrap();
    validate_json_str("thing", cddl_input, r#"["1st","2nd","3rd","4th","5th"]"#).unwrap();
    validate_json_str("thing", cddl_input, r#"["a","b","c","d","e","f"]"#).err_mismatch();
}

#[test]
fn json_generic_socket_example() {
    // TODO: use e.g. "bstr .size 4" once the .size control operator
    // is supported.
    let cddl_input = r#"
        port = uint
        socket_addr<HOST> = (HOST, port)
        hostname = tstr
        ipv4_addr = [uint, uint, uint, uint]
        ipv4_host = hostname / ipv4_addr
        ipv4_socket = socket_addr<ipv4_host>
        sock_struct = { name: tstr, sock: [ipv4_socket]}
    "#;
    validate_json_str(
        "sock_struct",
        cddl_input,
        r#" { "name": "foo", "sock": ["foo.dev", 8080]} "#,
    )
    .unwrap();
    validate_json_str(
        "sock_struct",
        cddl_input,
        r#" { "name": "foo", "sock": [[10,0,0,1], 8080]} "#,
    )
    .unwrap();
}

#[test]
fn json_generic_name_overlap() {
    let cddl_input = r#"
        IP = [uint, uint, uint, uint]
        PORT = uint
        socket = [IP, PORT]
        name = tstr
        conn<IP> = [name, socket, IP] ; This is the generic parameter named IP
        iptype = "v4" / "v6"
        thing = conn<iptype>          ; This is the rule named IP
    "#;
    validate_json_str("thing", cddl_input, r#"["foo", [[10,0,0,1], 8080], "v4"]"#).unwrap();
}

#[test]
fn json_generic_nested() {
    let cddl_input = r#"
        double<X> = (X, X)
        triple<Y> = (Y, Y, Y)
        sextuple<Z> = double<triple<Z>>
        thing = [sextuple<int>]
    "#;
    validate_json_str("thing", cddl_input, "[1, 2, 3, 4, 5, 6]").unwrap();
    validate_json_str("thing", cddl_input, "[[1, 2, 3], [4, 5, 6]]").err_mismatch();
    validate_json_str("thing", cddl_input, "[1, 2, 3, 4, 5]").err_mismatch();
    validate_json_str("thing", cddl_input, "[1, 2, 3, 4, 5, 6, 7]").err_mismatch();

    // Check to see if we get confused when the same generic parameter name
    // gets used across multiple levels.
    let cddl_input = r#"
        double<T> = (T, T)
        triple<T> = (T, T, T)
        sextuple<T> = double<triple<T>>
        thing = [sextuple<int>]
    "#;
    validate_json_str("thing", cddl_input, "[1, 2, 3, 4, 5, 6]").unwrap();

    // Verify that we can't access generic names in places we shouldn't.
    let cddl_input = r#"
        double<X> = (X, X)
        triple<Y> = (X, Y, Y)
        sextuple<Z> = double<triple<Z>>
        thing = [sextuple<int>]
    "#;
    validate_json_str("thing", cddl_input, "[1, 2, 3, 4, 5, 6]").err_missing_rule();

    let cddl_input = r#"
        double<X> = (X, X)
        triple<Y> = (Z, Y, Y)
        sextuple<Z> = double<triple<Z>>
        thing = [sextuple<int>]
    "#;
    validate_json_str("thing", cddl_input, "[1, 2, 3, 4, 5, 6]").err_missing_rule();
}

#[test]
fn json_generic_rfc8610() {
    // Generic examples from RFC8610
    let cddl_input = r#"
        messages = message<"reboot", "now"> / message<"sleep", 1..100>
        message<t, v> = {type: t, value: v}
    "#;
    validate_json_str("messages", cddl_input, r#"{"type":"reboot","value":"now"}"#).unwrap();
    validate_json_str("messages", cddl_input, r#"{"type":"sleep", "value":15}"#).unwrap();
    validate_json_str("messages", cddl_input, r#"{"type":"no", "value": "now"}"#).err_mismatch();
    validate_json_str("messages", cddl_input, r#"{"type":"sleep", "value":150}"#).err_mismatch();
}

#[test]
fn json_generic_malformed() {
    // Obviously bad rule lookup
    let cddl_input = r#"double<T> = (T, T) thing = [T, double<int>]"#;
    validate_json_str("thing", cddl_input, "[1, 2, 3]").err_missing_rule();

    // Missing parameters
    let cddl_input = r#"double<T> = (T, T) thing = [double]"#;
    validate_json_str("thing", cddl_input, "[1, 2]").err_generic();

    // Wrong number of parameters
    let cddl_input = r#"pair<T, U> = (T, U) thing = [pair<int>]"#;
    validate_json_str("thing", cddl_input, "[1, 2]").err_generic();
    let cddl_input = r#"pair<T, U> = (T, U) thing = [pair<int, int, int>]"#;
    validate_json_str("thing", cddl_input, "[1, 2]").err_generic();
}

#[test]
fn json_control_size() {
    let cddl_input = r#"thing = uint .size 3"#;
    validate_json_str("thing", cddl_input, "0").unwrap();
    validate_json_str("thing", cddl_input, "256").unwrap();
    validate_json_str("thing", cddl_input, "16777215").unwrap();
    validate_json_str("thing", cddl_input, "16777216").err_mismatch();
    validate_json_str("thing", cddl_input, "-256").err_mismatch();

    // indirection
    let cddl_input = r#"limit = 3  numb = uint  thing = numb .size limit"#;
    validate_json_str("thing", cddl_input, "0").unwrap();
    validate_json_str("thing", cddl_input, "256").unwrap();
    validate_json_str("thing", cddl_input, "16777215").unwrap();
    validate_json_str("thing", cddl_input, "16777216").err_mismatch();
    validate_json_str("thing", cddl_input, "-256").err_mismatch();

    let cddl_input = r#"thing = tstr .size 10"#;
    validate_json_str("thing", cddl_input, r#""""#).unwrap();
    validate_json_str("thing", cddl_input, r#""JSON""#).unwrap();
    validate_json_str("thing", cddl_input, r#""水""#).unwrap();
    validate_json_str("thing", cddl_input, r#""水水水水""#).err_mismatch();
    validate_json_str("thing", cddl_input, r#""abcdefghij""#).unwrap();
    validate_json_str("thing", cddl_input, r#""abcdefghijk""#).err_mismatch();

    // .size is not allowed on signed integers.
    let cddl_input = r#"thing = int .size 3"#;
    validate_json_str("thing", cddl_input, "0").unwrap_err();

    // bad target node type
    let cddl_input = r#"thing = [uint] .size 3"#;
    validate_json_str("thing", cddl_input, "0").unwrap_err();

    // bad argument node type
    let cddl_input = r#"thing = uint .size 0.1"#;
    validate_json_str("thing", cddl_input, "0").unwrap_err();
}
