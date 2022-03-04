#![cfg(feature = "serde_cbor")]

use cddl_cat::cbor::validate_cbor_bytes;
use cddl_cat::util::ErrorMatch;
use cddl_cat::ValidateResult;
use serde::{Deserialize, Serialize};

#[rustfmt::skip] // allow arbitrary indents for readability
pub mod cbor {
    // Mostly example values from rfc7049 appendix A
    pub const BOOL_FALSE:   &[u8] = b"\xF4";
    pub const BOOL_TRUE:    &[u8] = b"\xF5";
    pub const NULL:         &[u8] = b"\xF6";
    pub const UNDEFINED:    &[u8] = b"\xF7";

    pub const INT_0:        &[u8] = b"\x00";
    pub const INT_1:        &[u8] = b"\x01";
    pub const INT_9:        &[u8] = b"\x09";
    pub const INT_23:       &[u8] = b"\x17";
    pub const INT_24:       &[u8] = b"\x18\x18";
    pub const INT_1T:       &[u8] = b"\x1b\x00\x00\x00\xe8\xd4\xa5\x10\x00";
    pub const NINT_1000:    &[u8] = b"\x39\x03\xe7";  // -1000

    pub const FLOAT_0_0:    &[u8] = b"\xf9\x00\x00";            // #7.25 (f16)
    pub const FLOAT_1_0:    &[u8] = b"\xf9\x3c\x00";            // #7.25 (f16)
    pub const FLOAT_1E5:    &[u8] = b"\xfa\x47\xc3\x50\x00";    // #7.26 (f32)
    pub const FLOAT_1E300:  &[u8] = b"\xfb\x7e\x37\xe4\x3c\x88\x00\x75\x9c"; // #7.27 (f64)

    pub const ARRAY_EMPTY:  &[u8] = b"\x80";              // []
    pub const ARRAY_123:    &[u8] = b"\x83\x01\x02\x03";  // [1,2,3]
    pub const ARRAY_12:     &[u8] = b"\x82\x01\x02";  // [1,2]
    pub const ARRAY_1_23_45:&[u8] = b"\x83\x01\x82\x02\x03\x82\x04\x05";  // [1, [2, 3], [4, 5]]

    pub const TEXT_EMPTY:   &[u8] = b"\x60";
    pub const TEXT_IETF:    &[u8] = b"\x64\x49\x45\x54\x46"; // "IETF"
    pub const TEXT_CJK:     &[u8] = b"\x63\xe6\xb0\xb4";     // "水"

    pub const BYTES_EMPTY:  &[u8] = b"\x40";
    pub const BYTES_1234:   &[u8] = b"\x44\x01\x02\x03\x04"; // hex 01020304

}

#[test]
fn validate_cbor_null() {
    let cddl_input = r#"thing = nil"#;
    validate_cbor_bytes("thing", cddl_input, cbor::NULL).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::INT_0).unwrap_err();
    let err = validate_cbor_bytes("thing", cddl_input, cbor::BOOL_FALSE).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected nil)");
}

#[test]
fn validate_cbor_bool() {
    let cddl_input = r#"thing = true"#;
    validate_cbor_bytes("thing", cddl_input, cbor::BOOL_TRUE).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::BOOL_FALSE).unwrap_err();
    let err = validate_cbor_bytes("thing", cddl_input, cbor::NULL).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected true)");
}

#[test]
fn validate_cbor_float() {
    let cddl_input = r#"thing = 0.0"#;
    validate_cbor_bytes("thing", cddl_input, cbor::FLOAT_0_0).unwrap();
    let err = validate_cbor_bytes("thing", cddl_input, cbor::FLOAT_1_0).unwrap_err();
    // FIXME: It's annoying that the rust float syntax can omit the decimal point.
    assert_eq!(err.to_string(), "Mismatch(expected 0)");

    let cddl_input = r#"thing = float"#;
    validate_cbor_bytes("thing", cddl_input, cbor::FLOAT_1_0).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::FLOAT_1E5).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::FLOAT_1E300).unwrap();

    let cddl_input = r#"thing = float16"#;
    validate_cbor_bytes("thing", cddl_input, cbor::FLOAT_1_0).unwrap();

    // "Too small" floats should not cause a validation error.
    // "Canonical CBOR" suggests that floats should be shrunk to the smallest
    // size that can represent the value.  So 1.0 can be stored in 16 bits,
    // even if the CDDL specifies float64.
    let cddl_input = r#"thing = float32"#;
    validate_cbor_bytes("thing", cddl_input, cbor::FLOAT_1_0).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::FLOAT_1E5).unwrap();

    let cddl_input = r#"thing = float64"#;
    validate_cbor_bytes("thing", cddl_input, cbor::FLOAT_1_0).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::FLOAT_1E300).unwrap();

    // TODO: check that large floats don't validate against a smaller size.
    // E.g. CBOR #7.27 (64-bit) shouldn't validate against "float16" or "float32".
}

#[test]
fn validate_cbor_choice() {
    let cddl_input = r#"thing = 23 / 24"#;
    validate_cbor_bytes("thing", cddl_input, cbor::INT_23).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::INT_24).unwrap();

    let cddl_input = r#"thing = (foo // bar) foo = (int / float) bar = tstr"#;
    validate_cbor_bytes("thing", cddl_input, cbor::INT_23).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::FLOAT_1_0).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::TEXT_IETF).unwrap();
    let err = validate_cbor_bytes("thing", cddl_input, cbor::BOOL_TRUE).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected choice of 2)");

    let cddl_input = r#"thing = (foo / bar) foo = (int / float) bar = tstr"#;
    validate_cbor_bytes("thing", cddl_input, cbor::INT_23).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::FLOAT_1_0).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::TEXT_IETF).unwrap();
    let err = validate_cbor_bytes("thing", cddl_input, cbor::BOOL_TRUE).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected choice of 2)");

    let cddl_input = r#"thing = (int / float // tstr / bstr)"#;
    validate_cbor_bytes("thing", cddl_input, cbor::INT_23).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::FLOAT_1_0).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::TEXT_IETF).unwrap();
    let err = validate_cbor_bytes("thing", cddl_input, cbor::BOOL_TRUE).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected choice of 2)");
}

#[test]
fn validate_cbor_integer() {
    let cddl_input = r#"thing = 1"#;
    validate_cbor_bytes("thing", cddl_input, cbor::INT_1).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::NULL).unwrap_err();
    validate_cbor_bytes("thing", cddl_input, cbor::FLOAT_1_0).unwrap_err();
    validate_cbor_bytes("thing", cddl_input, cbor::BOOL_TRUE).unwrap_err();
    let cddl_input = r#"thing = int"#;
    validate_cbor_bytes("thing", cddl_input, cbor::INT_0).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::INT_24).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::NINT_1000).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::FLOAT_1_0).unwrap_err();
    let cddl_input = r#"thing = uint"#;
    validate_cbor_bytes("thing", cddl_input, cbor::INT_0).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::INT_24).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::NINT_1000).unwrap_err();
    let cddl_input = r#"thing = nint"#;
    validate_cbor_bytes("thing", cddl_input, cbor::NINT_1000).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::INT_0).unwrap_err();
    validate_cbor_bytes("thing", cddl_input, cbor::INT_24).unwrap_err();
}

#[test]
fn validate_cbor_ranges() {
    let cddl_input = r#"thing = 1..5"#;
    validate_cbor_bytes("thing", cddl_input, cbor::INT_1).unwrap();
    let err = validate_cbor_bytes("thing", cddl_input, cbor::INT_24).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected 1..5)");

    let err = validate_cbor_bytes("thing", cddl_input, cbor::FLOAT_1_0).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected 1..5)");

    let cddl_input = r#"thing = 1..24"#;
    validate_cbor_bytes("thing", cddl_input, cbor::INT_24).unwrap();

    let cddl_input = r#"thing = 1...24"#;
    validate_cbor_bytes("thing", cddl_input, cbor::INT_23).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::INT_24).unwrap_err();

    let cddl_input = r#"thing = 1 .. 5.3"#;
    let err = validate_cbor_bytes("thing", cddl_input, cbor::INT_1).unwrap_err();
    assert_eq!(
        err.to_string(),
        "Structural(mismatched types on range operator)"
    );

    let cddl_input = r#"max=5 thing = 1..max"#;
    validate_cbor_bytes("thing", cddl_input, cbor::INT_1).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::INT_24).unwrap_err();

    let cddl_input = r#"thing = 0.9..1.2"#;
    validate_cbor_bytes("thing", cddl_input, cbor::FLOAT_1_0).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::FLOAT_0_0).unwrap_err();
    let err = validate_cbor_bytes("thing", cddl_input, cbor::INT_1).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected 0.9..1.2)");

    let cddl_input = r#"thing = 1..uint"#;
    let err = validate_cbor_bytes("thing", cddl_input, cbor::INT_1).unwrap_err();
    assert_eq!(err.to_string(), "Structural(bad type on range operator)");

    let cddl_input = r#"thing = 1..[5]"#;
    let err = validate_cbor_bytes("thing", cddl_input, cbor::INT_1).unwrap_err();
    assert_eq!(err.to_string(), "Structural(bad type on range operator)");
}

#[test]
fn validate_cbor_textstring() {
    // "tstr" and "text" mean the same thing.
    for cddl_input in [r#"thing = tstr"#, r#"thing = text"#].iter() {
        validate_cbor_bytes("thing", cddl_input, cbor::TEXT_EMPTY).unwrap();
        validate_cbor_bytes("thing", cddl_input, cbor::TEXT_IETF).unwrap();
        validate_cbor_bytes("thing", cddl_input, cbor::TEXT_CJK).unwrap();
        let err = validate_cbor_bytes("thing", cddl_input, cbor::BYTES_EMPTY).unwrap_err();
        assert_eq!(err.to_string(), "Mismatch(expected tstr)");
    }
}

#[test]
fn validate_cbor_bytestring() {
    // "bstr" and "bytes" mean the same thing.
    for cddl_input in [r#"thing = bstr"#, r#"thing = bytes"#].iter() {
        validate_cbor_bytes("thing", cddl_input, cbor::BYTES_EMPTY).unwrap();
        validate_cbor_bytes("thing", cddl_input, cbor::BYTES_1234).unwrap();
        validate_cbor_bytes("thing", cddl_input, cbor::TEXT_EMPTY).unwrap_err();
        let err = validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap_err();
        assert_eq!(err.to_string(), "Mismatch(expected bstr)");
    }

    let cddl_input = r#"thing = h'01020304'"#;
    validate_cbor_bytes("thing", cddl_input, cbor::BYTES_1234).unwrap();
}

#[test]
fn validate_cbor_array() {
    let cddl_input = r#"thing = []"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_EMPTY).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::NULL).unwrap_err();
    let err = validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected shorter array)");

    let cddl_input = r#"thing = [1, 2, 3]"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();
    let err = validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_EMPTY).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected array element 1)");
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
fn validate_cbor_homogenous_array() {
    let cddl_input = r#"thing = [* int]"#; // zero or more
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_EMPTY).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();

    let cddl_input = r#"thing = [+ int]"#; // one or more
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();
    let err = validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_EMPTY).unwrap_err();
    assert_eq!(
        err.to_string(),
        "Mismatch(expected more array element [+ Int])"
    );

    let cddl_input = r#"thing = [? int]"#; // zero or one
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_EMPTY).unwrap();
    let cbor_bytes = serde_cbor::to_vec(&[42]).unwrap();
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();
    let err = validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected shorter array)");

    let cddl_input = r#"thing = [* tstr]"#;
    let err = validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap_err();
    // FIXME: this error message is confusing.
    // Having consumed 0 tstr, we find that the array still has values.
    assert_eq!(err.to_string(), "Mismatch(expected shorter array)");

    // Alias type.
    let cddl_input = r#"thing = [* zipcode]  zipcode = int"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_EMPTY).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();
}

#[test]
fn validate_cbor_array_groups() {
    let cddl_input = r#"thing = [int, (int, int)]"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();

    // Naming a group causes it to be inlined.
    let cddl_input = r#"thing = [int, foo] foo = (int, int)"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_EMPTY).err_mismatch();
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_12).err_mismatch();

    let cddl_input = r#"thing = [(int, int, int)]"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();

    // Consume values in groups of one, an arbitrary number of times.
    let cddl_input = r#"thing = [* (int)]"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_EMPTY).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();

    // Consume values in groups of three, an arbitrary number of times.
    let cddl_input = r#"thing = [* (int, int, int)]"#;
    //validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_EMPTY).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();

    // Consume values in groups of two, an arbitrary number of times.
    let cddl_input = r#"thing = [* (int, int)]"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_EMPTY).unwrap();
    // Shouldn't match because three doesn't go into two evenly.
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).err_mismatch();

    let cddl_input = r#"thing = [a: int, b: int, bar] bar = (c: int)"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();

    let cddl_input = r#"thing = [a: int, (bar)] bar = (b: int, c: int)"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();

    // This is incorrectly constructed, because this is a key-value with
    // a group name where the value should be.
    let cddl_input = r#"thing = [a: int, b: bar] bar = (b: int, c: int)"#;
    let err = validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap_err();
    assert_eq!(err.to_string(), "Unsupported standalone group");

    // This is constructed to require backtracking by the validator:
    // `foo` will consume an int before failing; we need to rewind to
    // a previous state so that `bar` will match.
    let cddl_input = r#"thing = [int, (foo // bar)] foo = (int, tstr) bar = (int, int)"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();

    // Test nested groups with lots of backtracking.
    let cddl_input = r#"thing = [(int, (int, bool // (int, tstr // int, int)))]"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();
}

#[test]
fn validate_cbor_array_unwrap() {
    // unwrap something into the head of an array
    let cddl_input = r#"header = [a: int, b: int] thing = [~header c: int]"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_EMPTY).unwrap_err();

    // unwrap something into the tail of an array
    let cddl_input = r#"footer = [a: int, b: int] thing = [c: int ~footer]"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();

    // unwrap something into the middle of an array
    let cddl_input = r#"middle = [int] thing = [a: int, ~middle, c: int]"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();

    // add an extra rule redirection while unwrapping
    let cddl_input = r#"foo = int middle = [foo] thing = [a: int, ~middle, c: int]"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();

    // Fail if we find too few items.
    let cddl_input = r#"header = [a: int] thing = [~header, c: int]"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap_err();

    let cddl_input = r#"footer = [a: int] thing = [c: int, ~footer]"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap_err();

    // Fail if we don't find enough matching items while unwrapping.
    let cddl_input = r#"footer = [a: int, b: int] thing = [c: int, d: int, ~footer]"#;
    let err = validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected array element Int)");

    // Fail if the unwrapped name doesn't resolve.
    let cddl_input = r#"thing = [c: int ~footer]"#;
    let err = validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap_err();
    assert_eq!(err.to_string(), "MissingRule(footer)");

    // Unwrapping a map into an array isn't allowed.
    let cddl_input = r#"header = {a: int, b: int} thing = [~header c: int]"#;
    let err = validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected unwrap array)");
}

#[test]
fn validate_cbor_array_record() {
    let cddl_input = r#"thing = [a: int, b: int, c: int]"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_EMPTY).unwrap_err();

    let cddl_input = r#"thing = [a: int, b: int, c: foo] foo = int"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();

    let cddl_input = r#"thing = [int, int, int]"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_EMPTY).unwrap_err();

    let cddl_input = r#"thing = [a: tstr, b: int]"#;

    let input = PersonTuple("Alice".to_string(), 42);
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();

    let input = BackwardsTuple(43, "Carol".to_string());
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap_err();

    let input = LongTuple("David".to_string(), 44, 45);
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap_err();

    let input = ShortTuple("Eve".to_string());
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap_err();

    let cddl_input = r#"thing = [a: tstr, b: uint, c: float, d: bool]"#;

    let input = KitchenSink("xyz".to_string(), 17, 9.9, false);
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();

    // FIXME: there isn't any way at present to serialize a struct
    // into a CBOR array. See https://github.com/pyfisch/cbor/issues/107
    //let input = PersonStruct{name: "Bob".to_string(), age: 43};
    //let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    //validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();

    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap_err();
}

#[test]
fn validate_cbor_map_unwrap() {
    let input = PersonStruct {
        name: "Bob".to_string(),
        age: 43,
    };
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    let cddl_input = r#"thing = {name: tstr, ~agroup} agroup = {age: int}"#;
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();

    // Unwrapping an array into a map isn't allowed.
    let cddl_input = r#"thing = {name: tstr, ~agroup} agroup = [age: int]"#;
    let err = validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected unwrap map)");
}

#[test]
fn validate_cbor_map_group() {
    let input = PersonStruct {
        name: "Bob".to_string(),
        age: 43,
    };
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    let cddl_input = r#"thing = {name: tstr, agroup} agroup = (age: int)"#;
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();

    let cddl_input = r#"thing = {agroup} agroup = (age: int, name: tstr)"#;
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();

    let cddl_input = r#"thing = {((agroup))} agroup = (age: int, name: tstr)"#;
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();

    let cddl_input = r#"thing = {agroup empty} agroup = (age: int, name: tstr) empty = ()"#;
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();

    let cddl_input =
        r#"thing = {agroup maybe} agroup = (age: int, name: tstr) maybe = (? minor: bool)"#;
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();

    let cddl_input = r#"thing = {name: tstr, agroup} agroup = (wrong: int)"#;
    let err = validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap_err();
    assert_eq!(err.to_string(), r#"Mismatch(expected map{"wrong"})"#);

    let cddl_input = r#"thing = {name: tstr, agroup} agroup = (age: bool)"#;
    let err = validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected bool)");

    // This is constructed to require backtracking by the validator:
    // `foo` will consume `age` before failing; we need to rewind to
    // a previous state so that `bar` will match.
    let cddl_input =
        r#"thing = { foo // bar } foo = (name: tstr, age: bool) bar = (name: tstr, age: int)"#;
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();

    // Test nested groups with lots of backtracking.
    let cddl_input = r#"thing = { (name: tstr, photo: bstr //
                                  (name: tstr, fail: bool // name: tstr, age: int)) }"#;
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();

    // Test a group where none of the variants match.
    let cddl_input =
        r#"thing = { foo // bar } foo = (name: tstr, age: bool) bar = (name: tstr, age: float)"#;
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).err_mismatch();
}

#[test]
fn validate_cbor_map() {
    let input = PersonStruct {
        name: "Bob".to_string(),
        age: 43,
    };
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    let cddl_input = r#"thing = {name: tstr, age: int}"#;
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();

    let cddl_input = r#"thing = {name: tstr, ? age: int}"#;
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();

    // Ensure that keys are optional if the occurrence is "?" or "*"
    // and required if the occurrence is "+"
    let cddl_input = r#"thing = {name: tstr, age: int, ? minor: bool}"#;
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();
    let cddl_input = r#"thing = {name: tstr, age: int, * minor: bool}"#;
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();
    let cddl_input = r#"thing = {name: tstr, age: int, + minor: bool}"#;
    let err = validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap_err();
    assert_eq!(
        err.to_string(),
        r#"Mismatch(expected map{+ "minor": Bool}])"#
    );

    let cddl_input = r#"thing = {name: tstr, age: tstr}"#;
    let err = validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected tstr)");

    let cddl_input = r#"thing = {name: tstr}"#;
    let err = validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected shorter map)");

    // "* keytype => valuetype" is the expected syntax for collecting
    // any remaining key/value pairs of the expected type.
    let cddl_input = r#"thing = {* tstr => any}"#;
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();
    let cddl_input = r#"thing = {name: tstr, * tstr => any}"#;
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();
    let cddl_input = r#"thing = {name: tstr, age: int, * tstr => any}"#;
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();
    let cddl_input = r#"thing = {+ tstr => any}"#;
    validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap();

    // Should fail because the CBOR input has two entries that can't be
    // collected because the key type doesn't match.
    let cddl_input = r#"thing = {* int => any}"#;
    let err = validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap_err();
    assert_eq!(err.to_string(), "Mismatch(expected shorter map)");

    let cddl_input = r#"thing = {name: tstr, age: int, minor: bool}"#;
    let err = validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap_err();
    assert_eq!(err.to_string(), r#"Mismatch(expected map{"minor"})"#);

    let cddl_input = r#"thing = {x: int, y: int, z: int}"#;
    validate_cbor_bytes("thing", cddl_input, cbor::ARRAY_123).unwrap_err();
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
    // for compatibility with serde_cbor.
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
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    validate_cbor_bytes("address", cddl_input, &cbor_bytes).unwrap();

    let input = StreetNumber {
        street: "Eleventh St.".to_string(),
        number: 375,
        name: "San Francisco".to_string(),
        zip_code: 94103,
    };
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    validate_cbor_bytes("address", cddl_input, &cbor_bytes).unwrap();

    let input = Pickup { per_pickup: true };
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    validate_cbor_bytes("address", cddl_input, &cbor_bytes).unwrap();
}

#[test]
fn validate_choiceify_example() {
    // This is an example from RFC8610 2.2.2
    // The only modification from the RFC example is to substitute "_" for "-" in barewords,
    // for compatibility with serde_cbor.
    let cddl_input = r#"
        terminal-color = &basecolors
        basecolors = (
            black: 0,  red: 1,  green: 2,  yellow: 3,
            blue: 4,  magenta: 5,  cyan: 6,  white: 7,
        )
        extended-color = &(
            basecolors,
            orange: 8,  pink: 9,  purple: 10,  brown: 11,
        )"#;

    // This tests the & operator against a named rule
    validate_cbor_bytes("terminal-color", cddl_input, cbor::INT_1).unwrap();
    validate_cbor_bytes("terminal-color", cddl_input, cbor::INT_23).err_mismatch();

    // This tests the & operator against an inline group.
    validate_cbor_bytes("extended-color", cddl_input, cbor::INT_1).unwrap();
    validate_cbor_bytes("extended-color", cddl_input, cbor::INT_9).unwrap();
    validate_cbor_bytes("extended-color", cddl_input, cbor::INT_23).err_mismatch();
}

#[test]
fn test_fatal_propagation() {
    // Ensure that standalone choices can't conceal fatal errors.
    let cddl_input = r#"thing = (bad_rule / bool)"#;
    let err = validate_cbor_bytes("thing", cddl_input, cbor::BOOL_TRUE).unwrap_err();
    assert_eq!(err.to_string(), "MissingRule(bad_rule)");

    // Ensure that array choices can't conceal fatal errors.
    let cddl_input = r#"thing = [a: (bad_rule / tstr), b: int]"#;
    let input = PersonTuple("Alice".to_string(), 42);
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    let err = validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap_err();
    assert_eq!(err.to_string(), "MissingRule(bad_rule)");

    // Ensure that map choices can't conceal fatal errors.
    let input = PersonStruct {
        name: "Bob".to_string(),
        age: 43,
    };
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    let cddl_input = r#"thing = {name: (bad_rule / tstr), age: int}"#;
    let err = validate_cbor_bytes("thing", cddl_input, &cbor_bytes).unwrap_err();
    assert_eq!(err.to_string(), "MissingRule(bad_rule)");
}

#[test]
fn cbor_control_size() {
    let cddl_input = r#"thing = bstr .size 3"#;
    validate_cbor_bytes("thing", cddl_input, cbor::BYTES_EMPTY).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::BYTES_1234).err_mismatch();
    validate_cbor_bytes("thing", cddl_input, cbor::TEXT_EMPTY).err_mismatch();
    validate_cbor_bytes("thing", cddl_input, cbor::TEXT_CJK).err_mismatch();

    let cddl_input = r#"thing = tstr .size 3"#;
    validate_cbor_bytes("thing", cddl_input, cbor::TEXT_EMPTY).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::TEXT_CJK).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::TEXT_IETF).err_mismatch();
    validate_cbor_bytes("thing", cddl_input, cbor::BYTES_EMPTY).err_mismatch();

    let cddl_input = r#"thing = uint .size 3"#;
    validate_cbor_bytes("thing", cddl_input, cbor::INT_0).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::INT_24).unwrap();
    validate_cbor_bytes("thing", cddl_input, cbor::INT_1T).err_mismatch();
    validate_cbor_bytes("thing", cddl_input, cbor::NINT_1000).err_mismatch();
    validate_cbor_bytes("thing", cddl_input, cbor::TEXT_EMPTY).err_mismatch();

    let cddl_input = r#"thing = uint .size 5"#;
    validate_cbor_bytes("thing", cddl_input, cbor::INT_1T).unwrap();

    let cddl_input = r#"thing = uint .size 16"#;
    validate_cbor_bytes("thing", cddl_input, cbor::INT_1T).unwrap();

    let cddl_input = r#"thing = uint .size 999999"#;
    validate_cbor_bytes("thing", cddl_input, cbor::INT_1T).unwrap();

    let cddl_input = r#"thing = bstr .size 0.1"#;
    validate_cbor_bytes("thing", cddl_input, cbor::BYTES_EMPTY).unwrap_err();

    let cddl_input = r#"thing = bstr .size -1"#;
    validate_cbor_bytes("thing", cddl_input, cbor::BYTES_EMPTY).unwrap_err();
}

#[track_caller]
fn validate_cbor_tstr(name: &str, cddl: &str, input: &str) -> ValidateResult {
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    validate_cbor_bytes(name, cddl, &cbor_bytes)
}

#[test]
fn cbor_control_regexp() {
    // Should match strings that look like integers with no leading zeroes.
    let cddl_input = r#" nolz = tstr .regexp "^(0|[1-9][0-9]*)$" "#;
    validate_cbor_tstr("nolz", cddl_input, "0").unwrap();
    validate_cbor_tstr("nolz", cddl_input, "1").unwrap();
    validate_cbor_tstr("nolz", cddl_input, "20").unwrap();
    validate_cbor_tstr("nolz", cddl_input, "23").unwrap();
    validate_cbor_tstr("nolz", cddl_input, "123").unwrap();
    validate_cbor_tstr("nolz", cddl_input, "01").err_mismatch();
    validate_cbor_tstr("nolz", cddl_input, "0a").err_mismatch();
    validate_cbor_tstr("nolz", cddl_input, "").err_mismatch();

    // Any string that starts with "A"
    let cddl_input = r#" pat = tstr .regexp "^A" "#;
    validate_cbor_tstr("pat", cddl_input, "A").unwrap();
    validate_cbor_tstr("pat", cddl_input, "ABC").unwrap();
    validate_cbor_tstr("pat", cddl_input, "AAA").unwrap();
    validate_cbor_tstr("pat", cddl_input, "ZA").err_mismatch();
    validate_cbor_tstr("pat", cddl_input, "").err_mismatch();

    // A string with "BB" anywhere inside.
    let cddl_input = r#" pat = tstr .regexp "BB" "#;
    validate_cbor_tstr("pat", cddl_input, "BB").unwrap();
    validate_cbor_tstr("pat", cddl_input, "ABCBBA").unwrap();
    validate_cbor_tstr("pat", cddl_input, "ABCBA").err_mismatch();

    // bad target node type (bstr)
    let cddl_input = r#" pat = bstr .regexp "CCC" "#;
    validate_cbor_tstr("pat", cddl_input, "CCC").err_structural();

    // bad argument node type (integer)
    let cddl_input = r#" pat = tstr .regexp 1234 "#;
    validate_cbor_tstr("pat", cddl_input, "1234").err_structural();

    // This is an example from RFC8610 2.2.2
    let cddl_input = r#" nai = tstr .regexp "[A-Za-z0-9]+@[A-Za-z0-9]+(\\.[A-Za-z0-9]+)+" "#;
    validate_cbor_tstr("nai", cddl_input, "N1@CH57HF.4Znqe0.dYJRN.igjf").unwrap();
}
