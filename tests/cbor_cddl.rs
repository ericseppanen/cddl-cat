use cddl_validator::cbor::validate_cbor_cddl as validate_cbor_from_slice;
use cddl_validator::cbor::validate_cbor_cddl_named;
use serde::{Deserialize, Serialize};

#[rustfmt::skip] // allow arbitrary indents for readability
pub mod cbor {
    // example values from rfc7049 appendix A
    pub const BOOL_FALSE:   &[u8] = b"\xF4";
    pub const BOOL_TRUE:    &[u8] = b"\xF5";
    pub const NULL:         &[u8] = b"\xF6";
    pub const UNDEFINED:    &[u8] = b"\xF7";

    pub const INT_0:        &[u8] = b"\x00";
    pub const INT_1:        &[u8] = b"\x01";
    pub const INT_23:       &[u8] = b"\x17";
    pub const INT_24:       &[u8] = b"\x18\x18";
    pub const NINT_1000:    &[u8] = b"\x39\x03\xe7";  // -1000

    pub const FLOAT_0_0:    &[u8] = b"\xf9\x00\x00";            // #7.25 (f16)
    pub const FLOAT_1_0:    &[u8] = b"\xf9\x3c\x00";            // #7.25 (f16)
    pub const FLOAT_1E5:    &[u8] = b"\xfa\x47\xc3\x50\x00";    // #7.26 (f32)
    pub const FLOAT_1E300:  &[u8] = b"\xfb\x7e\x37\xe4\x3c\x88\x00\x75\x9c"; // #7.27 (f64)

    pub const ARRAY_EMPTY:  &[u8] = b"\x80";              // []
    pub const ARRAY_123:    &[u8] = b"\x83\x01\x02\x03";  // [1,2,3]
    pub const ARRAY_1_23_45:&[u8] = b"\x83\x01\x82\x02\x03\x82\x04\x05";  // [1, [2, 3], [4, 5]]

    pub const TEXT_EMPTY:   &[u8] = b"\x60";
    pub const TEXT_IETF:    &[u8] = b"\x64\x49\x45\x54\x46";
    pub const TEXT_CJK:     &[u8] = b"\x63\xe6\xb0\xb4";    // "水

    pub const BYTES_EMPTY:  &[u8] = b"\x40";
    pub const BYTES_1234:   &[u8] = b"\x44\x01\x02\x03\x04"; // hex 01020304

}

#[test]
fn validate_cbor_bool() {
    let cddl_input = r#"thing = true"#;
    validate_cbor_from_slice(cddl_input, cbor::BOOL_TRUE).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::BOOL_FALSE).unwrap_err();
    validate_cbor_from_slice(cddl_input, cbor::NULL).unwrap_err();
}

#[test]
fn validate_cbor_float() {
    let cddl_input = r#"thing = 0.0"#;
    validate_cbor_from_slice(cddl_input, cbor::FLOAT_0_0).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::FLOAT_1_0).unwrap_err();

    let cddl_input = r#"thing = float"#;
    validate_cbor_from_slice(cddl_input, cbor::FLOAT_1_0).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::FLOAT_1E5).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::FLOAT_1E300).unwrap();

    if false {
        // FIXME: no support yet for float16/32/64.
        let cddl_input = r#"thing = float16"#;
        validate_cbor_from_slice(cddl_input, cbor::FLOAT_1_0).unwrap();

        // "Too small" floats should not cause a validation error.
        // "Canonical CBOR" suggests that floats should be shrunk to the smallest
        // size that can represent the value.  So 1.0 can be stored in 16 bits,
        // even if the CDDL specifies float64.
        let cddl_input = r#"thing = float32"#;
        validate_cbor_from_slice(cddl_input, cbor::FLOAT_1_0).unwrap();
        validate_cbor_from_slice(cddl_input, cbor::FLOAT_1E5).unwrap();

        let cddl_input = r#"thing = float64"#;
        validate_cbor_from_slice(cddl_input, cbor::FLOAT_1_0).unwrap();
        validate_cbor_from_slice(cddl_input, cbor::FLOAT_1E300).unwrap();
    }
    // TODO: check that large floats don't validate against a smaller size.
    // E.g. CBOR #7.27 (64-bit) shouldn't validate against "float16" or "float32".
}

#[test]
fn validate_cbor_choice() {
    let cddl_input = r#"thing = 23 / 24"#;
    validate_cbor_from_slice(cddl_input, cbor::INT_23).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::INT_24).unwrap();

    let cddl_input = r#"thing = (foo // bar) foo = (int / float) bar = tstr"#;
    validate_cbor_cddl_named("thing", cddl_input, cbor::INT_23).unwrap();
    validate_cbor_cddl_named("thing", cddl_input, cbor::FLOAT_1_0).unwrap();
    validate_cbor_cddl_named("thing", cddl_input, cbor::TEXT_IETF).unwrap();
    validate_cbor_cddl_named("thing", cddl_input, cbor::BOOL_TRUE).unwrap_err();
}

#[test]
fn validate_cbor_integer() {
    let cddl_input = r#"thing = 1"#;
    validate_cbor_from_slice(cddl_input, cbor::NULL).unwrap_err();
    validate_cbor_from_slice(cddl_input, cbor::FLOAT_1_0).unwrap_err();
    validate_cbor_from_slice(cddl_input, cbor::BOOL_TRUE).unwrap_err();
    let cddl_input = r#"thing = int"#;
    validate_cbor_from_slice(cddl_input, cbor::INT_0).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::INT_24).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::NINT_1000).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::FLOAT_1_0).unwrap_err();
    let cddl_input = r#"thing = uint"#;
    validate_cbor_from_slice(cddl_input, cbor::INT_0).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::INT_24).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::NINT_1000).unwrap_err();
}

#[test]
fn validate_cbor_textstring() {
    let cddl_input = r#"thing = tstr"#;
    validate_cbor_from_slice(cddl_input, cbor::TEXT_EMPTY).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::TEXT_IETF).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::TEXT_CJK).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::BYTES_EMPTY).unwrap_err();
}

#[test]
fn validate_cbor_bytestring() {
    let cddl_input = r#"thing = bstr"#;
    validate_cbor_from_slice(cddl_input, cbor::BYTES_EMPTY).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::BYTES_1234).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::TEXT_EMPTY).unwrap_err();
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_123).unwrap_err();

    let cddl_input = r#"thing = h'01020304'"#;
    validate_cbor_from_slice(cddl_input, cbor::BYTES_1234).unwrap();
}

#[test]
#[ignore]
fn validate_cbor_array() {
    let cddl_input = r#"thing = []"#;
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_EMPTY).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::NULL).unwrap_err();
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_123).unwrap_err();

    let cddl_input = r#"thing = [1, 2, 3]"#;
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_123).unwrap();
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
#[ignore]
// I'm not sure how much sense this particular test makes.  My guess
// is that groups really only make sense in the context of an array
// or map, since I haven't found any way to encode a "group-like"
// sequence of data.  So the only thing that could even validate
// against a group directly would be a single value, and I'm not
// sure that really makes sense.
fn validate_cbor_group() {
    let cddl_input = r#"thing = (* int)"#;
    validate_cbor_from_slice(cddl_input, cbor::INT_0).unwrap();
}

#[test]
fn validate_cbor_homogenous_array() {
    let cddl_input = r#"thing = [* int]"#; // zero or more
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_EMPTY).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_123).unwrap();
    let cddl_input = r#"thing = [+ int]"#; // one or more
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_123).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_EMPTY).unwrap_err();
    let cddl_input = r#"thing = [? int]"#; // zero or one
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_EMPTY).unwrap();
    let cbor_bytes = serde_cbor::to_vec(&[42]).unwrap();
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_123).unwrap_err();

    let cddl_input = r#"thing = [* tstr]"#;
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_123).unwrap_err();

    // Alias type.  Note the rule we want to validate must come first.
    let cddl_input = r#"thing = [* zipcode]  zipcode = int"#;
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_EMPTY).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_123).unwrap();
}

#[test]
fn validate_cbor_array_groups() {
    let cddl_input = r#"thing = [int, (int, int)]"#;
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_123).unwrap();

    let cddl_input = r#"thing = [(int, int, int)]"#;
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_123).unwrap();

    // FIXME broken
    if false {
        // Consume values in groups of one, an arbitrary number of times.
        let cddl_input = r#"thing = [* (int)]"#;
        validate_cbor_from_slice(cddl_input, cbor::ARRAY_EMPTY).unwrap();
        validate_cbor_from_slice(cddl_input, cbor::ARRAY_123).unwrap();

        // Consume values in groups of three, an arbitrary number of times.
        let cddl_input = r#"thing = [* (int, int, int)]"#;
        validate_cbor_from_slice(cddl_input, cbor::ARRAY_EMPTY).unwrap();
        validate_cbor_from_slice(cddl_input, cbor::ARRAY_123).unwrap();

        // Consume values in groups of two, an arbitrary number of times.
        let cddl_input = r#"thing = [* (int, int)]"#;
        validate_cbor_from_slice(cddl_input, cbor::ARRAY_EMPTY).unwrap();
        // Shouldn't match because three doesn't go into two evenly.
        validate_cbor_from_slice(cddl_input, cbor::ARRAY_123).unwrap_err();
    }

    let cddl_input = r#"thing = [a: int, b: int, bar] bar = (c: int)"#;
    validate_cbor_cddl_named("thing", cddl_input, cbor::ARRAY_123).unwrap();

    let cddl_input = r#"thing = [a: int, (bar)] bar = (b: int, c: int)"#;
    validate_cbor_cddl_named("thing", cddl_input, cbor::ARRAY_123).unwrap();

    // This is incorrectly constructed, because this is a key-value with
    // a group name where the value should be.
    let cddl_input = r#"thing = [a: int, b: bar] bar = (b: int, c: int)"#;
    validate_cbor_cddl_named("thing", cddl_input, cbor::ARRAY_123).unwrap_err();
}

#[test]
fn validate_cbor_array_record() {
    let cddl_input = r#"thing = [a: int, b: int, c: int]"#;
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_123).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_EMPTY).unwrap_err();

    let cddl_input = r#"thing = [a: int, b: int, c: foo] foo = int"#;
    validate_cbor_cddl_named("thing", cddl_input, cbor::ARRAY_123).unwrap();

    let cddl_input = r#"thing = [int, int, int]"#;
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_123).unwrap();
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_EMPTY).unwrap_err();

    let cddl_input = r#"thing = [a: tstr, b: int]"#;

    let input = PersonTuple("Alice".to_string(), 42);
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap();

    let input = BackwardsTuple(43, "Carol".to_string());
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap_err();

    let input = LongTuple("David".to_string(), 44, 45);
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap_err();

    let input = ShortTuple("Eve".to_string());
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap_err();

    let cddl_input = r#"thing = [a: tstr, b: uint, c: float, d: bool]"#;

    let input = KitchenSink("xyz".to_string(), 17, 9.9, false);
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap();

    // FIXME: there isn't any way at present to serialize a struct
    // into a CBOR array. See https://github.com/pyfisch/cbor/issues/107
    //let input = PersonStruct{name: "Bob".to_string(), age: 43};
    //let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    //validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap();

    validate_cbor_from_slice(cddl_input, cbor::ARRAY_123).unwrap_err();
}

#[test]
fn validate_cbor_map_group() {
    let input = PersonStruct {
        name: "Bob".to_string(),
        age: 43,
    };
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    let cddl_input = r#"thing = {name: tstr, agroup} agroup = (age: int)"#;
    validate_cbor_cddl_named("thing", cddl_input, &cbor_bytes).unwrap();

    let cddl_input = r#"thing = {agroup} agroup = (age: int, name: tstr)"#;
    validate_cbor_cddl_named("thing", cddl_input, &cbor_bytes).unwrap();

    let cddl_input = r#"thing = {((agroup))} agroup = (age: int, name: tstr)"#;
    validate_cbor_cddl_named("thing", cddl_input, &cbor_bytes).unwrap();

    let cddl_input = r#"thing = {agroup empty} agroup = (age: int, name: tstr) empty = ()"#;
    validate_cbor_cddl_named("thing", cddl_input, &cbor_bytes).unwrap();

    let cddl_input = r#"thing = {agroup maybe} agroup = (age: int, name: tstr) maybe = (? minor: bool)"#;
    validate_cbor_cddl_named("thing", cddl_input, &cbor_bytes).unwrap();

    let cddl_input = r#"thing = {name: tstr, agroup} agroup = (wrong: int)"#;
    validate_cbor_cddl_named("thing", cddl_input, &cbor_bytes).unwrap_err();

    let cddl_input = r#"thing = {name: tstr, agroup} agroup = (age: bool)"#;
    validate_cbor_cddl_named("thing", cddl_input, &cbor_bytes).unwrap_err();
}

#[test]
fn validate_cbor_map() {
    let input = PersonStruct {
        name: "Bob".to_string(),
        age: 43,
    };
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    let cddl_input = r#"thing = {name: tstr, age: int}"#;
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap();

    let cddl_input = r#"thing = {name: tstr, ? age: int}"#;
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap();

    // Ensure that keys are optional if the occurrence is "?" or "*"
    // and required if the occurrence is "+"
    let cddl_input = r#"thing = {name: tstr, age: int, ? minor: bool}"#;
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap();
    let cddl_input = r#"thing = {name: tstr, age: int, * minor: bool}"#;
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap();
    let cddl_input = r#"thing = {name: tstr, age: int, + minor: bool}"#;
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap_err();

    let cddl_input = r#"thing = {name: tstr, age: tstr}"#;
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap_err();

    let cddl_input = r#"thing = {name: tstr}"#;
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap_err();

    // "* keytype => valuetype" is the expected syntax for collecting
    // any remaining key/value pairs of the expected type.
    let cddl_input = r#"thing = {* tstr => any}"#;
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap();
    let cddl_input = r#"thing = {name: tstr, * tstr => any}"#;
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap();
    let cddl_input = r#"thing = {name: tstr, age: int, * tstr => any}"#;
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap();
    let cddl_input = r#"thing = {+ tstr => any}"#;
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap();

    if false {
        // The "=>" syntax has implicit non-cut semantics; a key mismatch
        // shouldn't cause the key to be "consumed" from the map.  We don't
        // support non-cut semantics yet, so this test fails.

        // Should fail because the CBOR input has one entry that can't be
        // collected because the value type doesn't match.
        let cddl_input = r#"thing = {* tstr => int}"#;
        validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap_err();
    }

    // Should fail because the CBOR input has two entries that can't be
    // collected because the key type doesn't match.
    let cddl_input = r#"thing = {* int => any}"#;
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap_err();

    let cddl_input = r#"thing = {name: tstr, age: int, minor: bool}"#;
    validate_cbor_from_slice(cddl_input, &cbor_bytes).unwrap_err();

    let cddl_input = r#"thing = {x: int, y: int, z: int}"#;
    validate_cbor_from_slice(cddl_input, cbor::ARRAY_123).unwrap_err();
}

#[derive(Debug, Serialize)]
struct StreetNumber {
    street: String,
    number: Option<u32>,
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
    // This is an example from rfc8610 2.2.2
    // modifications from the RFC example:
    // 1. The bareword "number" has been changed to "number"; otherwise the parser gets confused.
    // 2. Substitute "_" for "-" in barewords.
    let cddl_input = r#"
        address = { delivery }

        delivery = (
        street: tstr, ? "number": uint, city //
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
    validate_cbor_cddl_named("address", cddl_input, &cbor_bytes).unwrap();

    let input = StreetNumber {
        street: "Eleventh St.".to_string(),
        number: Some(375),
        name: "San Francisco".to_string(),
        zip_code: 94103,
    };
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    validate_cbor_cddl_named("address", cddl_input, &cbor_bytes).unwrap();

    let input = StreetNumber {
        street: "Eleventh St.".to_string(),
        number: None,
        name: "San Francisco".to_string(),
        zip_code: 94103,
    };
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    validate_cbor_cddl_named("address", cddl_input, &cbor_bytes).unwrap();

    let input = Pickup { per_pickup: true };
    let cbor_bytes = serde_cbor::to_vec(&input).unwrap();
    validate_cbor_cddl_named("address", cddl_input, &cbor_bytes).unwrap();
}
