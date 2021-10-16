use cddl_cat::parse_cddl;

#[test]
fn error_traits() {
    let bad_cddl = "!";
    let err = parse_cddl(bad_cddl).unwrap_err();

    // It would be unfriendly to not support Send + Sync + Unpin.
    // Error types should also support Error, Display, and Debug.
    fn has_traits1<T: Sized + Send + Sync + Unpin>(_: &T) {}
    fn has_traits2<T: std::error::Error + std::fmt::Display + std::fmt::Debug>(_: &T) {}

    has_traits1(&err);
    has_traits2(&err);
}

#[cfg(feature = "serde_json")]
mod uses_json {
    use cddl_cat::json::validate_json_str;

    #[test]
    fn error_display() {
        let err = validate_json_str("x", "!", "0").unwrap_err();
        assert_eq!(format!("{}", err), "Unparseable(!)");

        // JSON parsing error
        let err = validate_json_str("x", "x = nil", "🦀").unwrap_err();
        assert_eq!(
            format!("{}", err),
            "ValueError(expected value at line 1 column 1)"
        );

        let err = validate_json_str("x", "x = nil", "0").unwrap_err();
        assert_eq!(format!("{}", err), "Mismatch(expected nil)");
    }
}
