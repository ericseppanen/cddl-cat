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

    assert_eq!(format!("{}", err), "Unparseable(!)");
    assert_eq!(
        format!("{:?}", err),
        r#"ParseError { kind: Unparseable, ctx: "!" }"#
    );
}
