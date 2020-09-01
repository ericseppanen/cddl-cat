use cddl_cat::parser::parse_cddl;
use ntest::timeout;

#[test]
#[timeout(5000)] // 5 seconds
fn test_recursion() {
    parse_cddl("a = [[[[[[[[[[[[[[[[[[[[[[ int ]]]]]]]]]]]]]]]]]]]]]]").unwrap();
    parse_cddl("a = {{{{{{{{{{{{{{{{{{{{{{ int }}}}}}}}}}}}}}}}}}}}}}").unwrap();
}
