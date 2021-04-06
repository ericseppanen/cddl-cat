//! This module defines the Abstract Syntax Tree.
//!
//! It contains a tree representation of CDDL rules, closely matching the
//! syntax used in the original CDDL text.
//!

/// A literal value, i.e. `"foo"`, `1.0`, or `h'FFF7'`
///
/// CDDL ABNF grammar:
/// ```text
/// value = number / text / bytes
/// ```
#[derive(Debug, PartialEq)]
pub enum Value {
    /// A text-string literal.
    Text(String),
    /// An unsigned integer literal.
    Uint(u64),
    /// A negative integer literal.
    Nint(i64),
    /// A floating-point literal.
    Float(f64),
    /// A byte-tring literal.
    Bytes(Vec<u8>),
}

/// The "key" part of a key-value group member.
#[derive(Debug, PartialEq)]
#[allow(missing_docs)]
pub enum MemberKeyVal {
    /// Any type specified with the `=>` separator.
    Type1(Type1),
    /// A type name.
    Bareword(String),
    /// A literal value.
    Value(Value),
}

type IsCut = bool;

/// The "key" part of a key-value group member, along with its "cut" semantics.
///
/// When validating a map, "cut" means that once the key has matched, no other
/// keys in this CDDL group will be attempted.  See RFC8610 for more details.
///
#[derive(Debug, PartialEq)]
pub struct MemberKey {
    /// The actual key definition.
    pub val: MemberKeyVal,
    /// `true` if cut semantics are specified.
    pub cut: IsCut,
}

/// A group member, typically one element of an array or map.
#[derive(Debug, PartialEq)]
#[allow(missing_docs)]
pub struct Member {
    pub key: Option<MemberKey>,
    pub value: Type,
}

/// An "occurrence" which specifies how many elements can match a group member.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Clone)]
pub enum Occur {
    Optional,
    ZeroOrMore,
    OneOrMore,
    Numbered(usize, usize),
}

/// The part of a "group entry" after the occurrence.
#[derive(Debug, PartialEq)]
#[allow(missing_docs)]
pub enum GrpEntVal {
    Member(Member),
    Groupname(String),
    Parenthesized(Group),
}

/// A group entry contains one element of a group.
///
/// Each key-value pair in map, each element of an array, or each group
/// (inline or referenced by name) will be stored in a `GrpEnt`.
///
/// CDDL ABNF grammar:
/// ```text
/// grpent = [occur S] [memberkey S] type
///        / [occur S] groupname [genericarg]  ; preempted by above
///        / [occur S] "(" S group S ")"
/// ```
#[derive(Debug, PartialEq)]
#[allow(missing_docs)]
pub struct GrpEnt {
    pub occur: Option<Occur>,
    pub val: GrpEntVal,
}

/// A group choice contains one of the choices making up a group.
///
/// Each group choice is itself made up of individual group entries.
///
/// CDDL ABNF grammar:
/// ```text
/// grpchoice = *(grpent optcom)
/// ```
/// Translated: "zero-or-more group-entries separated by an optional comma"
#[derive(Debug, PartialEq)]
pub struct GrpChoice(pub Vec<GrpEnt>);

/// A group contains a number of elements.
///
/// Each group is itself made up of group choices, only one of which needs to
/// match.
///
/// CDDL ABNF grammar:
/// ```text
/// group = grpchoice *(S "//" S grpchoice)
/// ```
#[derive(Debug, PartialEq)]
pub struct Group(pub Vec<GrpChoice>);

/// A name identifier with generic arguments.
///
/// A name may have generic arguments, e.g. `message<K,V>`.
/// A name without generic arguments will have an empty generic_args Vec.
///
/// CDDL ABNF grammar:
/// ```text
/// genericarg = "<" S type1 S *("," S type1 S ) ">"
/// type2 = ...
///       / typename [genericarg]
///       / "~" S typename [genericarg]
///       / "&" S groupname [genericarg]
///       / ...
/// ```
#[derive(Debug, PartialEq)]
pub struct NameGeneric {
    /// A type or group name.
    pub name: String,
    /// Generic arguments, if any.
    pub generic_args: Vec<Type1>,
}

/// Type2 is the main representation of a CDDL type.
///
/// Note: not all type2 syntax is implemented.
/// Types starting with `&`, `#` are not yet supported.
///
/// CDDL ABNF grammar:
/// ```text
/// type2 = value
///       / typename [genericarg]
///       / "(" S type S ")"
///       / "{" S group S "}"
///       / "[" S group S "]"
///       / "~" S typename [genericarg]
///       / "&" S "(" S group S ")"
///       / "&" S groupname [genericarg]
///       / "#" "6" ["." uint] "(" S type S ")"
///       / "#" DIGIT ["." uint]
///       / "#"
/// ```
#[derive(Debug, PartialEq)]
#[allow(missing_docs)]
pub enum Type2 {
    Value(Value),
    Typename(NameGeneric),
    Parethesized(Type),
    Map(Group),
    Array(Group),
    Unwrap(NameGeneric),
    ChoiceifyInline(Group),
    Choiceify(NameGeneric),
}

/// A CDDL type, with an additional range or control operator.
///
/// CDDL ABNF grammar:
/// ```text
/// type1 = type2 [S (rangeop / ctlop) S type2]
/// ```
#[derive(Debug, PartialEq)]
pub enum Type1 {
    /// A `Type1` containing only a `Type2` with no operators
    Simple(Type2),
    /// A range (e.g. `1..10`)
    Range(TypeRange),
    /// A type with a control operator attached (e.g. `bstr .size 32`)
    Control(TypeControl),
}

/// A CDDL type, specified with a range operator.
///
/// Range operators are `..` (inclusive range) and `...` (exclusive range).
/// CDDL only allows the range operators on pairs of integers or floats.
#[derive(Debug, PartialEq)]
#[allow(missing_docs)]
pub struct TypeRange {
    pub start: Type2,
    pub end: Type2,
    pub inclusive: bool,
}

/// A CDDL type, specified with a control operator.
///
/// Control operators can express a range of possibilities, including
/// `.size N` (limit size of a value in bytes) or `.regexp` (requiring a text
/// string to match the given regular expression).
#[derive(Debug, PartialEq)]
#[allow(missing_docs)]
pub struct TypeControl {
    pub target: Type2,
    pub arg: Type2,
    pub op: String,
}

/// A CDDL type, with choices.
///
/// CDDL ABNF grammar:
/// ```text
/// type = type1 *(S "/" S type1)
/// ```
#[derive(Debug, PartialEq)]
pub struct Type(pub Vec<Type1>);

/// A CDDL data structure specification
///
/// Each CDDL rule has a name and a syntax tree.  Rules can be
/// referenced by name by other rules, or even within the same rule.
///
/// Note: `genericparm` is not yet supported.
/// Note: "extend" assignment operators (`/=`,`//=`) are not yet supported.
///
/// CDDL ABNF grammar:
/// ```text
/// rule = typename [genericparm] S assignt S type
///      / groupname [genericparm] S assigng S grpent
/// ```
#[derive(Debug, PartialEq)]
pub struct Rule {
    /// The rule name.
    pub name: String,
    /// Generic parameters.
    pub generic_parms: Vec<String>,
    /// The rule syntax tree.
    pub val: RuleVal,
}

/// A rule's syntax tree, in either [`Type`] or [`GrpEnt`] form.
///
/// Note: `genericparm` is not yet supported.
/// Note: "extend" assignment operators (`/=`,`//=`) are not yet supported.
///
/// CDDL ABNF grammar:
/// ```text
/// rule = typename [genericparm] S assignt S type
///      / groupname [genericparm] S assigng S grpent
/// ```
#[derive(Debug, PartialEq)]
pub enum RuleVal {
    /// A type assignment rule.
    AssignType(Type),
    /// A group assignment rule.
    AssignGroup(GrpEnt),
    // TODO: /= and //= operators --> ExtendType(Type), ExtendGroup(GrpEnt)
}

/// A CDDL specification, containing multiple rule syntax trees.
///
/// This is the output from the parser for a given CDDL text input.
///
#[derive(Debug, PartialEq)]
pub struct Cddl {
    /// Rules and their syntax trees.
    pub rules: Vec<Rule>,
}

/// A CDDL specification, containing multiple rule syntax trees.
///
/// This is the output from the parser for a given CDDL text input.
/// CddlSlice is exactly the same as [`Cddl`] except that it also
/// preserves a copy of the string used to compose that rule.
///
#[derive(Debug, PartialEq)]
pub struct CddlSlice {
    /// Rules and their syntax trees.
    pub rules: Vec<(Rule, String)>,
}
