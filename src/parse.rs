use crate::types::*;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until},
    character::{
        complete::{char, multispace1},
        streaming::not_line_ending,
    },
    combinator::{all_consuming, map, not, opt, peek, recognize},
    error::{ErrorKind, ParseError},
    multi::{fold_many0, fold_many1, many0, separated_list, separated_nonempty_list},
    sequence::{delimited, tuple},
    AsChar, IResult, InputIter, Slice,
};
use nom_locate::LocatedSpan;
use std::ops::{RangeBounds, RangeFrom};

type Span<'a> = LocatedSpan<&'a str>;

// per https://www.oasis-open.org/committees/relax-ng/compact-20021121.html

// TODO:
//  - annotations
//  - utf8 escape sequences

pub fn schema(input: Span) -> Result<Schema, nom::Err<(Span, nom::error::ErrorKind)>> {
    all_consuming(tuple((space_comment0, top_level, space_comment0)))(input).map(|(_, (_, r, _))| r)
}

// topLevel	  ::=  	decl* (pattern | grammarContent*)
fn top_level(input: Span) -> IResult<Span, Schema> {
    let (input, decls) = separated_list(space_comment1, decl)(input)?;
    let (input, _) = space_comment1(input)?;
    let (input, pattern_or_grammar) = alt((
        map(pattern, PatternOrGrammar::Pattern),
        map(
            separated_list(space_comment1, grammar_content),
            PatternOrGrammar::Grammar,
        ),
    ))(input)?;
    IResult::Ok((
        input,
        Schema {
            decls,
            pattern_or_grammar,
        },
    ))
}

/// ```plain
/// decl      ::=  	"namespace" identifierOrKeyword "=" namespaceURILiteral
///                 | "default" "namespace" [identifierOrKeyword] "=" namespaceURILiteral
///                 | "datatypes" identifierOrKeyword "=" literal
/// ```
fn decl(input: Span) -> IResult<Span, Decl> {
    alt((
        map(decl_namespace, Decl::Namespace),
        map(decl_default_namespace, Decl::DefaultNamespace),
        map(decl_datatypes, Decl::Detatypes),
    ))(input)
}
fn decl_namespace(input: Span) -> IResult<Span, NamespaceDeclaration> {
    let (input, (_keyword, _, prefix, _, _, _, uri)) = tuple((
        tag("namespace"),
        space_comment1,
        identifier_or_keyword,
        space_comment0,
        tag("="),
        space_comment0,
        namespace_uri_literal,
    ))(input)?;
    IResult::Ok((
        input,
        NamespaceDeclaration {
            prefix: prefix.to_string(),
            uri,
        },
    ))
}
fn decl_default_namespace(input: Span) -> IResult<Span, DefaultNamespaceDeclaration> {
    let (input, (_keyword, _, _, _, prefix, _, _, _, uri)) = tuple((
        tag("default"),
        space_comment1,
        tag("namespace"),
        space_comment1,
        opt(identifier_or_keyword),
        space_comment0,
        tag("="),
        space_comment0,
        namespace_uri_literal,
    ))(input)?;
    IResult::Ok((
        input,
        DefaultNamespaceDeclaration {
            prefix: prefix.map(|v| v.to_string()),
            uri,
        },
    ))
}
fn decl_datatypes(input: Span) -> IResult<Span, DatatypesDeclaration> {
    let (input, (_keyword, _, prefix, _, _, _, uri)) = tuple((
        tag("datatypes"),
        space_comment1,
        identifier_or_keyword,
        space_comment0,
        tag("="),
        space_comment0,
        literal,
    ))(input)?;
    IResult::Ok((
        input,
        DatatypesDeclaration {
            prefix: prefix.to_string(),
            uri,
        },
    ))
}

/// ```plain
/// identifierOrKeyword	  ::=  	identifier
///                             | keyword
/// ```
fn identifier_or_keyword(input: Span) -> IResult<Span, IdentifierOrKeyword> {
    alt((
        map(identifier, IdentifierOrKeyword::Identifier),
        map(keyword, IdentifierOrKeyword::Keyword),
    ))(input)
}

/// ```plain
/// namespaceURILiteral	  ::=  	literal
///                             | "inherit"
/// ```
fn namespace_uri_literal(input: Span) -> IResult<Span, NamespaceUriLiteral> {
    alt((
        map(tag("inherit"), |_| NamespaceUriLiteral::Inherit),
        map(literal, NamespaceUriLiteral::Uri),
    ))(input)
}

/// ```plain
/// literal	  ::=  	literalSegment ("~" literalSegment)+
/// ```
fn literal(input: Span) -> IResult<Span, Literal> {
    separated_nonempty_list(tag("~"), literal_segment)(input).map(|(input, v)| (input, Literal(v)))
}

/// ```plain
/// literalSegment	  ::=  	'"' (Char - ('"' | newline))* '"'
///                       | "'" (Char - ("'" | newline))* "'"
///                       | '"""' (['"'] ['"'] (Char - '"'))* '"""'
///                       | "'''" (["'"] ["'"] (Char - "'"))* "'''"
/// ```
fn literal_segment(input: Span) -> IResult<Span, LiteralSegment> {
    let (input, body) = alt((
        delimited(tag("\"\"\""), take_until("\"\"\""), tag("\"\"\"")),
        delimited(tag("'''"), take_until("'''"), tag("'''")),
        delimited(tag("\""), recognize(opt(is_not("\"\n"))), tag("\"")),
        delimited(tag("'"), recognize(opt(is_not("'\n"))), tag("'")),
    ))(input)?;

    IResult::Ok((
        input,
        LiteralSegment {
            body: body.to_string(),
        },
    ))
}

/// ```plain
/// identifier	  ::=  	(NCName - keyword)
///                   | quotedIdentifier
/// ```
pub fn identifier(input: Span) -> IResult<Span, Identifier> {
    let res = alt((
        recognize(tuple((tag("\\"), keyword))),
        recognize(tuple((not(peek(keyword)), nc_name))),
    ))(input);

    res.map(|(input, v)| (input, Identifier(v.to_string())))
}

fn nc_name(input: Span) -> IResult<Span, NcName> {
    let parse = recognize(tuple((nc_name_start_char, many0(nc_name_char))));

    let parse = map(parse, |v| NcName(v.fragment.to_string()));

    parse(input)
}
fn nc_name_start_char(input: Span) -> IResult<Span, char> {
    // per https://www.w3.org/TR/REC-xml/#NT-NameStartChar -- but without ':'
    alt((
        char_in('A'..='Z'),
        char('_'),
        char_in('a'..='z'),
        char_in('\u{C0}'..'\u{D6}'),
        char_in('\u{D8}'..='\u{F6}'),
        char_in('\u{F8}'..='\u{2FF}'),
        char_in('\u{370}'..='\u{37D}'),
        char_in('\u{37F}'..='\u{1FFF}'),
        char_in('\u{200C}'..='\u{200D}'),
        char_in('\u{2070}'..='\u{218F}'),
        char_in('\u{2C00}'..='\u{2FEF}'),
        char_in('\u{3001}'..='\u{D7FF}'),
        char_in('\u{F900}'..='\u{FDCF}'),
        char_in('\u{FDF0}'..='\u{FFFD}'),
        char_in('\u{10000}'..='\u{EFFFF}'),
    ))(input)
}

fn nc_name_char(input: Span) -> IResult<Span, char> {
    alt((
        nc_name_start_char,
        char('-'),
        char('.'),
        char_in('0'..='9'),
        char('\u{B7}'),
        char_in('\u{0300}'..='\u{036F}'),
        char_in('\u{203F}'..='\u{2040}'),
    ))(input)
}

fn keyword(input: Span) -> IResult<Span, Keyword> {
    let parse = tuple((
        alt((
            tag("attribute"),
            tag("default"),
            tag("datatypes"),
            tag("div"),
            tag("element"),
            tag("empty"),
            tag("external"),
            tag("grammar"),
            tag("include"),
            tag("inherit"),
            tag("list"),
            tag("mixed"),
            tag("namespace"),
            tag("notAllowed"),
            tag("parent"),
            tag("start"),
            tag("string"),
            tag("text"),
            tag("token"),
        )),
        peek(not(identifier)),
    ));

    let parse = map(parse, |(k, _)| Keyword(k.fragment.to_string()));

    parse(input)
}

// pattern  ::=
//	  "element" nameClass "{" pattern "}"
//    | "attribute" nameClass "{" pattern "}"
//    | pattern ("," pattern)+
//    | pattern ("&" pattern)+
//    | pattern ("|" pattern)+
//    | pattern "?"
//    | pattern "*"
//    | pattern "+"
//    | "list" "{" pattern "}"
//    | "mixed" "{" pattern "}"
//    | identifier
//    | "parent" identifier
//    | "empty"
//    | "text"
//    | [datatypeName] datatypeValue
//    | datatypeName ["{" param* "}"] [exceptPattern]
//    | "notAllowed"
//    | "external" anyURILiteral [inherit]
//    | "grammar" "{" grammarContent* "}"
//    | "(" pattern ")"
fn pattern(input: Span) -> IResult<Span, Pattern> {
    let (mut input, mut result) = alt((
        map(element_pattern, Pattern::Element),
        map(attribute_pattern, Pattern::Attribute),
        map(list_pattern, Pattern::List),
        map(mixed_pattern, Pattern::Mixed),
        map(datatype_value_pattern, Pattern::DatatypeValue),
        map(datatype_param_pattern, Pattern::DatatypeName),
        map(identifier, Pattern::Identifier),
        map(
            tuple((tag("parent"), space_comment1, identifier)),
            |(_, _, p)| Pattern::Parent(p),
        ),
        map(tag("empty"), |_| Pattern::Empty),
        map(tag("text"), |_| Pattern::Text),
        map(tag("notAllowed"), |_| Pattern::NotAllowed),
        map(external_pattern, Pattern::External),
        map(grammar_pattern, Pattern::Grammar),
        map(group_pattern, |p| Pattern::Group(Box::new(p))),
    ))(input)?;

    loop {
        let (i, _) = space_comment0(input)?;
        if let Ok((i, _)) = tag::<_, _, (Span, ErrorKind)>(",")(i) {
            let (i, _) = space_comment0(i)?;
            let (i, right) = pattern(i)?;
            result = Pattern::ListPair(Box::new(result), Box::new(right));
            input = i;
            continue;
        }
        if let Ok((i, _)) = tag::<_, _, (Span, ErrorKind)>("&")(i) {
            let (i, _) = space_comment0(i)?;
            let (i, right) = pattern(i)?;
            result = Pattern::InterleavePair(Box::new(result), Box::new(right));
            input = i;
            continue;
        }
        if let Ok((i, _)) = tag::<_, _, (Span, ErrorKind)>("|")(i) {
            let (i, _) = space_comment0(i)?;
            let (i, right) = pattern(i)?;
            result = Pattern::ChoicePair(Box::new(result), Box::new(right));
            input = i;
            continue;
        }
        if let Ok((i, _)) = tag::<_, _, (Span, ErrorKind)>("?")(i) {
            result = Pattern::Optional(Box::new(result));
            input = i;
            continue;
        }
        if let Ok((i, _)) = tag::<_, _, (Span, ErrorKind)>("*")(i) {
            result = Pattern::ZeroOrMore(Box::new(result));
            input = i;
            continue;
        }
        if let Ok((i, _)) = tag::<_, _, (Span, ErrorKind)>("+")(i) {
            result = Pattern::OneOrMore(Box::new(result));
            input = i;
            continue;
        }
        break;
    }
    IResult::Ok((input, result))
}
// "element" nameClass "{" pattern "}"
fn element_pattern(input: Span) -> IResult<Span, ElementPattern> {
    let parse = tuple((
        tag("element"),
        space_comment1,
        name_class,
        space_comment0,
        tag("{"),
        space_comment0,
        pattern,
        space_comment0,
        tag("}"),
    ));

    map(parse, |(_, _, name_class, _, _, _, pattern, _, _)| {
        ElementPattern {
            name_class,
            pattern: Box::new(pattern),
        }
    })(input)
}

// "attribute" nameClass "{" pattern "}"
fn attribute_pattern(input: Span) -> IResult<Span, AttributePattern> {
    let parse = tuple((
        tag("attribute"),
        space_comment1,
        name_class,
        space_comment0,
        tag("{"),
        space_comment0,
        pattern,
        space_comment0,
        tag("}"),
    ));

    map(parse, |(_, _, name_class, _, _, _, pattern, _, _)| {
        AttributePattern {
            name_class,
            pattern: Box::new(pattern),
        }
    })(input)
}
fn list_pattern(input: Span) -> IResult<Span, ListPattern> {
    let parse = tuple((
        tag("list"),
        space_comment0,
        tag("{"),
        space_comment0,
        pattern,
        space_comment0,
        tag("}"),
    ));

    map(parse, |(_, _, _, _, pattern, _, _)| {
        ListPattern(Box::new(pattern))
    })(input)
}
fn mixed_pattern(input: Span) -> IResult<Span, MixedPattern> {
    let parse = tuple((
        tag("mixed"),
        space_comment0,
        tag("{"),
        space_comment0,
        pattern,
        space_comment0,
        tag("}"),
    ));

    map(parse, |(_, _, _, _, pattern, _, _)| {
        MixedPattern(Box::new(pattern))
    })(input)
}

// "external" anyURILiteral [inherit]
fn external_pattern(input: Span) -> IResult<Span, ExternalPattern> {
    let parse = tuple((
        tag("external"),
        space_comment1,
        any_uri_literal,
        opt(map(tuple((space_comment1, inherit)), |(_, inherit)| {
            inherit
        })),
    ));

    let parse = map(parse, |(_, _, uri, inherit)| ExternalPattern(uri, inherit));

    parse(input)
}

// "grammar" "{" grammarContent* "}"
fn grammar_pattern(input: Span) -> IResult<Span, GrammarPattern> {
    let parse = tuple((
        tag("grammar"),
        space_comment0,
        tag("{"),
        space_comment0,
        many0(grammar_content),
        space_comment0,
        tag("}"),
    ));

    let parse = map(parse, |(_, _, _, _, content, _, _)| GrammarPattern(content));

    parse(input)
}

fn group_pattern(input: Span) -> IResult<Span, Pattern> {
    let parse = tuple((tag("("), space_comment0, pattern, space_comment0, tag(")")));

    let parse = map(parse, |(_, _, content, _, _)| content);

    parse(input)
}

// [datatypeName] datatypeValue
fn datatype_value_pattern(input: Span) -> IResult<Span, DatatypeValuePattern> {
    let parse = tuple((opt(datatype_name), datatype_value));

    let parse = map(parse, |(name, value)| DatatypeValuePattern(name, value));

    parse(input)
}

// datatypeName ["{" param* "}"] [exceptPattern]
fn datatype_param_pattern(input: Span) -> IResult<Span, DatatypeNamePattern> {
    let params = tuple((
        tag("{"),
        space_comment0,
        many0(param),
        space_comment0,
        tag("}"),
    ));
    let params = map(params, |(_, _, p, _, _)| p);

    let parse = tuple((datatype_name, opt(params), opt(except_pattern)));
    let parse = map(parse, |(name, params, except)| {
        DatatypeNamePattern(name, params, except.map(Box::new))
    });

    parse(input)
}

// datatypeValue  ::= 	literal
fn datatype_value(input: Span) -> IResult<Span, Literal> {
    literal(input)
}

// param	  ::=  	identifierOrKeyword "=" literal
fn param(input: Span) -> IResult<Span, Param> {
    let parse = tuple((
        identifier_or_keyword,
        space_comment0,
        tag("="),
        space_comment0,
        literal,
    ));

    let parse = map(parse, |(name, _, _, _, val)| Param(name, val));

    parse(input)
}

// exceptPattern     ::=   "-" pattern
fn except_pattern(input: Span) -> IResult<Span, Pattern> {
    let parse = tuple((tag("-"), space_comment0, pattern));

    let parse = map(parse, |(_, _, pat)| pat);

    parse(input)
}

// datatypeName  ::=   CName
//                   | "string"
//                   | "token"
fn datatype_name(input: Span) -> IResult<Span, DatatypeName> {
    alt((
        map(tag("string"), |_| DatatypeName::String),
        map(tag("token"), |_| DatatypeName::Token),
        map(cname, DatatypeName::Name),
    ))(input)
}

fn name_class(input: Span) -> IResult<Span, NameClass> {
    let (input, left) = alt((
        map(ns_name_nc, NameClass::NsName),
        map(name, NameClass::Name),
        map(any_name_nc, NameClass::AnyName),
        //map(alt_nc, |r| NameClass::Alt(r)),
        map(paren_nc, NameClass::Paren),
    ))(input)?;

    if let Ok((input, right)) = alt_nc(input) {
        return Ok((
            input,
            NameClass::Alt(AltName(Box::new(left), Box::new(right))),
        ));
    }

    Ok((input, left))
}

// name	  ::=  	identifierOrKeyword
//            | CName
fn name(input: Span) -> IResult<Span, Name> {
    alt((
        map(recognize(identifier_or_keyword), |i| {
            Name(i.fragment.to_string())
        }),
        map(recognize(cname), |i| Name(i.fragment.to_string())),
    ))(input)
}

fn ns_name_nc(input: Span) -> IResult<Span, NsName> {
    let parse = tuple((
        nc_name,
        tag(":*"),
        opt(tuple((
            space_comment0,
            tag("-"),
            space_comment0,
            name_class,
        ))),
    ));

    let parse = map(parse, |(name, _, except)| {
        NsName(
            name,
            except.map(|(_, _, _, name_class)| Box::new(name_class)),
        )
    });

    parse(input)
}

fn any_name_nc(input: Span) -> IResult<Span, AnyName> {
    let parse = tuple((
        tag("*"),
        opt(tuple((
            space_comment0,
            tag("-"),
            space_comment0,
            name_class,
        ))),
    ));

    let parse = map(parse, |(_, except)| {
        AnyName(except.map(|(_, _, _, name_class)| Box::new(name_class)))
    });

    parse(input)
}

fn alt_nc(input: Span) -> IResult<Span, NameClass> {
    let parse = tuple((space_comment0, tag("|"), space_comment0, name_class));

    let parse = map(parse, |(_, _, _, right)| right);

    parse(input)
}

fn paren_nc(input: Span) -> IResult<Span, ParenName> {
    let parse = tuple((
        tag("("),
        space_comment0,
        name_class,
        space_comment0,
        tag(")"),
    ));

    let parse = map(parse, |(_, _, name_class, _, _)| {
        ParenName(Box::new(name_class))
    });

    parse(input)
}

// NCName ":" NCName
fn cname(input: Span) -> IResult<Span, CName> {
    let parse = tuple((nc_name, tag(":"), nc_name));

    let parse = map(parse, |(prefix, _, local_name)| CName(prefix, local_name));

    parse(input)
}

// TODO: use `is_a()`  with an impl of `FindToken` for `RangeBounds`
pub fn char_in<I, R, Error: ParseError<I>>(range: R) -> impl Fn(I) -> IResult<I, char, Error>
where
    I: Slice<RangeFrom<usize>> + InputIter,
    <I as InputIter>::Item: AsChar + Copy,
    R: RangeBounds<char>,
{
    move |i: I| match (i)
        .iter_elements()
        .next()
        .map(|c| (c, range.contains(&c.as_char())))
    {
        Some((c, true)) => Ok((i.slice(c.len()..), c.as_char())),
        _ => Err(nom::Err::Error(Error::from_error_kind(i, ErrorKind::OneOf))),
    }
}

// grammarContent	  ::=  	start
//                        | define
//                        | "div" "{" grammarContent* "}"
//                        | "include" anyURILiteral [inherit] ["{" includeContent* "}"]
fn grammar_content(input: Span) -> IResult<Span, GrammarContent> {
    alt((
        map(start, GrammarContent::Start),
        map(define, GrammarContent::Define),
        map(div_grammar_content, GrammarContent::Div),
        map(include, GrammarContent::Include),
    ))(input)
}

// start	  ::=  	"start" assignMethod pattern
fn start(input: Span) -> IResult<Span, Start> {
    let parse = tuple((
        tag("start"),
        space_comment0,
        assign_method,
        space_comment0,
        pattern,
    ));

    let parse = map(parse, |(_, _, assign_method, _, pattern)| {
        Start(assign_method, pattern)
    });

    parse(input)
}

// define	  ::=  	identifier assignMethod pattern
fn define(input: Span) -> IResult<Span, Define> {
    let parse = tuple((
        identifier,
        space_comment0,
        assign_method,
        space_comment0,
        pattern,
    ));

    let parse = map(parse, |(identifier, _, assign_method, _, pattern)| {
        Define(identifier, assign_method, pattern)
    });

    parse(input)
}

fn assign_method(input: Span) -> IResult<Span, AssignMethod> {
    alt((
        map(tag("="), |_| AssignMethod::Assign),
        map(tag("|="), |_| AssignMethod::Choice),
        map(tag("&="), |_| AssignMethod::Interleave),
    ))(input)
}

// "div" "{" grammarContent* "}"
fn div_grammar_content(input: Span) -> IResult<Span, Vec<GrammarContent>> {
    let parse = tuple((
        tag("div"),
        space_comment0,
        tag("{"),
        space_comment0,
        many0(grammar_content),
        space_comment0,
        tag("}"),
    ));

    let parse = map(parse, |(_, _, _, _, content, _, _)| content);

    parse(input)
}

// "include" anyURILiteral [inherit] ["{" includeContent* "}"]
fn include(input: Span) -> IResult<Span, Include> {
    let parse = tuple((
        tag("include"),
        space_comment1,
        any_uri_literal,
        opt(map(tuple((space_comment1, inherit)), |(_, v)| v )),
        opt(map(
            tuple((space_comment0, tag("{"), many0(include_content), tag("}"))),
            |(_, _, inc, _)| inc,
        )),
    ));

    let parse = map(parse, |(_, _, uri, inherit, include)| {
        Include(uri, inherit, include)
    });

    parse(input)
}

fn any_uri_literal(input: Span) -> IResult<Span, Literal> {
    literal(input)
}

// inherit	  ::=  	"inherit" "=" identifierOrKeyword
fn inherit(input: Span) -> IResult<Span, Inherit> {
    let parse = tuple((
        tag("inherit"),
        space_comment0,
        tag("="),
        space_comment0,
        identifier_or_keyword,
    ));

    let parse = map(parse, |(_, _, _, _, id)| Inherit(id));

    parse(input)
}

// includeContent  ::=  define
//                    | start
//                    | "div" "{" includeContent* "}"
fn include_content(input: Span) -> IResult<Span, IncludeContent> {
    alt((
        map(define, IncludeContent::Define),
        map(start, IncludeContent::Start),
        map(div_include_content, IncludeContent::Div),
    ))(input)
}

// "div" "{" includeContent* "}"
fn div_include_content(input: Span) -> IResult<Span, Vec<IncludeContent>> {
    let parse = tuple((
        tag("div"),
        space_comment0,
        tag("{"),
        space_comment0,
        many0(include_content),
        space_comment0,
        tag("}"),
    ));

    let parse = map(parse, |(_, _, _, _, content, _, _)| content);

    parse(input)
}

fn space_comment0(input: Span) -> IResult<Span, Span> {
    recognize(fold_many0(alt((multispace1, comment)), (), |_, _| ()))(input)
}
fn space_comment1(input: Span) -> IResult<Span, Span> {
    recognize(fold_many1(alt((multispace1, comment)), (), |_, _| ()))(input)
}
fn comment(input: Span) -> IResult<Span, Span> {
    recognize(tuple((tag("#"), not_line_ending)))(input)
}

#[cfg(test)]
mod test {
    use super::*;
    use nom_locate::LocatedSpan;
    use std::fmt;

    fn ck<T, F>(f: F, input: &str, expected: T)
    where
        F: Fn(LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, T>,
        T: PartialEq + fmt::Debug,
    {
        let (remaining, result) =
            f(LocatedSpan::new(input)).expect(&format!("failed to parse {:#?}", input));
        assert_eq!(remaining.fragment, "");
        assert_eq!(result, expected);
    }

    #[test]
    fn test_start() {
        ck(
            start,
            "start = pattern",
            Start(
                AssignMethod::Assign,
                Pattern::Identifier(Identifier("pattern".to_string())),
            ),
        )
    }

    #[test]
    fn literal_seg() {
        ck(
            literal_segment,
            "'foo'",
            LiteralSegment {
                body: "foo".to_string(),
            },
        );
        ck(
            literal_segment,
            "'''foo'''",
            LiteralSegment {
                body: "foo".to_string(),
            },
        );
        ck(
            literal_segment,
            "'''foo''bar'''",
            LiteralSegment {
                body: "foo''bar".to_string(),
            },
        );
        ck(
            literal_segment,
            "\"\"",
            LiteralSegment {
                body: "".to_string(),
            },
        );
        ck(
            literal_segment,
            "\"foo\"",
            LiteralSegment {
                body: "foo".to_string(),
            },
        );
        ck(
            literal_segment,
            "\"\"\"foo\"\"\"",
            LiteralSegment {
                body: "foo".to_string(),
            },
        );
        ck(
            literal_segment,
            "\"\"\"foo\"\"bar\"\"\"",
            LiteralSegment {
                body: "foo\"\"bar".to_string(),
            },
        );
    }

    #[test]
    fn namespace() {
        ck(
            decl_namespace,
            "namespace rng = \"http://relaxng.org/ns/structure/1.0\"",
            NamespaceDeclaration {
                prefix: "rng".to_string(),
                uri: NamespaceUriLiteral::Uri(Literal(vec![LiteralSegment {
                    body: "http://relaxng.org/ns/structure/1.0".to_string(),
                }])),
            },
        );
    }

    #[test]
    fn def_namespace() {
        ck(
            decl_default_namespace,
            "default namespace rng = \"http://relaxng.org/ns/structure/1.0\"",
            DefaultNamespaceDeclaration {
                prefix: Some("rng".to_string()),
                uri: NamespaceUriLiteral::Uri(Literal(vec![LiteralSegment {
                    body: "http://relaxng.org/ns/structure/1.0".to_string(),
                }])),
            },
        );
    }

    #[test]
    fn pattern_list() {
        ck(
            pattern,
            "a,b , c",
            Pattern::ListPair(
                Box::new(Pattern::Identifier(Identifier("a".to_string()))),
                Box::new(Pattern::ListPair(
                    Box::new(Pattern::Identifier(Identifier("b".to_string()))),
                    Box::new(Pattern::Identifier(Identifier("c".to_string()))),
                )),
            ),
        )
    }

    #[test]
    fn pattern_opt() {
        ck(
            pattern,
            "a?",
            Pattern::Optional(Box::new(Pattern::Identifier(Identifier("a".to_string())))),
        )
    }

    #[test]
    fn attr_pat() {
        ck(
            attribute_pattern,
            "attribute * { text }",
            AttributePattern {
                name_class: NameClass::AnyName(AnyName(None)),
                pattern: Box::new(Pattern::Text),
            },
        )
    }

    #[test]
    fn list_zeromore_patt() {
        ck(
            pattern,
            "a*, b",
            Pattern::ListPair(
                Box::new(Pattern::ZeroOrMore(Box::new(Pattern::Identifier(
                    Identifier("a".to_string()),
                )))),
                Box::new(Pattern::Identifier(Identifier("b".to_string()))),
            ),
        )
    }

    #[test]
    fn alt_nc() {
        let data = "a|b";
        println!("{:#?}", name_class(LocatedSpan::new(data)));
    }

    #[test]
    fn keyw() {
        ck(
            identifier_or_keyword,
            "parent",
            IdentifierOrKeyword::Keyword(Keyword("parent".to_string())),
        );
        ck(
            identifier_or_keyword,
            "parents",
            IdentifierOrKeyword::Identifier(Identifier("parents".to_string())),
        );
    }

    #[test]
    fn test_pattern_cname() {
        ck(
            pattern,
            "xsd:string",
            Pattern::DatatypeName(DatatypeNamePattern(
                DatatypeName::Name(CName(
                    NcName("xsd".to_string()),
                    NcName("string".to_string()),
                )),
                None,
                None,
            )),
        )
    }
}
