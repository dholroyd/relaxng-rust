use crate::types::*;
use nom::combinator::cut;
use nom::sequence::preceded;
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
use nom_locate::{position, LocatedSpan};
use std::ops::{Range, RangeBounds, RangeFrom};

pub type Span<'a> = LocatedSpan<&'a str>;

// per https://www.oasis-open.org/committees/relax-ng/compact-20021121.html

// TODO:
//  - annotations
//  - utf8 escape sequences
//  - check rules are left-factored as required to avoid inefficiently rematching the same sub-rule
//    in multiple alternatives

pub fn schema(input: Span) -> Result<Schema, nom::Err<(Span, nom::error::ErrorKind)>> {
    all_consuming(tuple((space_comment0, top_level, space_comment0)))(input).map(|(_, (_, r, _))| r)
}

// topLevel	  ::=  	decl* (pattern | grammarContent*)
fn top_level(input: Span) -> IResult<Span, Schema> {
    let parse = tuple((
        separated_list(space_comment1, decl),
        space_comment0, // TODO: should be space_comment1, and only required if there are decls
        alt((
            // TODO: proper span for GrammarPattern
            map(
                separated_nonempty_list(space_comment1, grammar_content),
                |content| {
                    PatternOrGrammar::Grammar(GrammarPattern {
                        span: 0..0,
                        content,
                    })
                },
            ),
            map(pattern, PatternOrGrammar::Pattern),
        )),
    ));
    let parse = map(parse, |(decls, _, pattern_or_grammar)| Schema {
        decls,
        pattern_or_grammar,
    });

    parse(input)
}

// decl      ::=  	"namespace" identifierOrKeyword "=" namespaceURILiteral
//                 | "default" "namespace" [identifierOrKeyword] "=" namespaceURILiteral
//                 | "datatypes" identifierOrKeyword "=" literal
fn decl(input: Span) -> IResult<Span, Decl> {
    alt((
        map(decl_namespace, Decl::Namespace),
        map(decl_default_namespace, Decl::DefaultNamespace),
        map(decl_datatypes, Decl::Datatypes),
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

// identifierOrKeyword	  ::=  	identifier
//                             | keyword
fn identifier_or_keyword(input: Span) -> IResult<Span, IdentifierOrKeyword> {
    alt((
        map(identifier, IdentifierOrKeyword::Identifier),
        map(keyword, IdentifierOrKeyword::Keyword),
    ))(input)
}

// namespaceURILiteral	  ::=  	literal
//                             | "inherit"
fn namespace_uri_literal(input: Span) -> IResult<Span, NamespaceUriLiteral> {
    alt((
        map(tag("inherit"), |_| NamespaceUriLiteral::Inherit),
        map(literal, NamespaceUriLiteral::Uri),
    ))(input)
}

// literal	  ::=  	literalSegment ("~" literalSegment)+
fn literal(input: Span) -> IResult<Span, Literal> {
    let parser = tuple((
        position,
        separated_nonempty_list(
            tuple((space_comment0, tag("~"), space_comment0)),
            literal_segment,
        ),
        position,
    ));
    let parser = map(parser, |(start, v, end)| Literal(span(start, end), v));

    parser(input)
}

// literalSegment	  ::=  	'"' (Char - ('"' | newline))* '"'
//                       | "'" (Char - ("'" | newline))* "'"
//                       | '"""' (['"'] ['"'] (Char - '"'))* '"""'
//                       | "'''" (["'"] ["'"] (Char - "'"))* "'''"
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

fn span(start: LocatedSpan<&str>, end: LocatedSpan<&str>) -> Range<usize> {
    Range {
        start: start.offset,
        end: end.offset,
    }
}

// identifier	  ::=  	(NCName - keyword)
//                   | quotedIdentifier
fn identifier(input: Span) -> IResult<Span, Identifier> {
    let res = alt((
        recognize(tuple((tag("\\"), keyword))),
        recognize(tuple((not(peek(keyword)), nc_name))),
    ))(input);

    res.map(|(input, v)| (input, Identifier(span(v, input), v.to_string())))
}

pub fn nc_name(input: Span) -> IResult<Span, NcName> {
    let parse = tuple((
        position,
        recognize(tuple((nc_name_start_char, many0(nc_name_char)))),
        position,
    ));

    let parse = map(parse, |(start, v, end)| {
        NcName(span(start, end), v.fragment.to_string())
    });

    parse(input)
}

fn nc_name_start_char(input: Span) -> IResult<Span, char> {
    // per https://www.w3.org/TR/REC-xml/#NT-NameStartChar -- but without ':'
    alt((
        char_in('A'..='Z'),
        char('_'),
        char_in('a'..='z'),
        char_in('\u{C0}'..='\u{D6}'),
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
        position,
        peek(not(nc_name_char)),
    ));

    let parse = map(parse, |(k, end, _)| {
        Keyword(span(k, end), k.fragment.to_string())
    });

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
    let (input, annotation) = maybe_initial_annotation(input)?;
    if annotation.is_some() {
        println!(
            "pattern annotation found but ignored! {:?}",
            annotation.unwrap()
        );
    }
    let (input, mut result) = alt((
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

    let (mut input, follow_annotations) = follow_annotation_list(input)?;
    if !follow_annotations.is_empty() {
        println!(
            "pattern follow annotation found but ignored! {:?}",
            follow_annotations
        );
    }

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
        cut(pattern),
        space_comment0,
        cut(tag("}")),
        position,
    ));

    map(
        parse,
        |(start, _, name_class, _, _, _, pattern, _, _, end)| ElementPattern {
            span: span(start, end),
            name_class,
            pattern: Box::new(pattern),
        },
    )(input)
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
        cut(pattern),
        space_comment0,
        cut(tag("}")),
        position,
    ));

    map(
        parse,
        |(start, _, name_class, _, _, _, pattern, _, _, end)| AttributePattern {
            span: span(start, end),
            name_class,
            pattern: Box::new(pattern),
        },
    )(input)
}
fn list_pattern(input: Span) -> IResult<Span, ListPattern> {
    let parse = tuple((
        tag("list"),
        space_comment0,
        tag("{"),
        space_comment0,
        cut(pattern),
        space_comment0,
        cut(tag("}")),
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
        cut(pattern),
        space_comment0,
        cut(tag("}")),
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
        separated_list(space_comment1, grammar_content),
        space_comment0,
        cut(tag("}")),
        position,
    ));

    let parse = map(parse, |(start, _, _, _, content, _, _, end)| {
        GrammarPattern {
            span: span(start, end),
            content,
        }
    });

    parse(input)
}

fn group_pattern(input: Span) -> IResult<Span, Pattern> {
    let parse = tuple((tag("("), space_comment0, pattern, space_comment0, tag(")")));

    let parse = map(parse, |(_, _, content, _, _)| content);

    parse(input)
}

// [datatypeName] datatypeValue
fn datatype_value_pattern(input: Span) -> IResult<Span, DatatypeValuePattern> {
    let parse = tuple((opt(datatype_name), space_comment0, datatype_value));

    let parse = map(parse, |(name, _, value)| DatatypeValuePattern(name, value));

    parse(input)
}

// datatypeName ["{" param* "}"] [exceptPattern]
fn datatype_param_pattern(input: Span) -> IResult<Span, DatatypeNamePattern> {
    let params = tuple((
        space_comment0,
        tag("{"),
        space_comment0,
        separated_list(space_comment1, param),
        space_comment0,
        cut(tag("}")),
    ));
    let params = map(params, |(_, _, _, p, _, _)| p);

    let parse = tuple((
        datatype_name,
        opt(params),
        opt(map(tuple((space_comment0, except_pattern)), |(_, e)| e)),
    ));
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
        position,
        maybe_initial_annotation,
        space_comment0,
        identifier_or_keyword,
        space_comment0,
        tag("="),
        space_comment0,
        literal,
        position,
    ));

    let parse = map(
        parse,
        |(start, initial_annotation, _, name, _, _, _, val, end)| {
            Param(span(start, end), initial_annotation, name, val)
        },
    );

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
        map(cname, DatatypeName::CName),
    ))(input)
}

fn name_class(input: Span) -> IResult<Span, NameClass> {
    let (input, annotation) = maybe_initial_annotation(input)?;
    if annotation.is_some() {
        println!("name-class annotation found but ignored!")
    }
    let (input, left) = alt((
        map(ns_name_nc, NameClass::NsName),
        map(name, NameClass::Name),
        map(any_name_nc, NameClass::AnyName),
        //map(alt_nc, |r| NameClass::Alt(r)),
        map(paren_nc, NameClass::Paren),
    ))(input)?;

    let (input, follow_annotations) = follow_annotation_list(input)?;
    if !follow_annotations.is_empty() {
        println!(
            "name-class follow annotation found but ignored! {:?}",
            annotation.unwrap()
        );
    }

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
        map(cname, Name::CName),
        map(identifier_or_keyword, Name::Identifier),
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

    let parse = map(parse, |(name, _, except)| NsName {
        name: NamespaceOrPrefix::Prefix(name),
        except: except.map(|(_, _, _, name_class)| Box::new(name_class)),
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
fn cname(input: Span) -> IResult<Span, QName> {
    let parse = tuple((nc_name, tag(":"), nc_name));

    let parse = map(parse, |(prefix, _, local_name)| QName(prefix, local_name));

    parse(input)
}

// TODO: use `is_a()`  with an impl of `FindToken` for `RangeBounds`
fn char_in<I, R, Error: ParseError<I>>(range: R) -> impl Fn(I) -> IResult<I, char, Error>
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
        map(start, GrammarContent::Define),
        map(define, GrammarContent::Define),
        map(div_grammar_content, GrammarContent::Div),
        map(include, GrammarContent::Include),
        map(annotation_element, GrammarContent::Annotation),
    ))(input)
}

// start	  ::=  	"start" assignMethod pattern
fn start(input: Span) -> IResult<Span, Define> {
    let parse = tuple((
        position,
        tag("start"),
        space_comment0,
        assign_method,
        space_comment0,
        cut(pattern),
        position,
    ));

    // we just produce another 'Define' named "start", rather than using a dedicated 'Start' type,
    // so as to avoid duplication of code handling 'start' definitions and other definitions

    let parse = map(
        parse,
        |(start, start_tag, _, assign_method, _, pattern, end)| {
            Define(
                span(start, end),
                Identifier(span(start_tag, start_tag), "start".to_string()),
                assign_method,
                pattern,
            )
        },
    );

    parse(input)
}

// define	  ::=  	identifier assignMethod pattern
fn define(input: Span) -> IResult<Span, Define> {
    let parse = tuple((
        position,
        identifier,
        space_comment0,
        assign_method,
        space_comment0,
        cut(pattern),
        position,
    ));

    let parse = map(
        parse,
        |(start, identifier, _, assign_method, _, pattern, end)| {
            Define(span(start, end), identifier, assign_method, pattern)
        },
    );

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
        separated_list(space_comment1, grammar_content),
        space_comment0,
        cut(tag("}")),
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
        opt(map(tuple((space_comment1, inherit)), |(_, v)| v)),
        opt(map(
            tuple((
                space_comment0,
                tag("{"),
                space_comment0,
                separated_list(space_comment1, include_content),
                space_comment0,
                cut(tag("}")),
            )),
            |(_, _, _, inc, _, _)| inc,
        )),
    ));

    let parse = map(parse, |(_, _, uri, inherit, include)| {
        Include(uri, inherit, include)
    });

    parse(input)
}

// anyURILiteral	  ::=  	literal
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
    let (input, annotation) = maybe_initial_annotation(input)?;
    if annotation.is_some() {
        println!("include-content annotation found but ignored!")
    }
    alt((
        map(annotation_element, IncludeContent::Annotation),
        map(define, IncludeContent::Define),
        map(start, IncludeContent::Define),
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
        separated_list(space_comment1, include_content),
        space_comment0,
        cut(tag("}")),
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

fn maybe_initial_annotation(input: Span) -> IResult<Span, Option<InitialAnnotation>> {
    opt(map(
        tuple((initial_annotation, space_comment0)),
        |(anno, _)| anno,
    ))(input)
}

fn initial_annotation(input: Span) -> IResult<Span, InitialAnnotation> {
    let parse = tuple((
        tag("["),
        space_comment0,
        separated_list(space_comment1, annotation_attribute),
        space_comment0,
        separated_list(space_comment1, annotation_element),
        space_comment0,
        tag("]"),
    ));

    let parse = map(
        parse,
        |(start, _, attribute_annotations, _, element_annotations, _, end)| InitialAnnotation {
            span: span(start, end),
            attribute_annotations,
            element_annotations,
        },
    );

    parse(input)
}

fn follow_annotation_list(input: Span) -> IResult<Span, Vec<AnnotationElement>> {
    separated_list(space_comment0, follow_annotation)(input)
}

fn follow_annotation(input: Span) -> IResult<Span, AnnotationElement> {
    preceded(
        tuple((space_comment0, tag(">>"), space_comment0)),
        annotation_element,
    )(input)
}

fn annotation_attribute(input: Span) -> IResult<Span, AnnotationAttribute> {
    let parse = tuple((name, space_comment0, tag("="), space_comment0, literal));

    let parse = map(parse, |(name, _, _, _, value)| AnnotationAttribute {
        span: Range {
            start: name.span().start,
            end: value.0.end,
        },
        name,
        value,
    });

    parse(input)
}

fn annotation_element(input: Span) -> IResult<Span, AnnotationElement> {
    let parse = tuple((
        name,
        space_comment0,
        tag("["),
        space_comment0,
        separated_list(space_comment1, annotation_attribute),
        space_comment0,
        separated_list(space_comment1, annotation_element_or_literal),
        space_comment0,
        tag("]"),
    ));

    let parse = map(
        parse,
        |(name, _, _, _, annotation_attributes, _, annotation_elements_or_literals, _, end)| {
            AnnotationElement {
                span: Range {
                    start: name.span().start,
                    end: end.offset + end.fragment.len(),
                },
                name,
                annotation_attributes,
                annotation_elements_or_literals,
            }
        },
    );

    parse(input)
}

fn annotation_element_or_literal(input: Span) -> IResult<Span, AnnotationElementOrLiteral> {
    let parse = alt((
        map(annotation_element, AnnotationElementOrLiteral::Element),
        map(literal, AnnotationElementOrLiteral::Literal),
    ));

    parse(input)
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
            f(LocatedSpan::new(input)).unwrap_or_else(|_| panic!("failed to parse {:#?}", input));
        assert_eq!(result, expected);
        assert_eq!(remaining.fragment, "");
    }

    #[test]
    fn test_start() {
        ck(
            start,
            "start = pattern",
            Define(
                0..15,
                Identifier(0..0, "start".to_string()),
                AssignMethod::Assign,
                Pattern::Identifier(Identifier(8..15, "pattern".to_string())),
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
                uri: NamespaceUriLiteral::Uri(Literal(
                    16..53,
                    vec![LiteralSegment {
                        body: "http://relaxng.org/ns/structure/1.0".to_string(),
                    }],
                )),
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
                uri: NamespaceUriLiteral::Uri(Literal(
                    24..61,
                    vec![LiteralSegment {
                        body: "http://relaxng.org/ns/structure/1.0".to_string(),
                    }],
                )),
            },
        );
    }

    #[test]
    fn pattern_list() {
        ck(
            pattern,
            "a,b , c",
            Pattern::ListPair(
                Box::new(Pattern::Identifier(Identifier(0..1, "a".to_string()))),
                Box::new(Pattern::ListPair(
                    Box::new(Pattern::Identifier(Identifier(2..3, "b".to_string()))),
                    Box::new(Pattern::Identifier(Identifier(6..7, "c".to_string()))),
                )),
            ),
        )
    }

    #[test]
    fn pattern_opt() {
        ck(
            pattern,
            "a?",
            Pattern::Optional(Box::new(Pattern::Identifier(Identifier(
                0..1,
                "a".to_string(),
            )))),
        )
    }

    #[test]
    fn attr_pat() {
        ck(
            attribute_pattern,
            "attribute * { text }",
            AttributePattern {
                span: 0..20,
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
                    Identifier(0..1, "a".to_string()),
                )))),
                Box::new(Pattern::Identifier(Identifier(4..5, "b".to_string()))),
            ),
        )
    }

    #[test]
    fn alt_nc() {
        ck(
            name_class,
            "a|b",
            NameClass::Alt(AltName(
                Box::new(NameClass::Name(Name::Identifier(
                    IdentifierOrKeyword::Identifier(Identifier(0..1, "a".to_string())),
                ))),
                Box::new(NameClass::Name(Name::Identifier(
                    IdentifierOrKeyword::Identifier(Identifier(2..3, "b".to_string())),
                ))),
            )),
        )
    }

    #[test]
    fn keyw() {
        ck(
            identifier_or_keyword,
            "parent",
            IdentifierOrKeyword::Keyword(Keyword(0..6, "parent".to_string())),
        );
        ck(
            identifier_or_keyword,
            "parents",
            IdentifierOrKeyword::Identifier(Identifier(0..7, "parents".to_string())),
        );
    }

    #[test]
    fn test_pattern_cname() {
        ck(
            pattern,
            "xsd:string",
            Pattern::DatatypeName(DatatypeNamePattern(
                DatatypeName::CName(QName(
                    NcName(0..3, "xsd".to_string()),
                    NcName(4..10, "string".to_string()),
                )),
                None,
                None,
            )),
        )
    }

    #[test]
    fn test_it() {
        ck(
            name,
            "a:b",
            Name::CName(QName(
                NcName(0..1, "a".to_string()),
                NcName(2..3, "b".to_string()),
            )),
        )
    }

    #[test]
    fn test_include() {
        ck(
            include,
            "include \"foo.rnc\" { a = b  c=d }",
            Include(
                Literal(
                    8..17,
                    vec![LiteralSegment {
                        body: "foo.rnc".to_string(),
                    }],
                ),
                None,
                Some(vec![
                    IncludeContent::Define(Define(
                        20..25,
                        Identifier(20..21, "a".to_string()),
                        AssignMethod::Assign,
                        Pattern::Identifier(Identifier(24..25, "b".to_string())),
                    )),
                    IncludeContent::Define(Define(
                        27..30,
                        Identifier(27..28, "c".to_string()),
                        AssignMethod::Assign,
                        Pattern::Identifier(Identifier(29..30, "d".to_string())),
                    )),
                ]),
            ),
        )
    }

    #[test]
    fn pattern_id_keyword_prefix() {
        ck(
            pattern,
            "external-foo",
            Pattern::Identifier(Identifier(0..12, "external-foo".to_string())),
        )
    }

    #[test]
    fn test_datatypename_pattern_params() {
        ck(
            pattern,
            "ns:foo { pattern = \"bar\" }",
            Pattern::DatatypeName(DatatypeNamePattern(
                DatatypeName::CName(QName(
                    NcName(0..2, "ns".to_string()),
                    NcName(3..6, "foo".to_string()),
                )),
                Some(vec![Param(
                    9..24,
                    None,
                    IdentifierOrKeyword::Identifier(Identifier(9..16, "pattern".to_string())),
                    Literal(
                        19..24,
                        vec![LiteralSegment {
                            body: "bar".to_string(),
                        }],
                    ),
                )]),
                None,
            )),
        )
    }

    #[test]
    fn test_datatypename_pattern() {
        ck(
            top_level,
            "integer.datatype = xsd:integer",
            Schema {
                decls: vec![],
                pattern_or_grammar: PatternOrGrammar::Grammar(GrammarPattern {
                    span: 0..0,
                    content: vec![GrammarContent::Define(Define(
                        0..30,
                        Identifier(0..16, "integer.datatype".to_string()),
                        AssignMethod::Assign,
                        Pattern::DatatypeName(DatatypeNamePattern(
                            DatatypeName::CName(QName(
                                NcName(19..22, "xsd".to_string()),
                                NcName(23..30, "integer".to_string()),
                            )),
                            None,
                            None,
                        )),
                    ))],
                }),
            },
        )
    }

    #[test]
    fn test_grammar_content() {
        // we need the parser to recognise this with the 'define' production, and not get confused
        // into thinking it's a 'pattern' followed by useless trailing "= xsd:integer"
        ck(
            top_level,
            "integer.datatype = xsd:integer",
            Schema {
                decls: vec![],
                pattern_or_grammar: PatternOrGrammar::Grammar(GrammarPattern {
                    span: 0..0,
                    content: vec![GrammarContent::Define(Define(
                        0..30,
                        Identifier(0..16, "integer.datatype".to_string()),
                        AssignMethod::Assign,
                        Pattern::DatatypeName(DatatypeNamePattern(
                            DatatypeName::CName(QName(
                                NcName(19..22, "xsd".to_string()),
                                NcName(23..30, "integer".to_string()),
                            )),
                            None,
                            None,
                        )),
                    ))],
                }),
            },
        )
    }

    #[test]
    fn datatype_value() {
        ck(
            pattern,
            "string \"preserve\"",
            Pattern::DatatypeValue(DatatypeValuePattern(
                Some(DatatypeName::String),
                Literal(
                    7..17,
                    vec![LiteralSegment {
                        body: "preserve".to_string(),
                    }],
                ),
            )),
        )
    }

    #[test]
    fn initial_anno() {
        ck(
            maybe_initial_annotation,
            "[ xml:lang=\"en\" ]",
            Some(InitialAnnotation {
                span: 0..16,
                attribute_annotations: vec![AnnotationAttribute {
                    span: 2..15,
                    name: Name::CName(QName(
                        NcName(2..5, "xml".to_string()),
                        NcName(6..10, "lang".to_string()),
                    )),
                    value: Literal(
                        11..15,
                        vec![LiteralSegment {
                            body: "en".to_string(),
                        }],
                    ),
                }],
                element_annotations: vec![],
            }),
        )
    }

    #[test]
    fn top_level_pattern() {
        ck(
            top_level,
            "grammar { }",
            Schema {
                decls: vec![],
                pattern_or_grammar: PatternOrGrammar::Pattern(Pattern::Grammar(GrammarPattern {
                    span: 0..11,
                    content: vec![],
                })),
            },
        )
    }
}
