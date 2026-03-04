use crate::types::*;
use nom::character::complete::satisfy;
use nom::combinator::cut;
use nom::error::{Error, ParseError};
use nom::multi::separated_list1;
use nom::sequence::preceded;
use nom::{
    AsChar, IResult,
    branch::alt,
    bytes::complete::{is_not, tag, take_until},
    character::{
        complete::{char, multispace1},
        streaming::not_line_ending,
    },
    combinator::{all_consuming, map, not, opt, peek, recognize},
    error::ErrorKind,
    multi::{fold_many0, fold_many1, many0, separated_list0},
    sequence::delimited,
};
use nom::{Input, Parser};
use nom_locate::{LocatedSpan, position};
use std::borrow::Cow;
use std::cell::Cell;
use std::ops::{Range, RangeBounds};

const MAX_RECURSION_DEPTH: usize = 30;

thread_local! {
    static RECURSION_DEPTH: Cell<usize> = const { Cell::new(0) };
}

struct RecursionGuard;

impl RecursionGuard {
    fn enter(input: Span) -> IResult<Span, Self> {
        RECURSION_DEPTH.with(|depth| {
            let current = depth.get();
            if current >= MAX_RECURSION_DEPTH {
                Err(nom::Err::Failure(Error::new(input, ErrorKind::TooLarge)))
            } else {
                depth.set(current + 1);
                Ok((input, RecursionGuard))
            }
        })
    }
}

impl Drop for RecursionGuard {
    fn drop(&mut self) {
        RECURSION_DEPTH.with(|depth| {
            depth.set(depth.get() - 1);
        });
    }
}

pub type Span<'a> = LocatedSpan<&'a str>;

// per https://www.oasis-open.org/committees/relax-ng/compact-20021121.html

// TODO:
//  - check rules are left-factored as required to avoid inefficiently rematching the same sub-rule
//    in multiple alternatives

/// Error from [`resolve_escapes`], with the byte offset span of the invalid
/// escape sequence within the original input.
#[derive(Debug)]
pub struct EscapeError {
    /// Byte range of the escape sequence in the original input.
    pub span: Range<usize>,
    pub message: String,
}

/// Resolve a single `\x{N}` escape sequence, returning the character and the
/// byte length consumed from `input` (which must start with `\x`).
fn resolve_one_escape(input: &str) -> Result<(char, usize), EscapeError> {
    let bytes = input.as_bytes();
    let hex_start = is_escape_at(bytes, 0).ok_or_else(|| EscapeError {
        span: 0..input.len().min(2),
        message: "not an escape sequence".into(),
    })?;
    let mut i = hex_start;
    while i < bytes.len() && bytes[i] != b'}' {
        if !bytes[i].is_ascii_hexdigit() {
            return Err(EscapeError {
                span: 0..i + 1,
                message: format!(
                    "invalid character '{}' in escape sequence",
                    bytes[i] as char
                ),
            });
        }
        i += 1;
    }
    if i >= bytes.len() {
        return Err(EscapeError {
            span: 0..i,
            message: "unterminated escape sequence".into(),
        });
    }
    let hex = &input[hex_start..i];
    let end = i + 1; // include closing '}'
    if hex.is_empty() {
        return Err(EscapeError {
            span: 0..end,
            message: "empty escape sequence".into(),
        });
    }
    let code = u32::from_str_radix(hex, 16).map_err(|_| EscapeError {
        span: 0..end,
        message: "escape value too large".into(),
    })?;
    let c = char::from_u32(code).ok_or_else(|| EscapeError {
        span: 0..end,
        message: format!("\\x{{{hex}}} is not a valid Unicode value"),
    })?;
    if !is_xml_char(c) {
        return Err(EscapeError {
            span: 0..end,
            message: format!("\\x{{{hex}}} is not a valid XML character"),
        });
    }
    Ok((c, end))
}

/// Resolve `\x{N}` escape sequences in a string.
///
/// The escape format is `\x+{hex_digits}` where one or more `x` characters
/// may appear. The hex value must be a valid XML 1.0 Char.
pub fn resolve_escapes(input: &str) -> Result<Cow<'_, str>, EscapeError> {
    resolve_escapes_inner(input, 0)
}

fn resolve_escapes_inner(input: &str, span_offset: usize) -> Result<Cow<'_, str>, EscapeError> {
    let bytes = input.as_bytes();
    let first_escape = find_escape(bytes, 0);
    let Some(first_escape) = first_escape else {
        return Ok(Cow::Borrowed(input));
    };
    let mut result = String::with_capacity(input.len());
    result.push_str(&input[..first_escape]);
    let mut i = first_escape;
    while i < bytes.len() {
        if bytes[i] == b'\\' {
            if is_escape_at(bytes, i).is_some() {
                let (c, len) = resolve_one_escape(&input[i..]).map_err(|mut e| {
                    e.span.start += i + span_offset;
                    e.span.end += i + span_offset;
                    e
                })?;
                result.push(c);
                i += len;
                continue;
            }
            result.push('\\');
            i += 1;
        } else {
            let c = input[i..].chars().next().unwrap();
            result.push(c);
            i += c.len_utf8();
        }
    }
    Ok(Cow::Owned(result))
}

/// Resolve `\x{N}` escape sequences per the compact syntax spec (section 2.4),
/// skipping the bodies of string literals so that escapes producing control
/// characters (like `\x{a}`) do not break the parser.
///
/// Call this on the input before parsing with [`schema()`].
/// Escapes inside string literals are resolved later by the parser.
pub fn resolve_escapes_outside_literals(input: &str) -> Result<Cow<'_, str>, EscapeError> {
    let bytes = input.as_bytes();
    // Quick check: if no escapes at all, return borrowed
    if find_escape(bytes, 0).is_none() {
        return Ok(Cow::Borrowed(input));
    }
    let mut result = String::with_capacity(input.len());
    let mut i = 0;
    while i < bytes.len() {
        // Check for string literal delimiters and skip their bodies
        if bytes[i] == b'"' || bytes[i] == b'\'' {
            let quote = bytes[i];
            // Check for triple-quoted strings
            if i + 2 < bytes.len() && bytes[i + 1] == quote && bytes[i + 2] == quote {
                let delim = &bytes[i..i + 3];
                result.push_str(&input[i..i + 3]);
                i += 3;
                // Copy body verbatim until closing triple quote
                while i < bytes.len() {
                    if i + 2 < bytes.len() && &bytes[i..i + 3] == delim {
                        result.push_str(&input[i..i + 3]);
                        i += 3;
                        break;
                    }
                    let c = input[i..].chars().next().unwrap();
                    result.push(c);
                    i += c.len_utf8();
                }
            } else {
                // Single-quoted string: copy until matching quote or newline
                result.push(quote as char);
                i += 1;
                while i < bytes.len() && bytes[i] != quote && bytes[i] != b'\n' {
                    let c = input[i..].chars().next().unwrap();
                    result.push(c);
                    i += c.len_utf8();
                }
                if i < bytes.len() && bytes[i] == quote {
                    result.push(quote as char);
                    i += 1;
                }
            }
        } else if bytes[i] == b'#' {
            // Comment: copy verbatim until end of line
            while i < bytes.len() && bytes[i] != b'\n' {
                result.push(bytes[i] as char);
                i += 1;
            }
        } else if bytes[i] == b'\\' && is_escape_at(bytes, i).is_some() {
            let (c, len) = resolve_one_escape(&input[i..]).map_err(|mut e| {
                e.span.start += i;
                e.span.end += i;
                e
            })?;
            result.push(c);
            i += len;
        } else {
            let c = input[i..].chars().next().unwrap();
            result.push(c);
            i += c.len_utf8();
        }
    }
    Ok(Cow::Owned(result))
}

/// If `bytes[pos]` is `\` followed by `x+{`, returns the byte index of the
/// first hex digit (just after `{`). Otherwise returns `None`.
fn is_escape_at(bytes: &[u8], pos: usize) -> Option<usize> {
    let mut j = pos + 1;
    let mut x_count = 0;
    while j < bytes.len() && bytes[j] == b'x' {
        x_count += 1;
        j += 1;
    }
    if x_count > 0 && j < bytes.len() && bytes[j] == b'{' {
        Some(j + 1)
    } else {
        None
    }
}

/// Find the byte offset of the first `\x+{` escape in `bytes` starting from `from`.
fn find_escape(bytes: &[u8], from: usize) -> Option<usize> {
    let mut i = from;
    while i < bytes.len() {
        if bytes[i] == b'\\' && is_escape_at(bytes, i).is_some() {
            return Some(i);
        }
        i += 1;
    }
    None
}

fn is_xml_char(c: char) -> bool {
    matches!(c,
        '\u{9}' | '\u{A}' | '\u{D}' |
        '\u{20}'..='\u{D7FF}' |
        '\u{E000}'..='\u{FFFD}' |
        '\u{10000}'..='\u{10FFFF}'
    )
}

pub fn schema(input: Span) -> Result<Schema, nom::Err<Error<Span>>> {
    all_consuming((space_comment0, top_level, space_comment0))
        .parse(input)
        .map(|(_, (_, r, _))| r)
}

// topLevel	  ::=  	decl* (pattern | grammarContent*)
fn top_level(input: Span) -> IResult<Span, Schema> {
    let (input, decls) = separated_list0(space_comment1, decl).parse(input)?;
    let (input, _) = if decls.is_empty() {
        space_comment0(input)?
    } else {
        space_comment1(input)?
    };
    let start = input.location_offset();
    let (input, pattern_or_grammar) = alt((
        map(
            separated_list1(space_comment1, grammar_content),
            |content| {
                PatternOrGrammar::Grammar(GrammarPattern {
                    span: 0..0, // patched below
                    content,
                })
            },
        ),
        map(pattern, PatternOrGrammar::Pattern),
    ))
    .parse(input)?;
    let pattern_or_grammar = match pattern_or_grammar {
        PatternOrGrammar::Grammar(mut g) => {
            g.span = start..input.location_offset();
            PatternOrGrammar::Grammar(g)
        }
        other => other,
    };
    Ok((
        input,
        Schema {
            decls,
            pattern_or_grammar,
        },
    ))
}

// decl      ::=  	"namespace" identifierOrKeyword "=" namespaceURILiteral
//                 | "default" "namespace" [identifierOrKeyword] "=" namespaceURILiteral
//                 | "datatypes" identifierOrKeyword "=" literal
fn decl(input: Span) -> IResult<Span, Decl> {
    alt((
        map(decl_namespace, Decl::Namespace),
        map(decl_default_namespace, Decl::DefaultNamespace),
        map(decl_datatypes, Decl::Datatypes),
    ))
    .parse(input)
}
fn decl_namespace(input: Span) -> IResult<Span, NamespaceDeclaration> {
    let (input, (_keyword, _, prefix, _, _, _, uri)) = (
        tag("namespace"),
        space_comment1,
        identifier_or_keyword,
        space_comment0,
        tag("="),
        space_comment0,
        namespace_uri_literal,
    )
        .parse(input)?;
    IResult::Ok((
        input,
        NamespaceDeclaration {
            prefix: prefix.to_string(),
            uri,
        },
    ))
}
fn decl_default_namespace(input: Span) -> IResult<Span, DefaultNamespaceDeclaration> {
    let (input, (_keyword, _, _, _, prefix, _, _, _, uri)) = (
        tag("default"),
        space_comment1,
        tag("namespace"),
        space_comment1,
        opt(identifier_or_keyword),
        space_comment0,
        tag("="),
        space_comment0,
        namespace_uri_literal,
    )
        .parse(input)?;
    IResult::Ok((
        input,
        DefaultNamespaceDeclaration {
            prefix: prefix.map(|v| v.to_string()),
            uri,
        },
    ))
}
fn decl_datatypes(input: Span) -> IResult<Span, DatatypesDeclaration> {
    let (input, (_keyword, _, prefix, _, _, _, uri)) = (
        tag("datatypes"),
        space_comment1,
        identifier_or_keyword,
        space_comment0,
        tag("="),
        space_comment0,
        literal,
    )
        .parse(input)?;
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
    ))
    .parse(input)
}

// namespaceURILiteral	  ::=  	literal
//                             | "inherit"
fn namespace_uri_literal(input: Span) -> IResult<Span, NamespaceUriLiteral> {
    alt((
        map(tag("inherit"), |_| NamespaceUriLiteral::Inherit),
        map(literal, NamespaceUriLiteral::Uri),
    ))
    .parse(input)
}

// literal	  ::=  	literalSegment ("~" literalSegment)+
fn literal(input: Span) -> IResult<Span, Literal> {
    let parser = (
        position,
        separated_list1((space_comment0, tag("~"), space_comment0), literal_segment),
        position,
    );
    let mut parser = map(parser, |(start, v, end)| Literal(span(start, end), v));

    parser.parse(input)
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
    ))
    .parse(input)?;

    let body_offset = body.location_offset();
    let body_str = body.fragment();
    let resolved = resolve_escapes_inner(body_str, body_offset).map_err(|_| {
        nom::Err::Failure(Error {
            input: body,
            code: ErrorKind::Char,
        })
    })?;

    IResult::Ok((
        input,
        LiteralSegment {
            body: resolved.into_owned(),
        },
    ))
}

fn span(start: LocatedSpan<&str>, end: LocatedSpan<&str>) -> Range<usize> {
    Range {
        start: start.location_offset(),
        end: end.location_offset(),
    }
}

// identifier	  ::=  	(NCName - keyword)
//                   | quotedIdentifier
fn identifier(input: Span) -> IResult<Span, Identifier> {
    let res = alt((
        recognize((tag("\\"), keyword)),
        recognize((not(peek(keyword)), nc_name)),
    ))
    .parse(input);

    res.map(|(input, v)| (input, Identifier(span(v, input), v.to_string())))
}

pub fn nc_name(input: Span) -> IResult<Span, NcName> {
    let parse = (
        position,
        recognize((nc_name_start_char, many0(nc_name_char))),
        position,
    );

    let mut parser = map(parse, |(start, v, end)| {
        NcName(span(start, end), v.fragment().to_string())
    });

    parser.parse(input)
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
    ))
    .parse(input)
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
    ))
    .parse(input)
}

fn keyword(input: Span) -> IResult<Span, Keyword> {
    let parse = (
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
    );

    let mut parser = map(parse, |(k, end, _)| {
        Keyword(span(k, end), k.fragment().to_string())
    });

    parser.parse(input)
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
// Parse a single atomic pattern with annotations and postfix operators (?, *, +).
// This does NOT handle binary operators (|, &, ,) — those are handled iteratively
// in `pattern` to avoid O(n) recursion depth on long chains like `a | b | c | ...`.
fn pattern_primary(input: Span) -> IResult<Span, Pattern> {
    let (input, annotations) = maybe_annotations(input)?;
    let (input, _) = ws0(input)?;
    let (input, mut result) = alt((
        map(element_pattern, Pattern::Element),
        map(attribute_pattern, Pattern::Attribute),
        map(list_pattern, Pattern::List),
        map(mixed_pattern, Pattern::Mixed),
        map(datatype_value_pattern, Pattern::DatatypeValue),
        map(datatype_param_pattern, Pattern::DatatypeName),
        map(identifier, Pattern::Identifier),
        map((tag("parent"), space_comment1, identifier), |(_, _, p)| {
            Pattern::Parent(p)
        }),
        map((position, tag("empty"), position), |(s, _, e)| {
            Pattern::Empty(Some(span(s, e)))
        }),
        map((position, tag("text"), position), |(s, _, e)| {
            Pattern::Text(Some(span(s, e)))
        }),
        map((position, tag("notAllowed"), position), |(s, _, e)| {
            Pattern::NotAllowed(Some(span(s, e)))
        }),
        map(external_pattern, Pattern::External),
        map(grammar_pattern, Pattern::Grammar),
        map(group_pattern, |p| Pattern::Group(Box::new(p))),
    ))
    .parse(input)?;

    let (mut input, follow_annotations) = follow_annotation_list(input)?;

    // Wrap in Annotated if there are any annotations
    if annotations.is_some() || !follow_annotations.is_empty() {
        let mut annos = annotations.unwrap_or(Annotations {
            documentation: vec![],
            initial: None,
            follow_elements: vec![],
        });
        annos.follow_elements = follow_annotations;
        result = Pattern::Annotated(annos, Box::new(result));
    }

    // Postfix operators bind tighter than binary operators
    loop {
        let (i, _) = space_comment0(input)?;
        if let Ok((i, _)) = tag::<_, _, (Span, ErrorKind)>("?")(i) {
            result = Pattern::Optional(None, Box::new(result));
            input = i;
            continue;
        }
        if let Ok((i, _)) = tag::<_, _, (Span, ErrorKind)>("*")(i) {
            result = Pattern::ZeroOrMore(None, Box::new(result));
            input = i;
            continue;
        }
        if let Ok((i, _)) = tag::<_, _, (Span, ErrorKind)>("+")(i) {
            result = Pattern::OneOrMore(None, Box::new(result));
            input = i;
            continue;
        }
        break;
    }
    IResult::Ok((input, result))
}

fn pattern(input: Span) -> IResult<Span, Pattern> {
    let (input, _guard) = RecursionGuard::enter(input)?;
    let (mut input, mut result) = pattern_primary(input)?;

    loop {
        let (i, _) = space_comment0(input)?;
        if let Ok((i, _)) = tag::<_, _, (Span, ErrorKind)>(",")(i) {
            let (i, _) = space_comment0(i)?;
            let (i, right) = pattern_primary(i)?;
            result = Pattern::ListPair(Box::new(result), Box::new(right));
            input = i;
            continue;
        }
        if let Ok((i, _)) = tag::<_, _, (Span, ErrorKind)>("&")(i) {
            let (i, _) = space_comment0(i)?;
            let (i, right) = pattern_primary(i)?;
            result = Pattern::InterleavePair(Box::new(result), Box::new(right));
            input = i;
            continue;
        }
        if let Ok((i, _)) = tag::<_, _, (Span, ErrorKind)>("|")(i) {
            let (i, _) = space_comment0(i)?;
            let (i, right) = pattern_primary(i)?;
            result = Pattern::ChoicePair(Box::new(result), Box::new(right));
            input = i;
            continue;
        }
        break;
    }
    IResult::Ok((input, result))
}
// "element" nameClass "{" pattern "}"
fn element_pattern(input: Span) -> IResult<Span, ElementPattern> {
    let parse = (
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
    );

    map(
        parse,
        |(start, _, name_class, _, _, _, pattern, _, _, end)| ElementPattern {
            span: span(start, end),
            name_class,
            pattern: Box::new(pattern),
        },
    )
    .parse(input)
}

// "attribute" nameClass "{" pattern "}"
fn attribute_pattern(input: Span) -> IResult<Span, AttributePattern> {
    let parse = (
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
    );

    map(
        parse,
        |(start, _, name_class, _, _, _, pattern, _, _, end)| AttributePattern {
            span: span(start, end),
            name_class,
            pattern: Box::new(pattern),
        },
    )
    .parse(input)
}
fn list_pattern(input: Span) -> IResult<Span, ListPattern> {
    let parse = (
        tag("list"),
        space_comment0,
        tag("{"),
        space_comment0,
        cut(pattern),
        space_comment0,
        cut(tag("}")),
    );

    map(parse, |(_, _, _, _, pattern, _, _)| {
        ListPattern(Box::new(pattern))
    })
    .parse(input)
}
fn mixed_pattern(input: Span) -> IResult<Span, MixedPattern> {
    let parse = (
        tag("mixed"),
        space_comment0,
        tag("{"),
        space_comment0,
        cut(pattern),
        space_comment0,
        cut(tag("}")),
    );

    map(parse, |(_, _, _, _, pattern, _, _)| {
        MixedPattern(Box::new(pattern))
    })
    .parse(input)
}

// "external" anyURILiteral [inherit]
fn external_pattern(input: Span) -> IResult<Span, ExternalPattern> {
    let parse = (
        tag("external"),
        space_comment1,
        any_uri_literal,
        opt(map((space_comment1, inherit), |(_, inherit)| inherit)),
    );

    let mut parser = map(parse, |(_, _, uri, inherit)| {
        ExternalPattern(uri, inherit, None)
    });

    parser.parse(input)
}

// "grammar" "{" grammarContent* "}"
fn grammar_pattern(input: Span) -> IResult<Span, GrammarPattern> {
    let parse = (
        tag("grammar"),
        space_comment0,
        tag("{"),
        space_comment0,
        separated_list0(space_comment1, grammar_content),
        space_comment0,
        cut(tag("}")),
        position,
    );

    let mut parser = map(parse, |(start, _, _, _, content, _, _, end)| {
        GrammarPattern {
            span: span(start, end),
            content,
        }
    });

    parser.parse(input)
}

fn group_pattern(input: Span) -> IResult<Span, Pattern> {
    let parse = (tag("("), space_comment0, pattern, space_comment0, tag(")"));

    let mut parser = map(parse, |(_, _, content, _, _)| content);

    parser.parse(input)
}

// [datatypeName] datatypeValue
fn datatype_value_pattern(input: Span) -> IResult<Span, DatatypeValuePattern> {
    let parse = (
        position,
        opt(datatype_name),
        space_comment0,
        datatype_value,
        position,
    );

    let mut parser = map(parse, |(start, name, _, value, end)| {
        DatatypeValuePattern(span(start, end), name, value, None, vec![])
    });

    parser.parse(input)
}

// datatypeName ["{" param* "}"] [exceptPattern]
fn datatype_param_pattern(input: Span) -> IResult<Span, DatatypeNamePattern> {
    let params = (
        space_comment0,
        tag("{"),
        space_comment0,
        separated_list0(space_comment1, param),
        space_comment0,
        cut(tag("}")),
    );
    let params = map(params, |(_, _, _, p, _, _)| p);

    let parse = (
        position,
        datatype_name,
        opt(params),
        opt(map((space_comment0, except_pattern), |(_, e)| e)),
        position,
    );
    let mut parser = map(parse, |(start, name, params, except, end)| {
        DatatypeNamePattern(span(start, end), name, params, except.map(Box::new))
    });

    parser.parse(input)
}

// datatypeValue  ::= 	literal
fn datatype_value(input: Span) -> IResult<Span, Literal> {
    literal(input)
}

// param	  ::=  	identifierOrKeyword "=" literal
fn param(input: Span) -> IResult<Span, Param> {
    let parse = (
        position,
        maybe_annotations,
        ws0,
        identifier_or_keyword,
        space_comment0,
        tag("="),
        space_comment0,
        literal,
        position,
    );

    let mut parser = map(
        parse,
        |(start, annotations, _, name, _, _, _, value, end)| Param {
            span: span(start, end),
            annotations,
            name,
            value,
        },
    );

    parser.parse(input)
}

// exceptPattern     ::=   "-" pattern
fn except_pattern(input: Span) -> IResult<Span, Pattern> {
    let parse = (tag("-"), space_comment0, pattern);

    let mut parser = map(parse, |(_, _, pat)| pat);

    parser.parse(input)
}

// datatypeName  ::=   CName
//                   | "string"
//                   | "token"
fn datatype_name(input: Span) -> IResult<Span, DatatypeName> {
    alt((
        map(tag("string"), |_| DatatypeName::String),
        map(tag("token"), |_| DatatypeName::Token),
        map(cname, DatatypeName::CName),
    ))
    .parse(input)
}

// Parse a single atomic name class with annotations.
// Does NOT handle the binary `|` operator — that is handled iteratively in `name_class`.
fn name_class_primary(input: Span) -> IResult<Span, NameClass> {
    let (input, annotations) = maybe_annotations(input)?;
    let (input, _) = ws0(input)?;
    let (input, left) = alt((
        map(ns_name_nc, NameClass::NsName),
        map(name, NameClass::Name),
        map(any_name_nc, NameClass::AnyName),
        map(paren_nc, NameClass::Paren),
    ))
    .parse(input)?;

    let (input, follow_annotations) = follow_annotation_list(input)?;

    // Wrap in Annotated if there are any annotations
    let left = if annotations.is_some() || !follow_annotations.is_empty() {
        let mut annos = annotations.unwrap_or(Annotations {
            documentation: vec![],
            initial: None,
            follow_elements: vec![],
        });
        annos.follow_elements = follow_annotations;
        NameClass::Annotated(annos, Box::new(left))
    } else {
        left
    };

    Ok((input, left))
}

fn name_class(input: Span) -> IResult<Span, NameClass> {
    let (input, _guard) = RecursionGuard::enter(input)?;
    let (mut input, mut result) = name_class_primary(input)?;

    loop {
        let (i, _) = space_comment0(input)?;
        if let Ok((i, _)) = tag::<_, _, (Span, ErrorKind)>("|")(i) {
            let (i, _) = space_comment0(i)?;
            let (i, right) = name_class_primary(i)?;
            result = NameClass::Alt(AltName(Box::new(result), Box::new(right)));
            input = i;
            continue;
        }
        break;
    }

    Ok((input, result))
}

// name	  ::=  	identifierOrKeyword
//            | CName
fn name(input: Span) -> IResult<Span, Name> {
    alt((
        map(cname, Name::CName),
        map(identifier_or_keyword, Name::Identifier),
    ))
    .parse(input)
}

fn ns_name_nc(input: Span) -> IResult<Span, NsName> {
    let parse = (
        nc_name,
        tag(":*"),
        opt((space_comment0, tag("-"), space_comment0, name_class)),
    );

    let mut parser = map(parse, |(name, _, except)| NsName {
        name: NamespaceOrPrefix::Prefix(name),
        except: except.map(|(_, _, _, name_class)| Box::new(name_class)),
    });

    parser.parse(input)
}

fn any_name_nc(input: Span) -> IResult<Span, AnyName> {
    let parse = (
        tag("*"),
        opt((space_comment0, tag("-"), space_comment0, name_class)),
    );

    let mut parser = map(parse, |(_, except)| {
        AnyName(except.map(|(_, _, _, name_class)| Box::new(name_class)))
    });

    parser.parse(input)
}

fn paren_nc(input: Span) -> IResult<Span, ParenName> {
    let parse = (
        tag("("),
        space_comment0,
        name_class,
        space_comment0,
        tag(")"),
    );

    let mut parser = map(parse, |(_, _, name_class, _, _)| {
        ParenName(Box::new(name_class))
    });

    parser.parse(input)
}

// NCName ":" NCName
fn cname(input: Span) -> IResult<Span, QName> {
    let parse = (nc_name, tag(":"), nc_name);

    let mut parser = map(parse, |(prefix, _, local_name)| QName(prefix, local_name));

    parser.parse(input)
}

pub fn char_in<I, R, Error: ParseError<I>>(range: R) -> impl FnMut(I) -> IResult<I, char, Error>
where
    I: Input,
    <I as Input>::Item: AsChar,
    R: RangeBounds<char>,
{
    satisfy(move |c| range.contains(&c.as_char()))
}

// grammarContent	  ::=  	start
//                        | define
//                        | "div" "{" grammarContent* "}"
//                        | "include" anyURILiteral [inherit] ["{" includeContent* "}"]
fn grammar_content(input: Span) -> IResult<Span, GrammarContent> {
    let (input, _guard) = RecursionGuard::enter(input)?;
    let (input, annotations) = maybe_annotations(input)?;
    let (input, _) = ws0(input)?;
    let (input, mut item) = alt((
        map(start, GrammarContent::Define),
        map(define, GrammarContent::Define),
        map(div_grammar_content, GrammarContent::Div),
        map(include, GrammarContent::Include),
        map(annotation_element, GrammarContent::Annotation),
    ))
    .parse(input)?;

    // Attach annotations to the parsed item
    if let Some(annotations) = annotations {
        match &mut item {
            GrammarContent::Define(d) => {
                d.annotations = Some(annotations);
            }
            GrammarContent::Include(i) => {
                i.annotations = Some(annotations);
            }
            // For Div and Annotation, annotations are not currently stored
            _ => {}
        }
    }

    Ok((input, item))
}

// start	  ::=  	"start" assignMethod pattern
fn start(input: Span) -> IResult<Span, Define> {
    let parser = (
        position,
        tag("start"),
        space_comment0,
        assign_method,
        space_comment0,
        cut(pattern),
        position,
    );

    // we just produce another 'Define' named "start", rather than using a dedicated 'Start' type,
    // so as to avoid duplication of code handling 'start' definitions and other definitions

    let mut parser = map(
        parser,
        |(start, start_tag, _, assign_method, _, pattern, end)| Define {
            span: span(start, end),
            identifier: Identifier(span(start_tag, start_tag), "start".to_string()),
            assign_method,
            pattern,
            annotations: None,
        },
    );

    parser.parse(input)
}

// define	  ::=  	identifier assignMethod pattern
fn define(input: Span) -> IResult<Span, Define> {
    let parse = (
        position,
        identifier,
        space_comment0,
        assign_method,
        space_comment0,
        cut(pattern),
        position,
    );

    let mut parser = map(
        parse,
        |(start, identifier, _, assign_method, _, pattern, end)| Define {
            span: span(start, end),
            identifier,
            assign_method,
            pattern,
            annotations: None,
        },
    );

    parser.parse(input)
}

fn assign_method(input: Span) -> IResult<Span, AssignMethod> {
    alt((
        map(tag("="), |_| AssignMethod::Assign),
        map(tag("|="), |_| AssignMethod::Choice),
        map(tag("&="), |_| AssignMethod::Interleave),
    ))
    .parse(input)
}

// "div" "{" grammarContent* "}"
fn div_grammar_content(input: Span) -> IResult<Span, Vec<GrammarContent>> {
    let parse = (
        tag("div"),
        space_comment0,
        tag("{"),
        space_comment0,
        separated_list0(space_comment1, grammar_content),
        space_comment0,
        cut(tag("}")),
    );

    let mut parser = map(parse, |(_, _, _, _, content, _, _)| content);

    parser.parse(input)
}

// "include" anyURILiteral [inherit] ["{" includeContent* "}"]
fn include(input: Span) -> IResult<Span, Include> {
    let parse = (
        tag("include"),
        space_comment1,
        any_uri_literal,
        opt(map((space_comment1, inherit), |(_, v)| v)),
        opt(map(
            (
                space_comment0,
                tag("{"),
                space_comment0,
                separated_list0(space_comment1, include_content),
                space_comment0,
                cut(tag("}")),
            ),
            |(_, _, _, inc, _, _)| inc,
        )),
    );

    let mut parser = map(parse, |(_, _, uri, inherit, content)| Include {
        uri,
        inherit,
        content,
        annotations: None,
        ns: None,
    });

    parser.parse(input)
}

// anyURILiteral	  ::=  	literal
fn any_uri_literal(input: Span) -> IResult<Span, Literal> {
    literal(input)
}

// inherit	  ::=  	"inherit" "=" identifierOrKeyword
fn inherit(input: Span) -> IResult<Span, Inherit> {
    let parse = (
        tag("inherit"),
        space_comment0,
        tag("="),
        space_comment0,
        identifier_or_keyword,
    );

    let mut parser = map(parse, |(_, _, _, _, id)| Inherit(id));

    parser.parse(input)
}

// includeContent  ::=  define
//                    | start
//                    | "div" "{" includeContent* "}"
fn include_content(input: Span) -> IResult<Span, IncludeContent> {
    let (input, _guard) = RecursionGuard::enter(input)?;
    let (input, annotations) = maybe_annotations(input)?;
    let (input, _) = ws0(input)?;
    let (input, mut item) = alt((
        map(annotation_element, IncludeContent::Annotation),
        map(define, IncludeContent::Define),
        map(start, IncludeContent::Define),
        map(div_include_content, IncludeContent::Div),
    ))
    .parse(input)?;

    if let (Some(annotations), IncludeContent::Define(d)) = (annotations, &mut item) {
        d.annotations = Some(annotations);
    }

    Ok((input, item))
}

// "div" "{" includeContent* "}"
fn div_include_content(input: Span) -> IResult<Span, Vec<IncludeContent>> {
    let parse = (
        tag("div"),
        space_comment0,
        tag("{"),
        space_comment0,
        separated_list0(space_comment1, include_content),
        space_comment0,
        cut(tag("}")),
    );

    let mut parser = map(parse, |(_, _, _, _, content, _, _)| content);

    parser.parse(input)
}

fn space_comment0(input: Span) -> IResult<Span, Span> {
    recognize(fold_many0(
        alt((multispace1, any_comment)),
        || (),
        |_, _| (),
    ))
    .parse(input)
}
fn space_comment1(input: Span) -> IResult<Span, Span> {
    recognize(fold_many1(
        alt((multispace1, any_comment)),
        || (),
        |_, _| (),
    ))
    .parse(input)
}

/// Consume whitespace and single-# comments only (not ## documentation comments)
fn ws0(input: Span) -> IResult<Span, Span> {
    recognize(fold_many0(
        alt((multispace1, single_comment)),
        || (),
        |_, _| (),
    ))
    .parse(input)
}
fn ws1(input: Span) -> IResult<Span, Span> {
    recognize(fold_many1(
        alt((multispace1, single_comment)),
        || (),
        |_, _| (),
    ))
    .parse(input)
}

/// Matches any comment line (both # and ##)
fn any_comment(input: Span) -> IResult<Span, Span> {
    recognize((tag("#"), not_line_ending)).parse(input)
}

/// Matches a single-# comment but NOT ## documentation comments
fn single_comment(input: Span) -> IResult<Span, Span> {
    recognize((tag("#"), not(peek(tag("#"))), not_line_ending)).parse(input)
}

/// Parse a single ## documentation line. Returns the content after ##,
/// with one leading space stripped if present.
fn documentation(input: Span) -> IResult<Span, Documentation> {
    use nom::character::complete::not_line_ending as complete_not_line_ending;
    let (input, start) = position(input)?;
    let (input, _) = tag("##")(input)?;
    // Must NOT start with another # (that would be ###, not documentation)
    let (input, _) = not(peek(tag("#"))).parse(input)?;
    let (input, content) = complete_not_line_ending(input)?;
    let (input, end) = position(input)?;
    let content_str = content.fragment().to_string();
    // Strip one leading space after ## if present
    let content_str = content_str
        .strip_prefix(' ')
        .unwrap_or(&content_str)
        .to_string();
    Ok((
        input,
        Documentation {
            span: span(start, end),
            content: content_str,
        },
    ))
}

/// Parse zero or more ## documentation lines (separated by whitespace/single-# comments)
fn documentations(input: Span) -> IResult<Span, Vec<Documentation>> {
    separated_list0(ws1, documentation).parse(input)
}

fn maybe_initial_annotation(input: Span) -> IResult<Span, Option<InitialAnnotation>> {
    opt(map((initial_annotation, ws0), |(anno, _)| anno)).parse(input)
}

/// Parse optional annotations: zero or more ## doc lines, then optionally [...]
fn maybe_annotations(input: Span) -> IResult<Span, Option<Annotations>> {
    let (input, docs) = documentations(input)?;
    let (input, _) = ws0(input)?;
    let (input, initial) = maybe_initial_annotation(input)?;
    if docs.is_empty() && initial.is_none() {
        Ok((input, None))
    } else {
        Ok((
            input,
            Some(Annotations {
                documentation: docs,
                initial,
                follow_elements: vec![],
            }),
        ))
    }
}

fn initial_annotation(input: Span) -> IResult<Span, InitialAnnotation> {
    let parse = (
        tag("["),
        space_comment0,
        separated_list0(space_comment1, annotation_attribute),
        space_comment0,
        separated_list0(space_comment1, annotation_element),
        space_comment0,
        tag("]"),
    );

    let mut parser = map(
        parse,
        |(start, _, attribute_annotations, _, element_annotations, _, end)| InitialAnnotation {
            span: span(start, end),
            attribute_annotations,
            element_annotations,
        },
    );

    parser.parse(input)
}

fn follow_annotation_list(input: Span) -> IResult<Span, Vec<AnnotationElement>> {
    separated_list0(space_comment0, follow_annotation).parse(input)
}

fn follow_annotation(input: Span) -> IResult<Span, AnnotationElement> {
    preceded(
        (space_comment0, tag(">>"), space_comment0),
        annotation_element,
    )
    .parse(input)
}

fn annotation_attribute(input: Span) -> IResult<Span, AnnotationAttribute> {
    // Per spec, annotation attributes must use foreignAttributeName which is always a CName
    let parse = (
        map(cname, Name::CName),
        space_comment0,
        tag("="),
        space_comment0,
        literal,
    );

    let mut parser = map(parse, |(name, _, _, _, value)| AnnotationAttribute {
        span: Range {
            start: name.span().start,
            end: value.0.end,
        },
        name,
        value,
    });

    parser.parse(input)
}

/// Per spec, nestedAnnotationAttributes use anyAttributeName (identifierOrKeyword | prefixedName)
fn nested_annotation_attribute(input: Span) -> IResult<Span, AnnotationAttribute> {
    let parse = (name, space_comment0, tag("="), space_comment0, literal);

    let mut parser = map(parse, |(name, _, _, _, value)| AnnotationAttribute {
        span: Range {
            start: name.span().start,
            end: value.0.end,
        },
        name,
        value,
    });

    parser.parse(input)
}

fn annotation_element(input: Span) -> IResult<Span, AnnotationElement> {
    let (input, _guard) = RecursionGuard::enter(input)?;
    let parse = (
        name,
        space_comment0,
        tag("["),
        space_comment0,
        separated_list0(space_comment1, nested_annotation_attribute),
        space_comment0,
        separated_list0(space_comment1, annotation_element_or_literal),
        space_comment0,
        tag("]"),
    );

    let mut parser = map(
        parse,
        |(name, _, _, _, annotation_attributes, _, annotation_elements_or_literals, _, end)| {
            AnnotationElement {
                span: Range {
                    start: name.span().start,
                    end: end.location_offset() + end.fragment().len(),
                },
                name,
                annotation_attributes,
                annotation_elements_or_literals,
            }
        },
    );

    parser.parse(input)
}

fn annotation_element_or_literal(input: Span) -> IResult<Span, AnnotationElementOrLiteral> {
    let mut parser = alt((
        map(annotation_element, AnnotationElementOrLiteral::Element),
        map(literal, AnnotationElementOrLiteral::Literal),
    ));

    parser.parse(input)
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
            f(LocatedSpan::new(input)).unwrap_or_else(|_| panic!("failed to parse {input:#?}"));
        assert_eq!(result, expected);
        assert_eq!(remaining.fragment(), &"");
    }

    #[test]
    fn test_start() {
        ck(
            start,
            "start = pattern",
            Define {
                span: 0..15,
                identifier: Identifier(0..0, "start".to_string()),
                assign_method: AssignMethod::Assign,
                pattern: Pattern::Identifier(Identifier(8..15, "pattern".to_string())),
                annotations: None,
            },
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
                Box::new(Pattern::ListPair(
                    Box::new(Pattern::Identifier(Identifier(0..1, "a".to_string()))),
                    Box::new(Pattern::Identifier(Identifier(2..3, "b".to_string()))),
                )),
                Box::new(Pattern::Identifier(Identifier(6..7, "c".to_string()))),
            ),
        )
    }

    #[test]
    fn pattern_opt() {
        ck(
            pattern,
            "a?",
            Pattern::Optional(
                None,
                Box::new(Pattern::Identifier(Identifier(0..1, "a".to_string()))),
            ),
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
                pattern: Box::new(Pattern::Text(Some(14..18))),
            },
        )
    }

    #[test]
    fn list_zeromore_patt() {
        ck(
            pattern,
            "a*, b",
            Pattern::ListPair(
                Box::new(Pattern::ZeroOrMore(
                    None,
                    Box::new(Pattern::Identifier(Identifier(0..1, "a".to_string()))),
                )),
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
                0..10,
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
            Include {
                uri: Literal(
                    8..17,
                    vec![LiteralSegment {
                        body: "foo.rnc".to_string(),
                    }],
                ),
                inherit: None,
                content: Some(vec![
                    IncludeContent::Define(Define {
                        span: 20..25,
                        identifier: Identifier(20..21, "a".to_string()),
                        assign_method: AssignMethod::Assign,
                        pattern: Pattern::Identifier(Identifier(24..25, "b".to_string())),
                        annotations: None,
                    }),
                    IncludeContent::Define(Define {
                        span: 27..30,
                        identifier: Identifier(27..28, "c".to_string()),
                        assign_method: AssignMethod::Assign,
                        pattern: Pattern::Identifier(Identifier(29..30, "d".to_string())),
                        annotations: None,
                    }),
                ]),
                annotations: None,
                ns: None,
            },
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
                0..26,
                DatatypeName::CName(QName(
                    NcName(0..2, "ns".to_string()),
                    NcName(3..6, "foo".to_string()),
                )),
                Some(vec![Param {
                    span: 9..24,
                    annotations: None,
                    name: IdentifierOrKeyword::Identifier(Identifier(9..16, "pattern".to_string())),
                    value: Literal(
                        19..24,
                        vec![LiteralSegment {
                            body: "bar".to_string(),
                        }],
                    ),
                }]),
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
                    span: 0..30,
                    content: vec![GrammarContent::Define(Define {
                        span: 0..30,
                        identifier: Identifier(0..16, "integer.datatype".to_string()),
                        assign_method: AssignMethod::Assign,
                        pattern: Pattern::DatatypeName(DatatypeNamePattern(
                            19..30,
                            DatatypeName::CName(QName(
                                NcName(19..22, "xsd".to_string()),
                                NcName(23..30, "integer".to_string()),
                            )),
                            None,
                            None,
                        )),
                        annotations: None,
                    })],
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
                    span: 0..30,
                    content: vec![GrammarContent::Define(Define {
                        span: 0..30,
                        identifier: Identifier(0..16, "integer.datatype".to_string()),
                        assign_method: AssignMethod::Assign,
                        pattern: Pattern::DatatypeName(DatatypeNamePattern(
                            19..30,
                            DatatypeName::CName(QName(
                                NcName(19..22, "xsd".to_string()),
                                NcName(23..30, "integer".to_string()),
                            )),
                            None,
                            None,
                        )),
                        annotations: None,
                    })],
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
                0..17,
                Some(DatatypeName::String),
                Literal(
                    7..17,
                    vec![LiteralSegment {
                        body: "preserve".to_string(),
                    }],
                ),
                None,
                vec![],
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

    #[test]
    fn test_documentation_single() {
        ck(
            documentation,
            "## A greeting",
            Documentation {
                span: 0..13,
                content: "A greeting".to_string(),
            },
        )
    }

    #[test]
    fn test_documentation_strips_leading_space() {
        ck(
            documentation,
            "##No space",
            Documentation {
                span: 0..10,
                content: "No space".to_string(),
            },
        )
    }

    #[test]
    fn test_documentations_multiple() {
        let input = "## Line one\n## Line two";
        let (remaining, result) = documentations(LocatedSpan::new(input)).expect("failed to parse");
        assert_eq!(remaining.fragment(), &"");
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].content, "Line one");
        assert_eq!(result[1].content, "Line two");
    }

    #[test]
    fn test_maybe_annotations_docs_only() {
        let input = "## A doc comment\nelement";
        let (remaining, result) =
            maybe_annotations(LocatedSpan::new(input)).expect("failed to parse");
        assert_eq!(remaining.fragment(), &"element");
        let annos = result.unwrap();
        assert_eq!(annos.documentation.len(), 1);
        assert_eq!(annos.documentation[0].content, "A doc comment");
        assert!(annos.initial.is_none());
    }

    #[test]
    fn test_maybe_annotations_bracket_only() {
        let input = "[ xml:lang=\"en\" ] element";
        let (remaining, result) =
            maybe_annotations(LocatedSpan::new(input)).expect("failed to parse");
        assert_eq!(remaining.fragment(), &"element");
        let annos = result.unwrap();
        assert!(annos.documentation.is_empty());
        assert!(annos.initial.is_some());
        assert_eq!(annos.initial.unwrap().attribute_annotations.len(), 1);
    }

    #[test]
    fn test_maybe_annotations_docs_and_bracket() {
        let input = "## Some docs\n[ xml:lang=\"en\" ] element";
        let (remaining, result) =
            maybe_annotations(LocatedSpan::new(input)).expect("failed to parse");
        assert_eq!(remaining.fragment(), &"element");
        let annos = result.unwrap();
        assert_eq!(annos.documentation.len(), 1);
        assert_eq!(annos.documentation[0].content, "Some docs");
        assert!(annos.initial.is_some());
    }

    #[test]
    fn test_pattern_with_doc_annotation() {
        let input = "## A greeting\nelement greeting { text }";
        let (remaining, result) = pattern(LocatedSpan::new(input)).expect("failed to parse");
        assert_eq!(remaining.fragment(), &"");
        match result {
            Pattern::Annotated(annos, inner) => {
                assert_eq!(annos.documentation.len(), 1);
                assert_eq!(annos.documentation[0].content, "A greeting");
                assert!(matches!(*inner, Pattern::Element(_)));
            }
            other => panic!("Expected Annotated pattern, got {other:?}"),
        }
    }

    #[test]
    fn test_pattern_with_bracket_annotation() {
        let input = "[ xml:lang=\"en\" ] element foo { text }";
        let (remaining, result) = pattern(LocatedSpan::new(input)).expect("failed to parse");
        assert_eq!(remaining.fragment(), &"");
        match result {
            Pattern::Annotated(annos, inner) => {
                assert!(annos.initial.is_some());
                assert!(matches!(*inner, Pattern::Element(_)));
            }
            other => panic!("Expected Annotated pattern, got {other:?}"),
        }
    }

    #[test]
    fn test_unannotated_pattern_no_wrap() {
        // Unannotated patterns should NOT be wrapped in Annotated
        ck(pattern, "text", Pattern::Text(Some(0..4)));
    }

    #[test]
    fn test_grammar_content_with_doc_annotation() {
        let input = "## The start\nstart = element root { empty }";
        let (remaining, result) =
            grammar_content(LocatedSpan::new(input)).expect("failed to parse");
        assert_eq!(remaining.fragment(), &"");
        match result {
            GrammarContent::Define(d) => {
                assert!(d.annotations.is_some());
                let annos = d.annotations.unwrap();
                assert_eq!(annos.documentation.len(), 1);
                assert_eq!(annos.documentation[0].content, "The start");
            }
            other => panic!("Expected Define, got {other:?}"),
        }
    }

    #[test]
    fn test_single_comment_not_doc() {
        // A single # comment should NOT be parsed as documentation
        let input = "# regular comment\nelement foo { text }";
        let (remaining, result) = pattern(LocatedSpan::new(input)).expect("failed to parse");
        assert_eq!(remaining.fragment(), &"");
        // No annotation wrapping since # comment is consumed as whitespace
        assert!(matches!(result, Pattern::Element(_)));
    }

    #[test]
    fn test_annotation_attribute_requires_cname() {
        // Unprefixed attribute names should fail
        let result = annotation_attribute(LocatedSpan::new("lang=\"en\""));
        assert!(result.is_err());
    }

    #[test]
    fn deeply_nested_parens_pattern() {
        let depth = 200;
        let input = "(".repeat(depth) + "empty" + &")".repeat(depth);
        let result = schema(LocatedSpan::new(&input));
        assert!(result.is_err());
    }

    #[test]
    fn deeply_nested_parens_name_class() {
        let depth = 200;
        let parens = "(".repeat(depth) + "foo" + &")".repeat(depth);
        let input = format!("start = element {parens} {{ text }}");
        let result = schema(LocatedSpan::new(&input));
        assert!(result.is_err());
    }

    #[test]
    fn long_choice_chain() {
        // Regression test: long | chains should not hit the recursion limit
        // because binary operators are parsed iteratively, not recursively.
        let names: Vec<String> = (0..50).map(|i| format!("x{i}")).collect();
        let input = format!("start = {}", names.join(" | "));
        let result = schema(LocatedSpan::new(&input));
        assert!(result.is_ok(), "50-way choice should parse: {result:?}");
    }

    #[test]
    fn deeply_nested_div_grammar() {
        let depth = 200;
        let input = "div { ".repeat(depth) + "a = empty" + &" }".repeat(depth);
        let result = schema(LocatedSpan::new(&input));
        assert!(result.is_err());
    }

    #[test]
    fn deeply_nested_div_include() {
        let depth = 200;
        let divs = "div { ".repeat(depth) + "a = empty" + &" }".repeat(depth);
        let input = format!("include \"foo.rnc\" {{ {divs} }}");
        let result = schema(LocatedSpan::new(&input));
        assert!(result.is_err());
    }

    #[test]
    fn deeply_nested_annotation_elements() {
        let depth = 200;
        let input = "a:x [ ".repeat(depth) + "\"leaf\"" + &" ]".repeat(depth);
        // Try parsing as an annotation element directly
        let result = annotation_element(LocatedSpan::new(&input));
        assert!(result.is_err());
    }

    #[test]
    fn moderate_nesting_succeeds() {
        // A moderate depth should still parse fine
        let depth = 20;
        let input = "(".repeat(depth) + "empty" + &")".repeat(depth);
        let result = schema(LocatedSpan::new(&input));
        assert!(result.is_ok());
    }

    #[test]
    fn escape_basic() {
        // \x{66}\x{6f}\x{6f} == "foo"
        assert_eq!(resolve_escapes(r"\x{66}\x{6f}\x{6f}").unwrap(), "foo");
    }

    #[test]
    fn escape_multiple_x() {
        // Multiple x's are allowed: \xx{41} == "A"
        assert_eq!(resolve_escapes(r"\xx{41}").unwrap(), "A");
        assert_eq!(resolve_escapes(r"\xxx{41}").unwrap(), "A");
    }

    #[test]
    fn escape_mixed_with_text() {
        assert_eq!(
            resolve_escapes(r"hello \x{20} world").unwrap(),
            "hello   world"
        );
    }

    #[test]
    fn escape_no_escapes() {
        assert_eq!(resolve_escapes("plain text").unwrap(), "plain text");
    }

    #[test]
    fn escape_backslash_not_followed_by_x() {
        // \keyword should pass through unchanged
        assert_eq!(resolve_escapes(r"\keyword").unwrap(), r"\keyword");
    }

    #[test]
    fn escape_invalid_xml_char() {
        // U+0000 is not a valid XML char
        assert!(resolve_escapes(r"\x{0}").is_err());
    }

    #[test]
    fn escape_unterminated() {
        assert!(resolve_escapes(r"\x{41").is_err());
    }

    #[test]
    fn escape_empty_hex() {
        assert!(resolve_escapes(r"\x{}").is_err());
    }

    #[test]
    fn escape_non_bmp() {
        // U+10000 LINEAR B SYLLABLE B008 A
        assert_eq!(resolve_escapes(r"\x{10000}").unwrap(), "\u{10000}");
    }

    #[test]
    fn escape_in_schema() {
        // element \x{66}\x{6f}\x{6f} { empty } -- from the spec
        let input = resolve_escapes(r"element \x{66}\x{6f}\x{6f} { empty }").unwrap();
        let result = schema(LocatedSpan::new(&input));
        assert!(result.is_ok());
    }

    #[test]
    fn escape_outside_literals_skips_strings() {
        // \x{a} in a double-quoted string should be left as-is
        let input = r#"element foo { text "\x{a}" }"#;
        let result = resolve_escapes_outside_literals(input).unwrap();
        assert!(
            result.contains(r"\x{a}"),
            "escape in string should be preserved"
        );
    }

    #[test]
    fn escape_outside_literals_resolves_outside() {
        // \x{66} outside a string should be resolved
        let input = r"element \x{66}oo { empty }";
        let result = resolve_escapes_outside_literals(input).unwrap();
        assert_eq!(result.as_ref(), "element foo { empty }");
    }

    #[test]
    fn escape_outside_literals_skips_single_quoted() {
        let input = r"element foo { text '\x{a}' }";
        let result = resolve_escapes_outside_literals(input).unwrap();
        assert!(result.contains(r"\x{a}"));
    }

    #[test]
    fn escape_outside_literals_skips_triple_double() {
        let input = r#"element foo { text """\x{a}""" }"#;
        let result = resolve_escapes_outside_literals(input).unwrap();
        assert!(result.contains(r"\x{a}"));
    }

    #[test]
    fn escape_outside_literals_skips_triple_single() {
        let input = r"element foo { text '''\x{a}''' }";
        let result = resolve_escapes_outside_literals(input).unwrap();
        assert!(result.contains(r"\x{a}"));
    }

    #[test]
    fn escape_outside_literals_skips_comments() {
        let input = "element foo { empty } # \\x{a} in comment\n";
        let result = resolve_escapes_outside_literals(input).unwrap();
        assert!(result.contains(r"\x{a}"));
    }

    #[test]
    fn escape_newline_in_string_literal() {
        // \x{a} (newline) in a string literal should parse successfully
        // and produce the newline character in the literal value
        let input =
            resolve_escapes_outside_literals(r#"element foo { attribute bar { "\x{a}" } }"#)
                .unwrap();
        let result = schema(LocatedSpan::new(input.as_ref()));
        assert!(
            result.is_ok(),
            "schema with \\x{{a}} in string should parse: {:?}",
            result.err()
        );
    }

    #[test]
    fn escape_newline_in_triple_quoted_string() {
        let input =
            resolve_escapes_outside_literals(r#"element foo { attribute bar { """\x{a}""" } }"#)
                .unwrap();
        let result = schema(LocatedSpan::new(input.as_ref()));
        assert!(result.is_ok());
    }

    #[test]
    fn escape_tab_in_string_literal() {
        // \x{9} (tab) should also work in string literals
        let input =
            resolve_escapes_outside_literals(r#"element foo { attribute bar { "\x{9}" } }"#)
                .unwrap();
        let result = schema(LocatedSpan::new(input.as_ref()));
        assert!(result.is_ok());
    }

    #[test]
    fn literal_segment_resolves_escape() {
        // literal_segment should resolve \x{41} to 'A'
        let input = LocatedSpan::new(r#""\x{41}""#);
        let (_, seg) = literal_segment(input).unwrap();
        assert_eq!(seg.body, "A");
    }

    #[test]
    fn literal_segment_resolves_newline_escape() {
        // \x{a} should produce a newline in the literal body
        let input = LocatedSpan::new(r#""hello\x{a}world""#);
        let (_, seg) = literal_segment(input).unwrap();
        assert_eq!(seg.body, "hello\nworld");
    }

    #[test]
    fn literal_segment_mixed_text_and_escapes() {
        let input = LocatedSpan::new(r#""a\x{62}c""#);
        let (_, seg) = literal_segment(input).unwrap();
        assert_eq!(seg.body, "abc");
    }
}
