use crate::syntax::types::*;
use nom::Parser;
use nom::character::complete::satisfy;
use nom::combinator::cut;
use nom::multi::separated_list1;
use nom::sequence::preceded;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_till, take_until, take_while, take_while1},
    combinator::{all_consuming, map, not, opt, peek, recognize, verify},
    error::ErrorKind,
    multi::{fold_many0, fold_many1, many0, separated_list0},
    sequence::delimited,
};
use nom_locate::{LocatedSpan, position};
use std::borrow::Cow;
use std::cell::Cell;
use std::fmt;
use std::ops::Range;

const MAX_RECURSION_DEPTH: usize = 30;

/// Error from the compact syntax parser.
#[derive(Debug, PartialEq)]
pub enum Error<I> {
    /// The input doesn't match the grammar; the [`ErrorKind`] names the nom parser that gave up.
    Nom(I, ErrorKind),
    /// An operator applied to a `-` (except) expression without the parentheses the spec
    /// requires, e.g. `xsd:string - "a" | "b"`; the input is at the operator.
    ExceptNeedsParentheses(I),
    /// An annotation that would translate to a second top-level element: a follow annotation on
    /// the top-level pattern, or an annotation element on a top-level value; the span is that of
    /// the annotation, in original source offsets.
    TopLevelFollowAnnotation(Range<usize>),
}

impl<I> nom::error::ParseError<I> for Error<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        Error::Nom(input, kind)
    }

    fn append(_input: I, _kind: ErrorKind, other: Self) -> Self {
        other
    }
}

type IResult<I, O> = nom::IResult<I, O, Error<I>>;

thread_local! {
    static RECURSION_DEPTH: Cell<usize> = const { Cell::new(0) };
}

struct RecursionGuard;

impl RecursionGuard {
    fn enter(input: Span) -> IResult<Span, Self> {
        RECURSION_DEPTH.with(|depth| {
            let current = depth.get();
            if current >= MAX_RECURSION_DEPTH {
                Err(nom::Err::Failure(Error::Nom(input, ErrorKind::TooLarge)))
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

// per https://www.oasis-open.org/committees/relax-ng/compact-20021121.html

// TODO:
//  - check rules are left-factored as required to avoid inefficiently rematching the same sub-rule
//    in multiple alternatives
//
//  - the spec (Appendix A, sections 2.1 and 2.2) allows UTF-16 input (detected by a byte
//    order mark, or specified by metadata such as a MIME charset), and supports a leading byte
//    order mark in UTF-8.  We only support only UTF-8 without a BOM. It's not clear that
//    there is a real-world need for anything else.

/// Maps byte offsets in preprocessed text (see [`preprocess`]) back to offsets in the original
/// source.
///
/// Each entry `(decoded, original)` records offsets at which the two texts realign after a
/// transformation that changed the length of the text (an escape sequence, or a CR LF pair).
/// Between entries, the two texts correspond byte for byte.
#[derive(Clone, Copy, Default, Debug)]
pub struct SourceMap<'a>(&'a [(usize, usize)]);

/// The entries from which a [`SourceMap`] is built.
type OffsetPairs = Vec<(usize, usize)>;

impl SourceMap<'_> {
    fn original(&self, decoded: usize) -> usize {
        match self.0.partition_point(|&(d, _)| d <= decoded) {
            0 => decoded,
            i => {
                let (d, o) = self.0[i - 1];
                o + (decoded - d)
            }
        }
    }
}

/// Parser input.
///
/// The text being parsed is the output of [`preprocess`], so `location_offset()` gives offsets
/// that may differ from those in the original source.  Offsets that end up in the AST or in
/// errors must be derived via [`span`] or [`original_offset`], which translate them back.
pub type Span<'a> = LocatedSpan<&'a str, SourceMap<'a>>;

fn original_offset(s: &Span) -> usize {
    s.extra.original(s.location_offset())
}

/// Character substituted for an escaped line feed (`\x{A}`) in preprocessed text, so that the
/// tokeniser can tell it apart from a newline in the source text.  The spec allows the former,
/// but not the latter, inside single-line literals.  U+FFFF is not a legal XML character, and
/// [`preprocess`] rejects it in the source text, so it can't be confused with real input.
const ESCAPED_LF: char = '\u{FFFF}';

/// Problems with the source text found before tokenisation, by [`preprocess`].
///
/// Spans are byte offsets in the original source.
#[derive(Debug, Clone, PartialEq)]
pub enum InputError {
    /// A character in the source text that isn't a legal XML 1.0 `Char`.
    InvalidChar { span: Range<usize>, ch: char },
    /// A `\x{` sequence that doesn't form a complete escape sequence.
    MalformedEscape {
        span: Range<usize>,
        problem: EscapeProblem,
    },
    /// A well-formed escape sequence whose value isn't a legal XML 1.0 `Char`.
    InvalidEscapedChar { span: Range<usize>, code: u32 },
}

#[derive(Debug, Clone, PartialEq)]
pub enum EscapeProblem {
    /// The input ended before the closing `}`.
    Unterminated,
    /// No hex digits between the braces.
    Empty,
    /// Something other than a hex digit or `}` followed the opening `{`.
    NonHexDigit(char),
    /// The value exceeds the maximum Unicode code point, U+10FFFF.
    OutOfRange,
}

impl InputError {
    pub fn span(&self) -> Range<usize> {
        match self {
            InputError::InvalidChar { span, .. }
            | InputError::MalformedEscape { span, .. }
            | InputError::InvalidEscapedChar { span, .. } => span.clone(),
        }
    }

    fn offset_by(mut self, by: usize) -> Self {
        match &mut self {
            InputError::InvalidChar { span, .. }
            | InputError::MalformedEscape { span, .. }
            | InputError::InvalidEscapedChar { span, .. } => {
                span.start += by;
                span.end += by;
            }
        }
        self
    }
}

impl fmt::Display for InputError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InputError::InvalidChar { ch, .. } => write!(
                f,
                "character U+{:04X} is not allowed in a schema (not a legal XML character)",
                *ch as u32
            ),
            InputError::MalformedEscape { problem, .. } => match problem {
                EscapeProblem::Unterminated => {
                    f.write_str("unterminated escape sequence (missing '}')")
                }
                EscapeProblem::Empty => f.write_str("empty escape sequence (no hex digits)"),
                EscapeProblem::NonHexDigit(c) => write!(
                    f,
                    "invalid character {c:?} in escape sequence; expected a hex digit or '}}'"
                ),
                EscapeProblem::OutOfRange => f.write_str(
                    "escape sequence value exceeds the maximum Unicode code point, 10FFFF",
                ),
            },
            InputError::InvalidEscapedChar { code, .. } => {
                if char::from_u32(*code).is_some() {
                    write!(
                        f,
                        "escape sequence denotes U+{code:04X}, which is not a legal XML character"
                    )
                } else {
                    write!(
                        f,
                        "escape sequence denotes U+{code:04X}, a surrogate code point, which is not a legal XML character"
                    )
                }
            }
        }
    }
}

impl std::error::Error for InputError {}

/// Error from [`parse`].
#[derive(Debug)]
pub enum CompactError {
    /// The source text was rejected before tokenisation.
    Input(InputError),
    /// The text could not be parsed.  The span is in original source offsets.
    Syntax { span: Range<usize>, kind: ErrorKind },
    /// An operator applied to a `-` (except) expression without parentheses; the span is that of
    /// the operator.
    ExceptNeedsParentheses { span: Range<usize> },
    /// An annotation that would translate to a second top-level element: a follow annotation
    /// (`>>`) on the top-level pattern, or an annotation element on a top-level value; the span is
    /// that of the annotation.
    TopLevelFollowAnnotation { span: Range<usize> },
}

/// Resolve a single `\x{N}` escape sequence, returning the character and the byte length consumed
/// from `input`, which must start with a `\x+{` escape opener.  Error spans are relative to the
/// start of `input`.
fn resolve_one_escape(input: &str) -> Result<(char, usize), InputError> {
    let bytes = input.as_bytes();
    let hex_start = is_escape_at(bytes, 0).expect("caller must check for an escape opener");
    let malformed = |end, problem| InputError::MalformedEscape {
        span: 0..end,
        problem,
    };
    let mut i = hex_start;
    while i < bytes.len() && bytes[i] != b'}' {
        if !bytes[i].is_ascii_hexdigit() {
            let c = input[i..].chars().next().unwrap();
            return Err(malformed(i + c.len_utf8(), EscapeProblem::NonHexDigit(c)));
        }
        i += 1;
    }
    if i >= bytes.len() {
        return Err(malformed(i, EscapeProblem::Unterminated));
    }
    let hex = &input[hex_start..i];
    let end = i + 1; // include closing '}'
    if hex.is_empty() {
        return Err(malformed(end, EscapeProblem::Empty));
    }
    let code = u32::from_str_radix(hex, 16)
        .ok()
        .filter(|&c| c <= 0x10FFFF)
        .ok_or_else(|| malformed(end, EscapeProblem::OutOfRange))?;
    match char::from_u32(code).filter(|&c| is_xml_char(c)) {
        Some(c) => Ok((c, end)),
        None => Err(InputError::InvalidEscapedChar { span: 0..end, code }),
    }
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

fn is_xml_char(c: char) -> bool {
    matches!(c,
        '\u{9}' | '\u{A}' | '\u{D}' |
        '\u{20}'..='\u{D7FF}' |
        '\u{E000}'..='\u{FFFD}' |
        '\u{10000}'..='\u{10FFFF}'
    )
}

/// Performs the lexical stages that the spec (Appendix A, section 2) requires before
/// tokenisation:
///
///  - newline normalisation: each CR LF pair, lone CR, or lone LF becomes `\n`
///  - escape interpretation: each `\x{N}` escape is replaced by the character it denotes, except
///    that an escaped line feed becomes [`ESCAPED_LF`]
///
/// This differs from the spec, which proposes that the literal linefeeds (after normalisation) be
/// replaced by a sentinel, and escaped linefeeds be replaced by literal linefeeds.
///
/// Also rejects any character that isn't a legal XML 1.0 `Char`, since such characters are not
/// valid, and it means that the sentinel value `ESCAPED_LF` can't appear in the input and clash.
///
/// Returns the text to tokenise, and the offset mapping for a [`SourceMap`].  If the source needs
/// no changes, it is returned borrowed and the mapping is empty.
fn preprocess(src: &str) -> Result<(Cow<'_, str>, OffsetPairs), InputError> {
    let bytes = src.as_bytes();
    let mut out: Option<String> = None;
    let mut map = Vec::new();
    // `src[..copied]` has been fully transferred to `out`
    let mut copied = 0;
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'\r' => {
                let crlf = bytes.get(i + 1) == Some(&b'\n');
                let out = out.get_or_insert_with(|| String::with_capacity(src.len()));
                out.push_str(&src[copied..i]);
                out.push('\n');
                i += if crlf { 2 } else { 1 };
                copied = i;
                if crlf {
                    map.push((out.len(), i));
                }
            }
            b'\\' if is_escape_at(bytes, i).is_some() => {
                let (c, len) = resolve_one_escape(&src[i..]).map_err(|e| e.offset_by(i))?;
                let out = out.get_or_insert_with(|| String::with_capacity(src.len()));
                out.push_str(&src[copied..i]);
                out.push(if c == '\n' { ESCAPED_LF } else { c });
                i += len;
                copied = i;
                map.push((out.len(), i));
            }
            b @ (0x00..=0x08 | 0x0B | 0x0C | 0x0E..=0x1F) => {
                return Err(InputError::InvalidChar {
                    span: i..i + 1,
                    ch: b as char,
                });
            }
            // U+FFFE and U+FFFF are encoded as EF BF BE and EF BF BF.  0xEF is only ever a lead
            // byte in UTF-8, so this can't match in the middle of some other character.
            0xEF if bytes.get(i + 1) == Some(&0xBF)
                && matches!(bytes.get(i + 2), Some(0xBE | 0xBF)) =>
            {
                return Err(InputError::InvalidChar {
                    span: i..i + 3,
                    ch: src[i..].chars().next().unwrap(),
                });
            }
            _ => i += 1,
        }
    }
    Ok(match out {
        None => (Cow::Borrowed(src), map),
        Some(mut out) => {
            out.push_str(&src[copied..]);
            (Cow::Owned(out), map)
        }
    })
}

/// Parse a schema in the compact syntax.
pub fn parse(source: &str) -> Result<Schema, CompactError> {
    let (text, map) = preprocess(source).map_err(CompactError::Input)?;
    let input = Span::new_extra(&text, SourceMap(&map));
    schema(input).map_err(|e| match e {
        nom::Err::Error(Error::TopLevelFollowAnnotation(span))
        | nom::Err::Failure(Error::TopLevelFollowAnnotation(span)) => {
            CompactError::TopLevelFollowAnnotation { span }
        }
        nom::Err::Error(Error::ExceptNeedsParentheses(input))
        | nom::Err::Failure(Error::ExceptNeedsParentheses(input)) => {
            let start = original_offset(&input);
            CompactError::ExceptNeedsParentheses {
                span: start..start + 1,
            }
        }
        nom::Err::Error(Error::Nom(input, kind)) | nom::Err::Failure(Error::Nom(input, kind)) => {
            let end = input.location_offset() + input.fragment().len();
            CompactError::Syntax {
                span: original_offset(&input)..input.extra.original(end),
                kind,
            }
        }
        nom::Err::Incomplete(_) => CompactError::Syntax {
            span: source.len()..source.len(),
            kind: ErrorKind::Eof,
        },
    })
}

fn schema(input: Span) -> Result<Schema, nom::Err<Error<Span>>> {
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
    let (input, start) = position(input)?;
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
    if let PatternOrGrammar::Pattern(p) = &pattern_or_grammar
        && let Some(follow) = top_level_follow_annotation(p)
    {
        return Err(nom::Err::Failure(Error::TopLevelFollowAnnotation(
            follow.span.clone(),
        )));
    }
    let pattern_or_grammar = match pattern_or_grammar {
        PatternOrGrammar::Grammar(mut g) => {
            g.span = span(start, input);
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

/// Find an annotation that would translate to an element following the top-level pattern's
/// element, which the spec forbids, as "a top-level pattern [mustn't translate] to a sequence of
/// more than one element".  That is a follow annotation on the pattern as a whole, or an
/// annotation element on a value, which can't have element children.  Annotations within the
/// top-level pattern, e.g. on one alternative of a choice, stay within its element.
fn top_level_follow_annotation(top: &Pattern) -> Option<&AnnotationElement> {
    let mut p = top;
    while let Pattern::Annotated(annos, inner) = p {
        if let Some(follow) = annos.follow_elements.first() {
            return Some(follow);
        }
        if let Pattern::DatatypeValue(_) = **inner
            && let Some(element) = annos
                .initial
                .as_ref()
                .and_then(|i| i.element_annotations.first())
        {
            return Some(element);
        }
        p = inner;
    }
    None
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

    // an escaped newline is allowed even in single-line literals, and denotes a newline
    IResult::Ok((
        input,
        LiteralSegment {
            body: body.fragment().replace(ESCAPED_LF, "\n"),
        },
    ))
}

/// The range between two input positions, as byte offsets in the original source.
fn span(start: Span, end: Span) -> Range<usize> {
    original_offset(&start)..original_offset(&end)
}

// identifier	  ::=  	(NCName - keyword)
//                   | quotedIdentifier
fn identifier(input: Span) -> IResult<Span, Identifier> {
    let (input, start) = position(input)?;
    let (input, text) = alt((
        map(recognize((tag("\\"), keyword)), |v: Span| v.to_string()),
        map(preceded(not(peek(keyword)), nc_name), |name: NcName| name.1),
    ))
    .parse(input)?;
    let (input, end) = position(input)?;
    Ok((input, Identifier(span(start, end), text)))
}

pub(crate) fn nc_name(input: Span) -> IResult<Span, NcName> {
    let (input, start) = position(input)?;
    let (input, name) =
        recognize((satisfy(is_nc_name_start_char), take_while(is_nc_name_char))).parse(input)?;
    let (input, end) = position(input)?;
    Ok((input, NcName(span(start, end), name.fragment().to_string())))
}

fn nc_name_char(input: Span) -> IResult<Span, char> {
    satisfy(is_nc_name_char).parse(input)
}

fn is_nc_name_start_char(c: char) -> bool {
    // per https://www.w3.org/TR/REC-xml/#NT-NameStartChar -- but without ':'
    matches!(c,
        'A'..='Z' |
        '_' |
        'a'..='z' |
        '\u{C0}'..='\u{D6}' |
        '\u{D8}'..='\u{F6}' |
        '\u{F8}'..='\u{2FF}' |
        '\u{370}'..='\u{37D}' |
        '\u{37F}'..='\u{1FFF}' |
        '\u{200C}'..='\u{200D}' |
        '\u{2070}'..='\u{218F}' |
        '\u{2C00}'..='\u{2FEF}' |
        '\u{3001}'..='\u{D7FF}' |
        '\u{F900}'..='\u{FDCF}' |
        '\u{FDF0}'..='\u{FFFD}' |
        '\u{10000}'..='\u{EFFFF}'
    )
}

fn is_nc_name_char(c: char) -> bool {
    is_nc_name_start_char(c)
        || matches!(c,
            '-' |
            '.' |
            '0'..='9' |
            '\u{B7}' |
            '\u{0300}'..='\u{036F}' |
            '\u{203F}'..='\u{2040}'
        )
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
    // annotatedPrimary: follow annotations here belong to the primary, inside any repetition
    let (input, primary) = lead_annotated_primary(input)?;
    let (input, primary) = data_except(input, primary)?;
    let (input, follow) = follow_annotation_list(input)?;
    let mut result = with_follow_annotations(primary, follow);
    let mut input = input;

    // repeatedPrimary: a postfix operator, which binds tighter than binary operators, and then
    // follow annotations for the repetition as a whole
    loop {
        let (op_at, _) = space_comment0(input)?;
        let Ok((i, op)) =
            satisfy::<_, _, Error<Span>>(|c| matches!(c, '?' | '*' | '+')).parse(op_at)
        else {
            return Ok((input, result));
        };
        if is_data_except(&result) {
            return Err(nom::Err::Failure(Error::ExceptNeedsParentheses(op_at)));
        }
        let repeated = Box::new(result);
        let repeated = match op {
            '?' => Pattern::Optional(None, repeated),
            '*' => Pattern::ZeroOrMore(None, repeated),
            _ => Pattern::OneOrMore(None, repeated),
        };
        let (i, follow) = follow_annotation_list(i)?;
        result = with_follow_annotations(repeated, follow);
        input = i;
    }
}

/// leadAnnotatedPrimary: a primary pattern, with any leading annotations (`##` documentation,
/// and `[...]`)
fn lead_annotated_primary(input: Span) -> IResult<Span, Pattern> {
    let (input, annotations) = maybe_annotations(input)?;
    let (input, _) = space_comment0(input)?;
    let (input, primary) = alt((
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
    let result = match annotations {
        Some(annos) => Pattern::Annotated(annos, Box::new(primary)),
        None => primary,
    };
    Ok((input, result))
}

/// dataExcept: if `primary` is a datatype name, with any parameters, followed by `-`, add the
/// except pattern, a lead-annotated primary.  Leading annotations on `primary` then apply to the
/// whole, as do any follow annotations after the except pattern.
fn data_except(input: Span, mut primary: Pattern) -> IResult<Span, Pattern> {
    let target = match &mut primary {
        Pattern::Annotated(_, inner) => &mut **inner,
        other => other,
    };
    if let Pattern::DatatypeName(DatatypeNamePattern(span, _, _, except @ None)) = target {
        let (input, operand) = opt(preceded(
            (space_comment0, tag("-"), space_comment0),
            cut(lead_annotated_primary),
        ))
        .parse(input)?;
        if let Some(operand) = operand {
            *except = Some(Box::new(operand));
            span.end = original_offset(&input);
        }
        return Ok((input, primary));
    }
    Ok((input, primary))
}

/// Is `p` a datatype with an except pattern, which may not be the operand of another operator
/// without parentheses?
fn is_data_except(p: &Pattern) -> bool {
    match p {
        Pattern::Annotated(_, inner) => is_data_except(inner),
        Pattern::DatatypeName(DatatypeNamePattern(_, _, _, except)) => except.is_some(),
        _ => false,
    }
}

/// Attach `>>` follow annotations to `pattern`, sharing its `Annotated` wrapper if it has leading
/// annotations but no follow annotations yet.
fn with_follow_annotations(pattern: Pattern, follow: Vec<AnnotationElement>) -> Pattern {
    if follow.is_empty() {
        return pattern;
    }
    match pattern {
        Pattern::Annotated(mut annos, inner) if annos.follow_elements.is_empty() => {
            annos.follow_elements = follow;
            Pattern::Annotated(annos, inner)
        }
        other => Pattern::Annotated(
            Annotations {
                documentation: vec![],
                initial: None,
                follow_elements: follow,
            },
            Box::new(other),
        ),
    }
}

fn pattern(input: Span) -> IResult<Span, Pattern> {
    let (input, _guard) = RecursionGuard::enter(input)?;
    let (mut input, mut result) = pattern_primary(input)?;

    loop {
        let (op_at, _) = space_comment0(input)?;
        let Ok((i, op)) =
            satisfy::<_, _, Error<Span>>(|c| matches!(c, ',' | '&' | '|')).parse(op_at)
        else {
            break;
        };
        let parens_needed = || nom::Err::Failure(Error::ExceptNeedsParentheses(op_at));
        if is_data_except(&result) {
            return Err(parens_needed());
        }
        let (i, _) = space_comment0(i)?;
        let (i, right) = pattern_primary(i)?;
        if is_data_except(&right) {
            return Err(parens_needed());
        }
        let (left, right) = (Box::new(result), Box::new(right));
        result = match op {
            ',' => Pattern::ListPair(left, right),
            '&' => Pattern::InterleavePair(left, right),
            _ => Pattern::ChoicePair(left, right),
        };
        input = i;
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

    let parse = (position, datatype_name, opt(params), position);
    // any except pattern is added by pattern_primary()
    let mut parser = map(parse, |(start, name, params, end)| {
        DatatypeNamePattern(span(start, end), name, params, None)
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
        space_comment0,
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
    let (input, primary) = lead_annotated_primary_name_class(input)?;
    let (input, primary) = name_class_except(input, primary)?;
    let (input, follow) = follow_annotation_list(input)?;
    if follow.is_empty() {
        return Ok((input, primary));
    }
    // follow annotations share the wrapper of any leading annotations
    let result = match primary {
        NameClass::Annotated(mut annos, inner) => {
            annos.follow_elements = follow;
            NameClass::Annotated(annos, inner)
        }
        other => NameClass::Annotated(
            Annotations {
                documentation: vec![],
                initial: None,
                follow_elements: follow,
            },
            Box::new(other),
        ),
    };
    Ok((input, result))
}

/// A primary name class, with any leading annotations: a name, `ns:*`, `*` or a parenthesised
/// name class, but not an except
fn lead_annotated_primary_name_class(input: Span) -> IResult<Span, NameClass> {
    let (input, annotations) = maybe_annotations(input)?;
    let (input, _) = space_comment0(input)?;
    let (input, primary) = alt((
        map(ns_name_nc, NameClass::NsName),
        map(name, NameClass::Name),
        map(any_name_nc, NameClass::AnyName),
        map(paren_nc, NameClass::Paren),
    ))
    .parse(input)?;
    let result = match annotations {
        Some(annos) => NameClass::Annotated(annos, Box::new(primary)),
        None => primary,
    };
    Ok((input, result))
}

/// If `primary` is `ns:*` or `*` followed by `-`, add the except name class, a lead-annotated
/// primary.  Leading annotations on `primary` then apply to the whole.
fn name_class_except(input: Span, mut primary: NameClass) -> IResult<Span, NameClass> {
    let target = match &mut primary {
        NameClass::Annotated(_, inner) => &mut **inner,
        other => other,
    };
    let except = match target {
        NameClass::NsName(NsName { except, .. }) | NameClass::AnyName(AnyName(except)) => except,
        _ => return Ok((input, primary)),
    };
    let (input, operand) = opt(preceded(
        (space_comment0, tag("-"), space_comment0),
        cut(lead_annotated_primary_name_class),
    ))
    .parse(input)?;
    *except = operand.map(Box::new);
    Ok((input, primary))
}

/// Is `nc` an except name class, which may not be the operand of `|` without parentheses?
fn is_name_class_except(nc: &NameClass) -> bool {
    match nc {
        NameClass::Annotated(_, inner) => is_name_class_except(inner),
        NameClass::NsName(NsName { except, .. }) | NameClass::AnyName(AnyName(except)) => {
            except.is_some()
        }
        _ => false,
    }
}

fn name_class(input: Span) -> IResult<Span, NameClass> {
    let (input, _guard) = RecursionGuard::enter(input)?;
    let (mut input, mut result) = name_class_primary(input)?;

    loop {
        let (op_at, _) = space_comment0(input)?;
        let Ok((i, _)) = tag::<_, _, Error<Span>>("|")(op_at) else {
            break;
        };
        let parens_needed = || nom::Err::Failure(Error::ExceptNeedsParentheses(op_at));
        if is_name_class_except(&result) {
            return Err(parens_needed());
        }
        let (i, _) = space_comment0(i)?;
        let (i, right) = name_class_primary(i)?;
        if is_name_class_except(&right) {
            return Err(parens_needed());
        }
        result = NameClass::Alt(AltName(Box::new(result), Box::new(right)));
        input = i;
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
    // any except is added by name_class_primary()
    let mut parser = map((nc_name, tag(":*")), |(name, _)| NsName {
        name: NamespaceOrPrefix::Prefix(name),
        except: None,
    });

    parser.parse(input)
}

fn any_name_nc(input: Span) -> IResult<Span, AnyName> {
    // any except is added by name_class_primary()
    let mut parser = map(tag("*"), |_| AnyName(None));

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

// grammarContent	  ::=  	start
//                        | define
//                        | "div" "{" grammarContent* "}"
//                        | "include" anyURILiteral [inherit] ["{" includeContent* "}"]
//
// An annotation element may appear in place of grammarContent, but without annotations of its
// own (the spec's `member ::= annotatedComponent | annotationElementNotKeyword`).
fn grammar_content(input: Span) -> IResult<Span, GrammarContent> {
    let (input, _guard) = RecursionGuard::enter(input)?;
    alt((
        map(annotation_element_not_keyword, GrammarContent::Annotation),
        annotated_grammar_component,
    ))
    .parse(input)
}

fn annotated_grammar_component(input: Span) -> IResult<Span, GrammarContent> {
    let (input, annotations) = maybe_annotations(input)?;
    let (input, _) = space_comment0(input)?;
    let (input, mut item) = alt((
        map(start, GrammarContent::Define),
        map(define, GrammarContent::Define),
        map(div_grammar_content, GrammarContent::Div),
        map(include, GrammarContent::Include),
    ))
    .parse(input)?;
    match &mut item {
        GrammarContent::Define(Define { annotations: a, .. })
        | GrammarContent::Include(Include { annotations: a, .. })
        | GrammarContent::Div(Div { annotations: a, .. }) => *a = annotations,
        GrammarContent::Annotation(_) => unreachable!("not parsed here"),
    }
    Ok((input, item))
}

/// An annotation element in place of grammar or include content, which can't have an unprefixed
/// keyword as its name, e.g. `div [ ]`
fn annotation_element_not_keyword(input: Span) -> IResult<Span, AnnotationElement> {
    verify(annotation_element, |e| {
        !matches!(e.name, Name::Identifier(IdentifierOrKeyword::Keyword(_)))
    })
    .parse(input)
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
fn div_grammar_content(input: Span) -> IResult<Span, Div<GrammarContent>> {
    let parse = (
        tag("div"),
        space_comment0,
        tag("{"),
        space_comment0,
        separated_list0(space_comment1, grammar_content),
        space_comment0,
        cut(tag("}")),
    );

    let mut parser = map(parse, |(_, _, _, _, content, _, _)| Div {
        annotations: None,
        content,
    });

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
//
// As for grammarContent, an annotation element may appear here, without annotations of its own
fn include_content(input: Span) -> IResult<Span, IncludeContent> {
    let (input, _guard) = RecursionGuard::enter(input)?;
    alt((
        map(annotation_element_not_keyword, IncludeContent::Annotation),
        annotated_include_component,
    ))
    .parse(input)
}

fn annotated_include_component(input: Span) -> IResult<Span, IncludeContent> {
    let (input, annotations) = maybe_annotations(input)?;
    let (input, _) = space_comment0(input)?;
    let (input, mut item) = alt((
        map(start, IncludeContent::Define),
        map(define, IncludeContent::Define),
        map(div_include_content, IncludeContent::Div),
    ))
    .parse(input)?;
    match &mut item {
        IncludeContent::Define(Define { annotations: a, .. })
        | IncludeContent::Div(Div { annotations: a, .. }) => *a = annotations,
        IncludeContent::Annotation(_) => unreachable!("not parsed here"),
    }
    Ok((input, item))
}

// "div" "{" includeContent* "}"
fn div_include_content(input: Span) -> IResult<Span, Div<IncludeContent>> {
    let parse = (
        tag("div"),
        space_comment0,
        tag("{"),
        space_comment0,
        separated_list0(space_comment1, include_content),
        space_comment0,
        cut(tag("}")),
    );

    let mut parser = map(parse, |(_, _, _, _, content, _, _)| Div {
        annotations: None,
        content,
    });

    parser.parse(input)
}

/// Whitespace and comments, per the spec's `separator`.  A line starting `##` is not a comment
/// but documentation, so it is left for the annotation parsers; a `##` line anywhere else is a
/// syntax error, as in Jing.
fn space_comment0(input: Span) -> IResult<Span, Span> {
    recognize(fold_many0(
        alt((separator_chars, comment)),
        || (),
        |_, _| (),
    ))
    .parse(input)
}
fn space_comment1(input: Span) -> IResult<Span, Span> {
    recognize(fold_many1(
        alt((separator_chars, comment)),
        || (),
        |_, _| (),
    ))
    .parse(input)
}

/// Zero or more `item`s, with optional whitespace and comments between them.  Whitespace is
/// allowed, not required, between tokens; where two tokens would otherwise run together, e.g.
/// two NCNames, the item parsers' longest match keeps them apart.
fn spaced_list0<'a, O, P>(
    item: P,
) -> impl Parser<Span<'a>, Output = Vec<O>, Error = Error<Span<'a>>>
where
    P: Parser<Span<'a>, Output = O, Error = Error<Span<'a>>>,
{
    many0(preceded(space_comment0, item))
}

/// Matches one or more whitespace characters.  Following escape interpretation, an escaped
/// line feed counts as whitespace, but an escaped carriage return does not.
fn separator_chars(input: Span) -> IResult<Span, Span> {
    take_while1(|c| matches!(c, ' ' | '\t' | '\n' | ESCAPED_LF)).parse(input)
}

/// The remainder of a comment line; a comment is ended by either a newline or an escaped line
/// feed
fn rest_of_line(input: Span) -> IResult<Span, Span> {
    take_till(|c| c == '\n' || c == ESCAPED_LF).parse(input)
}

/// Matches a single-# comment but NOT ## documentation comments
fn comment(input: Span) -> IResult<Span, Span> {
    recognize((tag("#"), not(peek(tag("#"))), rest_of_line)).parse(input)
}

/// The content of one `##` line: the text after the run of #s, with one leading space stripped
/// if present.
fn documentation_line<'a>(input: Span<'a>) -> IResult<Span<'a>, &'a str> {
    // documentationLineContent skips any further #s, so `### x` documents "x"
    let (input, _) = (tag("##"), take_while(|c| c == '#')).parse(input)?;
    let (input, content) = rest_of_line(input)?;
    let content = *content.fragment();
    Ok((input, content.strip_prefix(' ').unwrap_or(content)))
}

/// One documentation comment: a `##` line, and any `##` lines which directly follow it, with
/// only indentation between.  The lines' content is joined with newlines.  A blank line or `#`
/// comment ends the documentation, so that what follows is separate documentation.
fn documentation(input: Span) -> IResult<Span, Documentation> {
    let (input, start) = position(input)?;
    let (input, first) = documentation_line(input)?;
    let continuation = preceded(
        (
            satisfy(|c| c == '\n' || c == ESCAPED_LF),
            take_while(|c| c == ' ' || c == '\t'),
        ),
        documentation_line,
    );
    let (input, content) = fold_many0(
        continuation,
        move || first.to_string(),
        |mut content, line| {
            content.push('\n');
            content.push_str(line);
            content
        },
    )
    .parse(input)?;
    let (input, end) = position(input)?;
    Ok((
        input,
        Documentation {
            span: span(start, end),
            content,
        },
    ))
}

/// Parse zero or more documentation comments (separated by whitespace/single-# comments)
fn documentations(input: Span) -> IResult<Span, Vec<Documentation>> {
    separated_list0(space_comment1, documentation).parse(input)
}

fn maybe_initial_annotation(input: Span) -> IResult<Span, Option<InitialAnnotation>> {
    opt(map((initial_annotation, space_comment0), |(anno, _)| anno)).parse(input)
}

/// Parse optional annotations: zero or more ## doc lines, then optionally [...]
fn maybe_annotations(input: Span) -> IResult<Span, Option<Annotations>> {
    let (input, docs) = documentations(input)?;
    let (input, _) = space_comment0(input)?;
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
        spaced_list0(annotation_attribute),
        space_comment0,
        spaced_list0(annotation_element),
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
        spaced_list0(nested_annotation_attribute),
        space_comment0,
        spaced_list0(annotation_element_or_literal),
        space_comment0,
        tag("]"),
        position,
    );

    let mut parser = map(
        parse,
        |(name, _, _, _, annotation_attributes, _, annotation_elements_or_literals, _, _, end)| {
            AnnotationElement {
                span: Range {
                    start: name.span().start,
                    end: original_offset(&end),
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

    fn ck<T, F>(f: F, input: &str, expected: T)
    where
        F: Fn(Span) -> IResult<Span, T>,
        T: PartialEq + fmt::Debug,
    {
        let (remaining, result) =
            f(Span::from(input)).unwrap_or_else(|_| panic!("failed to parse {input:#?}"));
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
        // adjacent lines are merged; a blank line or `#` comment separates documentation
        let input = "## line one\n  ##   line two\n\n## after blank\n# plain\n## after comment";
        let (remaining, result) = documentations(Span::from(input)).expect("failed to parse");
        assert_eq!(remaining.fragment(), &"");
        let content: Vec<_> = result.iter().map(|d| d.content.as_str()).collect();
        // as translated by trang
        assert_eq!(
            content,
            ["line one\n  line two", "after blank", "after comment"]
        );
        assert_eq!(result[0].span, 0..27);
    }

    #[test]
    fn test_maybe_annotations_docs_only() {
        let input = "## A doc comment\nelement";
        let (remaining, result) = maybe_annotations(Span::from(input)).expect("failed to parse");
        assert_eq!(remaining.fragment(), &"element");
        let annos = result.unwrap();
        assert_eq!(annos.documentation.len(), 1);
        assert_eq!(annos.documentation[0].content, "A doc comment");
        assert!(annos.initial.is_none());
    }

    #[test]
    fn test_maybe_annotations_bracket_only() {
        let input = "[ xml:lang=\"en\" ] element";
        let (remaining, result) = maybe_annotations(Span::from(input)).expect("failed to parse");
        assert_eq!(remaining.fragment(), &"element");
        let annos = result.unwrap();
        assert!(annos.documentation.is_empty());
        assert!(annos.initial.is_some());
        assert_eq!(annos.initial.unwrap().attribute_annotations.len(), 1);
    }

    #[test]
    fn test_maybe_annotations_docs_and_bracket() {
        let input = "## Some docs\n[ xml:lang=\"en\" ] element";
        let (remaining, result) = maybe_annotations(Span::from(input)).expect("failed to parse");
        assert_eq!(remaining.fragment(), &"element");
        let annos = result.unwrap();
        assert_eq!(annos.documentation.len(), 1);
        assert_eq!(annos.documentation[0].content, "Some docs");
        assert!(annos.initial.is_some());
    }

    #[test]
    fn test_pattern_with_doc_annotation() {
        let input = "## A greeting\nelement greeting { text }";
        let (remaining, result) = pattern(Span::from(input)).expect("failed to parse");
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
        let (remaining, result) = pattern(Span::from(input)).expect("failed to parse");
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
        let (remaining, result) = grammar_content(Span::from(input)).expect("failed to parse");
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
        let (remaining, result) = pattern(Span::from(input)).expect("failed to parse");
        assert_eq!(remaining.fragment(), &"");
        // No annotation wrapping since # comment is consumed as whitespace
        assert!(matches!(result, Pattern::Element(_)));
    }

    #[test]
    fn test_annotation_attribute_requires_cname() {
        // Unprefixed attribute names should fail
        let result = annotation_attribute(Span::from("lang=\"en\""));
        assert!(result.is_err());
    }

    #[test]
    fn deeply_nested_parens_pattern() {
        let depth = 200;
        let input = "(".repeat(depth) + "empty" + &")".repeat(depth);
        let result = schema(Span::from(input.as_str()));
        assert!(result.is_err());
    }

    #[test]
    fn deeply_nested_parens_name_class() {
        let depth = 200;
        let parens = "(".repeat(depth) + "foo" + &")".repeat(depth);
        let input = format!("start = element {parens} {{ text }}");
        let result = schema(Span::from(input.as_str()));
        assert!(result.is_err());
    }

    #[test]
    fn long_choice_chain() {
        // Regression test: long | chains should not hit the recursion limit
        // because binary operators are parsed iteratively, not recursively.
        let names: Vec<String> = (0..50).map(|i| format!("x{i}")).collect();
        let input = format!("start = {}", names.join(" | "));
        let result = schema(Span::from(input.as_str()));
        assert!(result.is_ok(), "50-way choice should parse: {result:?}");
    }

    #[test]
    fn deeply_nested_div_grammar() {
        let depth = 200;
        let input = "div { ".repeat(depth) + "a = empty" + &" }".repeat(depth);
        let result = schema(Span::from(input.as_str()));
        assert!(result.is_err());
    }

    #[test]
    fn deeply_nested_div_include() {
        let depth = 200;
        let divs = "div { ".repeat(depth) + "a = empty" + &" }".repeat(depth);
        let input = format!("include \"foo.rnc\" {{ {divs} }}");
        let result = schema(Span::from(input.as_str()));
        assert!(result.is_err());
    }

    #[test]
    fn deeply_nested_annotation_elements() {
        let depth = 200;
        let input = "a:x [ ".repeat(depth) + "\"leaf\"" + &" ]".repeat(depth);
        // Try parsing as an annotation element directly
        let result = annotation_element(Span::from(input.as_str()));
        assert!(result.is_err());
    }

    #[test]
    fn moderate_nesting_succeeds() {
        // A moderate depth should still parse fine
        let depth = 20;
        let input = "(".repeat(depth) + "empty" + &")".repeat(depth);
        let result = schema(Span::from(input.as_str()));
        assert!(result.is_ok());
    }

    fn decoded(src: &str) -> String {
        preprocess(src).unwrap().0.into_owned()
    }

    fn input_error(src: &str) -> InputError {
        match parse(src) {
            Err(CompactError::Input(e)) => e,
            other => panic!("expected InputError for {src:?}, got {other:?}"),
        }
    }

    fn only_define(schema: Schema) -> Define {
        match schema.pattern_or_grammar {
            PatternOrGrammar::Grammar(mut g) if g.content.len() == 1 => match g.content.remove(0) {
                GrammarContent::Define(d) => d,
                other => panic!("expected Define, got {other:?}"),
            },
            other => panic!("expected single-item grammar, got {other:?}"),
        }
    }

    #[test]
    fn escape_basic() {
        assert_eq!(decoded(r"\x{66}\x{6f}\x{6f}"), "foo");
    }

    #[test]
    fn escape_multiple_x() {
        assert_eq!(decoded(r"\xx{41}"), "A");
        assert_eq!(decoded(r"\xxx{41}"), "A");
    }

    #[test]
    fn escape_mixed_with_text() {
        assert_eq!(decoded(r"hello \x{20} world"), "hello   world");
    }

    #[test]
    fn preprocess_borrows_when_unchanged() {
        let (text, map) = preprocess("plain text\n").unwrap();
        assert!(matches!(text, Cow::Borrowed(_)));
        assert!(map.is_empty());
    }

    #[test]
    fn escape_backslash_not_followed_by_x() {
        // \keyword should pass through unchanged
        assert_eq!(decoded(r"\keyword"), r"\keyword");
        assert_eq!(decoded(r"\x41"), r"\x41");
    }

    #[test]
    fn escape_is_not_rescanned() {
        // \x{5C} is '\'; the result must not be treated as the start of another escape
        assert_eq!(decoded(r"\x{5C}x{41}"), r"\x{41}");
    }

    #[test]
    fn escape_non_bmp() {
        assert_eq!(decoded(r"\x{10000}"), "\u{10000}");
    }

    #[test]
    fn newline_normalisation() {
        assert_eq!(decoded("a\r\nb\rc\nd"), "a\nb\nc\nd");
    }

    #[test]
    fn escaped_newlines_are_distinguished() {
        assert_eq!(decoded(r"a\x{A}b"), format!("a{ESCAPED_LF}b"));
        // escaped CR is just a character, not a newline
        assert_eq!(decoded(r"a\x{D}b"), "a\rb");
    }

    #[test]
    fn escape_errors() {
        assert_eq!(
            input_error(r"start = \x{0}"),
            InputError::InvalidEscapedChar {
                span: 8..13,
                code: 0
            }
        );
        assert_eq!(
            input_error(r"start = \x{D800}"),
            InputError::InvalidEscapedChar {
                span: 8..16,
                code: 0xD800
            }
        );
        assert_eq!(
            input_error(r"start = \x{FFFF}"),
            InputError::InvalidEscapedChar {
                span: 8..16,
                code: 0xFFFF
            }
        );
        assert_eq!(
            input_error(r"\x{110000}"),
            InputError::MalformedEscape {
                span: 0..10,
                problem: EscapeProblem::OutOfRange
            }
        );
        assert_eq!(
            input_error(r"\x{FFFFFFFFFF}"),
            InputError::MalformedEscape {
                span: 0..14,
                problem: EscapeProblem::OutOfRange
            }
        );
        assert_eq!(
            input_error(r"\x{41"),
            InputError::MalformedEscape {
                span: 0..5,
                problem: EscapeProblem::Unterminated
            }
        );
        assert_eq!(
            input_error(r"\x{}"),
            InputError::MalformedEscape {
                span: 0..4,
                problem: EscapeProblem::Empty
            }
        );
        assert_eq!(
            input_error(r"\x{4G}"),
            InputError::MalformedEscape {
                span: 0..5,
                problem: EscapeProblem::NonHexDigit('G')
            }
        );
    }

    #[test]
    fn invalid_raw_chars() {
        for (src, start, ch) in [
            ("start = \"a\u{1}\"", 10, '\u{1}'),
            ("# \u{B}\nstart = empty", 2, '\u{B}'),
            ("start = \"\u{FFFE}\"", 9, '\u{FFFE}'),
            ("start = \"\u{FFFF}\"", 9, '\u{FFFF}'),
        ] {
            assert_eq!(
                input_error(src),
                InputError::InvalidChar {
                    span: start..start + ch.len_utf8(),
                    ch
                }
            );
        }
        // tab, LF, CR, and characters merely discouraged by XML are fine
        assert!(parse("start =\t\"\u{7F}\u{85}\u{FDD0}\u{1FFFF}\"\r\n").is_ok());
    }

    #[test]
    fn input_error_messages() {
        assert_eq!(
            input_error("start = \"\u{1}\"").to_string(),
            "character U+0001 is not allowed in a schema (not a legal XML character)"
        );
        assert_eq!(
            input_error(r"start = \x{1}").to_string(),
            "escape sequence denotes U+0001, which is not a legal XML character"
        );
        assert_eq!(
            input_error(r"start = \x{4G}").to_string(),
            "invalid character 'G' in escape sequence; expected a hex digit or '}'"
        );
    }

    #[test]
    fn escape_in_keyword() {
        // issue #32: escape processing happens before tokenisation, so this is the keyword
        // 'element'
        match parse(r"\x{65}lement foo { empty }")
            .unwrap()
            .pattern_or_grammar
        {
            PatternOrGrammar::Pattern(Pattern::Element(e)) => assert_eq!(e.span, 0..26),
            other => panic!("expected element pattern, got {other:?}"),
        }
    }

    #[test]
    fn escaped_keyword_is_still_a_keyword() {
        // issue #32: a keyword may not be used where an identifier is expected, even if escaped
        assert!(parse(r"start = \x{65}lement").is_err());
        assert!(parse(r"\x{65}lement = empty").is_err());
    }

    #[test]
    fn escaped_backslash_quotes_identifier() {
        // \x{5C} is '\', so this is the quoted identifier \element
        let define = only_define(parse(r"\x{5C}element = empty").unwrap());
        assert_eq!(define.identifier.0, 0..13);
    }

    #[test]
    fn escaped_newline_separates_tokens() {
        assert!(parse(r"element\x{A}foo { empty }").is_ok());
    }

    #[test]
    fn escaped_cr_is_not_a_separator() {
        assert!(parse(r"element\x{D}foo { empty }").is_err());
    }

    #[test]
    fn escaped_newline_ends_comment() {
        assert!(parse(r"# comment\x{A}start = element foo { empty }").is_ok());
    }

    #[test]
    fn escaped_newline_ends_documentation_line() {
        let define = only_define(parse(r"## doc\x{A}start = element foo { empty }").unwrap());
        let docs = define.annotations.unwrap().documentation;
        assert_eq!(docs.len(), 1);
        assert_eq!(docs[0].content, "doc");
    }

    #[test]
    fn comment_at_end_of_input() {
        assert!(parse("start = empty # no trailing newline").is_ok());
    }

    #[test]
    fn escaped_quote_ends_literal() {
        // \x{22} is '"', which terminates the literal like any other '"' -- so this becomes
        // the unterminated triple-quoted literal """ }
        assert!(parse(r#"element foo { "\x{22}" }"#).is_err());
        // ...whereas this is the one-character literal "'"
        assert!(parse(r#"element foo { "\x{27}" }"#).is_ok());
    }

    fn literal_body(src: &str) -> String {
        match parse(&format!("start = {src}")).map(only_define) {
            Ok(Define {
                pattern: Pattern::DatatypeValue(DatatypeValuePattern(_, _, lit, _, _)),
                ..
            }) => lit.as_string_value(),
            other => panic!("expected literal pattern for {src:?}, got {other:?}"),
        }
    }

    #[test]
    fn escape_newline_in_string_literal() {
        assert_eq!(literal_body(r#""hello\x{a}world""#), "hello\nworld");
        assert_eq!(literal_body(r#"'hello\x{a}world'"#), "hello\nworld");
    }

    #[test]
    fn real_newline_in_string_literal_rejected() {
        assert!(parse("start = \"hello\nworld\"").is_err());
        assert!(parse("start = \"hello\rworld\"").is_err());
        assert!(parse("start = \"hello\r\nworld\"").is_err());
    }

    #[test]
    fn newlines_in_triple_quoted_string() {
        assert_eq!(literal_body(r#""""a\x{a}b""""#), "a\nb");
        assert_eq!(literal_body("'''a\r\nb\rc'''"), "a\nb\nc");
    }

    #[test]
    fn escaped_cr_in_string_literal() {
        // unlike a raw CR, an escaped CR is not a newline
        assert_eq!(literal_body(r#""\x{d}""#), "\r");
    }

    #[test]
    fn spans_refer_to_original_source() {
        let define = only_define(parse("\\x{66}oo = a |\r\n \\x{62}").unwrap());
        assert_eq!(define.identifier.0, 0..8);
        assert_eq!(define.span, 0..23);
        match define.pattern {
            Pattern::ChoicePair(a, b) => {
                assert_eq!(*a, Pattern::Identifier(Identifier(11..12, "a".to_string())));
                assert_eq!(*b, Pattern::Identifier(Identifier(17..23, "b".to_string())));
            }
            other => panic!("expected choice, got {other:?}"),
        }
    }

    #[test]
    fn syntax_error_span_refers_to_original_source() {
        // the error is reported at the '!' following the escapes
        match parse(r"start = \x{61}\x{62} !") {
            Err(CompactError::Syntax { span, .. }) => assert_eq!(span, 21..22),
            other => panic!("expected syntax error, got {other:?}"),
        }
    }

    #[test]
    fn nc_name_does_not_interpret_escapes() {
        // nc_name() also validates xsd:NCName values, where escapes have no special meaning
        let (rest, name) = nc_name(Span::from(r"a\x{62}")).unwrap();
        assert_eq!(name.1, "a");
        assert_eq!(*rest.fragment(), r"\x{62}");
    }

    #[test]
    fn invalid_escape_in_string_literal_reports_the_escape() {
        let src = r#"element foo { attribute bar { "a\x{20}b\x{0}c" } }"#;
        let err = input_error(src);
        assert_eq!(&src[err.span()], r"\x{0}");
        assert_eq!(err.span().start, 39);
        assert_eq!(
            err.to_string(),
            "escape sequence denotes U+0000, which is not a legal XML character"
        );
    }

    #[test]
    fn invalid_escape_in_name_reports_the_escape() {
        let src = r"element f\x{zz} { empty }";
        let err = input_error(src);
        assert_eq!(&src[err.span()], r"\x{z");
        assert_eq!(err.span().start, 9);
        assert_eq!(
            err.to_string(),
            "invalid character 'z' in escape sequence; expected a hex digit or '}'"
        );
    }

    fn start_pattern(src: &str) -> Pattern {
        parse(src)
            .map(only_define)
            .unwrap_or_else(|e| panic!("failed to parse {src:?}: {e:?}"))
            .pattern
    }

    fn grammar_items(src: &str) -> Vec<GrammarContent> {
        match parse(src) {
            Ok(Schema {
                pattern_or_grammar: PatternOrGrammar::Grammar(g),
                ..
            }) => g.content,
            other => panic!("expected grammar for {src:?}, got {other:?}"),
        }
    }

    fn define_docs(item: &GrammarContent) -> Vec<&str> {
        match item {
            GrammarContent::Define(Define {
                annotations: Some(a),
                ..
            }) => a.documentation.iter().map(|d| d.content.as_str()).collect(),
            other => panic!("expected annotated define, got {other:?}"),
        }
    }

    fn pattern_docs(p: &Pattern) -> Vec<&str> {
        match p {
            Pattern::Annotated(a, _) => {
                a.documentation.iter().map(|d| d.content.as_str()).collect()
            }
            other => panic!("expected annotated pattern, got {other:?}"),
        }
    }

    #[test]
    fn doc_comments_reach_definitions() {
        let items = grammar_items("## first\nstart = a\n\n## second\na = empty");
        assert_eq!(define_docs(&items[0]), ["first"]);
        assert_eq!(define_docs(&items[1]), ["second"]);

        let items = grammar_items("namespace x = \"urn:x\"\n## doc\nstart = empty");
        assert_eq!(define_docs(&items[0]), ["doc"]);
    }

    #[test]
    fn doc_comments_reach_patterns() {
        let Pattern::Element(el) = start_pattern("start = element a {\n  ## inner\n  empty\n}")
        else {
            panic!("expected element");
        };
        assert_eq!(pattern_docs(&el.pattern), ["inner"]);

        let Pattern::ChoicePair(_, b) = start_pattern("start = a |\n  ## second\n  b") else {
            panic!("expected choice");
        };
        assert_eq!(pattern_docs(&b), ["second"]);
    }

    #[test]
    fn extra_hashes_start_documentation() {
        // as translated by trang: further #s are skipped, then one space
        let items = grammar_items("### triple\nstart = a\n##########\na = empty");
        assert_eq!(define_docs(&items[0]), ["triple"]);
        assert_eq!(define_docs(&items[1]), [""]);
    }

    #[test]
    fn misplaced_doc_comments_rejected() {
        // `##` lines are documentation, which can only precede a pattern, name class, grammar
        // content or include content.  Jing rejects all of these.
        for src in [
            "start = element a { empty }\n## at end of file",
            "start = element a { empty }\n##########",
            "start = element a {\n  empty\n  ## before brace\n}",
            "## before a declaration\nnamespace x = \"urn:x\"\nstart = empty",
        ] {
            assert!(
                matches!(parse(src), Err(CompactError::Syntax { .. })),
                "expected syntax error for {src:?}"
            );
        }
    }

    #[test]
    fn whitespace_between_annotation_items_is_optional() {
        // all accepted by Jing
        for src in [
            "namespace x = \"urn:x\"\nstart = [x:a=\"1\"x:b=\"2\"] element a { empty }",
            "namespace x = \"urn:x\"\nstart = [x:e[]x:f[x:g=\"1\"x:h=\"2\"]] element a { empty }",
            "namespace x = \"urn:x\"\nstart = [x:f[\"t\"x:g[]\"u\"]] element a { empty }",
            "namespace x = \"urn:x\"\nstart = [x:a=\"1\"x:e[]] element a { empty }",
        ] {
            let result = parse(src);
            assert!(result.is_ok(), "{src:?}: {result:?}");
        }
    }

    /// The pattern within `element a { … }`, for `body`
    fn element_body(body: &str) -> Pattern {
        let src = format!("namespace x = \"urn:x\"\nstart = element a {{ {body} }}");
        match parse(&src).map(only_define) {
            Ok(Define {
                pattern: Pattern::Element(el),
                ..
            }) => *el.pattern,
            other => panic!("expected element for {body:?}, got {other:?}"),
        }
    }

    /// Summarise the leading and follow annotations on an `Annotated` pattern, as
    /// (has leading [...], number of follow annotations, inner pattern)
    fn annos(p: Pattern) -> (bool, usize, Pattern) {
        match p {
            Pattern::Annotated(a, inner) => (a.initial.is_some(), a.follow_elements.len(), *inner),
            other => panic!("expected annotated pattern, got {other:?}"),
        }
    }

    fn except_of(p: &Pattern) -> &Pattern {
        match p {
            Pattern::DatatypeName(DatatypeNamePattern(_, _, _, Some(except))) => except,
            other => panic!("expected datatype with except, got {other:?}"),
        }
    }

    #[test]
    fn data_except_annotations() {
        // as translated by trang: the follow annotation follows <data>, not the <value>
        let (lead, follow, inner) = annos(element_body("xsd:string - \"a\" >> x:y[]"));
        assert_eq!((lead, follow), (false, 1));
        assert!(matches!(except_of(&inner), Pattern::DatatypeValue(_)));

        // leading annotations apply to <data>
        let (lead, follow, inner) = annos(element_body("[x:a=\"1\"] xsd:string - \"a\""));
        assert_eq!((lead, follow), (true, 0));
        assert!(matches!(except_of(&inner), Pattern::DatatypeValue(_)));

        // and on the except pattern, to <value>
        let p = element_body("xsd:string - [x:a=\"1\"] \"a\"");
        assert!(matches!(except_of(&p), Pattern::Annotated(a, _) if a.initial.is_some()));

        let p = element_body("xsd:string - (\"a\" | \"b\")");
        assert!(matches!(except_of(&p), Pattern::Group(_)));
    }

    #[test]
    fn data_except_needs_parentheses() {
        // Jing reports `parentheses required around "-" expression` for each
        for body in [
            "xsd:string - \"a\" | \"b\"",
            "xsd:string - \"a\" >> x:y[] | \"b\"",
            "xsd:string - \"a\", text",
            "text & xsd:string - \"a\"",
            "xsd:string - \"a\"*",
        ] {
            let src = format!("namespace x = \"urn:x\"\nstart = element a {{ {body} }}");
            match parse(&src) {
                Err(CompactError::ExceptNeedsParentheses { span }) => {
                    assert!(matches!(&src[span], "|" | "," | "&" | "*"), "{body:?}")
                }
                other => panic!("expected ExceptNeedsParentheses for {body:?}, got {other:?}"),
            }
        }
        assert!(parse("start = element a { (xsd:string - \"a\") | \"b\" }").is_ok());
    }

    /// The name class of `element <nc> { empty }`
    fn element_name_class(nc: &str) -> NameClass {
        let src = format!("namespace x = \"urn:x\"\nstart = element {nc} {{ empty }}");
        match parse(&src).map(only_define) {
            Ok(Define {
                pattern: Pattern::Element(el),
                ..
            }) => el.name_class,
            other => panic!("expected element for {nc:?}, got {other:?}"),
        }
    }

    #[test]
    fn name_class_except_annotations() {
        // as translated by trang: the follow annotation follows <anyName>
        let NameClass::Annotated(a, inner) = element_name_class("* - a >> x:y[]") else {
            panic!("expected annotated name class");
        };
        assert_eq!(a.follow_elements.len(), 1);
        assert!(matches!(*inner, NameClass::AnyName(AnyName(Some(_)))));

        // leading annotations apply to <nsName>
        let NameClass::Annotated(a, inner) = element_name_class("[x:z=\"1\"] x:* - x:a") else {
            panic!("expected annotated name class");
        };
        assert!(a.initial.is_some());
        assert!(matches!(
            *inner,
            NameClass::NsName(NsName {
                except: Some(_),
                ..
            })
        ));
    }

    #[test]
    fn name_class_except_needs_parentheses() {
        for nc in ["* - a | b", "x:* - x:a | b", "a | * - b"] {
            let src = format!("namespace x = \"urn:x\"\nstart = element {nc} {{ empty }}");
            match parse(&src) {
                Err(CompactError::ExceptNeedsParentheses { span }) => assert_eq!(&src[span], "|"),
                other => panic!("expected ExceptNeedsParentheses for {nc:?}, got {other:?}"),
            }
        }
        // an except within an except, rejected by Jing as a syntax error
        assert!(matches!(
            parse("namespace x = \"urn:x\"\nstart = element * - x:* - x:a { empty }"),
            Err(CompactError::Syntax { .. })
        ));
        for nc in ["(* - a) | b", "* - (a | b)"] {
            assert!(matches!(
                element_name_class(nc),
                NameClass::Alt(_) | NameClass::AnyName(_)
            ));
        }
    }

    #[test]
    fn div_annotations_kept() {
        let src = "namespace x = \"urn:x\"\n## grammar div\n[x:a=\"1\"] div { start = empty }\n\
                   include \"inc.rnc\" {\n  ## include div\n  div { b = text }\n}";
        let items = grammar_items(src);
        let GrammarContent::Div(Div {
            annotations: Some(a),
            content,
        }) = &items[0]
        else {
            panic!("expected annotated div, got {:?}", items[0]);
        };
        assert_eq!(a.documentation[0].content, "grammar div");
        assert!(a.initial.is_some());
        assert_eq!(content.len(), 1);

        let GrammarContent::Include(Include {
            content: Some(inc), ..
        }) = &items[1]
        else {
            panic!("expected include, got {:?}", items[1]);
        };
        let IncludeContent::Div(Div {
            annotations: Some(a),
            ..
        }) = &inc[0]
        else {
            panic!("expected annotated div, got {:?}", inc[0]);
        };
        assert_eq!(a.documentation[0].content, "include div");
    }

    #[test]
    fn grammar_level_annotation_elements() {
        let p = "namespace x = \"urn:x\"\nstart = empty\n";
        let items = grammar_items(&format!("{p}x:foo [ ]\nfoo [ ]"));
        assert!(matches!(items[1], GrammarContent::Annotation(_)));
        assert!(matches!(items[2], GrammarContent::Annotation(_)));
        // Jing rejects annotations on an annotation element, and an unprefixed keyword as its
        // name
        for rest in ["## doc\nx:foo [ ]", "[x:a=\"1\"] x:foo [ ]", "div [ ]"] {
            let src = format!("{p}{rest}");
            assert!(parse(&src).is_err(), "expected error for {rest:?}");
        }
    }

    #[test]
    fn top_level_follow_annotation_rejected() {
        // the spec: "It is an error if a top-level pattern translates to a sequence of more than
        // one element"
        let src = "namespace x = \"urn:x\"\nelement a { empty } >> x:y [ ]";
        match parse(src) {
            Err(CompactError::TopLevelFollowAnnotation { span }) => {
                assert_eq!(&src[span], "x:y [ ]")
            }
            other => panic!("expected TopLevelFollowAnnotation, got {other:?}"),
        }
        for top in [
            "(element a { empty } | empty) >> x:y [ ]",
            "element a { empty }* >> x:y [ ]",
            "[ x:a = \"1\" ] element a { empty } >> x:y [ ]",
            "## doc\nelement a { empty } >> x:y [ ]",
            "xsd:string - \"a\" >> x:y [ ]",
            // a <value> can't have element children, so the annotation would follow it
            "[ x:e [ ] ] \"a\"",
        ] {
            let src = format!("namespace x = \"urn:x\"\n{top}");
            assert!(
                matches!(
                    parse(&src),
                    Err(CompactError::TopLevelFollowAnnotation { .. })
                ),
                "{top:?}"
            );
        }
        // annotations that stay within the top-level element.  Jing rejects the first four,
        // although they translate to a single element.
        for top in [
            "element a { empty } | empty >> x:y [ ]",
            "element a { empty } >> x:y [ ] | empty",
            "(element a { empty } | empty >> x:y [ ])",
            "element a { empty } >> x:y [ ] *",
            "[ x:a = \"1\" ] \"a\"",
            "[ x:e [ ] ] element a { empty }",
            "element a { empty >> x:y [ ] }",
            "element a >> x:y [ ] { empty }",
            "grammar { start = element a { empty } >> x:y [ ] }",
        ] {
            let src = format!("namespace x = \"urn:x\"\n{top}");
            assert!(parse(&src).is_ok(), "{top:?}");
        }
    }
}
