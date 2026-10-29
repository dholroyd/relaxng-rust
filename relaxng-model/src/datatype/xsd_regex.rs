//! Support for the regular expression language of XML Schema Part 2 (Datatypes), as used by the
//! `pattern` facet.
//!
//! The XSD dialect differs from the syntax of Rust's `regex` crate in ways that can't be
//! papered over by rewriting the pattern as text (`.` excludes `\r`, `^` and `$` are ordinary
//! characters, `\w` and `\s` have their own definitions, there is subtraction between character
//! classes, Unicode blocks are addressed by name, and so on).  So instead the pattern is parsed
//! according to the XSD grammar (XSD 1.0 2nd ed, Appendix F), and the result is built directly as
//! a [`regex_syntax::hir::Hir`], which is then compiled by the same engine that backs
//! [`regex::Regex`].

use regex_automata::meta;
use regex_syntax::hir::{Class, ClassUnicode, ClassUnicodeRange, Hir, HirKind, Look, Repetition};
use std::fmt;
use std::ops::Range;

/// Limit on the size of the compiled program, in bytes.  Larger than the engine's default, since
/// legitimate XSD patterns commonly use bounded repetition of non-trivial sub-expressions.
const NFA_SIZE_LIMIT: usize = 64 * 1024 * 1024;

/// Limit on the nesting depth of groups and character classes, so that hostile input can't
/// overflow the stack of the recursive-descent parser.
const MAX_NESTING: u32 = 100;

/// A compiled XSD regular expression.
///
/// Matching is always against the whole of the input; there is no concept of finding a match
/// within a longer string.
#[derive(Clone, Debug)]
pub struct XsdRegex(meta::Regex);

impl XsdRegex {
    /// Parse and compile the given XSD pattern.
    pub fn new(pattern: &str) -> Result<XsdRegex, XsdRegexError> {
        let hir = Parser::new(pattern).parse()?;
        let hir = Hir::concat(vec![Hir::look(Look::Start), hir, Hir::look(Look::End)]);
        meta::Builder::new()
            .configure(meta::Config::new().nfa_size_limit(Some(NFA_SIZE_LIMIT)))
            .build_from_hir(&hir)
            .map(XsdRegex)
            .map_err(|e| XsdRegexError {
                kind: if e.size_limit().is_some() {
                    XsdRegexErrorKind::TooLarge
                } else {
                    XsdRegexErrorKind::Internal(e.to_string())
                },
                span: None,
            })
    }

    /// Returns true if the whole of `value` is matched by this pattern.
    pub fn is_match(&self, value: &str) -> bool {
        self.0.is_match(value)
    }
}

/// A problem found when compiling an XSD regular expression.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct XsdRegexError {
    kind: XsdRegexErrorKind,
    span: Option<Range<usize>>,
}

impl XsdRegexError {
    pub fn kind(&self) -> &XsdRegexErrorKind {
        &self.kind
    }

    /// The byte range within the pattern text at which the problem was found, or `None` if the
    /// problem relates to the pattern as a whole.
    pub fn span(&self) -> Option<Range<usize>> {
        self.span.clone()
    }
}

impl fmt::Display for XsdRegexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.kind, f)
    }
}

impl std::error::Error for XsdRegexError {}

#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum XsdRegexErrorKind {
    /// A `\` at the very end of the pattern
    TrailingBackslash,
    /// `\` followed by a character that doesn't form a valid escape
    InvalidEscape(char),
    /// A `(` with no matching `)`
    UnclosedGroup,
    /// A `)` with no matching `(`
    UnmatchedCloseParen,
    /// A `[` with no matching `]`
    UnclosedCharClass,
    /// A character class with no members, `[]`
    EmptyCharClass,
    /// A character that has a special meaning in the syntax, and which must be escaped to be
    /// used literally, was found where it is not valid
    UnescapedMetaChar(char),
    /// `?`, `*` or `+` with nothing before it to repeat
    QuantifierWithoutAtom(char),
    /// Malformed `{n}`, `{n,}` or `{n,m}`
    InvalidQuantifier,
    /// The number in a `{n,m}` quantifier is too big
    QuantifierTooLarge,
    /// `{n,m}` where `n` is larger than `m`
    QuantifierRangeReversed,
    /// A `-` in a character class that isn't a literal at the start or end, part of a character
    /// range, or the start of a subtraction
    InvalidHyphen,
    /// A character range like `[z-a]`
    ReversedCharRange { start: char, end: char },
    /// `\p{` or `\P{` without the expected property name and closing `}`
    MalformedProperty,
    /// The category name in `\p{…}` is not one defined by XSD
    UnknownCategory(String),
    /// The block name in `\p{Is…}` is not one defined by XSD
    UnknownBlock(String),
    /// Groups or character classes are nested too deeply
    NestingTooDeep,
    /// The pattern would need an unreasonably large amount of memory to match
    TooLarge,
    /// Unexpected failure while compiling an apparently-valid pattern
    Internal(String),
}

impl fmt::Display for XsdRegexErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use XsdRegexErrorKind::*;
        match self {
            TrailingBackslash => f.write_str("pattern ends with an unfinished '\\' escape"),
            InvalidEscape(c) => write!(f, "'\\{c}' is not a valid escape sequence"),
            UnclosedGroup => f.write_str("unclosed '(' group"),
            UnmatchedCloseParen => f.write_str("')' without a matching '('"),
            UnclosedCharClass => f.write_str("unclosed '[' character class"),
            EmptyCharClass => f.write_str("character class has no members"),
            UnescapedMetaChar(c) => write!(f, "'{c}' must be escaped as '\\{c}' here"),
            QuantifierWithoutAtom(c) => write!(f, "'{c}' has nothing before it to repeat"),
            InvalidQuantifier => f.write_str("malformed '{n,m}' quantifier"),
            QuantifierTooLarge => f.write_str("repetition count is too large"),
            QuantifierRangeReversed => {
                f.write_str("minimum repetition count is greater than the maximum")
            }
            InvalidHyphen => f.write_str(
                "'-' must be escaped as '\\-' unless it is at the start or end of a \
                 character class, or part of a range of single characters",
            ),
            ReversedCharRange { start, end } => {
                write!(f, "character range '{start}-{end}' ends before it starts")
            }
            MalformedProperty => f.write_str("expected a property name in '{…}' after '\\p'"),
            UnknownCategory(name) => write!(f, "unknown Unicode category {name:?}"),
            UnknownBlock(name) => write!(f, "unknown Unicode block {name:?}"),
            NestingTooDeep => f.write_str("pattern is nested too deeply"),
            TooLarge => f.write_str("pattern is too large or complex"),
            Internal(msg) => write!(f, "unable to compile pattern: {msg}"),
        }
    }
}

/// The meaning of a single escape sequence, or of a single plain character, within a pattern
enum Esc {
    Char(char),
    Class(ClassUnicode),
}

struct Parser<'a> {
    src: &'a str,
    pos: usize,
    depth: u32,
}

impl<'a> Parser<'a> {
    fn new(src: &'a str) -> Parser<'a> {
        Parser {
            src,
            pos: 0,
            depth: 0,
        }
    }

    fn parse(mut self) -> Result<Hir, XsdRegexError> {
        let hir = self.reg_exp()?;
        if self.pos < self.src.len() {
            // reg_exp() only stops early at a ')' that it didn't open
            return Err(self.err_here(XsdRegexErrorKind::UnmatchedCloseParen));
        }
        Ok(hir)
    }

    fn peek(&self) -> Option<char> {
        self.src[self.pos..].chars().next()
    }

    /// The character after the next one
    fn peek2(&self) -> Option<char> {
        self.src[self.pos..].chars().nth(1)
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    fn err(&self, kind: XsdRegexErrorKind, span: Range<usize>) -> XsdRegexError {
        XsdRegexError {
            kind,
            span: Some(span),
        }
    }

    /// An error spanning the character at the current position
    fn err_here(&self, kind: XsdRegexErrorKind) -> XsdRegexError {
        let end = self.pos + self.peek().map_or(0, char::len_utf8);
        self.err(kind, self.pos..end)
    }

    fn enter(&mut self) -> Result<(), XsdRegexError> {
        self.depth += 1;
        if self.depth > MAX_NESTING {
            return Err(self.err_here(XsdRegexErrorKind::NestingTooDeep));
        }
        Ok(())
    }

    fn leave(&mut self) {
        self.depth -= 1;
    }

    // regExp ::= branch ( '|' branch )*
    fn reg_exp(&mut self) -> Result<Hir, XsdRegexError> {
        self.enter()?;
        let mut alternatives = vec![self.branch()?];
        while self.peek() == Some('|') {
            self.bump();
            alternatives.push(self.branch()?);
        }
        self.leave();
        Ok(Hir::alternation(alternatives))
    }

    // branch ::= piece*
    fn branch(&mut self) -> Result<Hir, XsdRegexError> {
        let mut pieces = vec![];
        while let Some(c) = self.peek() {
            if c == '|' || c == ')' {
                break;
            }
            pieces.push(self.piece()?);
        }
        Ok(Hir::concat(pieces))
    }

    // piece ::= atom quantifier?
    fn piece(&mut self) -> Result<Hir, XsdRegexError> {
        let atom = self.atom()?;
        let (min, max) = match self.peek() {
            Some('?') => {
                self.bump();
                (0, Some(1))
            }
            Some('*') => {
                self.bump();
                (0, None)
            }
            Some('+') => {
                self.bump();
                (1, None)
            }
            Some('{') => self.quantity()?,
            _ => return Ok(atom),
        };
        Ok(Hir::repetition(Repetition {
            min,
            max,
            greedy: true,
            sub: Box::new(atom),
        }))
    }

    // quantity ::= QuantExact | QuantExact ',' | QuantExact ',' QuantExact
    fn quantity(&mut self) -> Result<(u32, Option<u32>), XsdRegexError> {
        let start = self.pos;
        self.bump(); // '{'
        let min = self
            .digits()?
            .ok_or_else(|| self.err_here(XsdRegexErrorKind::InvalidQuantifier))?;
        let max = if self.peek() == Some(',') {
            self.bump();
            self.digits()?
        } else {
            Some(min)
        };
        if self.peek() != Some('}') {
            return Err(self.err(XsdRegexErrorKind::InvalidQuantifier, start..self.pos));
        }
        self.bump();
        if max.is_some_and(|max| max < min) {
            return Err(self.err(XsdRegexErrorKind::QuantifierRangeReversed, start..self.pos));
        }
        Ok((min, max))
    }

    fn digits(&mut self) -> Result<Option<u32>, XsdRegexError> {
        let start = self.pos;
        while self.peek().is_some_and(|c| c.is_ascii_digit()) {
            self.bump();
        }
        if start == self.pos {
            return Ok(None);
        }
        self.src[start..self.pos]
            .parse()
            .map(Some)
            .map_err(|_| self.err(XsdRegexErrorKind::QuantifierTooLarge, start..self.pos))
    }

    // atom ::= Char | charClass | ( '(' regExp ')' )
    fn atom(&mut self) -> Result<Hir, XsdRegexError> {
        let start = self.pos;
        let c = self
            .bump()
            .expect("atom() is only called when input remains");
        match c {
            '(' => {
                let inner = self.reg_exp()?;
                if self.peek() != Some(')') {
                    return Err(self.err(XsdRegexErrorKind::UnclosedGroup, start..start + 1));
                }
                self.bump();
                Ok(inner)
            }
            '[' => Ok(class_hir(self.char_class(start)?)),
            '\\' => Ok(match self.escape()? {
                Esc::Char(c) => literal_hir(c),
                Esc::Class(class) => class_hir(class),
            }),
            '.' => Ok(class_hir(ranges_class(&[
                (0x0, 0x9),
                (0xB, 0xC),
                (0xE, 0x10FFFF),
            ]))),
            '?' | '*' | '+' => {
                Err(self.err(XsdRegexErrorKind::QuantifierWithoutAtom(c), start..self.pos))
            }
            '{' | '}' | ']' => {
                Err(self.err(XsdRegexErrorKind::UnescapedMetaChar(c), start..self.pos))
            }
            // note that '^' and '$' are ordinary characters in the XSD dialect
            c => Ok(literal_hir(c)),
        }
    }

    /// Parses the body of a character class (or the subtracted class within one) up to and
    /// including the closing `]`.  `open` is the position of the already-consumed `[`.
    ///
    /// ```text
    /// charClassExpr ::= '[' charGroup ']'
    /// charGroup     ::= posCharGroup | negCharGroup | charClassSub
    /// posCharGroup  ::= ( charRange | charClassEsc )+
    /// negCharGroup  ::= '^' posCharGroup
    /// charClassSub  ::= ( posCharGroup | negCharGroup ) '-' charClassExpr
    /// ```
    fn char_class(&mut self, open: usize) -> Result<ClassUnicode, XsdRegexError> {
        self.enter()?;
        let negated = self.peek() == Some('^');
        if negated {
            self.bump();
        }
        let mut class = ClassUnicode::empty();
        let mut first = true;
        loop {
            let Some(c) = self.peek() else {
                return Err(self.err(XsdRegexErrorKind::UnclosedCharClass, open..open + 1));
            };
            match c {
                ']' => {
                    if first {
                        return Err(self.err(XsdRegexErrorKind::EmptyCharClass, open..self.pos + 1));
                    }
                    self.bump();
                    break;
                }
                '[' => {
                    return Err(self.err_here(XsdRegexErrorKind::UnescapedMetaChar('[')));
                }
                '-' if !first && self.peek2() == Some('[') => {
                    // subtraction; the negation applies to the base class, before subtracting
                    self.bump();
                    let sub_open = self.pos;
                    self.bump();
                    let subtrahend = self.char_class(sub_open)?;
                    if negated {
                        class.negate();
                    }
                    class.difference(&subtrahend);
                    if self.peek() != Some(']') {
                        return Err(self.err_here(XsdRegexErrorKind::UnclosedCharClass));
                    }
                    self.bump();
                    self.leave();
                    return Ok(class);
                }
                '-' => {
                    // only meaningful as a literal at the very start or end of the group
                    if first || self.peek2() == Some(']') {
                        self.bump();
                        class.push(ClassUnicodeRange::new('-', '-'));
                    } else {
                        return Err(self.err_here(XsdRegexErrorKind::InvalidHyphen));
                    }
                }
                _ => self.class_item(&mut class)?,
            }
            first = false;
        }
        if negated {
            class.negate();
        }
        self.leave();
        Ok(class)
    }

    /// Parses one range, single character, or class escape from within a character class
    fn class_item(&mut self, class: &mut ClassUnicode) -> Result<(), XsdRegexError> {
        let item = self.class_atom()?;
        // a '-' is the start of a range, unless it is the last character of the group, or
        // introduces a subtraction
        let range_follows = self.peek() == Some('-') && !matches!(self.peek2(), Some(']' | '['));
        match item {
            Esc::Class(c) => {
                if range_follows {
                    return Err(self.err_here(XsdRegexErrorKind::InvalidHyphen));
                }
                class.union(&c);
            }
            Esc::Char(start) => {
                if range_follows {
                    let range_start = self.pos - start.len_utf8();
                    self.bump(); // '-'
                    let end_start = self.pos;
                    let end = match self.class_atom()? {
                        Esc::Char(end) => end,
                        Esc::Class(_) => {
                            return Err(
                                self.err(XsdRegexErrorKind::InvalidHyphen, end_start..self.pos)
                            );
                        }
                    };
                    if end < start {
                        return Err(self.err(
                            XsdRegexErrorKind::ReversedCharRange { start, end },
                            range_start..self.pos,
                        ));
                    }
                    class.push(ClassUnicodeRange::new(start, end));
                } else {
                    class.push(ClassUnicodeRange::new(start, start));
                }
            }
        }
        Ok(())
    }

    fn class_atom(&mut self) -> Result<Esc, XsdRegexError> {
        match self.bump() {
            Some('\\') => self.escape(),
            Some(c) => Ok(Esc::Char(c)),
            None => unreachable!("class_atom() is only called when input remains"),
        }
    }

    /// Parses an escape sequence, after the introducing `\` has been consumed.
    ///
    /// ```text
    /// SingleCharEsc ::= '\' [nrt\|.?*+(){}#x2D#x5B#x5D#x5E]
    /// MultiCharEsc  ::= '\' [sSiIcCdDwW]
    /// catEsc        ::= '\p{' charProp '}'
    /// complEsc      ::= '\P{' charProp '}'
    /// ```
    fn escape(&mut self) -> Result<Esc, XsdRegexError> {
        let start = self.pos - 1;
        let Some(c) = self.bump() else {
            return Err(self.err(XsdRegexErrorKind::TrailingBackslash, start..start + 1));
        };
        Ok(match c {
            'n' => Esc::Char('\n'),
            'r' => Esc::Char('\r'),
            't' => Esc::Char('\t'),
            '\\' | '|' | '.' | '?' | '*' | '+' | '(' | ')' | '{' | '}' | '-' | '[' | ']' | '^' => {
                Esc::Char(c)
            }
            's' => Esc::Class(space_class()),
            'S' => Esc::Class(negated(space_class())),
            'i' => Esc::Class(name_start_class()),
            'I' => Esc::Class(negated(name_start_class())),
            'c' => Esc::Class(name_char_class()),
            'C' => Esc::Class(negated(name_char_class())),
            'd' => Esc::Class(digit_class()),
            'D' => Esc::Class(negated(digit_class())),
            'w' => Esc::Class(word_class()),
            'W' => Esc::Class(negated(word_class())),
            'p' | 'P' => {
                let class = self.property(start)?;
                Esc::Class(if c == 'P' { negated(class) } else { class })
            }
            _ => {
                return Err(self.err(XsdRegexErrorKind::InvalidEscape(c), start..self.pos));
            }
        })
    }

    /// Parses `{Lu}` or `{IsBasicLatin}`, after the `\p` has been consumed
    fn property(&mut self, escape_start: usize) -> Result<ClassUnicode, XsdRegexError> {
        let malformed = |p: &Parser| {
            p.err(
                XsdRegexErrorKind::MalformedProperty,
                escape_start..p.pos.max(escape_start + 2),
            )
        };
        if self.peek() != Some('{') {
            return Err(malformed(self));
        }
        self.bump();
        let name_start = self.pos;
        while self.peek().is_some_and(|c| c != '}') {
            self.bump();
        }
        if self.peek().is_none() || name_start == self.pos {
            return Err(malformed(self));
        }
        let name = &self.src[name_start..self.pos];
        self.bump(); // '}'
        let span = escape_start..self.pos;
        match name.strip_prefix("Is") {
            Some(block) => block_class(block)
                .ok_or_else(|| self.err(XsdRegexErrorKind::UnknownBlock(name.to_string()), span)),
            None => category_class(name).ok_or_else(|| {
                self.err(XsdRegexErrorKind::UnknownCategory(name.to_string()), span)
            }),
        }
    }
}

fn literal_hir(c: char) -> Hir {
    Hir::literal(c.to_string().into_bytes())
}

fn class_hir(class: ClassUnicode) -> Hir {
    Hir::class(Class::Unicode(class))
}

fn negated(mut class: ClassUnicode) -> ClassUnicode {
    class.negate();
    class
}

/// Builds a class from inclusive ranges of code points.  Surrogates can't be matched by a Rust
/// `char`, so any part of a range falling in the surrogate area is dropped.
fn ranges_class(ranges: &[(u32, u32)]) -> ClassUnicode {
    let mut class = ClassUnicode::empty();
    let mut add = |lo: u32, hi: u32| {
        if let (Some(lo), Some(hi)) = (char::from_u32(lo), char::from_u32(hi)) {
            class.push(ClassUnicodeRange::new(lo, hi));
        }
    };
    for &(lo, hi) in ranges {
        if lo <= 0xD7FF {
            add(lo, hi.min(0xD7FF));
        }
        if hi >= 0xE000 {
            add(lo.max(0xE000), hi);
        }
    }
    class
}

// \s is just the four XML whitespace characters (and not all Unicode whitespace)
fn space_class() -> ClassUnicode {
    ranges_class(&[(0x9, 0xA), (0xD, 0xD), (0x20, 0x20)])
}

// XML NameStartChar ranges (XML 1.0 5th Ed):
const NAME_START_CHAR: &[(u32, u32)] = &[
    (0x3A, 0x3A), // ':'
    (0x41, 0x5A), // 'A'-'Z'
    (0x5F, 0x5F), // '_'
    (0x61, 0x7A), // 'a'-'z'
    (0xC0, 0xD6),
    (0xD8, 0xF6),
    (0xF8, 0x2FF),
    (0x370, 0x37D),
    (0x37F, 0x1FFF),
    (0x200C, 0x200D),
    (0x2070, 0x218F),
    (0x2C00, 0x2FEF),
    (0x3001, 0xD7FF),
    (0xF900, 0xFDCF),
    (0xFDF0, 0xFFFD),
    (0x10000, 0xEFFFF),
];

// XML NameChar = NameStartChar plus these
const NAME_CHAR_EXTRA: &[(u32, u32)] = &[
    (0x2D, 0x2E), // '-' and '.'
    (0x30, 0x39), // '0'-'9'
    (0xB7, 0xB7),
    (0x300, 0x36F),
    (0x203F, 0x2040),
];

fn name_start_class() -> ClassUnicode {
    ranges_class(NAME_START_CHAR)
}

fn name_char_class() -> ClassUnicode {
    let mut class = ranges_class(NAME_START_CHAR);
    class.union(&ranges_class(NAME_CHAR_EXTRA));
    class
}

// \d is any decimal digit, not just ASCII
fn digit_class() -> ClassUnicode {
    category_class("Nd").expect("Nd is a known category")
}

// \w is everything except punctuation, separators and 'other' (so it includes symbols, and not
// the underscore)
fn word_class() -> ClassUnicode {
    let mut excluded = category_class("P").expect("P is a known category");
    excluded.union(&category_class("Z").expect("Z is a known category"));
    excluded.union(&category_class("C").expect("C is a known category"));
    negated(excluded)
}

const CATEGORIES: &[&str] = &[
    "L", "Lu", "Ll", "Lt", "Lm", "Lo", //
    "M", "Mn", "Mc", "Me", //
    "N", "Nd", "Nl", "No", //
    "P", "Pc", "Pd", "Ps", "Pe", "Pi", "Pf", "Po", //
    "Z", "Zs", "Zl", "Zp", //
    "S", "Sm", "Sc", "Sk", "So", //
    "C", "Cc", "Cf", "Co", "Cn",
];

/// The characters in one of the general categories named by XSD, using the Unicode tables built
/// in to the `regex-syntax` crate.
fn category_class(name: &str) -> Option<ClassUnicode> {
    if !CATEGORIES.contains(&name) {
        return None;
    }
    let hir = regex_syntax::Parser::new()
        .parse(&format!("\\p{{{name}}}"))
        .ok()?;
    match hir.kind() {
        HirKind::Class(Class::Unicode(class)) => Some(class.clone()),
        _ => None,
    }
}

/// The Unicode blocks addressable with `\p{Is…}`, as defined by XSD 1.0 (which is a fixed list
/// that doesn't track newer versions of Unicode).  Surrogate blocks are present, but are
/// necessarily empty.
#[rustfmt::skip]
const BLOCKS: &[(&str, &[(u32, u32)])] = &[
    ("BasicLatin", &[(0x0000, 0x007F)]),
    ("Latin-1Supplement", &[(0x0080, 0x00FF)]),
    ("LatinExtended-A", &[(0x0100, 0x017F)]),
    ("LatinExtended-B", &[(0x0180, 0x024F)]),
    ("IPAExtensions", &[(0x0250, 0x02AF)]),
    ("SpacingModifierLetters", &[(0x02B0, 0x02FF)]),
    ("CombiningDiacriticalMarks", &[(0x0300, 0x036F)]),
    ("Greek", &[(0x0370, 0x03FF)]),
    ("Cyrillic", &[(0x0400, 0x04FF)]),
    ("Armenian", &[(0x0530, 0x058F)]),
    ("Hebrew", &[(0x0590, 0x05FF)]),
    ("Arabic", &[(0x0600, 0x06FF)]),
    ("Syriac", &[(0x0700, 0x074F)]),
    ("Thaana", &[(0x0780, 0x07BF)]),
    ("Devanagari", &[(0x0900, 0x097F)]),
    ("Bengali", &[(0x0980, 0x09FF)]),
    ("Gurmukhi", &[(0x0A00, 0x0A7F)]),
    ("Gujarati", &[(0x0A80, 0x0AFF)]),
    ("Oriya", &[(0x0B00, 0x0B7F)]),
    ("Tamil", &[(0x0B80, 0x0BFF)]),
    ("Telugu", &[(0x0C00, 0x0C7F)]),
    ("Kannada", &[(0x0C80, 0x0CFF)]),
    ("Malayalam", &[(0x0D00, 0x0D7F)]),
    ("Sinhala", &[(0x0D80, 0x0DFF)]),
    ("Thai", &[(0x0E00, 0x0E7F)]),
    ("Lao", &[(0x0E80, 0x0EFF)]),
    ("Tibetan", &[(0x0F00, 0x0FFF)]),
    ("Myanmar", &[(0x1000, 0x109F)]),
    ("Georgian", &[(0x10A0, 0x10FF)]),
    ("HangulJamo", &[(0x1100, 0x11FF)]),
    ("Ethiopic", &[(0x1200, 0x137F)]),
    ("Cherokee", &[(0x13A0, 0x13FF)]),
    ("UnifiedCanadianAboriginalSyllabics", &[(0x1400, 0x167F)]),
    ("Ogham", &[(0x1680, 0x169F)]),
    ("Runic", &[(0x16A0, 0x16FF)]),
    ("Khmer", &[(0x1780, 0x17FF)]),
    ("Mongolian", &[(0x1800, 0x18AF)]),
    ("LatinExtendedAdditional", &[(0x1E00, 0x1EFF)]),
    ("GreekExtended", &[(0x1F00, 0x1FFF)]),
    ("GeneralPunctuation", &[(0x2000, 0x206F)]),
    ("SuperscriptsandSubscripts", &[(0x2070, 0x209F)]),
    ("CurrencySymbols", &[(0x20A0, 0x20CF)]),
    ("CombiningMarksforSymbols", &[(0x20D0, 0x20FF)]),
    ("LetterlikeSymbols", &[(0x2100, 0x214F)]),
    ("NumberForms", &[(0x2150, 0x218F)]),
    ("Arrows", &[(0x2190, 0x21FF)]),
    ("MathematicalOperators", &[(0x2200, 0x22FF)]),
    ("MiscellaneousTechnical", &[(0x2300, 0x23FF)]),
    ("ControlPictures", &[(0x2400, 0x243F)]),
    ("OpticalCharacterRecognition", &[(0x2440, 0x245F)]),
    ("EnclosedAlphanumerics", &[(0x2460, 0x24FF)]),
    ("BoxDrawing", &[(0x2500, 0x257F)]),
    ("BlockElements", &[(0x2580, 0x259F)]),
    ("GeometricShapes", &[(0x25A0, 0x25FF)]),
    ("MiscellaneousSymbols", &[(0x2600, 0x26FF)]),
    ("Dingbats", &[(0x2700, 0x27BF)]),
    ("BraillePatterns", &[(0x2800, 0x28FF)]),
    ("CJKRadicalsSupplement", &[(0x2E80, 0x2EFF)]),
    ("KangxiRadicals", &[(0x2F00, 0x2FDF)]),
    ("IdeographicDescriptionCharacters", &[(0x2FF0, 0x2FFF)]),
    ("CJKSymbolsandPunctuation", &[(0x3000, 0x303F)]),
    ("Hiragana", &[(0x3040, 0x309F)]),
    ("Katakana", &[(0x30A0, 0x30FF)]),
    ("Bopomofo", &[(0x3100, 0x312F)]),
    ("HangulCompatibilityJamo", &[(0x3130, 0x318F)]),
    ("Kanbun", &[(0x3190, 0x319F)]),
    ("BopomofoExtended", &[(0x31A0, 0x31BF)]),
    ("EnclosedCJKLettersandMonths", &[(0x3200, 0x32FF)]),
    ("CJKCompatibility", &[(0x3300, 0x33FF)]),
    ("CJKUnifiedIdeographsExtensionA", &[(0x3400, 0x4DB5)]),
    ("CJKUnifiedIdeographs", &[(0x4E00, 0x9FFF)]),
    ("YiSyllables", &[(0xA000, 0xA48F)]),
    ("YiRadicals", &[(0xA490, 0xA4CF)]),
    ("HangulSyllables", &[(0xAC00, 0xD7A3)]),
    ("HighSurrogates", &[(0xD800, 0xDB7F)]),
    ("HighPrivateUseSurrogates", &[(0xDB80, 0xDBFF)]),
    ("LowSurrogates", &[(0xDC00, 0xDFFF)]),
    ("PrivateUse", &[(0xE000, 0xF8FF), (0xF0000, 0xFFFFD), (0x100000, 0x10FFFD)]),
    ("CJKCompatibilityIdeographs", &[(0xF900, 0xFAFF)]),
    ("AlphabeticPresentationForms", &[(0xFB00, 0xFB4F)]),
    ("ArabicPresentationForms-A", &[(0xFB50, 0xFDFF)]),
    ("CombiningHalfMarks", &[(0xFE20, 0xFE2F)]),
    ("CJKCompatibilityForms", &[(0xFE30, 0xFE4F)]),
    ("SmallFormVariants", &[(0xFE50, 0xFE6F)]),
    ("ArabicPresentationForms-B", &[(0xFE70, 0xFEFE)]),
    ("Specials", &[(0xFEFF, 0xFEFF), (0xFFF0, 0xFFFD)]),
    ("HalfwidthandFullwidthForms", &[(0xFF00, 0xFFEF)]),
    ("OldItalic", &[(0x10300, 0x1032F)]),
    ("Gothic", &[(0x10330, 0x1034F)]),
    ("Deseret", &[(0x10400, 0x1044F)]),
    ("ByzantineMusicalSymbols", &[(0x1D000, 0x1D0FF)]),
    ("MusicalSymbols", &[(0x1D100, 0x1D1FF)]),
    ("MathematicalAlphanumericSymbols", &[(0x1D400, 0x1D7FF)]),
    ("CJKUnifiedIdeographsExtensionB", &[(0x20000, 0x2A6D6)]),
    ("CJKCompatibilityIdeographsSupplement", &[(0x2F800, 0x2FA1F)]),
    ("Tags", &[(0xE0000, 0xE007F)]),
];

/// `name` is the block name without the leading `Is`
fn block_class(name: &str) -> Option<ClassUnicode> {
    BLOCKS
        .iter()
        .find(|(n, _)| *n == name)
        .map(|(_, ranges)| ranges_class(ranges))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn compile(pat: &str) -> XsdRegex {
        XsdRegex::new(pat).unwrap_or_else(|e| panic!("failed to compile {pat:?}: {e}"))
    }

    fn matches(pat: &str, value: &str) -> bool {
        compile(pat).is_match(value)
    }

    fn error(pat: &str) -> XsdRegexError {
        XsdRegex::new(pat).expect_err(&format!("{pat:?} should be rejected"))
    }

    /// The text of the pattern that the error points at
    fn error_text(pat: &str) -> Option<&str> {
        error(pat).span().map(|s| &pat[s])
    }

    #[test]
    fn anchoring() {
        assert!(matches("abc", "abc"));
        assert!(!matches("abc", "xabc"));
        assert!(!matches("abc", "abcx"));
        assert!(!matches("abc", "xabcx"));
        assert!(!matches("abc", "abc\n"));
    }

    #[test]
    fn alternation_is_anchored_as_a_whole() {
        assert!(matches("a|b", "a"));
        assert!(matches("a|b", "b"));
        assert!(!matches("a|b", "ab"));
        assert!(matches("a||b", ""));
    }

    #[test]
    fn empty_pattern() {
        assert!(matches("", ""));
        assert!(!matches("", "a"));
    }

    #[test]
    fn common_patterns() {
        assert!(matches("[^:]*", "test"));
        assert!(!matches("[^:]*", "te:st"));
        assert!(matches(".+/.+", "a/b"));
        assert!(matches("[A-Za-z]{1,8}(-[A-Za-z0-9]{1,8})*", "en-GB-x1"));
        assert!(!matches("[A-Za-z]{1,8}(-[A-Za-z0-9]{1,8})*", "abcdefghi"));
        assert!(matches(".+@.+", "me@example.com"));
    }

    #[test]
    fn quantifiers() {
        assert!(matches("a?", ""));
        assert!(matches("a+", "aaa"));
        assert!(!matches("a+", ""));
        assert!(matches("a{2}", "aa"));
        assert!(!matches("a{2}", "aaa"));
        assert!(matches("a{2,}", "aaaa"));
        assert!(!matches("a{2,}", "a"));
        assert!(matches("a{0,2}", ""));
        assert!(!matches("a{0,2}", "aaa"));
        // XSD has no lazy quantifiers, and a piece has at most one quantifier
        assert_eq!(
            &XsdRegexErrorKind::QuantifierWithoutAtom('?'),
            error("a*?").kind()
        );
        assert!(matches("(a*)?", "aaa"));
        assert!(matches("(ab){2}", "abab"));
    }

    #[test]
    fn dot_excludes_newline_and_carriage_return() {
        assert!(matches("a.b", "axb"));
        assert!(!matches("a.b", "a\nb"));
        assert!(!matches("a.b", "a\rb"));
        assert!(matches("a.b", "a\u{85}b"));
        assert!(matches("a.b", "a\u{1F600}b"));
    }

    #[test]
    fn caret_and_dollar_are_literals() {
        assert!(!matches("^abc$", "abc"));
        assert!(matches("^abc$", "^abc$"));
        assert!(matches("a^b", "a^b"));
        assert!(matches("[$^]+", "$^$"));
        assert!(matches("[a^]", "^"));
    }

    #[test]
    fn name_start_char() {
        assert!(matches(r"\i", "a"));
        assert!(matches(r"\i", "Z"));
        assert!(matches(r"\i", "_"));
        assert!(matches(r"\i", ":"));
        assert!(matches(r"\i", "\u{C0}"));
        assert!(!matches(r"\i", "1"));
        assert!(!matches(r"\i", "-"));
        assert!(!matches(r"\i", " "));
    }

    #[test]
    fn name_char() {
        assert!(matches(r"\c", "a"));
        assert!(matches(r"\c", "1"));
        assert!(matches(r"\c", "-"));
        assert!(matches(r"\c", "."));
        assert!(matches(r"\c", ":"));
        assert!(matches(r"\c", "_"));
        assert!(!matches(r"\c", " "));
        assert!(!matches(r"\c", "!"));
    }

    #[test]
    fn complement_name_start_char() {
        assert!(matches(r"\I", "1"));
        assert!(matches(r"\I", "-"));
        assert!(matches(r"\I", " "));
        assert!(!matches(r"\I", "a"));
        assert!(!matches(r"\I", "_"));
        assert!(!matches(r"\I", ":"));
    }

    #[test]
    fn complement_name_char() {
        assert!(matches(r"\C", " "));
        assert!(matches(r"\C", "!"));
        assert!(!matches(r"\C", "a"));
        assert!(!matches(r"\C", "1"));
        assert!(!matches(r"\C", "-"));
    }

    #[test]
    fn space() {
        for c in [" ", "\t", "\n", "\r"] {
            assert!(matches(r"\s", c));
            assert!(!matches(r"\S", c));
        }
        for c in ["\u{a0}", "\u{b}", "\u{c}", "\u{2003}", "x"] {
            assert!(!matches(r"\s", c), "{c:?}");
            assert!(matches(r"\S", c), "{c:?}");
        }
    }

    #[test]
    fn digit() {
        assert!(matches(r"\d+", "123"));
        assert!(matches(r"\d", "\u{663}")); // ARABIC-INDIC DIGIT THREE
        assert!(!matches(r"\d", "a"));
        assert!(matches(r"\D", "a"));
        assert!(!matches(r"\D", "1"));
    }

    #[test]
    fn word() {
        assert!(matches(r"\w+", "abc123"));
        assert!(matches(r"\w", "+")); // symbols count
        assert!(matches(r"\w", "$"));
        assert!(!matches(r"\w", "_")); // punctuation does not
        assert!(!matches(r"\w", "-"));
        assert!(!matches(r"\w", " "));
        assert!(!matches(r"\w", "\u{a0}"));
        assert!(!matches(r"\W", "a"));
        assert!(matches(r"\W", "_"));
        assert!(matches(r"\W", " "));
    }

    #[test]
    fn categories() {
        assert!(matches(r"\p{L}+", "abcΩ"));
        assert!(matches(r"\p{Lu}", "A"));
        assert!(!matches(r"\p{Lu}", "a"));
        assert!(matches(r"\P{Lu}", "a"));
        assert!(matches(r"\p{Nd}", "5"));
        assert!(matches(r"\p{Zs}", " "));
        assert!(matches(r"\p{Cn}", "\u{378}")); // unassigned
        assert!(matches(r"[\p{Lu}\p{Nd}]+", "A1B2"));
        assert!(!matches(r"[\p{Lu}\p{Nd}]+", "a"));
        assert!(matches(r"[^\p{L}]", "1"));
    }

    #[test]
    fn blocks() {
        assert!(matches(r"\p{IsBasicLatin}+", "abc"));
        assert!(!matches(r"\p{IsBasicLatin}", "é"));
        assert!(matches(r"\p{IsLatin-1Supplement}", "é"));
        assert!(matches(r"\p{IsGreek}", "\u{3b1}"));
        // a block, not a script: Greek Extended is separate
        assert!(!matches(r"\p{IsGreek}", "\u{1f00}"));
        assert!(matches(r"\p{IsGreekExtended}", "\u{1f00}"));
        assert!(matches(r"\p{IsCyrillic}", "\u{416}"));
        assert!(matches(r"\p{IsPrivateUse}", "\u{e000}"));
        assert!(matches(r"\p{IsPrivateUse}", "\u{f0000}"));
        assert!(matches(r"\p{IsSpecials}", "\u{feff}"));
        assert!(matches(r"\P{IsBasicLatin}", "é"));
        assert!(matches(r"[\p{IsGreek}\p{IsCyrillic}]+", "αЖ"));
        // surrogates can never appear in a Rust string
        assert!(!matches(r"\p{IsHighSurrogates}", "a"));
    }

    #[test]
    fn char_class_basics() {
        assert!(matches("[abc]+", "cab"));
        assert!(matches("[a-c]+", "cab"));
        assert!(!matches("[a-c]", "d"));
        assert!(matches("[^a-c]", "d"));
        assert!(!matches("[^a-c]", "b"));
        assert!(matches(r"[\n\t]", "\t"));
        assert!(matches(r"[\]]", "]"));
        assert!(matches(r"[\[]", "["));
        assert!(matches(r"[\^]", "^"));
        assert!(matches(r"[.]", "."));
        assert!(!matches(r"[.]", "x"));
        assert!(matches(r"[|]", "|"));
        assert!(matches(r"[a|b]", "|"));
    }

    #[test]
    fn char_class_hyphens() {
        // literal at start and end
        assert!(matches("[-a]", "-"));
        assert!(matches("[a-]", "-"));
        assert!(matches("[^-a]", "b"));
        assert!(!matches("[^-a]", "-"));
        assert!(matches("[a-z-]", "-"));
        assert!(matches(r"[a\-z]", "-"));
        assert!(!matches(r"[a\-z]", "b"));
    }

    #[test]
    fn char_class_operators_from_other_dialects_are_literal() {
        assert!(matches("[a&&b]", "&"));
        assert!(!matches("[a&&b]", "a&b"));
        assert!(matches("[a~~b]", "~"));
    }

    #[test]
    fn char_class_escapes_union_with_the_rest() {
        assert!(matches(r"[a\I]", "a"));
        assert!(matches(r"[a\I]", "1"));
        assert!(!matches(r"[a\I]", "b"));
        assert!(matches(r"[\s,]+", " ,\t"));
        assert!(matches(r"[\d\-]+", "1-2"));
    }

    #[test]
    fn char_class_subtraction() {
        assert!(matches("[a-z-[m]]", "a"));
        assert!(!matches("[a-z-[m]]", "m"));
        assert!(!matches("[a-z-[m]]", "1"));
        assert!(matches("[a-z-[aeiou]]+", "xyz"));
        assert!(!matches("[a-z-[aeiou]]+", "xya"));
        // negation applies to the base group, then the subtraction
        assert!(!matches("[^a-z-[m]]", "m"));
        assert!(!matches("[^a-z-[m]]", "b"));
        assert!(matches("[^a-z-[m]]", "1"));
        // nested
        assert!(matches("[a-z-[b-y-[m]]]", "m"));
        assert!(!matches("[a-z-[b-y-[m]]]", "c"));
        assert!(matches("[a-z-[b-y-[m]]]", "a"));
        // with escapes
        assert!(matches(r"[\i-[:]][\c-[:]]*", "foo-bar"));
        assert!(!matches(r"[\i-[:]][\c-[:]]*", "a:b"));
        assert!(!matches(r"[\i-[:]][\c-[:]]*", ":a"));
        assert!(matches(r"[\p{L}-[\p{Lu}]]", "a"));
        assert!(!matches(r"[\p{L}-[\p{Lu}]]", "A"));
    }

    #[test]
    fn escaped_metacharacters() {
        for c in r"\|.?*+(){}-[]^".chars() {
            let s = c.to_string();
            let pat = format!("\\{c}");
            assert!(matches(&pat, &s), "{pat}");
            assert!(!matches(&pat, "x"), "{pat}");
        }
        assert!(matches(r"\n", "\n"));
        assert!(matches(r"\r", "\r"));
        assert!(matches(r"\t", "\t"));
    }

    #[test]
    fn supplementary_plane_characters_are_single_chars() {
        assert!(matches("^.$", "^\u{1F600}$"));
        assert!(matches(".", "\u{1F600}"));
        assert!(matches("[\u{1F600}-\u{1F64F}]", "\u{1F610}"));
    }

    #[test]
    fn large_repetition() {
        assert!(matches("a{0,100000}", "a"));
        assert!(matches("(a{1,100}){1,100}", "a"));
    }

    #[test]
    fn too_large_repetition_is_rejected_rather_than_exhausting_memory() {
        let err = error(r"((\p{L}{1,1000}){1,1000}){1,1000}");
        assert_eq!(&XsdRegexErrorKind::TooLarge, err.kind());
        assert_eq!(None, err.span());
    }

    #[test]
    fn deep_nesting_is_rejected() {
        let pat = format!("{}a{}", "(".repeat(10_000), ")".repeat(10_000));
        assert_eq!(&XsdRegexErrorKind::NestingTooDeep, error(&pat).kind());
    }

    #[test]
    fn syntax_from_other_dialects_is_rejected() {
        assert_eq!(&XsdRegexErrorKind::InvalidEscape('b'), error(r"\b").kind());
        assert_eq!(
            &XsdRegexErrorKind::InvalidEscape('x'),
            error(r"\x41").kind()
        );
        assert_eq!(&XsdRegexErrorKind::InvalidHyphen, error(r"[\d-z]").kind());
        error("(?i)abc");
        error("(?:abc)");
        error("a**");
        error("a{,3}");
        error("a{3");
        error("a{x}");
        error("{1}");
        error(r"\pL");
        error(r"\p{letter}");
        error(r"\p{Cs}");
        error(r"\p{IsNoSuchBlock}");
        error(r"\p{}");
        error(r"\1");
    }

    #[test]
    fn error_spans() {
        assert_eq!(Some(r"\b"), error_text(r"ab\bcd"));
        assert_eq!(Some(r"\"), error_text(r"ab\"));
        assert_eq!(Some("("), error_text("a(b"));
        assert_eq!(Some(")"), error_text("a(b))c"));
        assert_eq!(Some("["), error_text("a[bc"));
        assert_eq!(Some("[]"), error_text("a[]"));
        assert_eq!(Some("*"), error_text("a|*"));
        assert_eq!(Some("{"), error_text("a|{"));
        assert_eq!(Some("{2,1}"), error_text("ab{2,1}c"));
        assert_eq!(Some("{3"), error_text("ab{3"));
        assert_eq!(Some("99999999999"), error_text("a{99999999999}"));
        assert_eq!(Some("z-a"), error_text("[0z-a9]"));
        assert_eq!(Some(r"\p{IsNope}"), error_text(r"x\p{IsNope}y"));
        assert_eq!(Some(r"\p{Xx}"), error_text(r"x\p{Xx}y"));
        assert_eq!(Some("-"), error_text("[a-c-e]"));
    }

    #[test]
    fn error_spans_are_byte_offsets_into_the_pattern() {
        // 'é' is two bytes in UTF-8
        let pat = "éé\\bé";
        let err = error(pat);
        assert_eq!(Some(4..6), err.span());
        assert_eq!(r"\b", &pat[err.span().unwrap()]);
    }

    #[test]
    fn error_messages() {
        assert_eq!(
            "'\\b' is not a valid escape sequence",
            error(r"\b").to_string()
        );
        assert_eq!(
            "unknown Unicode block \"IsNope\"",
            error(r"\p{IsNope}").to_string()
        );
    }
}
