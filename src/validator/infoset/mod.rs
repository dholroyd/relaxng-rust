//! Builds the XML-with-Namespaces view of a document from the token events of
//! `xml-syntax-reader`: resolves namespace prefixes, expands character references and the
//! predefined entities, assembles attribute values, and applies the well-formedness checks that
//! the tokenizer leaves to its caller. The results are delivered to a [`Handler`].
//!
//! The DOCTYPE declaration is skipped, so entities declared in a DTD are not supported.
//!
//! Only UTF-8 documents are supported (US-ASCII being a subset of it). A document in another
//! encoding, whether detected from its first bytes or named in its XML declaration, is rejected
//! with [`Error::UnsupportedEncoding`] rather than misread. A UTF-8 byte order mark is skipped.
//!
//! Nothing here knows about RELAX NG.

mod name;

pub(crate) use name::{Name, NameTable};

use crate::xml_whitespace::is_all_xml_whitespace;
use std::ops::Range;
use xml_syntax_reader::{Encoding, ErrorKind, ParseError, QName, Span, Visitor};

const XML_NAMESPACE: &str = "http://www.w3.org/XML/1998/namespace";
const MAX_ATTRIBUTES: usize = 256;
pub(crate) const MAX_ATTRIBUTE_VALUE: usize = 1024 * 1024;
/// The deepest element nesting accepted.  Each open element adds to the
/// validator's pattern table, so this prevents deep documents exhausting it.
pub(crate) const MAX_ELEMENT_DEPTH: usize = 1024;

/// A document that is not well-formed, or could not be read.
#[derive(Debug)]
pub(crate) enum Error {
    /// Error reported by the tokenizer.
    Xml(xml_syntax_reader::Error),
    UndefinedNamespacePrefix {
        prefix: String,
        span: Range<u64>,
    },
    UndefinedEntity {
        name: String,
        span: Range<u64>,
    },
    InvalidOrUnclosedEntity {
        span: Range<u64>,
    },
    /// An element or attribute name is not a valid XML Name.
    InvalidName {
        span: Range<u64>,
        kind: &'static str,
    },
    DuplicateAttribute {
        span: Range<u64>,
    },
    /// An end tag whose name differs from that of the open element, or that closes no element.
    MismatchedEndTag {
        span: Range<u64>,
        /// The name of the open element's start tag, if any element is open.
        start: Option<Range<u64>>,
    },
    /// The input ended inside an element. `span` is that of the innermost open element's
    /// start-tag name.
    UnclosedElement {
        span: Range<u64>,
    },
    /// The document has no root element.
    MissingRootElement,
    /// Something other than a comment, processing instruction or whitespace before or after the
    /// root element.
    OutsideRootElement {
        span: Range<u64>,
        kind: &'static str,
    },
    /// More than [`MAX_ATTRIBUTES`] attributes (including namespace declarations) on one element.
    TooManyAttributes {
        span: Range<u64>,
    },
    AttributeValueTooLong {
        span: Range<u64>,
    },
    /// An element nested more than [`MAX_ELEMENT_DEPTH`] deep. `span` is that of its name.
    ElementTooDeep {
        span: Range<u64>,
    },
    /// Bytes that should be UTF-8 (e.g. a namespace prefix) were not valid UTF-8.
    InvalidUtf8 {
        span: Range<u64>,
        kind: &'static str,
    },
    /// The document is in an encoding other than UTF-8. `span` is that of the byte order mark or
    /// first character from which a UTF-16 or UTF-32 encoding was detected, or of the `<?xml`
    /// starting the XML declaration that names the encoding.
    UnsupportedEncoding {
        encoding: String,
        span: Range<u64>,
    },
}

/// Why parsing stopped.
pub(crate) enum Failure<E> {
    /// The document is not well-formed, or could not be read.
    Infoset(Error),
    /// A [`Handler`] callback returned an error.
    Handler(E),
}

/// An element or attribute name with its prefix resolved.
#[derive(Clone, Copy)]
pub(crate) struct ExpandedName<'a> {
    /// `None` when the name is in no namespace.
    pub(crate) namespace_uri: Option<&'a str>,
    pub(crate) local_name: Name<'a>,
}

/// A complete start tag (or empty-element tag), with every attribute's value assembled and every
/// prefix resolved.
pub(crate) struct StartElement<'a> {
    pub(crate) name: ExpandedName<'a>,
    pub(crate) name_span: Span,
    /// The span of the `>` or `/>` that closed the tag.
    pub(crate) close_span: Span,
    /// True for an empty-element tag (`<a/>`), which is the whole element: no `end_element`
    /// follows.
    pub(crate) empty: bool,
    attrs: &'a [Attr],
    arena: &'a TagArena,
    stack: &'a ElementStack,
}

impl<'a> StartElement<'a> {
    /// The element's attributes in document order, excluding namespace declarations.
    pub(crate) fn attributes(&self) -> impl Iterator<Item = Attribute<'a>> {
        let (arena, stack) = (self.arena, self.stack);
        self.attrs.iter().map(move |a| Attribute {
            name: ExpandedName {
                namespace_uri: stack.namespace_uri(a.namespace),
                local_name: Name::new(arena.get_name(a.local)),
            },
            name_span: a.name_span,
            value: arena.get_value(a.value),
        })
    }
}

pub(crate) struct Attribute<'a> {
    pub(crate) name: ExpandedName<'a>,
    pub(crate) name_span: Span,
    /// The value with entity and character references expanded.
    pub(crate) value: &'a str,
}

/// The namespace declarations in scope at the current point in the document.
pub(crate) struct NamespaceScope<'a> {
    stack: &'a ElementStack,
}

impl<'a> NamespaceScope<'a> {
    /// The namespace URI bound to `prefix`, including the predeclared `xml` prefix. The empty
    /// prefix is never bound; see [`Self::default_namespace`].
    pub(crate) fn lookup_prefix(&self, prefix: &str) -> Option<&'a str> {
        if prefix.is_empty() {
            return None;
        }
        self.stack
            .lookup_prefix(prefix.as_bytes())
            .and_then(|ns| self.stack.namespace_uri(ns))
    }

    /// The default namespace URI, or `None` if there is none, including where it has been
    /// undeclared with `xmlns=""`.
    pub(crate) fn default_namespace(&self) -> Option<&'a str> {
        self.stack
            .default_ns
            .map(|i| {
                self.stack.ns_declarations[i as usize]
                    .namespace_uri
                    .as_str()
            })
            .filter(|uri| !uri.is_empty())
    }
}

/// Receives the events of a document.
///
/// Character data arrives in pieces through [`Handler::text`]. A run of adjacent pieces,
/// including any split up by comments or processing instructions, ends with a call to
/// [`Handler::end_text`] before the next element event, while the namespace scope is still the
/// one the text appeared in.
pub(crate) trait Handler {
    type Error;

    fn start_element(
        &mut self,
        ns: &NamespaceScope<'_>,
        element: &StartElement<'_>,
    ) -> Result<(), Self::Error>;

    /// The end tag of an element. `span` is that of the end tag's name. Not called for an
    /// empty-element tag, which [`Self::start_element`] receives whole.
    fn end_element(&mut self, ns: &NamespaceScope<'_>, span: Span) -> Result<(), Self::Error>;

    /// A piece of character data, already checked to be UTF-8, with references expanded.
    fn text(&mut self, ns: &NamespaceScope<'_>, text: &str, span: Span) -> Result<(), Self::Error>;

    fn end_text(&mut self, ns: &NamespaceScope<'_>) -> Result<(), Self::Error> {
        let _ = ns;
        Ok(())
    }
}

/// Parses a document supplied by the caller one buffer at a time, mirroring
/// [`xml_syntax_reader::Reader::parse`]: each call passes on the events of as much of the buffer
/// as can be tokenized, and returns how many bytes that was. The caller must pass the unconsumed
/// remainder again, at the front of the next buffer.
///
/// After an error or the final buffer, the parser must be reset before it is used again.
pub(crate) struct PushParser {
    reader: xml_syntax_reader::Reader,
    state: State,
    /// The stream offset of the first byte not yet consumed.
    offset: u64,
    /// Whether the first bytes of the document have been checked for its encoding.
    encoding_checked: bool,
}

/// How many bytes [`xml_syntax_reader::probe_encoding`] needs to tell every byte order mark and
/// UTF-16/UTF-32 pattern apart. Given fewer, it can mistake the start of a UTF-32LE byte order
/// mark for a UTF-16LE one, or miss a UTF-8 one.
const ENCODING_PROBE_LEN: usize = 4;

impl PushParser {
    pub(crate) fn new() -> Self {
        PushParser {
            reader: xml_syntax_reader::Reader::new(),
            state: State::new(),
            offset: 0,
            encoding_checked: false,
        }
    }

    /// Returns to the start of a new document, abandoning any document in progress.
    pub(crate) fn reset(&mut self) {
        self.reader.reset();
        self.state.reset();
        self.offset = 0;
        self.encoding_checked = false;
    }

    /// Parses as much of `buf` as possible, returning the number of bytes consumed.
    pub(crate) fn feed<H: Handler>(
        &mut self,
        buf: &[u8],
        handler: &mut H,
    ) -> Result<usize, Failure<H::Error>> {
        self.parse(buf, false, handler)
    }

    /// Parses the final buffer of the document, reporting any construct it leaves unfinished.
    pub(crate) fn finish<H: Handler>(
        &mut self,
        buf: &[u8],
        handler: &mut H,
    ) -> Result<(), Failure<H::Error>> {
        self.parse(buf, true, handler).map(|_| ())
    }

    fn parse<H: Handler>(
        &mut self,
        buf: &[u8],
        is_final: bool,
        handler: &mut H,
    ) -> Result<usize, Failure<H::Error>> {
        let mut skipped = 0;
        if !self.encoding_checked {
            if buf.len() < ENCODING_PROBE_LEN && !is_final {
                return Ok(0);
            }
            skipped = check_initial_encoding(buf)?;
            self.encoding_checked = true;
        }
        let mut driver = Driver::new(&mut self.state, handler);
        let consumed = self
            .reader
            .parse(
                &buf[skipped..],
                self.offset + skipped as u64,
                is_final,
                &mut driver,
            )
            .map_err(map_parse_error)?;
        let consumed = skipped + consumed as usize;
        self.offset += consumed as u64;
        Ok(consumed)
    }
}

fn check_initial_encoding<E>(buf: &[u8]) -> Result<usize, Failure<E>> {
    // TODO: assumes the first buffer holds the whole XML declaration
    let probe = xml_syntax_reader::probe_encoding(buf);
    let (encoding, unit_len) = match probe.encoding {
        Encoding::Utf16Le => ("UTF-16LE", 2),
        Encoding::Utf16Be => ("UTF-16BE", 2),
        Encoding::Utf32Le => ("UTF-32LE", 4),
        Encoding::Utf32Be => ("UTF-32BE", 4),
        Encoding::Utf8 | Encoding::Declared(_) | Encoding::Unknown => {
            // A byte order mark ends the probe before it reads the declaration, so look
            // again past one
            let bom_length = probe.bom_length;
            check_declared_encoding(&buf[bom_length..], bom_length as u64)?;
            return Ok(bom_length);
        }
    };
    Err(Failure::Infoset(Error::UnsupportedEncoding {
        encoding: encoding.to_string(),
        span: 0..unit_len,
    }))
}

fn check_declared_encoding<E>(buf: &[u8], offset: u64) -> Result<(), Failure<E>> {
    match xml_syntax_reader::probe_encoding(buf).encoding {
        Encoding::Declared(name) if !is_supported_encoding(name.as_bytes()) => {
            Err(Failure::Infoset(Error::UnsupportedEncoding {
                encoding: String::from_utf8_lossy(name.as_bytes()).into_owned(),
                // `probe_encoding` does not say where the declaration ends, only that it
                // starts with `<?xml`
                span: offset..offset + b"<?xml".len() as u64,
            }))
        }
        _ => Ok(()),
    }
}

fn is_supported_encoding(name: &[u8]) -> bool {
    const SUPPORTED: [&[u8]; 3] = [b"UTF-8", b"US-ASCII", b"ASCII"];
    SUPPORTED.iter().any(|s| name.eq_ignore_ascii_case(s))
}

fn map_parse_error<E>(e: ParseError<Failure<E>>) -> Failure<E> {
    match e {
        ParseError::Visitor(f) => f,
        ParseError::Xml(e) => Failure::Infoset(map_xml_error(e)),
    }
}

/// Map xml-syntax-reader parse errors to infoset errors.
/// `ExpectedName` is promoted to `InvalidName` since it means the tokenizer
/// encountered an invalid name-start character.
fn map_xml_error(e: xml_syntax_reader::Error) -> Error {
    if matches!(e.kind, ErrorKind::ExpectedName(_)) {
        let offset = e.offset;
        Error::InvalidName {
            span: offset..offset,
            kind: "element or attribute",
        }
    } else {
        Error::Xml(e)
    }
}

/// Arena for attribute names and values within a single start tag. All data is appended to one
/// `Vec<u8>` and referenced by `(start, len)` ranges. Reset once the start tag has been handled.
struct TagArena {
    buf: Vec<u8>,
}

impl Default for TagArena {
    fn default() -> Self {
        TagArena {
            buf: Vec::with_capacity(512),
        }
    }
}

/// Range into a name buffer for a short name (max 65535 bytes).
type NameRange = (u32, u16);
/// Range into the `TagArena` for an attribute value (may be longer).
type ValueRange = (u32, u32);

fn push_name(buf: &mut Vec<u8>, name: &[u8]) -> NameRange {
    let start = buf.len() as u32;
    buf.extend_from_slice(name);
    (start, name.len() as u16)
}

fn get_name(buf: &[u8], range: NameRange) -> &[u8] {
    let start = range.0 as usize;
    let end = start + range.1 as usize;
    &buf[start..end]
}

impl TagArena {
    /// Append bytes and return a name-sized range.
    fn push_name(&mut self, name: &[u8]) -> NameRange {
        push_name(&mut self.buf, name)
    }

    /// Mark the start of an attribute value being accumulated.
    fn begin_value(&self) -> u32 {
        self.buf.len() as u32
    }

    /// Append bytes to the attribute value that began at `start`, unless that would make the
    /// value longer than [`MAX_ATTRIBUTE_VALUE`].
    fn push_value(&mut self, start: u32, data: &[u8]) -> Result<(), ()> {
        if self.buf.len() - start as usize + data.len() > MAX_ATTRIBUTE_VALUE {
            return Err(());
        }
        self.buf.extend_from_slice(data);
        Ok(())
    }

    /// Finish an attribute value and return its range.
    fn finish_value(&self, start: u32) -> ValueRange {
        (start, self.buf.len() as u32)
    }

    /// Get the raw bytes for a name range.
    fn get_name(&self, range: NameRange) -> &[u8] {
        get_name(&self.buf, range)
    }

    /// Get a `&str` for a value range. Infallible by construction: every
    /// `push_value` call site contributes valid UTF-8 (validated at the
    /// `attribute_value` visitor boundary, or produced by `c.encode_utf8` /
    /// a `&str` source). Concatenation of valid UTF-8 is valid UTF-8.
    fn get_value(&self, range: ValueRange) -> &str {
        let start = range.0 as usize;
        let end = range.1 as usize;
        let bytes = &self.buf[start..end];
        debug_assert!(
            std::str::from_utf8(bytes).is_ok(),
            "TagArena value invariant violated"
        );
        std::str::from_utf8(bytes).expect("TagArena values are valid UTF-8 by construction")
    }

    fn clear(&mut self) {
        self.buf.clear();
    }
}

/// The namespace a name was resolved to.
#[derive(Clone, Copy)]
enum NsRef {
    None,
    Xml,
    /// Index into `ElementStack::ns_declarations`.
    Decl(u32),
}

/// Attribute with name and value stored as ranges into the `TagArena`.
struct Attr {
    prefix: Option<NameRange>,
    local: NameRange,
    name_span: Span,
    value: ValueRange,
    /// Resolved once the whole start tag has been read.
    namespace: NsRef,
}

/// A namespace prefix→URI binding in the flat declaration stack.
struct NsDecl {
    prefix: String,
    namespace_uri: String,
}

/// Per-element metadata on the scope stack.
struct ElementScope {
    /// Ranges into `ElementStack::names`.
    prefix: Option<NameRange>,
    local: NameRange,
    name_span: Span,
    /// Index into `ns_declarations` where this element's declarations start.
    ns_decl_start: u32,
    /// The default namespace declaration in scope outside this element, restored on pop.
    outer_default_ns: Option<u32>,
}

#[derive(Default)]
struct ElementStack {
    /// Element scope stack.
    scopes: Vec<ElementScope>,
    /// The names of the open elements, which unlike attribute names outlive their start tag.
    /// Pop truncates back to the popped scope's name.
    names: Vec<u8>,
    /// Flat list of all prefix→URI namespace declarations across all depths.
    /// Each element scope owns the range `[scope.ns_decl_start .. next_scope.ns_decl_start]`.
    /// Pop truncates back to the scope's start index.
    ns_declarations: Vec<NsDecl>,
    /// Index into `ns_declarations` of the default namespace declaration in scope.
    default_ns: Option<u32>,
}

impl ElementStack {
    fn clear(&mut self) {
        self.scopes.clear();
        self.names.clear();
        self.ns_declarations.clear();
        self.default_ns = None;
    }

    /// Byte-compare lookup. Stored prefixes are guaranteed valid UTF-8
    /// (validated at `add_namespace`), so a hit implies the input bytes are
    /// also valid UTF-8 by transitivity.
    fn lookup_prefix(&self, prefix: &[u8]) -> Option<NsRef> {
        if prefix == b"xml" {
            return Some(NsRef::Xml);
        }
        // Walk declarations in reverse (most recent first)
        self.ns_declarations
            .iter()
            .rposition(|d| d.prefix.as_bytes() == prefix)
            .map(|i| NsRef::Decl(i as u32))
    }

    fn namespace_uri(&self, ns: NsRef) -> Option<&str> {
        match ns {
            NsRef::None => None,
            NsRef::Xml => Some(XML_NAMESPACE),
            NsRef::Decl(i) => Some(self.ns_declarations[i as usize].namespace_uri.as_str()),
        }
    }

    fn resolve_element_namespace(
        &self,
        prefix: Option<&[u8]>,
        name_span: Span,
    ) -> Result<NsRef, Error> {
        match prefix {
            // For elements, empty prefix means look up the default namespace (xmlns="..."), where
            // xmlns="" means no namespace
            None => Ok(match self.default_ns {
                Some(i) if !self.ns_declarations[i as usize].namespace_uri.is_empty() => {
                    NsRef::Decl(i)
                }
                _ => NsRef::None,
            }),
            Some(p) => self
                .lookup_prefix(p)
                .ok_or_else(|| undefined_or_invalid_prefix(p, name_span)),
        }
    }

    fn resolve_attribute_namespace(
        &self,
        prefix: Option<&[u8]>,
        name_span: Span,
    ) -> Result<NsRef, Error> {
        match prefix {
            // Per XML Namespaces spec, unprefixed attributes have no namespace
            None => Ok(NsRef::None),
            Some(p) => self
                .lookup_prefix(p)
                .ok_or_else(|| undefined_or_invalid_prefix(p, name_span)),
        }
    }

    fn push(&mut self, name: QName<'_>) -> Result<(), Error> {
        if self.scopes.len() >= MAX_ELEMENT_DEPTH {
            let span = name.span();
            return Err(Error::ElementTooDeep {
                span: span.start..span.end,
            });
        }
        let prefix = name.prefix().map(|p| push_name(&mut self.names, p));
        let local = push_name(&mut self.names, name.local_name());
        self.scopes.push(ElementScope {
            prefix,
            local,
            name_span: name.span(),
            ns_decl_start: self.ns_declarations.len() as u32,
            outer_default_ns: self.default_ns,
        });
        Ok(())
    }

    /// Checks that `name` matches the name of the innermost open element, as the
    /// element type match constraint requires.
    fn check_end_tag(&self, name: QName<'_>) -> Result<(), Error> {
        let open = self.scopes.last();
        if let Some(scope) = open
            && scope.prefix.map(|r| get_name(&self.names, r)) == name.prefix()
            && get_name(&self.names, scope.local) == name.local_name()
        {
            return Ok(());
        }
        let span = name.span();
        Err(Error::MismatchedEndTag {
            span: span.start..span.end,
            start: open.map(|s| s.name_span.start..s.name_span.end),
        })
    }

    fn pop(&mut self) {
        let scope = self.scopes.pop().unwrap();
        let names_start = scope.prefix.unwrap_or(scope.local).0;
        self.names.truncate(names_start as usize);
        if (scope.ns_decl_start as usize) < self.ns_declarations.len() {
            self.ns_declarations.truncate(scope.ns_decl_start as usize);
            self.default_ns = scope.outer_default_ns;
        }
    }

    /// Validates prefix UTF-8, upholding the invariant that every stored prefix is valid
    /// UTF-8, so lookups can compare bytes without revalidating.
    fn add_namespace(
        &mut self,
        prefix_bytes: &[u8],
        prefix_span: Span,
        uri: String,
    ) -> Result<(), Error> {
        // TODO: reject forbidden declarations: an empty URI for a prefix (xmlns:p=""),
        //       binding `xml` to any other URI, binding xmlns at all, and binding any
        //       other prefix (or the default namespace) to the xml/xmlns namespace
        //       URIs
        let prefix = if prefix_bytes.is_empty() {
            String::new()
        } else {
            std::str::from_utf8(prefix_bytes)
                .map_err(|_| Error::InvalidUtf8 {
                    span: prefix_span.start..prefix_span.end,
                    kind: "namespace prefix",
                })?
                .to_owned()
        };
        if prefix.is_empty() {
            self.default_ns = Some(self.ns_declarations.len() as u32);
        }
        self.ns_declarations.push(NsDecl {
            prefix,
            namespace_uri: uri,
        });
        Ok(())
    }

    /// Number of namespace declarations on the current element.
    fn current_ns_count(&self) -> usize {
        let start = self.scopes.last().unwrap().ns_decl_start as usize;
        self.ns_declarations.len() - start
    }

    fn scope(&self) -> NamespaceScope<'_> {
        NamespaceScope { stack: self }
    }
}

/// Cold-path classifier for a prefix lookup miss: decide whether the bytes
/// are invalid UTF-8 (→ `InvalidUtf8`) or just an undefined prefix.
#[cold]
fn undefined_or_invalid_prefix(p: &[u8], name_span: Span) -> Error {
    match std::str::from_utf8(p) {
        Ok(s) => Error::UndefinedNamespacePrefix {
            prefix: s.to_owned(),
            span: name_span.start..name_span.end,
        },
        Err(_) => Error::InvalidUtf8 {
            span: name_span.start..name_span.end,
            kind: "namespace prefix",
        },
    }
}

fn decode_char_ref(value: &[u8]) -> Result<char, ()> {
    let s = std::str::from_utf8(value).map_err(|_| ())?;
    let n = if let Some(hex) = s.strip_prefix('x') {
        u32::from_str_radix(hex, 16).map_err(|_| ())?
    } else {
        s.parse::<u32>().map_err(|_| ())?
    };
    char::from_u32(n).ok_or(())
}

/// Document state, kept apart from the handler so that events can borrow it while the handler
/// is borrowed mutably.
struct State {
    stack: ElementStack,
    arena: TagArena,
    /// Attributes of the start tag being read, excluding namespace declarations.
    attrs: Vec<Attr>,
    // Attribute accumulation state — names and value stored in the arena
    current_attr_prefix: Option<NameRange>,
    current_attr_local: NameRange,
    current_attr_name_span: Span,
    current_attr_value_start: u32,
    /// Whether text has been passed to the handler since the last `end_text`.
    in_text: bool,
    /// Whether the root element has been closed.
    root_closed: bool,
}

impl State {
    fn new() -> Self {
        State {
            stack: ElementStack::default(),
            arena: TagArena::default(),
            attrs: Vec::new(),
            current_attr_prefix: None,
            current_attr_local: (0, 0),
            current_attr_name_span: Span::new(0, 0),
            current_attr_value_start: 0,
            in_text: false,
            root_closed: false,
        }
    }

    /// Returns to the state at the start of a document, keeping allocations for reuse.
    fn reset(&mut self) {
        self.stack.clear();
        self.arena.clear();
        self.attrs.clear();
        self.in_text = false;
        self.root_closed = false;
    }

    fn push_attribute_value<E>(&mut self, data: &[u8]) -> Result<(), Failure<E>> {
        self.arena
            .push_value(self.current_attr_value_start, data)
            .map_err(|()| attribute_value_too_long(self.current_attr_name_span))
    }
}

fn attribute_value_too_long<E>(name_span: Span) -> Failure<E> {
    Failure::Infoset(Error::AttributeValueTooLong {
        span: name_span.start..name_span.end,
    })
}

fn predefined_entity(name: &[u8]) -> Option<&'static str> {
    match name {
        b"lt" => Some("<"),
        b"gt" => Some(">"),
        b"amp" => Some("&"),
        b"apos" => Some("'"),
        b"quot" => Some("\""),
        _ => None,
    }
}

/// Turns tokenizer events into [`Handler`] events. The document state is borrowed rather than
/// owned so that it can outlive the driver, which lets [`PushParser`] build a driver per chunk.
struct Driver<'s, 'h, H> {
    state: &'s mut State,
    handler: &'h mut H,
}

impl<'s, 'h, H: Handler> Driver<'s, 'h, H> {
    fn new(state: &'s mut State, handler: &'h mut H) -> Self {
        Driver { state, handler }
    }

    fn end_text(&mut self) -> Result<(), Failure<H::Error>> {
        if !self.state.in_text {
            return Ok(());
        }
        self.state.in_text = false;
        self.handler
            .end_text(&self.state.stack.scope())
            .map_err(Failure::Handler)
    }

    fn text(&mut self, text: &str, span: Span) -> Result<(), Failure<H::Error>> {
        self.state.in_text = true;
        self.handler
            .text(&self.state.stack.scope(), text, span)
            .map_err(Failure::Handler)
    }

    /// Checks that `bytes` are UTF-8 and passes them on as text.
    fn text_bytes(
        &mut self,
        bytes: &[u8],
        span: Span,
        kind: &'static str,
    ) -> Result<(), Failure<H::Error>> {
        let s = std::str::from_utf8(bytes).map_err(|_| {
            Failure::Infoset(Error::InvalidUtf8 {
                span: span.start..span.end,
                kind,
            })
        })?;
        self.text(s, span)
    }

    /// The tag has been read in full: resolve its names and hand it over.
    fn finish_start_tag(&mut self, close_span: Span, empty: bool) -> Result<(), Failure<H::Error>> {
        let state = &mut self.state;
        let scope = state.stack.scopes.last().unwrap();
        let elem_prefix = scope.prefix.map(|r| get_name(&state.stack.names, r));
        let elem_ns = state
            .stack
            .resolve_element_namespace(elem_prefix, scope.name_span)
            .map_err(Failure::Infoset)?;
        for attr in &mut state.attrs {
            let prefix = attr.prefix.map(|r| state.arena.get_name(r));
            attr.namespace = state
                .stack
                .resolve_attribute_namespace(prefix, attr.name_span)
                .map_err(Failure::Infoset)?;
        }
        // No two attributes may have the same local name and namespace, even if their prefixes
        // differ (the Attributes Unique constraint of Namespaces in XML)
        for (i, attr) in state.attrs.iter().enumerate() {
            let local = state.arena.get_name(attr.local);
            let ns = state.stack.namespace_uri(attr.namespace);
            if state.attrs[..i].iter().any(|a| {
                state.arena.get_name(a.local) == local
                    && state.stack.namespace_uri(a.namespace) == ns
            }) {
                return Err(Failure::Infoset(Error::DuplicateAttribute {
                    span: attr.name_span.start..attr.name_span.end,
                }));
            }
        }

        let state = &self.state;
        let scope = state.stack.scopes.last().unwrap();
        let element = StartElement {
            name: ExpandedName {
                namespace_uri: state.stack.namespace_uri(elem_ns),
                local_name: Name::new(get_name(&state.stack.names, scope.local)),
            },
            name_span: scope.name_span,
            close_span,
            empty,
            attrs: &state.attrs,
            arena: &state.arena,
            stack: &state.stack,
        };
        self.handler
            .start_element(&state.stack.scope(), &element)
            .map_err(Failure::Handler)?;

        // Attribute names and values are no longer needed — reset for the next start tag
        self.state.arena.clear();
        self.state.attrs.clear();
        if empty {
            self.close_element();
        }
        Ok(())
    }

    fn end_element(&mut self, span: Span) -> Result<(), Failure<H::Error>> {
        self.handler
            .end_element(&self.state.stack.scope(), span)
            .map_err(Failure::Handler)?;
        self.close_element();
        Ok(())
    }

    fn close_element(&mut self) {
        self.state.stack.pop();
        if self.state.stack.scopes.is_empty() {
            self.state.root_closed = true;
        }
    }

    /// Whether the parser is outside the root element, before or after it.
    fn outside_root(&self) -> bool {
        self.state.stack.scopes.is_empty()
    }
}

/// Anything but comments, processing instructions and whitespace is an error outside the root
/// element.
fn outside_root<E>(kind: &'static str, span: Span) -> Failure<E> {
    Failure::Infoset(Error::OutsideRootElement {
        span: span.start..span.end,
        kind,
    })
}

/// The text of the predefined entity `name`, referenced at `span`. Entities declared in a DTD
/// are not supported.
fn entity_text<E>(name: &[u8], span: Span) -> Result<&'static str, Failure<E>> {
    predefined_entity(name).ok_or_else(|| {
        Failure::Infoset(Error::UndefinedEntity {
            name: String::from_utf8_lossy(name).into_owned(),
            span: span.start..span.end,
        })
    })
}

fn char_ref<E>(value: &[u8], span: Span) -> Result<char, Failure<E>> {
    decode_char_ref(value).map_err(|()| {
        Failure::Infoset(Error::InvalidOrUnclosedEntity {
            span: span.start..span.end,
        })
    })
}

impl<H: Handler> Visitor for Driver<'_, '_, H> {
    type Error = Failure<H::Error>;

    fn start_tag_open(&mut self, name: QName<'_>) -> Result<(), Self::Error> {
        if self.state.root_closed && self.outside_root() {
            return Err(outside_root("element", name.span()));
        }
        self.end_text()?;
        self.state.stack.push(name).map_err(Failure::Infoset)
    }

    fn attribute_name(&mut self, name: QName<'_>) -> Result<(), Self::Error> {
        let state = &mut self.state;
        state.current_attr_prefix = name.prefix().map(|p| state.arena.push_name(p));
        state.current_attr_local = state.arena.push_name(name.local_name());
        state.current_attr_name_span = name.span();
        state.current_attr_value_start = state.arena.begin_value();
        Ok(())
    }

    fn attribute_value(&mut self, value: &[u8], span: Span) -> Result<(), Self::Error> {
        // Validate UTF-8 at the boundary so the arena's invariant holds:
        // every byte in an attribute-value range is valid UTF-8.
        std::str::from_utf8(value).map_err(|_| {
            Failure::Infoset(Error::InvalidUtf8 {
                span: span.start..span.end,
                kind: "attribute value",
            })
        })?;
        self.state.push_attribute_value(value)
    }

    fn attribute_entity_ref(&mut self, name: &[u8], span: Span) -> Result<(), Self::Error> {
        let text = entity_text(name, span)?;
        self.state.push_attribute_value(text.as_bytes())
    }

    fn attribute_char_ref(&mut self, value: &[u8], span: Span) -> Result<(), Self::Error> {
        let c = char_ref(value, span)?;
        self.state
            .push_attribute_value(c.encode_utf8(&mut [0; 4]).as_bytes())
    }

    fn attribute_end(&mut self, _span: Span) -> Result<(), Self::Error> {
        let state = &mut self.state;
        let prefix = state.current_attr_prefix.take();
        let local = state.current_attr_local;
        let value = state.arena.finish_value(state.current_attr_value_start);
        let name_span = state.current_attr_name_span;

        // Check for namespace declarations using arena data
        let is_xmlns_prefix = prefix.is_some_and(|r| state.arena.get_name(r) == b"xmlns");
        let is_default_xmlns = prefix.is_none() && state.arena.get_name(local) == b"xmlns";

        let infoset = Failure::Infoset;

        // Enforce attribute count limit
        if state.attrs.len() + state.stack.current_ns_count() >= MAX_ATTRIBUTES {
            return Err(infoset(Error::TooManyAttributes {
                span: name_span.start..name_span.end,
            }));
        }

        // Duplicate attributes are checked once the start tag is complete and their prefixes can
        // be resolved; duplicate namespace declarations (xmlns:foo vs xmlns:foo) are checked here
        let new_local = state.arena.get_name(local);
        if is_xmlns_prefix || is_default_xmlns {
            let ns_start = state.stack.scopes.last().unwrap().ns_decl_start as usize;
            for decl in &state.stack.ns_declarations[ns_start..] {
                let matches = if is_xmlns_prefix {
                    decl.prefix.as_bytes() == new_local
                } else {
                    decl.prefix.is_empty()
                };
                if matches {
                    return Err(infoset(Error::DuplicateAttribute {
                        span: name_span.start..name_span.end,
                    }));
                }
            }
        }

        if is_xmlns_prefix || is_default_xmlns {
            let prefix_bytes: &[u8] = if is_xmlns_prefix {
                state.arena.get_name(local)
            } else {
                b""
            };
            let uri = state.arena.get_value(value).to_owned();
            state
                .stack
                .add_namespace(prefix_bytes, name_span, uri)
                .map_err(infoset)?;
        } else {
            state.attrs.push(Attr {
                prefix,
                local,
                name_span,
                value,
                namespace: NsRef::None,
            });
        }
        Ok(())
    }

    fn start_tag_close(&mut self, span: Span) -> Result<(), Self::Error> {
        self.finish_start_tag(span, false)
    }

    fn empty_element_end(&mut self, span: Span) -> Result<(), Self::Error> {
        self.finish_start_tag(span, true)
    }

    fn end_tag(&mut self, name: QName<'_>) -> Result<(), Self::Error> {
        self.state
            .stack
            .check_end_tag(name)
            .map_err(Failure::Infoset)?;
        self.end_text()?;
        self.end_element(name.span())
    }

    fn characters(&mut self, text: &[u8], span: Span) -> Result<(), Self::Error> {
        if self.outside_root() {
            // Whitespace here is not part of the document's content, so is not passed on
            return if is_all_xml_whitespace(text) {
                Ok(())
            } else {
                Err(outside_root("text", span))
            };
        }
        self.text_bytes(text, span, "text")
    }

    fn entity_ref(&mut self, name: &[u8], span: Span) -> Result<(), Self::Error> {
        if self.outside_root() {
            return Err(outside_root("reference", span));
        }
        let text = entity_text(name, span)?;
        self.text(text, span)
    }

    fn char_ref(&mut self, value: &[u8], span: Span) -> Result<(), Self::Error> {
        if self.outside_root() {
            return Err(outside_root("reference", span));
        }
        let c = char_ref(value, span)?;
        let mut buf = [0u8; 4];
        let s = c.encode_utf8(&mut buf);
        self.text(s, span)
    }

    fn cdata_start(&mut self, span: Span) -> Result<(), Self::Error> {
        if self.outside_root() {
            return Err(outside_root("CDATA section", span));
        }
        Ok(())
    }

    fn cdata_content(&mut self, text: &[u8], span: Span) -> Result<(), Self::Error> {
        self.text_bytes(text, span, "CDATA content")
    }

    fn end_document(&mut self) -> Result<(), Self::Error> {
        if let Some(scope) = self.state.stack.scopes.last() {
            let span = scope.name_span;
            return Err(Failure::Infoset(Error::UnclosedElement {
                span: span.start..span.end,
            }));
        }
        if !self.state.root_closed {
            return Err(Failure::Infoset(Error::MissingRootElement));
        }
        Ok(())
    }
}
