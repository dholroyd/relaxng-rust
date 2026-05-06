mod coverage;
mod derivative;
mod nameclass;
mod schema;

use relaxng_model::model;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::rc::Rc;
use xml_syntax_reader::{EntityKind, ErrorKind, ParseError, QName, Span, Visitor};

pub use coverage::{CoverageReport, TrackablePattern};
use nameclass::{is_ncname, *};
use schema::*;

#[derive(Debug)]
pub enum ValidatorError {
    /// XML well-formedness error from the parser.
    Xml(xml_syntax_reader::Error),
    /// The XML construct at the given span is not allowed by the schema.
    NotAllowed {
        span: Range<u64>,
        kind: &'static str,
    },
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
    /// Duplicate attribute on the same element.
    DuplicateAttribute {
        span: Range<u64>,
    },
    /// Too many attributes on a single element.
    TooManyAttributes {
        span: Range<u64>,
    },
    /// Bytes that should be UTF-8 (e.g. a namespace prefix) were not valid UTF-8.
    InvalidUtf8 {
        span: Range<u64>,
        kind: &'static str,
    },
    TextBufferOverflow,
    Io(std::io::Error),
    Schema(SchemaError),
}

impl std::fmt::Display for ValidatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValidatorError::Xml(e) => write!(f, "XML error: {e:?}"),
            ValidatorError::NotAllowed { kind, .. } => write!(f, "{kind} not expected here"),
            ValidatorError::UndefinedNamespacePrefix { prefix, .. } => {
                write!(f, "The prefix \"{prefix}\" is not defined")
            }
            ValidatorError::UndefinedEntity { name, .. } => {
                write!(f, "The entity &{name}; is not defined")
            }
            ValidatorError::InvalidOrUnclosedEntity { .. } => {
                write!(f, "Invalid or unclosed entity reference")
            }
            ValidatorError::InvalidName { kind, .. } => {
                write!(f, "{kind} name is not a valid XML Name")
            }
            ValidatorError::DuplicateAttribute { .. } => {
                write!(f, "duplicate attribute")
            }
            ValidatorError::TooManyAttributes { .. } => {
                write!(f, "too many attributes on element (limit: 256)")
            }
            ValidatorError::InvalidUtf8 { kind, .. } => {
                write!(f, "{kind} contains invalid UTF-8")
            }
            ValidatorError::TextBufferOverflow => {
                write!(f, "Text content exceeds maximum buffer size")
            }
            ValidatorError::Io(e) => write!(f, "I/O error: {e}"),
            ValidatorError::Schema(SchemaError::TooManyPatterns) => {
                write!(f, "Schema exceeds maximum number of patterns (65535)")
            }
        }
    }
}

/// Arena for element/attribute names and attribute values within a single
/// opening tag. All data is appended to one `Vec<u8>` and referenced by
/// `(start, len)` ranges. Reset after `close_element_start` consumes everything.
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

/// Range into the `TagArena` for a short name (max 65535 bytes).
type NameRange = (u32, u16);
/// Range into the `TagArena` for an attribute value (may be longer).
type ValueRange = (u32, u32);

impl TagArena {
    /// Append bytes and return a name-sized range.
    fn push_name(&mut self, name: &[u8]) -> NameRange {
        let start = self.buf.len() as u32;
        self.buf.extend_from_slice(name);
        (start, name.len() as u16)
    }

    /// Mark the start of an attribute value being accumulated.
    fn begin_value(&self) -> u32 {
        self.buf.len() as u32
    }

    /// Append bytes to the current attribute value.
    fn push_value_bytes(&mut self, data: &[u8]) {
        self.buf.extend_from_slice(data);
    }

    /// Append a single char (for entity/char ref expansion).
    fn push_value_char(&mut self, c: char) {
        let mut tmp = [0u8; 4];
        let s = c.encode_utf8(&mut tmp);
        self.buf.extend_from_slice(s.as_bytes());
    }

    /// Append a str to the current attribute value.
    fn push_value_str(&mut self, s: &str) {
        self.buf.extend_from_slice(s.as_bytes());
    }

    /// Finish an attribute value and return its range.
    fn finish_value(&self, start: u32) -> ValueRange {
        (start, self.buf.len() as u32)
    }

    /// Get the raw bytes for a name range.
    fn get_name(&self, range: NameRange) -> &[u8] {
        let start = range.0 as usize;
        let end = start + range.1 as usize;
        &self.buf[start..end]
    }

    /// Get a `&str` for a value range. Infallible by construction: every
    /// `push_value_*` call site contributes valid UTF-8 (validated at the
    /// `attribute_value` / `attribute_entity_ref` visitor boundaries, or
    /// produced by `c.encode_utf8` / a `&str` source). Concatenation of
    /// valid UTF-8 is valid UTF-8.
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

pub struct Validator {
    schema: Schema,
    current_step: PatId,
    last_was_start_element: bool,
    stack: ElementStack,
    entity_definitions: HashMap<String, String>,
    /// Text content buffered for deferred `text_deriv`. UTF-8 is enforced at
    /// the visitor boundary (`append_text_bytes`); pre-validated callers go
    /// through `append_validated_text`.
    text_buffer: String,
    /// Source span covering the buffered text run, used for diagnostics on
    /// `flush_text` failure. `None` when the buffer is empty.
    text_buffer_span: Option<Span>,
    max_text_buffer: usize,
    arena: TagArena,
    // Attribute accumulation state — names and value stored in the arena
    current_attr_prefix: Option<NameRange>,
    current_attr_local: NameRange,
    current_attr_name_span: Span,
    current_attr_value_start: u32,
}

impl Validator {
    pub fn new(model: Rc<RefCell<Option<model::DefineRule>>>) -> Result<Validator, ValidatorError> {
        let (schema, start) = build_schema(&model).map_err(ValidatorError::Schema)?;
        let mut entity_definitions = HashMap::default();
        entity_definitions.insert("lt".to_string(), "<".to_string());
        entity_definitions.insert("gt".to_string(), ">".to_string());
        entity_definitions.insert("amp".to_string(), "&".to_string());
        entity_definitions.insert("apos".to_string(), "'".to_string());
        entity_definitions.insert("quot".to_string(), "\"".to_string());
        Ok(Validator {
            schema,
            current_step: start,
            last_was_start_element: false,
            stack: ElementStack::default(),
            entity_definitions,
            text_buffer: String::new(),
            text_buffer_span: None,
            max_text_buffer: 1024 * 1024,
            arena: TagArena::default(),
            current_attr_prefix: None,
            current_attr_local: (0, 0),
            current_attr_name_span: Span::new(0, 0),
            current_attr_value_start: 0,
        })
    }

    /// Create a validator with coverage tracking enabled.
    pub fn new_with_coverage(
        model: Rc<RefCell<Option<model::DefineRule>>>,
    ) -> Result<Validator, ValidatorError> {
        let mut v = Self::new(model)?;
        let compile_time_count = v.schema.inner.borrow().patterns.len() as u16;
        let word_count = (compile_time_count as usize).div_ceil(64);
        v.schema.compile_time_count = compile_time_count;
        v.schema.coverage = Some(vec![0u64; word_count].into_boxed_slice());
        Ok(v)
    }

    /// Validate an XML document already in memory.
    pub fn validate(&mut self, source: &[u8]) -> Result<(), ValidatorError> {
        let mut reader = xml_syntax_reader::Reader::new();
        reader.parse_slice(source, self).map_err(map_parse_error)?;
        self.finish_validation()
    }

    /// Validate XML from a streaming reader, without loading the entire document into memory.
    pub fn validate_reader<R: std::io::Read>(
        &mut self,
        mut reader: R,
    ) -> Result<(), ValidatorError> {
        let mut xml_reader = xml_syntax_reader::Reader::new();
        let mut buf = vec![0u8; 65536];
        let mut valid: usize = 0;
        let mut stream_offset: u64 = 0;

        loop {
            let n = reader.read(&mut buf[valid..]).map_err(ValidatorError::Io)?;
            valid += n;
            let is_final = n == 0;

            let consumed = xml_reader
                .parse(&buf[..valid], stream_offset, is_final, self)
                .map_err(map_parse_error)?;

            let consumed = consumed as usize;
            let leftover = valid - consumed;
            if leftover > 0 {
                buf.copy_within(consumed..valid, 0);
            }
            valid = leftover;
            stream_offset += consumed as u64;

            if is_final && consumed == 0 {
                break;
            }
        }

        self.finish_validation()
    }

    fn finish_validation(&mut self) -> Result<(), ValidatorError> {
        self.flush_text()
    }

    /// Extract the coverage report. Returns `None` if coverage tracking was not enabled.
    pub fn coverage_report(&self) -> Option<CoverageReport> {
        self.schema.build_coverage_report()
    }

    /// Ensure ns_context is up to date if it has been invalidated.
    fn ensure_ns_context(&mut self) {
        if self.schema.ns_context_dirty {
            self.schema.ns_context = if self.stack.scopes.is_empty() {
                None
            } else {
                Some(self.stack.capture_ns_context())
            };
            self.schema.ns_context_dirty = false;
        }
    }

    /// Flush any buffered text by applying `text_deriv`. On `NotAllowed`,
    /// returns a `text` error tagged with the buffered run's span.
    fn flush_text(&mut self) -> Result<(), ValidatorError> {
        if self.text_buffer.is_empty() {
            return Ok(());
        }
        let next_id = self.schema.text_deriv(self.current_step, &self.text_buffer);
        let buf_span = self.text_buffer_span.take();
        self.text_buffer.clear();
        self.current_step = next_id;
        if matches!(self.schema.patt(next_id), Pat::NotAllowed) {
            let span = buf_span.map(|s| s.start..s.end).unwrap_or(0..0);
            return Err(ValidatorError::NotAllowed { span, kind: "text" });
        }
        Ok(())
    }

    /// Append a text fragment whose bytes came directly from the parser.
    /// Validates UTF-8 (per fragment, so the diagnostic span is precise) and
    /// then either skips storage on the fixed-point branch or buffers for
    /// deferred `text_deriv`.
    fn append_text_bytes(
        &mut self,
        bytes: &[u8],
        span: Span,
        kind: &'static str,
    ) -> Result<(), ValidatorError> {
        // Any text content (even fixed-point) means this element is not empty,
        // so clear the flag that would trigger an empty-text derivative on close.
        self.last_was_start_element = false;
        // Validate UTF-8 unconditionally — the validator rejects malformed
        // documents even when the schema's pattern accepts arbitrary text
        // (e.g. `text` / mixed content). The fixed-point fast path below skips
        // the buffer copy and the `text_deriv` walk, not the UTF-8 check.
        let s = std::str::from_utf8(bytes).map_err(|_| ValidatorError::InvalidUtf8 {
            span: span.start..span.end,
            kind,
        })?;
        if self.schema.mixed_text_deriv(self.current_step) == self.current_step {
            return Ok(());
        }
        self.append_str(s, span)
    }

    /// Append text whose bytes are already known to be valid UTF-8 (an
    /// expanded entity-reference value or an encoded character reference).
    fn append_validated_text(&mut self, s: &str, span: Span) -> Result<(), ValidatorError> {
        self.last_was_start_element = false;
        if self.schema.mixed_text_deriv(self.current_step) == self.current_step {
            return Ok(());
        }
        self.append_str(s, span)
    }

    fn append_str(&mut self, s: &str, span: Span) -> Result<(), ValidatorError> {
        // Buffer text for deferred processing — text fragments split by
        // PIs/comments must be concatenated before validation (spec 6.2.7).
        self.text_buffer.push_str(s);
        if self.text_buffer.len() > self.max_text_buffer {
            return Err(ValidatorError::TextBufferOverflow);
        }
        match self.text_buffer_span {
            None => self.text_buffer_span = Some(span),
            Some(ref mut existing) if span.end > existing.end => existing.end = span.end,
            Some(_) => {}
        }
        Ok(())
    }

    fn close_element_start(&mut self, span: Span) -> Result<(), ValidatorError> {
        self.ensure_ns_context();

        // Resolve element name — prefix/local borrow from the name arena
        let curr = self.stack.scopes.last().unwrap();
        let elem_name_span = curr.name_span;
        let elem_prefix_bytes = curr.prefix.map(|r| self.arena.get_name(r));
        let elem_ns = self
            .stack
            .resolve_element_namespace(elem_prefix_bytes, elem_name_span)?;

        let elem_qname = QualifiedName {
            namespace_uri: elem_ns,
            local_name: self.arena.get_name(self.stack.scopes.last().unwrap().local),
        };

        let pat_id = self.current_step;
        let next_pat = self.schema.start_tag_open_deriv(pat_id, elem_qname);
        if matches!(self.schema.patt(next_pat), Pat::NotAllowed) {
            let err = if !is_ncname(elem_qname.local_name) {
                ValidatorError::InvalidName {
                    span: elem_name_span.start..elem_name_span.end,
                    kind: "element",
                }
            } else {
                ValidatorError::NotAllowed {
                    span: elem_name_span.start..elem_name_span.end,
                    kind: "element-start",
                }
            };
            self.arena.clear();
            return Err(err);
        }

        // Process attributes — names borrow from the arena, attrs in flat buffer
        let mut pat = next_pat;
        let attr_start = self.stack.scopes.last().unwrap().attr_start as usize;
        let num_attrs = self.stack.attrs.len() - attr_start;
        for i in 0..num_attrs {
            let attr = &self.stack.attrs[attr_start + i];
            let attr_name_span = attr.name_span;
            let attr_prefix_bytes = attr.prefix.map(|r| self.arena.get_name(r));
            let attr_ns = self
                .stack
                .resolve_attribute_namespace(attr_prefix_bytes, attr_name_span)?;
            let attr_qname = QualifiedName {
                namespace_uri: attr_ns,
                local_name: self.arena.get_name(self.stack.attrs[attr_start + i].local),
            };
            let mid = self.schema.start_att_deriv(pat, attr_qname);
            let attr_value = self.arena.get_value(self.stack.attrs[attr_start + i].value);
            pat = self.schema.att_value_deriv(mid, attr_value);
            if matches!(self.schema.patt(pat), Pat::NotAllowed) {
                let err = if !is_ncname(attr_qname.local_name) {
                    ValidatorError::InvalidName {
                        span: attr_name_span.start..attr_name_span.end,
                        kind: "attribute",
                    }
                } else {
                    ValidatorError::NotAllowed {
                        span: attr_name_span.start..attr_name_span.end,
                        kind: "attribute",
                    }
                };
                self.arena.clear();
                return Err(err);
            }
        }

        // Names are no longer needed — reset arena for the next opening tag
        self.arena.clear();

        if matches!(self.schema.patt(pat), Pat::NotAllowed) {
            return Err(ValidatorError::NotAllowed {
                span: span.start..span.end,
                kind: "element-end",
            });
        }
        let next_pat = self.schema.start_tag_close_deriv(pat);
        if matches!(self.schema.patt(next_pat), Pat::NotAllowed) {
            return Err(ValidatorError::NotAllowed {
                span: span.start..span.end,
                kind: "element-end",
            });
        }
        self.current_step = next_pat;
        self.last_was_start_element = true;
        Ok(())
    }

    fn handle_end_tag(&mut self, span: Span) -> Result<(), ValidatorError> {
        self.ensure_ns_context();
        self.flush_text()?;
        let next_pid = if self.last_was_start_element {
            // The last event was XmlEvent::StartElement with no child elements or child
            // text nodes.
            //
            // Per https://relaxng.org/jclark/derivative.html ,
            //     "The case where the list of children is empty is
            //      treated as if there were a text node whose value
            //      were the empty string."
            //
            // This fake text node is required for a pattern like 'element foo { token }'
            // to match the input '<foo/>' or '<foo></foo>'
            self.schema.text_deriv(self.current_step, "")
        } else {
            self.current_step
        };
        let result = self.schema.end_tag_deriv(next_pid);
        if self.stack.pop_has_namespaces() {
            self.schema.ns_context_dirty = true;
        }
        if matches!(self.schema.patt(result), Pat::NotAllowed) {
            return Err(ValidatorError::NotAllowed {
                span: span.start..span.end,
                kind: "element-end",
            });
        }
        self.current_step = result;
        self.last_was_start_element = false;
        Ok(())
    }

    fn handle_empty_element_end(&mut self, span: Span) -> Result<(), ValidatorError> {
        self.close_element_start(span)?;
        // The last event was XmlEvent::StartElement with no child elements or child
        // text nodes.
        //
        // Per https://relaxng.org/jclark/derivative.html ,
        //     "The case where the list of children is empty is
        //      treated as if there were a text node whose value
        //      were the empty string."
        //
        // This fake text node is required for a pattern like 'element foo { token }'
        // to match the input '<foo/>' or '<foo></foo>'
        let p = self.schema.text_deriv(self.current_step, "");
        let result = self.schema.end_tag_deriv(p);
        if self.stack.pop_has_namespaces() {
            self.schema.ns_context_dirty = true;
        }
        if matches!(self.schema.patt(result), Pat::NotAllowed) {
            return Err(ValidatorError::NotAllowed {
                span: span.start..span.end,
                kind: "element-end",
            });
        }
        self.current_step = result;
        self.last_was_start_element = false;
        Ok(())
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

    #[allow(clippy::mutable_key_type)]
    fn heads(&self, id: PatId) -> HashSet<Pat> {
        let mut result = HashSet::new();
        self.head(&mut result, id);
        result
    }
    #[allow(clippy::mutable_key_type)]
    fn head(&self, result: &mut HashSet<Pat>, p: PatId) {
        // https://www.kohsuke.org/relaxng/implbook/Validation1.html#IDATGOO
        let pat = self.schema.patt(p);
        match pat {
            Pat::Choice(p1, p2, _) => {
                self.head(result, p1);
                self.head(result, p2);
            }
            Pat::Interleave(p1, p2, _) => {
                self.head(result, p1);
                self.head(result, p2);
            }
            Pat::Group(p1, p2, _) => {
                if self.schema.patt(p1).is_nullable() {
                    self.head(result, p1);
                } else {
                    self.head(result, p1);
                    self.head(result, p2);
                }
            }
            Pat::OneOrMore(p, _) => self.head(result, p),
            Pat::Empty => {}
            Pat::Text => {}
            Pat::NotAllowed => {}
            Pat::Attribute(_, _) => {
                result.insert(pat);
            }
            Pat::Element(_, _) => {
                result.insert(pat);
            }
            Pat::Datatype(_) => {
                result.insert(pat);
            }
            Pat::DatatypeValue(_) => {
                result.insert(pat);
            }
            Pat::DatatypeExcept(_, _) => {
                result.insert(pat);
            }
            Pat::List(p) => self.head(result, p),
            Pat::After(p, _) => self.head(result, p),
        }
    }

    fn describe_expected(&self, expected_patt: PatId) -> String {
        #[allow(clippy::mutable_key_type)]
        let heads = self.heads(expected_patt);
        let mut result = String::new();
        const MAX_ELEMENTS: usize = 4;
        let mut rest = 0;
        for (i, nameclass) in heads
            .iter()
            .filter_map(|p| {
                if let Pat::Element(nameclass, _) = p {
                    Some(nameclass)
                } else {
                    None
                }
            })
            .enumerate()
        {
            if i == 0 {
                result.push_str("Element ");
            }
            if i >= MAX_ELEMENTS {
                rest += 1;
            } else {
                if i > 0 {
                    result.push(' ');
                }
                // TODO: also provide namespace information; grouping by namespace to make the
                //       information more succinct
                let mut desc = String::new();
                describe_nameclass(nameclass, &mut desc);
                result.push_str(&desc);
            }
        }
        if rest > 0 {
            result.push_str(&format!(" .. or one of {rest} more"))
        }
        // TODO: plus attributes and everything else
        result
    }

    pub fn diagnostic(
        &self,
        name: String,
        source: &[u8],
        err: &ValidatorError,
    ) -> (codemap::CodeMap, Vec<codemap_diagnostic::Diagnostic>) {
        let mut map = codemap::CodeMap::new();
        let source_str = String::from_utf8_lossy(source).into_owned();
        let file = map.add_file(name, source_str);
        let mut diagnostics = vec![];
        match err {
            ValidatorError::Xml(err) => {
                let offset = err.offset;
                let span = file.span.subspan(offset as _, offset as _);

                let label = codemap_diagnostic::SpanLabel {
                    span,
                    label: None,
                    style: codemap_diagnostic::SpanStyle::Primary,
                };

                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("{err:?}"),
                    code: None,
                    spans: vec![label],
                });
            }
            ValidatorError::NotAllowed { span, kind } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(span.start as _, span.end as _),
                    label: Some("Not allowed".to_string()),
                    style: codemap_diagnostic::SpanStyle::Primary,
                };

                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("{kind} not expected here"),
                    code: None,
                    spans: vec![label],
                });

                let desc = self.describe_expected(self.current_step);
                let message = if desc.is_empty() {
                    "Remove this".to_string()
                } else {
                    format!("Expected {desc}")
                };
                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Help,
                    message,
                    code: None,
                    spans: vec![],
                });
            }
            ValidatorError::UndefinedNamespacePrefix { prefix, span } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(span.start as _, span.end as _),
                    label: Some(format!(
                        "Add an xmlns:{prefix}=\"..\" attribute to define this prefix"
                    )),
                    style: codemap_diagnostic::SpanStyle::Primary,
                };

                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("The prefix \"{prefix}\" is not defined"),
                    code: None,
                    spans: vec![label],
                });
            }
            ValidatorError::UndefinedEntity { name, span } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(span.start as _, span.end as _),
                    label: Some("undefined".to_string()),
                    style: codemap_diagnostic::SpanStyle::Primary,
                };

                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("The entity &{name}; is not defined"),
                    code: None,
                    spans: vec![label],
                });
            }
            ValidatorError::InvalidOrUnclosedEntity { span } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(span.start as _, span.end as _),
                    label: None,
                    style: codemap_diagnostic::SpanStyle::Primary,
                };

                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: "Invalid or unclosed entity reference".to_string(),
                    code: None,
                    spans: vec![label],
                });
            }
            ValidatorError::InvalidName { span, kind } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(span.start as _, span.end as _),
                    label: Some("not a valid XML Name".to_string()),
                    style: codemap_diagnostic::SpanStyle::Primary,
                };
                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("{kind} name is not a valid XML Name"),
                    code: None,
                    spans: vec![label],
                });
            }
            ValidatorError::DuplicateAttribute { span } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(span.start as _, span.end as _),
                    label: Some("duplicate".to_string()),
                    style: codemap_diagnostic::SpanStyle::Primary,
                };
                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: "duplicate attribute".to_string(),
                    code: None,
                    spans: vec![label],
                });
            }
            ValidatorError::TooManyAttributes { span } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(span.start as _, span.end as _),
                    label: None,
                    style: codemap_diagnostic::SpanStyle::Primary,
                };
                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: "too many attributes on element (limit: 256)".to_string(),
                    code: None,
                    spans: vec![label],
                });
            }
            ValidatorError::InvalidUtf8 { span, kind } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(span.start as _, span.end as _),
                    label: Some("invalid UTF-8".to_string()),
                    style: codemap_diagnostic::SpanStyle::Primary,
                };
                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("{kind} contains invalid UTF-8"),
                    code: None,
                    spans: vec![label],
                });
            }
            ValidatorError::TextBufferOverflow => {
                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: "Text content exceeds maximum buffer size".to_string(),
                    code: None,
                    spans: vec![],
                });
            }
            ValidatorError::Io(e) => {
                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("I/O error: {e}"),
                    code: None,
                    spans: vec![],
                });
            }
            ValidatorError::Schema(SchemaError::TooManyPatterns) => {
                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: "Schema exceeds maximum number of patterns (65535)".to_string(),
                    code: None,
                    spans: vec![],
                });
            }
        }
        (map, diagnostics)
    }
}

/// Cold-path classifier for a prefix lookup miss: decide whether the bytes
/// are invalid UTF-8 (→ `InvalidUtf8`) or just an undefined prefix.
#[cold]
fn undefined_or_invalid_prefix(p: &[u8], name_span: Span) -> ValidatorError {
    match std::str::from_utf8(p) {
        Ok(s) => ValidatorError::UndefinedNamespacePrefix {
            prefix: s.to_owned(),
            span: name_span.start..name_span.end,
        },
        Err(_) => ValidatorError::InvalidUtf8 {
            span: name_span.start..name_span.end,
            kind: "namespace prefix",
        },
    }
}

/// Map xml-syntax-reader parse errors to validator errors.
/// `ExpectedName` is promoted to `InvalidName` since it means the tokenizer
/// encountered an invalid name-start character.
fn map_parse_error(e: ParseError<ValidatorError>) -> ValidatorError {
    match e {
        ParseError::Xml(e) if matches!(e.kind, ErrorKind::ExpectedName(_)) => {
            let offset = e.offset;
            ValidatorError::InvalidName {
                span: offset..offset,
                kind: "element or attribute",
            }
        }
        ParseError::Xml(e) => ValidatorError::Xml(e),
        ParseError::Visitor(e) => e,
    }
}

impl Visitor for Validator {
    type Error = ValidatorError;

    fn start_tag_open(&mut self, name: QName<'_>) -> Result<(), Self::Error> {
        self.ensure_ns_context();
        self.flush_text()?;
        let prefix = name.prefix().map(|p| self.arena.push_name(p));
        let local = self.arena.push_name(name.local_name());
        let name_span = name.span();
        self.stack.push(prefix, local, name_span);
        Ok(())
    }

    fn attribute_name(&mut self, name: QName<'_>) -> Result<(), Self::Error> {
        self.current_attr_prefix = name.prefix().map(|p| self.arena.push_name(p));
        self.current_attr_local = self.arena.push_name(name.local_name());
        self.current_attr_name_span = name.span();
        self.current_attr_value_start = self.arena.begin_value();
        Ok(())
    }

    fn attribute_value(&mut self, value: &[u8], span: Span) -> Result<(), Self::Error> {
        // Validate UTF-8 at the boundary so the arena's invariant holds:
        // every byte in an attribute-value range is valid UTF-8.
        std::str::from_utf8(value).map_err(|_| ValidatorError::InvalidUtf8 {
            span: span.start..span.end,
            kind: "attribute value",
        })?;
        self.arena.push_value_bytes(value);
        Ok(())
    }

    fn attribute_entity_ref(&mut self, name: &[u8], span: Span) -> Result<(), Self::Error> {
        let name_str = std::str::from_utf8(name).map_err(|_| ValidatorError::InvalidUtf8 {
            span: span.start..span.end,
            kind: "entity name",
        })?;
        if let Some(val) = self.entity_definitions.get(name_str) {
            let val = val.clone();
            self.arena.push_value_str(&val);
            Ok(())
        } else {
            Err(ValidatorError::UndefinedEntity {
                name: name_str.to_owned(),
                span: span.start..span.end,
            })
        }
    }

    fn attribute_char_ref(&mut self, value: &[u8], span: Span) -> Result<(), Self::Error> {
        let c =
            Self::decode_char_ref(value).map_err(|()| ValidatorError::InvalidOrUnclosedEntity {
                span: span.start..span.end,
            })?;
        self.arena.push_value_char(c);
        Ok(())
    }

    fn attribute_end(&mut self, _span: Span) -> Result<(), Self::Error> {
        const MAX_ATTRIBUTES: usize = 256;

        let prefix = self.current_attr_prefix.take();
        let local = self.current_attr_local;
        let value = self.arena.finish_value(self.current_attr_value_start);
        let name_span = self.current_attr_name_span;

        // Check for namespace declarations using arena data
        let is_xmlns_prefix = prefix.is_some_and(|r| self.arena.get_name(r) == b"xmlns");
        let is_default_xmlns = prefix.is_none() && self.arena.get_name(local) == b"xmlns";

        // Enforce attribute count limit
        let total_attrs = self.stack.current_attr_count() + self.stack.current_ns_count();
        if total_attrs >= MAX_ATTRIBUTES {
            return Err(ValidatorError::TooManyAttributes {
                span: name_span.start..name_span.end,
            });
        }

        // Check for duplicate attributes (comparing raw name bytes in the arena)
        let new_prefix = prefix.map(|r| self.arena.get_name(r));
        let new_local = self.arena.get_name(local);
        let attr_start = self.stack.scopes.last().unwrap().attr_start as usize;
        for attr in &self.stack.attrs[attr_start..] {
            let existing_prefix = attr.prefix.map(|r| self.arena.get_name(r));
            let existing_local = self.arena.get_name(attr.local);
            if existing_prefix == new_prefix && existing_local == new_local {
                return Err(ValidatorError::DuplicateAttribute {
                    span: name_span.start..name_span.end,
                });
            }
        }
        // Also check against namespace declarations (xmlns:foo vs xmlns:foo)
        if is_xmlns_prefix || is_default_xmlns {
            let ns_start = self.stack.scopes.last().unwrap().ns_decl_start as usize;
            for decl in &self.stack.ns_declarations[ns_start..] {
                let matches = if is_xmlns_prefix {
                    decl.prefix.as_bytes() == new_local
                } else {
                    decl.prefix.is_empty()
                };
                if matches {
                    return Err(ValidatorError::DuplicateAttribute {
                        span: name_span.start..name_span.end,
                    });
                }
            }
        }

        if is_xmlns_prefix || is_default_xmlns {
            self.schema.ns_context_dirty = true;
            let prefix_bytes: &[u8] = if is_xmlns_prefix {
                self.arena.get_name(local)
            } else {
                b""
            };
            let uri = self.arena.get_value(value).to_owned();
            self.stack.add_namespace(prefix_bytes, name_span, uri)?;
        } else {
            self.stack.add_attr(prefix, local, name_span, value);
        }
        Ok(())
    }

    fn start_tag_close(&mut self, span: Span) -> Result<(), Self::Error> {
        self.close_element_start(span)
    }

    fn empty_element_end(&mut self, span: Span) -> Result<(), Self::Error> {
        self.handle_empty_element_end(span)
    }

    fn end_tag(&mut self, name: QName<'_>) -> Result<(), Self::Error> {
        self.handle_end_tag(name.span())
    }

    fn characters(&mut self, text: &[u8], span: Span) -> Result<(), Self::Error> {
        self.append_text_bytes(text, span, "text")
    }

    fn entity_ref(&mut self, name: &[u8], span: Span) -> Result<(), Self::Error> {
        let name_str = std::str::from_utf8(name).map_err(|_| ValidatorError::InvalidUtf8 {
            span: span.start..span.end,
            kind: "entity name",
        })?;
        if let Some(val) = self.entity_definitions.get(name_str) {
            let val = val.clone();
            self.append_validated_text(&val, span)
        } else {
            Err(ValidatorError::UndefinedEntity {
                name: name_str.to_owned(),
                span: span.start..span.end,
            })
        }
    }

    fn char_ref(&mut self, value: &[u8], span: Span) -> Result<(), Self::Error> {
        let c =
            Self::decode_char_ref(value).map_err(|()| ValidatorError::InvalidOrUnclosedEntity {
                span: span.start..span.end,
            })?;
        let mut buf = [0u8; 4];
        let s = c.encode_utf8(&mut buf);
        self.append_validated_text(s, span)
    }

    fn cdata_content(&mut self, text: &[u8], span: Span) -> Result<(), Self::Error> {
        self.append_text_bytes(text, span, "CDATA content")
    }

    // DTD entity declarations
    fn entity_decl_start(
        &mut self,
        name: &[u8],
        kind: EntityKind,
        span: Span,
    ) -> Result<(), Self::Error> {
        if kind == EntityKind::General {
            let name_str = std::str::from_utf8(name).map_err(|_| ValidatorError::InvalidUtf8 {
                span: span.start..span.end,
                kind: "entity name",
            })?;
            self.stack.pending_entity_name = Some(name_str.to_owned());
            self.stack.pending_entity_value = Some(String::new());
        }
        Ok(())
    }

    fn entity_decl_value(&mut self, value: &[u8], span: Span) -> Result<(), Self::Error> {
        let s = std::str::from_utf8(value).map_err(|_| ValidatorError::InvalidUtf8 {
            span: span.start..span.end,
            kind: "entity value",
        })?;
        if let Some(ref mut val) = self.stack.pending_entity_value {
            val.push_str(s);
        }
        Ok(())
    }

    fn entity_decl_char_ref(&mut self, value: &[u8], span: Span) -> Result<(), Self::Error> {
        let c =
            Self::decode_char_ref(value).map_err(|()| ValidatorError::InvalidOrUnclosedEntity {
                span: span.start..span.end,
            })?;
        if let Some(ref mut val) = self.stack.pending_entity_value {
            val.push(c);
        }
        Ok(())
    }

    fn entity_decl_entity_ref(&mut self, name: &[u8], span: Span) -> Result<(), Self::Error> {
        let name_str = std::str::from_utf8(name).map_err(|_| ValidatorError::InvalidUtf8 {
            span: span.start..span.end,
            kind: "entity name",
        })?;
        if let Some(resolved) = self.entity_definitions.get(name_str) {
            let resolved = resolved.clone();
            if let Some(ref mut val) = self.stack.pending_entity_value {
                val.push_str(&resolved);
            }
            Ok(())
        } else {
            Err(ValidatorError::UndefinedEntity {
                name: name_str.to_owned(),
                span: span.start..span.end,
            })
        }
    }

    fn entity_decl_end(&mut self, _span: Span) -> Result<(), Self::Error> {
        if let (Some(name), Some(value)) = (
            self.stack.pending_entity_name.take(),
            self.stack.pending_entity_value.take(),
        ) {
            self.entity_definitions.insert(name, value);
        }
        Ok(())
    }
}

/// Attribute with name and value stored as ranges into the `TagArena`.
struct Attr {
    prefix: Option<NameRange>,
    local: NameRange,
    name_span: Span,
    value: ValueRange,
}

/// A namespace prefix→URI binding in the flat declaration stack.
struct NsDecl {
    prefix: String,
    namespace_uri: String,
}

/// Per-element metadata on the scope stack. No per-element Vec allocations.
struct ElementScope {
    prefix: Option<NameRange>,
    local: NameRange,
    name_span: Span,
    /// Index into `ns_declarations` where this element's declarations start.
    ns_decl_start: u32,
    /// True if this scope declared a default namespace (xmlns="...").
    has_default_ns: bool,
    /// Index into the reusable `attrs` vec where this element's attributes start.
    attr_start: u32,
}

#[derive(Default)]
struct ElementStack {
    /// Element scope stack.
    scopes: Vec<ElementScope>,
    /// Flat list of all prefix→URI namespace declarations across all depths.
    /// Each element scope owns the range `[scope.ns_decl_start .. next_scope.ns_decl_start]`.
    /// Pop truncates back to the scope's start index.
    ns_declarations: Vec<NsDecl>,
    /// Cached default namespace URI. Updated eagerly on declaration, recomputed
    /// on pop only if the popped scope had a default namespace declaration.
    cached_default_ns: String,
    /// Reusable buffer for attributes of the current element. Cleared per element.
    attrs: Vec<Attr>,
    // Temporary state for DTD entity declarations
    pending_entity_name: Option<String>,
    pending_entity_value: Option<String>,
}

impl ElementStack {
    /// Byte-compare lookup. Stored prefixes are guaranteed valid UTF-8
    /// (validated at `add_namespace`), so a hit implies the input bytes are
    /// also valid UTF-8 by transitivity.
    fn lookup_namespace_uri(&self, prefix: &[u8]) -> Option<&str> {
        if prefix == b"xml" {
            return Some("http://www.w3.org/XML/1998/namespace");
        }
        // Walk declarations in reverse (most recent first)
        self.ns_declarations
            .iter()
            .rev()
            .find(|d| d.prefix.as_bytes() == prefix)
            .map(|d| d.namespace_uri.as_str())
    }

    fn capture_ns_context(&self) -> NsContext {
        let mut default_ns = String::new();
        let mut bindings = Vec::new();
        for decl in &self.ns_declarations {
            if decl.prefix.is_empty() {
                default_ns = decl.namespace_uri.clone();
            } else {
                bindings.push((decl.prefix.clone(), decl.namespace_uri.clone()));
            }
        }
        NsContext {
            default_ns,
            bindings,
        }
    }

    fn resolve_element_namespace(
        &self,
        prefix: Option<&[u8]>,
        name_span: Span,
    ) -> Result<Option<&str>, ValidatorError> {
        match prefix {
            None => {
                // For elements, empty prefix means look up the default namespace (xmlns="...")
                if self.cached_default_ns.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(self.cached_default_ns.as_str()))
                }
            }
            Some(p) => match self.lookup_namespace_uri(p) {
                Some(uri) => Ok(Some(uri)),
                None => Err(undefined_or_invalid_prefix(p, name_span)),
            },
        }
    }

    fn resolve_attribute_namespace(
        &self,
        prefix: Option<&[u8]>,
        name_span: Span,
    ) -> Result<Option<&str>, ValidatorError> {
        match prefix {
            // Per XML Namespaces spec, unprefixed attributes have no namespace
            None => Ok(None),
            Some(p) => match self.lookup_namespace_uri(p) {
                Some(uri) => Ok(Some(uri)),
                None => Err(undefined_or_invalid_prefix(p, name_span)),
            },
        }
    }

    fn push(&mut self, prefix: Option<NameRange>, local: NameRange, name_span: Span) {
        let ns_decl_start = self.ns_declarations.len() as u32;
        let attr_start = self.attrs.len() as u32;
        self.scopes.push(ElementScope {
            prefix,
            local,
            name_span,
            ns_decl_start,
            has_default_ns: false,
            attr_start,
        });
    }

    fn pop_has_namespaces(&mut self) -> bool {
        let scope = self.scopes.pop().unwrap();
        let had_ns = (scope.ns_decl_start as usize) < self.ns_declarations.len();
        self.ns_declarations.truncate(scope.ns_decl_start as usize);
        self.attrs.truncate(scope.attr_start as usize);
        // Recompute cached default namespace if the popped scope declared one
        if scope.has_default_ns {
            self.cached_default_ns = self
                .ns_declarations
                .iter()
                .rev()
                .find(|d| d.prefix.is_empty())
                .map(|d| d.namespace_uri.clone())
                .unwrap_or_default();
        }
        had_ns
    }

    /// Validates prefix UTF-8 once, here. Anchors the invariant that every
    /// stored `NsDecl.prefix` is valid UTF-8, so hot-path lookups can byte-compare
    /// without revalidating.
    fn add_namespace(
        &mut self,
        prefix_bytes: &[u8],
        prefix_span: Span,
        uri: String,
    ) -> Result<(), ValidatorError> {
        let prefix = if prefix_bytes.is_empty() {
            String::new()
        } else {
            std::str::from_utf8(prefix_bytes)
                .map_err(|_| ValidatorError::InvalidUtf8 {
                    span: prefix_span.start..prefix_span.end,
                    kind: "namespace prefix",
                })?
                .to_owned()
        };
        if prefix.is_empty() {
            self.cached_default_ns = uri.clone();
            self.scopes.last_mut().unwrap().has_default_ns = true;
        }
        self.ns_declarations.push(NsDecl {
            prefix,
            namespace_uri: uri,
        });
        Ok(())
    }

    fn add_attr(
        &mut self,
        prefix: Option<NameRange>,
        local: NameRange,
        name_span: Span,
        value: ValueRange,
    ) {
        self.attrs.push(Attr {
            prefix,
            local,
            name_span,
            value,
        });
    }

    /// Number of attributes on the current element.
    fn current_attr_count(&self) -> usize {
        let start = self.scopes.last().unwrap().attr_start as usize;
        self.attrs.len() - start
    }

    /// Number of namespace declarations on the current element.
    fn current_ns_count(&self) -> usize {
        let start = self.scopes.last().unwrap().ns_decl_start as usize;
        self.ns_declarations.len() - start
    }
}

#[cfg(test)]
mod tests {
    use crate::Validator;
    use assert_matches::assert_matches;
    use relaxng_model::model::DefineRule;
    use relaxng_model::{Compiler, Files, RelaxError, Syntax};
    use std::io;
    use std::path::Path;

    struct Fixture {
        schema: std::rc::Rc<std::cell::RefCell<Option<DefineRule>>>,
    }
    impl Fixture {
        fn correct(schema: &str) -> Fixture {
            struct FS(String);
            impl Files for FS {
                fn load(&self, name: &Path) -> Result<String, RelaxError> {
                    let t = match name.to_str().unwrap() {
                        "main.rnc" => self.0.clone(),
                        _ => {
                            return Err(RelaxError::Io(
                                name.to_path_buf(),
                                io::Error::from(io::ErrorKind::NotFound),
                            ));
                        }
                    };
                    Ok(t)
                }
            }
            let mut c = Compiler::new(FS(schema.to_string()), Syntax::Compact);
            let input = Path::new("main.rnc");
            let schema = match c.compile(input) {
                Ok(s) => s,
                Err(e) => {
                    c.dump_diagnostic(&e);
                    panic!("{e:?}");
                }
            };
            Fixture {
                schema: schema.start,
            }
        }

        fn valid(&self, xml: &str) {
            let mut v = Validator::new(self.schema.clone()).unwrap();
            if let Err(err) = v.validate(xml.as_bytes()) {
                let (map, d) = v.diagnostic("valid.xml".to_string(), xml.as_bytes(), &err);
                let mut emitter = codemap_diagnostic::Emitter::stderr(
                    codemap_diagnostic::ColorConfig::Auto,
                    Some(&map),
                );
                emitter.emit(&d[..]);
                panic!("{err:?}");
            }
        }

        fn valid_with_coverage(&self, xml: &str) -> super::CoverageReport {
            let mut v = Validator::new_with_coverage(self.schema.clone()).unwrap();
            if let Err(err) = v.validate(xml.as_bytes()) {
                let (map, d) = v.diagnostic("valid.xml".to_string(), xml.as_bytes(), &err);
                let mut emitter = codemap_diagnostic::Emitter::stderr(
                    codemap_diagnostic::ColorConfig::Auto,
                    Some(&map),
                );
                emitter.emit(&d[..]);
                panic!("{err:?}");
            }
            v.coverage_report().expect("coverage should be enabled")
        }

        fn invalid(&self, xml: &str) {
            let mut v = Validator::new(self.schema.clone()).unwrap();
            if v.validate(xml.as_bytes()).is_ok() {
                panic!("Invalid input was accepted by the validator")
            }
        }
    }

    fn check_simple(schema: &str, doc: &str) -> Result<(), String> {
        struct FS(String);
        impl Files for FS {
            fn load(&self, name: &Path) -> Result<String, RelaxError> {
                let t = match name.to_str().unwrap() {
                    "main.rnc" => self.0.clone(),
                    _ => {
                        return Err(RelaxError::Io(
                            name.to_path_buf(),
                            io::Error::from(io::ErrorKind::NotFound),
                        ));
                    }
                };
                Ok(t)
            }
        }
        let mut c = Compiler::new(FS(schema.to_string()), Syntax::Compact);
        let input = Path::new("main.rnc");
        let schema = match c.compile(input) {
            Ok(s) => s,
            Err(e) => {
                c.dump_diagnostic(&e);
                panic!("{e:?}");
            }
        };

        let mut v = Validator::new(schema.start).unwrap();
        println!("====");
        v.schema.d(v.current_step).unwrap();
        println!("====");
        v.validate(doc.as_bytes()).map_err(|e| format!("{e:?}"))
    }

    #[test]
    fn recur() {
        let res = check_simple(
            "start = b  b = element a { b? }",
            "<?xml version=\"1.0\"?><a><a></a></a>",
        );
        assert!(res.is_ok())
    }

    #[test]
    fn attr() {
        let res = check_simple(
            "start = element a { attribute b { text } }",
            "<?xml version=\"1.0\"?><a b=\"\"/>",
        );
        assert_matches!(res, Ok(()));
    }

    #[test]
    fn attr_group_incomplete() {
        // schema requires both attributes, but the instance document only has the first one
        let res = check_simple(
            "start = element e { attribute a { text }, attribute b { text } }",
            "<?xml version=\"1.0\"?><e a=\"\"/>",
        );
        assert_matches!(res, Err(_));
    }

    #[test]
    fn attr_choice() {
        let res = check_simple(
            "start = element a { attribute a { text } | attribute b { text } }",
            "<?xml version=\"1.0\"?><a a=\"\"/>",
        );
        assert_matches!(res, Ok(()));
    }

    #[test]
    fn attr_unexpected() {
        let res = check_simple(
            "start = element a { attribute a { text } }",
            "<?xml version=\"1.0\"?><a a=\"\" b=\"\"/>",
        );
        assert_matches!(res, Err(_));
    }

    #[test]
    fn elem_text_empty() {
        // if the only child of an element is whitespace, this should still match 'empty'
        let res = check_simple(
            "start = element e { empty }",
            "<?xml version=\"1.0\"?><e> </e>",
        );
        assert_matches!(res, Ok(()));
    }

    #[test]
    fn elem_only_children_whitespace() {
        // if the only child of an element is whitespace, this should still match 'empty'
        let res = check_simple(
            "start = element e1 { element e2 { empty } }",
            "<?xml version=\"1.0\"?><e1> <e2/> </e1>",
        );
        assert_matches!(res, Ok(()));
    }

    #[test]
    fn list() {
        let res = check_simple(
            "start = element e1 { list { token 'one', token 'two' } }",
            "<?xml version=\"1.0\"?><e1>one two</e1>",
        );
        if let Err(e) = res {
            panic!("{e:?}");
        }
    }

    #[test]
    fn list_one_or_more() {
        let res = check_simple(
            "start = element e1 { list { 'x'+ } }",
            "<?xml version=\"1.0\"?><e1>x</e1>",
        );
        if let Err(e) = res {
            panic!("{e:?}");
        }
    }

    #[test]
    fn element_whitespace_one_or_more() {
        // the whitespace prior to the initial <b/> should not make the doc invalid
        Fixture::correct("start = element a { element b { empty }+ }")
            .valid("<a> <b/><b/><b/></a>");
    }

    #[test]
    fn text_element_group() {
        // a bug in the implementation of start_tag_open_deriv(Pat::Group) used to cause this to
        // fail
        Fixture::correct("start = element a { text, element b { empty } }")
            .valid("<a>foo <b/></a>");
    }

    #[test]
    fn datavalue_in_parts() {
        // the ampersand in the input document means that the text is delivered to the validator
        // in pieces,
        //  - the initial "foo "
        //  - the "&" of the decoded character reference
        //  - the trailing " bar"
        // this should still be a valid value of the string datatype
        Fixture::correct("start = element a { xsd:string }").valid("<a>foo &amp; bar</a>");
    }

    #[test]
    fn top_level_grammar() {
        Fixture::correct("grammar { start = element a { empty } }").valid("<a/>");
    }

    #[test]
    fn attribute_any_other_namespace() {
        Fixture::correct(
            "namespace local = \"\" \
             default namespace foo = \"urn:foo\" \
             element MPD { \
                attribute mediaPresentationDuration { xsd:duration }?, \
                (attribute * - (foo:* | local:*)  { text })+ \
             }",
        )
        .invalid(
            "<?xml verion=\"1.0\"?>\
            <MPD xmlns=\"urn:foo\" mediaPresentationDuration=\"XPT0H11M58.998S\"></MPD>",
        );
    }

    #[test]
    fn blowup() {
        // https://relaxng.org/jclark/derivative.html#Avoiding_exponential_blowup
        Fixture::correct(
            "start = element root { a8 | b8 } \
            a8 = a7 | b7+ \
            b8 = b7 | a7+ \
            a7 = a6 | b6+ \
            b7 = b6 | a6+ \
            a6 = a5 | b5+ \
            b6 = b5 | a5+ \
            a5 = a4 | b4+ \
            b5 = b4 | a4+ \
            a4 = a3 | b3+ \
            b4 = b3 | a3+ \
            a3 = a2 | b2+ \
            b3 = b2 | a2+ \
            a2 = a1 | b1+ \
            b2 = b1 | a1+ \
            a1 = a | b+ \
            b1 = b | a+ \
            a = element a { text } \
            b = element b { text }",
        )
        .valid("<root><b/><b/><b/></root>");
    }

    #[test]
    fn parse_entities() {
        Fixture::correct("start = element a { text }").valid("<a>foo &amp; bar</a>");
    }

    #[test]
    fn accept_empty_except() {
        let res = check_simple(
            "start = element a { token - \"x\" }",
            "<?xml version=\"1.0\"?><a/>",
        );
        assert_matches!(res, Ok(()));
    }

    #[test]
    fn coverage_element_choice() {
        let f = Fixture::correct("start = element a { element b { empty } | element c { empty } }");
        let report = f.valid_with_coverage("<a><b/></a>");
        // Element 'a' and 'b' covered, 'c' not covered
        assert!(report.covered_count() > 0);
        assert!(report.total_trackable() > 0);
        assert!(report.covered_count() < report.total_trackable());
        let uncovered: Vec<_> = report.uncovered_patterns().collect();
        assert!(
            uncovered
                .iter()
                .any(|p| p.kind == "Element" && p.name == "c"),
            "Element 'c' should be uncovered, got: {:?}",
            uncovered
                .iter()
                .map(|p| format!("{}:{}", p.kind, p.name))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn coverage_full() {
        let f = Fixture::correct("start = element a { element b { empty } }");
        let report = f.valid_with_coverage("<a><b/></a>");
        // Both elements covered
        assert_eq!(report.covered_count(), report.total_trackable());
    }

    #[test]
    fn coverage_attribute() {
        let f =
            Fixture::correct("start = element a { attribute x { text }, attribute y { text } }");
        let report = f.valid_with_coverage("<a x=\"1\" y=\"2\"/>");
        let uncovered: Vec<_> = report.uncovered_patterns().collect();
        // Both attributes should be covered
        assert!(
            !uncovered.iter().any(|p| p.kind == "Attribute"),
            "All attributes should be covered, uncovered: {:?}",
            uncovered
                .iter()
                .map(|p| format!("{}:{}", p.kind, p.name))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn coverage_text() {
        let f = Fixture::correct("start = element a { text }");
        let report = f.valid_with_coverage("<a>hello</a>");
        let covered: Vec<_> = report
            .patterns()
            .iter()
            .filter(|p| report.is_covered(p.pat_id) && p.kind == "Text")
            .collect();
        assert!(!covered.is_empty(), "Text pattern should be covered");
    }

    #[test]
    fn coverage_datatype() {
        let f = Fixture::correct("start = element a { xsd:string }");
        let report = f.valid_with_coverage("<a>hello</a>");
        let covered: Vec<_> = report
            .patterns()
            .iter()
            .filter(|p| report.is_covered(p.pat_id) && p.kind == "Datatype")
            .collect();
        assert!(!covered.is_empty(), "Datatype pattern should be covered");
    }

    #[test]
    fn coverage_merge() {
        let f = Fixture::correct("start = element a { element b { empty } | element c { empty } }");
        let mut report1 = f.valid_with_coverage("<a><b/></a>");
        let report2 = f.valid_with_coverage("<a><c/></a>");
        let before = report1.covered_count();
        report1.merge(&report2);
        // After merging, more patterns should be covered
        assert!(report1.covered_count() > before);
    }

    #[test]
    fn default_namespace_compact_syntax() {
        // default namespace declaration must apply to unqualified element names
        let schema = "default namespace = \"urn:example:books\" \
                      start = element catalog { \
                          element book { \
                              element title { text }, \
                              element author { text } \
                          }+ \
                      }";
        let f = Fixture::correct(schema);

        // Document with matching namespace should be valid
        f.valid(
            "<catalog xmlns=\"urn:example:books\">\
               <book><title>T</title><author>A</author></book>\
             </catalog>",
        );

        // Document with no namespace should be invalid
        f.invalid("<catalog><book><title>T</title><author>A</author></book></catalog>");

        // Document with wrong namespace should be invalid
        f.invalid(
            "<catalog xmlns=\"urn:wrong\">\
               <book><title>T</title><author>A</author></book>\
             </catalog>",
        );
    }

    #[test]
    fn default_namespace_with_prefix_alias() {
        // default namespace with prefix alias should set both the default and the prefix binding
        let schema = "default namespace books = \"urn:example:books\" \
                      start = element catalog { \
                          element books:book { text } \
                      }";
        let f = Fixture::correct(schema);

        // Unqualified element uses default namespace; prefixed element uses same namespace
        f.valid(
            "<catalog xmlns=\"urn:example:books\">\
               <book>text</book>\
             </catalog>",
        );

        // No namespace should be invalid
        f.invalid("<catalog><book>text</book></catalog>");
    }

    #[test]
    fn error_message_includes_namespace() {
        let schema = "namespace foo = \"urn:foo\" \
                      start = element foo:root { \
                          element foo:child { text } \
                      }";
        let f = Fixture::correct(schema);

        let xml = "<root xmlns=\"urn:foo\"><wrong xmlns=\"urn:bar\">text</wrong></root>";
        let mut v = Validator::new(f.schema.clone()).unwrap();
        let err = v.validate(xml.as_bytes()).unwrap_err();
        let (_, diagnostics) = v.diagnostic("test.xml".to_string(), xml.as_bytes(), &err);
        let messages: Vec<_> = diagnostics.iter().map(|d| d.message.as_str()).collect();
        assert!(
            messages.iter().any(|m| m.contains("{urn:foo}child")),
            "Error should mention namespace URI, got: {messages:?}"
        );
    }

    #[test]
    fn error_message_no_namespace_omits_braces() {
        let schema = "start = element root { element child { text } }";
        let f = Fixture::correct(schema);

        let xml = "<root><wrong>text</wrong></root>";
        let mut v = Validator::new(f.schema.clone()).unwrap();
        let err = v.validate(xml.as_bytes()).unwrap_err();
        let (_, diagnostics) = v.diagnostic("test.xml".to_string(), xml.as_bytes(), &err);
        let messages: Vec<_> = diagnostics.iter().map(|d| d.message.as_str()).collect();
        assert!(
            messages
                .iter()
                .any(|m| m.contains("child") && !m.contains('{')),
            "Error for no-namespace element should not have braces, got: {messages:?}"
        );
    }

    #[test]
    fn coverage_disabled_by_default() {
        let f = Fixture::correct("start = element a { empty }");
        let mut v = Validator::new(f.schema.clone()).unwrap();
        v.validate(b"<a/>").unwrap();
        assert!(v.coverage_report().is_none());
    }

    #[test]
    fn invalid_element_name_reported_as_invalid_name() {
        // Schema accepts any element name, but "b!ad" is not a valid XML NCName
        // (xml-syntax-reader accepts '!' in names since it's not a name-terminator)
        let f = Fixture::correct("start = element * { empty }");
        let mut v = Validator::new(f.schema.clone()).unwrap();
        let err = v.validate(b"<b!ad/>").unwrap_err();
        assert_matches!(
            err,
            super::ValidatorError::InvalidName {
                kind: "element",
                ..
            }
        );
    }

    #[test]
    fn invalid_name_start_char_reported_as_invalid_name() {
        // "1bad" starts with a digit — rejected by xml-syntax-reader as ExpectedName,
        // which the validator maps to InvalidName (not Xml error)
        let f = Fixture::correct("start = element a { empty }");
        let mut v = Validator::new(f.schema.clone()).unwrap();
        let err = v.validate(b"<1bad/>").unwrap_err();
        assert_matches!(err, super::ValidatorError::InvalidName { .. });
    }

    #[test]
    fn invalid_attribute_name_reported_as_invalid_name() {
        // Schema requires an attribute with a specific name plus allows any others;
        // "b@d" is not a valid NCName
        let f = Fixture::correct(
            "start = element a { attribute x { text }, attribute * - x { text }* }",
        );
        let mut v = Validator::new(f.schema.clone()).unwrap();
        let err = v.validate(b"<a x=\"v\" b@d=\"x\"/>").unwrap_err();
        assert_matches!(
            err,
            super::ValidatorError::InvalidName {
                kind: "attribute",
                ..
            }
        );
    }

    #[test]
    fn duplicate_attribute_rejected() {
        let f = Fixture::correct("start = element a { attribute x { text } }");
        let mut v = Validator::new(f.schema.clone()).unwrap();
        let err = v.validate(b"<a x=\"1\" x=\"2\"/>").unwrap_err();
        assert_matches!(err, super::ValidatorError::DuplicateAttribute { .. });
    }

    #[test]
    fn duplicate_xmlns_rejected() {
        let f = Fixture::correct("default namespace = \"urn:x\" start = element a { empty }");
        let mut v = Validator::new(f.schema.clone()).unwrap();
        let err = v
            .validate(b"<a xmlns=\"urn:x\" xmlns=\"urn:y\"/>")
            .unwrap_err();
        assert_matches!(err, super::ValidatorError::DuplicateAttribute { .. });
    }

    #[test]
    fn invalid_utf8_in_text_rejected() {
        // Schema permits any text — we must still reject malformed UTF-8.
        let f = Fixture::correct("start = element a { text }");
        let mut v = Validator::new(f.schema.clone()).unwrap();
        // 0xC0 is an invalid UTF-8 lead byte (overlong encoding).
        let bad = b"<a>hello\xC0world</a>";
        let err = v.validate(bad).unwrap_err();
        assert_matches!(err, super::ValidatorError::InvalidUtf8 { kind: "text", .. });
    }

    #[test]
    fn invalid_utf8_in_text_under_fixed_point_pattern_rejected() {
        // `text` is a fixed-point — the validator's hot path skips the buffer
        // copy and `text_deriv` walk, but Option A keeps the UTF-8 check.
        let f = Fixture::correct("start = element a { text }");
        let mut v = Validator::new(f.schema.clone()).unwrap();
        // Lone continuation byte — invalid UTF-8.
        let bad = b"<a>\x80</a>";
        let err = v.validate(bad).unwrap_err();
        assert_matches!(err, super::ValidatorError::InvalidUtf8 { kind: "text", .. });
    }

    #[test]
    fn invalid_utf8_in_cdata_rejected() {
        let f = Fixture::correct("start = element a { text }");
        let mut v = Validator::new(f.schema.clone()).unwrap();
        let bad = b"<a><![CDATA[\xFF]]></a>";
        let err = v.validate(bad).unwrap_err();
        assert_matches!(
            err,
            super::ValidatorError::InvalidUtf8 {
                kind: "CDATA content",
                ..
            }
        );
    }

    #[test]
    fn invalid_utf8_in_attribute_value_rejected() {
        // Schema accepts any text in attribute — must still reject malformed UTF-8.
        let f = Fixture::correct("start = element a { attribute x { text } }");
        let mut v = Validator::new(f.schema.clone()).unwrap();
        let bad = b"<a x=\"hi\xC0there\"/>";
        let err = v.validate(bad).unwrap_err();
        assert_matches!(
            err,
            super::ValidatorError::InvalidUtf8 {
                kind: "attribute value",
                ..
            }
        );
    }

    #[test]
    fn invalid_utf8_in_xmlns_uri_rejected() {
        // xmlns URI is an attribute value — bad UTF-8 must be rejected even
        // though no schema URI comparison would ever hit it.
        let f = Fixture::correct("start = element a { empty }");
        let mut v = Validator::new(f.schema.clone()).unwrap();
        let bad = b"<a xmlns:foo=\"urn:\xFFbad\"/>";
        let err = v.validate(bad).unwrap_err();
        assert_matches!(
            err,
            super::ValidatorError::InvalidUtf8 {
                kind: "attribute value",
                ..
            }
        );
    }

    #[test]
    fn text_not_allowed_diagnostic_uses_text_span() {
        // When buffered text is rejected, the error span should cover the text
        // run, not the surrounding element.
        let f = Fixture::correct("start = element a { empty }");
        let mut v = Validator::new(f.schema.clone()).unwrap();
        let xml = b"<a>nope</a>";
        let err = v.validate(xml).unwrap_err();
        let span = match err {
            super::ValidatorError::NotAllowed { span, kind: "text" } => span,
            other => panic!("expected NotAllowed text error, got {other:?}"),
        };
        // "nope" starts at byte 3 in the document.
        assert_eq!(span.start, 3, "span start should point at the text");
        assert_eq!(span.end, 7, "span end should be after the text");
    }
}
