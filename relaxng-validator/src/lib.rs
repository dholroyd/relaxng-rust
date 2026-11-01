mod coverage;
mod derivative;
mod infoset;
mod nameclass;
mod schema;

use relaxng_model::model;
use std::cell::RefCell;
use std::collections::HashSet;
use std::ops::Range;
use std::rc::Rc;
use xml_syntax_reader::Span;

pub use coverage::{CoverageReport, TrackablePattern};
// Re-exported so downstream crates can match `ValidatorError::Schema(..)` exhaustively.
pub use schema::SchemaError;

use infoset::{Failure, Handler, NamespaceScope, StartElement};
use nameclass::*;
use schema::*;

#[derive(Debug)]
pub enum ValidatorError {
    /// XML well-formedness error from the parser.
    Xml(xml_syntax_reader::Error),
    /// The XML construct at the given span is not allowed by the schema.
    NotAllowed {
        span: Range<u64>,
        kind: &'static str,
        /// What the schema would have accepted at this point, used by
        /// [`Explainer::diagnostic`].
        expected: Expected,
    },
    UndefinedNamespacePrefix {
        prefix: String,
        span: Range<u64>,
    },
    /// A reference to an entity other than the predefined ones. Entities declared in a DTD are
    /// not supported.
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
    /// Too many attributes on a single element.
    TooManyAttributes {
        span: Range<u64>,
    },
    /// An attribute value is too long. `span` is that of the attribute's name.
    AttributeValueTooLong {
        span: Range<u64>,
    },
    /// Bytes that should be UTF-8 (e.g. a namespace prefix) were not valid UTF-8.
    InvalidUtf8 {
        span: Range<u64>,
        kind: &'static str,
    },
    TextBufferOverflow,
    Schema(SchemaError),
}

impl ValidatorError {
    /// Whether validation can continue past this error. Schema errors are recoverable; errors
    /// that leave the parser or element stack in an unknown state are not.
    fn is_recoverable(&self) -> bool {
        matches!(
            self,
            ValidatorError::NotAllowed { .. } | ValidatorError::InvalidName { .. }
        )
    }
}

/// The schema position at which a [`ValidatorError::NotAllowed`] occurred, captured when the
/// error is found because error recovery may move the validator on before it is reported.
#[derive(Debug, Clone, Copy)]
pub struct Expected(PatId);

/// Returned when at least one error was passed to the sink.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Invalid;

impl std::fmt::Display for Invalid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "document is not valid")
    }
}

impl std::error::Error for Invalid {}

/// Why [`validate_reader`] failed.
#[derive(Debug)]
pub enum ReadError {
    /// The document is not valid; the errors have been passed to the sink.
    Invalid,
    /// Reading the document failed.
    Io(std::io::Error),
    /// A single XML construct, such as a tag, was larger than the read buffer.
    BufferTooSmall { capacity: usize },
}

impl From<Invalid> for ReadError {
    fn from(_: Invalid) -> Self {
        ReadError::Invalid
    }
}

impl std::fmt::Display for ReadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReadError::Invalid => Invalid.fmt(f),
            ReadError::Io(e) => write!(f, "I/O error: {e}"),
            ReadError::BufferTooSmall { capacity } => {
                write!(
                    f,
                    "XML construct larger than the {capacity}-byte read buffer"
                )
            }
        }
    }
}

impl std::error::Error for ReadError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ReadError::Io(e) => Some(e),
            _ => None,
        }
    }
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
            ValidatorError::MismatchedEndTag { start: Some(_), .. } => {
                write!(f, "end tag does not match the start tag")
            }
            ValidatorError::MismatchedEndTag { start: None, .. } => {
                write!(f, "end tag has no matching start tag")
            }
            ValidatorError::UnclosedElement { .. } => {
                write!(f, "element is not closed")
            }
            ValidatorError::MissingRootElement => {
                write!(f, "document has no root element")
            }
            ValidatorError::OutsideRootElement { kind, .. } => {
                write!(f, "{kind} not allowed outside the root element")
            }
            ValidatorError::TooManyAttributes { .. } => {
                write!(f, "too many attributes on element (limit: 256)")
            }
            ValidatorError::AttributeValueTooLong { .. } => {
                write!(
                    f,
                    "attribute value too long (limit: {} bytes)",
                    infoset::MAX_ATTRIBUTE_VALUE
                )
            }
            ValidatorError::InvalidUtf8 { kind, .. } => {
                write!(f, "{kind} contains invalid UTF-8")
            }
            ValidatorError::TextBufferOverflow => {
                write!(f, "Text content exceeds maximum buffer size")
            }
            ValidatorError::Schema(SchemaError::TooManyPatterns) => {
                write!(f, "Schema exceeds maximum number of patterns (65535)")
            }
        }
    }
}

impl From<infoset::Error> for ValidatorError {
    fn from(e: infoset::Error) -> Self {
        match e {
            infoset::Error::Xml(e) => ValidatorError::Xml(e),
            infoset::Error::UndefinedNamespacePrefix { prefix, span } => {
                ValidatorError::UndefinedNamespacePrefix { prefix, span }
            }
            infoset::Error::UndefinedEntity { name, span } => {
                ValidatorError::UndefinedEntity { name, span }
            }
            infoset::Error::InvalidOrUnclosedEntity { span } => {
                ValidatorError::InvalidOrUnclosedEntity { span }
            }
            infoset::Error::InvalidName { span, kind } => {
                ValidatorError::InvalidName { span, kind }
            }
            infoset::Error::DuplicateAttribute { span } => {
                ValidatorError::DuplicateAttribute { span }
            }
            infoset::Error::MismatchedEndTag { span, start } => {
                ValidatorError::MismatchedEndTag { span, start }
            }
            infoset::Error::UnclosedElement { span } => ValidatorError::UnclosedElement { span },
            infoset::Error::MissingRootElement => ValidatorError::MissingRootElement,
            infoset::Error::OutsideRootElement { span, kind } => {
                ValidatorError::OutsideRootElement { span, kind }
            }
            infoset::Error::TooManyAttributes { span } => {
                ValidatorError::TooManyAttributes { span }
            }
            infoset::Error::AttributeValueTooLong { span } => {
                ValidatorError::AttributeValueTooLong { span }
            }
            infoset::Error::InvalidUtf8 { span, kind } => {
                ValidatorError::InvalidUtf8 { span, kind }
            }
        }
    }
}

/// The outcome of a failed attempt to close an element's start tag (see
/// [`Engine::close_element_start`]).
struct CloseStartError {
    error: ValidatorError,
    /// A position to continue validating from despite the error, when a sound one exists.
    /// `None` means the caller must leave its current position untouched.
    recovery: Option<PatId>,
}

impl CloseStartError {
    /// An error with no recovery position — the caller must leave its current position
    /// untouched.
    fn unrecoverable(error: ValidatorError) -> Self {
        CloseStartError {
            error,
            recovery: None,
        }
    }
}

/// Receives each error found while validating. The [`Explainer`] argument lets the sink render
/// details, such as [`Explainer::diagnostic`], only when it wants them.
///
/// To stop validating once an error has been found, check [`Validator::is_invalid`] between
/// buffers.
pub trait Sink: FnMut(&Explainer<'_>, ValidatorError) {}
impl<F> Sink for F where F: FnMut(&Explainer<'_>, ValidatorError) {}

/// Renders details of a [`ValidatorError`] that are costly to produce, using the schema the
/// error was found against.
pub struct Explainer<'a> {
    schema: &'a Schema,
}

/// Validates XML documents against a schema, passing each error found to a [`Sink`].
///
/// The caller supplies each document one buffer at a time. Like
/// [`xml_syntax_reader::Reader::parse`], the validator reads directly from the caller's buffer
/// and never copies the input. [`Self::feed`] returns how many bytes it consumed. Any remainder
/// is the start of a construct the buffer ended partway through, such as a tag or a multi-byte
/// character, and the caller must pass it again at the front of the next buffer:
///
/// ```ignore
/// let (mut buf, mut len) = (vec![0; 64 * 1024], 0);
/// loop {
///     let n = input.read(&mut buf[len..])?;
///     if n == 0 {
///         break validator.finish(&buf[..len]);
///     }
///     len += n;
///     let consumed = validator.feed(&buf[..len])?;
///     if validator.is_invalid() {
///         validator.reset();
///         break Err(Invalid);
///     }
///     buf.copy_within(consumed..len, 0);
///     len -= consumed;
/// }
/// ```
///
/// A new validator is ready for its first document, and [`Self::finish`] makes it ready for the
/// next. [`validate_slice`] and [`validate_reader`] are conveniences built on this interface.
pub struct Validator<S> {
    engine: Engine<S>,
    parser: infoset::PushParser,
    /// Whether parsing of the current document has stopped, after an error the validator
    /// cannot recover from.
    stopped: bool,
}

impl<S: Sink> Validator<S> {
    pub fn new(
        model: Rc<RefCell<Option<model::DefineRule>>>,
        sink: S,
    ) -> Result<Validator<S>, ValidatorError> {
        Ok(Validator {
            engine: Engine::new(model, sink)?,
            parser: infoset::PushParser::new(),
            stopped: false,
        })
    }

    /// Create a validator with coverage tracking enabled.
    pub fn new_with_coverage(
        model: Rc<RefCell<Option<model::DefineRule>>>,
        sink: S,
    ) -> Result<Validator<S>, ValidatorError> {
        let mut v = Self::new(model, sink)?;
        v.engine.enable_coverage();
        Ok(v)
    }

    /// Validates as much of `buf` as possible, passing any errors found to the sink, and returns
    /// the number of bytes consumed.
    ///
    /// Returns `Err(Invalid)` if validation of the document has stopped after an error it cannot
    /// recover from, so that further input would be ignored. Errors that validation recovers from
    /// do not stop it; see [`Self::is_invalid`] to stop at those.
    pub fn feed(&mut self, buf: &[u8]) -> Result<usize, Invalid> {
        if self.stopped {
            return Err(Invalid);
        }
        let result = self.parser.feed(buf, &mut self.engine);
        self.stop_on(result)
    }

    /// Validates the last of the document, reporting anything left unfinished such as unclosed
    /// elements, and returns the outcome of validating the whole document. The validator is then
    /// ready for a new document.
    pub fn finish(&mut self, buf: &[u8]) -> Result<(), Invalid> {
        if !self.stopped {
            let result = self.parser.finish(buf, &mut self.engine);
            let _ = self.stop_on(result);
        }
        let outcome = if self.is_invalid() {
            Err(Invalid)
        } else {
            Ok(())
        };
        self.reset();
        outcome
    }

    /// Whether any error has been found in the current document, so that it is known to be
    /// invalid.
    pub fn is_invalid(&self) -> bool {
        self.engine.invalid
    }

    /// Abandons the current document, ready to validate a new one from its start. Only needed
    /// when stopping before [`Self::finish`].
    pub fn reset(&mut self) {
        self.engine.reset();
        self.parser.reset();
        self.stopped = false;
    }

    /// Extract the coverage report. Returns `None` if coverage tracking was not enabled.
    pub fn coverage_report(&self) -> Option<CoverageReport> {
        self.engine.coverage_report()
    }

    /// Stops validation after a fatal error, passing the error to the sink.
    fn stop_on<T>(&mut self, result: Result<T, Failure<ValidatorError>>) -> Result<T, Invalid> {
        result.map_err(|failure| {
            self.stopped = true;
            self.engine.fail(match failure {
                Failure::Infoset(err) => err.into(),
                Failure::Handler(err) => err,
            });
            Invalid
        })
    }
}

/// Validates an XML document already in memory.
pub fn validate_slice<S: Sink>(validator: &mut Validator<S>, source: &[u8]) -> Result<(), Invalid> {
    validator.reset();
    validator.finish(source)
}

/// Validates XML from a streaming reader, without loading the entire document into memory.
pub fn validate_reader<S: Sink, R: std::io::Read>(
    validator: &mut Validator<S>,
    mut reader: R,
) -> Result<(), ReadError> {
    validate_reader_with_capacity(validator, &mut reader, 64 * 1024)
}

fn validate_reader_with_capacity<S: Sink, R: std::io::Read>(
    validator: &mut Validator<S>,
    reader: &mut R,
    capacity: usize,
) -> Result<(), ReadError> {
    validator.reset();
    let result = feed_from_reader(validator, reader, capacity);
    if result.is_err() {
        validator.reset();
    }
    result
}

fn feed_from_reader<S: Sink, R: std::io::Read>(
    validator: &mut Validator<S>,
    reader: &mut R,
    capacity: usize,
) -> Result<(), ReadError> {
    let mut buf = vec![0u8; capacity];
    let mut len = 0;
    loop {
        let n = match reader.read(&mut buf[len..]) {
            Ok(n) => n,
            Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
            Err(e) => return Err(ReadError::Io(e)),
        };
        if n == 0 {
            return Ok(validator.finish(&buf[..len])?);
        }
        len += n;
        let consumed = validator.feed(&buf[..len])?;
        buf.copy_within(consumed..len, 0);
        len -= consumed;
        // A full buffer with nothing consumed means a single construct is larger than the
        // buffer, and reading into an empty slice would be mistaken for the end of input
        if len == buf.len() {
            return Err(ReadError::BufferTooSmall { capacity: len });
        }
    }
}

/// Validates the events of one document at a time against the schema.
struct Engine<S> {
    schema: Schema,
    /// The schema's start pattern, where each document's validation begins.
    start: PatId,
    current_step: PatId,
    last_was_start_element: bool,
    /// Text content buffered for deferred `text_deriv`.
    text_buffer: String,
    /// Source span covering the buffered text run, used for diagnostics on
    /// `flush_text` failure. `None` when the buffer is empty.
    text_buffer_span: Option<Span>,
    max_text_buffer: usize,
    skip_depth: u32,
    sink: S,
    invalid: bool,
}

impl<S: Sink> Engine<S> {
    fn new(
        model: Rc<RefCell<Option<model::DefineRule>>>,
        sink: S,
    ) -> Result<Engine<S>, ValidatorError> {
        let (schema, start) = build_schema(&model).map_err(ValidatorError::Schema)?;
        Ok(Engine {
            schema,
            start,
            current_step: start,
            last_was_start_element: false,
            text_buffer: String::new(),
            text_buffer_span: None,
            max_text_buffer: 1024 * 1024,
            skip_depth: 0,
            sink,
            invalid: false,
        })
    }

    fn enable_coverage(&mut self) {
        let compile_time_count = self.schema.inner.borrow().patterns.len() as u16;
        let word_count = (compile_time_count as usize).div_ceil(64);
        self.schema.compile_time_count = compile_time_count;
        self.schema.coverage = Some(vec![0u64; word_count].into_boxed_slice());
    }

    /// Resets the per-document state that must match a fresh parse.
    fn reset(&mut self) {
        self.current_step = self.start;
        self.last_was_start_element = false;
        self.text_buffer.clear();
        self.text_buffer_span = None;
        self.skip_depth = 0;
        self.invalid = false;
    }

    /// Passes an error that validation recovers from to the sink.
    fn report(&mut self, err: ValidatorError) {
        debug_assert!(err.is_recoverable(), "{err:?}");
        self.fail(err);
    }

    /// Passes an error to the sink, marking the document invalid.
    fn fail(&mut self, err: ValidatorError) {
        self.invalid = true;
        (self.sink)(
            &Explainer {
                schema: &self.schema,
            },
            err,
        );
    }

    /// Extract the coverage report. Returns `None` if coverage tracking was not enabled.
    fn coverage_report(&self) -> Option<CoverageReport> {
        self.schema.build_coverage_report()
    }

    /// Flush any buffered text by applying `text_deriv`. On `NotAllowed`,
    /// reports a `text` error tagged with the buffered run's span and carries on
    /// as though the text were absent.
    fn flush_text(&mut self, ns: &NamespaceScope<'_>) -> Result<(), ValidatorError> {
        if self.text_buffer.is_empty() {
            return Ok(());
        }
        let next_id = self
            .schema
            .text_deriv(self.current_step, &self.text_buffer, ns);
        let buf_span = self.text_buffer_span.take();
        self.text_buffer.clear();
        if matches!(self.schema.patt(next_id), Pat::NotAllowed) {
            let span = buf_span.map(|s| s.start..s.end).unwrap_or(0..0);
            self.report(ValidatorError::NotAllowed {
                span,
                kind: "text",
                expected: Expected(self.current_step),
            });
            return Ok(());
        }
        self.current_step = next_id;
        Ok(())
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

    /// Derive the pattern across an element's start tag, returning the pattern for the
    /// element's content.
    ///
    /// On failure, [`CloseStartError::recovery`] is a *recovery* pattern: `Some(pid)` means the
    /// caller should still advance to `pid` despite reporting the error, so that the element's
    /// children are validated against the element's own content model rather than against the
    /// position the element itself was expected at. `None` means there is no sound state to
    /// continue from and the caller should leave its position untouched.
    ///
    /// Names that the schema accepts are marked valid, which spares the reader from checking
    /// them: every name in the schema is a valid NCName, so a name that matches is one too.
    fn close_element_start(
        &mut self,
        ns: &NamespaceScope<'_>,
        element: &StartElement<'_>,
    ) -> Result<PatId, CloseStartError> {
        let elem_name_span = element.name_span;
        let elem_qname = QualifiedName {
            namespace_uri: element.name.namespace_uri,
            local_name: element.name.local_name.as_bytes(),
        };

        let pat_id = self.current_step;
        let next_pat = self.schema.start_tag_open_deriv(pat_id, elem_qname);
        if matches!(self.schema.patt(next_pat), Pat::NotAllowed) {
            let err = if !element.name.local_name.is_valid() {
                ValidatorError::InvalidName {
                    span: elem_name_span.start..elem_name_span.end,
                    kind: "element",
                }
            } else {
                ValidatorError::NotAllowed {
                    span: elem_name_span.start..elem_name_span.end,
                    kind: "element-start",
                    expected: Expected(pat_id),
                }
            };
            // The element name itself is not allowed here, so there is no content model to
            // descend into and nothing to recover to.
            return Err(CloseStartError::unrecoverable(err));
        }
        element.name.local_name.mark_valid();

        // A rejected attribute is recorded but does not abandon the derivation: the
        // remaining attributes are still applied, so that an element with one bad attribute
        // still yields accurate diagnostics for its children.  Only the first bad attribute
        // is reported, to keep one error per start tag.
        let mut pat = next_pat;
        let mut first_attr_error = None;
        for attr in element.attributes() {
            let attr_name_span = attr.name_span;
            let attr_qname = QualifiedName {
                namespace_uri: attr.name.namespace_uri,
                local_name: attr.name.local_name.as_bytes(),
            };
            let mid = self.schema.start_att_deriv(pat, attr_qname);
            if !matches!(self.schema.patt(mid), Pat::NotAllowed) {
                // The name matched, even if the value goes on to fail
                attr.name.local_name.mark_valid();
            }
            let derived = self.schema.att_value_deriv(mid, attr.value, ns);
            if matches!(self.schema.patt(derived), Pat::NotAllowed) {
                if first_attr_error.is_none() {
                    first_attr_error = Some(if !attr.name.local_name.is_valid() {
                        ValidatorError::InvalidName {
                            span: attr_name_span.start..attr_name_span.end,
                            kind: "attribute",
                        }
                    } else {
                        ValidatorError::NotAllowed {
                            span: attr_name_span.start..attr_name_span.end,
                            kind: "attribute",
                            expected: Expected(pat_id),
                        }
                    });
                }
                // Leave `pat` alone: carry on as though the attribute were absent.
            } else {
                pat = derived;
            }
        }

        let closed = self.schema.start_tag_close_deriv(pat);
        if !matches!(self.schema.patt(closed), Pat::NotAllowed) {
            return match first_attr_error {
                None => Ok(closed),
                // The bad attribute aside, the start tag is complete, so the children can be
                // checked from here.
                Some(error) => Err(CloseStartError {
                    error,
                    recovery: Some(closed),
                }),
            };
        }

        // The start tag did not close: a required attribute was left unsatisfied, either
        // because it was invalid and skipped above, or because it was never given at all — the
        // element name still matched either way. Force through any outstanding required
        // attributes purely to reach the element's content model; the document is already
        // known to be invalid (via `error` below), so this only affects which further errors
        // get reported for the rest of the document.
        let span = element.close_span;
        let error = first_attr_error.unwrap_or(ValidatorError::NotAllowed {
            span: span.start..span.end,
            kind: "element-end",
            expected: Expected(pat_id),
        });
        let recovered = self.schema.force_start_tag_close_deriv(pat);
        let recovery =
            (!matches!(self.schema.patt(recovered), Pat::NotAllowed)).then_some(recovered);
        Err(CloseStartError { error, recovery })
    }

    fn handle_start_tag_close(
        &mut self,
        ns: &NamespaceScope<'_>,
        element: &StartElement<'_>,
    ) -> Result<(), ValidatorError> {
        match self.close_element_start(ns, element) {
            Ok(next_pat) => {
                self.current_step = next_pat;
                self.last_was_start_element = true;
                Ok(())
            }
            Err(CloseStartError {
                mut error,
                recovery: Some(recovered),
            }) => {
                self.current_step = recovered;
                self.last_was_start_element = true;
                set_expected(&mut error, recovered);
                self.report(error);
                Ok(())
            }
            Err(CloseStartError {
                error,
                recovery: None,
            }) => {
                // Nothing to check this element's contents against, so skip them; the
                // position is left where the element was expected.
                self.skip_depth = 1;
                self.report(error);
                Ok(())
            }
        }
    }

    fn handle_end_tag(
        &mut self,
        ns: &NamespaceScope<'_>,
        span: Span,
    ) -> Result<(), ValidatorError> {
        let result = if self.last_was_start_element {
            // The element had no child elements or text nodes, so complete it through the
            // same empty-content rule as a self-closing tag.
            self.schema
                .close_empty_element(self.current_step, false, ns)
        } else {
            self.schema.end_tag_deriv(self.current_step)
        };
        if matches!(self.schema.patt(result), Pat::NotAllowed) {
            // Some required content is missing. Report it against the element's content
            // position, then force the element closed so following siblings are checked at
            // the right depth. If even that fails, leave the position alone.
            let before = self.current_step;
            let forced = if self.last_was_start_element {
                self.schema.close_empty_element(before, true, ns)
            } else {
                self.schema.force_end_tag_deriv(before)
            };
            if !matches!(self.schema.patt(forced), Pat::NotAllowed) {
                self.current_step = forced;
                self.last_was_start_element = false;
            }
            self.report(ValidatorError::NotAllowed {
                span: span.start..span.end,
                kind: "element-end",
                expected: Expected(before),
            });
            return Ok(());
        }
        self.current_step = result;
        self.last_was_start_element = false;
        Ok(())
    }

    /// Validates the whole of an empty-element tag, which is complete as soon as it starts.
    fn handle_empty_element(
        &mut self,
        ns: &NamespaceScope<'_>,
        element: &StartElement<'_>,
    ) -> Result<(), ValidatorError> {
        let span = element.close_span;
        let next_id = match self.close_element_start(ns, element) {
            Ok(next_id) => next_id,
            Err(CloseStartError {
                mut error,
                recovery,
            }) => {
                if let Some(recovered) = recovery {
                    // `recovered` sits inside the element, but the tag is already complete, so
                    // close it too — otherwise following siblings would be matched at the wrong
                    // depth. Force through any outstanding content requirements (e.g. a missing
                    // required child) as well, since this element's own error has already been
                    // reported and there is nothing more to gain from cascading that into every
                    // sibling. If even that can't produce a sound position, leave it alone.
                    let closed = self.schema.close_empty_element(recovered, true, ns);
                    if !matches!(self.schema.patt(closed), Pat::NotAllowed) {
                        self.current_step = closed;
                        self.last_was_start_element = false;
                    }
                    set_expected(&mut error, recovered);
                }
                self.report(error);
                return Ok(());
            }
        };
        let result = self.schema.close_empty_element(next_id, false, ns);
        if matches!(self.schema.patt(result), Pat::NotAllowed) {
            // As in `handle_end_tag`: report the missing content, then force the element
            // closed.
            let forced = self.schema.close_empty_element(next_id, true, ns);
            if !matches!(self.schema.patt(forced), Pat::NotAllowed) {
                self.current_step = forced;
                self.last_was_start_element = false;
            }
            self.report(ValidatorError::NotAllowed {
                span: span.start..span.end,
                kind: "element-end",
                expected: Expected(next_id),
            });
            return Ok(());
        }
        self.current_step = result;
        self.last_was_start_element = false;
        Ok(())
    }
}

impl Explainer<'_> {
    fn heads(&self, id: PatId) -> HashSet<Pat> {
        let mut result = HashSet::new();
        self.head(&mut result, id);
        result
    }
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
                    self.head(result, p2);
                } else {
                    self.head(result, p1);
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
                describe_nameclass(self.schema.name_class(*nameclass), &mut desc);
                result.push_str(&desc);
            }
        }
        if rest > 0 {
            result.push_str(&format!(" .. or one of {rest} more"))
        }
        // TODO: plus attributes and everything else
        result
    }

    /// Build diagnostics for `err`, which was found while validating `source`.
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
            ValidatorError::NotAllowed {
                span,
                kind,
                expected,
            } => {
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

                let desc = self.describe_expected(expected.0);
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
                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Help,
                    message: "Only the predefined entities (&lt; &gt; &amp; &apos; &quot;) are \
                              supported (entities declared in a DTD are unsupported)"
                        .to_string(),
                    code: None,
                    spans: vec![],
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
            ValidatorError::MismatchedEndTag { span, start } => {
                let mut spans = vec![codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(span.start as _, span.end as _),
                    label: None,
                    style: codemap_diagnostic::SpanStyle::Primary,
                }];
                let message = if let Some(start) = start {
                    spans.push(codemap_diagnostic::SpanLabel {
                        span: file.span.subspan(start.start as _, start.end as _),
                        label: Some("start tag".to_string()),
                        style: codemap_diagnostic::SpanStyle::Secondary,
                    });
                    "end tag does not match the start tag"
                } else {
                    "end tag has no matching start tag"
                };
                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: message.to_string(),
                    code: None,
                    spans,
                });
            }
            ValidatorError::UnclosedElement { span } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(span.start as _, span.end as _),
                    label: Some("start tag".to_string()),
                    style: codemap_diagnostic::SpanStyle::Primary,
                };
                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: "element is not closed".to_string(),
                    code: None,
                    spans: vec![label],
                });
            }
            ValidatorError::MissingRootElement => {
                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: "document has no root element".to_string(),
                    code: None,
                    spans: vec![],
                });
            }
            ValidatorError::OutsideRootElement { span, kind } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(span.start as _, span.end as _),
                    label: None,
                    style: codemap_diagnostic::SpanStyle::Primary,
                };
                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("{kind} not allowed outside the root element"),
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
            ValidatorError::AttributeValueTooLong { span } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(span.start as _, span.end as _),
                    label: Some("value too long".to_string()),
                    style: codemap_diagnostic::SpanStyle::Primary,
                };
                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!(
                        "attribute value too long (limit: {} bytes)",
                        infoset::MAX_ATTRIBUTE_VALUE
                    ),
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

fn set_expected(err: &mut ValidatorError, pid: PatId) {
    if let ValidatorError::NotAllowed { expected, .. } = err {
        *expected = Expected(pid);
    }
}

/// A fatal error stops parsing, and is passed to the sink by [`Validator::stop_on`].
impl<S: Sink> Handler for Engine<S> {
    type Error = ValidatorError;

    fn start_element(
        &mut self,
        ns: &NamespaceScope<'_>,
        element: &StartElement<'_>,
    ) -> Result<(), ValidatorError> {
        if self.skip_depth > 0 {
            if !element.empty {
                self.skip_depth += 1;
            }
            return Ok(());
        }
        if element.empty {
            self.handle_empty_element(ns, element)
        } else {
            self.handle_start_tag_close(ns, element)
        }
    }

    fn end_element(&mut self, ns: &NamespaceScope<'_>, span: Span) -> Result<(), ValidatorError> {
        if self.skip_depth > 0 {
            self.skip_depth -= 1;
            return Ok(());
        }
        self.handle_end_tag(ns, span)
    }

    /// Buffers text for deferred `text_deriv`, unless the current pattern accepts any text
    /// unchanged, in which case there is nothing to check.
    fn text(
        &mut self,
        _ns: &NamespaceScope<'_>,
        s: &str,
        span: Span,
    ) -> Result<(), ValidatorError> {
        // Any text content (even fixed-point) means this element is not empty,
        // so clear the flag that would trigger an empty-text derivative on close.
        self.last_was_start_element = false;
        if self.skip_depth > 0
            || self.schema.mixed_text_deriv(self.current_step) == self.current_step
        {
            return Ok(());
        }
        self.append_str(s, span)
    }

    fn end_text(&mut self, ns: &NamespaceScope<'_>) -> Result<(), ValidatorError> {
        self.flush_text(ns)
    }
}

#[cfg(test)]
mod tests {
    use crate::{Explainer, Validator};
    use assert_matches::assert_matches;
    use relaxng_model::model::DefineRule;
    use relaxng_model::{Compiler, Files, RelaxError, Syntax};
    use std::cell::RefCell;
    use std::io;
    use std::rc::Rc;

    type BoxedSink = Box<dyn FnMut(&Explainer<'_>, crate::ValidatorError)>;

    /// A validator whose sink collects every error, for tests that inspect them afterwards.
    struct Case {
        v: Validator<BoxedSink>,
        errors: Rc<RefCell<Vec<crate::ValidatorError>>>,
    }

    impl Case {
        fn new(model: Rc<RefCell<Option<DefineRule>>>) -> Case {
            Self::build(model, false)
        }

        fn with_coverage(model: Rc<RefCell<Option<DefineRule>>>) -> Case {
            Self::build(model, true)
        }

        fn build(model: Rc<RefCell<Option<DefineRule>>>, coverage: bool) -> Case {
            let errors = Rc::new(RefCell::new(vec![]));
            let sink = errors.clone();
            let sink: BoxedSink = Box::new(move |_: &Explainer<'_>, err| {
                sink.borrow_mut().push(err);
            });
            let v = if coverage {
                Validator::new_with_coverage(model, sink)
            } else {
                Validator::new(model, sink)
            };
            Case {
                v: v.unwrap(),
                errors,
            }
        }

        fn validate_all(&mut self, source: &[u8]) -> Result<(), Vec<crate::ValidatorError>> {
            match crate::validate_slice(&mut self.v, source) {
                Ok(()) => Ok(()),
                Err(super::Invalid) => Err(self.errors.take()),
            }
        }
    }

    impl std::ops::Deref for Case {
        type Target = Validator<BoxedSink>;
        fn deref(&self) -> &Self::Target {
            &self.v
        }
    }

    impl std::ops::DerefMut for Case {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.v
        }
    }

    fn first_diagnostics(
        v: &mut Case,
        source: &[u8],
    ) -> (crate::ValidatorError, Vec<codemap_diagnostic::Diagnostic>) {
        let err = v
            .validate_all(source)
            .expect_err("document should be invalid")
            .remove(0);
        let (_, d) = Explainer {
            schema: &v.engine.schema,
        }
        .diagnostic("doc".to_string(), source, &err);
        (err, d)
    }

    fn emit(explainer: &Explainer<'_>, source: &[u8], err: &crate::ValidatorError) {
        let (map, d) = explainer.diagnostic("valid.xml".to_string(), source, err);
        let mut emitter =
            codemap_diagnostic::Emitter::stderr(codemap_diagnostic::ColorConfig::Auto, Some(&map));
        emitter.emit(&d[..]);
    }
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
            let mut v = Case::new(self.schema.clone());
            if let Err(errors) = v.validate_all(xml.as_bytes()) {
                for err in &errors {
                    emit(
                        &Explainer {
                            schema: &v.engine.schema,
                        },
                        xml.as_bytes(),
                        err,
                    );
                }
                panic!("{errors:?}");
            }
        }

        fn valid_with_coverage(&self, xml: &str) -> super::CoverageReport {
            let mut v = Case::with_coverage(self.schema.clone());
            if let Err(errors) = v.validate_all(xml.as_bytes()) {
                for err in &errors {
                    emit(
                        &Explainer {
                            schema: &v.engine.schema,
                        },
                        xml.as_bytes(),
                        err,
                    );
                }
                panic!("{errors:?}");
            }
            v.coverage_report().expect("coverage should be enabled")
        }

        fn invalid(&self, xml: &str) {
            let mut v = Case::new(self.schema.clone());
            if v.validate_all(xml.as_bytes()).is_ok() {
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

        let mut v = Case::new(schema.start);
        println!("====");
        v.engine.schema.d(v.engine.current_step).unwrap();
        println!("====");
        v.validate_all(doc.as_bytes()).map_err(|e| format!("{e:?}"))
    }

    const RECIPE: &str = r#"start = element recipe {
        attribute servings { text },
        element title { text },
        element ingredient { text }+,
        element step { text }*
    }"#;

    /// A reported error: its `Display` text, the source text its span covers, and the
    /// "Expected .." help text the diagnostics produce for it.
    #[derive(Debug)]
    struct Reported {
        message: String,
        source: String,
        help: String,
    }

    /// Validate `doc` to the end, collecting every error.
    fn all_errors(schema: &str, doc: &str) -> Vec<Reported> {
        let f = Fixture::correct(schema);
        let mut v = Case::new(f.schema.clone());
        let errors = v.validate_all(doc.as_bytes()).err().unwrap_or_default();
        errors
            .into_iter()
            .map(|err| {
                let (_, diagnostics) = Explainer {
                    schema: &v.engine.schema,
                }
                .diagnostic("doc".to_string(), doc.as_bytes(), &err);
                let help = diagnostics
                    .iter()
                    .find(|d| d.level == codemap_diagnostic::Level::Help)
                    .map(|d| d.message.clone())
                    .unwrap_or_default();
                let source = match &err {
                    crate::ValidatorError::NotAllowed { span, .. }
                    | crate::ValidatorError::InvalidName { span, .. } => {
                        doc[span.start as usize..span.end as usize].to_string()
                    }
                    _ => String::new(),
                };
                Reported {
                    message: err.to_string(),
                    source,
                    help,
                }
            })
            .collect()
    }

    #[test]
    fn bad_attribute_does_not_cascade_into_children() {
        // Apart from the misspelled attribute this document matches the schema, so the
        // misspelling should be the only thing reported: the children must still be
        // validated against `recipe`'s content model.
        let errs = all_errors(
            RECIPE,
            r#"<recipe serving="2"><title>t</title><ingredient>i</ingredient></recipe>"#,
        );
        assert_eq!(
            errs.len(),
            1,
            "expected only the attribute error, got {errs:?}"
        );
        assert_eq!(
            errs[0].source, "serving",
            "expected the error to name the misspelled attribute, got {errs:?}"
        );
    }

    /// The one error reported against a child element start tag.
    fn child_element_error(errs: &[Reported]) -> &Reported {
        match errs
            .iter()
            .find(|e| e.message == "element-start not expected here")
        {
            Some(e) => e,
            None => panic!("expected an error on a child element, got {errs:?}"),
        }
    }

    #[test]
    fn bad_attribute_reports_accurate_expectation_for_children() {
        let errs = all_errors(
            RECIPE,
            r#"<recipe serving="2"><ingredient>i</ingredient></recipe>"#,
        );
        assert_eq!(
            child_element_error(&errs).help,
            "Expected Element title",
            "got {errs:?}"
        );
        assert!(
            !errs.iter().any(|e| e.help.contains("recipe")),
            "no error should still be expecting <recipe>, got {errs:?}"
        );
    }

    #[test]
    fn expected_elements_respect_sequence_position() {
        // `title` is the only element allowed first; `ingredient` and `step` come later in
        // the sequence and must not be offered here.
        let errs = all_errors(
            RECIPE,
            r#"<recipe servings="2"><ingredient>i</ingredient></recipe>"#,
        );
        assert_eq!(
            child_element_error(&errs).help,
            "Expected Element title",
            "got {errs:?}"
        );
    }

    #[test]
    fn qname_values_resolve_against_the_scope_they_appear_in() {
        let f = Fixture::correct(
            r#"namespace a = "urn:a"
            start = element root {
              (element item { attribute ref { xsd:QName }?, xsd:QName }
               | element v | a:v { xsd:QName "a:x" })*
            }"#,
        );
        // A prefix declared on a sibling has gone out of scope
        f.valid(r#"<root><item xmlns:p="urn:p">p:y</item></root>"#);
        f.invalid(r#"<root><item xmlns:p="urn:p">p:y</item><item>p:y</item></root>"#);
        // Attribute values see the declarations on their own element, but not a sibling's
        f.valid(r#"<root><item xmlns:p="urn:p" ref="p:z">y</item></root>"#);
        f.invalid(r#"<root><item xmlns:p="urn:p">y</item><item ref="p:z">y</item></root>"#);
        // A nested declaration shadows an outer one until its element ends
        f.valid(r#"<root xmlns:p="urn:other"><v xmlns:p="urn:a">p:x</v></root>"#);
        f.invalid(r#"<root xmlns:p="urn:a"><v xmlns:p="urn:other">p:x</v></root>"#);
        f.invalid(r#"<root xmlns:p="urn:other"><v xmlns:p="urn:a">p:x</v><v>p:x</v></root>"#);
        // Unprefixed names take the default namespace in scope
        f.valid(r#"<root><v xmlns="urn:a">x</v></root>"#);
        f.invalid(r#"<root><v xmlns="urn:a">x</v><v>x</v></root>"#);
    }

    #[test]
    fn bad_attribute_on_empty_element_leaves_stack_balanced() {
        // The start tag is consumed whether or not it validates, so the element stack must
        // still be unwound.
        let f = Fixture::correct(
            "start = element root { element a { attribute x { text } }, element b { empty } }",
        );
        let mut v = Case::new(f.schema.clone());
        let doc = br#"<root><a y="1"/><b/></root>"#;
        assert_eq!(v.feed(doc), Ok(doc.len()));
        // Checked before `finish`, which resets it for the next document
        assert_eq!(
            v.engine.skip_depth, 0,
            "element tracking not unwound after the document ended"
        );
        assert_eq!(v.finish(b""), Err(super::Invalid));
        let errors = v.errors.take();
        assert_eq!(
            errors.len(),
            1,
            "only the bad attribute should be reported, got {errors:?}"
        );
    }

    #[test]
    fn wholly_missing_attribute_does_not_cascade_into_children() {
        let errs = all_errors(
            RECIPE,
            r#"<recipe><title>t</title><ingredient>i</ingredient></recipe>"#,
        );
        assert_eq!(
            errs.len(),
            1,
            "expected only the missing-attribute error, got {errs:?}"
        );
        assert!(
            !errs[0].help.contains("recipe"),
            "the single error should not still be expecting <recipe>, got {errs:?}"
        );
    }

    #[test]
    fn self_closing_element_missing_required_child_does_not_cascade() {
        let errs = all_errors(
            "start = element root { \
                element a { attribute x { text }, element b { empty } }, \
                element c { empty } \
            }",
            r#"<root><a y="1"/><c/></root>"#,
        );
        assert_eq!(
            errs.len(),
            1,
            "only the bad attribute on <a> should be reported, got {errs:?}"
        );
        assert_eq!(errs[0].source, "y", "got {errs:?}");
    }

    #[test]
    fn abandoned_self_closing_recovery_does_not_let_a_later_element_steal_its_slot() {
        let errs = all_errors(
            "start = element root { element a { attribute x { text }, element b { empty } } }",
            r#"<root><a y="1"/><a x="1"><b/></a></root>"#,
        );
        assert!(
            errs.len() >= 2,
            "expected both the bad attribute and the illegal second <a> to be reported, got {errs:?}"
        );
        assert!(
            errs.iter()
                .any(|e| e.message == "element-start not expected here"),
            "expected the second, schema-illegal <a> to be rejected outright, got {errs:?}"
        );
    }

    #[test]
    fn disallowed_element_contents_are_skipped() {
        let errs = all_errors(
            "start = element root { element a { empty }, element b { empty } }",
            r#"<root><a/><bogus><x>text</x><y/></bogus><b/></root>"#,
        );
        assert_eq!(errs.len(), 1, "got {errs:?}");
        assert_eq!(errs[0].source, "bogus", "got {errs:?}");
    }

    #[test]
    fn missing_required_child_does_not_cascade_into_siblings() {
        let errs = all_errors(
            "start = element root { element a { element b { empty } }, element c { empty } }",
            r#"<root><a></a><c/></root>"#,
        );
        assert_eq!(errs.len(), 1, "got {errs:?}");
        assert_eq!(errs[0].message, "element-end not expected here");
        assert_eq!(errs[0].help, "Expected Element b");
    }

    #[test]
    fn disallowed_text_is_skipped() {
        let errs = all_errors(
            "start = element root { element a { empty }, element b { empty } }",
            r#"<root><a>oops</a><b>again</b></root>"#,
        );
        assert_eq!(errs.len(), 2, "got {errs:?}");
        assert_eq!(errs[0].source, "oops");
        assert_eq!(errs[1].source, "again");
    }

    #[test]
    fn sink_receives_fatal_error_last() {
        let f = Fixture::correct("start = element root { element a { empty } }");
        let mut v = Case::new(f.schema.clone());
        let errors = v
            .validate_all(b"<root>text<a/>&undefined;</root>")
            .unwrap_err();
        assert_eq!(errors.len(), 2, "got {errors:?}");
        assert_matches!(
            errors[0],
            super::ValidatorError::NotAllowed { kind: "text", .. }
        );
        assert_matches!(errors[1], super::ValidatorError::UndefinedEntity { .. });
    }

    #[test]
    fn text_after_root_is_rejected() {
        let f = Fixture::correct("start = element root { empty }");
        let doc = b"<root/>trailing";

        let mut v = Case::new(f.schema.clone());
        let errors = v.validate_all(doc).unwrap_err();
        assert_eq!(errors.len(), 1, "got {errors:?}");
        assert_matches!(
            errors[0],
            super::ValidatorError::OutsideRootElement { kind: "text", .. }
        );

        let mut v = Case::new(f.schema.clone());
        let errors = match crate::validate_reader(&mut v.v, &doc[..]) {
            Ok(()) => vec![],
            Err(super::ReadError::Invalid) => v.errors.take(),
            Err(e) => panic!("{e}"),
        };
        assert_eq!(errors.len(), 1, "got {errors:?}");
        assert_matches!(
            errors[0],
            super::ValidatorError::OutsideRootElement { kind: "text", .. }
        );
    }

    /// Validates `doc` as it arrives in chunks of `size` bytes, keeping what each
    /// [`Validator::feed`] leaves unconsumed for the next call, as a caller must.
    /// Returns the result and the errors reported.
    fn push_in_chunks(
        v: &mut Case,
        doc: &[u8],
        size: usize,
    ) -> (Result<(), super::Invalid>, String) {
        let mut buf = vec![];
        for chunk in doc.chunks(size) {
            buf.extend_from_slice(chunk);
            match v.feed(&buf) {
                Ok(consumed) => drop(buf.drain(..consumed)),
                Err(super::Invalid) => break,
            }
        }
        let result = v.finish(&buf);
        (result, format!("{:?}", v.errors.take()))
    }

    #[test]
    fn push_validation_matches_whole_document() {
        let schema =
            "start = element root { attribute a { text }?, (text | element x { empty })* }";
        let docs: &[&[u8]] = &[
            "<root a='é'>héllo &amp; <x/>wörld&#x1F600;</root>".as_bytes(),
            b"<!DOCTYPE root [<!ENTITY e 'ent'>]><root>&lt;<![CDATA[<x/>]]><!-- c --></root>",
            b"<root><y/><x>bad</x><x/></root>",
            b"<root b='1'>text<x/>",
            b"<root></wrong>",
            // A single character, as the span of an error in text outside the root covers only
            // the first piece of the text when it is split across chunks
            b"<root/>t",
            b"",
        ];
        let f = Fixture::correct(schema);
        for doc in docs {
            let mut v = Case::new(f.schema.clone());
            let whole = crate::validate_slice(&mut v.v, doc);
            let expected = (whole, format!("{:?}", v.errors.take()));
            for size in 1..=doc.len().max(1) {
                assert_eq!(
                    push_in_chunks(&mut v, doc, size),
                    expected,
                    "chunk size {size}, doc {}",
                    String::from_utf8_lossy(doc)
                );
            }
        }
    }

    #[test]
    fn push_validation_reports_invalid_before_the_end() {
        let f = Fixture::correct("start = element root { element x { empty }* }");
        let mut v = Case::new(f.schema.clone());
        // The buffer ends partway through a tag, which is left for the next call
        assert_eq!(v.feed(b"<root><x/><y"), Ok(10));
        assert!(!v.is_invalid());
        // The validator recovers from the unexpected element, so carries on, but the document
        // is now known to be invalid
        assert_eq!(v.feed(b"<y/><x"), Ok(4));
        assert!(v.is_invalid());
        // The caller stops here, having learned what it needs, without finishing the document
        v.reset();
        assert_eq!(v.errors.borrow().len(), 1);

        // The validator can then be reused for another document
        v.errors.take();
        assert!(!v.is_invalid());
        assert_eq!(v.feed(b"<root><x/></root>"), Ok(17));
        assert!(!v.is_invalid());
        assert_eq!(v.finish(b""), Ok(()));
    }

    #[test]
    fn push_validation_ignores_input_after_fatal_error() {
        let f = Fixture::correct("start = element root { empty }");
        let mut v = Case::new(f.schema.clone());
        assert_eq!(v.feed(b"<root></wrong>"), Err(super::Invalid));
        assert_eq!(v.feed(b"<more"), Err(super::Invalid));
        assert_eq!(v.finish(b"<more"), Err(super::Invalid));
        let errors = v.errors.take();
        assert_eq!(errors.len(), 1, "got {errors:?}");
        assert_matches!(errors[0], super::ValidatorError::MismatchedEndTag { .. });
    }

    #[test]
    fn read_failure_is_returned_rather_than_passed_to_sink() {
        struct Failing;
        impl io::Read for Failing {
            fn read(&mut self, _: &mut [u8]) -> io::Result<usize> {
                Err(io::Error::other("disk on fire"))
            }
        }
        let f = Fixture::correct("start = element root { empty }");
        let mut v = Case::new(f.schema.clone());
        assert_matches!(
            crate::validate_reader(&mut v.v, Failing),
            Err(super::ReadError::Io(_))
        );
        assert!(v.errors.borrow().is_empty());
    }

    #[test]
    fn name_longer_than_read_buffer_is_a_read_error_not_a_validation_error() {
        // Unlike text and attribute values, which are passed on in pieces, a name must be read
        // whole
        let f = Fixture::correct("start = element * { empty }");
        let mut v = Case::new(f.schema.clone());
        let doc = format!("<{}/>", "x".repeat(100));
        assert_matches!(
            crate::validate_reader_with_capacity(&mut v.v, &mut doc.as_bytes(), 64),
            Err(super::ReadError::BufferTooSmall { .. })
        );
        assert!(v.errors.borrow().is_empty());
    }

    /// Validates `doc`, which is expected to be well-formed, against `schema`.
    fn validate_doc(schema: &str, doc: &str) -> Result<(), Vec<crate::ValidatorError>> {
        let f = Fixture::correct(schema);
        Case::new(f.schema.clone()).validate_all(doc.as_bytes())
    }

    #[test]
    fn predefined_entities_are_expanded() {
        let doc = r#"<r v="&lt;&gt;&amp;&apos;&quot;">&lt;&gt;&amp;&apos;&quot;</r>"#;
        assert_matches!(
            validate_doc(
                r#"start = element r { attribute v { "<>&'" ~ '"' }, "<>&'" ~ '"' }"#,
                doc
            ),
            Ok(())
        );
    }

    #[test]
    fn entity_declared_in_dtd_is_undefined() {
        // The DOCTYPE is skipped, including the `]>` in the literal
        let doc = r#"<!DOCTYPE r [<!ENTITY e "]>">]><r v="&e;">&e;</r>"#;
        let errors =
            validate_doc("start = element r { attribute v { text }, text }", doc).unwrap_err();
        assert_matches!(
            &errors[..],
            [super::ValidatorError::UndefinedEntity { name, span }, ..]
                if name == "e" && doc[span.start as usize..span.end as usize] == *"e"
        );
    }

    #[test]
    fn attribute_value_at_limit_is_allowed() {
        let value = "x".repeat(super::infoset::MAX_ATTRIBUTE_VALUE);
        let doc = format!(r#"<r v="{value}"/>"#);
        let schema = "start = element r { attribute v { text } }";
        assert_matches!(validate_doc(schema, &doc), Ok(()));
    }

    #[test]
    fn attribute_value_over_limit_is_rejected() {
        let max = super::infoset::MAX_ATTRIBUTE_VALUE;
        let schema = "start = element r { attribute v { text } }";
        let long = "x".repeat(max);
        for doc in [
            format!(r#"<r v="{long}x"/>"#),
            format!(r#"<r v="{long}&#120;"/>"#),
            format!(r#"<r v="{long}&amp;"/>"#),
        ] {
            let errors = validate_doc(schema, &doc).unwrap_err();
            assert_matches!(
                &errors[..],
                [super::ValidatorError::AttributeValueTooLong { span }]
                    if doc[span.start as usize..span.end as usize] == *"v"
            );
        }
    }

    #[test]
    fn attribute_value_read_in_pieces_is_limited() {
        let f = Fixture::correct("start = element r { attribute v { text } }");
        let mut case = Case::new(f.schema.clone());
        let doc = format!(
            r#"<r v="{}"/>"#,
            "x".repeat(super::infoset::MAX_ATTRIBUTE_VALUE + 1)
        );
        let result = crate::validate_reader_with_capacity(&mut case.v, &mut doc.as_bytes(), 4096);
        assert_matches!(result, Err(_));
        assert_matches!(
            case.errors.borrow()[..],
            [super::ValidatorError::AttributeValueTooLong { .. }]
        );
    }

    #[test]
    fn valid_document_reports_nothing_to_sink() {
        let f = Fixture::correct("start = element root { empty }");
        let mut v = Validator::new(f.schema.clone(), |_: &_, err| {
            panic!("unexpected error {err:?}")
        })
        .unwrap();
        assert!(crate::validate_reader(&mut v, &b"<root/>"[..]).is_ok());
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
        let mut v = Case::new(f.schema.clone());
        let (_, diagnostics) = first_diagnostics(&mut v, xml.as_bytes());
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
        let mut v = Case::new(f.schema.clone());
        let (_, diagnostics) = first_diagnostics(&mut v, xml.as_bytes());
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
        let mut v = Case::new(f.schema.clone());
        v.validate_all(b"<a/>").unwrap();
        assert!(v.coverage_report().is_none());
    }

    #[test]
    fn invalid_element_name_reported_as_invalid_name() {
        // Schema accepts any element name, but "b!ad" is not a valid XML NCName
        // (xml-syntax-reader accepts '!' in names since it's not a name-terminator)
        let f = Fixture::correct("start = element * { empty }");
        let mut v = Case::new(f.schema.clone());
        let err = v.validate_all(b"<b!ad/>").unwrap_err().remove(0);
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
        let mut v = Case::new(f.schema.clone());
        let err = v.validate_all(b"<1bad/>").unwrap_err().remove(0);
        assert_matches!(err, super::ValidatorError::InvalidName { .. });
    }

    #[test]
    fn invalid_attribute_name_reported_as_invalid_name() {
        // Schema requires an attribute with a specific name plus allows any others;
        // "b@d" is not a valid NCName
        let f = Fixture::correct(
            "start = element a { attribute x { text }, attribute * - x { text }* }",
        );
        let mut v = Case::new(f.schema.clone());
        let err = v
            .validate_all(b"<a x=\"v\" b@d=\"x\"/>")
            .unwrap_err()
            .remove(0);
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
        let mut v = Case::new(f.schema.clone());
        let err = v
            .validate_all(b"<a x=\"1\" x=\"2\"/>")
            .unwrap_err()
            .remove(0);
        assert_matches!(err, super::ValidatorError::DuplicateAttribute { .. });
    }

    #[test]
    fn duplicate_expanded_attribute_name_rejected() {
        let f = Fixture::correct("start = element e { attribute * { text }* }");
        let mut v = Case::new(f.schema.clone());
        let doc = b"<e xmlns:a=\"u\" xmlns:b=\"u\" a:x=\"1\" b:x=\"2\"/>";
        let err = v.validate_all(doc).unwrap_err().remove(0);
        assert_matches!(
            err,
            super::ValidatorError::DuplicateAttribute { span } if span == (35..38)
        );
    }

    #[test]
    fn same_local_name_in_different_namespaces_accepted() {
        let f = Fixture::correct("start = element e { attribute * { text }* }");
        let mut v = Case::new(f.schema.clone());
        v.validate_all(b"<e xmlns:a=\"u\" xmlns:b=\"v\" x=\"0\" a:x=\"1\" b:x=\"2\"/>")
            .unwrap();
    }

    #[test]
    fn duplicate_xmlns_rejected() {
        let f = Fixture::correct("default namespace = \"urn:x\" start = element a { empty }");
        let mut v = Case::new(f.schema.clone());
        let err = v
            .validate_all(b"<a xmlns=\"urn:x\" xmlns=\"urn:y\"/>")
            .unwrap_err()
            .remove(0);
        assert_matches!(err, super::ValidatorError::DuplicateAttribute { .. });
    }

    #[test]
    fn mismatched_end_tag_rejected() {
        let f = Fixture::correct("start = element a { empty }");
        let mut v = Case::new(f.schema.clone());
        let err = v.validate_all(b"<a></b>").unwrap_err().remove(0);
        assert_matches!(
            err,
            super::ValidatorError::MismatchedEndTag { span, start: Some(start) }
                if span == (5..6) && start == (1..2)
        );
    }

    #[test]
    fn end_tag_prefix_must_match() {
        // The names must match literally, even when both prefixes are bound to the same URI.
        let f = Fixture::correct("namespace x = \"urn:x\" start = element x:a { empty }");
        let mut v = Case::new(f.schema.clone());
        let err = v
            .validate_all(b"<p:a xmlns:p=\"urn:x\" xmlns:q=\"urn:x\"></q:a>")
            .unwrap_err()
            .remove(0);
        assert_matches!(err, super::ValidatorError::MismatchedEndTag { .. });
    }

    #[test]
    fn stray_end_tag_rejected() {
        let f = Fixture::correct("start = element a { empty }");
        let mut v = Case::new(f.schema.clone());
        let errs = v.validate_all(b"<a/></a>").unwrap_err();
        assert_matches!(
            errs.last(),
            Some(super::ValidatorError::MismatchedEndTag { start: None, .. })
        );
    }

    #[test]
    fn unclosed_element_rejected() {
        let f = Fixture::correct("start = element a { element b { empty } }");
        let mut v = Case::new(f.schema.clone());
        let err = v.validate_all(b"<a><b>").unwrap_err().remove(0);
        assert_matches!(
            err,
            super::ValidatorError::UnclosedElement { span } if span == (4..5)
        );
    }

    #[test]
    fn missing_root_element_rejected() {
        let f = Fixture::correct("start = element a { empty }");
        for doc in ["", " \n", "<?xml version=\"1.0\"?><!-- c -->"] {
            let mut v = Case::new(f.schema.clone());
            let err = v.validate_all(doc.as_bytes()).unwrap_err().remove(0);
            assert_matches!(err, super::ValidatorError::MissingRootElement, "{doc:?}");
        }
    }

    #[test]
    fn content_outside_root_rejected() {
        // `element a { text }*` would accept these as content, were they inside an element
        let f = Fixture::correct("start = element a { empty }");
        for (doc, expected) in [
            ("<a/><a/>", "element"),
            ("x<a/>", "text"),
            ("<a/>x", "text"),
            ("<a/>&amp;", "reference"),
            ("<a/>&#32;", "reference"),
            ("<a/><![CDATA[]]>", "CDATA section"),
        ] {
            let mut v = Case::new(f.schema.clone());
            let err = v.validate_all(doc.as_bytes()).unwrap_err().remove(0);
            assert_matches!(
                err,
                super::ValidatorError::OutsideRootElement { kind, .. } if kind == expected,
                "{doc:?}"
            );
        }
    }

    #[test]
    fn misc_outside_root_accepted() {
        let f = Fixture::correct("start = element a { empty }");
        let mut v = Case::new(f.schema.clone());
        v.validate_all(b"<?xml version=\"1.0\"?>\n<!-- c --> <?pi?>\r\n<a/>\t<!-- c --><?pi?>\n")
            .unwrap();
    }

    #[test]
    fn validator_reusable_after_each_document() {
        let f = Fixture::correct("start = element a { element b { empty } }");
        let mut v = Case::new(f.schema.clone());
        v.validate_all(b"<a><b/></a>").unwrap();
        v.validate_all(b"<a><b/></a>").unwrap();
        v.validate_all(b"<a><b>").unwrap_err();
        v.validate_all(b"<a><b/></a>").unwrap();
    }

    #[test]
    fn invalid_utf8_in_text_rejected() {
        // Schema permits any text — we must still reject malformed UTF-8.
        let f = Fixture::correct("start = element a { text }");
        let mut v = Case::new(f.schema.clone());
        // 0xC0 is an invalid UTF-8 lead byte (overlong encoding).
        let bad = b"<a>hello\xC0world</a>";
        let err = v.validate_all(bad).unwrap_err().remove(0);
        assert_matches!(err, super::ValidatorError::InvalidUtf8 { kind: "text", .. });
    }

    #[test]
    fn invalid_utf8_in_text_under_fixed_point_pattern_rejected() {
        // the validator ignores the content entirely when matching `text`;
        // invalid UTF-8 must still be reported, as the document is not well-formed
        let f = Fixture::correct("start = element a { text }");
        let mut v = Case::new(f.schema.clone());
        // Lone continuation byte — invalid UTF-8.
        let bad = b"<a>\x80</a>";
        let err = v.validate_all(bad).unwrap_err().remove(0);
        assert_matches!(err, super::ValidatorError::InvalidUtf8 { kind: "text", .. });
    }

    #[test]
    fn invalid_utf8_in_cdata_rejected() {
        let f = Fixture::correct("start = element a { text }");
        let mut v = Case::new(f.schema.clone());
        let bad = b"<a><![CDATA[\xFF]]></a>";
        let err = v.validate_all(bad).unwrap_err().remove(0);
        assert_matches!(
            err,
            super::ValidatorError::InvalidUtf8 {
                kind: "CDATA content",
                ..
            }
        );
    }

    #[test]
    fn cdata_with_leading_whitespace() {
        let f = Fixture::correct("start = element Data { element a { xsd:string } }");
        f.valid("<Data>\n    <a>\n      <![CDATA[my_test_data]]>\n    </a>\n</Data>");
        f.valid("<Data><a> <![CDATA[my_test_data]]></a></Data>");
        f.valid("<Data><a><![CDATA[my_test_data]]> </a></Data>");
    }

    #[test]
    fn invalid_utf8_in_attribute_value_rejected() {
        // Schema accepts any text in attribute — must still reject malformed UTF-8.
        let f = Fixture::correct("start = element a { attribute x { text } }");
        let mut v = Case::new(f.schema.clone());
        let bad = b"<a x=\"hi\xC0there\"/>";
        let err = v.validate_all(bad).unwrap_err().remove(0);
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
        let mut v = Case::new(f.schema.clone());
        let bad = b"<a xmlns:foo=\"urn:\xFFbad\"/>";
        let err = v.validate_all(bad).unwrap_err().remove(0);
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
        let mut v = Case::new(f.schema.clone());
        let xml = b"<a>nope</a>";
        let err = v.validate_all(xml).unwrap_err().remove(0);
        let span = match err {
            super::ValidatorError::NotAllowed {
                span, kind: "text", ..
            } => span,
            other => panic!("expected NotAllowed text error, got {other:?}"),
        };
        // "nope" starts at byte 3 in the document.
        assert_eq!(span.start, 3, "span start should point at the text");
        assert_eq!(span.end, 7, "span end should be after the text");
    }
}
