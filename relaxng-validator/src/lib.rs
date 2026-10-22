mod coverage;
mod derivative;
mod nameclass;
mod schema;

use relaxng_model::model;
use relaxng_model::model::NameClass;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use xmlparser::{ElementEnd, EntityDefinition, StrSpan, Token, Tokenizer};

pub use coverage::{CoverageReport, TrackablePattern};
// Re-exported so downstream crates can match `ValidatorError::Schema(..)` exhaustively.
pub use schema::SchemaError;

use nameclass::*;
use schema::*;

#[derive(Debug)]
pub enum ValidatorError<'a> {
    Xml(xmlparser::Error),
    NotAllowed(Token<'a>),
    UndefinedNamespacePrefix {
        prefix: StrSpan<'a>,
    },
    UndefinedEntity {
        name: &'a str,
        span: std::ops::Range<usize>,
    },
    InvalidOrUnclosedEntity {
        span: std::ops::Range<usize>,
    },
    TextBufferOverflow,
    Schema(SchemaError),
}

/// The outcome of a failed attempt to close an element's start tag (see
/// [`Validator::close_element_start`]).
struct CloseStartError<'b> {
    error: ValidatorError<'b>,
    /// A position to continue validating from despite the error, when a sound one exists.
    /// `None` means the caller must leave its current position untouched.
    recovery: Option<PatId>,
}

impl<'b> CloseStartError<'b> {
    /// An error with no recovery position — the caller must leave its current position
    /// untouched.
    fn unrecoverable(error: ValidatorError<'b>) -> Self {
        CloseStartError {
            error,
            recovery: None,
        }
    }
}

pub struct Validator<'a> {
    schema: Schema,
    tokenizer: Tokenizer<'a>,
    current_step: PatId,
    last_was_start_element: bool,
    stack: ElementStack<'a>,
    entity_definitions: HashMap<String, String>,
    text_buffer: String,
    max_text_buffer: usize,
}

impl<'a> Validator<'a> {
    pub fn new(
        model: Rc<RefCell<Option<model::DefineRule>>>,
        tokenizer: Tokenizer<'a>,
    ) -> Result<Validator<'a>, ValidatorError<'a>> {
        let (schema, start) = build_schema(&model).map_err(ValidatorError::Schema)?;
        let mut entity_definitions = HashMap::default();
        entity_definitions.insert("lt".to_string(), "<".to_string());
        entity_definitions.insert("gt".to_string(), ">".to_string());
        entity_definitions.insert("amp".to_string(), "&".to_string());
        entity_definitions.insert("apos".to_string(), "'".to_string());
        entity_definitions.insert("quot".to_string(), "\"".to_string());
        Ok(Validator {
            schema,
            tokenizer,
            current_step: start,
            last_was_start_element: false,
            stack: ElementStack::default(),
            entity_definitions,
            text_buffer: String::new(),
            max_text_buffer: 1024 * 1024,
        })
    }

    /// Create a validator with coverage tracking enabled.
    pub fn new_with_coverage(
        model: Rc<RefCell<Option<model::DefineRule>>>,
        tokenizer: Tokenizer<'a>,
    ) -> Result<Validator<'a>, ValidatorError<'a>> {
        let mut v = Self::new(model, tokenizer)?;
        let compile_time_count = v.schema.inner.borrow().patterns.len() as u16;
        let word_count = (compile_time_count as usize).div_ceil(64);
        v.schema.compile_time_count = compile_time_count;
        v.schema.coverage = Some(vec![0u64; word_count].into_boxed_slice());
        Ok(v)
    }

    /// Extract the coverage report. Returns `None` if coverage tracking was not enabled.
    pub fn coverage_report(&self) -> Option<CoverageReport> {
        self.schema.build_coverage_report()
    }

    pub fn validate_next(&mut self) -> Option<Result<(), ValidatorError<'a>>> {
        match self.tokenizer.next() {
            Some(Ok(evt)) => Some(self.validate(evt)),
            Some(Err(err)) => Some(Err(ValidatorError::Xml(err))),
            None => None,
        }
    }

    /// Ensure ns_context is up to date if it has been invalidated.
    fn ensure_ns_context(&mut self) {
        if self.schema.ns_context_dirty {
            self.schema.ns_context = if self.stack.elements.is_empty() {
                None
            } else {
                Some(self.stack.capture_ns_context())
            };
            self.schema.ns_context_dirty = false;
        }
    }

    /// Flush any buffered text by applying text_deriv.
    /// Returns true if the result was NotAllowed.
    fn flush_text(&mut self) -> bool {
        if !self.text_buffer.is_empty() {
            let next_id = self.schema.text_deriv(self.current_step, &self.text_buffer);
            self.text_buffer.clear();
            self.current_step = next_id;
            matches!(self.schema.patt(next_id), Pat::NotAllowed)
        } else {
            false
        }
    }

    #[allow(unused)]
    fn assert_health(&self) {
        let mut seen = vec![];
        self.schema.check_choices(self.current_step, &mut seen);
    }

    fn validate(&mut self, evt: Token<'a>) -> Result<(), ValidatorError<'a>> {
        self.ensure_ns_context();
        let new = match evt {
            Token::EmptyDtd { .. }
            | Token::Comment { .. }
            | Token::ProcessingInstruction { .. } => {
                // does not change current_step state
                return Ok(());
            }
            Token::ElementStart {
                prefix,
                local,
                span,
            } => {
                if self.flush_text() {
                    return Err(ValidatorError::NotAllowed(evt));
                }
                self.stack.push(prefix, local, span);
                // does not change current_step state
                return Ok(());
            }
            /*
                let next_pat = Self::start_tag_open_deriv(pat, &mut self.schema, namespace, &name);
                // TODO: refactor early-returns
                let next_pat = match self.schema.patt(next_pat) {
                    Pat::NotAllowed => return Err(ValidatorError::NotAllowed(evt)),
                    p => Self::attrs_deriv(next_pat, &mut self.schema, attributes)
                };
                let next_pat = match self.schema.patt(next_pat) {
                    Pat::NotAllowed => return Err(ValidatorError::NotAllowed(evt)),
                    p => Self::start_tag_close_deriv(next_pat, &mut self.schema)
                };
                match self.schema.patt(next_pat) {
                    Pat::NotAllowed => return Err(ValidatorError::NotAllowed(evt)),
                    p => next_pat //Self::children_deriv(next_pat, &mut self.schema)
                }
            */
            Token::Attribute {
                prefix,
                local,
                value,
                span,
            } => {
                if prefix.as_str() == "xmlns"
                    || (prefix.as_str() == "" && local.as_str() == "xmlns")
                {
                    self.schema.ns_context_dirty = true;
                }
                self.stack.add_attr(prefix, local, value, span);
                // does not change current_step state
                return Ok(());
            }
            Token::ElementEnd { end, span: _ } => {
                if self.flush_text() {
                    return Err(ValidatorError::NotAllowed(evt));
                }
                match end {
                    ElementEnd::Open => match Self::close_element_start(
                        &self.stack,
                        &mut self.schema,
                        evt,
                        self.current_step,
                    ) {
                        Ok(next_pat) => next_pat,
                        Err(CloseStartError { error, recovery }) => {
                            if let Some(recovered) = recovery {
                                self.current_step = recovered;
                                self.last_was_start_element = true;
                            }
                            return Err(error);
                        }
                    },
                    ElementEnd::Close(_, _) => {
                        let result = if self.last_was_start_element {
                            // The element had no child elements or text nodes, so complete it
                            // through the same empty-content rule as a self-closing tag.
                            self.schema.close_empty_element(self.current_step, false)
                        } else {
                            self.schema.end_tag_deriv(self.current_step)
                        };
                        if self.stack.pop_has_namespaces() {
                            self.schema.ns_context_dirty = true;
                        }
                        result
                    }
                    ElementEnd::Empty => {
                        let result = match Self::close_element_start(
                            &self.stack,
                            &mut self.schema,
                            evt,
                            self.current_step,
                        ) {
                            Ok(next_id) => self.schema.close_empty_element(next_id, false),
                            Err(CloseStartError { error, recovery }) => {
                                if let Some(recovered) = recovery {
                                    // `recovered` sits inside the element, but the tag is
                                    // already complete, so close it too — otherwise following
                                    // siblings would be matched at the wrong depth. Force
                                    // through any outstanding content requirements (e.g. a
                                    // missing required child) as well, since this element's
                                    // own error has already been reported and there is nothing
                                    // more to gain from cascading that into every sibling. If
                                    // even that can't produce a sound position, leave it alone.
                                    let closed = self.schema.close_empty_element(recovered, true);
                                    if !matches!(self.schema.patt(closed), Pat::NotAllowed) {
                                        self.current_step = closed;
                                        self.last_was_start_element = false;
                                    }
                                }
                                // The start tag was consumed regardless of the error, so the
                                // element stack has to be unwound to keep it balanced.
                                if self.stack.pop_has_namespaces() {
                                    self.schema.ns_context_dirty = true;
                                }
                                return Err(error);
                            }
                        };
                        if self.stack.pop_has_namespaces() {
                            self.schema.ns_context_dirty = true;
                        }
                        result
                    }
                }
            }
            Token::Cdata { text, span: _ } => {
                if self.flush_text() {
                    return Err(ValidatorError::NotAllowed(evt));
                }
                let mixed = self.schema.mixed_text_deriv(self.current_step);
                if mixed == self.current_step {
                    self.current_step
                } else {
                    self.schema.text_deriv(self.current_step, &text)
                }
            }
            Token::Text { text } => {
                let mut buffer = String::new();
                for val in parse_entities(text.start(), text.as_str()) {
                    match val {
                        Ok(val) => {
                            let txt = match val {
                                Txt::Text(_pos, val) => val,
                                Txt::Entity(pos, name) => {
                                    if let Some(txt) = self.entity_definitions.get(name) {
                                        txt
                                    } else {
                                        return Err(ValidatorError::UndefinedEntity {
                                            name,
                                            span: pos..pos + name.len(),
                                        });
                                    }
                                }
                                Txt::Char(_pos, val) => {
                                    buffer.push(val);
                                    continue;
                                }
                            };
                            // we only reach this point for Txt::Text and Txt::Entity cases,
                            if txt.len() == text.len() {
                                // no need to copy data into the buffer, just process the whole input in one go
                                break;
                            } else {
                                // the input contains entities, so we decode these and append to buffer
                                buffer.push_str(txt);
                            }
                        }
                        Err(err) => {
                            return Err(err);
                        }
                    }
                }
                let data = if buffer.is_empty() {
                    text.as_str()
                } else {
                    &buffer[..]
                };
                // Fast path: if mixed_text_deriv returns the same PatId, the pattern is a
                // text fixed-point (e.g. After(Text, cont)) and text_deriv is a no-op.
                let mixed = self.schema.mixed_text_deriv(self.current_step);
                if mixed == self.current_step {
                    self.current_step
                } else {
                    // Buffer text for deferred processing — text fragments split by
                    // PIs/comments must be concatenated before validation (spec 6.2.7).
                    self.text_buffer.push_str(data);
                    if self.text_buffer.len() > self.max_text_buffer {
                        return Err(ValidatorError::TextBufferOverflow);
                    }
                    self.last_was_start_element = false;
                    return Ok(());
                }
            }
            Token::EntityDeclaration {
                name,
                definition,
                span: _,
            } => {
                match definition {
                    EntityDefinition::EntityValue(val) => {
                        self.entity_definitions
                            .insert(name.to_string(), val.to_string());
                        // does not change current_step state
                        return Ok(());
                    }
                    EntityDefinition::ExternalId(_) => {
                        // no support for resolving external ids
                        // does not change current_step state
                        return Err(ValidatorError::NotAllowed(evt));
                    }
                }
            }
            Token::Declaration { .. } | Token::DtdStart { .. } | Token::DtdEnd { .. } => {
                // does not change current_step state
                return Ok(());
            }
        };
        if let Token::ElementEnd {
            end: ElementEnd::Open,
            ..
        } = evt
        {
            self.last_was_start_element = true;
        } else {
            self.last_was_start_element = false;
        }
        if let Pat::NotAllowed = self.schema.patt(new) {
            Err(ValidatorError::NotAllowed(evt))
        } else {
            self.current_step = new;
            Ok(())
        }
    }

    /// Derive the pattern across an element's start tag.
    ///
    /// On failure, [`CloseStartError::recovery`] is a *recovery* pattern: `Some(pid)` means the
    /// caller should still advance to `pid` despite reporting the error, so that the element's
    /// children are validated against the element's own content model rather than against the
    /// position the element itself was expected at. `None` means there is no sound state to
    /// continue from and the caller should leave its position untouched.
    ///
    /// See [`CloseStartError`] for the return type.
    fn close_element_start<'b: 'a>(
        stack: &ElementStack<'b>,
        schema: &mut Schema,
        evt: Token<'b>,
        pat_id: PatId,
    ) -> Result<PatId, CloseStartError<'b>> {
        let elem = stack
            .current_element()
            .map_err(CloseStartError::unrecoverable)?;
        let next_pat = schema.start_tag_open_deriv(pat_id, elem.name);
        if let Pat::NotAllowed = schema.patt(next_pat) {
            // The element name itself is not allowed here, so there is no content model to
            // descend into and nothing to recover to.
            return Err(CloseStartError::unrecoverable(ValidatorError::NotAllowed(
                Token::ElementStart {
                    prefix: elem.raw_prefix,
                    local: elem.raw_local,
                    span: elem.raw_local,
                },
            )));
        }

        // A rejected attribute is recorded but does not abandon the derivation: the
        // remaining attributes are still applied, so that an element with one bad attribute
        // still yields accurate diagnostics for its children.  Only the first bad attribute
        // is reported, to keep one error per start tag.
        let attributes = stack
            .current_attributes()
            .map_err(CloseStartError::unrecoverable)?;
        let mut pat = next_pat;
        let mut first_attr_error = None;
        for att in attributes {
            let mid = schema.start_att_deriv(pat, att.name);
            let derived = schema.att_value_deriv(mid, att.value);
            if let Pat::NotAllowed = schema.patt(derived) {
                if first_attr_error.is_none() {
                    first_attr_error = Some(ValidatorError::NotAllowed(Token::Attribute {
                        prefix: att.raw_prefix,
                        local: att.raw_local,
                        value: att.raw_value,
                        span: att.raw_span,
                    }));
                }
                // Leave `pat` alone: carry on as though the attribute were absent.
            } else {
                pat = derived;
            }
        }

        let closed = schema.start_tag_close_deriv(pat);
        if !matches!(schema.patt(closed), Pat::NotAllowed) {
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
        let error = first_attr_error.unwrap_or(ValidatorError::NotAllowed(evt));
        let recovered = schema.force_start_tag_close_deriv(pat);
        let recovery = (!matches!(schema.patt(recovered), Pat::NotAllowed)).then_some(recovered);
        Err(CloseStartError { error, recovery })
    }

    #[allow(clippy::mutable_key_type)] // false-positive
    fn heads(&self, id: PatId) -> HashSet<Pat> {
        let mut result = HashSet::new();
        self.head(&mut result, id);
        result
    }
    #[allow(clippy::mutable_key_type)] // false-positive
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
        #[allow(clippy::mutable_key_type)] // false-positive
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
                self.describe_nameclass(nameclass, &mut desc);
                result.push_str(&desc);
            }
        }
        if rest > 0 {
            result.push_str(&format!(" .. or one of {rest} more"))
        }
        // TODO: plus attributes and everything else
        result
    }
    fn describe_nameclass(&self, nc: &NameClass, desc: &mut String) {
        describe_nameclass(nc, desc);
    }

    pub fn diagnostic(
        &self,
        name: String,
        source: String,
        err: &ValidatorError,
    ) -> (codemap::CodeMap, Vec<codemap_diagnostic::Diagnostic>) {
        let mut map = codemap::CodeMap::new();
        let file = map.add_file(name, source);
        let mut diagnostics = vec![];
        match err {
            ValidatorError::Xml(err) => {
                let line = file.line_span(err.pos().row as _);
                let span = line.subspan(err.pos().row as _, err.pos().row as _);

                let label = codemap_diagnostic::SpanLabel {
                    span,
                    label: None,
                    style: codemap_diagnostic::SpanStyle::Primary,
                };

                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("{err}"),
                    code: None,
                    spans: vec![label],
                });
            }
            ValidatorError::NotAllowed(tok) => {
                let span = match tok {
                    Token::Declaration { span, .. }
                    | Token::ProcessingInstruction { span, .. }
                    | Token::Comment { span, .. }
                    | Token::DtdStart { span, .. }
                    | Token::EmptyDtd { span, .. }
                    | Token::EntityDeclaration { span, .. }
                    | Token::DtdEnd { span, .. }
                    | Token::ElementStart { span, .. }
                    | Token::Attribute { span, .. }
                    | Token::ElementEnd { span, .. }
                    | Token::Cdata { span, .. } => span,
                    Token::Text { text } => text,
                };
                let name = match tok {
                    Token::Declaration { .. } => "declaration",
                    Token::ProcessingInstruction { .. } => "processing-instruction",
                    Token::Comment { .. } => "comment",
                    Token::DtdStart { .. } => "dtd-start",
                    Token::EmptyDtd { .. } => "empty-dtd",
                    Token::EntityDeclaration { .. } => "entity-declaration",
                    Token::DtdEnd { .. } => "dtd-end",
                    Token::ElementStart { .. } => "element-start",
                    Token::Attribute { .. } => "attribute",
                    Token::ElementEnd { end: _, .. } => "element-end",
                    Token::Text { .. } => "text",
                    Token::Cdata { .. } => "cdata",
                };
                let label = codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(span.start() as _, span.end() as _),
                    label: Some("Not allowed".to_string()),
                    style: codemap_diagnostic::SpanStyle::Primary,
                };

                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("{name} not expected here"),
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
            ValidatorError::UndefinedNamespacePrefix { prefix } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(prefix.start() as _, prefix.end() as _),
                    label: Some(format!(
                        "Add an xmlns:{}=\"..\" attribute to define this prefix",
                        prefix.as_str()
                    )),
                    style: codemap_diagnostic::SpanStyle::Primary,
                };

                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("The prefix {:?} is not defined", prefix.as_str()),
                    code: None,
                    spans: vec![label],
                })
            }
            ValidatorError::UndefinedEntity { name, span } => {
                let label = codemap_diagnostic::SpanLabel {
                    span: file.span.subspan(span.start as _, span.end as _),
                    label: Some("undefined".to_string()),
                    style: codemap_diagnostic::SpanStyle::Primary,
                };

                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: format!("The entity &{name:?}; is not defined"),
                    code: None,
                    spans: vec![label],
                })
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
                })
            }
            ValidatorError::TextBufferOverflow => {
                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: "Text content exceeds maximum buffer size".to_string(),
                    code: None,
                    spans: vec![],
                })
            }
            ValidatorError::Schema(SchemaError::TooManyPatterns) => {
                diagnostics.push(codemap_diagnostic::Diagnostic {
                    level: codemap_diagnostic::Level::Error,
                    message: "Schema exceeds maximum number of patterns (65535)".to_string(),
                    code: None,
                    spans: vec![],
                })
            }
        }
        (map, diagnostics)
    }
}

#[derive(Debug)]
enum Txt<'a> {
    Text(usize, &'a str),
    Entity(usize, &'a str),
    Char(usize, char),
}

fn parse_entities(
    pos: usize,
    text: &str,
) -> impl Iterator<Item = Result<Txt<'_>, ValidatorError<'_>>> {
    struct Entities<'a> {
        text: &'a str,
        pos: usize,
        offset: usize,
        in_entity: bool,
    }
    impl<'a> Iterator for Entities<'a> {
        type Item = Result<Txt<'a>, ValidatorError<'a>>;

        fn next(&mut self) -> Option<Self::Item> {
            if self.offset == self.text.len() {
                return None;
            }
            for (i, c) in self.text[self.offset..].char_indices() {
                if self.in_entity {
                    if c == ';' {
                        self.in_entity = false;
                        let text = &self.text[self.offset..self.offset + i];
                        let result = if let Some(text) = text.strip_prefix('#') {
                            numeric_entity(self.offset, text)
                        } else {
                            Ok(Txt::Entity(self.offset + self.pos, text))
                        };
                        self.offset += i + 1;
                        return Some(result);
                    }
                } else if c == '&' {
                    self.in_entity = true;
                    let result = Txt::Text(
                        self.offset + self.pos,
                        &self.text[self.offset..self.offset + i],
                    );
                    self.offset += i + 1;
                    return Some(Ok(result));
                }
            }
            if self.in_entity {
                Some(Err(ValidatorError::InvalidOrUnclosedEntity {
                    span: self.pos + self.offset - 1..self.pos + self.offset,
                }))
            } else {
                let result = Txt::Text(self.offset + self.pos, &self.text[self.offset..]);
                self.offset = self.text.len();
                Some(Ok(result))
            }
        }
    }
    fn numeric_entity(pos: usize, text: &str) -> Result<Txt<'_>, ValidatorError<'_>> {
        if text.is_empty() {
            return Err(ValidatorError::InvalidOrUnclosedEntity { span: pos..pos });
        }
        let c = if let Some(text) = text.strip_prefix('x') {
            let pos = pos + 1;
            if text.is_empty() {
                return Err(ValidatorError::InvalidOrUnclosedEntity { span: pos..pos });
            }
            u32::from_str_radix(text, 16)
                .map_err(|_e| ValidatorError::InvalidOrUnclosedEntity { span: pos..pos })?
        } else {
            text.parse()
                .map_err(|_e| ValidatorError::InvalidOrUnclosedEntity { span: pos..pos })?
        };
        Ok(Txt::Char(
            pos,
            std::char::from_u32(c)
                .ok_or(ValidatorError::InvalidOrUnclosedEntity { span: pos..pos })?,
        ))
    }
    Entities {
        text,
        pos,
        offset: 0,
        in_entity: false,
    }
}

#[derive(Default)]
struct ElementStack<'a> {
    elements: Vec<ElementState<'a>>,
}

impl<'a> ElementStack<'a> {
    fn lookup_namespace_uri(&self, prefix: &str) -> Option<StrSpan<'a>> {
        if prefix == "xml" {
            return Some(StrSpan::from("http://www.w3.org/XML/1998/namespace"));
        }
        self.elements
            .iter()
            .rev()
            .find_map(|elem| elem.lookup_namespace_uri(prefix))
    }

    fn capture_ns_context(&self) -> NsContext {
        let mut default_ns = String::new();
        let mut bindings = Vec::new();
        for elem in &self.elements {
            for ns in &elem.namespaces {
                if ns.prefix.as_str().is_empty() {
                    // Default namespace (xmlns="...")
                    default_ns = ns.namespace_uri.as_str().to_string();
                } else {
                    // Prefixed namespace (xmlns:prefix="...")
                    bindings.push((
                        ns.prefix.as_str().to_string(),
                        ns.namespace_uri.as_str().to_string(),
                    ));
                }
            }
        }
        NsContext {
            default_ns,
            bindings,
        }
    }

    fn resolve_element_namespace(
        &self,
        prefix: StrSpan<'a>,
    ) -> Result<Option<StrSpan<'a>>, ValidatorError<'a>> {
        if prefix.as_str() == "" {
            // For elements, empty prefix means look up the default namespace (xmlns="...")
            Ok(self.lookup_namespace_uri(""))
        } else {
            Ok(Some(self.lookup_namespace_uri(&prefix).ok_or(
                ValidatorError::UndefinedNamespacePrefix { prefix },
            )?))
        }
    }

    fn resolve_attribute_namespace(
        &self,
        prefix: StrSpan<'a>,
    ) -> Result<Option<StrSpan<'a>>, ValidatorError<'a>> {
        if prefix.as_str() == "" {
            // Per XML Namespaces spec, unprefixed attributes have no namespace
            Ok(None)
        } else {
            Ok(Some(self.lookup_namespace_uri(&prefix).ok_or(
                ValidatorError::UndefinedNamespacePrefix { prefix },
            )?))
        }
    }

    fn push(&mut self, prefix: StrSpan<'a>, local: StrSpan<'a>, _span: StrSpan<'a>) {
        self.elements.push(ElementState {
            prefix,
            local,
            namespaces: vec![],
            attributes: vec![],
        })
    }
    fn pop_has_namespaces(&mut self) -> bool {
        self.elements
            .pop()
            .is_some_and(|e| !e.namespaces.is_empty())
    }
    fn add_attr(
        &mut self,
        prefix: StrSpan<'a>,
        local: StrSpan<'a>,
        value: StrSpan<'a>,
        span: StrSpan<'a>,
    ) {
        if prefix.as_str() == "xmlns" {
            self.elements.last_mut().unwrap().namespaces.push(Ns {
                prefix: local,
                namespace_uri: value,
            })
        } else if prefix.as_str() == "" && local.as_str() == "xmlns" {
            self.elements.last_mut().unwrap().namespaces.push(Ns {
                prefix,
                namespace_uri: value,
            })
        } else {
            self.elements
                .last_mut()
                .unwrap()
                .attributes
                .push(UnresolvedAttr {
                    prefix,
                    local,
                    value,
                    span,
                })
        }
    }
    fn current_element(&self) -> Result<ResolvedElement<'a>, ValidatorError<'a>> {
        let curr = self.elements.last().unwrap();
        let namespace_uri = self.resolve_element_namespace(curr.prefix)?;
        Ok(ResolvedElement {
            name: QualifiedName {
                namespace_uri: namespace_uri.map(|s| s.as_str()),
                local_name: curr.local.as_str(),
            },
            raw_prefix: curr.prefix,
            raw_local: curr.local,
        })
    }
    fn current_attributes(&self) -> Result<Vec<ResolvedAttr<'a>>, ValidatorError<'a>> {
        self.elements
            .last()
            .unwrap()
            .attributes
            .iter()
            .map(move |unresolved| {
                let namespace_uri = self.resolve_attribute_namespace(unresolved.prefix)?;
                Ok(ResolvedAttr {
                    name: QualifiedName {
                        namespace_uri: namespace_uri.map(|s| s.as_str()),
                        local_name: unresolved.local.as_str(),
                    },
                    value: unresolved.value.as_str(),
                    raw_prefix: unresolved.prefix,
                    raw_local: unresolved.local,
                    raw_value: unresolved.value,
                    raw_span: unresolved.span,
                })
            })
            .collect()
    }
}

/// Resolved element with both &str QualifiedName (for derivatives) and raw StrSpan (for errors).
struct ResolvedElement<'a> {
    name: QualifiedName<'a>,
    raw_prefix: StrSpan<'a>,
    raw_local: StrSpan<'a>,
}

/// Resolved attribute with both &str QualifiedName (for derivatives) and raw StrSpan (for errors).
struct ResolvedAttr<'a> {
    name: QualifiedName<'a>,
    value: &'a str,
    raw_prefix: StrSpan<'a>,
    raw_local: StrSpan<'a>,
    raw_value: StrSpan<'a>,
    raw_span: StrSpan<'a>,
}

struct UnresolvedAttr<'a> {
    prefix: StrSpan<'a>,
    local: StrSpan<'a>,
    value: StrSpan<'a>,
    span: StrSpan<'a>,
}
struct ElementState<'a> {
    prefix: StrSpan<'a>,
    local: StrSpan<'a>,
    namespaces: Vec<Ns<'a>>,
    attributes: Vec<UnresolvedAttr<'a>>,
}

impl<'a> ElementState<'a> {
    fn lookup_namespace_uri(&self, prefix: &str) -> Option<StrSpan<'a>> {
        self.namespaces
            .iter()
            .find(|ns| ns.prefix.as_str() == prefix)
            .map(|ns| ns.namespace_uri)
    }
}

struct Ns<'a> {
    prefix: StrSpan<'a>,
    namespace_uri: StrSpan<'a>,
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
            let reader = xmlparser::Tokenizer::from(xml);
            let mut v = Validator::new(self.schema.clone(), reader).unwrap();
            while let Some(i) = v.validate_next() {
                if let Err(err) = i {
                    let (map, d) = v.diagnostic("valid.xml".to_string(), xml.to_string(), &err);
                    let mut emitter = codemap_diagnostic::Emitter::stderr(
                        codemap_diagnostic::ColorConfig::Auto,
                        Some(&map),
                    );
                    emitter.emit(&d[..]);
                    panic!("{err:?}");
                }
            }
        }

        fn valid_with_coverage(&self, xml: &str) -> super::CoverageReport {
            let reader = xmlparser::Tokenizer::from(xml);
            let mut v = Validator::new_with_coverage(self.schema.clone(), reader).unwrap();
            while let Some(i) = v.validate_next() {
                if let Err(err) = i {
                    let (map, d) = v.diagnostic("valid.xml".to_string(), xml.to_string(), &err);
                    let mut emitter = codemap_diagnostic::Emitter::stderr(
                        codemap_diagnostic::ColorConfig::Auto,
                        Some(&map),
                    );
                    emitter.emit(&d[..]);
                    panic!("{err:?}");
                }
            }
            v.coverage_report().expect("coverage should be enabled")
        }

        fn invalid(&self, xml: &str) {
            let reader = xmlparser::Tokenizer::from(xml);
            let mut v = Validator::new(self.schema.clone(), reader).unwrap();
            while let Some(i) = v.validate_next() {
                if let Err(_err) = i {
                    return;
                }
            }
            panic!("Invalid input was accepted by the validator")
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

        let reader = xmlparser::Tokenizer::from(doc);
        let mut v = Validator::new(schema.start, reader).unwrap();
        println!("====");
        v.schema.d(v.current_step).unwrap();
        println!("====");
        let mut fail = None;
        while let Some(i) = v.validate_next() {
            if let Err(err) = i {
                fail = Some(format!("{err:?}"));
                break;
            }
        }
        if let Some(err) = fail {
            return Err(format!("{err:?}"));
        }
        Ok(())
    }

    const RECIPE: &str = r#"start = element recipe {
        attribute servings { text },
        element title { text },
        element ingredient { text }+,
        element step { text }*
    }"#;

    /// Validate `doc` to the end, collecting every error together with the "Expected .."
    /// help text the diagnostics produce for it.
    fn all_errors(schema: &str, doc: &str) -> Vec<(String, String)> {
        let f = Fixture::correct(schema);
        let reader = xmlparser::Tokenizer::from(doc);
        let mut v = Validator::new(f.schema.clone(), reader).unwrap();
        let mut out = vec![];
        while let Some(i) = v.validate_next() {
            if let Err(err) = i {
                let (_, diagnostics) = v.diagnostic("doc".to_string(), doc.to_string(), &err);
                let help = diagnostics
                    .iter()
                    .find(|d| d.level == codemap_diagnostic::Level::Help)
                    .map(|d| d.message.clone())
                    .unwrap_or_default();
                out.push((format!("{err:?}"), help));
            }
        }
        out
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
        assert!(
            errs[0].0.contains("local: StrSpan(\"serving\" "),
            "expected the error to name the misspelled attribute 'serving' \
             (not the schema's 'servings'), got {errs:?}"
        );
    }

    /// The one error reported against a child element start tag.
    fn child_element_error(errs: &[(String, String)]) -> &(String, String) {
        match errs.iter().find(|(err, _)| err.contains("ElementStart")) {
            Some(e) => e,
            None => panic!("expected an error on a child element, got {errs:?}"),
        }
    }

    #[test]
    fn bad_attribute_reports_accurate_expectation_for_children() {
        // This document omits <title> too, so an error on <ingredient> is legitimate — but it
        // must describe recipe's content model rather than expecting <recipe> all over again.
        let errs = all_errors(
            RECIPE,
            r#"<recipe serving="2"><ingredient>i</ingredient></recipe>"#,
        );
        assert_eq!(
            child_element_error(&errs).1,
            "Expected Element title",
            "got {errs:?}"
        );
        assert!(
            !errs.iter().any(|(_, help)| help.contains("recipe")),
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
            child_element_error(&errs).1,
            "Expected Element title",
            "got {errs:?}"
        );
    }

    #[test]
    fn bad_attribute_on_empty_element_leaves_stack_balanced() {
        // The start tag is consumed whether or not it validates, so the element stack must
        // still be unwound — otherwise every later element is seen at the wrong depth.
        let f = Fixture::correct(
            "start = element root { element a { attribute x { text } }, element b { empty } }",
        );
        let doc = r#"<root><a y="1"/><b/></root>"#;
        let reader = xmlparser::Tokenizer::from(doc);
        let mut v = Validator::new(f.schema.clone(), reader).unwrap();
        let mut errors = vec![];
        while let Some(i) = v.validate_next() {
            if let Err(err) = i {
                errors.push(format!("{err:?}"));
            }
        }
        assert_eq!(
            errors.len(),
            1,
            "only the bad attribute should be reported, got {errors:?}"
        );
        assert_eq!(
            v.stack.elements.len(),
            0,
            "element stack still holds frames after the document ended"
        );
    }

    #[test]
    fn wholly_missing_attribute_does_not_cascade_into_children() {
        // Unlike a misspelled attribute, there is no bad `Attribute` token to blame here —
        // `servings` is simply never given. Recovery must still kick in so <title> and
        // <ingredient> are checked against recipe's real content model instead of cascading.
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
            !errs[0].1.contains("recipe"),
            "the single error should not still be expecting <recipe>, got {errs:?}"
        );
    }

    #[test]
    fn self_closing_element_missing_required_child_does_not_cascade() {
        // `<a y="1"/>` is invalid twice over (wrong attribute, missing required <b>), but
        // since it can never be closed even after forcing the attribute, recovery has to fall
        // back to forcing the element's content requirements too — otherwise the sibling <c/>
        // gets wrongly rejected against a's stale, never-advanced position.
        let f = Fixture::correct(
            "start = element root { \
                element a { attribute x { text }, element b { empty } }, \
                element c { empty } \
            }",
        );
        let doc = r#"<root><a y="1"/><c/></root>"#;
        let reader = xmlparser::Tokenizer::from(doc);
        let mut v = Validator::new(f.schema.clone(), reader).unwrap();
        let mut errors = vec![];
        while let Some(i) = v.validate_next() {
            if let Err(err) = i {
                errors.push(format!("{err:?}"));
            }
        }
        assert_eq!(
            errors.len(),
            1,
            "only the bad attribute on <a> should be reported, got {errors:?}"
        );
        assert!(
            errors[0].contains("\"y\""),
            "expected the one error to be about attribute 'y', got {errors:?}"
        );
    }

    #[test]
    fn abandoned_self_closing_recovery_does_not_let_a_later_element_steal_its_slot() {
        // `a` is required exactly once. The first `<a y="1"/>` is invalid (wrong attribute,
        // missing required <b>) and can't be recovered into a position *inside* a's content,
        // so recovery instead advances past `a` entirely. A second `<a>` must then be rejected
        // as an extra, disallowed element — not silently accepted as satisfying the same slot.
        let f = Fixture::correct(
            "start = element root { element a { attribute x { text }, element b { empty } } }",
        );
        let doc = r#"<root><a y="1"/><a x="1"><b/></a></root>"#;
        let reader = xmlparser::Tokenizer::from(doc);
        let mut v = Validator::new(f.schema.clone(), reader).unwrap();
        let mut errors = vec![];
        while let Some(i) = v.validate_next() {
            if let Err(err) = i {
                errors.push(format!("{err:?}"));
            }
        }
        assert!(
            errors.len() >= 2,
            "expected both the bad attribute and the illegal second <a> to be reported, got {errors:?}"
        );
        assert!(
            errors.iter().any(|e| e.contains("ElementStart")),
            "expected the second, schema-illegal <a> to be rejected outright, got {errors:?}"
        );
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
        let mut iter = super::parse_entities(0, "foo &bar; blat");
        assert_matches!(iter.next(), Some(Ok(super::Txt::Text(0, "foo "))));
        assert_matches!(iter.next(), Some(Ok(super::Txt::Entity(5, "bar"))));
        assert_matches!(iter.next(), Some(Ok(super::Txt::Text(9, " blat"))));
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
        let reader = xmlparser::Tokenizer::from(xml);
        let mut v = Validator::new(f.schema.clone(), reader).unwrap();
        let mut diagnostics = vec![];
        while let Some(i) = v.validate_next() {
            if let Err(err) = i {
                let (_, d) = v.diagnostic("test.xml".to_string(), xml.to_string(), &err);
                diagnostics.extend(d);
            }
        }
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
        let reader = xmlparser::Tokenizer::from(xml);
        let mut v = Validator::new(f.schema.clone(), reader).unwrap();
        let mut diagnostics = vec![];
        while let Some(i) = v.validate_next() {
            if let Err(err) = i {
                let (_, d) = v.diagnostic("test.xml".to_string(), xml.to_string(), &err);
                diagnostics.extend(d);
            }
        }
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
        let reader = xmlparser::Tokenizer::from("<a/>");
        let mut v = Validator::new(f.schema.clone(), reader).unwrap();
        while let Some(i) = v.validate_next() {
            i.unwrap();
        }
        assert!(v.coverage_report().is_none());
    }
}
