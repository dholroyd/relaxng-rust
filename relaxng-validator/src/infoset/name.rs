use std::cell::Cell;

/// Whether a [`Name`] is known to be a valid NCName.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum NameState {
    /// Nobody has checked the name yet.
    Unchecked,
    Valid,
    Invalid,
}

/// An element or attribute local name, as raw bytes from the document.
///
/// The NCName check (which includes the UTF-8 check) is deferred, because a handler that compares
/// the bytes with a name it already knows to be valid gets that check for free. Every name a
/// handler leaves [`NameState::Unchecked`] is checked by the reader once the handler returns, so
/// a name is only guaranteed well-formed after the event that carries it has been handled.
#[derive(Clone, Copy)]
pub(crate) struct Name<'a> {
    bytes: &'a [u8],
    state: &'a Cell<NameState>,
}

impl<'a> Name<'a> {
    pub(super) fn new(bytes: &'a [u8], state: &'a Cell<NameState>) -> Self {
        Name { bytes, state }
    }

    pub(crate) fn as_bytes(&self) -> &'a [u8] {
        self.bytes
    }

    /// Checks that the name is a valid NCName, caching the result. A handler that finds the name
    /// invalid this way is responsible for reporting it; the reader only reports names that no
    /// handler examined.
    pub(crate) fn is_valid(&self) -> bool {
        match self.state.get() {
            NameState::Valid => true,
            NameState::Invalid => false,
            NameState::Unchecked => {
                let valid = is_ncname(self.bytes);
                self.state.set(if valid {
                    NameState::Valid
                } else {
                    NameState::Invalid
                });
                valid
            }
        }
    }

    /// Records that the caller has established the name is a valid NCName, for instance because
    /// its bytes equal a name taken from a schema, so the reader need not check it.
    pub(crate) fn mark_valid(&self) {
        self.state.set(NameState::Valid);
    }
}

/// Check if `bytes` is a valid XML NCName (Name without colons).
/// See <https://www.w3.org/TR/xml-names/#NT-NCName>.
pub(crate) fn is_ncname(bytes: &[u8]) -> bool {
    let s = match std::str::from_utf8(bytes) {
        Ok(s) => s,
        Err(_) => return false,
    };
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if is_name_start_char(c) => {}
        _ => return false,
    }
    chars.all(is_name_char)
}

/// XML NameStartChar (excluding ':'): Letter | '_'
/// See <https://www.w3.org/TR/xml/#NT-NameStartChar>.
fn is_name_start_char(c: char) -> bool {
    matches!(c,
        'A'..='Z'
        | '_'
        | 'a'..='z'
        | '\u{C0}'..='\u{D6}'
        | '\u{D8}'..='\u{F6}'
        | '\u{F8}'..='\u{2FF}'
        | '\u{370}'..='\u{37D}'
        | '\u{37F}'..='\u{1FFF}'
        | '\u{200C}'..='\u{200D}'
        | '\u{2070}'..='\u{218F}'
        | '\u{2C00}'..='\u{2FEF}'
        | '\u{3001}'..='\u{D7FF}'
        | '\u{F900}'..='\u{FDCF}'
        | '\u{FDF0}'..='\u{FFFD}'
        | '\u{10000}'..='\u{EFFFF}'
    )
}

/// XML NameChar (excluding ':'): NameStartChar | '-' | '.' | digit | combining/extender
/// See <https://www.w3.org/TR/xml/#NT-NameChar>.
fn is_name_char(c: char) -> bool {
    is_name_start_char(c)
        || matches!(c,
            '-'
            | '.'
            | '0'..='9'
            | '\u{B7}'
            | '\u{0300}'..='\u{036F}'
            | '\u{203F}'..='\u{2040}'
        )
}
