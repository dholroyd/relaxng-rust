use fnv::FnvHashMap;

/// An element or attribute local name, as raw bytes from the document.
///
/// The reader does not check that the name is a valid.  The handler must check
/// that a Name is valid (valid UTF-8 + valid NCName), either by finding the name
/// in a `NameTable`, which only contains valid names, or by calling
/// `Name::is_valid()`, and must report an invalid name itself.
#[derive(Clone, Copy)]
pub(crate) struct Name<'a> {
    bytes: &'a [u8],
}

impl<'a> Name<'a> {
    pub(super) fn new(bytes: &'a [u8]) -> Self {
        Name { bytes }
    }

    /// Checks that the name is a valid NCName.
    pub(crate) fn is_valid(&self) -> bool {
        is_ncname(self.bytes)
    }

    /// Looks the name up in `table`. A hit proves the name is a valid NCName, since the table
    /// holds nothing else.
    pub(crate) fn lookup(&self, table: &NameTable) -> Option<u32> {
        table.ids.get(self.bytes).copied()
    }
}

/// A fixed set of names, such as those a schema mentions, each assigned an id.
///
/// Only valid NCNames can be added, which is what lets [`Name::lookup`] treat a hit as proof
/// that a document's name is valid without checking it.
#[derive(Default)]
pub(crate) struct NameTable {
    ids: FnvHashMap<Box<[u8]>, u32>,
}

impl NameTable {
    /// Adds `name` if it is not already present, returning its id.
    ///
    /// # Panics
    ///
    /// If `name` is not a valid NCName.
    pub(crate) fn insert(&mut self, name: &str) -> u32 {
        if let Some(&id) = self.ids.get(name.as_bytes()) {
            return id;
        }
        assert!(is_ncname(name.as_bytes()), "{name:?} is not an NCName");
        let id = self.ids.len() as u32;
        self.ids.insert(name.as_bytes().into(), id);
        id
    }
}

/// Check if `bytes` is a valid XML NCName (Name without colons).
/// See <https://www.w3.org/TR/xml-names/#NT-NCName>.
fn is_ncname(bytes: &[u8]) -> bool {
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
