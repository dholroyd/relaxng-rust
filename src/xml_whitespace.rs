//! Whitespace as XML defines it.
//!
//! The XML 1.0 `S` production (§2.3) admits only space, tab, carriage return and line feed, and
//! XML Schema's whitespace facet and RELAX NG's `token` datatype and `list` pattern use that
//! same set.  Rust's `str::trim()`, `str::split_whitespace()` and `char::is_whitespace()` instead
//! use the much larger Unicode `White_Space` property (including e.g. U+00A0 NO-BREAK SPACE and
//! U+2003 EM SPACE), and `u8::is_ascii_whitespace()` includes form feed, so none of those should
//! be used when interpreting XML content.

use std::borrow::Cow;

/// True for the characters matched by the XML `S` production: `#x20 | #x9 | #xD | #xA`.
#[inline]
pub(crate) fn is_xml_whitespace(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\r' | '\n')
}

/// Byte-oriented version of [`is_xml_whitespace`].  Every XML whitespace character is ASCII, so
/// this is correct for UTF-8 input: no byte of a multi-byte sequence can match.
#[cfg(any(feature = "validator", feature = "exemplifier", test))]
#[inline]
pub(crate) fn is_xml_whitespace_byte(b: u8) -> bool {
    matches!(b, b' ' | b'\t' | b'\r' | b'\n')
}

/// True if `text` is empty or consists entirely of XML whitespace.
#[cfg(any(feature = "validator", feature = "exemplifier", test))]
#[inline]
pub(crate) fn is_all_xml_whitespace(text: &[u8]) -> bool {
    text.iter().all(|&b| is_xml_whitespace_byte(b))
}

/// Strips leading and trailing XML whitespace (but not other Unicode whitespace).
#[inline]
pub(crate) fn trim_xml_whitespace(s: &str) -> &str {
    s.trim_matches(is_xml_whitespace)
}

/// Splits on runs of XML whitespace, yielding the non-empty tokens in between.
#[inline]
pub(crate) fn split_xml_whitespace(s: &str) -> impl Iterator<Item = &str> {
    s.split(is_xml_whitespace).filter(|t| !t.is_empty())
}

/// Applies XML Schema's `whiteSpace="collapse"`: strips leading and trailing XML whitespace and
/// reduces each internal run of it to a single space.  Borrows when the input is already collapsed.
pub(crate) fn collapse_xml_whitespace(s: &str) -> Cow<'_, str> {
    let already_collapsed = !s.starts_with(is_xml_whitespace)
        && !s.ends_with(is_xml_whitespace)
        && !s.contains(['\t', '\r', '\n'])
        && !s.contains("  ");
    if already_collapsed {
        return Cow::Borrowed(s);
    }
    let mut out = String::with_capacity(s.len());
    for tok in split_xml_whitespace(s) {
        if !out.is_empty() {
            out.push(' ');
        }
        out.push_str(tok);
    }
    Cow::Owned(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_xml_whitespace_is_whitespace() {
        for c in [' ', '\t', '\r', '\n'] {
            assert!(is_xml_whitespace(c));
            assert!(is_xml_whitespace_byte(c as u8));
        }
        for c in ['\x0B', '\x0C', '\u{85}', '\u{A0}', '\u{2003}', '\u{3000}'] {
            assert!(!is_xml_whitespace(c), "{c:?}");
        }
        assert!(!is_xml_whitespace_byte(0x0C));
    }

    #[test]
    fn all_whitespace() {
        assert!(is_all_xml_whitespace(b""));
        assert!(is_all_xml_whitespace(b" \t\r\n"));
        assert!(!is_all_xml_whitespace(" \u{A0} ".as_bytes()));
    }

    #[test]
    fn trim() {
        assert_eq!(trim_xml_whitespace(" \t a b \r\n"), "a b");
        assert_eq!(trim_xml_whitespace("\u{A0}a\u{A0}"), "\u{A0}a\u{A0}");
        assert_eq!(trim_xml_whitespace(" \u{2003}a "), "\u{2003}a");
    }

    #[test]
    fn split() {
        let v: Vec<_> = split_xml_whitespace("  a\tb\r\n c  ").collect();
        assert_eq!(v, ["a", "b", "c"]);
        let v: Vec<_> = split_xml_whitespace("a\u{A0}b c").collect();
        assert_eq!(v, ["a\u{A0}b", "c"]);
        assert_eq!(split_xml_whitespace(" \n ").count(), 0);
    }

    #[test]
    fn collapse() {
        for (input, expected) in [
            ("", ""),
            ("  ", ""),
            ("a", "a"),
            ("a b", "a b"),
            (" a", "a"),
            ("a\n", "a"),
            ("\t a \r\n b  c ", "a b c"),
            ("a\u{A0} b", "a\u{A0} b"),
        ] {
            let got = collapse_xml_whitespace(input);
            assert_eq!(got, expected, "{input:?}");
            assert_eq!(
                matches!(got, Cow::Borrowed(_)),
                input == expected,
                "{input:?}"
            );
        }
    }
}
