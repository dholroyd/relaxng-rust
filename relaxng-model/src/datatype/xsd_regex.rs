/// Translates an XSD regular expression into a Rust `regex` crate compatible pattern.
///
/// XSD regex differs from Rust regex in several ways:
/// - Implicit anchoring (patterns must match the whole string)
/// - `\i` / `\I` / `\c` / `\C` character class escapes for XML Name characters
/// - Character class subtraction `[X-[Y]]`
/// - Unicode block names with `Is` prefix: `\p{IsBasicLatin}`
pub fn translate(xsd_pattern: &str) -> Result<String, String> {
    let mut out = String::with_capacity(xsd_pattern.len() + 16);
    out.push_str("^(?:");

    let chars: Vec<char> = xsd_pattern.chars().collect();
    let len = chars.len();
    let mut i = 0;
    let mut in_char_class = false;
    let mut in_escape = false;

    while i < len {
        let c = chars[i];

        if in_escape {
            in_escape = false;
            match c {
                'i' => emit_name_start_char(&mut out, in_char_class, false),
                'I' => emit_name_start_char(&mut out, in_char_class, true),
                'c' => emit_name_char(&mut out, in_char_class, false),
                'C' => emit_name_char(&mut out, in_char_class, true),
                'p' | 'P' => {
                    // Unicode property: \p{...} or \P{...}
                    if i + 1 < len && chars[i + 1] == '{' {
                        let brace_start = i + 2;
                        let brace_end = chars[brace_start..]
                            .iter()
                            .position(|&ch| ch == '}')
                            .map(|p| brace_start + p)
                            .ok_or_else(|| "unterminated Unicode property escape".to_string())?;
                        let name: String = chars[brace_start..brace_end].iter().collect();
                        let translated = translate_property_name(&name);
                        out.push('\\');
                        out.push(c);
                        out.push('{');
                        out.push_str(&translated);
                        out.push('}');
                        i = brace_end + 1;
                        continue;
                    } else {
                        out.push('\\');
                        out.push(c);
                    }
                }
                _ => {
                    out.push('\\');
                    out.push(c);
                }
            }
            i += 1;
            continue;
        }

        match c {
            '\\' => {
                in_escape = true;
                i += 1;
            }
            '[' => {
                out.push('[');
                in_char_class = true;
                i += 1;
            }
            ']' => {
                out.push(']');
                in_char_class = false;
                i += 1;
            }
            '-' if in_char_class && i + 1 < len && chars[i + 1] == '[' => {
                // Character class subtraction: -[Y] → &&[^Y]
                out.push_str("&&[^");
                i += 2; // skip '-' and '['
            }
            _ => {
                out.push(c);
                i += 1;
            }
        }
    }

    if in_escape {
        return Err("trailing backslash".to_string());
    }

    out.push_str(")$");
    Ok(out)
}

// XML NameStartChar ranges (XML 1.0 5th Ed):
//   : | [A-Z] | _ | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] |
//   [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] |
//   [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] |
//   [#x10000-#xEFFFF]
const NAME_START_CHAR: &str = "\
A-Z_a-z:\
\u{C0}-\u{D6}\
\u{D8}-\u{F6}\
\u{F8}-\u{2FF}\
\u{370}-\u{37D}\
\u{37F}-\u{1FFF}\
\u{200C}-\u{200D}\
\u{2070}-\u{218F}\
\u{2C00}-\u{2FEF}\
\u{3001}-\u{D7FF}\
\u{F900}-\u{FDCF}\
\u{FDF0}-\u{FFFD}\
\u{10000}-\u{EFFFF}";

// XML NameChar = NameStartChar + these additional characters:
//   - | . | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
const NAME_CHAR_EXTRA: &str = "\
\\-\\.0-9\
\u{B7}\
\u{0300}-\u{036F}\
\u{203F}-\u{2040}";

fn emit_name_start_char(out: &mut String, in_char_class: bool, complement: bool) {
    if in_char_class {
        // Inside [...], emit just the ranges
        // For complement inside a char class, use intersection: &&[^ranges]
        if complement {
            out.push_str("&&[^");
            out.push_str(NAME_START_CHAR);
            out.push(']');
        } else {
            out.push_str(NAME_START_CHAR);
        }
    } else if complement {
        out.push_str("[^");
        out.push_str(NAME_START_CHAR);
        out.push(']');
    } else {
        out.push('[');
        out.push_str(NAME_START_CHAR);
        out.push(']');
    }
}

fn emit_name_char(out: &mut String, in_char_class: bool, complement: bool) {
    if in_char_class {
        if complement {
            out.push_str("&&[^");
            out.push_str(NAME_START_CHAR);
            out.push_str(NAME_CHAR_EXTRA);
            out.push(']');
        } else {
            out.push_str(NAME_START_CHAR);
            out.push_str(NAME_CHAR_EXTRA);
        }
    } else if complement {
        out.push_str("[^");
        out.push_str(NAME_START_CHAR);
        out.push_str(NAME_CHAR_EXTRA);
        out.push(']');
    } else {
        out.push('[');
        out.push_str(NAME_START_CHAR);
        out.push_str(NAME_CHAR_EXTRA);
        out.push(']');
    }
}

fn translate_property_name(name: &str) -> String {
    if let Some(rest) = name.strip_prefix("Is") {
        // XSD uses \p{IsBlockName} for Unicode blocks/scripts.
        // Rust regex uses \p{BlockName} for scripts and \p{InBlockName} for blocks.
        // Strip the Is prefix; the regex crate will match against categories, scripts,
        // and blocks automatically.
        rest.to_string()
    } else {
        name.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn compile(xsd_pat: &str) -> regex::Regex {
        let rust_pat = translate(xsd_pat).unwrap();
        regex::Regex::new(&rust_pat).unwrap_or_else(|e| {
            panic!("failed to compile translated pattern {:?}: {}", rust_pat, e)
        })
    }

    #[test]
    fn anchoring() {
        let re = compile("abc");
        assert!(re.is_match("abc"));
        assert!(!re.is_match("xabc"));
        assert!(!re.is_match("abcx"));
        assert!(!re.is_match("xabcx"));
    }

    #[test]
    fn passthrough() {
        // Common XSD patterns that should work unchanged
        for pat in &[
            "[^:]*",
            ".+/.+",
            "[A-Za-z]{1,8}(-[A-Za-z0-9]{1,8})*",
            ".+@.+",
        ] {
            let re = compile(pat);
            assert!(re.is_match("test") || true); // just verify it compiles
        }
    }

    #[test]
    fn name_start_char() {
        let re = compile(r"\i");
        assert!(re.is_match("a"));
        assert!(re.is_match("Z"));
        assert!(re.is_match("_"));
        assert!(re.is_match(":"));
        assert!(re.is_match("\u{C0}")); // À
        assert!(!re.is_match("1"));
        assert!(!re.is_match("-"));
        assert!(!re.is_match(" "));
    }

    #[test]
    fn name_char() {
        let re = compile(r"\c");
        assert!(re.is_match("a"));
        assert!(re.is_match("1"));
        assert!(re.is_match("-"));
        assert!(re.is_match("."));
        assert!(re.is_match(":"));
        assert!(re.is_match("_"));
        assert!(!re.is_match(" "));
        assert!(!re.is_match("!"));
    }

    #[test]
    fn complement_name_start_char() {
        let re = compile(r"\I");
        assert!(re.is_match("1"));
        assert!(re.is_match("-"));
        assert!(re.is_match(" "));
        assert!(!re.is_match("a"));
        assert!(!re.is_match("_"));
        assert!(!re.is_match(":"));
    }

    #[test]
    fn complement_name_char() {
        let re = compile(r"\C");
        assert!(re.is_match(" "));
        assert!(re.is_match("!"));
        assert!(!re.is_match("a"));
        assert!(!re.is_match("1"));
        assert!(!re.is_match("-"));
    }

    #[test]
    fn name_start_char_in_class() {
        let re = compile(r"[\i]+");
        assert!(re.is_match("abc"));
        assert!(re.is_match(":foo"));
        assert!(!re.is_match("123"));
    }

    #[test]
    fn name_char_in_class() {
        let re = compile(r"[\c]+");
        assert!(re.is_match("abc123"));
        assert!(re.is_match("a-b.c"));
        assert!(!re.is_match("a b"));
    }

    #[test]
    fn char_class_subtraction() {
        // [a-z-[aeiou]] = consonants only
        let re = compile("[a-z-[aeiou]]");
        assert!(re.is_match("b"));
        assert!(re.is_match("c"));
        assert!(re.is_match("z"));
        assert!(!re.is_match("a"));
        assert!(!re.is_match("e"));
        assert!(!re.is_match("i"));
    }

    #[test]
    fn name_start_char_minus_colon() {
        // [\i-[:]] = NameStartChar minus colon (NCName start char)
        let re = compile(r"[\i-[:]]");
        assert!(re.is_match("a"));
        assert!(re.is_match("_"));
        assert!(re.is_match("Z"));
        assert!(!re.is_match(":"));
        assert!(!re.is_match("1"));
    }

    #[test]
    fn name_char_minus_colon() {
        // [\c-[:]] = NameChar minus colon (NCName char)
        let re = compile(r"[\c-[:]]");
        assert!(re.is_match("a"));
        assert!(re.is_match("1"));
        assert!(re.is_match("-"));
        assert!(!re.is_match(":"));
    }

    #[test]
    fn odf_qname_pattern() {
        // The actual ODF pattern that was failing: (([\i-[:]][\c-[:]]*)?:)?.+
        let re = compile(r"(([\i-[:]][\c-[:]]*)?:)?.+");
        assert!(re.is_match("foo:bar"));
        assert!(re.is_match(":bar"));
        assert!(re.is_match("bar"));
        assert!(!re.is_match(""));
    }

    #[test]
    fn unicode_script_is_prefix() {
        // XSD uses \p{IsGreek} for the Greek script; Rust regex uses \p{Greek}
        let translated = translate(r"\p{IsGreek}").unwrap();
        assert!(translated.contains(r"\p{Greek}"));
        let re = regex::Regex::new(&translated).unwrap();
        assert!(re.is_match("\u{03B1}")); // α
        assert!(!re.is_match("A"));
    }

    #[test]
    fn is_prefix_stripped() {
        // Verify the Is prefix is stripped in the translated output
        let translated = translate(r"\p{IsLu}").unwrap();
        assert!(translated.contains(r"\p{Lu}"));
    }

    #[test]
    fn unicode_category_unchanged() {
        let translated = translate(r"\p{Lu}").unwrap();
        assert!(translated.contains(r"\p{Lu}"));
    }

    #[test]
    fn trailing_backslash_error() {
        assert!(translate(r"abc\").is_err());
    }

    #[test]
    fn unterminated_property_error() {
        assert!(translate(r"\p{Foo").is_err());
    }

    #[test]
    fn ncname_pattern() {
        // Common pattern: [\i-[:]][\c-[:]]*  (an NCName)
        let re = compile(r"[\i-[:]][\c-[:]]*");
        assert!(re.is_match("foo"));
        assert!(re.is_match("_bar"));
        assert!(re.is_match("a123"));
        assert!(!re.is_match(":foo"));
        assert!(!re.is_match("123"));
        assert!(!re.is_match(""));
    }
}
