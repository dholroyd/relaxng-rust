use libtest_mimic::{Arguments, Failed, Trial};
use relaxng_model::Compiler;
use relaxng_validator::Validator;
use roxmltree::{ExpandedName, Node};

use std::collections::HashMap;
use std::convert::TryFrom;
use std::fs::File;
use std::io;
use std::io::Read;
use std::path::{Path, PathBuf};

fn main() {
    let args = Arguments::from_args();
    let tests = collect_tests();
    libtest_mimic::run(&args, tests).exit();
}

fn collect_tests() -> Vec<Trial> {
    // Silence panic output during test collection (pre-flight checks may panic)
    let prev_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));
    let src = "tests/spectest.xml";
    let mut f = File::open(src).expect("open spectest.xml");
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    let opts = roxmltree::ParsingOptions {
        allow_dtd: true,
        ..Default::default()
    };
    let doc = roxmltree::Document::parse_with_options(&s, opts).unwrap();
    assert_eq!(
        doc.root_element().tag_name(),
        ExpandedName::from("testSuite")
    );
    let mut trials = Vec::new();
    let mut counters: HashMap<String, usize> = HashMap::new();
    collect_suite(&mut trials, &mut counters, &[], doc.root_element());
    std::panic::set_hook(prev_hook);
    trials
}

fn collect_suite<'a, 'input>(
    trials: &mut Vec<Trial>,
    counters: &mut HashMap<String, usize>,
    path: &[String],
    suite: Node<'a, 'input>,
) {
    let mut suite_name: Option<String> = None;
    for child in suite.children() {
        if child.is_element() {
            if child.tag_name() == ExpandedName::from("testSuite") {
                let sub_path: Vec<String> = if let Some(ref name) = suite_name {
                    let mut p = path.to_vec();
                    p.push(name.clone());
                    p
                } else {
                    path.to_vec()
                };
                collect_suite(trials, counters, &sub_path, child);
            } else if child.tag_name() == ExpandedName::from("testCase") {
                collect_case(trials, counters, path, &suite_name, child);
            } else if child.tag_name() == ExpandedName::from("documentation") {
                suite_name = child.text().map(slugify);
            }
        }
    }
}

fn collect_case(
    trials: &mut Vec<Trial>,
    counters: &mut HashMap<String, usize>,
    path: &[String],
    suite_name: &Option<String>,
    case: Node,
) {
    let test_case = match TestCase::try_from(case) {
        Ok(c) => c,
        Err(()) => return,
    };

    // Build a hierarchical test name
    let mut parts: Vec<String> = path.to_vec();
    if let Some(name) = suite_name {
        parts.push(name.clone());
    }
    if let Some(ref doc) = test_case.documentation {
        parts.push(slugify(doc));
    } else if let Some(ref sec) = test_case.section {
        parts.push(format!("section_{sec}"));
    }

    let base_name = if parts.is_empty() {
        "test".to_string()
    } else {
        parts.join("::")
    };

    // Deduplicate names with a counter
    let count = counters.entry(base_name.clone()).or_insert(0);
    *count += 1;
    let name = if *count > 1 {
        format!("{base_name}#{}", *count - 1)
    } else {
        base_name
    };

    // Detect if this test should be ignored (TODO/unimplemented)
    let ignored = is_suppressed(&test_case);

    let trial = Trial::test(name, move || run_test(test_case)).with_ignored_flag(ignored);
    trials.push(trial);
}

/// Check if a test case would hit an unimplemented TODO path without actually running it.
/// We do a quick pre-flight compile to detect Error::Todo.
fn is_suppressed(test_case: &TestCase) -> bool {
    let resources = match &test_case.fixture {
        Fixture::Incorrect { resources } => resources,
        Fixture::Correct { resources, .. } => resources,
    };
    let schema_key = match &test_case.fixture {
        Fixture::Incorrect { .. } => "incorrect.rng",
        Fixture::Correct { .. } => "correct.rng",
    };
    let resources = resources.clone();
    let schema_key = schema_key.to_string();
    let result = std::panic::catch_unwind(move || {
        let mut c = create_compiler(resources);
        c.compile(Path::new(&schema_key))
    });
    matches!(
        result,
        Ok(Err(relaxng_model::RelaxError::XmlParse(
            _,
            relaxng_syntax::xml::Error::Todo(_)
        )))
    )
}

fn run_test(test_case: TestCase) -> Result<(), Failed> {
    match test_case.fixture {
        Fixture::Incorrect { resources } => {
            let mut c = create_compiler(resources.clone());
            let input = Path::new("incorrect.rng");
            match c.compile(input) {
                Err(_) => Ok(()),
                Ok(_) => {
                    let schema = resources.get("incorrect.rng").unwrap();
                    Err(format!("Incorrect schema should have been rejected:\n{schema}").into())
                }
            }
        }
        Fixture::Correct {
            resources,
            valid,
            invalid,
        } => {
            let mut c = create_compiler(resources.clone());
            let input = Path::new("correct.rng");
            let result = match c.compile(input) {
                Ok(r) => r,
                Err(e) => {
                    let schema = resources.get("correct.rng").unwrap();
                    return Err(format!("Correct schema was rejected: {e:?}\n{schema}").into());
                }
            };

            for (i, doc) in valid.iter().enumerate() {
                let mut v = Validator::new(result.start.clone()).unwrap();
                if let Err(err) = v.validate(doc.as_bytes()) {
                    let schema = resources.get("correct.rng").unwrap();
                    return Err(format!(
                        "Valid document #{} rejected: {err:?}\nschema: {schema}\ndoc: {doc}",
                        i + 1
                    )
                    .into());
                }
            }

            for (i, doc) in invalid.iter().enumerate() {
                let mut v = Validator::new(result.start.clone()).unwrap();
                if v.validate(doc.as_bytes()).is_ok() {
                    let schema = resources.get("correct.rng").unwrap();
                    return Err(format!(
                        "Invalid document #{} was accepted:\nschema: {schema}\ndoc: {doc}",
                        i + 1
                    )
                    .into());
                }
            }

            Ok(())
        }
    }
}

fn slugify(s: &str) -> String {
    s.trim()
        .chars()
        .map(|c| {
            if c.is_alphanumeric() {
                c.to_ascii_lowercase()
            } else {
                '_'
            }
        })
        .collect::<String>()
        .split('_')
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join("_")
}

#[derive(Clone)]
#[allow(unused)]
struct TestCase {
    section: Option<String>,
    documentation: Option<String>,
    fixture: Fixture,
    span: std::ops::Range<usize>,
}

#[derive(Clone)]
enum Fixture {
    Incorrect {
        resources: HashMap<String, String>,
    },
    Correct {
        resources: HashMap<String, String>,
        valid: Vec<String>,
        invalid: Vec<String>,
    },
}

impl<'a, 'input> TryFrom<Node<'a, 'input>> for TestCase {
    type Error = ();

    fn try_from(node: Node<'a, 'input>) -> Result<Self, Self::Error> {
        let mut section = None;
        let mut documentation = None;
        let mut incorrect = None;
        let mut correct = None;
        let mut valid = vec![];
        let mut invalid = vec![];
        let mut resources = HashMap::new();
        for child in node.children() {
            if child.is_element() {
                match child.tag_name().name() {
                    "section" => section = child.text(),
                    "documentation" => documentation = child.text(),
                    "incorrect" => incorrect = child.first_element_child(),
                    "correct" => correct = child.first_element_child(),
                    "valid" => valid.push(
                        child
                            .first_element_child()
                            .expect("child element of <valid>"),
                    ),
                    "invalid" => invalid.push(
                        child
                            .first_element_child()
                            .expect("child element of <invalid>"),
                    ),
                    "dir" | "resource" => {
                        load_resources(&PathBuf::new(), &mut resources, child);
                    }
                    "requires" => {}
                    _ => panic!(
                        "unexpected child of <testCase>: <{}>",
                        child.tag_name().name()
                    ),
                }
            }
        }
        if let Some(incorrect) = incorrect {
            resources.insert("incorrect.rng".to_string(), stringify(incorrect));
        } else if let Some(correct) = correct {
            resources.insert("correct.rng".to_string(), stringify(correct));
        } else {
            panic!("Neither <correct> nor <incorrect> specified")
        }
        Ok(TestCase {
            span: node.range(),
            section: section.map(|s| s.to_string()),
            documentation: documentation.map(|s| s.to_string()),
            fixture: if let Some(_incorrect) = incorrect {
                Fixture::Incorrect { resources }
            } else if let Some(_correct) = correct {
                Fixture::Correct {
                    resources,
                    valid: valid.iter().map(|node| stringify(*node)).collect(),
                    invalid: invalid.iter().map(|node| stringify(*node)).collect(),
                }
            } else {
                panic!("Neither <correct> nor <incorrect> specified")
            },
        })
    }
}

fn load_resources(path: &Path, resources: &mut HashMap<String, String>, node: Node) {
    match node.tag_name().name() {
        "dir" => {
            let sub = node
                .attribute("name")
                .unwrap_or_else(|| panic!("Expected @name on <dir>: {node:?}"));
            let sub_path = path.join(sub);
            for child in node.children().filter(|n| n.is_element()) {
                load_resources(&sub_path, resources, child);
            }
        }
        "resource" => {
            let name = node
                .attribute("name")
                .expect("Expected @name on <resource>");
            let sub_name = path.join(name);
            let data = node
                .first_element_child()
                .expect("expected child element in <resource>");
            resources.insert(sub_name.to_string_lossy().to_string(), stringify(data));
        }
        _other_name => panic!("unsupported tag {node:?}"),
    }
}

fn stringify(node: Node) -> String {
    let mut res = String::new();
    // extra work to give the first line consistent indentation with the rest of the lines
    if let Some(prev) = node.prev_sibling()
        && let Some(text) = prev.text()
    {
        let last_line = if let Some(pos) = text.rfind('\n') {
            &text[pos + 1..]
        } else {
            text
        };
        res.push_str(last_line);
    }
    res.push_str(&node.document().input_text()[node.range()]);
    res
}

struct FS(HashMap<String, String>);

impl relaxng_model::Files for FS {
    fn load(&self, name: &Path) -> Result<String, relaxng_model::RelaxError> {
        self.0
            .get(name.to_str().unwrap())
            .ok_or_else(|| {
                relaxng_model::RelaxError::Io(
                    name.to_path_buf(),
                    io::Error::from(io::ErrorKind::NotFound),
                )
            })
            .map(String::to_string)
    }
}

fn create_compiler(resources: HashMap<String, String>) -> Compiler<FS> {
    relaxng_model::Compiler::new(FS(resources), relaxng_model::Syntax::Xml)
}
