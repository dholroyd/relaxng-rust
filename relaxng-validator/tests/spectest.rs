// TODO: https://github.com/LukasKalbertodt/libtest-mimic ?

use relaxng_model::model::DefineRule;
use relaxng_model::{Compiler, RelaxError};
use relaxng_validator::{Validator, ValidatorError};
use roxmltree::{ExpandedName, Node, NodeType};
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fs::File;
use std::io;
use std::io::Read;
use std::panic;
use std::path::{Path, PathBuf};
use std::rc::Rc;

fn main() {
    // TODO: only run this test if testcases are not filtered or if the filter matches this case
    //       this hack of checking for Some("spectest") does not handle all filtering cases
    if let Some("spectest") = std::env::args().skip(1).next().as_ref().map(String::as_str) {
        spectest()
    }
}

fn spectest() {
    let src = "tests/spectest.xml";
    let mut f = File::open(src).expect("open example xml");
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    let mut map = codemap::CodeMap::new();
    let file = map.add_file(src.to_string(), s.clone());
    let mut emitter =
        codemap_diagnostic::Emitter::stderr(codemap_diagnostic::ColorConfig::Auto, Some(&map));
    let mut opts = roxmltree::ParsingOptions::default();
    opts.allow_dtd = true;
    let doc = roxmltree::Document::parse_with_options(&s, opts).unwrap();
    assert_eq!(
        doc.root_element().tag_name(),
        ExpandedName::from("testSuite")
    );
    let mut stats = Stats::default();
    process_suite(&mut emitter, &file, &mut stats, doc.root_element());
    eprintln!(
        "{} passed, {} failed, {} skipped",
        stats.passed, stats.failed, stats.skipped
    );
}

#[derive(Default)]
struct Stats {
    passed: u64,
    failed: u64,
    skipped: u64,
}

fn process_suite(
    emitter: &mut codemap_diagnostic::Emitter,
    file: &codemap::File,
    stats: &mut Stats,
    suite: Node,
) {
    for child in suite.children() {
        if child.is_element() {
            if child.tag_name() == ExpandedName::from("testSuite") {
                process_suite(emitter, file, stats, child)
            } else if child.tag_name() == ExpandedName::from("testCase") {
                process_case(emitter, file, stats, child)
            } else if child.tag_name() == ExpandedName::from("documentation") {
                if let Some(text) = child.text() {
                    eprintln!("== {} ==", text);
                }
            }
        }
    }
}

#[derive(Clone)]
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
impl Fixture {
    fn resources(&self) -> &HashMap<String, String> {
        match self {
            Fixture::Incorrect { resources, .. } | Fixture::Correct { resources, .. } => resources,
        }
    }
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
                    "requires" => {
                        println!("TODO: ignoring {:?}", child);
                    }
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
                .expect(&format!("Expected @name on <dir>: {:?}", node));
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
            println!("resource {:?} --", sub_name);
            let data = node
                .first_element_child()
                .expect("expected child element in <resource>");
            println!("{}", stringify(data));
            resources.insert(sub_name.to_string_lossy().to_string(), stringify(data));
        }
        other_name => panic!("unsupported tag {:?}", node),
    }
}

fn process_case(
    emitter: &mut codemap_diagnostic::Emitter,
    file: &codemap::File,
    stats: &mut Stats,
    case: Node,
) {
    let test_case = match TestCase::try_from(case) {
        Ok(c) => c,
        Err(e) => {
            eprintln!(" 🙈 TODO: Test case not handled");
            return;
        }
    };

    let (tx, rx) = std::sync::mpsc::channel();
    let test = test_case.clone();
    let thread = std::thread::spawn(move || {
        let result = panic::catch_unwind(|| run_test(test.clone()));
        tx.send(result)
    });
    let result = rx.recv_timeout(std::time::Duration::from_secs(5));
    match result {
        Ok(result) => match result {
            Ok(test_result) => match test_result {
                TestResult::Pass => {
                    stats.passed += 1;
                    eprintln!("  ✅ passed");
                }
                TestResult::Fail => {
                    stats.failed += 1;
                    diagnostic(emitter, file, case.range());
                    eprintln!("  ❌ failed");
                }
                TestResult::Suppressed => {
                    stats.skipped += 1;
                    eprintln!("  🙈 skipped");
                }
            },
            Err(e) => {
                eprintln!(" ❌ Test thread failed: {:?}", e);
                stats.failed += 1;
            }
        },
        Err(err) => {
            eprintln!(" ⛔ Timeout waiting for test to complete:");
            panic!("{:?}", err);
        }
    }
}

fn diagnostic(
    emitter: &mut codemap_diagnostic::Emitter,
    file: &codemap::File,
    span: std::ops::Range<usize>,
) {
    let label = codemap_diagnostic::SpanLabel {
        span: file.span.subspan(span.start as _, span.end as _),
        style: codemap_diagnostic::SpanStyle::Primary,
        label: None,
    };
    let d = codemap_diagnostic::Diagnostic {
        level: codemap_diagnostic::Level::Error,
        message: "Test failed".to_string(),
        code: None,
        spans: vec![label],
    };
    emitter.emit(&[d]);
}

enum TestResult {
    Pass,
    Fail,
    Suppressed,
}

fn run_test(test_case: TestCase) -> TestResult {
    match test_case.fixture {
        Fixture::Incorrect { resources } => {
            let mut c = create_compiler(resources.clone());
            let input = Path::new("incorrect.rng");

            match c.compile(input) {
                Err(e) => {
                    if let relaxng_model::RelaxError::XmlParse(
                        _,
                        relaxng_syntax::xml::Error::Todo(e),
                    ) = e
                    {
                        eprintln!("  {}", resources.get("incorrect.rng").unwrap());
                        eprintln!("  ❌ TODO: {}", e);
                        TestResult::Suppressed
                    } else {
                        //eprintln!("--------");
                        //eprintln!("  {}", s);
                        //eprintln!("  ✅ Incorrect schema was rejected");
                        c.dump_diagnostic(&e);
                        TestResult::Pass
                    }
                }
                Ok(result) => {
                    eprintln!("--------");
                    eprintln!("  {}", resources.get("incorrect.rng").unwrap());
                    eprintln!("  ❌ Incorrect schema should have failed");
                    TestResult::Fail
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

            match c.compile(input) {
                Err(e) => {
                    if let relaxng_model::RelaxError::XmlParse(
                        _,
                        relaxng_syntax::xml::Error::Todo(e),
                    ) = e
                    {
                        eprintln!("  {}", resources.get("correct.rng").unwrap());
                        eprintln!("  ❌ TODO: {}", e);
                        TestResult::Suppressed
                    } else {
                        //eprintln!("--------");
                        eprintln!("  {}", resources.get("correct.rng").unwrap());
                        eprintln!("  ❌ Correct schema was rejected");
                        c.dump_diagnostic(&e);
                        TestResult::Fail
                    }
                }
                Ok(result) => {
                    for doc in valid {
                        let reader = xmlparser::Tokenizer::from(&doc[..]);
                        let mut v = Validator::new(result.clone(), reader);
                        //eprintln!("--------");
                        //eprintln!("  {}", schema);
                        //eprintln!("  ❌ Incorrect schema should have failed");
                        loop {
                            match v.next() {
                                None => break,
                                Some(Ok(())) => {}
                                Some(Err(err)) => {
                                    println!("v.next(): {:?}", err);
                                    eprintln!("  {}", resources.get("correct.rng").unwrap());
                                    eprintln!("  {}", doc);
                                    eprintln!("  ❌ Valid input rejected");
                                    return TestResult::Fail;
                                }
                            }
                        }
                        //eprintln!("  ✅ Valid input accepted");
                        // fall through to the 'invalid' case
                    }
                    for doc in invalid {
                        let reader = xmlparser::Tokenizer::from(&doc[..]);
                        let mut v = Validator::new(result, reader);
                        //eprintln!("--------");
                        //eprintln!("  {}", schema);
                        //eprintln!("  ❌ Incorrect schema should have failed");
                        loop {
                            match v.next() {
                                None => break,
                                Some(Ok(())) => {}
                                Some(Err(err)) => {
                                    //eprintln!("  ✅ Invalid input rejected");
                                    return TestResult::Pass;
                                }
                            }
                        }
                        eprintln!("  {}", resources.get("correct.rng").unwrap());
                        eprintln!("  {}", doc);
                        eprintln!("  ❌ Invalid input accepted");
                        return TestResult::Fail;
                    }
                    // if the valid / invalid cases above didn't Fail, then it's a Pass
                    TestResult::Pass
                }
            }
        }
    }
}

fn stringify(node: Node) -> String {
    let mut res = String::new();
    // extra work to give the first line consistent indentation with the rest of the lines
    if let Some(prev) = node.prev_sibling() {
        if let Some(text) = prev.text() {
            let last_line = if let Some(pos) = text.rfind("\n") {
                &text[pos + 1..]
            } else {
                text
            };
            res.push_str(last_line);
        }
    }
    res.push_str(&node.document().input_text()[node.range()]);
    res
}

struct FS(HashMap<String, String>);

fn create_compiler(resources: HashMap<String, String>) -> Compiler<FS> {
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
    let mut c = relaxng_model::Compiler::new(FS(resources), relaxng_model::Syntax::Xml);
    c
}

fn validate(schema: Rc<RefCell<Option<DefineRule>>>, doc: &str) -> Result<(), ValidatorError> {
    let reader = xmlparser::Tokenizer::from(&doc[..]);
    let mut v = relaxng_validator::Validator::new(schema, reader);
    loop {
        match v.next() {
            None => break,
            Some(Ok(())) => {}
            Some(Err(err)) => return Err(err),
        }
    }
    Ok(())
}
