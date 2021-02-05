use relaxng_model::model::DefineRule;
use relaxng_model::{Compiler, RelaxError};
use relaxng_validator::Validator;
use std::cell::RefCell;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::process::exit;
use std::rc::Rc;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
enum Cli {
    Validate { schema: PathBuf, xml: Vec<PathBuf> },
}

fn main() {
    match Cli::from_args() {
        Cli::Validate { schema, xml } => validate(schema, xml),
    }
}

fn validate(schema: PathBuf, xmls: Vec<PathBuf>) {
    let mut compiler = Compiler::default();
    let model = match compiler.compile(&schema) {
        Ok(m) => m,
        Err(err) => {
            let d = compiler.dump_diagnostic(&err);
            exit(1);
        }
    };
    for xml in xmls {
        let mut f = File::open(&xml).expect("open example xml");
        let mut doc = String::new();
        f.read_to_string(&mut doc).expect("read xml");
        let src = doc.clone();
        let reader = xmlparser::Tokenizer::from(&src[..]);
        let mut v = Validator::new(model.clone(), reader);
        //v.assert_health();
        eprintln!("Validating {:?}", xml);
        loop {
            match v.next() {
                Some(Ok(())) => {}
                Some(Err(err)) => {
                    let (map, d) = v.diagnostic(xml.to_string_lossy().to_string(), doc, &err);
                    let mut emitter = codemap_diagnostic::Emitter::stderr(
                        codemap_diagnostic::ColorConfig::Auto,
                        Some(&map),
                    );
                    emitter.emit(&[d]);
                    println!("Explanation: {}", v.explain());
                    exit(2);
                }
                None => break,
            }
        }
    }
}
