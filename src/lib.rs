use std::fs::File;
use std::io;
use std::io::Read;
use relaxng_compact_syntax::{parse, types};
use nom;
use nom_locate::LocatedSpan;
use std::collections::{HashMap, HashSet};
use relaxng_compact_syntax::types::{Schema, NamespaceUriLiteral};
use std::rc::Rc;
use std::path::{PathBuf, Path};
use crate::model::Pattern;
use std::ops::Range;
use std::cell::RefCell;

pub mod model;

// TODO: detect ambiguous grammars per https://www.kohsuke.org/relaxng/ambiguity/AmbiguousGrammarDetection.pdf

type Span = Range<usize>;

#[derive(Debug)]
pub enum RelaxError {
    Io(PathBuf, io::Error),
    Parse(nom::Err<(String, nom::error::ErrorKind)>),
    DuplicateDefinition { file: PathBuf, name: String, duplicate: Span, original: Span },
    IncompatibleCombination { file: PathBuf, combine: Span, original: Span },
    UndefinedNamespacePrefix { file: PathBuf, whence: Span, prefix: String },
    UndefinedDatatypePrefix(String),
    NamespacePrefixAlreadyDefined(String),
    DatatypePrefixAlreadyDefined(String),
    DuplicateParameterName(String),
    /// the grammar has 'start = ...' more than once
    StartRuleRedefined,
    /// the grammar did not define a 'start' rule
    StartRuleNotDefined,
    /// the grammar has both 'foo |= ...' and 'foo &= ...' constructs
    DefineRulesMixesChoiceAndInterleave,
    /// a 'grammar' section failed to specify a 'start = ...' rule
    GrammarMissingStartRule,
    /// a 'parent ...' pattern was used in a context where no parent grammar is available
    NoParentAvailable,
}

struct Context<'a> {
    parent: Option<&'a Context<'a>>,
    file: PathBuf,
    refs: HashMap<String, Rc<RefCell<Option<model::DefineRule>>>>,
    /// references that will need to be resolved aganst the parent context, once we are done
    /// compiling the rules of this context (this step is deferred to avoid needing to mutate
    /// the parent context - our 'parent' ref is not mutable)
    parent_refs: HashMap<String, Rc<RefCell<Option<model::DefineRule>>>>,
    namespaces: HashMap<String, String>,
    default_namespace: String,
    datatypes: HashMap<String, String>,
    //start: Option<DefineRule>,
}
impl<'a> Context<'a> {
    pub fn new(file: &Path) -> Context<'a> {
        Self::new_impl(None, file)
    }
    pub fn new_child_of(parent: &'a Context, file: &Path) -> Context<'a> {
        Self::new_impl(Some(parent), file)
    }
    fn new_impl(parent: Option<&'a Context>, file: &Path) -> Context<'a> {
        let mut namespaces = HashMap::new();
        namespaces.insert("xml".to_string(), "http://www.w3.org/XML/1998/namespace".to_string());
        let mut datatypes = HashMap::new();
        datatypes.insert("xsd".to_string(), "http://www.w3.org/2001/XMLSchema-datatypes".to_string());
        Context {
            parent,
            file: file.to_path_buf(),
            refs: HashMap::new(),
            parent_refs: HashMap::new(),
            namespaces,
            default_namespace: "".to_string(),
            datatypes,
            //start: None,
        }
    }

    // TODO: an enum to indicate either 'no namespace', or a uri
    fn default_namespace_uri(&self) -> &str {
        &self.default_namespace[..]
    }

    fn declare_namespace(&mut self, prefix: String, uri: String) -> Result<(), RelaxError> {
        if self.namespaces.contains_key(&prefix) {
            Err(RelaxError::NamespacePrefixAlreadyDefined(prefix))
        } else {
            self.namespaces.insert(prefix, uri);
            Ok(())
        }
    }
    fn namespace_uri_for_prefix(&self, prefix: &types::NcName) -> Result<&str, RelaxError> {
        // TODO: return Option, and have caller work out RelaxError value
        self.namespaces
            .get(&prefix.1)
            .map(|s| &s[..] )
            .ok_or_else(|| RelaxError::UndefinedNamespacePrefix{
                file: self.file.clone(),
                whence: prefix.0.clone(),
                prefix: prefix.1.to_string(),
            })
    }

    fn declare_datatype(&mut self, prefix: String, uri: String) -> Result<(), RelaxError> {
        if self.datatypes.contains_key(&prefix) {
            Err(RelaxError::DatatypePrefixAlreadyDefined(prefix))
        } else {
            self.datatypes.insert(prefix, uri);
            Ok(())
        }
    }
    fn datatype_uri_for_prefix(&self, prefix: &types::NcName) -> Result<&str, RelaxError> {
        // TODO: return Option, and have caller work out RelaxError value
        self.datatypes.get(&prefix.1).map(|s| &s[..] ).ok_or_else(|| RelaxError::UndefinedDatatypePrefix(prefix.1.to_string()))
    }
}

pub struct Compiler {
    loaded: HashMap<PathBuf, Rc<Schema>>,
}
impl Default for Compiler {
    fn default() -> Self {
        Compiler {
            loaded: HashMap::default(),
        }
    }
}
impl Compiler {
    // TODO: does this need to support URLs?
    pub fn compile(&mut self, name: &Path) -> Result<Rc<RefCell<Option<model::DefineRule>>>, RelaxError> {
        let schema = self.get_schema(name)?;
        let mut ctx = Context::new(name);
        self.compile_schema(&mut ctx, schema)?;
        for (name, r) in ctx.refs.iter() {
            if r.borrow().is_none() {
                println!("Undefined: {:?}", name);
            }
        }
        if let Some(start) = ctx.refs.get("start") {
            Ok(start.clone())
        } else {
            Err(RelaxError::StartRuleNotDefined)
        }
    }

    #[inline(never)]
    fn get_schema(&mut self, name: &Path) -> Result<Rc<Schema>, RelaxError> {
        if let Some(s) = self.loaded.get(name) {
            return Ok(s.clone());
        }
        let mut io = File::open(name).map_err(|e| RelaxError::Io(name.to_path_buf(), e))?;
        let mut data = String::new();
        io.read_to_string(&mut data).map_err(|e| RelaxError::Io(name.to_path_buf(), e))?;
        let input = LocatedSpan::new(&data[..]);
        let schema = parse::schema(input).map_err(|e| match e {
            nom::Err::Error((i, e)) => RelaxError::Parse(nom::Err::Error((i.to_string(), e))),
            nom::Err::Failure((i, e)) => RelaxError::Parse(nom::Err::Failure((i.to_string(), e))),
            nom::Err::Incomplete(n) => RelaxError::Parse(nom::Err::Incomplete(n)),
        })?;
        let schema = Rc::new(schema);
        self.loaded.insert(name.to_path_buf(), schema.clone());
        Ok(schema)
    }

    fn compile_schema(&mut self, ctx: &mut Context, schema: Rc<Schema>) -> Result<(), RelaxError> {
        Compiler::compile_declarations(ctx, schema.as_ref())?;
        match schema.pattern_or_grammar {
            types::PatternOrGrammar::Pattern(ref _p) => {
                unimplemented!("TODO: top_level containing just a pattern");
            },
            types::PatternOrGrammar::Grammar(ref elements) => {
                for e in elements.iter() {
                    self.compile_grammar_content(ctx, e)?;
                }
            }
        }
        Ok(())
    }
    fn compile_grammar_content(&mut self, ctx: &mut Context, grammar_content: &types::GrammarContent) -> Result<(), RelaxError> {
        match grammar_content {
            //types::GrammarContent::Start(start) => self.compile_start(ctx, start),
            types::GrammarContent::Define(def) => self.compile_define(ctx, def),
            types::GrammarContent::Div(div) => self.compile_grammar_div(ctx, &div[..]),
            types::GrammarContent::Include(inc) => self.compile_include(ctx, inc),
        }
    }
    fn append_choice(choice: &mut Pattern, c: Pattern) {
        if let Pattern::Choice(ref mut this) = choice {
            if let Pattern::Choice(mut other) = c {
                this.extend(other.drain(..))
            } else {
                this.push(c)
            }
        } else {
            panic!("Not a Choice pattern {:?}", choice);
        }
    }
    fn append_interleave(interleave: &mut Pattern, c: Pattern) {
        if let Pattern::Interleave(ref mut this) = interleave {
            if let Pattern::Interleave(mut other) = c {
                this.extend(other.drain(..))
            } else {
                this.push(c)
            }
        } else {
            panic!("Not an Interleave pattern {:?}", interleave);
        }
    }
    fn compile_define(&mut self, ctx: &mut Context, define: &types::Define) -> Result<(), RelaxError> {
        let id = (define.1).1.clone();
        let rule = self.compile_pattern(ctx, &define.3)?;
        let new_rule = match define.2 {
            types::AssignMethod::Assign => model::DefineRule::AssignCombine(None, rule),
            types::AssignMethod::Choice => model::DefineRule::CombineOnly(model::CombineRule::Choice, Pattern::Choice(vec![rule])),
            types::AssignMethod::Interleave => model::DefineRule::CombineOnly(model::CombineRule::Interleave, Pattern::Interleave(vec![rule])),
        };
        if ctx.refs.contains_key(&id) {
            let a = ctx.refs.get(&id);
            let mut rule_ref = a.unwrap().borrow_mut();
            *rule_ref = Self::merge(&id, rule_ref.take(), Some(new_rule))?
        } else {
            ctx.refs.insert(id, Rc::new(RefCell::new(Some(new_rule))));
        };
        Ok(())
    }
    fn compile_grammar_div(&mut self, _ctx: &mut Context, _div: &[types::GrammarContent]) -> Result<(), RelaxError> {
        unimplemented!()
    }
    fn compile_include_div(&mut self, _ctx: &mut Context, _div: &[types::IncludeContent]) -> Result<(), RelaxError> {
        unimplemented!()
    }
    fn compile_include(&mut self, ctx: &mut Context, inc: &types::Include) -> Result<(), RelaxError> {
        if inc.1.is_some() {
            unimplemented!("inherit");
        }
        let path = ctx.file
            .parent()
            .expect("TODO: no parent?")
            .join(&inc.0.as_string_value());
        let s = self.get_schema(&path)?;
        // TODO: maintain a context stack and use that prevent recursive-include loop
        let mut child_ctx = Context::new_child_of(ctx, &path);
        self.compile_schema(&mut child_ctx, s)?;

        let mut child_refs = HashMap::new();
        std::mem::swap(&mut child_refs, &mut child_ctx.refs);
        let mut parent_refs = HashMap::new();
        std::mem::swap(&mut parent_refs, &mut child_ctx.parent_refs);
        Self::copy_parent_refs(ctx, parent_refs);

        // replace any definitions for which the including file provides an override
        let mut inc_refs = if let Some(ref overrides) = inc.2 {
            let mut inc_ctx = Context::new_child_of(ctx, &ctx.file);
            // TODO: copy declarations from parent,
            for o in overrides.iter() {
                match o {
                    types::IncludeContent::Define(d) => self.compile_define(&mut inc_ctx, d)?,
                    types::IncludeContent::Div(d) => self.compile_include_div(&mut inc_ctx, &d[..])?,
                }
            }
            inc_ctx.refs
        } else {
            HashMap::new()
        };

        // now import each reference from the child doc into the current schema
        for (name, def) in child_refs {
            // if the include-directive has an override for this definition, use that instead,
            let def = inc_refs.remove(&name).unwrap_or(def);
            if ctx.refs.contains_key(&name) {
                let mut r = ctx.refs.get_mut(&name).unwrap().borrow_mut();
                *r = Self::merge(&name, r.take(), def.borrow_mut().take())?
            } else {
                ctx.refs.insert(name, def);
            }
        }
        Ok(())
    }
    fn copy_parent_refs(ctx: &mut Context, mut parent_refs: HashMap<String, Rc<RefCell<Option<model::DefineRule>>>>) {
        for (name, def) in parent_refs.drain() {
            if ctx.refs.contains_key(&name) {
                panic!("Parent already contains {:?}, but we were supposed to have checked that it was absent earlier on");
            } else {
                ctx.refs.insert(name, def);
            }

        }
    }
    fn merge(name: &str, a: Option<model::DefineRule>, b: Option<model::DefineRule>) -> Result<Option<model::DefineRule>, RelaxError> {
        match (a, b) {
            (None, None) => Ok(None),
            (Some(a), None) => Ok(Some(a)),
            (None, Some(b)) => Ok(Some(b)),
            (Some(a), Some(b)) => {
                // TODO: does this really work if other areas of the grammar retain references to the unmodified 'b'?
                Self::merge1(name, a, b).map(Some)
            }
        }
    }
    fn merge1(name: &str, a: model::DefineRule, b: model::DefineRule) -> Result<model::DefineRule, RelaxError> {
        use model::{ DefineRule, CombineRule };
        match (a, b) {
            (DefineRule::AssignCombine(_, _), DefineRule::AssignCombine(_, _)) => {
                return Err(RelaxError::DuplicateDefinition {
                    file: "TODO: file name".into(),
                    name: name.to_string(),
                    duplicate: 0..0,
                    original: 0..0
                })
            },
            // choice-related cases,
            (DefineRule::CombineOnly(CombineRule::Choice, mut patt_a), DefineRule::CombineOnly(CombineRule::Choice, patt_b)) => {
                Self::append_choice(&mut patt_a, patt_b);
                Ok(DefineRule::CombineOnly(CombineRule::Choice, patt_a))
            },
            (DefineRule::AssignCombine(Some(CombineRule::Choice), mut patt_a), DefineRule::CombineOnly(CombineRule::Choice, patt_b)) |
            (DefineRule::CombineOnly(CombineRule::Choice, mut patt_a), DefineRule::AssignCombine(Some(CombineRule::Choice), patt_b)) |
            (DefineRule::AssignCombine(None, mut patt_a), DefineRule::CombineOnly(CombineRule::Choice, patt_b)) |
            (DefineRule::CombineOnly(CombineRule::Choice, mut patt_a), DefineRule::AssignCombine(None, patt_b)) => {
                let result = if let Pattern::Choice(_) = patt_a {
                    Self::append_choice(&mut patt_a, patt_b);
                    patt_a
                } else {
                    Pattern::Choice(vec![patt_a, patt_b])
                };
                Ok(DefineRule::AssignCombine(Some(CombineRule::Choice), result))
            }
            // interleave-related cases,
            (DefineRule::CombineOnly(CombineRule::Interleave, mut patt_a), DefineRule::CombineOnly(CombineRule::Interleave, patt_b)) => {
                Self::append_interleave(&mut patt_a, patt_b);
                Ok(DefineRule::CombineOnly(CombineRule::Interleave, patt_a))
            },
            (DefineRule::AssignCombine(Some(CombineRule::Interleave), mut patt_a), DefineRule::CombineOnly(CombineRule::Interleave, patt_b)) |
            (DefineRule::CombineOnly(CombineRule::Interleave, mut patt_a), DefineRule::AssignCombine(Some(CombineRule::Interleave), patt_b)) |
            (DefineRule::AssignCombine(None, mut patt_a), DefineRule::CombineOnly(CombineRule::Interleave, patt_b)) |
            (DefineRule::CombineOnly(CombineRule::Interleave, mut patt_a), DefineRule::AssignCombine(None, patt_b)) => {
                let result = if let Pattern::Choice(_) = patt_a {
                    Self::append_interleave(&mut patt_a, patt_b);
                    patt_a
                } else {
                    Pattern::Interleave(vec![patt_a, patt_b])
                };
                Ok(DefineRule::AssignCombine(Some(CombineRule::Interleave), result))
            }
            // invalid combinations,
            (DefineRule::CombineOnly(CombineRule::Choice, _), DefineRule::CombineOnly(CombineRule::Interleave, _)) |
            (DefineRule::CombineOnly(CombineRule::Interleave, _), DefineRule::CombineOnly(CombineRule::Choice, _)) |
            (DefineRule::AssignCombine(Some(CombineRule::Choice), _), DefineRule::CombineOnly(CombineRule::Interleave, _)) |
            (DefineRule::AssignCombine(Some(CombineRule::Interleave), _), DefineRule::CombineOnly(CombineRule::Choice, _)) |
            (DefineRule::CombineOnly(CombineRule::Choice, _), DefineRule::AssignCombine(Some(CombineRule::Interleave), _)) |
            (DefineRule::CombineOnly(CombineRule::Interleave, _), DefineRule::AssignCombine(Some(CombineRule::Choice), _)) => {
                return Err(RelaxError::DefineRulesMixesChoiceAndInterleave)
            }
        }
    }

    fn compile_pattern(&mut self, ctx: &mut Context, pattern: &types::Pattern) -> Result<model::Pattern, RelaxError> {
        match pattern {
            types::Pattern::Element(e) => self.compile_element(ctx, e),
            types::Pattern::Attribute(e) => self.compile_attribute(ctx, e),
            types::Pattern::List(l) => self.compile_list(ctx, l),
            types::Pattern::Mixed(p) => Ok(model::Pattern::Mixed(Box::new(self.compile_pattern(ctx, &p.0)?))),
            types::Pattern::Identifier(i) => self.compile_ref(ctx, i),
            types::Pattern::Parent(p) => self.compile_parent(ctx, p),
            types::Pattern::Empty => Ok(model::Pattern::Empty),
            types::Pattern::Text => Ok(model::Pattern::Text),
            types::Pattern::NotAllowed => Ok(model::Pattern::NotAllowed),
            types::Pattern::External(e) => self.compile_external(ctx, e),
            types::Pattern::Grammar(g) => self.compile_grammar(ctx, g),
            types::Pattern::Group(g) => self.compile_pattern(ctx, g),
            types::Pattern::ListPair(a, b) => self.compile_sequence(ctx, a, b),
            types::Pattern::InterleavePair(a, b) => self.compile_interleave(ctx, a, b),
            types::Pattern::ChoicePair(a, b) => self.compile_choice(ctx, a, b),
            types::Pattern::Optional(p) => Ok(model::Pattern::Optional(Box::new(self.compile_pattern(ctx, p)?))),
            types::Pattern::ZeroOrMore(p) => Ok(model::Pattern::ZeroOrMore(Box::new(self.compile_pattern(ctx, p)?))),
            types::Pattern::OneOrMore(p) => Ok(model::Pattern::OneOrMore(Box::new(self.compile_pattern(ctx, p)?))),
            types::Pattern::DatatypeValue(d) => self.compile_datatype_value_pattern(ctx, d),
            types::Pattern::DatatypeName(d) => self.compile_datatype_name_pattern(ctx, d),
        }
    }
    fn compile_element(&mut self, ctx: &mut Context, element: &types::ElementPattern) -> Result<model::Pattern, RelaxError> {
        Ok(model::Pattern::Element(
            self.compile_nameclass(ctx, ElemAttr::Element, &element.name_class)?,
            Box::new(self.compile_pattern(ctx, &element.pattern)?),
        ))
    }
    fn compile_attribute(&mut self, ctx: &mut Context, attribute: &types::AttributePattern) -> Result<model::Pattern, RelaxError> {
        Ok(model::Pattern::Attribute(
            self.compile_nameclass(ctx, ElemAttr::Attribute, &attribute.name_class)?,
            Box::new(self.compile_pattern(ctx, &attribute.pattern)?),
        ))
    }
    fn compile_list(&mut self, ctx: &mut Context, list: &types::ListPattern) -> Result<model::Pattern, RelaxError> {
        Ok(model::Pattern::List(Box::new(self.compile_pattern(ctx, &list.0)?)))
    }
    fn compile_ref(&mut self, ctx: &mut Context, reference: &types::Identifier) -> Result<model::Pattern, RelaxError> {
        Ok(Pattern::Ref(Self::acquire_ref(&mut ctx.refs, &reference.1)))
    }
    fn acquire_ref(refs: &mut HashMap<String, Rc<RefCell<Option<model::DefineRule>>>>, id: &str) -> Rc<RefCell<Option<model::DefineRule>>> {
        if let Some(r) = refs.get(id) {
            r.clone()
        } else {
            let r = Rc::new(RefCell::new(None));
            refs.insert(id.to_string(), r.clone());
            r
        }
    }
    fn compile_parent(&mut self, ctx: &mut Context, reference: &types::Identifier) -> Result<model::Pattern, RelaxError> {
        if let Some(parent) = ctx.parent {
            if let Some(r) = parent.refs.get(&reference.1) {
                Ok(Pattern::Ref(r.clone()))
            } else {
                // the parent lacks this right now, but we can't add it to the ctx.parent.refs
                // because ctx.parent is immutable; so we stow this in the local context and the
                // caller will copy all parent_refs entries into the parent context when this
                // context is discarded
                Ok(Pattern::Ref(Self::acquire_ref(&mut ctx.parent_refs, &reference.1)))
            }
        } else {
            Err(RelaxError::NoParentAvailable)
        }
    }
    fn compile_external(&mut self, _ctx: &mut Context, _external: &types::ExternalPattern) -> Result<model::Pattern, RelaxError> {
        unimplemented!()
    }
    fn compile_grammar(&mut self, ctx: &mut Context, grammar: &types::GrammarPattern) -> Result<model::Pattern, RelaxError> {
        let mut child_ctx = Context::new_child_of(ctx, &ctx.file);

        // an embedded 'grammar { }' inherits namespace declarations from the containing file;
        // we achieve this inefficiently by copying
        for (prefix, uri) in &ctx.namespaces {
            child_ctx.namespaces.insert(prefix.to_string(), uri.to_string());
        }
        for g in &grammar.0 {
            match g {
                types::GrammarContent::Define(ref d) => self.compile_define(&mut child_ctx, d)?,
                types::GrammarContent::Div(ref d) => self.compile_grammar_div(&mut child_ctx, d)?,
                types::GrammarContent::Include(ref i) => self.compile_include(&mut child_ctx, i)?,
            }
        }
        if let Some(r) = child_ctx.refs.get("start") {
            r.borrow_mut().take()
                .map(|d| match d {
                    model::DefineRule::AssignCombine(_, p) | model::DefineRule::CombineOnly(_, p) => p,
                })
                .ok_or_else(|| RelaxError::GrammarMissingStartRule)
        } else {
            Err(RelaxError::GrammarMissingStartRule)
        }
    }
    fn compile_sequence(&mut self, ctx: &mut Context, a: &types::Pattern, b: &types::Pattern) -> Result<model::Pattern, RelaxError> {
        let a = self.compile_pattern(ctx, a)?;
        let b = self.compile_pattern(ctx, b)?;
        let mut i = vec![];
        match a {
            Pattern::Sequence(mut v) => i.extend(v.drain(..)),
            _ => i.push(a),
        }
        match b {
            Pattern::Sequence(mut v) => i.extend(v.drain(..)),
            _ => i.push(b),
        }
        Ok(model::Pattern::Sequence(i))
    }
    fn compile_interleave(&mut self, ctx: &mut Context, a: &types::Pattern, b: &types::Pattern) -> Result<model::Pattern, RelaxError> {
        let a = self.compile_pattern(ctx, a)?;
        let b = self.compile_pattern(ctx, b)?;
        let mut i = vec![];
        match a {
            Pattern::Interleave(mut v) => i.extend(v.drain(..)),
            _ => i.push(a),
        }
        match b {
            Pattern::Interleave(mut v) => i.extend(v.drain(..)),
            _ => i.push(b),
        }
        Ok(model::Pattern::Interleave(i))
    }
    fn compile_choice(&mut self, ctx: &mut Context, a: &types::Pattern, b: &types::Pattern) -> Result<model::Pattern, RelaxError> {
        let a = self.compile_pattern(ctx, a)?;
        let b = self.compile_pattern(ctx, b)?;
        let mut c = vec![];
        match a {
            Pattern::Choice(mut v) => c.extend(v.drain(..)),
            _ => c.push(a),
        }
        match b {
            Pattern::Choice(mut v) => c.extend(v.drain(..)),
            _ => c.push(b),
        }
        Ok(model::Pattern::Choice(c))
    }
    fn compile_datatype_value_pattern(&mut self, ctx: &mut Context, datatype_value: &types::DatatypeValuePattern) -> Result<model::Pattern, RelaxError> {
        Ok(model::Pattern::DatatypeValue(
            if let Some(name) = &datatype_value.0 { Some(self.compile_datatype_name(ctx, name)?) } else { None },
            datatype_value.1.as_string_value(),
        ))
    }
    fn compile_datatype_name(&mut self, ctx: &mut Context, name: &types::DatatypeName) -> Result<model::DatatypeName, RelaxError> {
        match name {
            types::DatatypeName::String => Ok(model::DatatypeName::String),
            types::DatatypeName::Token => Ok(model::DatatypeName::Token),
            types::DatatypeName::Name(types::CName(prefix, local)) => {
                Ok(model::DatatypeName::Name {
                    namespace_uri: ctx.datatype_uri_for_prefix(&prefix)?.to_string(),
                    name: local.1.clone()
                })
            },
        }
    }
    fn compile_datatype_name_pattern(&mut self, ctx: &mut Context, datatype_name: &types::DatatypeNamePattern) -> Result<model::Pattern, RelaxError> {
        Ok(model::Pattern::DatatypeName {
            name: self.compile_datatype_name(ctx, &datatype_name.0)?,
            params: if let Some(ref params) = datatype_name.1 { self.compile_params(ctx, params)? } else { vec![] },
            except: if let Some(ref except) = datatype_name.2 { Some(Box::new(self.compile_pattern(ctx, &except)?)) } else { None }
        })
    }
    fn compile_params(&mut self, ctx: &mut Context, params: &[types::Param]) -> Result<Vec<model::Param>, RelaxError> {
        let mut seen = HashSet::new();
        let mut result = vec![];
        for p in params {
            let name = p.0.to_string();
            if seen.contains(&name) {
                return Err(RelaxError::DuplicateParameterName(name));
            }
            result.push(self.compile_param(ctx, p)?);
            seen.insert(name);
        }
        Ok(result)
    }
    fn compile_param(&mut self, _ctx: &mut Context, param: &types::Param) -> Result<model::Param, RelaxError> {
        Ok(model::Param {
            name: param.0.to_string(),
            value: param.1.as_string_value()
        })
    }

    fn compile_declarations(ctx: &mut Context, schema: &Schema) -> Result<(), RelaxError> {
        for dec in schema.decls.iter() {
            match dec {
                types::Decl::DefaultNamespace(types::DefaultNamespaceDeclaration { prefix, uri }) => {
                    match uri {
                        NamespaceUriLiteral::Inherit => {
                            eprintln!("Can't inherit namespace {:?} at top level, I think?", prefix);
                        },
                        NamespaceUriLiteral::Uri(uri) => {
                            if let Some(ref p) = prefix {
                                ctx.declare_namespace(p.clone(), uri.as_string_value())?;
                            }
                        }
                    }
                },
                types::Decl::Namespace(types::NamespaceDeclaration { prefix, uri }) => {
                    match uri {
                        NamespaceUriLiteral::Inherit => {
                            eprintln!("Can't inherit namespace {:?} at top level, I think?", prefix);
                        },
                        NamespaceUriLiteral::Uri(uri) => {
                            ctx.declare_namespace(prefix.clone(), uri.as_string_value())?;
                        }
                    }
                },
                types::Decl::Datatypes(types::DatatypesDeclaration { prefix, uri }) => {
                    ctx.declare_datatype(prefix.clone(), uri.as_string_value())?;
                }
            }
        }
        Ok(())
    }

    fn compile_nameclass(&mut self, ctx: &mut Context, elem_attr: ElemAttr, name_class: &types::NameClass) -> Result<model::NameClass, RelaxError> {
        // TODO: wrap namespace-prefix resolution failure with source-span information from the AST
        match name_class {
            types::NameClass::Name(name) => match name {
                types::Name::Identifier(id) => Ok(model::NameClass::named(
                    if elem_attr == ElemAttr::Element { ctx.default_namespace_uri().to_string() } else { "".to_string() },
                    id.to_string()
                )),
                types::Name::CName(cname) => Ok(model::NameClass::named(ctx.namespace_uri_for_prefix(&cname.0)?.to_string(), (cname.1).1.clone())),
            },
            types::NameClass::NsName(types::NsName(name, except)) => {
                let except = if let Some(except) = except {
                    Some(self.compile_nameclass(ctx, elem_attr, except)?)
                } else {
                    None
                };
                Ok(model::NameClass::ns_name(ctx.namespace_uri_for_prefix(name)?.to_string(), except))
            },
            types::NameClass::AnyName(types::AnyName(except)) => {
                Ok(model::NameClass::any_name(if let Some(except) = except {
                    Some(self.compile_nameclass(ctx, elem_attr, except)?)
                } else {
                    None
                }))
            },
            types::NameClass::Alt(types::AltName(a, b)) => {
                let a = self.compile_nameclass(ctx, elem_attr, a)?;
                let b = self.compile_nameclass(ctx, elem_attr, b)?;
                Ok(model::NameClass::alt(a, b))
            },
            types::NameClass::Paren(types::ParenName(n)) => self.compile_nameclass(ctx, elem_attr, n),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum ElemAttr {
    Element,
    Attribute,
}

#[cfg(test)]
mod tests {
    use super::*;

    // TODO: maybe take tests from https://github.com/relaxng/jing-trang/blob/master/mod/rng-validate/test/spectest.xml

    #[test]
    fn it_works() {
        let tests = [
            "../relaxng-compact-syntax/resources/examples/fo/main.rnc",
            "../relaxng-compact-syntax/resources/examples/nu-validator/html5/html5.rnc",
            "../dash-mpd/DASH-MPD.rnc",
            // svg11 needs annotation support
            //"../relaxng-compact-syntax/resources/examples/nu-validator/svg11/svg11.rnc",
        ];
        for n in &tests {
            let mut compiler = Compiler::default();
            let input = Path::new(n);
            //let input = Path::new();
            compiler.compile(input).expect("compiler.load()");
        }
    }
}
