use std::{fmt::Display, ops::Range};

pub type Span = Range<usize>;

#[derive(Debug, PartialEq)]
pub struct Schema {
    pub decls: Vec<Decl>,
    pub pattern_or_grammar: PatternOrGrammar,
}

#[derive(Debug, PartialEq)]
pub enum Decl {
    Namespace(NamespaceDeclaration),
    DefaultNamespace(DefaultNamespaceDeclaration),
    Datatypes(DatatypesDeclaration),
}

#[derive(Debug, PartialEq)]
pub struct NamespaceDeclaration {
    pub prefix: String,
    pub uri: NamespaceUriLiteral,
}

#[derive(Debug, PartialEq)]
pub struct DefaultNamespaceDeclaration {
    pub prefix: Option<String>,
    pub uri: NamespaceUriLiteral,
}

#[derive(Debug, PartialEq)]
pub struct DatatypesDeclaration {
    pub prefix: String,
    pub uri: Literal,
}

// TODO: ensure all patterns have spans
#[derive(Debug, PartialEq)]
pub enum Pattern {
    Element(ElementPattern),
    Attribute(AttributePattern),
    List(ListPattern),
    Mixed(MixedPattern),
    // TODO: maybe rename this variant 'Ref'
    Identifier(Identifier),
    Parent(Identifier),
    Empty,
    Text(Option<Span>),
    NotAllowed,
    External(ExternalPattern),
    Grammar(GrammarPattern),
    Group(Box<Pattern>),
    // TODO: don't simplify into 'pairs' at this level of representation; have these hold
    //       Vec<Pattern>, and then simplify into pairs when transforming into relaxng-model form
    // TODO: ListPart should really be GroupPair to follow spec terminology
    ListPair(Box<Pattern>, Box<Pattern>),
    InterleavePair(Box<Pattern>, Box<Pattern>),
    ChoicePair(Box<Pattern>, Box<Pattern>),
    Optional(Box<Pattern>),
    ZeroOrMore(Box<Pattern>),
    OneOrMore(Box<Pattern>),
    DatatypeValue(DatatypeValuePattern),
    DatatypeName(DatatypeNamePattern),
    Annotated(Annotations, Box<Pattern>),
}

#[derive(Debug, PartialEq)]
pub struct ElementPattern {
    pub span: Span,
    pub name_class: NameClass,
    pub pattern: Box<Pattern>,
}

#[derive(Debug, PartialEq)]
pub struct GrammarPattern {
    pub span: Span,
    pub content: Vec<GrammarContent>,
}

#[derive(Debug, PartialEq)]
pub struct AttributePattern {
    pub span: Span,
    pub name_class: NameClass,
    pub pattern: Box<Pattern>,
}

#[derive(Debug, PartialEq)]
pub struct ListPattern(pub Box<Pattern>);

#[derive(Debug, PartialEq)]
pub struct MixedPattern(pub Box<Pattern>);

#[derive(Debug, PartialEq)]
pub struct ExternalPattern(pub Literal, pub Option<Inherit>, pub Option<String>);

// DatatypeValuePattern & DatatypeNamePattern diverge in style between xml and compact syntaxes,
// with compact syntax requiring the type name be qualified to identify the datatype-library, were
// xml syntax instead requires an unqualified name and the (possibly inherited) datatypeLibrary
// attribute

#[derive(Debug, PartialEq)]
pub struct DatatypeValuePattern(
    pub Span,
    // The default datatype if the schema doesn't specify one explicitly is "token"
    pub Option<DatatypeName>,
    pub Literal,
    /// The `ns` attribute value from the schema element, used as the default namespace
    /// when resolving namespace-sensitive values like QNames.
    pub Option<String>,
    /// In-scope namespace prefix bindings from the schema element (XML syntax only),
    /// used to resolve prefixed values like QNames at compile time.
    pub Vec<(String, String)>,
);

#[derive(Debug, PartialEq)]
pub struct DatatypeNamePattern(
    pub Span,
    pub DatatypeName,
    pub Option<Vec<Param>>,
    pub Option<Box<Pattern>>,
);

#[derive(Debug, PartialEq)]
pub enum DatatypeName {
    // TODO: special cases for "string" and "token" maybe not worth the trouble
    String,
    Token,
    // Per compact-syntax usage
    CName(QName),
    // Per xml-syntax usage
    NamespacedName(NamespacedName),
}

#[derive(Debug, PartialEq)]
pub struct NamespacedName {
    pub namespace_uri: Literal,
    pub localname: NcName,
}

impl NamespacedName {
    pub(crate) fn span(&self) -> Span {
        Span {
            start: self.namespace_uri.0.start,
            end: self.localname.0.end,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Param {
    pub span: Span,
    pub annotations: Option<Annotations>,
    pub name: IdentifierOrKeyword,
    pub value: Literal,
}

// TODO: remove this; Pattern can be a grammar anyway
#[derive(Debug, PartialEq)]
pub enum PatternOrGrammar {
    Pattern(Pattern),
    Grammar(GrammarPattern),
}

#[derive(Debug, PartialEq)]
pub enum GrammarContent {
    Define(Define),
    Div(Vec<GrammarContent>),
    Include(Include),
    Annotation(AnnotationElement),
}

#[derive(Debug, PartialEq)]
pub enum AssignMethod {
    Assign,
    Choice,
    Interleave,
}

#[derive(Debug, PartialEq)]
pub struct Define {
    pub span: Span,
    pub identifier: Identifier,
    pub assign_method: AssignMethod,
    pub pattern: Pattern,
    pub annotations: Option<Annotations>,
}

#[derive(Debug, PartialEq)]
pub struct Include {
    pub uri: Literal,
    pub inherit: Option<Inherit>,
    pub content: Option<Vec<IncludeContent>>,
    pub annotations: Option<Annotations>,
    pub ns: Option<String>,
}

#[derive(Debug, PartialEq)]
pub struct Inherit(pub IdentifierOrKeyword);

#[derive(Debug, PartialEq)]
pub enum IncludeContent {
    Define(Define),
    Div(Vec<IncludeContent>),
    Annotation(AnnotationElement),
}

// TODO: the spec shows that a keywords may also be used in positon were identifiers are expected,
//       but in hindsight it is not useful to maintain this destinction here in the data model;
//       remove this enum and use Identifier alone
#[derive(Debug, PartialEq)]
pub enum IdentifierOrKeyword {
    Identifier(Identifier),
    Keyword(Keyword),
}

impl IdentifierOrKeyword {
    pub fn span(&self) -> Span {
        match self {
            IdentifierOrKeyword::Identifier(i) => i.0.clone(),
            IdentifierOrKeyword::Keyword(k) => k.0.clone(),
        }
    }
}

impl Display for IdentifierOrKeyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IdentifierOrKeyword::Identifier(id) => f.write_str(&id.1),
            IdentifierOrKeyword::Keyword(k) => f.write_str(&k.1),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum NamespaceUriLiteral {
    Inherit,
    Uri(Literal),
}

#[derive(Debug, PartialEq)]
pub struct Literal(pub Span, pub Vec<LiteralSegment>);
impl Literal {
    pub fn new(span: Span, body: String) -> Literal {
        Literal(span, vec![LiteralSegment { body }])
    }
    pub fn as_string_value(&self) -> String {
        // TODO any unescaping etc reqiored?
        let mut val = String::new();
        for s in self.1.iter() {
            val.push_str(&s.body);
        }
        val
    }
}

#[derive(Debug, PartialEq)]
pub struct LiteralSegment {
    pub body: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier(pub Span, pub String);

#[derive(Debug, PartialEq)]
pub struct Keyword(pub Span, pub String);

#[derive(Debug, PartialEq)]
pub struct NcName(pub Span, pub String);

#[derive(Debug, PartialEq)]
pub enum NameClass {
    Name(Name),
    NsName(NsName),
    AnyName(AnyName),
    Alt(AltName),
    Paren(ParenName),
    Annotated(Annotations, Box<NameClass>),
}

#[derive(Debug, PartialEq)]
pub enum Name {
    Identifier(IdentifierOrKeyword),
    // Per compact-syntax usage
    CName(QName),
    // Per xml-syntax usage
    NamespacedName(NamespacedName),
}

impl Name {
    pub fn span(&self) -> Span {
        match self {
            Name::Identifier(i) => i.span(),
            Name::CName(n) => n.span(),
            Name::NamespacedName(n) => n.span(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum NamespaceOrPrefix {
    // Per compact-syntax usage
    Prefix(NcName),
    // Per xml-syntax usage
    NamespaceUri(Literal),
}
#[derive(Debug, PartialEq)]
pub struct NsName {
    pub name: NamespaceOrPrefix,
    pub except: Option<Box<NameClass>>,
}

#[derive(Debug, PartialEq)]
pub struct AnyName(pub Option<Box<NameClass>>);
#[derive(Debug, PartialEq)]
// TODO: ChoiceName would better match terminology used elsewhere
pub struct AltName(pub Box<NameClass>, pub Box<NameClass>);
#[derive(Debug, PartialEq)]
pub struct ParenName(pub Box<NameClass>);

#[derive(Debug, PartialEq)]
pub struct QName(pub NcName, pub NcName);

impl QName {
    pub fn span(&self) -> Span {
        Span {
            start: (self.0).0.start,
            end: self.1.0.end,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Annotations {
    pub documentation: Vec<Documentation>,
    pub initial: Option<InitialAnnotation>,
    pub follow_elements: Vec<AnnotationElement>,
}

#[derive(Debug, PartialEq)]
pub struct Documentation {
    pub span: Span,
    pub content: String,
}

#[derive(Debug, PartialEq)]
pub struct InitialAnnotation {
    pub span: Span,
    pub attribute_annotations: Vec<AnnotationAttribute>,
    pub element_annotations: Vec<AnnotationElement>,
}

#[derive(Debug, PartialEq)]
pub struct AnnotationAttribute {
    pub span: Span,
    pub name: Name,
    pub value: Literal,
}

#[derive(Debug, PartialEq)]
pub struct AnnotationElement {
    pub span: Span,
    pub name: Name,
    pub annotation_attributes: Vec<AnnotationAttribute>,
    pub annotation_elements_or_literals: Vec<AnnotationElementOrLiteral>,
}

#[derive(Debug, PartialEq)]
pub enum AnnotationElementOrLiteral {
    Element(AnnotationElement),
    Literal(Literal),
}
