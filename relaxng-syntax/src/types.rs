use std::ops::Range;

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
    Text,
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
pub struct ExternalPattern(pub Literal, pub Option<Inherit>);

// DatatypeValuePattern & DatatypeNamePattern diverge in style between xml and compact syntaxes,
// with compact syntax requiring the type name be qualified to identify the datatype-library, were
// xml syntax instead requires an unqualified name and the (possibly inherited) datatypeLibrary
// attribute

#[derive(Debug, PartialEq)]
pub struct DatatypeValuePattern(
    // The default datatype if the schema doesn't specify one explicitly is "token"
    pub Option<DatatypeName>,
    pub Literal,
);

#[derive(Debug, PartialEq)]
pub struct DatatypeNamePattern(
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
pub struct Param(
    pub Span,
    pub Option<InitialAnnotation>,
    pub IdentifierOrKeyword,
    pub Literal,
);

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
pub struct Define(pub Span, pub Identifier, pub AssignMethod, pub Pattern);

#[derive(Debug, PartialEq)]
pub struct Include(
    pub Literal,
    pub Option<Inherit>,
    pub Option<Vec<IncludeContent>>,
);

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

impl ToString for IdentifierOrKeyword {
    fn to_string(&self) -> String {
        match self {
            IdentifierOrKeyword::Identifier(id) => id.1.clone(),
            IdentifierOrKeyword::Keyword(k) => k.1.clone(),
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
            end: self.1 .0.end,
        }
    }
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
