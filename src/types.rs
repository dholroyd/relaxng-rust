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

#[derive(Debug, PartialEq)]
pub enum Pattern {
    Element(ElementPattern),
    Attribute(AttributePattern),
    List(ListPattern),
    Mixed(MixedPattern),
    Identifier(Identifier),
    Parent(Identifier),
    Empty,
    Text,
    NotAllowed,
    External(ExternalPattern),
    Grammar(GrammarPattern),
    Group(Box<Pattern>),
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

#[derive(Debug, PartialEq)]
pub struct DatatypeValuePattern(pub Option<DatatypeName>, pub Literal);

#[derive(Debug, PartialEq)]
pub struct DatatypeNamePattern(
    pub DatatypeName,
    pub Option<Vec<Param>>,
    pub Option<Box<Pattern>>,
);

#[derive(Debug, PartialEq)]
pub enum DatatypeName {
    String,
    Token,
    Name(CName),
}

#[derive(Debug, PartialEq)]

pub struct Param(
    pub Span,
    pub IdentifierOrKeyword,
    pub Literal
);
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
}

#[derive(Debug, PartialEq)]
pub enum IdentifierOrKeyword {
    Identifier(Identifier),
    Keyword(Keyword),
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

#[derive(Debug, PartialEq)]
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
    CName(CName)
}
#[derive(Debug, PartialEq)]
pub struct NsName(pub NcName, pub Option<Box<NameClass>>);
#[derive(Debug, PartialEq)]
pub struct AnyName(pub Option<Box<NameClass>>);
#[derive(Debug, PartialEq)]
pub struct AltName(pub Box<NameClass>, pub Box<NameClass>);
#[derive(Debug, PartialEq)]
pub struct ParenName(pub Box<NameClass>);

#[derive(Debug, PartialEq)]
pub struct CName(pub NcName, pub NcName);
