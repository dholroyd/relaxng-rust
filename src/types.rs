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
    pub name_class: NameClass,
    pub pattern: Box<Pattern>,
}

#[derive(Debug, PartialEq)]
pub struct GrammarPattern(pub Vec<GrammarContent>);

#[derive(Debug, PartialEq)]
pub struct AttributePattern {
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
pub struct Param(pub IdentifierOrKeyword, pub Literal);

#[derive(Debug, PartialEq)]
pub enum PatternOrGrammar {
    Pattern(Pattern),
    Grammar(Vec<GrammarContent>),
}

#[derive(Debug, PartialEq)]
pub enum GrammarContent {
    Start(Start),
    Define(Define),
    Div(Vec<GrammarContent>),
    Include(Include),
}

#[derive(Debug, PartialEq)]
pub struct Start(pub AssignMethod, pub Pattern);

#[derive(Debug, PartialEq)]
pub enum AssignMethod {
    Assign,
    Choice,
    Interleave,
}

#[derive(Debug, PartialEq)]
pub struct Define(pub Identifier, pub AssignMethod, pub Pattern);

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
    Start(Start),
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
            IdentifierOrKeyword::Identifier(id) => id.0.clone(),
            IdentifierOrKeyword::Keyword(k) => k.0.clone(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum NamespaceUriLiteral {
    Inherit,
    Uri(Literal),
}

#[derive(Debug, PartialEq)]
pub struct Literal(pub Vec<LiteralSegment>);

#[derive(Debug, PartialEq)]
pub struct LiteralSegment {
    pub body: String,
}

#[derive(Debug, PartialEq)]
pub struct Identifier(pub String);

#[derive(Debug, PartialEq)]
pub struct Keyword(pub String);

#[derive(Debug, PartialEq)]
pub struct NcName(pub String);

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
