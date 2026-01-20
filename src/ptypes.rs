#![allow(unused_variables)]
#![allow(unused_assignments)]
#![allow(dead_code)]

use crate::tokenizer::{Token};
use std::fmt;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: String,
        found: Token,
        idx: usize,
    },
    EOF {
        message: String,
    },
    Generic(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                expected,
                found,
                idx,
            } => write!(
                f,
                "Parse error at {}:{}: expected {}, found {}",
                found.line, found.column, expected, found
            ),
            ParseError::EOF { message } => write!(f, "Unexpected EOF: {}", message),
            ParseError::Generic(s) => write!(f, "Parse error: {}", s),
        }
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub enum Item {
    ModuleDecl(ModuleDecl),
    Import(ImportDecl),
    Function(FunctionDecl),
    Struct(StructDecl),
    Enum(EnumDecl),
    Trait(TraitDecl),
    Impl(ImplDecl),
    // Class(ClassDecl),
    // ... add other top-level items: trait, impl, class, macro, etc.
}

#[derive(Debug, Clone)]
pub struct ModuleDecl {
    pub path: String,
}

#[derive(Debug, Clone)]
pub struct ImportDecl {
    pub path: String,
    pub symbols: Option<Vec<String>>, // `import A::{x,y}` or `import A::B`
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub attributes: Vec<String>,
    pub visibility: Visibility,
    pub modifiers: Vec<FunctionModifier>,
    pub name: String,
    pub is_async: bool,
    pub generics: Vec<GenericParam>,
    pub params: Vec<Param>,
    pub ret_type: Option<TypeRef>,
    pub effects: Vec<Effect>,
    pub where_clauses: Vec<WhereClause>,
    pub body: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct GenericParam {
    pub name: String,
    pub constraints: Vec<TypeConstraint>,
}

#[derive(Debug, Clone)]
pub enum TypeConstraint {
    TraitBound(String),
    LifetimeBound(String),
    TypeEq(String, String),
    SubtypeOf(String),
    SupertypeOf(String),
}

#[derive(Debug, Clone)]
pub struct WhereClause {
    pub param_name: String,
    pub constraints: Vec<TypeConstraint>,
}

#[derive(Debug, Clone)]
pub struct Effect {
    pub name: String,
    pub params: Option<Vec<TypeRef>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
    Protected,
    Internal,
    Package,
}

#[derive(Debug, Clone)]
pub enum FunctionModifier {
    Async,
    Multidispatch,
    // Add other modifiers as needed
}

#[derive(Debug, Clone)]
pub struct Param {
    pub pattern: Pattern,
    pub typ: Option<TypeRef>,
    pub default: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct StructDecl {
    pub name: String,
    // simple fields (name, type)
    pub fields: Vec<(String, TypeRef)>,
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub name: String,
    pub generics: Vec<GenericParam>,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone)]
pub enum VariantKind {
    Unit,
    Tuple(Vec<TypeRef>),
    Struct(Vec<(String, TypeRef)>),
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub kind: VariantKind,
}

#[derive(Debug, Clone)]
pub struct TraitDecl {
    pub name: String,
    pub methods: Vec<FunctionDecl>,
}

#[derive(Debug, Clone)]
pub struct ImplDecl {
    pub trait_name: Option<String>, // None = inherent impl, Some("Trait") = trait impl
    pub target: TypeRef,            // type name เช่น "Point"
    pub methods: Vec<FunctionDecl>, // body ของ impl
}

#[derive(Debug, Clone)]
pub struct ClassFieldDecl {
    pub attributes: Vec<String>,
    pub vis: Visibility,
    pub mutable: bool,
    pub name: String,
    pub typ: TypeRef,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct ConstructorDecl {
    pub name: Option<String>,
    pub params: Vec<Param>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        pattern: Pattern,
        mutable: bool,
        typ: Option<TypeRef>,
        expr: Option<Expr>,
    },
    Expr(Expr),
    Return(Option<Expr>),
    Continue,
    Break,
    If {
        cond: Expr,
        then_block: Block,
        else_block: Option<Block>,
    },
    While {
        cond: Expr,
        body: Block,
    },
    For {
        pat: String,
        iter: Expr,
        body: Block,
    },
    Match {
        expr: Expr,
        arms: Vec<MatchArm>,
    },
    Watch {
        variables: Vec<Expr>,
        clauses: Vec<WatchClause>,
    },
    Converge {
        variable: String,
        body: Block,
        until: Expr,
    },
    Within {
        time: Expr,
        condition: Option<Expr>,
        body: Block,
    },
    Atomically {
        body: Block,
    },
    Trap {
        error_condition: Expr,
        body: Block,
    },
    Guard {
        condition: Expr,
        then_block: Block,
        else_block: Option<Block>,
    },
    Poll {
        signal: Expr,
        interval: Expr,
        body: Block,
    },
    Delete {
        expr: Expr,
    },
    Joins {
        decls: Vec<Stmt>,
        body: Block,
    },
    // ... add more statements
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub kind: PatternKind,
    pub bindings: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    Identifier(String),
    Wildcard,
    Literal(Literal),
    Struct {
        name: Option<String>,
        fields: Vec<(String, Pattern)>,
    },
    Tuple(Vec<Pattern>),
    Or(Vec<Pattern>),
    SomeVariant(Box<Pattern>),
    NoneVariant,
    OkVariant(Box<Pattern>),
    ErrVariant(Box<Pattern>),
    EnumVariant {
        variant_name: String,
        inner_pattern: Option<Box<Pattern>>,
    },
    Array(Vec<Pattern>),
    Nil,
}

#[derive(Debug, Clone)]
pub struct WatchClause {
    pub condition: Expr,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Ident(String),
    Binary {
        left: Box<Expr>,
        op: String,
        right: Box<Expr>,
    },
    Unary {
        op: String,
        rhs: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Call {
        callee: Box<Expr>,
        generics: Vec<TypeRef>, // Added for type generics in calls
        args: Vec<Expr>,
    },
    MethodCall {
        object: Box<Expr>,
        method: String,
        generics: Vec<TypeRef>, // Added for type generics in method calls
        args: Vec<Expr>,
    },
    FieldAccess {
        object: Box<Expr>,
        field: String,
    },
    Index {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    Lambda {
        params: Vec<Param>,
        body: Box<Expr>,
    },
    Pipeline {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    // more: pointer ops, etc.
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(String),
    Float(String),
    String(String),
    Bool(bool),
    Unit {
        v: String,
        u: String,
    },
    Array(Vec<Expr>),
    Vector(Vec<Expr>),
    Struct {
        name: String,
        fields: Vec<(String, Expr)>,
    },
    Tuple(Vec<Expr>),
}

#[derive(Debug, Clone)]
pub struct TypeRef {
    pub name: String,           // ชื่อ type หลัก เช่น "Box", "Option", "Result"
    pub generics: Vec<TypeRef>, // พารามิเตอร์ generic เช่น [T], [K, V], [String, Int]
    pub nullable: bool,
    pub pointer_type: Option<PointerType>, // อนาคตอาจขยาย: array, slice, reference, etc.
}

#[derive(Debug, Clone, PartialEq)]
pub enum PointerType {
    RawPointer,
    ManagedPointer,
    WeakPointer,
    SharedPointer,
}