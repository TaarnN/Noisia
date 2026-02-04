#![allow(unused_variables)]
#![allow(unused_assignments)]
#![allow(dead_code)]

use crate::tokenizer::Token;
use crate::style::Style;
use std::fmt;

#[derive(Debug)]
// note: parser error kinds
pub enum ParseError {
    UnexpectedToken {
        expected: String,
        found: Token,
        idx: usize,
    },
    EOF {
        message: String,
        line: usize,
        column: usize,
    },
    Generic {
        message: String,
        line: usize,
        column: usize,
    },
}

impl fmt::Display for ParseError {
    // note: human friendly parse error
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.pretty(&Style::plain()))
    }
}

impl std::error::Error for ParseError {}

impl ParseError {
    pub fn pretty(&self, style: &Style) -> String {
        match self {
            ParseError::UnexpectedToken {
                expected,
                found,
                idx,
            } => {
                let header = style.paint("error[parse] Unexpected token", &["1", "31"]);
                let loc = style.fg_yellow(&format!("{}:{}", found.line, found.column));
                let idx_note = style.dim(&format!("(token #{})", idx));
                let expected_line = format!(
                    "  {} {}",
                    style.dim("expected:"),
                    style.fg_cyan(expected)
                );
                let found_lexeme = if found.lexeme.is_empty() {
                    "<empty>".to_string()
                } else {
                    format!("`{}`", found.lexeme.escape_default())
                };
                let found_value = format!("{:?} {}", found.token_type, found_lexeme);
                let found_line = format!(
                    "  {} {}",
                    style.dim("found:"),
                    style.fg_green(&found_value)
                );
                format!(
                    "{}\n  {} {} {}\n{}\n{}",
                    header,
                    style.dim("at"),
                    loc,
                    idx_note,
                    expected_line,
                    found_line
                )
            }
            ParseError::EOF {
                message,
                line,
                column,
            } => {
                let header = style.paint("error[parse] Unexpected end of input", &["1", "31"]);
                let loc = style.fg_yellow(&format!("{}:{}", line, column));
                let details = format!("  {} {}", style.dim("details:"), style.fg_cyan(message));
                format!("{}\n  {} {}\n{}", header, style.dim("at"), loc, details)
            }
            ParseError::Generic {
                message,
                line,
                column,
            } => {
                let header = style.paint("error[parse]", &["1", "31"]);
                let loc = style.fg_yellow(&format!("{}:{}", line, column));
                format!(
                    "{} {}\n  {} {}",
                    header,
                    style.fg_cyan(message),
                    style.dim("at"),
                    loc
                )
            }
        }
    }
}

// note: parser result alias
pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone)]
// note: whole program AST
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
// note: top-level item kinds
pub enum Item {
    ModuleDecl(ModuleDecl),
    Import(ImportDecl),
    MacroDecl(MacroDecl),
    ExtensionDecl(ExtensionDecl),
    IGMDecl(IGMDecl),
    PluginDecl(PluginDecl),
    Function(FunctionDecl),
    Struct(StructDecl),
    Enum(EnumDecl),
    Trait(TraitDecl),
    Impl(ImplDecl),
    MixinDecl(MixinDecl),
    InterfaceDecl(InterfaceDecl),
    ProtocolDecl(ProtocolDecl),
    Class(ClassDecl),
}

#[derive(Debug, Clone)]
// note: module declaration
pub struct ModuleDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: module path like A::B
    pub path: String,
}

#[derive(Debug, Clone)]
// note: import declaration
pub struct ImportDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: module path like A::B
    pub path: String,
    // note: optional symbols list
    pub symbols: Option<Vec<String>>,
}

#[derive(Debug, Clone)]
// note: macro declaration
pub struct MacroDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: macro name
    pub name: String,
    // note: macro params as raw identifiers
    pub params: Vec<String>,
    // note: macro body
    pub body: Block,
}

#[derive(Debug, Clone)]
// note: extension declaration
pub struct ExtensionDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: optional receiver binding name
    pub receiver: Option<String>,
    // note: target type for extension
    pub target: TypeRef,
    // note: method list
    pub methods: Vec<FunctionDecl>,
}

#[derive(Debug, Clone)]
// note: inline grammar macro declaration
pub struct IGMDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: macro name
    pub name: String,
    // note: regex pattern string
    pub pattern: String,
    // note: expand params
    pub expand_params: Vec<Param>,
    // note: expand expression
    pub expand: Option<Expr>,
}

#[derive(Debug, Clone)]
// note: plugin declaration
pub struct PluginDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: plugin name
    pub name: String,
    // note: optional activation condition
    pub condition: Option<Expr>,
}

#[derive(Debug, Clone)]
// note: mixin declaration
pub struct MixinDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: mixin name
    pub name: String,
    // note: field list
    pub fields: Vec<ClassFieldDecl>,
    // note: property list
    pub properties: Vec<ClassPropertyDecl>,
    // note: delegate list
    pub delegates: Vec<ClassDelegateDecl>,
    // note: method list
    pub methods: Vec<FunctionDecl>,
}

#[derive(Debug, Clone)]
// note: interface declaration
pub struct InterfaceDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: interface name
    pub name: String,
    // note: generic params
    pub generics: Vec<GenericParam>,
    // note: method list
    pub methods: Vec<FunctionDecl>,
}

#[derive(Debug, Clone)]
// note: protocol declaration
pub struct ProtocolDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: protocol name
    pub name: String,
    // note: generic params
    pub generics: Vec<GenericParam>,
    // note: associated types
    pub associated_types: Vec<String>,
    // note: method list
    pub methods: Vec<FunctionDecl>,
    // note: extension method list
    pub extensions: Vec<FunctionDecl>,
}

#[derive(Debug, Clone)]
// note: function signature and body
pub struct FunctionDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: access level
    pub visibility: Visibility,
    // note: extra flags like async or multidispatch
    pub modifiers: Vec<FunctionModifier>,
    // note: function name
    pub name: String,
    // note: quick async flag
    pub is_async: bool,
    // note: generic params like <T>
    pub generics: Vec<GenericParam>,
    // note: param list
    pub params: Vec<Param>,
    // note: context params after (using ...)
    pub context_params: Vec<Param>,
    // note: return type or None
    pub ret_type: Option<TypeRef>,
    // note: effect list after marker
    pub effects: Vec<Effect>,
    // note: where bounds
    pub where_clauses: Vec<WhereClause>,
    // note: body block or None for decl
    pub body: Option<Block>,
}

#[derive(Debug, Clone)]
// note: type param with bounds
pub struct GenericParam {
    // note: param name
    pub name: String,
    // note: bounds for this param
    pub constraints: Vec<TypeConstraint>,
}

#[derive(Debug, Clone)]
// note: constraint kinds
pub enum TypeConstraint {
    TraitBound(String),
    LifetimeBound(String),
    TypeEq(String, String),
    SubtypeOf(String),
    SupertypeOf(String),
}

#[derive(Debug, Clone)]
// note: where clause node
pub struct WhereClause {
    // note: name on left side
    pub param_name: String,
    // note: bounds list
    pub constraints: Vec<TypeConstraint>,
}

#[derive(Debug, Clone)]
// note: effect name and params
pub struct Effect {
    // note: effect name
    pub name: String,
    // note: optional type params
    pub params: Option<Vec<TypeRef>>,
}

#[derive(Debug, Clone, PartialEq)]
// note: access level
pub enum Visibility {
    Public,
    Private,
    Protected,
    Internal,
    Package,
}

#[derive(Debug, Clone)]
// note: function modifiers
pub enum FunctionModifier {
    Async,
    Multidispatch,
    Constexpr,
    Scoped,
    Comptime,
}

#[derive(Debug, Clone)]
// note: function param pattern
pub struct Param {
    // note: pattern for destructuring
    pub pattern: Pattern,
    // note: optional type
    pub typ: Option<TypeRef>,
    // note: default value
    pub default: Option<Expr>,
}

#[derive(Debug, Clone)]
// note: struct declaration
pub struct StructDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: struct name
    pub name: String,
    // note: field list (name, type)
    pub fields: Vec<(String, TypeRef)>,
}

#[derive(Debug, Clone)]
// note: enum declaration
pub struct EnumDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: enum name
    pub name: String,
    // note: generic params
    pub generics: Vec<GenericParam>,
    // note: variant list
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone)]
// note: enum variant field
pub struct EnumVariantField {
    // note: optional field name
    pub name: Option<String>,
    // note: field type
    pub typ: TypeRef,
}

#[derive(Debug, Clone)]
// note: enum variant shape
pub enum VariantKind {
    Unit,
    Tuple(Vec<EnumVariantField>),
    Struct(Vec<EnumVariantField>),
}

#[derive(Debug, Clone)]
// note: enum variant
pub struct EnumVariant {
    pub name: String,
    pub kind: VariantKind,
}

#[derive(Debug, Clone)]
// note: trait declaration
pub struct TraitDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: trait name
    pub name: String,
    // note: method list
    pub methods: Vec<FunctionDecl>,
}

#[derive(Debug, Clone)]
// note: impl block
pub struct ImplDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: Some(trait) for trait impl, None for inherent
    pub trait_name: Option<String>,
    // note: target type for impl
    pub target: TypeRef,
    // note: method list
    pub methods: Vec<FunctionDecl>,
}

#[derive(Debug, Clone)]
// note: class declaration
pub struct ClassDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: class name
    pub name: String,
    // note: extends list
    pub extends: Vec<TypeRef>,
    // note: mixin list after with
    pub mixins: Vec<TypeRef>,
    // note: implements list
    pub implements: Vec<TypeRef>,
    // note: field list
    pub fields: Vec<ClassFieldDecl>,
    // note: property list
    pub properties: Vec<ClassPropertyDecl>,
    // note: static init blocks
    pub static_inits: Vec<Block>,
    // note: deinit block
    pub deinit: Option<Block>,
    // note: delegate list
    pub delegates: Vec<ClassDelegateDecl>,
    // note: constructor list
    pub ctors: Vec<ConstructorDecl>,
    // note: method list
    pub methods: Vec<FunctionDecl>,
}

#[derive(Debug, Clone)]
// note: class field
pub struct ClassFieldDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: access level
    pub vis: Visibility,
    // note: mutable flag
    pub mutable: bool,
    // note: field name
    pub name: String,
    // note: field type
    pub typ: TypeRef,
    // note: default value
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
// note: class property
pub struct ClassPropertyDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: access level
    pub vis: Visibility,
    // note: property name
    pub name: String,
    // note: optional type
    pub typ: Option<TypeRef>,
    // note: computed value expression
    pub value: Option<Expr>,
    // note: getter block
    pub getter: Option<Block>,
    // note: setter param name
    pub setter_param: Option<String>,
    // note: setter block
    pub setter_body: Option<Block>,
}

#[derive(Debug, Clone)]
// note: class delegate
pub struct ClassDelegateDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: access level
    pub vis: Visibility,
    // note: delegate name
    pub name: String,
    // note: target expression
    pub target: Expr,
}

#[derive(Debug, Clone)]
// note: constructor declaration
pub struct ConstructorDecl {
    // note: raw @attrs in source order
    pub attributes: Vec<String>,
    // note: access level
    pub visibility: Visibility,
    // note: optional named constructor
    pub name: Option<String>,
    // note: param list
    pub params: Vec<Param>,
    // note: constructor body
    pub body: Block,
}

#[derive(Debug, Clone)]
// note: block of statements
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
// note: statement kinds
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
    TryCatch {
        try_block: Block,
        catch_name: String,
        catch_block: Block,
    },
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
        pattern: Pattern,
        iter: Expr,
        body: Block,
    },
    Loop {
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
        history: bool,
        variable: String,
        body: Block,
        until: Expr,
    },
    InContext {
        target: Expr,
        arms: Vec<ContextArm>,
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
    MatchCond {
        arms: Vec<MatchCondArm>,
    },
    OnSequence {
        target: Expr,
        sequence: Expr,
        body: Block,
    },
    PanicUnless {
        condition: Expr,
    },
    Scope {
        body: Block,
    },
    Defer {
        expr: Expr,
    },
    Checkpoint {
        name: Option<String>,
        metadata: Option<Expr>,
        body: Block,
    },
    Rewind {
        target: Option<Expr>,
        condition: Option<Expr>,
        query: Option<Block>,
    },
    Inspect {
        target: Option<Expr>,
        filter: Option<Block>,
        body: Option<Block>,
    },
    TemporalScope {
        name: Option<String>,
        body: Block,
    },
    TemporalTransaction {
        config: Option<Expr>,
        body: Block,
        catch_name: Option<String>,
        catch_block: Option<Block>,
    },
    TemporalTest {
        name: String,
        body: Block,
    },
    TemporalMemory {
        config: Option<Expr>,
        body: Block,
    },
    BatchTemporal {
        body: Block,
        optimize: Option<Expr>,
    },
    Replay {
        recording: Expr,
        body: Block,
    },
    Snapshot {
        name: Option<String>,
        metadata: Option<Expr>,
    },
    Rollback {
        target: Option<Expr>,
        condition: Option<Expr>,
        metadata: Option<Expr>,
    },
    DebugTemporal {
        body: Block,
    },
    TemporalHandle {
        body: Block,
        handlers: Vec<TemporalClause>,
    },
    TemporalMatch {
        target: Expr,
        clauses: Vec<TemporalClause>,
    },
}

#[derive(Debug, Clone)]
// note: temporal pattern
pub struct TemporalPattern {
    // note: pattern kind keyword
    pub kind: String,
    // note: raw args for the pattern
    pub args: Vec<Expr>,
    // note: optional condition
    pub condition: Option<Expr>,
}

#[derive(Debug, Clone)]
// note: temporal clause
pub struct TemporalClause {
    pub pattern: TemporalPattern,
    pub guard: Option<Expr>,
    pub body: Block,
}

#[derive(Debug, Clone)]
// note: match arm
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Expr,
}

#[derive(Debug, Clone)]
// note: pattern node
pub struct Pattern {
    // note: kind of pattern
    pub kind: PatternKind,
    // note: bindings found in pattern
    pub bindings: Vec<String>,
}

#[derive(Debug, Clone)]
// note: pattern variants
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
// note: watch clause
pub struct WatchClause {
    pub condition: Expr,
    pub body: Block,
}

#[derive(Debug, Clone)]
// note: context arm
pub struct ContextArm {
    pub value: Expr,
    pub body: Block,
}

#[derive(Debug, Clone)]
// note: match condition arm
pub struct MatchCondArm {
    pub condition: Expr,
    pub body: Block,
}

#[derive(Debug, Clone)]
// note: expression kinds
pub enum Expr {
    Literal(Literal),
    Ident(String),
    Await(Box<Expr>),
    Spawn(Box<Expr>),
    Try(Box<Expr>),
    IfExpr {
        cond: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    },
    MatchExpr {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
    },
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
    Block(Block),
    Call {
        callee: Box<Expr>,
        generics: Vec<TypeRef>,
        args: Vec<Expr>,
    },
    MethodCall {
        object: Box<Expr>,
        method: String,
        generics: Vec<TypeRef>,
        args: Vec<Expr>,
    },
    FieldAccess {
        object: Box<Expr>,
        field: String,
    },
    OptionalFieldAccess {
        object: Box<Expr>,
        field: String,
    },
    Index {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    Slice {
        target: Box<Expr>,
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
    },
    Lambda {
        params: Vec<Param>,
        body: Box<Expr>,
    },
    Range {
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
        inclusive: bool,
        step: Option<Box<Expr>>,
    },
    Pipeline {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    SelectorPipeline {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    ListComp {
        expr: Box<Expr>,
        pattern: Pattern,
        iter: Box<Expr>,
        guard: Option<Box<Expr>>,
    },
    Coalesce {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    OptionalCall {
        callee: Box<Expr>,
        generics: Vec<TypeRef>,
        args: Vec<Expr>,
    },
    InterpolatedString {
        parts: Vec<InterpolatedPart>,
    },
    MacroCall {
        name: String,
        args: Vec<Expr>,
    },
    PointerDeref {
        expr: Box<Expr>,
    },
    PointerNew {
        pointer_type: PointerType,
        expr: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
// note: interpolated string part kind
pub enum InterpolatedPartKind {
    Text(String),
    Expr(Expr),
}

#[derive(Debug, Clone)]
// note: interpolated string part
pub struct InterpolatedPart {
    pub kind: InterpolatedPartKind,
}

#[derive(Debug, Clone)]
// note: literal values
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
        base: Option<Box<Expr>>,
        fields: Vec<(String, Expr)>,
    },
    Tuple(Vec<Expr>),
}

#[derive(Debug, Clone)]
// note: type reference
pub struct TypeRef {
    // note: type name or path
    pub name: String,
    // note: generic args
    pub generics: Vec<TypeRef>,
    // note: nullable flag
    pub nullable: bool,
    // note: pointer flavor
    pub pointer_type: Option<PointerType>,
}

#[derive(Debug, Clone, PartialEq)]
// note: pointer flavor
pub enum PointerType {
    RawPointer,
    ManagedPointer,
    WeakPointer,
    SharedPointer,
}
