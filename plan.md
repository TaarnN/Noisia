# Full Parser Coverage Plan for Noisia Syntax 1–45

## Summary
- Extend the tokenizer, AST (`src/ptypes.rs`), and parser (`src/parser.rs`) to cover every missing syntax item in sections 1–45.
- Keep semantics out of scope; the goal is pure syntax support and stable AST output.
- Add parser fixture tests for every new syntax cluster.

## Important API / Type Changes
- `src/tokenizer.rs`
  - Add token support for `?.` (optional chaining), regex literals `/.../`, and missing keywords used in sections 31–45 (`constexpr`, `using`, `plugin`, `temporal`, `checkpoint`, `rewind`, `snapshot`, `rollback`, `replay`, `batch`, `gc`, `preserve`, `debug`, `breakpoint`, `trace`, `analyze`, `assert`, `sequence`, `between`, `at`, `yield`, `by`).
  - Add tokenizer state to enable regex literal parsing when the previous non‑comment token is `pattern` or `=`.
  - Ensure `??` is treated as operator for coalescing.

- `src/ptypes.rs`
  - Add new `Item` variants:
    - `MacroDecl`, `ExtensionDecl`, `IGMDecl`, `PluginDecl`, `MixinDecl`, `InterfaceDecl`, `ProtocolDecl`.
  - Extend `FunctionDecl`:
    - Add modifiers for `Constexpr`, `Scoped`, and `Comptime` (if used).
    - Add `context_params: Vec<Param>` for `(...)(using ...)` syntax.
  - Update `Stmt`:
    - Add `Scope`, `Defer`, `Checkpoint`, `Rewind`, `Inspect`, `TemporalScope`, `TemporalTransaction`, `TemporalTest`, `TemporalMemory`, `BatchTemporal`, `Replay`, `Snapshot`, `Rollback`, `DebugTemporal`, `TemporalHandle`, `TemporalMatch`.
  - Update `Expr`:
    - Add `IfExpr`, `MatchExpr`, `Range`, `Slice`, `Coalesce`, `OptionalFieldAccess`, `OptionalCall`, `SelectorPipeline`, `InterpolatedString`, `MacroCall`, `PointerDeref`, `PointerNew`.
  - Update `Stmt::For` to use `pattern: Pattern` instead of `pat: String`.
  - Extend `ClassDecl` to include:
    - `properties: Vec<ClassPropertyDecl>`
    - `static_inits: Vec<Block>`
    - `deinit: Option<Block>`
    - `delegates: Vec<ClassDelegateDecl>`
  - Add supporting structs:
    - `InterpolatedPart`, `TemporalClause`, `TemporalPattern`, `ClassPropertyDecl`, `ClassDelegateDecl`.

## Implementation Steps

### 1) Tokenizer Updates (`src/tokenizer.rs`)
- Add missing keywords from 31–45 (see above) if not already in `init_keywords`.
- Add operator `?.` for optional chaining.
- Add regex literal tokenization when the previous non‑comment token is `pattern` or `=`:
  - New `TokenType::RegexLiteral`.
- Ensure `??` is treated as operator for coalescing.
- Keep existing `..` and `..=` operators; they will become range tokens in parser.

### 2) AST Extensions (`src/ptypes.rs`)
- Introduce new `Item` types for macros, extensions, IGM, plugins, mixins, interfaces, protocols.
- Add `Expr::IfExpr` and `Expr::MatchExpr` so `if` / `match` can be expressions.
- Add `Expr::Range { start, end, inclusive, step }` and `Expr::Slice { start, end, inclusive }`.
- Add `Expr::OptionalFieldAccess`, `Expr::OptionalCall`, `Expr::Coalesce`.
- Add `Expr::SelectorPipeline` and keep `Expr::Pipeline` for `|>`.
- Add `Expr::InterpolatedString { parts: Vec<InterpolatedPart> }`.
- Add pointer expressions: `Expr::PointerDeref { expr, safe }`, `Expr::PointerNew { kind, expr }`.

### 3) Parser Updates (`src/parser.rs`)
#### Expressions
- Parse `if/elif/else` as `Expr::IfExpr` (use `parse_block_or_expr_body` for branch bodies).
- Parse `match` as `Expr::MatchExpr`.
- Parse `??` as coalesce operator with low precedence.
- Parse `..` and `..=` as `Expr::Range`; detect optional `by <expr>` and store `step`.
- In `parse_postfix`, detect slices inside `[...]` and return `Expr::Slice`.
- Parse `?.` in `parse_postfix` to produce `OptionalFieldAccess` / `OptionalCall`.
- Parse `>>` as `Expr::SelectorPipeline` with the same precedence as `|>`.
- Parse interpolated strings by splitting `$"..."` lexeme on `{...}` and re‑tokenizing those fragments into expressions.
- Parse macro calls: `ident!(...)` into `Expr::MacroCall`.

#### Statements
- Update `if` statement parsing to allow `elif` chains.
- Update `for` parsing to accept patterns (`for i, v in ...` or `for (a,b) in ...`).
- Add `scope { ... }` and `defer expr`.
- Implement temporal statements:
  - `checkpoint`, `rewind`, `inspect`, `snapshot`, `rollback`, `replay`, `batch temporal`, `temporal scope`, `temporal transaction`, `temporal test`, `temporal memory`, `debug temporal`, `handle temporal effects`.

#### Declarations
- `macro name!(params) { ... }`
  - Body is a normal `Block`; allow `compile-time { ... }` as a special `Stmt::CompileTimeBlock`.
- `extension (name: Type) { ... }` and `extension Type { ... }`
  - Parse receiver + methods.
- `igm name! { pattern = /.../ expand(m: Match) = Expr }`
  - Store regex literal + expand signature + body expression.
- `plugin name when <expr>`
  - Store name + condition expression.
- `constexpr fn` and `comptime fn`
  - Parse as function modifiers.

#### Class Body Enhancements
- Computed property: `area :> expr`
- Getter/setter: `prop { get :> expr set(value) { ... } }`
- `deinit { ... }`
- `@static init { ... }`
- `delegate X to Expr()`
- Parse `mixin`, `interface`, `protocol` as top‑level declarations.

### 4) Temporal Syntax Rules (Decision‑Complete Parsing)
- `checkpoint`:
  - `checkpoint "name" { ... }`
  - `checkpoint { ... } as "name"`
  - `checkpoint "name" with { metadata } { ... }`
  - `checkpoint "name" with config { ... }`
- `rewind`:
  - `rewind`
  - `rewind to "name"`
  - `rewind to "name" if <expr>`
  - `rewind to latest where { ... }`
  - Expression form: `let x = rewind to "name" else <expr>`
- `inspect`:
  - `inspect current checkpoint`
  - `inspect history`
  - `inspect checkpoints where { ... }`
  - `inspect state at "name" { ... }`
  - `inspect diff between "A" and "B" { ... }`
- `temporal scope`, `temporal transaction`, `temporal test`, `temporal memory`:
  - Always parse `{ config } { body }` pattern; store config as optional key/value block.
- `batch temporal { ... } optimize for { ... }`
- `debug temporal { ... }` and `breakpoint`/`trace`/`analyze` blocks:
  - Parse clauses into `TemporalClause::Raw(Vec<Token>)` for now (syntax‑preserving, semantics deferred).
- `match temporal state { ... }`:
  - Parse arms into `TemporalPattern::Raw(Vec<Token>)` + body.

### 5) Tests (Parser Fixtures)
- Add new files in `tests/parser/`:
  - `6-control-flow.nx` (elif, if‑expr, match‑expr)
  - `7-range-slice-optchain.nx` (`..`, `by`, `[..]`, `?.`, `??`, `>>`)
  - `8-macro-igm-extension.nx`
  - `9-context-using-constexpr.nx`
  - `10-pointer-ops.nx` (->, ->?, +>, <+, <->, ~>, scope, defer)
  - `11-class-advanced.nx` (computed properties, deinit, static init, delegate)
  - `12-temporal-40-45.nx` (checkpoint/rewind/inspect/transaction/test/memory/batch)
- Ensure each new grammar form appears at least once.

## Test Scenarios
- Parse success for each fixture file.
- Parse failure tests for common syntax errors:
  - `if` without `else` in expression form.
  - malformed ranges (`1.. by 2`).
  - regex literal with missing closing `/`.
  - `checkpoint` missing block.

## Assumptions / Defaults
- This work targets parsing only, no semantics or evaluation.
- Advanced temporal patterns are preserved as raw token sequences (`TemporalPattern::Raw`) until a later semantic phase.
- Optional chaining uses `?.` exactly as in the syntax doc (not `.?`).
- Selector pipelines treat `>>` as pipeline (not bitshift) to align with the syntax doc.
