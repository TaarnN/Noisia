## ตอนนี้มี methods ใน `Parser` แล้วดังนี้:

### Helpers

1. `peek(&self) -> &Token`  

   **หน้าที่:** ส่งคืน Token ปัจจุบันโดยไม่เลื่อน pointer ของ parser (ใช้สำหรับตรวจสอบ Token ถัดไปโดยไม่ consume)  
   **ตัวอย่างการใช้งาน:** ใช้คู่กับ `match_tnv`/`expect_nv` เพื่อตรวจ `lexeme` ก่อนตัดสินใจ parse เส้นทางใดเส้นทางหนึ่ง  
   **เรียกใช้ methods อื่นๆ:** ไม่เรียก

2. `is_at_end(&self) -> bool`  

   **หน้าที่:** เช็คว่า token ปัจจุบันเป็น `EOF` แล้วหรือไม่  
   **ตัวอย่างการใช้งาน:** loop ใน `parse_program` เพื่อหยุดเมื่อจบไฟล์  
   **เรียกใช้ methods อื่นๆ:** `peek`

3. `advance(&mut self) -> &Token`  

   **หน้าที่:** consume token ปัจจุบัน แล้วเลื่อน `idx` ไปยัง token ถัดไป  
   **ตัวอย่างการใช้งาน:** consume keyword/สัญลักษณ์ที่ตรวจไว้แล้ว เช่น consume `(` ก่อน parse arguments  
   **เรียกใช้ methods อื่นๆ:** `is_at_end`, `peek`

4. `expect(&mut self, expected: TokenType) -> ParseResult<Token>`  

   **หน้าที่:** บังคับว่า token ปัจจุบันต้องมี `TokenType` ตรงตามที่คาดหวัง ถ้าตรงจะ consume และคืนค่า token ถ้าไม่ตรงจะคืน `ParseError::UnexpectedToken`  
   **ตัวอย่างการใช้งาน:** ใน `parse_block` บังคับ `{` และ `}` ด้วย `expect(TokenType::LeftBrace)?;`  
   **เรียกใช้ methods อื่นๆ:** `peek`, `advance`

5. `expect_nv(&mut self, expected: TokenType, v: &str) -> ParseResult<Token>`  

   **หน้าที่:** บังคับทั้ง `TokenType` และ `lexeme` ให้ตรงตามที่คาดหวัง ("nv" = type + value)  
   **ตัวอย่างการใช้งาน:** ในการบังคับ operator เฉพาะ เช่น `self.expect_nv(TokenType::Operator, ">")?;`  
   **เรียกใช้ methods อื่นๆ:** `peek`, `advance`

6. `match_one(&mut self, t: TokenType) -> bool`  

   **หน้าที่:** ถ้า token ปัจจุบันมี `TokenType` ตรงตามที่ระบุ จะ consume และคืน `true` ไม่เช่นนั้นคืน `false` (ไม่ consume)  
   **ตัวอย่างการใช้งาน:** optional `,` หรือ `;` (เช่นใน let/expr stmt)  
   **เรียกใช้ methods อื่นๆ:** `peek`, `advance`

7. `match_tnv(&mut self, t: TokenType, ch: &str) -> bool`  

   **หน้าที่:** คล้าย `match_one` แต่ match ทั้ง `TokenType` และ `lexeme`  
   **ตัวอย่างการใช้งาน:** ตรวจ `"async"` ก่อนเข้าสู่ `parse_function` หรือ match `"<"` เพื่อเริ่ม parse generics  
   **เรียกใช้ methods อื่นๆ:** `peek`, `advance`

---
### Top-level parsers

1. `parse_program(&mut self) -> ParseResult<Program>`  

   **หน้าที่:** Parse ทั้งไฟล์เป็น `Program { items }` โดยวน `parse_item` จน `EOF`  
   **ตัวอย่างการใช้งาน:** จุดเข้า parse หลัก (entry point) เพื่อสร้าง AST ของทั้งไฟล์  
   **เรียกใช้ methods อื่นๆ:** `parse_item`, `is_at_end`

2. `parse_item(&mut self) -> ParseResult<Item>`  

   **หน้าที่:** Parse top-level item จาก keyword/attribute ที่นำหน้า  
   **ตัวอย่างการใช้งาน:** เจอ `module` => `Item::ModuleDecl`, เจอ `fn`/`async`/`@attr` => `Item::Function`  
   **เรียกใช้ methods อื่นๆ:** `parse_module_path_string`, `parse_import`, `parse_function`, `parse_struct`, `parse_enum`, `parse_statement`

   **หมายเหตุ:** `Item::Trait` และ `Item::Impl` มีชนิดใน `ptypes` แล้ว แต่ตอนนี้ `parse_item` ยังไม่รองรับ keyword `trait`/`impl` (จะตกไปที่ error)

3. `parse_module_path_string(&mut self) -> ParseResult<String>`  

   **หน้าที่:** Parse module path เช่น `App::Utils::Math` โดยรับได้ทั้ง `TokenType::ModulePath` หรือประกอบจาก `Identifier` + `DoubleColon`  
   **ตัวอย่างการใช้งาน:** ใช้ใน `module` และ `import`  
   **เรียกใช้ methods อื่นๆ:** `peek`, `advance`, `expect`

4. `parse_import(&mut self) -> ParseResult<(String, Option<Vec<String>>)>`  

   **หน้าที่:** Parse `import <path>` และ optional symbol list `{a, b, ...}`  
   **ตัวอย่างการใช้งาน:** `import Core::{List, Option}` -> `(path="Core", symbols=Some(["List","Option"]))`  
   **เรียกใช้ methods อื่นๆ:** `parse_module_path_string`, `match_one`, `expect`

5. `parse_function(&mut self) -> ParseResult<FunctionDecl>`  

   **หน้าที่:** Parse function declaration ครบชุด: attributes, visibility, modifiers (`async`, multidispatch `fn+`), generics, params (รับ `Pattern`), return type (`->`), effects (`!Effect`), where clauses, body (`:>` expression หรือ `{...}`)  
   **ตัวอย่างการใช้งาน:**

   - `@inline public async fn+ map<T>(x: T) -> T !IO where T: Eq { ... }`

   **เรียกใช้ methods อื่นๆ:** `parse_generics`, `parse_effects`, `parse_where_clauses`, `parse_type`, `parse_pattern`, `parse_block`, `parse_expression`

6. `parse_struct(&mut self) -> ParseResult<StructDecl>`  

   **หน้าที่:** Parse `struct Name { field: Type, ... }` เป็น `StructDecl { name, fields }`  
   **ตัวอย่างการใช้งาน:** `struct Point { x: Float, y: Float }`  
   **เรียกใช้ methods อื่นๆ:** `parse_type`, `expect`, `match_one`

7. `parse_enum(&mut self) -> ParseResult<EnumDecl>`  

   **หน้าที่:** Parse `enum Name<T> { Variant, V(Type), V{f:Type} }` เป็น `EnumDecl` ที่มี `VariantKind` (Unit/Tuple/Struct)  
   **ตัวอย่างการใช้งาน:** `enum Result<T> { Ok(T), Err{msg: String} }`  
   **เรียกใช้ methods อื่นๆ:** `parse_generics`, `parse_type`, `expect`, `match_one`

8. `parse_block(&mut self) -> ParseResult<Block>`  

   **หน้าที่:** Parse `{ <stmt>* }` เป็น `Block { stmts }`  
   **ตัวอย่างการใช้งาน:** function body หรือ body ของ control-flow หลายชนิด  
   **เรียกใช้ methods อื่นๆ:** `parse_statement`, `expect`

---
### Sub-parsers (Declarations / Constraints / Type / Pattern)

1. `parse_generics(&mut self) -> ParseResult<Vec<GenericParam>>`  

   **หน้าที่:** Parse generic param list `<T: Bound + Bound, U>` (จะหยุดเมื่อเจอ `>` )  
   **ตัวอย่างการใช้งาน:** ใน `parse_function` และ `parse_enum`  
   **เรียกใช้ methods อื่นๆ:** `parse_type_constraints`, `expect`, `match_one`, `match_tnv`

2. `parse_effects(&mut self) -> ParseResult<Vec<Effect>>`  

   **หน้าที่:** Parse ชุดของ effects หลังจากเจอ `TokenType::EffectMarker` เช่น `!Network(String)!State`  
   **ตัวอย่างการใช้งาน:** ใน `parse_function` เพื่อได้ `Vec<Effect>`  
   **เรียกใช้ methods อื่นๆ:** `parse_effect_params`, `expect`, `match_one`

3. `parse_type_constraints(&mut self) -> ParseResult<Vec<TypeConstraint>>`  

   **หน้าที่:** Parse constraints หลัง `:` เช่น `T: Trait1 + Trait2` รวมรูปแบบพิเศษบางตัว (`TypeEq`, `SubtypeOf`, `LifetimeBound`)  
   **ตัวอย่างการใช้งาน:** generic param constraints และ where clause constraints  
   **เรียกใช้ methods อื่นๆ:** `parse_type`, `expect`, `match_tnv`, `match_one`

4. `parse_effect_params(&mut self) -> ParseResult<Vec<TypeRef>>`  

   **หน้าที่:** Parse parameter list ของ effect เช่น `(String, Int)`  
   **ตัวอย่างการใช้งาน:** `!Network(String)` จะได้ `params=[TypeRef{name="String"}]`  
   **เรียกใช้ methods อื่นๆ:** `parse_type`, `match_one`

5. `parse_where_clauses(&mut self) -> ParseResult<Vec<WhereClause>>`  

   **หน้าที่:** Parse `where T: Bound, U: Bound` จนก่อนเจอ `{` หรือ `:>`  
   **ตัวอย่างการใช้งาน:** ใน `parse_function`  
   **เรียกใช้ methods อื่นๆ:** `parse_type_constraints`, `expect`, `match_one`

6. `parse_type(&mut self) -> ParseResult<TypeRef>`  

   **หน้าที่:** Parse type reference ที่รองรับ:

   - pointer prefix: `~T` (Raw), `@T` (Managed), `&T` (Weak), `+T` (Shared)
   - generics: `Type<A,B>`
   - nullable suffix: `T?`

   **ตัวอย่างการใช้งาน:** `@Option<Int>?`  
   **เรียกใช้ methods อื่นๆ:** `expect_nv`, `match_tnv`, `advance`

7. `parse_pattern(&mut self) -> ParseResult<Pattern>`  

   **หน้าที่:** Parse pattern สำหรับ `match` และ destructuring ใน `let`/params โดยรองรับ:

   - tuple `(p1, p2)` / array `[p1, p2]`
   - struct pattern `{a, b: p}` และ `Name { ... }`
   - wildcard `_`, literal pattern, `Nil`, `Some(p)`, `Ok(p)`, `Err(p)`
   - enum variant: `Variant`, `Variant(p)`, `Variant{...}`
   - or-pattern: `p1 | p2 | p3`

   **ตัวอย่างการใช้งาน:** `Point { x, y: z } | None` จะสะสม `bindings=["x","z"]`  
   **เรียกใช้ methods อื่นๆ:** `parse_primary`, `expect`, `match_one`, `match_tnv`

8. `parse_lambda(&mut self) -> ParseResult<Expr>`  

   **หน้าที่:** Parse lambda literal รูปแบบ `\param1, param2 :> <expr>` ให้เป็น `Expr::Lambda`  
   **ตัวอย่างการใช้งาน:** `\\x, y :> x + y`  
   **เรียกใช้ methods อื่นๆ:** `expect`, `match_one`, `parse_expression`

---
### Statements

1. `parse_statement(&mut self) -> ParseResult<Stmt>`  

   **หน้าที่:** Router หลักของ statement โดยดู `lexeme` เพื่อเลือกชนิด statement  
   **ตัวอย่างการใช้งาน:** รองรับ `let`, `try/catch`, `panic unless`, `return`, `while`, `for`, `loop`, `match` (2 โหมด), `if`, `watch`, `converge`, `within`, `atomically`, `trap`, `guard`, `poll`, `delete`, `joins` และ fallback เป็น expression statement  
   **เรียกใช้ methods อื่นๆ:** `parse_let_stmt`, `parse_try_catch_stmt`, `parse_in_context_stmt`, `parse_on_sequence_stmt`, `parse_expression`, `parse_block`, `parse_block_or_expr_body`, `parse_pattern`

2. `parse_let_stmt(&mut self) -> ParseResult<Stmt>`  

   **หน้าที่:** Parse `let` statement รองรับตัวแปร mutable แบบ `let~` (ตรวจ `~` เป็น operator) รองรับ type annotation และ initializer  
   **ตัวอย่างการใช้งาน:** `let~ (x, y): Pair<Int,Int> = f();`  
   **เรียกใช้ methods อื่นๆ:** `parse_pattern`, `parse_type`, `parse_expression`, `match_tnv`, `match_one`

3. `parse_try_catch_stmt(&mut self) -> ParseResult<Stmt>`  

   **หน้าที่:** Parse `try { ... } catch (e) { ... }` หรือ `catch e { ... }` เป็น `Stmt::TryCatch`  
   **ตัวอย่างการใช้งาน:** `try { f() } catch (err) { g(err) }`  
   **เรียกใช้ methods อื่นๆ:** `parse_block`, `expect_nv`, `expect`, `match_one`

4. `parse_in_context_stmt(&mut self) -> ParseResult<Stmt>`  

   **หน้าที่:** Parse statement พิเศษ `in context <target> { <value :> body>* }` เป็น `Stmt::InContext` โดยมี `ContextArm`  
   **ตัวอย่างการใช้งาน:** `in context state { Ready :> { ... } Busy :> { ... } }`  
   **เรียกใช้ methods อื่นๆ:** `parse_expr_no_struct_literal_before_block`, `parse_expression`, `parse_block_or_expr_body`, `expect`, `expect_nv`

5. `parse_on_sequence_stmt(&mut self) -> ParseResult<Stmt>`  

   **หน้าที่:** Parse `on sequence <target> = <sequence> :> <body>` เป็น `Stmt::OnSequence`  
   **ตัวอย่างการใช้งาน:** `on sequence x = [1,2,3] :> { ... }`  
   **เรียกใช้ methods อื่นๆ:** `parse_expression`, `parse_block_or_expr_body`, `expect_nv`, `expect`

---
### Expressions

1. `parse_expression(&mut self, min_prec: u8) -> ParseResult<Expr>`  

   **หน้าที่:** Pratt/precedence parser สำหรับ binary ops โดยอ้างอิงตาราง `op_precedence` และมี special-case:

   - `?` -> `Expr::Try(lhs)` (postfix try)
   - `|>` -> `Expr::Pipeline { left, right }`

   **ตัวอย่างการใช้งาน:** `a |> f(1)` -> `Pipeline(a, Call(f,[1]))`  
   **เรียกใช้ methods อื่นๆ:** `parse_unary_or_primary`, `op_precedence`

2. `parse_unary_or_primary(&mut self) -> ParseResult<Expr>`  

   **หน้าที่:** Parse unary layer และ keyword unary:

   - `await <expr>` -> `Expr::Await`
   - `spawn <expr>` -> `Expr::Spawn`
   - `-`, `+`, `!`, `not` -> `Expr::Unary`

   แล้วตามด้วย `parse_postfix` เพื่อรองรับ call/field/index/generics หลัง primary  
   **เรียกใช้ methods อื่นๆ:** `parse_primary`, `parse_postfix`, `parse_expression`

3. `parse_primary(&mut self) -> ParseResult<Expr>`  

   **หน้าที่:** Parse primary:

   - literals: int/float/string/bool/unit
   - grouping/tuple: `(expr)` หรือ `(a,b,)`
   - array literal: `[a,b]`
   - vector literal: `v[ ... ]`
   - struct literal: `Name { field: expr, ... }`
   - lambda literal: `\\x :> x`

   **เรียกใช้ methods อื่นๆ:** `parse_expression`, `parse_lambda`, `expect`, `match_one`

4. `parse_postfix(&mut self, left: Expr) -> ParseResult<Expr>`  

   **หน้าที่:** Parse postfix บน expression:

   - `.` field access / method call (รองรับ generics หลัง method name: `.m<T>(...)`)
   - `[idx]` index
   - `<T,...>(...)` generic function call (มีการ disambiguate กับ operator `<` เปรียบเทียบ)
   - `(args)` call
   - `with { ... }` record update (แปลงเป็น `Literal::Struct` โดยใช้ `base=Some(expr)`)

   **เรียกใช้ methods อื่นๆ:** `parse_arguments`, `parse_expression`, `parse_type`, `expect`, `expect_nv`, `match_one`

5. `parse_arguments(&mut self) -> ParseResult<Vec<Expr>>`  

   **หน้าที่:** Parse argument list ภายใน `(...)`  
   **ตัวอย่างการใช้งาน:** `(1, "a")` -> `[Literal(1), Literal("a")]`  
   **เรียกใช้ methods อื่นๆ:** `parse_expression`, `match_one`

---
### Utilities (นอก `impl Parser`)

1. `op_precedence(tok: &Token) -> Option<(u8, bool)>`  

   **หน้าที่:** ตาราง precedence สำหรับ operator โดยคืน `(precedence, right_associative)` เพื่อใช้ใน `parse_expression`  
   **ตัวอย่างการใช้งาน:** `"*"` precedence สูงกว่า `"+"`, และ assignment เป็น right-assoc precedence ต่ำสุด  
   **เรียกใช้ methods อื่นๆ:** -

---
## ข้อมูลทั้งหมดของ `ptypes` (AST / Parse Types)

### 1) Error model

1. `ParseError`  

   **หน้าที่:** เป็น error type หลักของ parser  
   **รูปแบบ:**

   - `UnexpectedToken { expected: String, found: Token, idx: usize }` (เจอ token ไม่ตรงที่คาด)
   - `EOF { message: String }` (จบไฟล์ก่อนโครงสร้างครบ)
   - `Generic(String)` (error ทั่วไป)

2. `ParseResult<T> = Result<T, ParseError>`  

   **หน้าที่:** ชนิดผลลัพธ์มาตรฐานของทุกฟังก์ชัน parse

---
### 2) โครงสร้างระดับโปรแกรม (Top-level AST)

1. `Program`  

   **ฟิลด์:** `items: Vec<Item>`  
   **ความหมาย:** AST ของทั้งไฟล์/โมดูล

2. `Item`  

   **หน้าที่:** top-level node ของโปรแกรม  
   **variants:**

   - `ModuleDecl(ModuleDecl)`
   - `Import(ImportDecl)`
   - `Function(FunctionDecl)`
   - `Struct(StructDecl)`
   - `Enum(EnumDecl)`
   - `Trait(TraitDecl)`
   - `Impl(ImplDecl)`
   - `Class(ClassDecl)`

   **หมายเหตุ:** ตอนนี้ parser สร้างได้จริง `ModuleDecl/Import/Function/Struct/Enum/Trait/Impl/Class`

3. `ModuleDecl`  

   **ฟิลด์:**

   - `attributes: Vec<String>`
   - `path: String` (เช่น `"App::Hello"`)

4. `ImportDecl`  

   **ฟิลด์:**

   - `attributes: Vec<String>`
   - `path: String`
   - `symbols: Option<Vec<String>>` (รองรับ `import A::{x,y}`)

---
### 3) Function / Generics / Effect / Visibility

1. `FunctionDecl`  

   **ฟิลด์:**

   - `attributes: Vec<String>` (token `@attr`)
   - `visibility: Visibility`
   - `modifiers: Vec<FunctionModifier>`
   - `name: String`
   - `is_async: bool` (flag แยกไว้ใช้งานง่าย)
   - `generics: Vec<GenericParam>`
   - `params: Vec<Param>`
   - `ret_type: Option<TypeRef>`
   - `effects: Vec<Effect>`
   - `where_clauses: Vec<WhereClause>`
   - `body: Option<Block>` (None = declaration ไม่มี body)

2. `Visibility`  

   **variants:** `Public | Private | Protected | Internal | Package`

3. `FunctionModifier`  

   **variants:**

   - `Async`
   - `Multidispatch`

4. `Param`  

   **ฟิลด์:**

   - `pattern: Pattern` (ทำให้ param รองรับ destructuring)
   - `typ: Option<TypeRef>`
   - `default: Option<Expr>`

5. `GenericParam`  

   **ฟิลด์:**

   - `name: String`
   - `constraints: Vec<TypeConstraint>`

6. `TypeConstraint`  

   **variants:**

   - `TraitBound(String)`
   - `LifetimeBound(String)`
   - `TypeEq(String, String)`
   - `SubtypeOf(String)`
   - `SupertypeOf(String)`

7. `WhereClause`  

   **ฟิลด์:**

   - `param_name: String`
   - `constraints: Vec<TypeConstraint>`

8. `Effect`  

   **ฟิลด์:**

   - `name: String`
   - `params: Option<Vec<TypeRef>>`

---
### 4) Struct / Enum / Trait / Impl

1. `StructDecl`  

   **ฟิลด์:**

   - `attributes: Vec<String>`
   - `name: String`
   - `fields: Vec<(String, TypeRef)>`

2. `EnumDecl`  

   **ฟิลด์:**

   - `attributes: Vec<String>`
   - `name: String`
   - `generics: Vec<GenericParam>`
   - `variants: Vec<EnumVariant>`

3. `EnumVariant`  

   **ฟิลด์:**

   - `name: String`
   - `kind: VariantKind`

4. `VariantKind`  

   **variants:**

   - `Unit`
   - `Tuple(Vec<EnumVariantField>)`
   - `Struct(Vec<EnumVariantField>)`

5. `EnumVariantField`  

   **ฟิลด์:**

   - `name: Option<String>` (named field สำหรับ tuple-variant แบบ `x: T`)
   - `typ: TypeRef`

6. `TraitDecl`  

   **ฟิลด์:**

   - `attributes: Vec<String>`
   - `name: String`
   - `methods: Vec<FunctionDecl>`

7. `ImplDecl`  

   **ฟิลด์:**

   - `attributes: Vec<String>`
   - `trait_name: Option<String>` (None = inherent impl, Some("Trait") = trait impl)
   - `target: TypeRef` (ชนิดเป้าหมายที่ impl ให้ เช่น `Point` หรือ `Module::Point`)
   - `methods: Vec<FunctionDecl>`

8. `ClassDecl`  

   **ฟิลด์:**

   - `attributes: Vec<String>`
   - `name: String`
   - `extends: Vec<TypeRef>`
   - `mixins: Vec<TypeRef>`
   - `implements: Vec<TypeRef>`
   - `fields: Vec<ClassFieldDecl>`
   - `ctors: Vec<ConstructorDecl>`
   - `methods: Vec<FunctionDecl>`

9. `ClassFieldDecl`, `ConstructorDecl`  

   **ฟิลด์ (สำคัญ):**

   - `ClassFieldDecl`:
     - `attributes: Vec<String>`
     - `vis: Visibility`
     - `mutable: bool`
     - `name: String`
     - `typ: TypeRef`
     - `value: Option<Expr>`
   - `ConstructorDecl`:
     - `attributes: Vec<String>`
     - `visibility: Visibility`
     - `name: Option<String>`
     - `params: Vec<Param>`
     - `body: Block`

---
### 5) Statements / Blocks

1. `Block`  

   **ฟิลด์:** `stmts: Vec<Stmt>`

2. `Stmt`  

   **variants (พร้อมฟิลด์สำคัญ):**

   - `Let { pattern: Pattern, mutable: bool, typ: Option<TypeRef>, expr: Option<Expr> }`
   - `Expr(Expr)`
   - `Return(Option<Expr>)`
   - `Continue`, `Break`
   - `TryCatch { try_block: Block, catch_name: String, catch_block: Block }`
   - `If { cond: Expr, then_block: Block, else_block: Option<Block> }`
   - `While { cond: Expr, body: Block }`
   - `For { pat: String, iter: Expr, body: Block }`
   - `Loop { body: Block }`
   - `Match { expr: Expr, arms: Vec<MatchArm> }`
   - `Watch { variables: Vec<Expr>, clauses: Vec<WatchClause> }`
   - `Converge { history: bool, variable: String, body: Block, until: Expr }`
   - `InContext { target: Expr, arms: Vec<ContextArm> }`
   - `Within { time: Expr, condition: Option<Expr>, body: Block }`
   - `Atomically { body: Block }`
   - `Trap { error_condition: Expr, body: Block }`
   - `Guard { condition: Expr, then_block: Block, else_block: Option<Block> }`
   - `Poll { signal: Expr, interval: Expr, body: Block }`
   - `Delete { expr: Expr }`
   - `Joins { decls: Vec<Stmt>, body: Block }`
   - `MatchCond { arms: Vec<MatchCondArm> }` (match แบบเงื่อนไขล้วน)
   - `OnSequence { target: Expr, sequence: Expr, body: Block }`
   - `PanicUnless { condition: Expr }`

3. `MatchArm`  

   **ฟิลด์:**

   - `pattern: Pattern`
   - `guard: Option<Expr>`
   - `body: Expr`

4. `WatchClause` / `ContextArm` / `MatchCondArm`  

   **ฟิลด์:**

   - `WatchClause { condition: Expr, body: Block }`
   - `ContextArm { value: Expr, body: Block }`
   - `MatchCondArm { condition: Expr, body: Block }`

---
### 6) Patterns

1. `Pattern`  

   **ฟิลด์:**

   - `kind: PatternKind`
   - `bindings: Vec<String>` (รวบรวมชื่อ identifier ที่ถูก bind ใน pattern เพื่อใช้ใน semantic pass ต่อไป)

2. `PatternKind`  

   **variants:**

   - `Identifier(String)`
   - `Wildcard`
   - `Literal(Literal)`
   - `Struct { name: Option<String>, fields: Vec<(String, Pattern)> }`
   - `Tuple(Vec<Pattern>)`
   - `Or(Vec<Pattern>)`
   - `SomeVariant(Box<Pattern>)`, `NoneVariant`
   - `OkVariant(Box<Pattern>)`, `ErrVariant(Box<Pattern>)`
   - `EnumVariant { variant_name: String, inner_pattern: Option<Box<Pattern>> }`
   - `Array(Vec<Pattern>)`
   - `Nil`

---
### 7) Expressions / Literals

1. `Expr`  

   **variants (ย่อ):**

   - `Literal(Literal)`
   - `Ident(String)`
   - `Await(Box<Expr>)`, `Spawn(Box<Expr>)`, `Try(Box<Expr>)`
   - `Binary { left: Box<Expr>, op: String, right: Box<Expr> }`
   - `Unary { op: String, rhs: Box<Expr> }`
   - `Grouping(Box<Expr>)`
   - `Call { callee: Box<Expr>, generics: Vec<TypeRef>, args: Vec<Expr> }`
   - `MethodCall { object: Box<Expr>, method: String, generics: Vec<TypeRef>, args: Vec<Expr> }`
   - `FieldAccess { object: Box<Expr>, field: String }`
   - `Index { array: Box<Expr>, index: Box<Expr> }`
   - `Lambda { params: Vec<Param>, body: Box<Expr> }`
   - `Pipeline { left: Box<Expr>, right: Box<Expr> }`

2. `Literal`  

   **variants:**

   - `Int(String)`, `Float(String)`, `String(String)`, `Bool(bool)`
   - `Unit { v: String, u: String }` (ตัวเลข+หน่วย เช่น `"10ms"` -> `v="10"`, `u="ms"`)
   - `Array(Vec<Expr>)`, `Vector(Vec<Expr>)`
   - `Struct { name: String, base: Option<Box<Expr>>, fields: Vec<(String, Expr)> }`
   - `Tuple(Vec<Expr>)`

---
### 8) Type system node

1. `TypeRef`  

   **ฟิลด์:**

   - `name: String`
   - `generics: Vec<TypeRef>`
   - `nullable: bool`
   - `pointer_type: Option<PointerType>`

2. `PointerType`  

   **variants:** `RawPointer | ManagedPointer | WeakPointer | SharedPointer`
