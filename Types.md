# Types: คู่มือ Type System (`tokenizer.rs` + `ptypes.rs`) แบบไทยง่ายมาก + ละเอียด

## Scope และแหล่งอ้างอิง
- ไฟล์หลัก:
  - `/Users/taarn/Documents/PGM/Prog-Languages/Noisia/src/tokenizer.rs`
  - `/Users/taarn/Documents/PGM/Prog-Languages/Noisia/src/ptypes.rs`
- ไฟล์บริบทการใช้งาน:
  - `/Users/taarn/Documents/PGM/Prog-Languages/Noisia/src/parser.rs`
  - `/Users/taarn/Documents/PGM/Prog-Languages/Noisia/src/main.rs`
  - `/Users/taarn/Documents/PGM/Prog-Languages/Noisia/Noisia_Syntax.md`
- ขอบเขต: ครอบคลุมทุก type ทั้ง `pub` และ `private` รวม `type alias`

## Big Picture
`Token layer -> Parse error/result layer -> AST declarations -> Statement/Expression layer -> Supporting types`

1. `tokenizer.rs` นิยาม token model และ state ภายใน tokenizer
2. `ptypes.rs` นิยาม parse result + AST model เกือบทั้งหมดของภาษา
3. `parser.rs` ใช้ type เหล่านี้สร้าง AST จาก token stream
4. `main.rs` ใช้ type เหล่านี้ใน flow รันจริง (tokenize -> parse -> print)

## ผลลัพธ์ 3 รอบ (Quality Gates)
### รอบ 1: Inventory + Skeleton
- พบ type ทั้งหมด `54` ตัว (tokenizer=4, ptypes=50)
- สร้าง inventory table ครบทุก type พร้อม line/file/visibility/kind/category
- สร้างหัวข้อรายละเอียดครบทุก type

### รอบ 2: เติมเนื้อหาเต็ม
- ทุก type ใช้ template เดียวกันและแตก members ครบ
- enum แตก variant พร้อม payload ครบตาม source
- struct แตก field พร้อมชนิดและคำอธิบาย

### รอบ 3: QA เข้ม
- ตรวจ coverage source vs doc แบบอัตโนมัติ
- ตรวจความสอดคล้อง line/name/visibility/kind
- ตรวจภาษาให้อ่านง่ายและศัพท์คงที่

## Type Inventory (ครบทุก type)
| # | line | file | visibility | kind | name | category |
|---:|---:|---|---|---|---|---|
| 1 | 11 | `ptypes.rs` | `pub` | `enum` | `ParseError` | Error/Result |
| 2 | 105 | `ptypes.rs` | `pub` | `type` | `ParseResult<T>` | Core Utility |
| 3 | 109 | `ptypes.rs` | `pub` | `struct` | `Program` | Top-level AST |
| 4 | 115 | `ptypes.rs` | `pub` | `enum` | `Item` | Top-level AST |
| 5 | 136 | `ptypes.rs` | `pub` | `struct` | `ModuleDecl` | Top-level AST |
| 6 | 145 | `ptypes.rs` | `pub` | `struct` | `ImportDecl` | Top-level AST |
| 7 | 156 | `ptypes.rs` | `pub` | `struct` | `UsingDecl` | Top-level AST |
| 8 | 165 | `ptypes.rs` | `pub` | `struct` | `MacroDecl` | Top-level AST |
| 9 | 178 | `ptypes.rs` | `pub` | `struct` | `ExtensionDecl` | Top-level AST |
| 10 | 191 | `ptypes.rs` | `pub` | `struct` | `IGMDecl` | Top-level AST |
| 11 | 206 | `ptypes.rs` | `pub` | `struct` | `PluginDecl` | Top-level AST |
| 12 | 217 | `ptypes.rs` | `pub` | `struct` | `MixinDecl` | Top-level AST |
| 13 | 234 | `ptypes.rs` | `pub` | `struct` | `InterfaceDecl` | Top-level AST |
| 14 | 247 | `ptypes.rs` | `pub` | `struct` | `ProtocolDecl` | Top-level AST |
| 15 | 264 | `ptypes.rs` | `pub` | `struct` | `FunctionDecl` | Function/Type System |
| 16 | 295 | `ptypes.rs` | `pub` | `struct` | `GenericParam` | Function/Type System |
| 17 | 304 | `ptypes.rs` | `pub` | `enum` | `TypeConstraint` | Function/Type System |
| 18 | 314 | `ptypes.rs` | `pub` | `struct` | `WhereClause` | Function/Type System |
| 19 | 323 | `ptypes.rs` | `pub` | `struct` | `Effect` | Function/Type System |
| 20 | 332 | `ptypes.rs` | `pub` | `enum` | `Visibility` | Function/Type System |
| 21 | 342 | `ptypes.rs` | `pub` | `enum` | `FunctionModifier` | Function/Type System |
| 22 | 352 | `ptypes.rs` | `pub` | `struct` | `Param` | Function/Type System |
| 23 | 363 | `ptypes.rs` | `pub` | `struct` | `StructDecl` | Top-level AST |
| 24 | 374 | `ptypes.rs` | `pub` | `struct` | `EnumDecl` | Top-level AST |
| 25 | 387 | `ptypes.rs` | `pub` | `struct` | `EnumVariantField` | Top-level AST |
| 26 | 396 | `ptypes.rs` | `pub` | `enum` | `VariantKind` | Top-level AST |
| 27 | 404 | `ptypes.rs` | `pub` | `struct` | `EnumVariant` | Top-level AST |
| 28 | 411 | `ptypes.rs` | `pub` | `struct` | `TraitDecl` | Top-level AST |
| 29 | 422 | `ptypes.rs` | `pub` | `struct` | `ImplDecl` | Top-level AST |
| 30 | 435 | `ptypes.rs` | `pub` | `struct` | `ClassDecl` | Class/Object |
| 31 | 466 | `ptypes.rs` | `pub` | `struct` | `ClassFieldDecl` | Class/Object |
| 32 | 483 | `ptypes.rs` | `pub` | `struct` | `ClassPropertyDecl` | Class/Object |
| 33 | 504 | `ptypes.rs` | `pub` | `struct` | `ClassDelegateDecl` | Class/Object |
| 34 | 517 | `ptypes.rs` | `pub` | `struct` | `ConstructorDecl` | Class/Object |
| 35 | 534 | `ptypes.rs` | `pub` | `struct` | `Block` | Statement/Temporal |
| 36 | 540 | `ptypes.rs` | `pub` | `enum` | `Stmt` | Statement/Temporal |
| 37 | 759 | `ptypes.rs` | `pub` | `enum` | `TemporalPattern` | Statement/Temporal |
| 38 | 772 | `ptypes.rs` | `pub` | `enum` | `TemporalClause` | Statement/Temporal |
| 39 | 785 | `ptypes.rs` | `pub` | `struct` | `MatchArm` | Pattern/Match |
| 40 | 793 | `ptypes.rs` | `pub` | `struct` | `Pattern` | Pattern/Match |
| 41 | 802 | `ptypes.rs` | `pub` | `enum` | `PatternKind` | Pattern/Match |
| 42 | 827 | `ptypes.rs` | `pub` | `struct` | `WatchClause` | Statement/Temporal |
| 43 | 834 | `ptypes.rs` | `pub` | `struct` | `ContextArm` | Statement/Temporal |
| 44 | 841 | `ptypes.rs` | `pub` | `struct` | `MatchCondArm` | Statement/Temporal |
| 45 | 848 | `ptypes.rs` | `pub` | `enum` | `Expr` | Expression/Literal |
| 46 | 982 | `ptypes.rs` | `pub` | `enum` | `InterpolatedPartKind` | Expression/Literal |
| 47 | 989 | `ptypes.rs` | `pub` | `struct` | `InterpolatedPart` | Expression/Literal |
| 48 | 995 | `ptypes.rs` | `pub` | `enum` | `Literal` | Expression/Literal |
| 49 | 1017 | `ptypes.rs` | `pub` | `struct` | `TypeRef` | Function/Type System |
| 50 | 1030 | `ptypes.rs` | `pub` | `enum` | `PointerType` | Function/Type System |
| 51 | 7 | `tokenizer.rs` | `pub` | `enum` | `TokenType` | Tokenizer Types |
| 52 | 74 | `tokenizer.rs` | `pub` | `struct` | `Token` | Tokenizer Types |
| 53 | 180 | `tokenizer.rs` | `private` | `enum` | `InterpolatedState` | Tokenizer Types |
| 54 | 185 | `tokenizer.rs` | `pub` | `struct` | `Tokenizer` | Tokenizer Types |

## Category Map
- Tokenizer Types: `TokenType`, `Token`, `InterpolatedState`, `Tokenizer`
- Error/Result: `ParseError`
- Top-level AST: `Program`, `Item`, `ModuleDecl`, `ImportDecl`, `UsingDecl`, `MacroDecl`, `ExtensionDecl`, `IGMDecl`, `PluginDecl`, `MixinDecl`, `InterfaceDecl`, `ProtocolDecl`, `StructDecl`, `EnumDecl`, `EnumVariantField`, `VariantKind`, `EnumVariant`, `TraitDecl`, `ImplDecl`
- Function/Type System: `FunctionDecl`, `GenericParam`, `TypeConstraint`, `WhereClause`, `Effect`, `Visibility`, `FunctionModifier`, `Param`, `TypeRef`, `PointerType`
- Class/Object: `ClassDecl`, `ClassFieldDecl`, `ClassPropertyDecl`, `ClassDelegateDecl`, `ConstructorDecl`
- Statement/Temporal: `Block`, `Stmt`, `TemporalPattern`, `TemporalClause`, `WatchClause`, `ContextArm`, `MatchCondArm`
- Pattern/Match: `MatchArm`, `Pattern`, `PatternKind`
- Expression/Literal: `Expr`, `InterpolatedPartKind`, `InterpolatedPart`, `Literal`
- Core Utility: `ParseResult<T>`

## Type Details (Template เดียวกันทุก type)

### T001 `ParseError`
- ชื่อ type: `ParseError`
- ไฟล์+บรรทัด: `ptypes.rs:11`
- ชนิด/visibility: `enum` / `pub`
- นิยามย่อ: ชุดตัวเลือก (variants) สำหรับ `ParseError`
- หน้าที่: ใช้แทนผลสำเร็จ/ล้มเหลวระหว่าง parse
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `UnexpectedToken { expected: String, found: Token, idx: usize }` (struct variant)
  - `EOF { message: String, line: usize, column: usize }` (struct variant)
  - `Generic { message: String, line: usize, column: usize }` (struct variant)
- ความหมายของแต่ละ field/variant:
  - `UnexpectedToken`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `EOF`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Generic`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
- เชื่อมกับ type อื่นอย่างไร:
  - variants ของ `ParseError` พ่วง type อื่น เช่น `String, Token, usize`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 19 จุด, `ptypes.rs` 8 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:205`, `parser.rs:214`, `ptypes.rs:11`, `ptypes.rs:29`
- ตัวอย่างสั้น:
```rust
fn parse_x() -> ParseResult<Expr> {
    // ...
}
```
- ข้อควรระวัง: เวลา match enum ต้องระวัง variant ที่มี payload และ variant ที่ไม่มี payload ให้ถูกรูปแบบ

### T002 `ParseResult<T>`
- ชื่อ type: `ParseResult<T>`
- ไฟล์+บรรทัด: `ptypes.rs:105`
- ชนิด/visibility: `type` / `pub`
- นิยามย่อ: alias สำหรับลดการเขียนซ้ำ และคุมรูปแบบผลลัพธ์
- หน้าที่: แทน utility type ที่ใช้ข้ามชั้นงาน
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `ParseResult<T> = Result<T, ParseError>`
- ความหมายของแต่ละ field/variant:
  - alias นี้ช่วยให้ signature อ่านง่ายและมาตรฐานเดียวทั้งระบบ
- เชื่อมกับ type อื่นอย่างไร:
  - alias ชี้ไปที่ `Result<T, ParseError>` โดยตรง
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 106 จุด, `ptypes.rs` 1 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:200`, `parser.rs:224`, `ptypes.rs:105`
- ตัวอย่างสั้น:
```rust
type ParseResult<T> = Result<T, ParseError>;
```
- ข้อควรระวัง: alias นี้มีผลต่อรูปแบบ error flow โดยรวม; การเปลี่ยน RHS จะกระทบฟังก์ชันจำนวนมาก

### T003 `Program`
- ชื่อ type: `Program`
- ไฟล์+บรรทัด: `ptypes.rs:109`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `Program`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `items: Vec<Item>`
- ความหมายของแต่ละ field/variant:
  - `items`: ข้อมูล `items` ของ `Program`
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `Program` อ้าง type อื่น เช่น `Vec<Item>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 2 จุด, `ptypes.rs` 1 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:488`, `parser.rs:494`, `ptypes.rs:109`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T004 `Item`
- ชื่อ type: `Item`
- ไฟล์+บรรทัด: `ptypes.rs:115`
- ชนิด/visibility: `enum` / `pub`
- นิยามย่อ: ชุดตัวเลือก (variants) สำหรับ `Item`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `ModuleDecl(ModuleDecl)` (tuple variant)
  - `Import(ImportDecl)` (tuple variant)
  - `Using(UsingDecl)` (tuple variant)
  - `MacroDecl(MacroDecl)` (tuple variant)
  - `ExtensionDecl(ExtensionDecl)` (tuple variant)
  - `IGMDecl(IGMDecl)` (tuple variant)
  - `PluginDecl(PluginDecl)` (tuple variant)
  - `Function(FunctionDecl)` (tuple variant)
  - `Struct(StructDecl)` (tuple variant)
  - `Enum(EnumDecl)` (tuple variant)
  - `Trait(TraitDecl)` (tuple variant)
  - `Impl(ImplDecl)` (tuple variant)
  - `MixinDecl(MixinDecl)` (tuple variant)
  - `InterfaceDecl(InterfaceDecl)` (tuple variant)
  - `ProtocolDecl(ProtocolDecl)` (tuple variant)
  - `Class(ClassDecl)` (tuple variant)
- ความหมายของแต่ละ field/variant:
  - `ModuleDecl`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Import`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Using`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `MacroDecl`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `ExtensionDecl`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `IGMDecl`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `PluginDecl`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Function`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Struct`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Enum`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Trait`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Impl`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `MixinDecl`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `InterfaceDecl`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `ProtocolDecl`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Class`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
- เชื่อมกับ type อื่นอย่างไร:
  - variants ของ `Item` พ่วง type อื่น เช่น `ModuleDecl, ImportDecl, UsingDecl, MacroDecl, ExtensionDecl, IGMDecl, PluginDecl, FunctionDecl`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 51 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:11`, `parser.rs:15`, `ptypes.rs:110`, `ptypes.rs:115`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: เวลา match enum ต้องระวัง variant ที่มี payload และ variant ที่ไม่มี payload ให้ถูกรูปแบบ

### T005 `ModuleDecl`
- ชื่อ type: `ModuleDecl`
- ไฟล์+บรรทัด: `ptypes.rs:136`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `ModuleDecl`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `path: String`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `path`: module path like A::B
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `ModuleDecl` อ้าง type อื่น เช่น `Vec<String>, String`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 3 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:15`, `parser.rs:38`, `ptypes.rs:116`, `ptypes.rs:136`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T006 `ImportDecl`
- ชื่อ type: `ImportDecl`
- ไฟล์+บรรทัด: `ptypes.rs:145`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `ImportDecl`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `path: String`
  - `symbols: Option<Vec<String>>`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `path`: module path like A::B
  - `symbols`: optional symbols list
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `ImportDecl` อ้าง type อื่น เช่น `Vec<String>, String, Option<Vec<String>>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 1 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:542`, `ptypes.rs:117`, `ptypes.rs:145`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T007 `UsingDecl`
- ชื่อ type: `UsingDecl`
- ไฟล์+บรรทัด: `ptypes.rs:156`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `UsingDecl`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `target: Expr`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `target`: target expression
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `UsingDecl` อ้าง type อื่น เช่น `Vec<String>, Expr`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 1 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:551`, `ptypes.rs:118`, `ptypes.rs:156`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T008 `MacroDecl`
- ชื่อ type: `MacroDecl`
- ไฟล์+บรรทัด: `ptypes.rs:165`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `MacroDecl`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `name: String`
  - `params: Vec<String>`
  - `body: Block`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `name`: macro name
  - `params`: macro params as raw identifiers
  - `body`: macro body
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `MacroDecl` อ้าง type อื่น เช่น `Vec<String>, String, Block`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 5 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:18`, `parser.rs:41`, `ptypes.rs:119`, `ptypes.rs:165`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T009 `ExtensionDecl`
- ชื่อ type: `ExtensionDecl`
- ไฟล์+บรรทัด: `ptypes.rs:178`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `ExtensionDecl`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `receiver: Option<String>`
  - `target: TypeRef`
  - `methods: Vec<FunctionDecl>`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `receiver`: optional receiver binding name
  - `target`: target type for extension
  - `methods`: method list
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `ExtensionDecl` อ้าง type อื่น เช่น `Vec<String>, Option<String>, TypeRef, Vec<FunctionDecl>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 5 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:19`, `parser.rs:42`, `ptypes.rs:120`, `ptypes.rs:178`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T010 `IGMDecl`
- ชื่อ type: `IGMDecl`
- ไฟล์+บรรทัด: `ptypes.rs:191`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `IGMDecl`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `name: String`
  - `pattern: String`
  - `expand_params: Vec<Param>`
  - `expand: Option<Expr>`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `name`: macro name
  - `pattern`: regex pattern string
  - `expand_params`: expand params
  - `expand`: expand expression
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `IGMDecl` อ้าง type อื่น เช่น `Vec<String>, String, Vec<Param>, Option<Expr>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 5 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:20`, `parser.rs:43`, `ptypes.rs:121`, `ptypes.rs:191`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T011 `PluginDecl`
- ชื่อ type: `PluginDecl`
- ไฟล์+บรรทัด: `ptypes.rs:206`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `PluginDecl`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `name: String`
  - `condition: Option<Expr>`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `name`: plugin name
  - `condition`: optional activation condition
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `PluginDecl` อ้าง type อื่น เช่น `Vec<String>, String, Option<Expr>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 5 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:21`, `parser.rs:44`, `ptypes.rs:122`, `ptypes.rs:206`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T012 `MixinDecl`
- ชื่อ type: `MixinDecl`
- ไฟล์+บรรทัด: `ptypes.rs:217`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `MixinDecl`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `name: String`
  - `fields: Vec<ClassFieldDecl>`
  - `properties: Vec<ClassPropertyDecl>`
  - `delegates: Vec<ClassDelegateDecl>`
  - `methods: Vec<FunctionDecl>`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `name`: mixin name
  - `fields`: field list
  - `properties`: property list
  - `delegates`: delegate list
  - `methods`: method list
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `MixinDecl` อ้าง type อื่น เช่น `Vec<String>, String, Vec<ClassFieldDecl>, Vec<ClassPropertyDecl>, Vec<ClassDelegateDecl>, Vec<FunctionDecl>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 5 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:27`, `parser.rs:50`, `ptypes.rs:128`, `ptypes.rs:217`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T013 `InterfaceDecl`
- ชื่อ type: `InterfaceDecl`
- ไฟล์+บรรทัด: `ptypes.rs:234`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `InterfaceDecl`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `name: String`
  - `generics: Vec<GenericParam>`
  - `methods: Vec<FunctionDecl>`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `name`: interface name
  - `generics`: generic params
  - `methods`: method list
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `InterfaceDecl` อ้าง type อื่น เช่น `Vec<String>, String, Vec<GenericParam>, Vec<FunctionDecl>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 5 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:28`, `parser.rs:51`, `ptypes.rs:129`, `ptypes.rs:234`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T014 `ProtocolDecl`
- ชื่อ type: `ProtocolDecl`
- ไฟล์+บรรทัด: `ptypes.rs:247`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `ProtocolDecl`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `name: String`
  - `generics: Vec<GenericParam>`
  - `associated_types: Vec<String>`
  - `methods: Vec<FunctionDecl>`
  - `extensions: Vec<FunctionDecl>`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `name`: protocol name
  - `generics`: generic params
  - `associated_types`: associated types
  - `methods`: method list
  - `extensions`: extension method list
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `ProtocolDecl` อ้าง type อื่น เช่น `Vec<String>, String, Vec<GenericParam>, Vec<FunctionDecl>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 5 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:29`, `parser.rs:52`, `ptypes.rs:130`, `ptypes.rs:247`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T015 `FunctionDecl`
- ชื่อ type: `FunctionDecl`
- ไฟล์+บรรทัด: `ptypes.rs:264`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `FunctionDecl`
- หน้าที่: แทนข้อมูล function signature, constraint และ type metadata
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `visibility: Visibility`
  - `modifiers: Vec<FunctionModifier>`
  - `name: String`
  - `is_async: bool`
  - `generics: Vec<GenericParam>`
  - `params: Vec<Param>`
  - `context_params: Vec<Param>`
  - `ret_type: Option<TypeRef>`
  - `effects: Vec<Effect>`
  - `throws: bool`
  - `where_clauses: Vec<WhereClause>`
  - `body: Option<Block>`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `visibility`: access level
  - `modifiers`: extra flags like async or multidispatch
  - `name`: function name
  - `is_async`: quick async flag
  - `generics`: generic params like <T>
  - `params`: param list
  - `context_params`: context params after (using ...)
  - `ret_type`: return type or None
  - `effects`: effect list after marker
  - `throws`: throws flag
  - `where_clauses`: where bounds
  - `body`: body block or None for decl
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `FunctionDecl` อ้าง type อื่น เช่น `Vec<String>, Visibility, Vec<FunctionModifier>, String, bool, Vec<GenericParam>, Vec<Param>, Option<TypeRef>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 4 จุด, `ptypes.rs` 10 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:804`, `parser.rs:883`, `ptypes.rs:123`, `ptypes.rs:186`
- ตัวอย่างสั้น:
```rust
let t = TypeRef { name: "Result".into(), generics: vec![], nullable: false, pointer_type: None };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T016 `GenericParam`
- ชื่อ type: `GenericParam`
- ไฟล์+บรรทัด: `ptypes.rs:295`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `GenericParam`
- หน้าที่: แทนข้อมูล function signature, constraint และ type metadata
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `name: String`
  - `constraints: Vec<TypeConstraint>`
- ความหมายของแต่ละ field/variant:
  - `name`: param name
  - `constraints`: bounds for this param
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `GenericParam` อ้าง type อื่น เช่น `String, Vec<TypeConstraint>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 2 จุด, `ptypes.rs` 5 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:1276`, `parser.rs:1291`, `ptypes.rs:240`, `ptypes.rs:253`
- ตัวอย่างสั้น:
```rust
let t = TypeRef { name: "Result".into(), generics: vec![], nullable: false, pointer_type: None };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T017 `TypeConstraint`
- ชื่อ type: `TypeConstraint`
- ไฟล์+บรรทัด: `ptypes.rs:304`
- ชนิด/visibility: `enum` / `pub`
- นิยามย่อ: ชุดตัวเลือก (variants) สำหรับ `TypeConstraint`
- หน้าที่: แทนข้อมูล function signature, constraint และ type metadata
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `TraitBound(String)` (tuple variant)
  - `LifetimeBound(String)` (tuple variant)
  - `TypeEq(String, String)` (tuple variant)
  - `SubtypeOf(String)` (tuple variant)
  - `SupertypeOf(String)` (tuple variant)
- ความหมายของแต่ละ field/variant:
  - `TraitBound`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `LifetimeBound`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `TypeEq`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `SubtypeOf`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `SupertypeOf`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
- เชื่อมกับ type อื่นอย่างไร:
  - variants ของ `TypeConstraint` พ่วง type อื่น เช่น `String`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 6 จุด, `ptypes.rs` 3 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:1359`, `parser.rs:1371`, `ptypes.rs:299`, `ptypes.rs:304`
- ตัวอย่างสั้น:
```rust
let t = TypeRef { name: "Result".into(), generics: vec![], nullable: false, pointer_type: None };
```
- ข้อควรระวัง: เวลา match enum ต้องระวัง variant ที่มี payload และ variant ที่ไม่มี payload ให้ถูกรูปแบบ

### T018 `WhereClause`
- ชื่อ type: `WhereClause`
- ไฟล์+บรรทัด: `ptypes.rs:314`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `WhereClause`
- หน้าที่: แทนข้อมูล function signature, constraint และ type metadata
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `param_name: String`
  - `constraints: Vec<TypeConstraint>`
- ความหมายของแต่ละ field/variant:
  - `param_name`: name on left side
  - `constraints`: bounds list
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `WhereClause` อ้าง type อื่น เช่น `String, Vec<TypeConstraint>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 2 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:1411`, `parser.rs:1421`, `ptypes.rs:288`, `ptypes.rs:314`
- ตัวอย่างสั้น:
```rust
let t = TypeRef { name: "Result".into(), generics: vec![], nullable: false, pointer_type: None };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T019 `Effect`
- ชื่อ type: `Effect`
- ไฟล์+บรรทัด: `ptypes.rs:323`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `Effect`
- หน้าที่: แทนข้อมูล function signature, constraint และ type metadata
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `name: String`
  - `params: Option<Vec<TypeRef>>`
- ความหมายของแต่ละ field/variant:
  - `name`: effect name
  - `params`: optional type params
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `Effect` อ้าง type อื่น เช่น `String, Option<Vec<TypeRef>>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 2 จุด, `tokenizer.rs` 1 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:1310`, `parser.rs:1352`, `tokenizer.rs:248`, `ptypes.rs:284`, `ptypes.rs:323`
- ตัวอย่างสั้น:
```rust
let t = TypeRef { name: "Result".into(), generics: vec![], nullable: false, pointer_type: None };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T020 `Visibility`
- ชื่อ type: `Visibility`
- ไฟล์+บรรทัด: `ptypes.rs:332`
- ชนิด/visibility: `enum` / `pub`
- นิยามย่อ: ชุดตัวเลือก (variants) สำหรับ `Visibility`
- หน้าที่: แทนข้อมูล function signature, constraint และ type metadata
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `Public` (unit variant)
  - `Private` (unit variant)
  - `Protected` (unit variant)
  - `Internal` (unit variant)
  - `Package` (unit variant)
- ความหมายของแต่ละ field/variant:
  - `Public`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Private`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Protected`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Internal`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Package`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
- เชื่อมกับ type อื่นอย่างไร:
  - variants เป็น unit เป็นหลัก จึงไม่พ่วง type payload เพิ่ม
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 14 จุด, `ptypes.rs` 6 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:370`, `parser.rs:372`, `ptypes.rs:268`, `ptypes.rs:332`
- ตัวอย่างสั้น:
```rust
let t = TypeRef { name: "Result".into(), generics: vec![], nullable: false, pointer_type: None };
```
- ข้อควรระวัง: เวลา match enum ต้องระวัง variant ที่มี payload และ variant ที่ไม่มี payload ให้ถูกรูปแบบ

### T021 `FunctionModifier`
- ชื่อ type: `FunctionModifier`
- ไฟล์+บรรทัด: `ptypes.rs:342`
- ชนิด/visibility: `enum` / `pub`
- นิยามย่อ: ชุดตัวเลือก (variants) สำหรับ `FunctionModifier`
- หน้าที่: แทนข้อมูล function signature, constraint และ type metadata
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `Async` (unit variant)
  - `Multidispatch` (unit variant)
  - `Constexpr` (unit variant)
  - `Scoped` (unit variant)
  - `Comptime` (unit variant)
- ความหมายของแต่ละ field/variant:
  - `Async`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Multidispatch`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Constexpr`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Scoped`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Comptime`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
- เชื่อมกับ type อื่นอย่างไร:
  - variants เป็น unit เป็นหลัก จึงไม่พ่วง type payload เพิ่ม
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 1 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:1168`, `ptypes.rs:270`, `ptypes.rs:342`
- ตัวอย่างสั้น:
```rust
let t = TypeRef { name: "Result".into(), generics: vec![], nullable: false, pointer_type: None };
```
- ข้อควรระวัง: เวลา match enum ต้องระวัง variant ที่มี payload และ variant ที่ไม่มี payload ให้ถูกรูปแบบ

### T022 `Param`
- ชื่อ type: `Param`
- ไฟล์+บรรทัด: `ptypes.rs:352`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `Param`
- หน้าที่: แทนข้อมูล function signature, constraint และ type metadata
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `pattern: Pattern`
  - `typ: Option<TypeRef>`
  - `default: Option<Expr>`
- ความหมายของแต่ละ field/variant:
  - `pattern`: pattern for destructuring
  - `typ`: optional type
  - `default`: default value
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `Param` อ้าง type อื่น เช่น `Pattern, Option<TypeRef>, Option<Expr>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 9 จุด, `ptypes.rs` 6 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:822`, `parser.rs:832`, `ptypes.rs:199`, `ptypes.rs:278`
- ตัวอย่างสั้น:
```rust
let t = TypeRef { name: "Result".into(), generics: vec![], nullable: false, pointer_type: None };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T023 `StructDecl`
- ชื่อ type: `StructDecl`
- ไฟล์+บรรทัด: `ptypes.rs:363`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `StructDecl`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `name: String`
  - `fields: Vec<(String, TypeRef)>`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `name`: struct name
  - `fields`: field list (name, type)
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `StructDecl` อ้าง type อื่น เช่น `Vec<String>, String, Vec<(String, TypeRef)>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 2 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:1437`, `parser.rs:1452`, `ptypes.rs:124`, `ptypes.rs:363`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T024 `EnumDecl`
- ชื่อ type: `EnumDecl`
- ไฟล์+บรรทัด: `ptypes.rs:374`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `EnumDecl`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `name: String`
  - `generics: Vec<GenericParam>`
  - `variants: Vec<EnumVariant>`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `name`: enum name
  - `generics`: generic params
  - `variants`: variant list
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `EnumDecl` อ้าง type อื่น เช่น `Vec<String>, String, Vec<GenericParam>, Vec<EnumVariant>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 2 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:1460`, `parser.rs:1537`, `ptypes.rs:125`, `ptypes.rs:374`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T025 `EnumVariantField`
- ชื่อ type: `EnumVariantField`
- ไฟล์+บรรทัด: `ptypes.rs:387`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `EnumVariantField`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `name: Option<String>`
  - `typ: TypeRef`
- ความหมายของแต่ละ field/variant:
  - `name`: optional field name
  - `typ`: field type
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `EnumVariantField` อ้าง type อื่น เช่น `Option<String>, TypeRef`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 3 จุด, `ptypes.rs` 3 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:1486`, `parser.rs:1509`, `ptypes.rs:387`, `ptypes.rs:398`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T026 `VariantKind`
- ชื่อ type: `VariantKind`
- ไฟล์+บรรทัด: `ptypes.rs:396`
- ชนิด/visibility: `enum` / `pub`
- นิยามย่อ: ชุดตัวเลือก (variants) สำหรับ `VariantKind`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `Unit` (unit variant)
  - `Tuple(Vec<EnumVariantField>)` (tuple variant)
  - `Struct(Vec<EnumVariantField>)` (tuple variant)
- ความหมายของแต่ละ field/variant:
  - `Unit`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Tuple`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Struct`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
- เชื่อมกับ type อื่นอย่างไร:
  - variants ของ `VariantKind` พ่วง type อื่น เช่น `Vec<EnumVariantField>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 3 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:1493`, `parser.rs:1520`, `ptypes.rs:396`, `ptypes.rs:406`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: เวลา match enum ต้องระวัง variant ที่มี payload และ variant ที่ไม่มี payload ให้ถูกรูปแบบ

### T027 `EnumVariant`
- ชื่อ type: `EnumVariant`
- ไฟล์+บรรทัด: `ptypes.rs:404`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `EnumVariant`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `name: String`
  - `kind: VariantKind`
- ความหมายของแต่ละ field/variant:
  - `name`: ข้อมูล `name` ของ `EnumVariant`
  - `kind`: ข้อมูล `kind` ของ `EnumVariant`
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `EnumVariant` อ้าง type อื่น เช่น `String, VariantKind`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 4 จุด, `ptypes.rs` 3 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:1525`, `parser.rs:4733`, `ptypes.rs:382`, `ptypes.rs:404`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T028 `TraitDecl`
- ชื่อ type: `TraitDecl`
- ไฟล์+บรรทัด: `ptypes.rs:411`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `TraitDecl`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `name: String`
  - `methods: Vec<FunctionDecl>`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `name`: trait name
  - `methods`: method list
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `TraitDecl` อ้าง type อื่น เช่น `Vec<String>, String, Vec<FunctionDecl>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 2 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:1618`, `parser.rs:1635`, `ptypes.rs:126`, `ptypes.rs:411`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T029 `ImplDecl`
- ชื่อ type: `ImplDecl`
- ไฟล์+บรรทัด: `ptypes.rs:422`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `ImplDecl`
- หน้าที่: แทนโครงสร้างระดับประกาศ (declarations) ของภาษา
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `trait_name: Option<String>`
  - `target: TypeRef`
  - `methods: Vec<FunctionDecl>`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `trait_name`: Some(trait) for trait impl, None for inherent
  - `target`: target type for impl
  - `methods`: method list
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `ImplDecl` อ้าง type อื่น เช่น `Vec<String>, Option<String>, TypeRef, Vec<FunctionDecl>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 2 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:1754`, `parser.rs:1779`, `ptypes.rs:127`, `ptypes.rs:422`
- ตัวอย่างสั้น:
```rust
let program = Program { items: vec![Item::Struct(my_struct)] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T030 `ClassDecl`
- ชื่อ type: `ClassDecl`
- ไฟล์+บรรทัด: `ptypes.rs:435`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `ClassDecl`
- หน้าที่: แทนองค์ประกอบระบบ class/object
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `name: String`
  - `extends: Vec<TypeRef>`
  - `mixins: Vec<TypeRef>`
  - `implements: Vec<TypeRef>`
  - `friends: Vec<String>`
  - `fields: Vec<ClassFieldDecl>`
  - `properties: Vec<ClassPropertyDecl>`
  - `static_inits: Vec<Block>`
  - `deinit: Option<Block>`
  - `delegates: Vec<ClassDelegateDecl>`
  - `ctors: Vec<ConstructorDecl>`
  - `methods: Vec<FunctionDecl>`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `name`: class name
  - `extends`: extends list
  - `mixins`: mixin list after with
  - `implements`: implements list
  - `friends`: friend class list
  - `fields`: field list
  - `properties`: property list
  - `static_inits`: static init blocks
  - `deinit`: deinit block
  - `delegates`: delegate list
  - `ctors`: constructor list
  - `methods`: method list
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `ClassDecl` อ้าง type อื่น เช่น `Vec<String>, String, Vec<TypeRef>, Vec<ClassFieldDecl>, Vec<ClassPropertyDecl>, Vec<Block>, Option<Block>, Vec<ClassDelegateDecl>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 2 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:648`, `parser.rs:783`, `ptypes.rs:131`, `ptypes.rs:435`
- ตัวอย่างสั้น:
```rust
let ctor = ConstructorDecl { attributes: vec![], visibility: Visibility::Public, name: None, params: vec![], throws: false, body: Block { stmts: vec![] } };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T031 `ClassFieldDecl`
- ชื่อ type: `ClassFieldDecl`
- ไฟล์+บรรทัด: `ptypes.rs:466`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `ClassFieldDecl`
- หน้าที่: แทนองค์ประกอบระบบ class/object
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `vis: Visibility`
  - `mutable: bool`
  - `name: String`
  - `typ: TypeRef`
  - `value: Option<Expr>`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `vis`: access level
  - `mutable`: mutable flag
  - `name`: field name
  - `typ`: field type
  - `value`: default value
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `ClassFieldDecl` อ้าง type อื่น เช่น `Vec<String>, Visibility, bool, String, TypeRef, Option<Expr>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 2 จุด, `ptypes.rs` 3 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:163`, `parser.rs:991`, `ptypes.rs:223`, `ptypes.rs:449`
- ตัวอย่างสั้น:
```rust
let ctor = ConstructorDecl { attributes: vec![], visibility: Visibility::Public, name: None, params: vec![], throws: false, body: Block { stmts: vec![] } };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T032 `ClassPropertyDecl`
- ชื่อ type: `ClassPropertyDecl`
- ไฟล์+บรรทัด: `ptypes.rs:483`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `ClassPropertyDecl`
- หน้าที่: แทนองค์ประกอบระบบ class/object
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `vis: Visibility`
  - `name: String`
  - `typ: Option<TypeRef>`
  - `value: Option<Expr>`
  - `getter: Option<Block>`
  - `setter_param: Option<String>`
  - `setter_body: Option<Block>`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `vis`: access level
  - `name`: property name
  - `typ`: optional type
  - `value`: computed value expression
  - `getter`: getter block
  - `setter_param`: setter param name
  - `setter_body`: setter block
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `ClassPropertyDecl` อ้าง type อื่น เช่น `Vec<String>, Visibility, String, Option<TypeRef>, Option<Expr>, Option<Block>, Option<String>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 5 จุด, `ptypes.rs` 3 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:164`, `parser.rs:928`, `ptypes.rs:225`, `ptypes.rs:451`
- ตัวอย่างสั้น:
```rust
let ctor = ConstructorDecl { attributes: vec![], visibility: Visibility::Public, name: None, params: vec![], throws: false, body: Block { stmts: vec![] } };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T033 `ClassDelegateDecl`
- ชื่อ type: `ClassDelegateDecl`
- ไฟล์+บรรทัด: `ptypes.rs:504`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `ClassDelegateDecl`
- หน้าที่: แทนองค์ประกอบระบบ class/object
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `vis: Visibility`
  - `name: String`
  - `target: Expr`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `vis`: access level
  - `name`: delegate name
  - `target`: target expression
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `ClassDelegateDecl` อ้าง type อื่น เช่น `Vec<String>, Visibility, String, Expr`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 2 จุด, `ptypes.rs` 3 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:904`, `parser.rs:909`, `ptypes.rs:227`, `ptypes.rs:457`
- ตัวอย่างสั้น:
```rust
let ctor = ConstructorDecl { attributes: vec![], visibility: Visibility::Public, name: None, params: vec![], throws: false, body: Block { stmts: vec![] } };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T034 `ConstructorDecl`
- ชื่อ type: `ConstructorDecl`
- ไฟล์+บรรทัด: `ptypes.rs:517`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `ConstructorDecl`
- หน้าที่: แทนองค์ประกอบระบบ class/object
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `attributes: Vec<String>`
  - `visibility: Visibility`
  - `name: Option<String>`
  - `params: Vec<Param>`
  - `throws: bool`
  - `body: Block`
- ความหมายของแต่ละ field/variant:
  - `attributes`: raw @attrs in source order
  - `visibility`: access level
  - `name`: optional named constructor
  - `params`: param list
  - `throws`: throws flag
  - `body`: constructor body
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `ConstructorDecl` อ้าง type อื่น เช่น `Vec<String>, Visibility, Option<String>, Vec<Param>, bool, Block`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 2 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:1066`, `parser.rs:1104`, `ptypes.rs:459`, `ptypes.rs:517`
- ตัวอย่างสั้น:
```rust
let ctor = ConstructorDecl { attributes: vec![], visibility: Visibility::Public, name: None, params: vec![], throws: false, body: Block { stmts: vec![] } };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T035 `Block`
- ชื่อ type: `Block`
- ไฟล์+บรรทัด: `ptypes.rs:534`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `Block`
- หน้าที่: แทน statement ทั่วไปและ temporal statements
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `stmts: Vec<Stmt>`
- ความหมายของแต่ละ field/variant:
  - `stmts`: ข้อมูล `stmts` ของ `Block`
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `Block` อ้าง type อื่น เช่น `Vec<Stmt>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 20 จุด, `ptypes.rs` 55 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:874`, `parser.rs:1003`, `ptypes.rs:173`, `ptypes.rs:290`
- ตัวอย่างสั้น:
```rust
let stmt = Stmt::Checkpoint { name: Some("cp".into()), metadata: None, body: Block { stmts: vec![] }, preserve: None };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T036 `Stmt`
- ชื่อ type: `Stmt`
- ไฟล์+บรรทัด: `ptypes.rs:540`
- ชนิด/visibility: `enum` / `pub`
- นิยามย่อ: ชุดตัวเลือก (variants) สำหรับ `Stmt`
- หน้าที่: แทน statement ทั่วไปและ temporal statements
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `Attributed { attributes: Vec<String>, stmt: Box<Stmt> }` (struct variant)
  - `Using { target: Expr }` (struct variant)
  - `Let { pattern: Pattern, mutable: bool, typ: Option<TypeRef>, expr: Option<Expr> }` (struct variant)
  - `Expr(Expr)` (tuple variant)
  - `Return(Option<Expr>)` (tuple variant)
  - `Continue` (unit variant)
  - `Break` (unit variant)
  - `TryCatch { try_block: Block, catch_name: String, catch_block: Block }` (struct variant)
  - `If { cond: Expr, then_block: Block, else_block: Option<Block> }` (struct variant)
  - `While { cond: Expr, body: Block }` (struct variant)
  - `For { pattern: Pattern, iter: Expr, body: Block }` (struct variant)
  - `Loop { body: Block }` (struct variant)
  - `Match { expr: Expr, arms: Vec<MatchArm> }` (struct variant)
  - `Watch { variables: Vec<Expr>, clauses: Vec<WatchClause> }` (struct variant)
  - `Converge { history: bool, variable: String, body: Block, until: Expr }` (struct variant)
  - `InContext { target: Expr, arms: Vec<ContextArm> }` (struct variant)
  - `Within { time: Expr, condition: Option<Expr>, body: Block, fallback: Option<Expr> }` (struct variant)
  - `Atomically { body: Block }` (struct variant)
  - `Trap { error_condition: Expr, body: Block }` (struct variant)
  - `Guard { condition: Expr, then_block: Block, else_block: Option<Block> }` (struct variant)
  - `Poll { signal: Expr, interval: Expr, body: Block }` (struct variant)
  - `Delete { expr: Expr }` (struct variant)
  - `Joins { decls: Vec<Stmt>, body: Block }` (struct variant)
  - `MatchCond { arms: Vec<MatchCondArm> }` (struct variant)
  - `OnSequence { target: Expr, sequence: Expr, body: Block }` (struct variant)
  - `PanicUnless { condition: Expr }` (struct variant)
  - `Scope { body: Block }` (struct variant)
  - `Defer { expr: Expr }` (struct variant)
  - `Checkpoint { name: Option<String>, metadata: Option<Expr>, body: Block, preserve: Option<Block> }` (struct variant)
  - `Rewind { subject: Option<Expr>, target: Option<Expr>, condition: Option<Expr>, query: Option<Block> }` (struct variant)
  - `Inspect { target: Option<Expr>, filter: Option<Block>, body: Option<Block> }` (struct variant)
  - `TemporalScope { name: Option<String>, config: Option<Expr>, body: Block }` (struct variant)
  - `TemporalTransaction { config: Option<Expr>, body: Block, catch_name: Option<String>, catch_block: Option<Block> }` (struct variant)
  - `TemporalTest { name: String, config: Option<Expr>, body: Block }` (struct variant)
  - `TemporalMemory { config: Option<Expr>, body: Block }` (struct variant)
  - `BatchTemporal { body: Block, optimize: Option<Expr> }` (struct variant)
  - `Replay { recording: Expr, body: Block }` (struct variant)
  - `ReplayPause { each: bool, checkpoint: Option<Expr> }` (struct variant)
  - `ReplayOnCheckpoint { checkpoint: Expr, body: Block }` (struct variant)
  - `ReplayModify { target: Expr, value: Expr }` (struct variant)
  - `Snapshot { name: Option<String>, metadata: Option<Expr> }` (struct variant)
  - `Rollback { subject: Option<Expr>, target: Option<Expr>, condition: Option<Expr>, metadata: Option<Expr>, query: Option<Block> }` (struct variant)
  - `MergeBranch { branch: Expr, target: Expr }` (struct variant)
  - `TemporalRetry { max_times: Expr, body: Block }` (struct variant)
  - `AutoCheckpoint { body: Block }` (struct variant)
  - `Emit { target: Expr, body: Block }` (struct variant)
  - `GcTemporal { filter: Option<Block> }` (struct variant)
  - `AssertTemporal { kind: String, body: Block }` (struct variant)
  - `Commit { metadata: Option<Expr> }` (struct variant)
  - `Cleanup { body: Block }` (struct variant)
  - `DebugTemporal { clauses: Vec<TemporalClause> }` (struct variant)
  - `TemporalHandle { body: Block, handlers: Vec<TemporalClause> }` (struct variant)
  - `TemporalMatch { target: Expr, clauses: Vec<TemporalClause> }` (struct variant)
  - `CompileTimeBlock { body: Block }` (struct variant)
- ความหมายของแต่ละ field/variant:
  - `Attributed`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Using`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Let`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Expr`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Return`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Continue`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Break`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `TryCatch`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `If`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `While`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `For`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Loop`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Match`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Watch`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Converge`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `InContext`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Within`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Atomically`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Trap`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Guard`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Poll`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Delete`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Joins`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `MatchCond`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `OnSequence`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `PanicUnless`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Scope`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Defer`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Checkpoint`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Rewind`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Inspect`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `TemporalScope`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `TemporalTransaction`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `TemporalTest`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `TemporalMemory`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `BatchTemporal`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Replay`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `ReplayPause`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `ReplayOnCheckpoint`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `ReplayModify`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Snapshot`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Rollback`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `MergeBranch`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `TemporalRetry`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `AutoCheckpoint`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Emit`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `GcTemporal`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `AssertTemporal`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Commit`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Cleanup`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `DebugTemporal`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `TemporalHandle`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `TemporalMatch`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `CompileTimeBlock`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
- เชื่อมกับ type อื่นอย่างไร:
  - variants ของ `Stmt` พ่วง type อื่น เช่น `Vec<String>, Box<Stmt>, Expr, Pattern, bool, Option<TypeRef>, Option<Expr>, Block`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 115 จุด, `ptypes.rs` 4 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:875`, `parser.rs:1015`, `ptypes.rs:535`, `ptypes.rs:540`
- ตัวอย่างสั้น:
```rust
let stmt = Stmt::Checkpoint { name: Some("cp".into()), metadata: None, body: Block { stmts: vec![] }, preserve: None };
```
- ข้อควรระวัง: enum นี้มี variants จำนวนมาก; เวลาเพิ่ม/แก้ parser ต้องตรวจ exhaustiveness ให้ครบทุกแขนง

### T037 `TemporalPattern`
- ชื่อ type: `TemporalPattern`
- ไฟล์+บรรทัด: `ptypes.rs:759`
- ชนิด/visibility: `enum` / `pub`
- นิยามย่อ: ชุดตัวเลือก (variants) สำหรับ `TemporalPattern`
- หน้าที่: แทน statement ทั่วไปและ temporal statements
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `Parsed { kind: String, args: Vec<Expr>, condition: Option<Expr> }` (struct variant)
  - `Raw(Vec<Token>)` (tuple variant)
- ความหมายของแต่ละ field/variant:
  - `Parsed`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Raw`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
- เชื่อมกับ type อื่นอย่างไร:
  - variants ของ `TemporalPattern` พ่วง type อื่น เช่น `String, Vec<Expr>, Option<Expr>, Vec<Token>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 3 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:2766`, `parser.rs:2784`, `ptypes.rs:759`, `ptypes.rs:775`
- ตัวอย่างสั้น:
```rust
let stmt = Stmt::Checkpoint { name: Some("cp".into()), metadata: None, body: Block { stmts: vec![] }, preserve: None };
```
- ข้อควรระวัง: เวลา match enum ต้องระวัง variant ที่มี payload และ variant ที่ไม่มี payload ให้ถูกรูปแบบ

### T038 `TemporalClause`
- ชื่อ type: `TemporalClause`
- ไฟล์+บรรทัด: `ptypes.rs:772`
- ชนิด/visibility: `enum` / `pub`
- นิยามย่อ: ชุดตัวเลือก (variants) สำหรับ `TemporalClause`
- หน้าที่: แทน statement ทั่วไปและ temporal statements
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `Parsed { pattern: TemporalPattern, guard: Option<Expr>, body: Block }` (struct variant)
  - `Raw(Vec<Token>)` (tuple variant)
- ความหมายของแต่ละ field/variant:
  - `Parsed`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Raw`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
- เชื่อมกับ type อื่นอย่างไร:
  - variants ของ `TemporalClause` พ่วง type อื่น เช่น `TemporalPattern, Option<Expr>, Block, Vec<Token>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 4 จุด, `ptypes.rs` 4 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:2758`, `parser.rs:2792`, `ptypes.rs:742`, `ptypes.rs:746`
- ตัวอย่างสั้น:
```rust
let stmt = Stmt::Checkpoint { name: Some("cp".into()), metadata: None, body: Block { stmts: vec![] }, preserve: None };
```
- ข้อควรระวัง: เวลา match enum ต้องระวัง variant ที่มี payload และ variant ที่ไม่มี payload ให้ถูกรูปแบบ

### T039 `MatchArm`
- ชื่อ type: `MatchArm`
- ไฟล์+บรรทัด: `ptypes.rs:785`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `MatchArm`
- หน้าที่: แทนโครง pattern matching และการ bind ตัวแปร
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `pattern: Pattern`
  - `guard: Option<Expr>`
  - `body: Expr`
- ความหมายของแต่ละ field/variant:
  - `pattern`: ข้อมูล `pattern` ของ `MatchArm`
  - `guard`: ข้อมูล `guard` ของ `MatchArm`
  - `body`: ข้อมูล `body` ของ `MatchArm`
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `MatchArm` อ้าง type อื่น เช่น `Pattern, Option<Expr>, Expr`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 2 จุด, `ptypes.rs` 3 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:3173`, `parser.rs:3604`, `ptypes.rs:582`, `ptypes.rs:785`
- ตัวอย่างสั้น:
```rust
let p = Pattern { kind: PatternKind::Identifier("x".into()), bindings: vec!["x".into()] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T040 `Pattern`
- ชื่อ type: `Pattern`
- ไฟล์+บรรทัด: `ptypes.rs:793`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `Pattern`
- หน้าที่: แทนโครง pattern matching และการ bind ตัวแปร
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `kind: PatternKind`
  - `bindings: Vec<String>`
- ความหมายของแต่ละ field/variant:
  - `kind`: kind of pattern
  - `bindings`: bindings found in pattern
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `Pattern` อ้าง type อื่น เช่น `PatternKind, Vec<String>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 15 จุด, `ptypes.rs` 15 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:2893`, `parser.rs:2915`, `ptypes.rs:354`, `ptypes.rs:549`
- ตัวอย่างสั้น:
```rust
let p = Pattern { kind: PatternKind::Identifier("x".into()), bindings: vec!["x".into()] };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T041 `PatternKind`
- ชื่อ type: `PatternKind`
- ไฟล์+บรรทัด: `ptypes.rs:802`
- ชนิด/visibility: `enum` / `pub`
- นิยามย่อ: ชุดตัวเลือก (variants) สำหรับ `PatternKind`
- หน้าที่: แทนโครง pattern matching และการ bind ตัวแปร
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `Identifier(String)` (tuple variant)
  - `Ref(Box<Pattern>)` (tuple variant)
  - `Wildcard` (unit variant)
  - `Literal(Literal)` (tuple variant)
  - `Struct { name: Option<String>, fields: Vec<(String, Pattern)> }` (struct variant)
  - `Tuple(Vec<Pattern>)` (tuple variant)
  - `Or(Vec<Pattern>)` (tuple variant)
  - `SomeVariant(Box<Pattern>)` (tuple variant)
  - `NoneVariant` (unit variant)
  - `OkVariant(Box<Pattern>)` (tuple variant)
  - `ErrVariant(Box<Pattern>)` (tuple variant)
  - `EnumVariant { variant_name: String, inner_pattern: Option<Box<Pattern>> }` (struct variant)
  - `Array(Vec<Pattern>)` (tuple variant)
  - `Nil` (unit variant)
- ความหมายของแต่ละ field/variant:
  - `Identifier`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Ref`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Wildcard`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Literal`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Struct`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Tuple`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Or`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `SomeVariant`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `NoneVariant`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `OkVariant`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `ErrVariant`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `EnumVariant`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Array`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Nil`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
- เชื่อมกับ type อื่นอย่างไร:
  - variants ของ `PatternKind` พ่วง type อื่น เช่น `String, Box<Pattern>, Literal, Option<String>, Vec<(String, Pattern)>, Vec<Pattern>, Option<Box<Pattern>>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 26 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:1797`, `parser.rs:2916`, `ptypes.rs:795`, `ptypes.rs:802`
- ตัวอย่างสั้น:
```rust
let p = Pattern { kind: PatternKind::Identifier("x".into()), bindings: vec!["x".into()] };
```
- ข้อควรระวัง: enum นี้มี variants จำนวนมาก; เวลาเพิ่ม/แก้ parser ต้องตรวจ exhaustiveness ให้ครบทุกแขนง

### T042 `WatchClause`
- ชื่อ type: `WatchClause`
- ไฟล์+บรรทัด: `ptypes.rs:827`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `WatchClause`
- หน้าที่: แทน statement ทั่วไปและ temporal statements
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `condition: Expr`
  - `body: Block`
- ความหมายของแต่ละ field/variant:
  - `condition`: ข้อมูล `condition` ของ `WatchClause`
  - `body`: ข้อมูล `body` ของ `WatchClause`
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `WatchClause` อ้าง type อื่น เช่น `Expr, Block`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 1 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:3252`, `ptypes.rs:586`, `ptypes.rs:827`
- ตัวอย่างสั้น:
```rust
let stmt = Stmt::Checkpoint { name: Some("cp".into()), metadata: None, body: Block { stmts: vec![] }, preserve: None };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T043 `ContextArm`
- ชื่อ type: `ContextArm`
- ไฟล์+บรรทัด: `ptypes.rs:834`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `ContextArm`
- หน้าที่: แทน statement ทั่วไปและ temporal statements
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `value: Expr`
  - `body: Block`
- ความหมายของแต่ละ field/variant:
  - `value`: ข้อมูล `value` ของ `ContextArm`
  - `body`: ข้อมูล `body` ของ `ContextArm`
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `ContextArm` อ้าง type อื่น เช่น `Expr, Block`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 1 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:3448`, `ptypes.rs:596`, `ptypes.rs:834`
- ตัวอย่างสั้น:
```rust
let stmt = Stmt::Checkpoint { name: Some("cp".into()), metadata: None, body: Block { stmts: vec![] }, preserve: None };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T044 `MatchCondArm`
- ชื่อ type: `MatchCondArm`
- ไฟล์+บรรทัด: `ptypes.rs:841`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `MatchCondArm`
- หน้าที่: แทน statement ทั่วไปและ temporal statements
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `condition: Expr`
  - `body: Block`
- ความหมายของแต่ละ field/variant:
  - `condition`: ข้อมูล `condition` ของ `MatchCondArm`
  - `body`: ข้อมูล `body` ของ `MatchCondArm`
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `MatchCondArm` อ้าง type อื่น เช่น `Expr, Block`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 1 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:3153`, `ptypes.rs:629`, `ptypes.rs:841`
- ตัวอย่างสั้น:
```rust
let stmt = Stmt::Checkpoint { name: Some("cp".into()), metadata: None, body: Block { stmts: vec![] }, preserve: None };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T045 `Expr`
- ชื่อ type: `Expr`
- ไฟล์+บรรทัด: `ptypes.rs:848`
- ชนิด/visibility: `enum` / `pub`
- นิยามย่อ: ชุดตัวเลือก (variants) สำหรับ `Expr`
- หน้าที่: แทนโครง expression และ literal values
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `Literal(Literal)` (tuple variant)
  - `Ident(String)` (tuple variant)
  - `Phrase(String)` (tuple variant)
  - `EnumCase(String)` (tuple variant)
  - `Await(Box<Expr>)` (tuple variant)
  - `Spawn(Box<Expr>)` (tuple variant)
  - `Try(Box<Expr>)` (tuple variant)
  - `Rewind { subject: Option<Box<Expr>>, target: Option<Box<Expr>>, condition: Option<Box<Expr>>, query: Option<Block>, else_expr: Option<Box<Expr>> }` (struct variant)
  - `IfExpr { cond: Box<Expr>, then_branch: Box<Expr>, else_branch: Option<Box<Expr>> }` (struct variant)
  - `MatchExpr { expr: Box<Expr>, arms: Vec<MatchArm> }` (struct variant)
  - `Binary { left: Box<Expr>, op: String, right: Box<Expr> }` (struct variant)
  - `Unary { op: String, rhs: Box<Expr> }` (struct variant)
  - `Grouping(Box<Expr>)` (tuple variant)
  - `Block(Block)` (tuple variant)
  - `AsyncBlock { body: Block, checkpoint: Option<Box<Expr>> }` (struct variant)
  - `Call { callee: Box<Expr>, generics: Vec<TypeRef>, args: Vec<Expr> }` (struct variant)
  - `MethodCall { object: Box<Expr>, method: String, generics: Vec<TypeRef>, args: Vec<Expr> }` (struct variant)
  - `FieldAccess { object: Box<Expr>, field: String }` (struct variant)
  - `OptionalFieldAccess { object: Box<Expr>, field: String }` (struct variant)
  - `Index { array: Box<Expr>, index: Box<Expr> }` (struct variant)
  - `Slice { target: Box<Expr>, start: Option<Box<Expr>>, end: Option<Box<Expr>> }` (struct variant)
  - `Lambda { params: Vec<Param>, body: Box<Expr> }` (struct variant)
  - `Range { start: Option<Box<Expr>>, end: Option<Box<Expr>>, inclusive: bool, step: Option<Box<Expr>> }` (struct variant)
  - `Pipeline { left: Box<Expr>, right: Box<Expr> }` (struct variant)
  - `PointerPipeline { left: Box<Expr>, right: Box<Expr> }` (struct variant)
  - `SelectorPipeline { left: Box<Expr>, right: Box<Expr> }` (struct variant)
  - `ListComp { expr: Box<Expr>, pattern: Pattern, iter: Box<Expr>, guard: Option<Box<Expr>> }` (struct variant)
  - `Coalesce { left: Box<Expr>, right: Box<Expr> }` (struct variant)
  - `OptionalCall { callee: Box<Expr>, generics: Vec<TypeRef>, args: Vec<Expr> }` (struct variant)
  - `InterpolatedString { parts: Vec<InterpolatedPart> }` (struct variant)
  - `MacroCall { name: String, args: Vec<Expr> }` (struct variant)
  - `PointerDeref { expr: Box<Expr>, safe: bool }` (struct variant)
  - `PointerRef { pointer_type: PointerType, expr: Box<Expr> }` (struct variant)
  - `PointerNew { pointer_type: PointerType, expr: Box<Expr>, at: Option<Box<Expr>>, expires: Option<Box<Expr>> }` (struct variant)
  - `Branch { name: Option<String>, from: Box<Expr>, body: Block }` (struct variant)
- ความหมายของแต่ละ field/variant:
  - `Literal`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Ident`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Phrase`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `EnumCase`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Await`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Spawn`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Try`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Rewind`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `IfExpr`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `MatchExpr`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Binary`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Unary`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Grouping`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Block`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `AsyncBlock`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Call`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `MethodCall`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `FieldAccess`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `OptionalFieldAccess`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Index`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Slice`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Lambda`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Range`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Pipeline`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `PointerPipeline`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `SelectorPipeline`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `ListComp`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Coalesce`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `OptionalCall`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `InterpolatedString`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `MacroCall`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `PointerDeref`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `PointerRef`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `PointerNew`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Branch`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
- เชื่อมกับ type อื่นอย่างไร:
  - variants ของ `Expr` พ่วง type อื่น เช่น `Literal, String, Box<Expr>, Option<Box<Expr>>, Option<Block>, Vec<MatchArm>, Block, Vec<TypeRef>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 124 จุด, `tokenizer.rs` 5 จุด, `ptypes.rs` 123 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:398`, `parser.rs:427`, `tokenizer.rs:182`, `tokenizer.rs:489`, `ptypes.rs:160`
- ตัวอย่างสั้น:
```rust
let e = Expr::Literal(Literal::String("hello".into()));
```
- ข้อควรระวัง: enum นี้มี variants จำนวนมาก; เวลาเพิ่ม/แก้ parser ต้องตรวจ exhaustiveness ให้ครบทุกแขนง

### T046 `InterpolatedPartKind`
- ชื่อ type: `InterpolatedPartKind`
- ไฟล์+บรรทัด: `ptypes.rs:982`
- ชนิด/visibility: `enum` / `pub`
- นิยามย่อ: ชุดตัวเลือก (variants) สำหรับ `InterpolatedPartKind`
- หน้าที่: แทนโครง expression และ literal values
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `Text(String)` (tuple variant)
  - `Expr(Expr)` (tuple variant)
- ความหมายของแต่ละ field/variant:
  - `Text`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Expr`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
- เชื่อมกับ type อื่นอย่างไร:
  - variants ของ `InterpolatedPartKind` พ่วง type อื่น เช่น `String, Expr`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 2 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:3976`, `parser.rs:4021`, `ptypes.rs:982`, `ptypes.rs:990`
- ตัวอย่างสั้น:
```rust
let e = Expr::Literal(Literal::String("hello".into()));
```
- ข้อควรระวัง: เวลา match enum ต้องระวัง variant ที่มี payload และ variant ที่ไม่มี payload ให้ถูกรูปแบบ

### T047 `InterpolatedPart`
- ชื่อ type: `InterpolatedPart`
- ไฟล์+บรรทัด: `ptypes.rs:989`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `InterpolatedPart`
- หน้าที่: แทนโครง expression และ literal values
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `kind: InterpolatedPartKind`
- ความหมายของแต่ละ field/variant:
  - `kind`: ข้อมูล `kind` ของ `InterpolatedPart`
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `InterpolatedPart` อ้าง type อื่น เช่น `InterpolatedPartKind`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 3 จุด, `ptypes.rs` 2 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:3967`, `parser.rs:3975`, `ptypes.rs:953`, `ptypes.rs:989`
- ตัวอย่างสั้น:
```rust
let e = Expr::Literal(Literal::String("hello".into()));
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T048 `Literal`
- ชื่อ type: `Literal`
- ไฟล์+บรรทัด: `ptypes.rs:995`
- ชนิด/visibility: `enum` / `pub`
- นิยามย่อ: ชุดตัวเลือก (variants) สำหรับ `Literal`
- หน้าที่: แทนโครง expression และ literal values
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `Int(String)` (tuple variant)
  - `Float(String)` (tuple variant)
  - `String(String)` (tuple variant)
  - `Bool(bool)` (tuple variant)
  - `Unit { v: String, u: String }` (struct variant)
  - `Array(Vec<Expr>)` (tuple variant)
  - `Vector(Vec<Expr>)` (tuple variant)
  - `Map(Vec<(Expr, Expr)>)` (tuple variant)
  - `Struct { name: String, base: Option<Box<Expr>>, fields: Vec<(String, Expr)> }` (struct variant)
  - `Tuple(Vec<Expr>)` (tuple variant)
- ความหมายของแต่ละ field/variant:
  - `Int`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Float`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `String`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Bool`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Unit`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Array`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Vector`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Map`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
  - `Struct`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
  - `Tuple`: ใช้แทนกรณีที่แนบ payload แบบตำแหน่ง (tuple)
- เชื่อมกับ type อื่นอย่างไร:
  - variants ของ `Literal` พ่วง type อื่น เช่น `String, bool, Vec<Expr>, Vec<(Expr, Expr)>, Option<Box<Expr>>, Vec<(String, Expr)>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 19 จุด, `ptypes.rs` 3 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:1938`, `parser.rs:2256`, `ptypes.rs:806`, `ptypes.rs:849`
- ตัวอย่างสั้น:
```rust
let e = Expr::Literal(Literal::String("hello".into()));
```
- ข้อควรระวัง: เวลา match enum ต้องระวัง variant ที่มี payload และ variant ที่ไม่มี payload ให้ถูกรูปแบบ

### T049 `TypeRef`
- ชื่อ type: `TypeRef`
- ไฟล์+บรรทัด: `ptypes.rs:1017`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `TypeRef`
- หน้าที่: แทนข้อมูล function signature, constraint และ type metadata
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `name: String`
  - `generics: Vec<TypeRef>`
  - `nullable: bool`
  - `pointer_type: Option<PointerType>`
- ความหมายของแต่ละ field/variant:
  - `name`: type name or path
  - `generics`: generic args
  - `nullable`: nullable flag
  - `pointer_type`: pointer flavor
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `TypeRef` อ้าง type อื่น เช่น `String, Vec<TypeRef>, bool, Option<PointerType>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 6 จุด, `ptypes.rs` 18 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:611`, `parser.rs:616`, `ptypes.rs:184`, `ptypes.rs:282`
- ตัวอย่างสั้น:
```rust
let t = TypeRef { name: "Result".into(), generics: vec![], nullable: false, pointer_type: None };
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T050 `PointerType`
- ชื่อ type: `PointerType`
- ไฟล์+บรรทัด: `ptypes.rs:1030`
- ชนิด/visibility: `enum` / `pub`
- นิยามย่อ: ชุดตัวเลือก (variants) สำหรับ `PointerType`
- หน้าที่: แทนข้อมูล function signature, constraint และ type metadata
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `RawPointer` (unit variant)
  - `ManagedPointer` (unit variant)
  - `WeakPointer` (unit variant)
  - `SharedPointer` (unit variant)
- ความหมายของแต่ละ field/variant:
  - `RawPointer`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `ManagedPointer`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `WeakPointer`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `SharedPointer`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
- เชื่อมกับ type อื่นอย่างไร:
  - variants เป็น unit เป็นหลัก จึงไม่พ่วง type payload เพิ่ม
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 12 จุด, `ptypes.rs` 4 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:3678`, `parser.rs:3692`, `ptypes.rs:964`, `ptypes.rs:968`
- ตัวอย่างสั้น:
```rust
let t = TypeRef { name: "Result".into(), generics: vec![], nullable: false, pointer_type: None };
```
- ข้อควรระวัง: เวลา match enum ต้องระวัง variant ที่มี payload และ variant ที่ไม่มี payload ให้ถูกรูปแบบ

### T051 `TokenType`
- ชื่อ type: `TokenType`
- ไฟล์+บรรทัด: `tokenizer.rs:7`
- ชนิด/visibility: `enum` / `pub`
- นิยามย่อ: ชุดตัวเลือก (variants) สำหรับ `TokenType`
- หน้าที่: ใช้ในขั้นตอนแยก source code เป็น token
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `Keyword` (unit variant)
  - `Operator` (unit variant)
  - `ShortArrow` (unit variant)
  - `FatArrow` (unit variant)
  - `LeftParen` (unit variant)
  - `RightParen` (unit variant)
  - `LeftBracket` (unit variant)
  - `RightBracket` (unit variant)
  - `LeftBrace` (unit variant)
  - `RightBrace` (unit variant)
  - `Comma` (unit variant)
  - `Semicolon` (unit variant)
  - `Colon` (unit variant)
  - `DoubleColon` (unit variant)
  - `Dot` (unit variant)
  - `TripleDot` (unit variant)
  - `Hash` (unit variant)
  - `Dollar` (unit variant)
  - `Backtick` (unit variant)
  - `Pipe` (unit variant)
  - `IntLiteral` (unit variant)
  - `FloatLiteral` (unit variant)
  - `StringLiteral` (unit variant)
  - `MultilineStringLiteral` (unit variant)
  - `InterpolatedStringStart` (unit variant)
  - `InterpolatedStringText` (unit variant)
  - `InterpolatedStringEnd` (unit variant)
  - `InterpolatedExprStart` (unit variant)
  - `InterpolatedExprEnd` (unit variant)
  - `RegexLiteral` (unit variant)
  - `BoolLiteral` (unit variant)
  - `UnitLiteral` (unit variant)
  - `Identifier` (unit variant)
  - `ModulePath` (unit variant)
  - `Comment` (unit variant)
  - `Attribute` (unit variant)
  - `ParameterizedAttribute` (unit variant)
  - `LambdaArrow` (unit variant)
  - `EffectMarker` (unit variant)
  - `Unknown` (unit variant)
  - `UnterminatedString` (unit variant)
  - `UnterminatedComment` (unit variant)
  - `InvalidUnit` (unit variant)
  - `UnterminatedRegex` (unit variant)
  - `EOF` (unit variant)
- ความหมายของแต่ละ field/variant:
  - `Keyword`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Operator`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `ShortArrow`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `FatArrow`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `LeftParen`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `RightParen`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `LeftBracket`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `RightBracket`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `LeftBrace`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `RightBrace`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Comma`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Semicolon`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Colon`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `DoubleColon`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Dot`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `TripleDot`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Hash`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Dollar`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Backtick`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Pipe`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `IntLiteral`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `FloatLiteral`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `StringLiteral`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `MultilineStringLiteral`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `InterpolatedStringStart`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `InterpolatedStringText`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `InterpolatedStringEnd`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `InterpolatedExprStart`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `InterpolatedExprEnd`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `RegexLiteral`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `BoolLiteral`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `UnitLiteral`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Identifier`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `ModulePath`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Comment`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Attribute`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `ParameterizedAttribute`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `LambdaArrow`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `EffectMarker`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Unknown`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `UnterminatedString`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `UnterminatedComment`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `InvalidUnit`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `UnterminatedRegex`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `EOF`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
- เชื่อมกับ type อื่นอย่างไร:
  - variants เป็น unit เป็นหลัก จึงไม่พ่วง type payload เพิ่ม
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 657 จุด, `tokenizer.rs` 129 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:8`, `parser.rs:177`, `tokenizer.rs:7`, `tokenizer.rs:75`
- ตัวอย่างสั้น:
```rust
let mut tokenizer = Tokenizer::new(source);
let tokens: Vec<Token> = tokenizer.tokenize();
```
- ข้อควรระวัง: enum นี้มี variants จำนวนมาก; เวลาเพิ่ม/แก้ parser ต้องตรวจ exhaustiveness ให้ครบทุกแขนง

### T052 `Token`
- ชื่อ type: `Token`
- ไฟล์+บรรทัด: `tokenizer.rs:74`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `Token`
- หน้าที่: ใช้ในขั้นตอนแยก source code เป็น token
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `token_type: TokenType`
  - `lexeme: String`
  - `line: usize`
  - `column: usize`
- ความหมายของแต่ละ field/variant:
  - `token_type`: ข้อมูล `token_type` ของ `Token`
  - `lexeme`: ข้อมูล `lexeme` ของ `Token`
  - `line`: ข้อมูล `line` ของ `Token`
  - `column`: ข้อมูล `column` ของ `Token`
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `Token` อ้าง type อื่น เช่น `TokenType, String, usize`
- ถูกใช้ที่ไหนใน parser/tokenizer: `parser.rs` 18 จุด, `tokenizer.rs` 49 จุด, `ptypes.rs` 4 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `parser.rs:8`, `parser.rs:158`, `tokenizer.rs:74`, `tokenizer.rs:81`, `ptypes.rs:5`
- ตัวอย่างสั้น:
```rust
let mut tokenizer = Tokenizer::new(source);
let tokens: Vec<Token> = tokenizer.tokenize();
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

### T053 `InterpolatedState`
- ชื่อ type: `InterpolatedState`
- ไฟล์+บรรทัด: `tokenizer.rs:180`
- ชนิด/visibility: `enum` / `private`
- นิยามย่อ: ชุดตัวเลือก (variants) สำหรับ `InterpolatedState`
- หน้าที่: ใช้ในขั้นตอนแยก source code เป็น token
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `String` (unit variant)
  - `Expr { brace_depth: usize }` (struct variant)
- ความหมายของแต่ละ field/variant:
  - `String`: ใช้แทนสถานะ/กรณีแบบไม่ต้องมี payload
  - `Expr`: ใช้แทนกรณีที่แนบ payload แบบมีชื่อ field
- เชื่อมกับ type อื่นอย่างไร:
  - variants ของ `InterpolatedState` พ่วง type อื่น เช่น `usize`
- ถูกใช้ที่ไหนใน parser/tokenizer: `tokenizer.rs` 10 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `tokenizer.rs:180`, `tokenizer.rs:193`
- ตัวอย่างสั้น:
```rust
let mut tokenizer = Tokenizer::new(source);
let tokens: Vec<Token> = tokenizer.tokenize();
```
- ข้อควรระวัง: เวลา match enum ต้องระวัง variant ที่มี payload และ variant ที่ไม่มี payload ให้ถูกรูปแบบ

### T054 `Tokenizer`
- ชื่อ type: `Tokenizer`
- ไฟล์+บรรทัด: `tokenizer.rs:185`
- ชนิด/visibility: `struct` / `pub`
- นิยามย่อ: โครงข้อมูลแบบมี field ชื่อชัดเจน สำหรับ `Tokenizer`
- หน้าที่: ใช้ในขั้นตอนแยก source code เป็น token
- โครงสร้างภายใน (fields/variants ทั้งหมด):
  - `(private) input: Vec<char>`
  - `(private) current: usize`
  - `(private) line: usize`
  - `(private) column: usize`
  - `(private) keywords: HashMap<String, TokenType>`
  - `(private) operators: HashMap<String, TokenType>`
  - `(private) last_non_comment_lexeme: Option<String>`
  - `(private) interp_stack: Vec<InterpolatedState>`
- ความหมายของแต่ละ field/variant:
  - `input`: ข้อมูล `input` ของ `Tokenizer`
  - `current`: ข้อมูล `current` ของ `Tokenizer`
  - `line`: ข้อมูล `line` ของ `Tokenizer`
  - `column`: ข้อมูล `column` ของ `Tokenizer`
  - `keywords`: ข้อมูล `keywords` ของ `Tokenizer`
  - `operators`: ข้อมูล `operators` ของ `Tokenizer`
  - `last_non_comment_lexeme`: ข้อมูล `last_non_comment_lexeme` ของ `Tokenizer`
  - `interp_stack`: ข้อมูล `interp_stack` ของ `Tokenizer`
- เชื่อมกับ type อื่นอย่างไร:
  - field ใน `Tokenizer` อ้าง type อื่น เช่น `Vec<char>, usize, HashMap<String, TokenType>, Option<String>, Vec<InterpolatedState>`
- ถูกใช้ที่ไหนใน parser/tokenizer: `tokenizer.rs` 2 จุด, `main.rs` 1 จุด
  - ตัวอย่างตำแหน่งอ้างอิง: `tokenizer.rs:185`, `tokenizer.rs:196`, `main.rs:16`
- ตัวอย่างสั้น:
```rust
let mut tokenizer = Tokenizer::new(source);
let tokens: Vec<Token> = tokenizer.tokenize();
```
- ข้อควรระวัง: field หลายตัวเชื่อมกับ parser/tokenizer โดยตรง; ถ้าเปลี่ยนชื่อหรือชนิดต้องไล่จุดใช้งานทั้งหมด

## Appendix: Impl ที่ผูกกับ type โดยตรง
- `ptypes.rs:29` `ParseError (for fmt::Display)` (trait_impl)
  - `fmt` (private) at line 31
- `ptypes.rs:38` `ParseError` (inherent_impl)
  - `pretty` (pub) at line 39
- `tokenizer.rs:81` `Token` (inherent_impl)
  - `new` (pub) at line 82
- `tokenizer.rs:92` `Token (for fmt::Display)` (trait_impl)
  - `fmt` (private) at line 93
- `tokenizer.rs:98` `Token` (inherent_impl)
  - `pretty` (pub) at line 99
- `tokenizer.rs:196` `Tokenizer` (inherent_impl)
  - `new` (pub) at line 197
  - `init_keywords` (private) at line 214
  - `init_operators` (private) at line 380
  - `tokenize` (pub) at line 439
  - `scan_token` (private) at line 484
  - `scan_token_normal` (private) at line 498
  - `line_comment` (private) at line 619
  - `block_comment` (private) at line 631
  - `doc_comment` (private) at line 661
  - `string_literal` (private) at line 673
  - `multiline_string` (private) at line 704
  - `scan_interpolated_string_token` (private) at line 732
  - `scan_interpolated_expr_token` (private) at line 804
  - `number` (private) at line 844
  - `identifier` (private) at line 934
  - `effect_marker` (private) at line 976
  - `attribute` (private) at line 990
  - `operator` (private) at line 1043
  - `matches_string` (private) at line 1073
  - `skip_whitespace` (private) at line 1089
  - `is_at_end` (private) at line 1100
  - `regex_allowed` (private) at line 1104
  - `peek` (private) at line 1111
  - `peek_next` (private) at line 1119
  - `peek_ahead` (private) at line 1127
  - `advance` (private) at line 1135
  - `regex_literal` (private) at line 1151

## QA Checklist
- [x] Type Coverage: source types = `54`
- [x] Type Details sections = `54`
- [x] ครอบคลุมทั้ง `pub` และ `private` และ `type alias`
- [x] มี inventory table + category map + appendix impl
