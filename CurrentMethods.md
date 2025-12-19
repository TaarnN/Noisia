## ตอนนี้มี methods ใน Parser แล้วดังนี้:

### Helpers

1. `peek(&self) -> &Token`  

   **หน้าที่:** ส่งคืน Token ปัจจุบันโดยไม่เลื่อน pointer ของ parser (ใช้สำหรับตรวจสอบ Token ถัดไปโดยไม่ consume มัน)  
   **ตัวอย่างการใช้งาน:** ใน parse_function เพื่อตรวจว่ามี keyword "async" หน้าฟังก์ชันหรือไม่ เช่น `if self.peek().lexeme == "async" { ... }`  
   **เรียกใช้ methods อื่นๆ:** ไม่เรียก (เป็น helper พื้นฐาน)

2. `is_at_end(&self) -> bool`  

   **หน้าที่:** ตรวจสอบว่าถึงจุดสิ้นสุดของ stream Token แล้วหรือไม่ (โดยเช็คว่า Token ถัดไปคือ EOF)  
   **ตัวอย่างการใช้งาน:** ใน parse_program เพื่อหยุด loop การ parse item เมื่อถึง EOF เช่น `while !self.is_at_end() { ... }`  
   **เรียกใช้ methods อื่นๆ:** ไม่เรียก (เป็น helper พื้นฐาน)

3. `advance(&mut self) -> &Token`  

   **หน้าที่:** เลื่อน pointer ไป Token ถัดไปและส่งคืน Token ที่เพิ่ง consume  
   **ตัวอย่างการใช้งาน:** ใน parse_function เพื่อ consume keyword "fn" เช่น `self.advance();` หลังจากตรวจ peek แล้ว  
   **เรียกใช้ methods อื่นๆ:** ไม่เรียก (เป็น helper พื้นฐาน)

4. `expect(&mut self, expected: TokenType) -> ParseResult<Token>`  

   **หน้าที่:** ตรวจสอบและ consume Token ถัดไปว่าตรงกับ TokenType ที่คาดหวัง ถ้าไม่ตรงจะ return error  
   **ตัวอย่างการใช้งาน:** ใน parse_block เพื่อคาดหวัง LeftBrace `{` เช่น `self.expect(TokenType::LeftBrace)?;`  
   **เรียกใช้ methods อื่นๆ:** ไม่เรียก (เป็น helper พื้นฐาน)

5. `expect_nv(&mut self, expected: TokenType, v: &str) -> ParseResult<Token>`  

   **หน้าที่:** คล้าย expect แต่ตรวจทั้ง TokenType และค่า lexeme เฉพาะเจาะจง (เช่น คาดหวัง Operator ที่มีค่า ">")  
   **ตัวอย่างการใช้งาน:** ใน parse_type เพื่อคาดหวัง ">" ใน generics เช่น `self.expect_nv(TokenType::Operator, ">")?;`  
   **เรียกใช้ methods อื่นๆ:** ไม่เรียก (เป็น helper พื้นฐาน)

6. `match_one(&mut self, t: TokenType) -> bool`  

   **หน้าที่:** ตรวจสอบ Token ถัดไปว่าตรงกับ TokenType ที่กำหนด ถ้าตรงจะ consume และ return true มิเช่นนั้น return false โดยไม่ consume  
   **ตัวอย่างการใช้งาน:** ใน parse_function เพื่อตรวจ optional modifier เช่น `if self.match_one(TokenType::Keyword) && self.peek().lexeme == "async" { ... }`  
   **เรียกใช้ methods อื่นๆ:** ไม่เรียก (เป็น helper พื้นฐาน)

7. `match_tnv(&mut self, t: TokenType, ch: &str) -> bool`  

   **หน้าที่:** คล้าย match_one แต่ตรวจทั้ง TokenType และค่า lexeme ถ้าตรงจะ consume และ return true  
   **ตัวอย่างการใช้งาน:** ใน parse_type เพื่อตรวจและ consume "<" ใน generics เช่น `if self.match_tnv(TokenType::Operator, "<") { ... }`  
   **เรียกใช้ methods อื่นๆ:** ไม่เรียก (เป็น helper พื้นฐาน)

---

### Top-level parsers

1. `parse_program(&mut self) -> ParseResult<Program>`  

   **หน้าที่:** Parse โปรแกรมทั้งหมด โดย loop เพื่อเก็บ item ต่างๆ (เช่น function, struct) จนถึง EOF และสร้าง Program node  
   **ตัวอย่างการใช้งาน:** เรียกจาก main parser เพื่อ parse source code ทั้งไฟล์ เช่น `let program = parser.parse_program()?;` จะได้ AST ของโปรแกรมทั้งหมด  
   **เรียกใช้ methods อื่นๆ:** `parse_item`

2. `parse_item(&mut self) -> ParseResult<Item>`  

   **หน้าที่:** Parse top-level item เดียว เช่น module, import, function, struct, enum, class โดยตรวจ keyword เพื่อเลือก parser ที่เหมาะสม  
   **ตัวอย่างการใช้งาน:** ใน parse_program เพื่อ parse แต่ละ item เช่น ถ้า peek เห็น "fn" จะเรียก parse_function และ wrap เป็น Item::Function  
   **เรียกใช้ methods อื่นๆ:** `parse_module_path_string`, `parse_import`, `parse_function`, `parse_struct`, `parse_enum`, `parse_class`, `parse_statement`

3. `parse_import(&mut self) -> ParseResult<(String, Option<Vec<String>>)>`

   **หน้าที่:** Parse import statement โดย parse path ของ module และ optional symbols ที่นำเข้า (ในรูป {a, b})  
   **ตัวอย่างการใช้งาน:** Parse `import Core::{List, Option}` ได้ tuple ("Core", Some(["List", "Option"]))  
   **เรียกใช้ methods อื่นๆ:** `parse_module_path_string`

4. `parse_module_path_string(&mut self) -> ParseResult<String>`  

   **หน้าที่:** Parse module path เช่น "App::Utils::Math" โดยจัดการ ModulePath token หรือ build จาก identifier + "::"  
   **ตัวอย่างการใช้งาน:** ใน module declaration เช่น `module App::Hello` จะ parse ได้ "App::Hello"  
   **เรียกใช้ methods อื่นๆ:** -

5. `parse_function(&mut self) -> ParseResult<FunctionDecl>`

   **หน้าที่:** Parse function declaration รวม attributes, visibility, modifiers, generics, params, return type, effects, where clauses, และ body  
   **ตัวอย่างการใช้งาน:** Parse `fn add(x: Int, y: Int) -> Int { x + y }` ได้ FunctionDecl node กับ params, ret_type, body  

   **เรียกใช้ methods อื่นๆ:** `parse_generics`, `parse_effects`, `parse_where_clauses`, `parse_type`, `parse_pattern`, `parse_block`

6. `parse_class(&mut self) -> ParseResult<ClassDecl>`  

   **หน้าที่:** Parse class declaration รวม name, super class, implements, fields, constructors, methods, static methods  
   **ตัวอย่างการใช้งาน:** Parse `class Point extends Shape implements Display { x: Float; fn show() { ... } }` ได้ ClassDecl node  
   **เรียกใช้ methods อื่นๆ:** `parse_type`, `parse_pattern`, `parse_function`, `parse_block`

7. `parse_struct(&mut self) -> ParseResult<StructDecl>`  

   **หน้าที่:** Parse struct declaration รวม name และ fields (name-type pairs)  
   **ตัวอย่างการใช้งาน:** Parse `struct Point { x: Float, y: Float }` ได้ StructDecl กับ fields vec![("x", TypeRef::Float), ("y", TypeRef::Float)]  
   **เรียกใช้ methods อื่นๆ:** `parse_type`

8. `parse_enum(&mut self) -> ParseResult<EnumDecl>`  

   **หน้าที่:** Parse enum declaration รวม name และ variants (simplified เป็น list ของ string)  
   **ตัวอย่างการใช้งาน:** Parse `enum Shape { Circle(Float), Rect(Float, Float) }` ได้ EnumDecl กับ variants ["Circle", "Rect"] (simplified)  
   **เรียกใช้ methods อื่นๆ:** `parse_type`, `parse_pattern`

9. `parse_block(&mut self) -> ParseResult<Block>`  

   **หน้าที่:** Parse block statement ในรูป { stmts } โดย parse statements จนเจอ }  
   **ตัวอย่างการใช้งาน:** ใน function body เช่น `{ let x = 1; return x; }` ได้ Block กับ stmts vec  
   **เรียกใช้ methods อื่นๆ:** `parse_statement`

---

### Sub-parsers

1. `parse_generics(&mut self) -> ParseResult<Vec<GenericParam>>`  

   **หน้าที่:** Parse generic parameters ในรูป <T: Trait, U> รวม constraints  
   **ตัวอย่างการใช้งาน:** Parse `<T: Display, U>` ได้ vec![GenericParam { name: "T", constraints: [TraitBound("Display")] }, GenericParam { name: "U", constraints: [] }]  
   **เรียกใช้ methods อื่นๆ:** `parse_type_constraints`

2. `parse_effects(&mut self) -> ParseResult<Vec<Effect>>`  

   **หน้าที่:** Parse effect markers เช่น !Network, !State รวม params ถ้ามี  
   **ตัวอย่างการใช้งาน:** Parse `!Network(String)` ได้ vec![Effect { name: "Network", params: Some(vec![TypeRef::String]) }]  
   **เรียกใช้ methods อื่นๆ:** `parse_effect_params`

3. `parse_type_constraints(&mut self) -> ParseResult<Vec<TypeConstraint>>`  

   **หน้าที่:** Parse constraints สำหรับ generic param เช่น : Trait1 + Trait2  
   **ตัวอย่างการใช้งาน:** Parse `: Display + Eq` ได้ vec![TraitBound("Display"), TraitBound("Eq")]  
   **เรียกใช้ methods อื่นๆ:** `parse_type`

4. `parse_effect_params(&mut self) -> ParseResult<Vec<TypeRef>>`  

   **หน้าที่:** Parse parameters สำหรับ effect ในรูป (Type1, Type2)  
   **ตัวอย่างการใช้งาน:** Parse `(String, Int)` ได้ vec![TypeRef::String, TypeRef::Int]  
   **เรียกใช้ methods อื่นๆ:** `parse_type`

5. `parse_where_clauses(&mut self) -> ParseResult<Vec<WhereClause>>`  

   **หน้าที่:** Parse where clauses เช่น where T: Trait, U: Eq  
   **ตัวอย่างการใช้งาน:** Parse `where T: Display` ได้ vec![WhereClause { param_name: "T", constraints: [TraitBound("Display")] }]  
   **เรียกใช้ methods อื่นๆ:** `parse_type_constraints`

6. `parse_type(&mut self) -> ParseResult<TypeRef>`  

   **หน้าที่:** Parse type reference รวม generics, nullable, pointer type เช่น Option<Int>?, @String  
   **ตัวอย่างการใช้งาน:** Parse `Option<Int>?` ได้ TypeRef { name: "Option", generics: [TypeRef::Int], nullable: true, pointer_type: None }  
   **เรียกใช้ methods อื่นๆ:** -

7. `parse_pattern(&mut self) -> ParseResult<Pattern>`  

   **หน้าที่:** Parse pattern สำหรับ matching เช่น identifier, tuple, struct destructuring, or patterns  
   **ตัวอย่างการใช้งาน:** Parse `Point { x, y: z } | None` ได้ PatternKind::Or กับ sub-patterns และ bindings ["x", "z"]  
   **เรียกใช้ methods อื่นๆ:** `parse_primary` (สำหรับ literals)

---

### Statements

1. `parse_statement(&mut self) -> ParseResult<Stmt>`  

   **หน้าที่:** Parse statement เดียว เช่น let, expr, return, if, while, for, match, watch, etc. โดยตรวจ keyword เพื่อเลือก  
   **ตัวอย่างการใช้งาน:** Parse `let x = 1;` ได้ Stmt::Let { pattern: Identifier("x"), mutable: false, typ: None, expr: Some(Literal(1)) }  
   **เรียกใช้ methods อื่นๆ:** `parse_let_stmt`, `parse_expression`, `parse_block`, `parse_pattern`

2. `parse_let_stmt(&mut self) -> ParseResult<Stmt>`  

   **หน้าที่:** Parse let statement รวม mutable (~), type annotation, และ initializer  
   **ตัวอย่างการใช้งาน:** Parse `let~ x: Int = 42;` ได้ Stmt::Let { mutable: true, typ: Some(TypeRef::Int), expr: Some(Literal(42)) }  
   **เรียกใช้ methods อื่นๆ:** `parse_pattern`, `parse_type`, `parse_expression`

---

### Expressions

1. `parse_expression(&mut self, min_prec: u8) -> ParseResult<Expr>`  

   **หน้าที่:** Parse expression ด้วย precedence climbing สำหรับ binary ops โดย recurse สำหรับ sub-expr ที่ prec สูงกว่า  
   **ตัวอย่างการใช้งาน:** Parse `2 + 3 * 4` ได้ Binary { left: Literal(2), op: "+", right: Binary { left: Literal(3), op: "*", right: Literal(4) } }  
   **เรียกใช้ methods อื่นๆ:** `parse_unary_or_primary`, `parse_postfix`

2. `parse_unary_or_primary(&mut self) -> ParseResult<Expr>`  

   **หน้าที่:** Parse unary expression (เช่น -x, !y) หรือ fallback ไป primary ถ้าไม่มี unary op  
   **ตัวอย่างการใช้งาน:** Parse `-x` ได้ Unary { op: "-", rhs: Ident("x") }  
   **เรียกใช้ methods อื่นๆ:** `parse_primary`

3. `parse_primary(&mut self) -> ParseResult<Expr>`  

   **หน้าที่:** Parse primary expression เช่น literal, identifier, grouping ( ), array literal, etc.  
   **ตัวอย่างการใช้งาน:** Parse `"hello"` ได้ Expr::Literal(String("hello"))  
   **เรียกใช้ methods อื่นๆ:** `parse_expression` (สำหรับ grouping), `parse_arguments` (สำหรับ calls ถ้ามี postfix)

4. `parse_postfix(&mut self, left: Expr) -> ParseResult<Expr>`  

   **หน้าที่:** Parse postfix operators บน left expr เช่น .field, [index], (args) สำหรับ method/field/call/index  
   **ตัวอย่างการใช้งาน:** Parse `obj.method(1)` ได้ MethodCall { object: Ident("obj"), method: "method", args: [Literal(1)] }  
   **เรียกใช้ methods อื่นๆ:** `parse_arguments`, `parse_expression` (สำหรับ index)

5. `parse_arguments(&mut self) -> ParseResult<Vec<Expr>>`  

   **หน้าที่:** Parse argument list ในรูป (expr, expr, ...) สำหรับ function/method call  
   **ตัวอย่างการใช้งาน:** Parse `(1, "a")` ได้ vec![Literal(1), Literal("a")]  
   **เรียกใช้ methods อื่นๆ:** `parse_expression`