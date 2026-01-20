# Missing Features

#### 1. Hello World & พื้นฐานการพิมพ์
- **ครบถ้วน**: รองรับ `module`, `@entry` (via attributes in function), `fn main() -> Int { println("Hello, Noisia!") return 0 }`.
- **ไม่ขาด**: Parser มี `parse_module`, `parse_function` (incl. attributes, ret_type, body with Expr and Return stmt). `println` เป็น call expr ซึ่ง parse ได้ via `parse_expression` (Call).

#### 2. Module & Import
- **ครบถ้วน**: `module App::Hello`, `import Core::{List, Option}`.
- **ไม่ขาด**: `parse_module_path_string` รองรับ path ด้วย `::`, `parse_import` รองรับ symbols ด้วย `{}`.

#### 3. Types & Declarations
- **ครบถ้วน**: Primitives (Int, Float, etc. via TypeRef), `let x: Int = 10`, `let~ y: Int = 20` (mutable via Let stmt with mutable flag), `delete temp` (Delete stmt with expr).
- **~~ขาดบางส่วน~~**:
  - ~~`joins (let a=5; let b=10; let~ isOpening=true) { body }`: มี `Joins` stmt แต่ decls เป็น Vec<Stmt> (ซึ่ง Stmt รวม Let) ดังนั้นรองรับ decls ที่เป็น let statements แต่ไม่เช็คว่า decls ต้องเป็น let เท่านั้น (ตาม md เป็น joins for decls), และไม่ parse parentheses รอบ decls (ใน parse_joins คาดว่า parse decls ตรงๆ ไม่มี `(` ).~~
- **ไม่ขาด**: Auto-delete ตาม scope เป็น semantics ไม่ใช่ syntax.

#### 4. Struct (Product Type), Enum (Sum Type), Array, Tuple, Struct Literal, Vector Literal
- **ครบถ้วน**: Struct `struct Point { x: Float, y: Float, }` (parse_struct รองรับ fields with type, comma-terminated), Enum simplified `enum Shape { Circle, Rect }`, Array `[a,b,c]`, Tuple `(a,b,c)`, Struct literal `Point { x:1, y:2 }` (in parse_primary, if ident + { fields }), Vector `v[a,b,c]` (in parse_primary, if "v" + [elements]).
- **ขาดบางส่วน**:
  - Enum variants: md มี `Circle { radius: Float }` (named fields), `Rect {..}`, `Polygon(Vector<Point>)` (positional). แต่ parse_enum เป็น simplified variants เพียง String (no fields or positional/named distinction). ขาด parse variants with {fields} or (types).
  - ไม่มี parse สำหรับ enum with generics หรือ complex inner types (แต่ TypeRef รองรับ generics).

#### 5. ฟังก์ชัน และ Generic
- **ครบถ้วน**: `fn greet(name: String) -> String {..}`, generics `fn head<T>(..)` (parse_generics with constraints).
- **ไม่ขาด**: รองรับ params with pattern:type=default, ret_type, body.

#### 6. Control Flow Statements (Basic & Advanced)
- **ครบถ้วน**: If/elif/else (parse_if with else), Match (parse_match with arms, guard), Loop (ไม่มี explicit แต่ While true {} สามารถแทน), While, For `for pat in iter {..}`.
- **ขาดบางส่วน**:
  - Basic: `loop {..}` ขาด (ไม่มี Stmt::Loop, มี While แต่ไม่ใช่ infinite โดย default).
  - Advanced: 
    - `watch counter, temperature { when changed(counter) :> .. }`: มี Watch stmt with variables (Vec<Expr>) and clauses (cond body), แต่ clauses เป็น `when cond :> body`? Parser มี WatchClause {condition: Expr, body: Block}, แต่ไม่ parse "when" keyword หรือ :> (อาจ assume cond เป็น changed(..) แต่ไม่ explicit).
    - `converge with history x {..} until stable or pattern(..)`: มี Converge {variable, body, until}, แต่ขาด "with history" (no field for it).
    - `in context role { "guest" :> .. }`: ขาด Stmt สำหรับ contextual branching (คล้าย match แต่ string-based).
    - `match { user.isLoggedIn -> user.hasPermission :> .. }`: parse_match เป็น match expr { arms }, ไม่ใช่ match { multi-cond :> body } (ขาด variant นี้).
    - `within 5s if server.isOnline() :> connect()`: มี Within {time: Expr, condition?, body}, แต่ md มี :> สำหรับ expr body? Parser มี body as Block (ไม่ explicit :>).
    - `for v in stream :> process(v)`: มี For แต่ body เป็น Block, ไม่ใช่ :> expr.
    - `atomically {..}`: ครบ.
    - `trap error.code == 500 :> retry()`: ครบ (Trap {error_condition, body}).
    - `on sequence input.keys = ["↑","↑",..] :> unlockSecret()`: ขาด Stmt::OnSequence (ไม่มี parse สำหรับ on sequence).
    - `guard db.unlocked :> updateData() else :> rollback()`: มี Guard {cond, then, else}, แต่ md มี :> สำหรับ body (parser มี Block, อาจไม่ parse :> เป็น shorthand).
    - `panic unless config.valid`: ขาด Stmt::Panic (unless เป็น not cond).
    - `poll signal.received with interval=100ms :> checkSocket()`: มี Poll {signal, interval, body}, แต่ md มี with interval= (parser อาจ assume parse exprs ตรงๆ).

#### 7. Lambda & Pipeline
- **ครบถ้วน**: -.

#### 8. มาโคร & Compile-Time Metaprogramming
- **ขาดทั้งหมด**: ไม่มี Item::Macro, parse_macro (keyword "macro", compile-time { stringify, emit }).

#### 9. Effect System
- **ครบถ้วน**: `fn .. -> String !Network {..}` (parse_effects with name(params?)).

#### 10. Attributes
- **ครบถ้วน**: `@inline @test fn ..` (attributes Vec<String> in Function/Class etc.).

#### 11. Traits & Implementations
- **~~ครบถ้วน~~ ขาดทั้งหมด**: `trait Display { fn show.. }`, `impl Display for Point { fn show.. }`.

#### 12. Concurrency & Async/Await
- **ครบถ้วน**: `async fn` (modifiers include Async, is_async flag).
- **ขาดบางส่วน**:
  - `await http.get(url)`: Expr ไม่มี Await (แต่ Call ได้, ถ้า await เป็น prefix unary? แต่ไม่มี).
  - `spawn task`: Stmt/Expr ไม่มี Spawn.

#### 13. Error Handling: Result & Try/Catch
- **ขาดทั้งหมด**: Stmt ไม่มี TryCatch, และ operator `?` ไม่มีใน parse_expression (no propagate Err).

#### 14. Destructuring Assignment
- **ครบถ้วน**: `let (a,b,c)=(1,2,3)` (pattern in Let support Tuple), `let Point{x,y}=p` (Struct pattern).

#### 15. List Comprehensions
- **ขาดทั้งหมด**: `[ n*2 | n <- [1..], n%2==0 ]` (ไม่มี Expr::ListComp).

#### 16. Extension Methods
- **ขาดทั้งหมด**: `extension (s: String) { fn shout.. }` (ไม่มี Item::Extension).

#### 17. Inline Grammar Macros (IGM)
- **ขาดทั้งหมด**: `igm date! { pattern=/../ expand=.. }` (ไม่มี parse_igm).

#### 18. Expression-Only Functions
- **ครบถ้วน**: `fn greet.. :> "Hello"` (in parse_function, if match ShortArrow, create Block with Return).

#### 19. Record Update Syntax
- **ขาดทั้งหมด**: `p with { x: p.x+1 }` (ไม่มี Expr::WithUpdate).

#### 20. Multi-Dispatch Functions (fn+)
- **ครบถ้วน**: `fn+ area(shape: Circle)..` (modifiers include Multidispatch via "fn+").

#### 21. Built-in Interval & Stride Literal
- **ขาดทั้งหมด**: `[1..10 by 2]` (Literal ไม่มี Range/Stride).

#### 22. Unique Unit-Aware Literals
- **ครบถ้วน**: `10km` (Literal::Unit {v,u}).

#### 23. Inline JSON-like Struct Literal
- **ครบถ้วน**: `MyConfig { "host": "localhost", .. }` (Struct literal support string fields, ถ้า fields เป็น (String, Expr)).

#### 24. Selector Pipelines (>>)
- **ขาดทั้งหมด**: `data >> filter.. >> map..` (op_precedence ไม่มี ">>").

#### 25. Pattern Matching ขั้นสูง
- **ครบถ้วน**: match with ADT, tuple, nested (Pattern support nested Struct/Tuple/Variants/Or).

#### 26. Destructuring Assignment ขั้นสูง
- **ครบถ้วน**: เหมือน 14, Pattern รองรับ nested.

#### 27. List/Sequence Comprehensions
- **ขาดทั้งหมด**: `[ for x in 1..10 yield x*x ]` (variant อีกแบบ, ไม่มี parse).

#### 28. Extension Methods & Traits Syntax
- **ขาดทั้งหมด**: เหมือน 16.

#### 29. Optional Chaining / Safe Navigation
- **ขาดทั้งหมด**: `user?.address?.country ?? "Unknown"` (op_precedence ไม่มี "?.", "??").

#### 30. Range & Step Literals
- **ขาดทั้งหมด**: `0..10 by 2`, slices `data[5..]` (Index มีแต่ [expr], ไม่มี .. range).

#### 31. Compile-Time Functions
- **ขาดทั้งหมด**: `constexpr fn ..` (ไม่มี modifiers สำหรับ constexpr).

#### 32. Default Value
- **ครบถ้วน**: `param: String = "Unknown"` (Param have default: Option<Expr>).

#### 33. Context Parameters / Implicit Arguments
- **ขาดทั้งหมด**: `(using logger:Logger)`, `using ConsoleLogger` (ไม่มี parse สำหรับ using clauses).

#### 34-45. Provenance, Self-Tuning, Plugins, Security, Pointers, Temporal Features, etc.
- **ครบถ้วน**: Pointers `~Int, @Int, &Int, +Int` (TypeRef have pointer_type), บาง temporal เช่น checkpoint (แต่ Stmt ไม่มี Checkpoint, มี Atomically/Converge/Within แต่ขาดส่วนใหญ่).
- **ขาดเกือบทั้งหมด**:
  - 34: `@track data |> t1`, `inspect provenance..` (ขาด @track, provenance syntax).
  - 35: `@autotune(maxTime=..)` (attributes มีแต่ไม่ parse params like (maxTime=)).
  - 36: `plugin .. when ..` (ขาด Stmt/Item::Plugin).
  - 37: `@secure(level="high")` (attributes มีแต่ไม่ parse params).
  - 38: Pointer deref `ptr->` (op_precedence มี "->" แต่ postfix ไม่ handle -> as deref).
  - 39: (เพิ่มเอง) Class
  - 40-45: Temporal (checkpoint "name", rewind to "label", temporal {..}, @checkpoint.config {strategy:..}, batch temporal {..}, temporal memory {..}, gc temporal, match temporal state, sequence patterns, etc.) - ขาด Stmt::Checkpoint, Stmt::Rewind, Stmt::TemporalBlock, parse configs, batch, memory limits, gc, temporal match variants.

**ข้อเสนอแนะ**: Parser ควรเพิ่ม Stmt/Expr/Item ใหม่สำหรับ features ขาด, ขยาย Pattern/Enum parsing ให้ครบ variants, และเพิ่ม operators ใน precedence table. ถ้าต้องการ implement ต่อ, เริ่มจาก macro/extension ก่อนเพราะพื้นฐาน.