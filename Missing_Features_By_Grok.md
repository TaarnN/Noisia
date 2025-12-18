### สรุปสิ่งที่ขาดหายไปทั้งหมดใน tokenizer และ parser ของภาษา Noisia (ไม่รวม bugs)

ก่อนอื่น ผมคิดอย่างละเอียดทีละขั้นตอนเพื่อให้แน่ใจว่าสรุปนี้ครอบคลุมและแม่นยำ โดยอ้างอิงจาก syntax reference ใน `Noisia_Syntax.md` เทียบกับ code ใน `tokenizer.rs` และ `parser.rs` ที่มีอยู่. ผมแยก "สิ่งที่ขาดหายไป" (missing features) จาก "bugs" ตามที่คุณขอ. Missing features คือส่วนที่ syntax กำหนดแต่ code ยังไม่มี handling เลย (no parsing/stmt/expr variants, no keywords/ops ใน tokenizer ถ้าจำเป็น).

ผมจัดกลุ่มตาม sections ใน md เพื่อความชัดเจน, และ list เฉพาะที่ขาดจริง (ไม่รวมที่ handle partial เช่น effects มีแล้ว, แต่ miss บาง advanced). โดยรวม, tokenizer ขาด ~10-15% (mostly ops/units สำหรับ advanced), parser ขาด ~40-50% (mostly advanced stmts/exprs และ temporal).

#### 1. **Types & Declarations (Section 3)** (✅)
   - ☑︎ Variable deletion: ไม่มี parsing สำหรับ `delete <var>` (ควร add Stmt::Delete).
   - ☑︎ Auto-delete scope: ไม่มี parsing สำหรับ `joins (let a=5; let b=10; ...) { ... }` (ควร add Stmt::Joins { decls: Vec<Stmt>, body: Block }).
   - ☑︎ Destructuring assignment: ไม่ support full ใน let (เช่น `let (a,b,c)=...` หรือ `let Point{x,y}=p`) – parser ใช้ name: String เท่านั้น (ควร change to Pattern ใน Let).
  
#### 1.5. (เพิ่มเอง) **Array, Vector, Object, Tuple** (✅)
   - เพิ่มใน syntax
   - เพิ่มใน parser

#### 2. **Control Flow Statements (Section 5)**
   - **Basic**: 
     - If let: มี partial แต่ miss nested/full pattern.
     - For with enumerate: มี for แต่ miss `.enumerate()` as built-in (tokenizer มี enumerate as keyword? ไม่, ควร handle as method).
   - **Advanced (จาก 12 แบบ)**:
     - Contextual branching: ไม่มี `in context role { "guest" :> showLogin() ... }` (ควร add Stmt::ContextBranch).
     - Multi-condition handler: ไม่มี `match { cond1 :> handle() ... }` (ควร add Stmt::MultiMatch).
     - Stream processing: ไม่มี special for `for v in stream :> process(v)` (ควร expand For).
     - Sequence detector: ไม่มี `on sequence keys = ["↑","↓"] :> unlock()` (ควร add Stmt::OnSequence).
     - Immediate assertion: ไม่มี `panic unless config.valid` (ควร add Stmt::PanicUnless).
     - (อื่นๆ เช่น reactive watcher, convergence, time-bound, atomic, error trapping, guard, poll – มี partial แล้ว, แต่ miss full integration ในบางตัว เช่น high() ใน watch).

#### 3. **Lambda & Pipeline (Section 6)**
   - Lambda: Tokenizer handle \ ok, แต่ parser miss Expr::Lambda { params, body } สำหรับ `\n :> n*2`.
   - Selector pipelines: มี |> as op, แต่ miss `>>` special (ควร handle as binary op แต่ add precedence).

#### 4. **Macros & Compile-Time Metaprogramming (Section 7)**
   - Full macro: Tokenizer มี keywords (macro, compile-time, stringify, emit), แต่ parser miss Item::Macro { name, body } สำหรับ `macro log! { compile-time { ... } }`.
   - Compile-time functions: Partial (constexpr as keyword), แต่ miss full constexpr fn parsing (ควร add modifier ใน FunctionDecl).

#### 5. **Inline Grammar Macros (IGM) (Section 16)**
   - ไม่มี parsing สำหรับ `igm date! { pattern = /\d{4}-\d{2}-\d{2}/ expand(m) = ... }` (tokenizer miss / for regex – fall to Unknown; ควร add Item::IGM { name, pattern: String, expand: Expr }).

#### 6. **Error Handling (Section 12)**
   - Try/catch: ไม่มี Stmt::TryCatch { try_block, catch_var, catch_block } สำหรับ `try { ... } catch e { ... }`.
   - ? propagate: Tokenizer มี ? ok, แต่ parser miss postfix ? in expr (ควร add in parse_postfix as Expr::Propagate).

#### 7. **Destructuring Assignment (Section 13)**
   - Advanced destructuring: Miss optional fields เช่น `let { x, y:coordY, z? } = point3D` (pattern มีแต่ miss ?).

#### 8. **List/Sequence Comprehensions (Section 14 & 27)**
   - ไม่มี Expr::ListComp สำหรับ `[ n*2 | n <- [1..5], n % 2 == 0 ]` หรือ `[ for x in nums if cond yield x ]` (tokenizer miss <- as op? ควร add).

#### 9. **Extension Methods (Section 15 & 28)**
   - ไม่มี Item::Extension { receiver: TypeRef, methods: Vec<FunctionDecl> } สำหรับ `extension (s: String) { fn shout() ... }`.

#### 10. **Optional Chaining (Section 29)**
   - Tokenizer มี .? ok, แต่ parser miss handling in postfix (ควร add Expr::OptionalChain { base, field } สำหรับ `user?.address?`).

#### 11. **Range & Step Literals (Section 20 & 30)**
   - ไม่มี Expr::Range { start, end, step } สำหรับ `[1..10 by 2]` หรือ slices `data[5..]` (tokenizer miss by as keyword? ควร add op).

#### 12. **Unique Unit-Aware Literals (Section 21)**
   - Tokenizer handle units limited (km, m, etc.), แต่ miss temporal units เต็ม (h, min, s, ms – แต่คุณบอกไม่รวม bugs, ถ้า count as missing expand list).

#### 13. **Inline JSON-like Struct Literal (Section 22)**
   - ไม่ support string keys ใน struct lit เช่น `MyConfig { "host": "localhost" }` (parser struct fields as idents เท่านั้น; ควร add Expr::StructLit with expr keys).

#### 14. **Record Update Syntax (Section 18)**
   - ไม่มี Expr::RecordUpdate { base, updates } สำหรับ `p2 = p with { x: p.x + 1 }`.

#### 15. **Multi-Dispatch Functions (Section 19)**
   - มี fn+ as modifier ok, แต่ miss full dispatch logic (ถ้า syntax ต้องการ separate bodies, ควร add in Item::Function group).

#### 16. **Context Parameters / Implicit Arguments (Section 33)**
   - ไม่มี handling สำหรับ `(using logger:Logger)` ใน params หรือ `using ConsoleLogger` (ควร add implicit_params ใน FunctionDecl).

#### 17. **Provenance-Aware Pipelines (Section 34)**
   - ไม่มี @track หรือ inspect provenance (ควร add attribute + Stmt::InspectProvenance).

#### 18. **Self-Tuning Algorithms (Section 35)**
   - ไม่มี @autotune (tokenizer มี @ as attr ok, แต่ miss parsing config like maxTime=100ms).

#### 19. **Contextual Micro-Plugins (Section 36)**
   - ไม่มี `plugin name when condition` (ควร add Item::Plugin).

#### 20. **Adaptive Security Modules (Section 37)**
   - ไม่มี @secure(level="high") full (attr ok, แต่ miss level parsing).

#### 21. **Advanced Pointer & Reference System (Section 38)**
   - TypeRef handle pointers ok, แต่ miss expr ops เต็ม เช่น managed @new(42), downgrade() (ควร add Expr::NewPointer, etc.).

#### 22. **Temporal Programming Features (Section 44-45)**
   - Miss เยอะ: Stmt::Checkpoint { name, config }, Stmt::Rewind { to }, Stmt::Inspect { temporal state }, temporal match (`match temporal { checkpoints matching { ... } :> ... }`), sequence patterns (`sequence ["a","b"] :> ...`), batch temporal { ... } optimize for { ... }, temporal memory { max_size: ... } { ... }, selective preserve { ... }, gc temporal where { ... }.
   - Miss performance tuning full เช่น @checkpoint.config { strategy: "copy_on_write" }.

#### 23. **Built-in Methods (Section 46)**
   - Parser ไม่ handle calls เฉพาะ (แต่ general Call/MethodCall ok), ถ้าต้อง special validation สำหรับ built-ins เช่น String.toUpper – no need แต่ถ้า syntax ต้องการ, add.