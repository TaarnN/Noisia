async fn ยังไม่รองรับ (รวม async method ใน class)
syntax อ้างว่าใช้ได้: Noisia_Syntax.md (line 471), Noisia_Syntax.md (line 1000), Noisia_Syntax.md (line 1581), Noisia_Syntax.md (line 2141)
parser จริง: start function เช็กแค่ fn parser.rs (line 386), method class ก็เช่นกัน parser.rs (line 749)
หมายเหตุ: รองรับ async { ... } block expression เท่านั้น parser.rs (line 3766)

constexpr fn / comptime fn / scope fn ยังไม่รองรับเป็นรูปแบบประกาศฟังก์ชัน
syntax: Noisia_Syntax.md (line 718), Noisia_Syntax.md (line 1519), Noisia_Syntax.md (line 1482)
parser: function header ยังบังคับ fn ตรง ๆ parser.rs (line 1165)

Generic declaration ของ struct/class/impl<T> ยังไม่รองรับ
syntax: Noisia_Syntax.md (line 947), Noisia_Syntax.md (line 1319), Noisia_Syntax.md (line 952)
parser: struct คาด { หลังชื่อทันที parser.rs (line 1438), class ไม่มี parse generics parser.rs (line 651), impl ไม่รับ header แบบ <T> parser.rs (line 1757)

match { A -> B :> ... } แบบ multi-condition ใน docs ชนกับ pointer-deref ->
syntax: Noisia_Syntax.md (line 275)
parser: -> ถูกจับเป็น postfix pointer deref parser.rs (line 4222) ทำให้ logic ลูกศรใน match แตก แม้มีโค้ดพยายามรองรับ parser.rs (line 3141)

บั๊กสำคัญ: เงื่อนไขแบบ if a < b { ... } (rhs เป็น identifier แล้วตามด้วย {) ถูกตีเป็น struct literal
สาเหตุ: parse primary ของ identifier ถ้าเจอ { ต่อท้ายจะตีเป็น struct literal parser.rs (line 3847)
ผลกระทบ: โค้ดสไตล์นี้พังหลายจุดใน docs (เช่น pointer compare) เว้นใส่วงเล็บ rhs (if a < (b) { ... })

Struct literal shorthand { value, next: ... } ไม่รองรับ
syntax: Noisia_Syntax.md (line 954)
parser บังคับ field: expr parser.rs (line 4518)

ปรับให้ lambda รองรับ `\_ :>` หากไม่มี args (ถ้าปัจจุบันยังไม่รองรับ)

checkpoint.config { ... } แบบ standalone (attribute + block) ไม่รองรับเป็น item
syntax: Noisia_Syntax.md (line 2250)
parser หลัง attributes ต้องตามด้วย top-level keyword item parser.rs (line 512)

auto checkpoint before { a, b, c } ใน docs ไม่ตรง grammar parser
syntax: Noisia_Syntax.md (line 1946)
parser ต้องเป็น block statements (call()/stmt) ไม่ใช่ comma-list identifiers parser.rs (line 2457)

match execution history { ... } (DSL history pattern) ยังไม่รองรับ
syntax: Noisia_Syntax.md (line 2224)
parser ที่รองรับแบบ raw temporal pattern มีเฉพาะ match temporal state parser.rs (line 2810)

Unit literals รองรับจำกัดกว่าเอกสาร
docs ใช้ 512MB, 1hour Noisia_Syntax.md (line 2289), Noisia_Syntax.md (line 2308)
tokenizer รองรับแค่ km,m,cm,mm,h,min,s,ms,deg,rad tokenizer.rs (line 914)

ตัวอย่าง macro interpolation ใน docs มี quoting ที่ parse ไม่ผ่าน
syntax: Noisia_Syntax.md (line 409)
ปัญหา: string ซ้อน quote โดยไม่ escape ทำให้ token stream แตก

ตัวอย่างที่ใช้ placeholder ... / … ไม่ใช่ grammar จริง
เช่น Noisia_Syntax.md (line 731), Noisia_Syntax.md (line 767), Noisia_Syntax.md (line 789)

พฤติกรรม parser เพิ่มจาก docs: auto-delete let ทุกตัวเมื่อจบบล็อก
parser ทำ inject Stmt::Delete อัตโนมัติให้ทุก let ใน block parser.rs (line 1816)
docs พูดเรื่องนี้ในบริบท joins เป็นหลัก Noisia_Syntax.md (line 75)