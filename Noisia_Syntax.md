# Noisia Language Syntax Reference

เอกสารนี้สรุป **ไวยากรณ์ (syntax)** ของภาษา Noisia โดยละเอียด ตั้งแต่พื้นฐานจนถึงฟีเจอร์ขั้นสูง รวมไปถึงนวัตกรรมล่าสุด เพื่อให้นักพัฒนาสามารถอ้างอิงและเขียนโค้ดได้ทันที

---

## 1. Hello World & พื้นฐานการพิมพ์

```nx
module App::Hello

@entry
fn main() -> Int {
    println("Hello, Noisia!")
    return 0
}
```

- `module App::Hello` ประกาศ module
- `@entry` ระบุจุดเริ่มต้นของโปรแกรม
- `fn main() -> Int` ฟังก์ชันหลักคืนค่า `Int`
- `println(...)` พิมพ์ข้อความตามด้วย newline

---

## 2. Module & Import

```nx
module App::Utils::Math

import Core::{List, Option, Result}
import IO::File::{open, readAll}
```

- `module <path>`: แบ่ง namespace ด้วย `::`
- `import <module>::{A, B, C}`: นำเข้า symbols แบบเจาะจงด้วย `{}`

---

## 3. Types & Declarations

### 3.1 Primitive Types

| ชนิด      | คำย่อ      | ตัวอย่าง                     |
| -------- | -------- | -------------------------- |
| Integer  | `Int`    | `let x: Int = 42`          |
| Floating | `Float`  | `let r: Float = 3.14`      |
| Boolean  | `Bool`   | `let b: Bool = true`       |
| String   | `String` | `let s: String = "Noisia"` |

### 3.2 Variable Declaration

```nx
let x: Int = 10        // immutable, เปลี่ยนค่าไม่ได้
let~ y: Int = 20       // mutable, สามารถเปลี่ยนค่าได้

y = y + 5  // ถูกต้อง เพราะ y เป็น mutable
x = 15     // ❌ error เพราะ x เป็น immutable
```

- ใช้ `let` เพื่อประกาศ immutable variable
- ใช้ `let~` เพื่อประกาศ mutable variable

### 3.3 Variable Deletion

- ใช้ `delete <var>` เพื่อลบตัวแปรออกจาก memory และคืน resource

```nx
let~ temp = 100
println(temp)
delete temp   // temp ถูกลบออกจาก memory
println(temp) // ❌ error: temp ไม่ถูกประกาศแล้ว
```

- รองรับ auto-delete ตาม scope โดยใช้ joins เพื่อรวมตัวแปรที่มีอายุอยู่แค่ในบล็อก

```nx
joins (let a = 5; let b = 10; let~ isOpening = true) {
    println(a + b)
    if isOpening { println("Door is open") }
}

// ตัวแปร a, b, isOpening จะถูกลบอัตโนมัติหลังออกจาก scope
println(a)  // ❌ error: a ไม่อยู่ใน scope แล้ว
```

### 3.4 Struct (Product Type)

```nx
struct Point {
    x: Float,
    y: Float,
}
```

- ท้ายฟิลด์ต้องมี comma
- สร้าง instance: `let p = Point { x:1.0, y:2.0 }`

### 3.5 Enum (Sum Type)

```nx
enum Shape {
    Circle { radius: Float },
    Rect { width: Float, height: Float },
    Polygon(Vector<Point>),
}
```

- `{}` สำหรับ named fields, `()` สำหรับ positional

### 3.6 Array Literal

สร้าง Array โดยใช้เครื่องหมาย [...]: `[a, b, c, ...]`

### 3.7 Tuple Literal

สร้าง Tuple โดยใช้เครื่องหมาย (...): `(a, b, c, ...)`

### 3.8 Struct Object Literal

สร้าง instance ของ Struct โดยใช้ชื่อ Struct ตามด้วยเครื่องหมายปีกกาและกำหนดค่า field: `StructName { field1: value1, field2: value2, ... }`

### 3.9 Vector Literal

สร้าง Vector โดยใช้ `v` ตามด้วยเครื่องหมายเหลี่ยม: `v[a, b, c, ...]`
---

## 4. ฟังก์ชัน และ Generic

### 4.1 ฟังก์ชันปกติ

```nx
fn greet(name: String) -> String {
    return "Hello, " + name
}
```

- พารามิเตอร์ระบุชื่อและชนิด
- คืนค่าด้วย `return`

### 4.2 Generic

```nx
fn head<T>(lst: [T]) -> Option<T> {
    match lst {
        Cons(h, _):> Some(h)
        Nil:> None
    }
}
```

- `<T>` ระบุ type parameter
- `Option<T>` เป็น built-in ADT

---

## 5. Control Flow Statements

### 5.1 Basic

```nx
// 1. If / Elif / Else (Expression)
let result = if x > 0 { "positive" }
             elif x < 0 { "negative" }
             else         { "zero" }
```

- คืนค่า (expression) ตามเงื่อนไขที่ตรง
- รองรับหลายชั้นด้วย `elif`
- มี `else` สำรองเมื่อไม่มีเงื่อนไขใดตรง
- เหมาะกับการเขียน logic ที่ประกอบเป็นค่ากลับได้ทันที

```nx
// 3. Match (Pattern Matching Expression)
let desc = match shape {
    Circle{radius: r}:> "Circle r=" + r.toString()
    Rect{width:w, height:h}:> "Rect " + (w*h).toString()
    _:> "Unknown"
}
```

- เป็น expression คืนค่าได้เหมือน `if`
- รองรับการ destructure struct/enum
- มีกรณี fallback ด้วย `_`
- ช่วยแยกกรณีของข้อมูลแบบครบถ้วนและอ่านง่าย

```nx
// 4. Loop (Infinite loop)
loop {
    // รันซ้ำไม่สิ้นสุด จนกว่าจะใช้คำสั่ง break
    if shouldStop { break }
}
```

- สร้าง infinite loop ได้ชัดเจน
- ใช้ `break` เพื่อออกจากลูป, `continue` เพื่อข้ามรอบ
- เหมาะกับ server loop หรือ retry logic

```nx
// 5. While (Conditional loop)
while count < 10 {
    count += 1
}
```

- ตรวจเงื่อนไขก่อนทุกครั้ง
- รันซ้ำจนเงื่อนไขเป็น `false`
- เหมาะกับการนับรอบหรือรอจน state เปลี่ยน

```nx
// 6. For (Iterable loop)
for item in items {
    println(item)
}
```

- รองรับเดินผ่านทุก container ที่ implement `Iterable`
- ลดโอกาสผิดพลาดจากการจัดการ index เอง
- syntax อ่านง่ายและกระชับ

```nx
// 7. For with index (Enumerate)
for i, v in items.enumerate() {
    println("[" + i.toString() + "] = " + v)
}
```

- ใช้งานคู่ index และค่า element พร้อมกัน
- ไม่ต้องสร้างตัวแปร index แยกเอง
- เหมาะกับกรณีที่ต้องการตำแหน่งและค่าพร้อมกัน

### 5.2 Advanced Control Flow

Noisia มีโครงสร้างควบคุมขั้นสูง 12 แบบ สำหรับจัดการสถานการณ์ที่ซับซ้อน

```nx
// 1. Reactive watcher - ตรวจจับการเปลี่ยนแปลงและแนวโน้ม
watch counter, temperature {
  when changed(counter) :> println("counter ถูกปรับค่า!")
  when increasing(temperature) :> turnOnFan()
}
```

- รวมการตรวจจับการเปลี่ยนแปลงและแนวโน้มในบล็อกเดียv
- ลด boilerplate สำหรับ reactive programming
- ใช้ใน monitoring หรือ real-time systems

```nx
// 2. Convergence control - วนซ้ำจนเสถียรหรือเจอ pattern
converge with history x {
  x = calculate(x)
} until stable or pattern(x, [1,2,4,8])
```

- รวม fixed-point iteration กับ pattern matching
- ใช้ได้ทั้ง numerical optimization และ cycle detection
- เหมาะกับ algorithm ปรับปรุงเชิง iterative

```nx
// 3. Contextual branching - แยก flow ตาม state
in context role {
  "guest" :> showLoginPrompt()
  "admin" :> showDashboard()
  "maintenance" :> showBanner()
}
```

- จัดการ flow ตาม context ได้หลากหลาย
- คล้าย `switch-case` แต่แยกบล็อกชัดเจน
- เหมาะกับ state machines หรือ UI management

```nx
// 4. Multi-condition handler - รวม logic เงื่อนไขซับซ้อน
match {
  user.isLoggedIn -> user.hasPermission("admin") :> grantAccess()
  x>10 and y<5 :> handleRange()
  x<0 or isNaN(x) :> handleError()
}
```

- แทน nested if ได้หมด
- รองรับ chaining และ boolean expressions
- ลดความซับซ้อนของเงื่อนไขหลายชั้น

```nx
// 5. Time-bound execution - รันภายในระยะเวลากำหนด
within 5s if server.isOnline() :> connect()
```

- timeout-based conditional execution
- เหมาะกับ network retry หรือ event waiting
- ป้องกัน blocking ที่นานเกินไป

```nx
// 6. Stream processing - ประมวลผล stream อัตโนมัติ
for v in stream :> process(v)  // auto-terminate
```

- จัดการ state หมดอัตโนมัติ
- เหมาะกับ reactive streams หรือ data pipeline
- syntax เรียบง่ายสำหรับงานที่พบบ่อย

```nx
// 7. Atomic execution - transaction ที่ปลอดภัย
atomically {
  if cart.stock >= qty {
    cart.stock -= qty
    commit()
  } else rollback()
}
```

- รวม transaction mechanisms
- ป้องกัน race conditions ใน concurrent programming
- รับประกันความสอดคล้องของข้อมูล

```nx
// 8. Error trapping - จัดการ error แบบ declarative
trap error.code == 500 :> retry()
```

- ฝัง event handler ใน flow
- รัน recovery logic อัตโนมัติ
- ลดความซับซ้อนของ error handling

```nx
// 9. Sequence detector - ตรวจสอบ event sequence
on sequence input.keys = ["↑","↑","↓","↓","←","→","←","→","B","A"]
:> unlockSecret()
```

- ตรวจ sequence ของ events/inputs ตรงกับ pattern
- ใช้ทำ cheat code หรือ complex input sequences
- ทำให้ flow ตอบสนองต่อ patterns ได้ง่าย

```nx
// 10. Guard clause - ตรวจสอบ precondition
guard db.unlocked :> updateData()
else :> rollback()
```

- precondition checking กระชับ
- เป็น transactional flow แบบง่าย ๆ
- รันบล็อกหลักถ้าเงื่อนไขทั้งหมดผ่าน

```nx
// 11. Immediate assertion - ตรวจสอบแบบ zero-cost
panic unless config.valid
```

- ตรวจเงื่อนไขและ abort ทันทีหากไม่ตรง
- จำเป็นสำหรับ critical systems
- คุณสมบัติเหมือนกับการ throw new Error() ใน JavaScript

```nx
// 12. Non-blocking wait - รอแบบ resource-friendly
poll signal.received with interval=100ms :> checkSocket()
```

- หลีกเลี่ยง busy-wait ที่สิ้นเปลือง CPU
- async I/O ที่ resource-friendly
- เหมาะกับ event polling หรือ hardware I/O

### ตัวอย่างการใช้งานจริง

```nx
// ระบบจัดการออร์เดอร์
atomically {
  guard inventory[item] >= quantity :> {
    inventory[item] -= quantity
    commit()
  }
  else :> rollback("Out of stock")
}

// ตรวจสอบระบบ
watch server.load, network.latency {
  when increasing(server.load) :> scaleUp()
  when high(network.latency) :> switchRoute()
}

// ผู้ใช้พิมพ์คีย์ลับ
on sequence user.keystrokes = ["A","B","A","C"]
:> enableDebugMode()
```

---

## 6. Lambda & Pipeline

```nx
let nums = [1, 2, 3, 4, 5]
let result = nums
    |> map( \n :> n * 2 )
    |> filter( \n :> n % 2 == 0 )
```

- Lambda: `\\args :> body`
- Pipeline: `|>` เชื่อมสายการประมวลผล

---

## 7. มาโคร & Compile-Time Metaprogramming

```nx
macro log!(expr) {
    compile-time {
        let code = stringify(expr)
        emit($"println("[LOG] " + code + " = " + ({expr}).toString())")
    }
}

fn demo() {
    log!(2 + 3 * 4)
}
```

- `macro name! { compile-time { ... } }`
- `stringify(expr)`, `emit(...)`

---

## 8. Effect System

```nx
fn fetch(url: String) -> String !Network { /* ... */ }
fn pureAdd(a: Int, b: Int) -> Int { return a + b }
```

- `!State`, `!Temporal`, `!Unsafe`, `!Network` ระบุ effect
- ฟังก์ชันที่ไม่มี `!` ถือ pure

---

## 9. Attributes

```nx
@inline
@test
fn testAdd() {
    assert Eq(pureAdd(2,3), 5)
}
```

- `@...` ใส่หน้า declaration เช่น `@entry`, `@deprecated`, `@test`, `@inline`

---

## 10. Traits & Implementations

```nx
trait Display {
    fn show(self) -> String
}

impl Display for Point {
    fn show(self) -> String {
        return "(" + self.x.toString() + ", " + self.y.toString() + ")"
    }
}
```

- `trait` กำหนด interface
- `impl <Trait> for <Type>`

---

## 11. Concurrency & Async/Await

```nx
async fn fetch(url: String) -> String !Network {
    let resp = await http.get(url)
    return resp.bodyText()
}

fn main() -> Int {
    let task = fetch("https://...")
    spawn task
    let data = await task
    println(data)
    return 0
}
```

- `async fn`, `await <expr>`, `spawn <future>`

---

## 12. Error Handling: Result & Try/Catch

```nx
fn parseInt(s: String) -> Result<Int, String> {
    try {
        return Ok(s.toInt())
    } catch e {
        return Err("Parse error: " + e.message)
    }
}

fn compute() -> Result<Int, String> {
    let x = parseInt("123")?
    let y = parseInt("abc")?
    return Ok(x + y)
}
```

- `try { ... } catch e { ... }`
- `?` propagate `Err`

---

## 13. Destructuring Assignment

```nx
let (a, b, c) = (1, 2, 3)
let Point{x, y} = p
fn dist({x, y}: Point) -> Float { ... }
```

- รองรับ tuple และ struct destructuring

---

## 14. List Comprehensions

```nx
let evens = [ n*2 | n <- [1,2,3,4,5], n % 2 == 0 ]
```

- `[ expr | pattern <- iterable, guard? ]`

---

## 15. Extension Methods

```nx
extension (s: String) {
    fn shout(self): String { return self.toUpperCase() + "!" }
}

println("hi".shout())
```

- `extension (receiver) { fn ... }`

---

## 16. Inline Grammar Macros (IGM)

```nx
igm date! {
    pattern = /\d{4}-\d{2}-\d{2}/
    expand(m: Match) = Date(m.group(0))
}

let d = date!("2025-12-31")
println(d.year)
```

- สร้าง syntax ใหม่ด้วย regex และ expand function

---

## 17. Expression-Only Functions

```nx
fn greet(name: String) :> "Hello, " + name
fn head<T>(lst: [T]) :> match lst {
    Cons(h, _):> Some(h)
    Nil:> None
}
```

- ลด boilerplate ให้ฟังก์ชันสั้นกระชับ
- รองรับ generic

---

## 18. Record Update Syntax แบบ immutable

```nx
let p2 = p with { x: p.x + 1, y: 0 }
```

- อัพเดต struct แบบ immutable

---

## 19. Multi-Dispatch Functions (fn+)

```nx
fn+ area(shape: Circle) -> Float :> 3.14 * shape.radius^2
fn+ area(shape: Rect) -> Float :> shape.width * shape.height
let a = area(Circle{radius:2})
```

- multimethod ในไวยากรณ์เดียว

---

## 20. Built-in Interval & Stride Literal

```nx
let evens = [1..10 by 2]
let down  = [10..1 by -1]
```

- range พร้อม step

---

## 21. Unique Unit-Aware Literals

```nx
let d = 10km + 500m
let t = 2h - 30min
```

- คอมไพล์ไทม์จัดการ unit

---

## ~~22. Inline JSON-like Struct Literal~~

- ~~สร้าง struct เหมือน JSON~~

---

## 23. Enhanced String Interpolation

```nx
let msg = $"User {user.name} has {notifications.count} new messages."
```

- expression ใน `{}`

---

## 24. Selector Pipelines (>>)

```nx
let result = data >> filter(\x :> x.active) >> map(\x :> x.value)
```

- transformation chain

---

## 25. Pattern Matching ขั้นสูง

```nx
match value {
  Some(x)          :> handleSome(x)
  None             :> handleNone()
  Error(code,msg)  :> handleError(code,msg)
}
```

- รองรับ ADT, tuple, nested patterns อย่างครบถ้วน
- คอมไพเลอร์ตรวจสอบ exhaustive coverage

---

## 26. Destructuring Assignment ขั้นสูง

```nx
let (id,name,age) = getUserInfo()
let { x, y, z } = point3D
```

- แยกค่า tuple/record มาเป็นตัวแปรย่อยทันที

---

## ~~27. List/Sequence Comprehensions~~

...

---

## 28. Extension Methods & Traits Syntax

```nx
extension String {
  fn toTitleCase() -> String { self.split(" ").map(capitalize).join(" ") }
}
```

- เพิ่ม method ให้ type ที่มีอยู่โดยไม่แก้ซอร์สหลัก

---

## 29. Optional Chaining / Safe Navigation

```nx
let country = user?.address?.country ?? "Unknown"
```

- เข้าถึงฟิลด์ลึกได้โดยไม่ต้องเช็ค null ทีละขั้น

---

## 30. Range & Step Literals

```nx
for i in 0..10 by 2 { /*...*/ }
let lastFive   = data[5..]
let firstThree = data[..3]
```

- นิพจน์ช่วง, slice ของ array/string

---

## 31. Compile-Time Functions

```nx
constexpr fn factorial(n: Int) -> Int {
  if n<=1 {1} else {n*factorial(n-1)}
}
let fiveFactorial = factorial(5)
```

- คำนวณในช่วง compile-time

---

## 32. Default Value

```nx
fn hello(name: String = "Unknown Guy") -> Void {...}
```

- = ต่อหลัง type เพื่อกำหนด default value สำหรับ parameter

---

## 33. Context Parameters / Implicit Arguments

```nx
fn log(msg: String)(using logger:Logger) {
  logger.write(msg)
}
using ConsoleLogger
log("Hello, Noisia!")
```

- ส่งผ่าน dependency โดยไม่ต้องระบุซ้ำ

---

## 34. Provenance-Aware Pipelines

```nx
@track data |> t1 |> t2
inspect provenance of data at stage 2
```

- ฝัง metadata เก็บประวัติการแปลงข้อมูลในแต่ละขั้นตอน

---

## 35. Self-Tuning Algorithms

```nx
@autotune(maxTime=100ms)
fn sort(items:[Int]){…}
```

- ทดลองอัลกอริทึมหลายแบบ runtime แล้วเลือกอันเหมาะสมสุด

---

## 36. Contextual Micro-Plugins

```nx
plugin imageOptimizer when building webAssets
plugin dbProfiler   when in debugMode
```

- ติดตั้งปลั๊กอินเฉพาะตามบริบทการใช้งาน

---

## 37. Adaptive Security Modules

```nx
@secure(level="high")
fn processPayment(p:Payment){…}
```

- ปรับกลไกความปลอดภัยตามระดับความเสี่ยง

---

## 38. Advanced Pointer & Reference System

Noisia ใช้ระบบ pointer ที่หลากหลายและปลอดภัย

### 38.1 ชนิดของ Pointer

```nx
// Raw pointer (ไม่ปลอดภัย - ใช้เมื่อจำเป็น)
let rawPtr: ~Int = ~&value

// Managed pointer (มีการจัดการหน่วยความจำอัตโนมัติ)
let managedPtr: @Int = @new(42)

// Weak pointer (ไม่ส่งผลต่อการนับ reference)
let weakPtr: &Int = managedPtr.downgrade()

// Shared pointer (เจ้าของหลายคน)
let sharedPtr: +Int = +new(100)
```

- `~` แทน raw pointer ที่ไม่มีการจัดการอัตโนมัติ
- `@` แทน managed pointer ที่มี reference counting
- `&` แทน weak reference ที่ไม่ป้องกัน garbage collection
- `+` แทน shared pointer สำหรับ ownership หลายคน

### 38.2 การ Dereference แบบต่างๆ

```nx
let ptr = @new(42)

// แบบดั้งเดิม
let val1 = *ptr

// แบบ Noisia (ชัดเจนกว่า)
let val2 = ptr->

// แบบปลอดภัย (คืน Option)
let val3 = ptr->?

// แบบ method call
let val4 = ptr.get()
```

- `ptr->` dereference ปกติ
- `ptr->?` ตรวจสอบ null ก่อน dereference
- เหมาะกับการ chain operations

### 38.3 Pointer Arithmetic

```nx
let arr = [1, 2, 3, 4, 5]
let basePtr = ~&arr[0]

// เลื่อน pointer
let ptr2 = basePtr +> 2    // ไปข้างหน้า 2 elements
let ptr3 = ptr2 <+ 1       // ย้อนกลับ 1 element

// คำนวณระยะทาง
let distance = ptr2 <-> basePtr  // คืน 2

// เปรียบเทียบตำแหน่ง
if basePtr <@ ptr2 {
    println("basePtr อยู่ก่อน ptr2 ใน memory")
}
```

- `+>` และ `<+` แสดงทิศทางการเคลื่อนไหวอย่างชัดเจน
- `<->` คำนวณระยะห่างระหว่าง pointers
- `<@` และ `@>` เปรียบเทียบตำแหน่งใน memory

### 38.4 Safe Operations

```nx
// ตรวจสอบความถูกต้อง
if ptr.alive? {
    process(ptr->)
}

// ยืนยันหรือ panic
panic unless ptr.ensure

// การทำความสะอาดชัดเจน
ptr.release()        // สำหรับ managed pointer
sharedPtr.drop()     // ลด reference count
```

- `alive?` ตรวจสอบว่า pointer ยังใช้งานได้
- `ensure` true ถ้า pointer ถูกต้อง
- การจัดการ lifetime แบบชัดเจน

### 38.5 Scoped Management

```nx
scope {
    let ptr = @new(expensiveResource())
    use(ptr->)
    // ทำความสะอาดอัตโนมัติเมื่อออกจาก scope
}

// RAII pattern
fn processFile(path: String) {
    let file = @new(File.open(path)?)
    defer file.close()  // ปิดไฟล์เมื่อจบฟังก์ชัน
    return file->read()
}
```

- `scope` block รับประกันการทำความสะอาด
- `defer` เลื่อนการทำงานไปยังตอนจบฟังก์ชัน
- ป้องกัน memory/resource leaks

### 38.6 Temporal Pointers (Noisia Extension)

```nx
// Pointer พร้อม timestamp
let timedPtr = @new(data) at now()

// ตรวจสอบอายุ
if timedPtr.age() > 5min {
    timedPtr.refresh()
}

// หมดอายุอัตโนมัติ
let cache = @new(data) expires 1h
```

- ติดตาม timestamp ของ pointer
- หมดอายุอัตโนมัติสำหรับ cache
- เหมาะกับ time-sensitive data

### 38.7 Pipeline Integration

```nx
let data = @new(rawData)

// แบบ traditional pipeline
let result = data
    |> ptr-> transform(*)
    |> ptr-> validate(*)

// แบบ pointer-specific pipeline
let result2 = data ~> transform ~> validate ~> serialize
```

- `~>` รวม pointer follow กับ pipeline
- ลดความซับซ้อนใน data transformation
- เหมาะกับ functional programming style

### 38.8 Complete Example: Linked List

```nx
struct Node<T> {
    value: T,
    next: @Node<T>?  // Optional managed pointer
}

impl<T> LinkedList<T> {
    fn push(value: T) {
        let newNode = @new(Node { value, next: self.head })
        self.head = Some(newNode)
    }

    fn pop() -> T? {
        match self.head {
            Some(node) :> {
                self.head = node.next->?  // Safe dereference
                Some(node.value->)        // Get value before cleanup
            }
            None :> None
        }
    }

    // Iterator using safe pointer traversal
    fn iter(self) -> Iterator<T> {
        let~ current = self.head
        Iterator {
            next: || {
                current.take().map(|node| {
                    current = node.next->?
                    node.value->
                })
            }
        }
    }
}
```

- ใช้ managed pointers เพื่อความปลอดภัย
- Safe dereference ป้องกัน null pointer
- Integration กับ Iterator pattern

### 38.9 Effect System Integration

```nx
// Pointers มี effect information
fn unsafeRead(ptr: ~T) -> T !Unsafe {
    return ptr->  // !Unsafe effect แพร่กระจาย
}

fn safeRead(ptr: @T) -> T {
    return ptr.get()  // สำหรับ potential allocation
}

// Async pointer operations
async fn remotePtr(url: String) -> @Data !Network {
    let data = await fetch(url)
    return @new(data)
}
```

- Pointers ส่งผ่าน effect information
- รวมเข้ากับ async/await system
- Type safety รวมถึง side effects

---

## 39. Modern Class & Object System

Noisia มีระบบ OOP ที่ผสมผสานแนวคิด modern กับนวัตกรรมเฉพาะตัว

### 39.1 Class Declaration & Instantiation

```nx
// Class พื้นฐานพร้อม constructor
class Person {
    private name: String,
    age: Int, // = public age: Int,

    // Primary constructor
    init(name: String, age: Int) {
        self.name = name
        self.age = age
    }

    // Secondary constructor
    init.withName(name: String) {
        self.init(name, 0)
    }
}

// การสร้าง instance หลายแบบ
let p1 = Person("Alice", 25)           // Primary constructor
let p2 = Person.withName("Bob")        // Named constructor
let p3 = Person { name: "Carol", age: 30 }  // Struct-like syntax
```

- `private`, `public` กำหนด access level
- _access level แบบ default จะเป็น public_
- `init` สำหรับ primary constructor
- `init.withName` สำหรับ named constructor
- รองรับ struct-like instantiation

### 39.2 Smart Properties & Computed Fields

```nx
class Rectangle {
    width: Float,
    height: Float,

    // Computed property
    area :> width * height

    // Property with getter/setter
    diagonal {
        get :> (width^2 + height^2).sqrt()
        set(value) {
            let ratio = width / height
            width = (value^2 / (1 + ratio^2)).sqrt()
            height = width / ratio
        }
    }

    // Lazy property (คำนวณครั้งแรกที่เรียกใช้)
    @lazy perimeter :> 2 * (width + height)

    // Reactive property (อัพเดทอัตโนมัติ)
    @reactive aspectRatio :> width / height
}
```

- `:>` สำหรับ computed properties
- `get`/`set` blocks สำหรับ custom accessors
- `@lazy` properties คำนวณครั้งแรกที่ใช้
- `@reactive` อัพเดทอัตโนมัติเมื่อ dependencies เปลี่ยน

### 39.3 Method Overloading & Multi-dispatch

```nx
class Calculator {
    // Traditional method
    fn add(a: Int, b: Int) -> Int :> a + b

    // Method overloading with different signatures
    fn+ add(a: Float, b: Float) -> Float :> a + b
    fn+ add(a: String, b: String) -> String :> a + b
    fn+ add(items: [Int]) -> Int :> items.sum()

    // Multi-dispatch based on runtime type
    fn process(data: Any) {
        match Type(data) {
            Int:> processNumber(data)
            String:> processText(data)
            [Any]:> processArray(data)
            _:> processDefault(data)
        }
    }
}
```

- `fn+` สำหรับ method overloading
- `match Type()` สำหรับ runtime type-based routing
- รองรับ pattern matching ใน method match

### 39.4 Inheritance & Virtual Methods

```nx
// Base class
class Animal {
    name: String,

    init(name: String) {
        self.name = name
    }

    // Virtual method (สามารถ override ได้)
    @virtual fn sound() -> String :> "Some sound"

    // Final method (ไม่สามารถ override)
    @final fn getName() -> String :> name

    // Abstract method (ต้อง implement ใน subclass)
    @abstract fn move() -> String
}

// Inheritance
class Dog extends Animal {
    breed: String,

    init(name: String, breed: String) {
        super(name)
        self.breed = breed
    }

    // Override virtual method
    @override fn sound() -> String :> "Woof!"

    // Implement abstract method
    fn move() -> String :> "Running on four legs"

    // Method chaining support
    fn sit() -> Self {
        println(name + " sits down")
        return self
    }

    fn stay() -> Self {
        println(name + " stays")
        return self
    }
}

// Usage with method chaining
let dog = Dog("Buddy", "Golden Retriever")
dog.sit().stay()
```

- `extends` สำหรับ inheritance
- `@virtual`, `@final`, `@abstract` modifiers
- `Self` type สำหรับ method chaining
- `super()` เรียก parent constructor

### 39.5 Mixins & Composition

```nx
// Mixin definition
mixin Flyable {
    altitude: Float = 0.0,

    fn takeOff() {
        altitude = 100.0
        println("Taking off to " + altitude.toString() + "m")
    }

    fn land() {
        altitude = 0.0
        println("Landing")
    }
}

mixin Swimmable {
    depth: Float = 0.0,

    fn dive(targetDepth: Float) {
        depth = targetDepth
        println("Diving to " + depth.toString() + "m deep")
    }
}

// Class with multiple mixins
class Duck extends Animal with Flyable, Swimmable {
    init(name: String) {
        super(name)
    }

    override fn sound() -> String :> "Quack!"
    fn move() -> String :> "Walking, flying, or swimming"

    // Composition pattern with delegation
    delegate swimming to SwimmingBehavior(),
    delegate flying to FlyingBehavior()
}
```

- `mixin` สำหรับ reusable behaviors
- `with` สำหรับ mixin composition
- `delegate` สำหรับ composition pattern

### 39.6 Interfaces & Protocols

```nx
// Interface definition
interface Drawable {
    fn draw()
    fn getBounds() -> Rectangle

    // Default implementation
    fn isVisible() -> Bool :> true
}

// Protocol with associated types
protocol Container<T> {
    type Iterator<T>

    fn add(item: T)
    fn remove(item: T) -> Bool
    fn iter() -> Iterator<T>

    // Protocol extension
    extension {
        fn isEmpty() -> Bool :> self.count() == 0
        fn count() -> Int :> self.iter().count()
    }
}

// Multiple interface implementation
class Canvas implements Drawable, Container<Shape> {
    shapes: [Shape] = [],

    fn draw() {
        for shape in shapes {
            shape.draw()
        }
    }

    fn getBounds() -> Rectangle {
        // Calculate bounding box of all shapes
    }

    fn add(shape: Shape) {
        shapes.append(shape)
    }

    fn remove(shape: Shape) -> Bool {
        return shapes.remove(shape)
    }

    fn iter() -> Iterator<Shape> :> shapes.iter()
}
```

- `interface` สำหรับ behavior contracts
- `protocol` สำหรับ generic interfaces
- `implements` สำหรับ interface implementation
- Protocol extensions ให้ default implementations

### 39.7 Access Control & Visibility

```nx
class BankAccount {
    // Public (default)
    accountNumber: String,

    // Private (accessible only within class)
    private balance: Float,

    // Protected (accessible in subclasses)
    protected transactions: [Transaction],

    // Internal (accessible within module)
    internal auditLog: [String],

    // Package-private (accessible within package)
    package metadata: [String:Any],

    init(accountNumber: String, initialBalance: Float) {
        self.accountNumber = accountNumber
        self.balance = initialBalance
        self.transactions = []
        self.auditLog = []
        self.metadata = [:]
    }

    // Public method
    fn getBalance() -> Float :> balance

    // Private method
    private fn validateAmount(amount: Float) -> Bool {
        return amount > 0 && amount <= balance
    }

    // Friend classes can access private members
    friend class BankManager
}
```

- `private`, `protected`, `internal`, `package` access modifiers
- `friend` classes สำหรับ controlled access
- Default เป็น public

### 39.8 Generic Classes & Constraints

```nx
// Generic class
class Stack<T> {
    private items: [T] = [],

    fn push(item: T) {
        items.append(item)
    }

    fn pop() -> T? {
        return items.popLast()
    }

    fn peek() -> T? {
        return items.last()
    }

    // Generic method with additional constraints
    fn findMax<U>() -> U? {
        return items.max()
    }
}

// Specialized generic class
class PriorityQueue<T> extends Stack<T> {
    @override fn push(item: T) {
        // Insert in priority order
        let pos = items.binarySearch(item)
        items.insert(pos, item)
    }
}
```

- Generic classes
- Generic method overloading

### 39.9 Singleton & Static Members

```nx
class Logger {
    // Static property
    @static instance: Logger? = None

    // Static method
    @static fn getInstance() -> Logger {
        if instance.isNone() {
            instance = Some(Logger())
        }
        return instance.unwrap()
    }

    // Singleton pattern with thread safety
    @threadsafe
    @static fn getSharedLogger() -> Logger {
        @static @lazy sharedInstance = Logger()
        return sharedInstance
    }

    // Static initialization block
    @static init {
        println("Logger class initialized")
    }

    // Instance members
    private logLevel: LogLevel = .Info

    fn log(message: String, level: LogLevel = .Info) {
        if level >= logLevel {
            println($"[{level}] {message}")
        }
    }
}

// Usage
Logger.getInstance().log("Application started")
```

- `@static` members สำหรับ class-level data/methods
- `@threadsafe` สำหรับ thread-safe singletons
- `@static init` สำหรับ class initialization
- `@static @lazy` สำหรับ lazy initialization

### 39.10 Operator Overloading & DSL Support

```nx
class Vector2D {
    x: Float,
    y: Float,

    init(x: Float, y: Float) {
        self.x = x
        self.y = y
    }

    // Arithmetic operators
    @operator +(other: Vector2D) -> Vector2D {
        return Vector2D(x + other.x, y + other.y)
    }

    @operator -(other: Vector2D) -> Vector2D {
        return Vector2D(x - other.x, y - other.y)
    }

    @operator *(scalar: Float) -> Vector2D {
        return Vector2D(x * scalar, y * scalar)
    }

    // Comparison operators
    @operator ==(other: Vector2D) -> Bool {
        return x == other.x && y == other.y
    }

    // Unary operators
    @operator -() -> Vector2D {
        return Vector2D(-x, -y)
    }

    // Custom operators
    @operator dot(other: Vector2D) -> Float {
        return x * other.x + y * other.y
    }

    // DSL support with builder pattern
    fn build(&self) -> VectorBuilder {
        return VectorBuilder(self)
    }
}

// DSL usage
let v = Vector2D(1, 2)
    .build()
    .normalize()
    .scale(2.0)
    .rotate(44.degrees)
    .finish()
```

- `@operator` สำหรับ operator overloading
- Custom operators เช่น `dot`
- Builder pattern สำหรับ DSL
- Method chaining ด้วย `&self`

### 39.11 Memory Management & Resource Control

```nx
class FileManager {
    private fileHandle: @FileHandle

    init(path: String) throws {
        fileHandle = @new(try FileHandle.open(path))
    }

    // RAII destructor
    deinit {
        fileHandle.close()
        println("File closed automatically")
    }

    // Manual resource management
    fn close() {
        fileHandle.close()
        fileHandle.release()
    }

    // Scoped resource usage
    scope fn withFile<T>(path: String, block: (FileManager) -> T) -> T {
        let manager = FileManager(path)
        defer manager.close()
        return block(manager)
    }
}

// Usage with automatic cleanup
FileManager.withFile("data.txt") { manager in
    manager.write("Hello, Noisia!")
}
```

- `deinit` สำหรับ automatic cleanup
- `scope fn` สำหรับ scoped resource management
- `defer` สำหรับ cleanup scheduling

### 39.12 Reflection & Metaprogramming

```nx
class User {
    @serializable
    name: String,

    @validate(min=0, max=150)
    age: Int,

    @deprecated("Use email instead")
    username: String,

    init(name: String, age: Int, username: String) {
        self.name = name
        self.age = age
        self.username = username
    }

    // Compile-time reflection
    comptime fn getFieldNames() -> [String] {
        return Self.fields.map(\.name)
    }

    // Runtime reflection
    fn toJSON() -> String {
        let mirror = Mirror(self)
        let fields = mirror.fields.map { field in
            $"\"{field.name}\": \"{field.value}\""
        }
        return "{ " + fields.join(", ") + " }"
    }
}

// Metaprogramming usage
@derive(Serializable, Comparable, Debug)
class Product {
    name: String,
    price: Float,
    category: String,
}
```

- Attribute-based metaprogramming
- `comptime` สำหรับ compile-time reflection
- `Mirror` สำหรับ runtime reflection
- `@derive` สำหรับ automatic trait implementation

### 39.13 Complete Example: Modern Web Server Class

```nx
@component
class WebServer {
    private host: String = "localhost",
    private port: Int = 8080,
    private routes: [String: Handler] = [:],

    @reactive isRunning: Bool = false,

    @lazy logger: Logger :> Logger.getInstance(),

    init.default() {
        self.init("localhost", 8080)
    }

    init(host: String, port: Int) {
        self.host = host
        self.port = port
    }

    // Fluent API
    fn route(path: String, handler: Handler) -> Self {
        routes[path] = handler
        return self
    }

    fn middleware(mw: Middleware) -> Self {
        // Add middleware
        return self
    }

    // Async operation
    async fn start() !Network {
        logger.log($"Starting server on {host}:{port}")
        isRunning = true

        // Start listening loop
        loop {
            if !isRunning { break }
            await handleRequest()
        }
    }

    fn stop() {
        isRunning = false
        logger.log("Server stopped")
    }

    // Resource cleanup
    deinit {
        if isRunning {
            stop()
        }
    }

    // DSL support
    @static fn configure(&block: (WebServer) -> Void) -> WebServer {
        let server = WebServer.default()
        block(server)
        return server
    }
}

// Usage with DSL
let server = WebServer.configure { server in
    server
        .route("/api/users", UserHandler())
        .route("/api/products", ProductHandler())
        .middleware(AuthMiddleware())
        .middleware(LoggingMiddleware())
}

await server.start()
```

- `@component` สำหรับ dependency injection
- Fluent API ด้วย method chaining
- Async/await integration
- DSL support ด้วย configuration blocks
- Automatic resource management

---

## 40. Core Temporal Operations

### 40.1 Checkpoint Creation

```nx
fn processOrder(order: Order) -> Result<Receipt, Error> {
    // สร้าง checkpoint ชื่อ "validation"
    checkpoint "validation" {
        validateOrder(order)
    }

    // สร้าง checkpoint แบบ automatic labeling
    checkpoint {
        let inventory = checkInventory(order.items)
        deductStock(inventory)
    } as "inventory_update"

    // สร้าง checkpoint พร้อม metadata
    checkpoint "payment" with {
        timestamp: now(),
        user: order.userId,
        amount: order.total
    } {
        processPayment(order.payment)
    }
}
```

- `checkpoint "name" { ... }` สร้าง named checkpoint
- `checkpoint { ... } as "name"` labeling หลังการทำงาน
- `checkpoint "name" with metadata { ... }` เก็บข้อมูลเพิ่มเติม

### 40.2 Rewind Operations

```nx
fn handleError() {
    // Rewind ไปยัง checkpoint ล่าสุด
    rewind

    // Rewind ไปยัง checkpoint ที่ระบุ
    rewind to "validation"

    // Rewind พร้อมเงื่อนไข
    rewind to "inventory_update" if error.code == "INSUFFICIENT_STOCK"

    // Rewind แบบมีขอบเขต (scoped)
    rewind to latest where {
        checkpoint.metadata.user == currentUser.id
    }

    // Conditional rewind expression
    let result = rewind to "payment" else handleFailure()
}
```

- `rewind` ไปยัง checkpoint ล่าสุดใน scope
- `rewind to "name"` ไปยัง checkpoint ที่ระบุ
- `rewind to "name" if condition` rewind แบบมีเงื่อนไข
- `rewind to latest where { ... }` query-based rewind

### 40.3 Inspection & History Querying

```nx
fn debugTransaction() {
    // ตรวจสอบ checkpoint ปัจจุบัน
    inspect current checkpoint

    // ดู history ทั้งหมด
    inspect history

    // Query specific checkpoints
    inspect checkpoints where {
        name.contains("payment") &&
        metadata.amount > 1000.0
    }

    // ตรวจสอบ state ณ เวลาใดเวลาหนึ่ง
    inspect state at "validation" {
        println("Order state: " + order.status)
        println("Inventory: " + inventory.available)
    }

    // Compare states ระหว่าง checkpoints
    inspect diff between "inventory_update" and "payment" {
        showFieldChanges(order)
        showVariableChanges()
    }
}
```

- `inspect current checkpoint` ดูข้อมูล checkpoint ปัจจุบัน
- `inspect history` แสดง execution timeline
- `inspect checkpoints where { ... }` query checkpoints
- `inspect state at "name" { ... }` ดู state ณ checkpoint
- `inspect diff between "A" and "B" { ... }` เปรียบเทียบ states

---

## 41. Advanced Temporal Features

### 41.1 Temporal Scoping & Isolation

```nx
// Temporal scope - การเปลี่ยนแปลงไม่ส่งผลกับ outer scope
temporal scope {
    checkpoint "experiment"

    // ทดลองการเปลี่ยนแปลง
    user.balance -= 1000.0

    if experimentFailed {
        rewind to "experiment"  // ย้อนกลับใน scope
    } else {
        commit  // ยืนยันการเปลี่ยนแปลง
    }
} // หาก commit ไม่ถูกเรียก การเปลี่ยนแปลงจะถูก rollback อัตโนมัติ

// Nested temporal scopes
temporal scope "outer" {
    checkpoint "start"

    temporal scope "inner" {
        checkpoint "nested"
        // ... operations

        if shouldRevert {
            rewind to "nested"  // rewind ใน inner scope
        }
    }

    // ไม่สามารถ rewind ไป "nested" จาก outer scope
}
```

- `temporal scope { ... }` สร้าง isolated temporal context
- `commit` ยืนยันการเปลี่ยนแปลงจาก scope
- Nested scopes มี checkpoint isolation

### 41.2 Branch & Merge Operations

```nx
fn exploreOptions(data: Data) -> BestResult {
    checkpoint "base"

    // สร้าง temporal branches
    let branch1 = branch from "base" {
        let result1 = optimizeMethod1(data)
        checkpoint "method1_done"
        return result1
    }

    let branch2 = branch from "base" {
        let result2 = optimizeMethod2(data)
        checkpoint "method2_done"
        return result2
    }

    // รอผลลัพธ์จากทุก branches
    let results = await [branch1, branch2]

    // เลือก branch ที่ดีที่สุด
    let best = results.maxBy(\.score)

    // Merge กลับไปยัง main timeline
    merge branch best.branch to current

    return best.result
}
```

- `branch from "checkpoint" { ... }` สร้าง temporal branch
- `merge branch <branch> to current` รวม branch กลับ
- Parallel exploration ของ different execution paths

### 41.3 Conditional Temporal Logic

```nx
fn robustTransaction() {
    checkpoint "start"

    // Temporal guard - rewind หากเงื่อนไขไม่ตรง
    guard network.isConnected() else rewind to "start"

    // Temporal retry with automatic checkpointing
    retry with rewind up to 3 times {
        checkpoint "attempt"

        let response = await api.call()

        // หาก timeout, จะ rewind ไป "attempt" อัตโนมัติ
        if response.isTimeout() {
            continue  // จะทำการ rewind และ retry
        }

        return response
    }

    // Time-bounded execution
    within 30s or rewind to "start" {
        complexCalculation()
    }
}
```

- `guard condition else rewind to "name"` conditional checkpoint protection
- `retry with rewind` automatic retry ด้วย temporal rollback
- `within time or rewind` time-bounded execution

---

## 42. Production Temporal Features

### 42.1 Live Snapshot & Rollback

```nx
@production
class OrderService {
    // Production-safe checkpointing
    @checkpoint.production
    fn processOrder(order: Order) -> Result<Receipt, Error> {
        // Lightweight checkpoint สำหรับ production
        snapshot "order_start" {
            order_id: order.id,
            user_id: order.userId,
            timestamp: now()
        }

        try {
            validateOrder(order)
            deductInventory(order.items)
            processPayment(order.payment)

            snapshot "order_completed"
            return Ok(Receipt.from(order))

        } catch error {
            // Production rollback
            rollback to "order_start" with {
                reason: error.message,
                recovery: "full_rollback"
            }

            return Err(error)
        }
    }

    // Hot-swappable rollback
    @admin.endpoint
    fn emergencyRollback(orderId: String) {
        rollback order orderId to latest snapshot where {
            stage == "order_start"
        }
    }
}
```

- `@checkpoint.production` สำหรับ production checkpoints
- `snapshot "name" { metadata }` lightweight checkpointing
- `rollback to "name" with { ... }` production rollback พร้อมเหตุผล

### 42.2 Reproducible Replay System

```nx
// Record execution สำหรับ replay
@record.execution
fn criticalProcess(data: ProcessData) -> Result<Output, Error> {
    checkpoint "input_validation"
    let validated = validateInput(data)

    checkpoint "computation"
    let result = complexComputation(validated)

    checkpoint "output_generation"
    return generateOutput(result)
}

// Replay recorded execution
fn debugIssue(executionId: String) {
    // โหลด recorded execution
    let recording = ExecutionRecording.load(executionId)

    // Replay แบบ step-by-step
    replay recording {
        // หยุดที่แต่ละ checkpoint เพื่อ inspect
        pause at each checkpoint

        // แสดง state ณ จุดนั้น
        on checkpoint "computation" {
            inspect variables
            inspect call_stack
        }

        // Modify และ continue
        on checkpoint "output_generation" {
            // เปลี่ยน input แล้วดูผลลัพธ์
            modify data.parameters = newParams
            continue
        }
    }
}
```

- `@record.execution` record การทำงานสำหรับ replay
- `replay recording { ... }` เล่น execution ซ้ำ
- `pause at each checkpoint` หยุดที่ checkpoint เพื่อ debug

### 42.3 Temporal Observability

```nx
// Temporal metrics และ monitoring
@observable.temporal
class DatabaseConnection {
    fn query(sql: String) -> QueryResult {
        // อัตโนมัติ checkpoint ก่อน critical operations
        auto checkpoint before {
            database_operations,
            network_calls,
            file_writes
        }

        let result = database.execute(sql)

        // Temporal metrics
        emit temporal.metric {
            operation: "database.query",
            duration: checkpoint.elapsed,
            checkpoint_count: checkpoint.count_in_scope,
            rewind_count: rewind.count_in_scope
        }

        return result
    }
}

// Global temporal dashboard
inspect global temporal state {
    // ดู checkpoint statistics
    println("Active checkpoints: " + checkpoints.active.count())
    println("Total rewinds today: " + rewinds.today.count())

    // Top operations with most rewinds
    let problematic = operations
        .filter(\.rewind_count > 10)
        .sortBy(\.rewind_count)
        .reverse()

    for op in problematic {
        println($"{op.name}: {op.rewind_count} rewinds")
    }
}
```

- `@observable.temporal` สำหรับ temporal metrics
- `auto checkpoint before` อัตโนมัติ checkpoint ตาม operation types
- `inspect global temporal state` global temporal dashboard

---

## 43. Temporal Patterns & Best Practices

### 43.1 Temporal Transaction Pattern

```nx
// Pattern สำหรับ complex transactions
temporal transaction {
    name: "user_registration",
    timeout: 60s,
    max_rewinds: 5
} {
    checkpoint "validation" {
        validateUserData(userData)
        validateEmailUnique(userData.email)
    }

    checkpoint "user_creation" {
        let user = createUser(userData)
        sendWelcomeEmail(user)
    }

    checkpoint "activation" {
        activateUser(user)
        logUserActivity(user, "registered")
    }

    // Success commit point
    commit with {
        user_id: user.id,
        registration_time: now(),
        checkpoints_used: checkpoint.count
    }

} catch error {
    // Automatic rollback พร้อม cleanup
    cleanup {
        deletePartialUser()
        cancelPendingEmails()
        logFailure(error)
    }
}
```

- `temporal transaction { config } { ... }` structured temporal transaction
- `commit with metadata` success checkpoint พร้อมข้อมูล
- `cleanup { ... }` automatic cleanup ในกรณี rollback

### 43.2 Temporal Testing Pattern

```nx
@test
fn testComplexWorkflow() {
    temporal test "user_workflow" {
        // Setup checkpoint
        checkpoint "setup" {
            let user = createTestUser()
            let account = createTestAccount()
        }

        // Test normal flow
        branch "happy_path" from "setup" {
            let result = processOrder(validOrder)
            assert(result.isSuccess())
            checkpoint "happy_done"
        }

        // Test error scenarios
        branch "error_path" from "setup" {
            let result = processOrder(invalidOrder)
            assert(result.isError())

            // Verify rollback behavior
            inspect state at "setup" {
                assert(account.balance == original.balance)
            }
        }

        // Test recovery scenarios
        branch "recovery_path" from "setup" {
            // Simulate failure
            simulateNetworkFailure()

            let result = processOrderWithRetry(validOrder)
            assert(result.isSuccess())

            // Verify retry behavior
            assert(rewind.count > 0)
        }
    }
}
```

- `temporal test "name" { ... }` structured temporal testing
- Multiple branches สำหรับ test scenarios
- Built-in assertions สำหรับ temporal behavior

### 43.3 Temporal Debugging Utilities

```nx
// Temporal debugging helpers
debug temporal {
    // Set breakpoints on temporal events
    breakpoint on rewind to "payment"
    breakpoint on checkpoint creation where name.contains("critical")

    // Conditional temporal breakpoints
    breakpoint when {
        checkpoint.count > 10 or
        rewind.count > 5 or
        temporal_scope.depth > 3
    }

    // Temporal call stack
    trace temporal stack

    // Variable value across time
    trace variable user.balance across checkpoints

    // Performance analysis
    analyze temporal performance {
        checkpoint_overhead,
        rewind_cost,
        memory_usage
    }
}

// Custom temporal assertions
fn assertTemporalInvariant() {
    assert temporal invariant {
        // Property ที่ต้องเป็นจริงในทุก checkpoint
        user.balance >= 0.0
    }

    assert temporal progress {
        // ต้องมี progress ใน operation
        checkpoint.count > previous.checkpoint.count
    }
}
```

- `debug temporal { ... }` เครื่องมือ debug เฉพาะ temporal
- `assert temporal invariant` ตรวจสอบ properties across time
- `trace variable ... across checkpoints` ติดตาม variable history

---

## 44. Temporal Integration with Other Language Features

### 44.1 Temporal + Async/Await

```nx
async fn distributedTransaction() !Network {
    checkpoint "distributed_start"

    // Parallel async operations with temporal safety
    let tasks = [
        async { await updateDatabase1() } with checkpoint "db1",
        async { await updateDatabase2() } with checkpoint "db2",
        async { await updateCache() } with checkpoint "cache"
    ]

    try {
        await Promise.all(tasks)
        checkpoint "all_completed"

    } catch error {
        // Distributed rollback
        rewind all tasks to their initial checkpoints

        // หรือ selective rollback
        match error.source {
            "db1" :> rewind task[0] to "db1"
            "db2" :> rewind task[1] to "db2"
            "cache" :> rewind task[2] to "cache"
        }
    }
}
```

- Temporal operations ทำงานร่วมกับ async/await
- `with checkpoint "name"` สำหรับ async tasks
- `rewind all tasks` distributed temporal control

### 44.2 Temporal + Effect System

```nx
fn processWithEffects() !State !Temporal {
    // Temporal effect จะถูกส่งต่อ
    checkpoint "before_io"

    let data = readFile("data.txt")  //
    updateGlobalState(data)          // !State

    if shouldRollback {
        rewind to "before_io"        // !Temporal
    }
}

// Effect handler สำหรับ temporal operations
handle temporal effects in {
    processWithEffects()
} with {
    on rewind(checkpoint_name) :> {
        logRewind(checkpoint_name)
        restoreState(checkpoint_name)
    }

    on checkpoint(name, metadata) :> {
        persistCheckpoint(name, metadata)
    }
}
```

- `!Temporal` effect type
- Temporal operations เป็น trackable effects
- Effect handlers สำหรับ temporal events

### 44.3 Temporal + Pattern Matching

```nx
// Pattern matching กับ temporal state
match temporal state {
    at checkpoint "validation" with errors > 0 :> handleValidationError()
    at checkpoint "payment" with retries > 3 :> escalatePaymentIssue()
    between "start" and "end" with duration > 5min :> optimizePerformance()
    in rewind from "error" to "recovery" :> logRecoveryAttempt()

    // Temporal sequence patterns
    sequence ["validation", "processing", "completion"] :> markSuccessful()
    sequence ending with rewind :> markFailed()
}

// Advanced temporal pattern matching
fn analyzeExecution() {
    match execution history {
        // Pattern: checkpoints ที่มี rewind
        checkpoints matching { name, metadata }
        where name.startsWith("payment")
        and followed_by rewind within 1s
        :> identifyPaymentIssues()

        // Pattern: temporal loops
        checkpoint_sequence that repeats > 5 times
        :> detectInfiniteRetry()
    }
}
```

- `match temporal state` pattern matching สำหรับ temporal conditions
- `sequence` patterns สำหรับ checkpoint sequences
- Advanced history pattern matching

---

## 45. Performance & Optimization

### 45.1 Temporal Performance Tuning

```nx
// Checkpoint optimization
@checkpoint.config {
    strategy: "copy_on_write",      // หรือ "full_copy", "incremental"
    compression: true,
    max_history: 100,
    gc_policy: "lru"
}
fn optimizedFunction() {
    checkpoint "opt1" with config {
        lightweight: true,           // เก็บแค่ essential state
        variables: ["user", "order"] // ระบุตัวแปรที่ต้องการ
    }
}

// Bulk checkpoint operations
batch temporal {
    checkpoint "batch1"
    processItem1()

    checkpoint "batch2"
    processItem2()

    checkpoint "batch3"
    processItem3()

} optimize for {
    memory_usage: "low",
    rewind_speed: "fast"
}
```

- `@checkpoint.config` กำหนด checkpoint strategy
- `lightweight` checkpoints สำหรับ performance
- `batch temporal` สำหรับ bulk operations

### 45.2 Memory Management

```nx
// Temporal memory control
temporal memory {
    max_size: 512MB,
    gc_threshold: 0.8,
    checkpoint_limit: 1000
} {
    // Operations ที่มี memory limit
    heavyComputation()
}

// Selective state preservation
checkpoint "selective" preserve {
    essential_data: user.criticalInfo,
    skip: [large_cache, temporary_buffers]
} {
    processLargeDataset()
}

// Temporal garbage collection
fn cleanupOldCheckpoints() {
    gc temporal where {
        age > 1hour and
        not referenced and
        not tagged as "important"
    }
}
```

- `temporal memory { config }` memory limits
- `preserve { ... }` selective state saving
- `gc temporal` manual cleanup

---

## 46. Built-in methods

ตารางด้านล่างสรุป **built-in methods** ที่สำคัญของหลาย ๆ type ใน Noisia พร้อมตัวอย่างการใช้งาน สัญลักษณ์พารามิเตอร์ และคุณสมบัติพิเศษ (behavior / complexity / notes)

> หมายเหตุ: ตารางนี้รวบรวมเมธอดที่พบบ่อยและมีประโยชน์สำหรับการเขียนโค้ดประจำวัน — หากต้องการสามารถขยายให้ครอบคลุม type ย่อยหรือเวอร์ชันเฉพาะของเมธอดได้

### 46.1 String

| Method (signature)                                            |                    Params | Return     | คำอธิบาย / คุณสมบัติ                                                      |
| ------------------------------------------------------------- | ------------------------: | ---------- | -------------------------------------------------------------------- |
| `len(self)`                                                   |                         — | `Int`      | คืนความยาวของสตริง (O(1) ถ้าเก็บ utf/length cache, O(n) ถ้าเป็น byte-scan) |
| `toUpper(self)`                                               |                         — | `String`   | คืนสตริงตัวพิมพ์ใหญ่ (unicode-aware)                                       |
| `toLower(self)`                                               |                         — | `String`   | คืนสตริงตัวพิมพ์เล็ก (unicode-aware)                                       |
| `slice(self, start: Int, end: Int?)`                          | `start`, `end` (optional) | `String`   | slice แบบ half-open `[start, end)` รองรับ negative index              |
| `split(self, sep: String = " ")`                              |                     `sep` | `[String]` | แยกเป็นลิสต์ (lazy iterator variant available `.splitIter()` )          |
| `replace(self, from: String, to: String, count: Int? = None)` |     `from`, `to`, `count` | `String`   | แทนที่ occurrences (count=null => replace all)                         |
| `startsWith(self, prefix: String)`                            |                  `prefix` | `Bool`     | ตรวจ prefix (O(                                                      | prefix | )) |
| `contains(self, sub: String)`                                 |                     `sub` | `Bool`     | ตรวจ substring (algorithm: fast search / KMP fallback)               |
| `format(self, args: ...)`                                     |                  variadic | `String`   | string interpolation / printf-like helper                            |
| `toInt(self) -> Result<Int,String>`                           |                         — | `Result`   | พยายาม parse เป็น Int, เก็บ error message ถ้า fail                      |

**ตัวอย่าง**
```nx
let s = "Hello, Noisia"
println(s.len())           // 12 (depends on encoding)
println(s.split(",").map(trim))
```

---

### 46.2 Vector (`Vector<T>`)

| Method (signature)                          |              Params | Return     | คำอธิบาย / คุณสมบัติ                                         |
| ------------------------------------------- | ------------------: | ---------- | ------------------------------------------------------- |
| `append(&self, item: T)`                    |              `item` | `Void`     | เพิ่มท้ายลิสต์ (mutable) amortized O(1)                      |
| `pop(&self) -> T?`                          |                   — | `T?`       | เอาตัวท้ายออก คืน optional ถ้าว่าง                           |
| `insert(&self, index: Int, item: T)`        |     `index`, `item` | `Void`     | แทรกที่ตำแหน่ง (O(n))                                       |
| `remove(&self, index: Int) -> T`            |             `index` | `T`        | เอา element ออก (shifts)                                |
| `map(self, f: (T) -> U) -> [U]`             |                 `f` | `[U]`      | functional map (eager) — `.mapIter()` สำหรับ lazy         |
| `filter(self, f: (T) -> Bool) -> [T]`       |                 `f` | `[T]`      | filter, preserve order                                  |
| `enumerate(self) -> Iterator<(Int,T)>`      |                   — | `Iterator` | ให้ index + value pair                                   |
| `sort(&self, cmp: ((T,T)->Int)? = None)`    | optional comparator | `Void`     | in-place sort, stable by default (T: Ord or custom cmp) |
| `slice(self, start: Int, end: Int?) -> [T]` |      `start`, `end` | `[T]`      | slice copy or view (configurable)                       |
| `find(self, predicate) -> Int?`             |         `predicate` | `Int?`     | คืน index แรกที่ match                                     |

**ตัวอย่าง**
```nx
let nums: Vector<Int> = v[5,1,3]
nums.sort()
let doubled = nums.map(\x :> x*2)
```

---

### 46.3 Struct Object (Can be overwriten in struct declaration)

| Method (signature)                                                     |              Params | Return     | คำอธิบาย / คุณสมบัติ         |
| ---------------------------------------------------------------------- | ------------------: | ---------- | ----------------------- |
| `get(self, key: K) -> V?`                                              |               `key` | `V?`       | คืน optional value       |
| `set(&self, key: K, value: V)`                                         |      `key`, `value` | `Void`     | ตั้งค่าที่ key               |
| `has(self, key: K) -> Bool`                                            |               `key` | `Bool`     | ตรวจ key อยู่หรือไม่        |
| `remove(&self, key: K) -> V?`                                          |               `key` | `V?`       | เอาออกและคืนค่าเก่า        |
| `keys(self) -> Iterator<K>`                                            |                   — | `Iterator` | iterator ของ keys       |
| `values(self) -> Iterator<V>`                                          |                   — | `Iterator` | iterator ของ values     |
| `merge(&self, other: Map<K,V>, strategy: MergeStrategy = "overwrite")` | `other`, `strategy` | `Void`     | รวม maps (configurable) |

---

### 46.4 Option / Maybe (`Option<T>`)

| Method (signature)                              |    Params | Return      | คำอธิบาย / คุณสมบัติ                                            |
| ----------------------------------------------- | --------: | ----------- | ---------------------------------------------------------- |
| `isSome(self)`                                  |         — | `Bool`      | true ถ้ามีค่า                                                 |
| `isNone(self)`                                  |         — | `Bool`      | true ถ้าไม่มีค่า                                               |
| `unwrap(self) -> T`                             |         — | `T`         | คืนค่า ถ้า None จะ `panic` (มี `unwrapOr(default)` เป็นทางเลือก) |
| `unwrapOr(self, default: T) -> T`               | `default` | `T`         | คืนค่า default ถ้า None                                       |
| `map(self, f: (T)->U) -> Option<U>`             |       `f` | `Option<U>` | แปลงค่าเมื่อมี                                                 |
| `andThen(self, f: (T)->Option<U>) -> Option<U>` |       `f` | `Option<U>` | chain แบบ monadic                                          |

---

### 46.5 Result / Either (`Result<T,E>`)

| Method (signature)                        | Params | Return        | คำอธิบาย / คุณสมบัติ              |
| ----------------------------------------- | -----: | ------------- | ---------------------------- |
| `isOk(self)` / `isErr(self)`              |      — | `Bool`        | ตรวจสถานะ                    |
| `unwrap(self) -> T`                       |      — | `T`           | คืน Ok value หรือ panic ถ้า Err |
| `unwrapErr(self) -> E`                    |      — | `E`           | คืน Err value                 |
| `map(self, f: \(T)->U) -> Result<U,E>`    |    `f` | `Result<U,E>` | แปลง Ok value                |
| `mapErr(self, f: \(E)->F) -> Result<T,F>` |    `f` | `Result<T,F>` | แปลง Err value               |
| `andThen(self, f: \(T)->Result<U,E>)`     |    `f` | `Result<U,E>` | chain แบบ fallible           |

---

### 46.6 Numeric Types (`Int`, `Float`)

| Method (signature)                |     Params | Return   | คำอธิบาย / คุณสมบัติ                     |
| --------------------------------- | ---------: | -------- | ----------------------------------- |
| `abs(self)`                       |          — | same     | ค่าสัมบูรณ์                             |
| `clamp(self, lo: Self, hi: Self)` | `lo`, `hi` | `Self`   | clamp value ระหว่าง bounds           |
| `toString(self)`                  |          — | `String` | แปลงเป็นสตริง                         |
| `pow(self, exp: Int)`             |      `exp` | `Self`   | ยกกำลัง                               |
| `sqrt(self)` (Float)              |          — | `Float`  | square root (NaN handling ตาม IEEE) |

---

### 46.7 Bool

| Method           | Params | Return   | Notes                                       |
| ---------------- | -----: | -------- | ------------------------------------------- |
| `toggle(&self)`  |      — | `Void`   | เปลี่ยน true <-> false (สำหรับ mutable binding) |
| `toString(self)` |      — | `String` | "true" / "false"                            |

---

### 46.8 Pointer types (managed `@T`, raw `~T`, shared `+T`, weak `&T`)

| Method (signature)           | Params | Return            | คำอธิบาย / คุณสมบัติ                             |
| ---------------------------- | -----: | ----------------- | ------------------------------------------- |
| `get(self) -> T` (for `@T`)  |      — | `T`               | คืนค่าที่ชี้ (อาจ clone/borrow ขึ้นกับ policy)       |
| `->` / `->?` syntactic deref |      — | `T` / `Option<T>` | `ptr->` deref ปกติ, `ptr->?` คืน None ถ้า dead |
| `release(&self)`             |      — | `Void`            | ปลด resource (managed pointers)             |
| `downgrade(self) -> &T`      |      — | `&T`              | สร้าง weak reference                         |
| `alive?(self) -> Bool`       |      — | `Bool`            | ตรวจว่า pointer ยัง valid                     |

**ตัวอย่าง**
```nx
let p = @new(42)
println(p->)       // deref
if p.alive? { /* safe */ }
```

---

### 46.9 File / IO (`FileHandle`, `IO::File`)

| Method (signature)                                                   |         Params | Return   | คำอธิบาย / คุณสมบัติ                    |
| -------------------------------------------------------------------- | -------------: | -------- | ---------------------------------- |
| `open(path: String, mode: String = "r") -> Result<FileHandle,Error>` | `path`, `mode` | `Result` | เปิดไฟล์ (throws/Result)             |
| `readAll(&self) -> Result<String, Error>`                            |              — | `Result` | อ่านทั้งไฟล์ (อาจ streaming variant)   |
| `read(&self, bufSize: Int) -> Result<String, Error>`                 |      `bufSize` | `Result` | อ่านเป็นชิ้น ๆ                         |
| `write(&self, data: String) -> Result<Int, Error>`                   |         `data` | `Result` | เขียนกลับ คืนจำนวนไบต์                  |
| `close(&self)`                                                       |              — | `Void`   | ปิด handle (defer / RAII supported) |

---

### 46.10 DateTime / Temporal (`DateTime`, `Duration`)

| Method                                    |  Params | Return     | Notes                  |
| ----------------------------------------- | ------: | ---------- | ---------------------- |
| `now()` (static)                          |       — | `DateTime` | เวลาปัจจุบัน              |
| `add(self, dur: Duration) -> DateTime`    |   `dur` | `DateTime` | บวกเวลา                |
| `diff(self, other: DateTime) -> Duration` | `other` | `Duration` | หาความต่าง              |
| `format(self, fmt: String) -> String`     |   `fmt` | `String`   | รูปแบบแบบ strftime-like |

---

### 46.11 Iterator / Generator

| Method                  | Params | Return     | Notes              |
| ----------------------- | -----: | ---------- | ------------------ |
| `next(&mut self) -> T?` |      — | `T?`       | คืนค่า next หรือ None |
| `collect(self) -> [T]`  |      — | `[T]`      | เก็บทุกค่าเป็นลิสต์      |
| `map(self, f)`          |    `f` | `Iterator` | lazy map           |
| `filter(self, f)`       |    `f` | `Iterator` | lazy filter        |

---

### 46.12 Future / Task / Promise

| Method          | Params | Return       | Notes                         |
| --------------- | -----: | ------------ | ----------------------------- |
| `await(task)`   |      — | `T`          | รอผลลัพธ์ของ async task         |
| `spawn(task)`   |      — | `TaskHandle` | สร้าง background task          |
| `then(task, f)` |    `f` | `Future`     | chain callback (non-blocking) |
| `cancel(&self)` |      — | `Void`       | ยกเลิก task ถ้า supported       |

---

_เอกสารนี้ครอบคลุมไวยากรณ์หลัก ฟีเจอร์ขั้นสูง และนวัตกรรมล่าสุดของ Noisia เพื่อรองรับการพัฒนาแอปพลิเคชันยุคใหม่อย่างครบวงจร_
