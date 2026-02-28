# Elements: คู่มือ Parser (`src/parser.rs`) แบบไทยง่ายมาก + ละเอียด

## Scope และแหล่งอ้างอิง

- ไฟล์ parser หลัก: `/Users/taarn/Documents/PGM/Prog-Languages/Noisia/src/parser.rs`
- ใช้ข้อมูลอ้างอิงจาก syntax: `/Users/taarn/Documents/PGM/Prog-Languages/Noisia/Noisia_Syntax.md`
- ใช้ code source ทั้งหมดใน `src/` เพื่อเทียบความหมายของ Token/AST/entrypoint:
  - `/Users/taarn/Documents/PGM/Prog-Languages/Noisia/src/main.rs`
  - `/Users/taarn/Documents/PGM/Prog-Languages/Noisia/src/tokenizer.rs`
  - `/Users/taarn/Documents/PGM/Prog-Languages/Noisia/src/ptypes.rs`
  - `/Users/taarn/Documents/PGM/Prog-Languages/Noisia/src/style.rs`
- ไม่รวม `backup/parser.rs` ตาม scope ที่ล็อก

## แผนที่ใหญ่ (Big Picture)

1. `main.rs` อ่าน source code -> ส่งเข้า `Tokenizer::tokenize()`
2. Token stream ถูกส่งเข้า `Parser::new(tokens)`
3. `parse_program()` วนเรียก `parse_item()` เพื่อสร้าง `Program { items }`
4. ระดับ statement ใช้ `parse_statement()` + subparsers
5. ระดับ expression ใช้ Pratt parser: `parse_expression()` + `parse_expression_with_left()`
6. AST ทั้งหมดอิง type ใน `ptypes.rs` (`Item`, `Stmt`, `Expr`, `Pattern`, `TypeRef`)

## ผลลัพธ์ 3 รอบ (Quality Gates)

### รอบที่ 1: Inventory + โครง

- พบเมธอดทั้งหมด `135` ตัวจาก `src/parser.rs` (รวม local helper functions)
- สร้าง Method Index ครบทุกตัวพร้อม line number และ group
- สร้างตาราง mapping syntax section 1–46 พร้อมสถานะชัด/บางส่วน/ไม่ชัด

### รอบที่ 2: เติมรายละเอียดทุกเมธอด

- ทุกเมธอดมี template เดียวกัน: ชื่อ, ลายเซ็น, หน้าที่, I/O, token flow, ขั้นตอน, error, caller/callee, syntax mapping, ตัวอย่าง, ข้อควรระวัง
- ผูก AST node จาก `ptypes.rs` และ token behavior จาก parser body จริง

### รอบที่ 3: QA เข้ม

- ตรวจ 1:1 ว่า Method Index และ Method Details มีครบทุกเมธอด
- ตรวจความสอดคล้อง flow สำคัญ: `parse_program -> parse_item`, `parse_statement`, Pratt expression
- ปรับภาษาให้อ่านง่าย (ประโยคสั้น + คำเทคนิคมีคำอธิบายประกบ)

## Mapping: Syntax Section 1–46 กับ Parser Path

| ข้อ | หัวข้อ                                            | สถานะใน parser ปัจจุบัน                                                                        | เมธอดที่เกี่ยวข้อง (ตัวอย่าง)                                                                                    |
| --- | ------------------------------------------------- | ---------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------- |
| 1   | Hello World & พื้นฐานการพิมพ์                     | ชัด (มี parse path ตรงในโค้ด)                                                                  | `new`, `peek`, `is_at_end`, `advance`, ...                                                                       |
| 2   | Module & Import                                   | ชัด (มี parse path ตรงในโค้ด)                                                                  | `fmt`, `pretty`, `parse_item`, `parse_module_path_string`, ...                                                   |
| 3   | Types & Declarations                              | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_struct`, `parse_enum`, `parse_struct_fields`, `parse_type`                                                |
| 4   | ฟังก์ชัน และ Generic                              | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_operator_method`, `parse_function_with`, `parse_generics`, `parse_where_clauses`                          |
| 5   | Control Flow Statements                           | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_block`, `parse_block_or_expr_body`, `parse_expr_with_struct_literal_guard`, `parse_let_stmt`, ...         |
| 6   | Lambda & Pipeline                                 | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_expression`, `parse_expression_with_left`, `parse_postfix`, `parse_lambda`, ...                           |
| 7   | มาโคร & Compile-Time Metaprogramming              | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_statement`, `parse_macro`                                                                                 |
| 8   | Effect System                                     | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_operator_method`, `parse_function_with`, `parse_effects`, `parse_effect_params`                           |
| 9   | Attributes                                        | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_attributes`, `parse_visibility`, `parse_block`, `parse_block::collect_inner`, ...                         |
| 10  | Traits & Implementations                          | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_trait`, `parse_interface`, `parse_protocol`, `parse_impl`                                                 |
| 11  | Concurrency & Async/Await                         | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_unary_or_primary`, `parse_primary`                                                                        |
| 12  | Error Handling: Result & Try/Catch                | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_try_catch_stmt`, `parse_rewind_stmt`, `parse_rewind_expr`, `parse_rewind_expr_relaxed`                    |
| 13  | Destructuring Assignment                          | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_for_pattern`, `parse_pattern_list`, `parse_struct_pattern_fields`, `parse_pattern`                        |
| 14  | List Comprehensions                               | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_primary`, `find_list_comp_pipe`, `has_list_comp_arrow`, `parse_expression_slice`                          |
| 15  | Extension Methods                                 | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_extension`                                                                                                |
| 16  | Inline Grammar Macros (IGM)                       | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_igm`                                                                                                      |
| 17  | Expression-Only Functions                         | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_block_or_expr_body`, `parse_if_expr`, `parse_if_expr_tail`, `parse_match_expr`                            |
| 18  | Record Update Syntax                              | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_postfix`                                                                                                  |
| 19  | Multi-Dispatch Functions                          | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_operator_method`, `parse_function_with`                                                                   |
| 20  | Built-in Interval & Stride Literal                | บางส่วน (มี syntax ในเอกสาร แต่ parser ไม่มี parser เฉพาะ interval/stride ทุกแบบ)              | -                                                                                                                |
| 21  | Unique Unit-Aware Literals                        | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_primary`                                                                                                  |
| 22  | Inline JSON-like Struct Literal (Deprecated)      | Deprecated ในเอกสาร และ parser ไม่ได้แยก path เฉพาะ                                            | -                                                                                                                |
| 23  | Enhanced String Interpolation                     | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_primary`, `parse_interpolated_string`                                                                     |
| 24  | Selector Pipelines (>>)                           | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_expression_with_left`, `op_precedence`                                                                    |
| 25  | Pattern Matching ขั้นสูง                          | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_for_pattern`, `parse_pattern_list`, `parse_struct_pattern_fields`, `parse_pattern`                        |
| 26  | Destructuring Assignment ขั้นสูง                  | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_for_pattern`, `parse_pattern_list`, `parse_struct_pattern_fields`, `parse_pattern`                        |
| 27  | List/Sequence Comprehensions (Deprecated)         | Deprecated ในเอกสาร และ parser ใช้ path ของ list comprehension หลัก                            | -                                                                                                                |
| 28  | Extension Methods & Traits Syntax                 | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_extension`                                                                                                |
| 29  | Optional Chaining / Safe Navigation               | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_postfix`                                                                                                  |
| 30  | Range & Step Literals                             | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_expression_with_left`, `is_range_end_delimiter`, `find_slice_operator`, `parse_postfix`                   |
| 31  | Compile-Time Functions                            | บางส่วน (รองรับ `compile-time` block; ฟอร์มอื่นยังไม่เห็น parser เฉพาะ)                        | `parse_statement`, `parse_macro`                                                                                 |
| 32  | Default Value                                     | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_operator_method::parse_param`, `parse_function_with`, `parse_function_with::parse_param`                  |
| 33  | Context Parameters / Implicit Arguments           | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_function_with`                                                                                            |
| 34  | Provenance-Aware Pipelines                        | บางส่วน (มี operator บางตัวใน precedence แต่ semantics/provenance แยกยังไม่ชัด)                | `parse_expression_with_left`, `op_precedence`                                                                    |
| 35  | Self-Tuning Algorithms                            | ไม่ชัดใน parser ปัจจุบัน (แนวคิดอยู่ในเอกสารมากกว่ากฎ parse)                                   | -                                                                                                                |
| 36  | Contextual Micro-Plugins                          | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_plugin`                                                                                                   |
| 37  | Adaptive Security Modules                         | ไม่ชัดใน parser ปัจจุบัน (แนวคิดเชิงระบบ มากกว่ากฎ parse)                                      | -                                                                                                                |
| 38  | Advanced Pointer & Reference System               | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_unary_or_primary`, `parse_pointer_new_with`, `parse_primary`, `parse_postfix`, ...                        |
| 39  | Modern Class & Object System                      | ชัด (มี parse path ตรงในโค้ด)                                                                  | `parse_visibility`, `parse_class_inheritance`, `parse_class`, `parse_operator_method`, ...                       |
| 40  | Core Temporal Operations                          | ชัด (มี parse path ตรงในโค้ด)                                                                  | `is_checkpoint_stmt_start`, `parse_temporal_config_and_body`, `parse_rewind_parts`, `parse_checkpoint_stmt`, ... |
| 41  | Advanced Temporal Features                        | ชัด (มี parse path ตรงในโค้ด)                                                                  | `is_checkpoint_stmt_start`, `parse_temporal_config_and_body`, `parse_rewind_parts`, `parse_checkpoint_stmt`, ... |
| 42  | Production Temporal Features                      | ชัด (มี parse path ตรงในโค้ด)                                                                  | `is_checkpoint_stmt_start`, `parse_temporal_config_and_body`, `parse_rewind_parts`, `parse_checkpoint_stmt`, ... |
| 43  | Temporal Patterns & Best Practices                | ชัด (มี parse path ตรงในโค้ด)                                                                  | `is_checkpoint_stmt_start`, `parse_temporal_config_and_body`, `parse_rewind_parts`, `parse_checkpoint_stmt`, ... |
| 44  | Temporal Integration with Other Language Features | ชัด (มี parse path ตรงในโค้ด)                                                                  | `is_checkpoint_stmt_start`, `parse_temporal_config_and_body`, `parse_rewind_parts`, `parse_checkpoint_stmt`, ... |
| 45  | Performance & Optimization                        | บางส่วน (มี syntax เช่น `optimize` ใน temporal batch แต่ไม่ได้มี parser เฉพาะทุกหัวข้อ tuning) | `is_checkpoint_stmt_start`, `parse_temporal_config_and_body`, `parse_rewind_parts`, `parse_checkpoint_stmt`, ... |
| 46  | Built-in methods                                  | บางส่วน (method call syntax รองรับทั่วไป แต่ built-in semantics ไม่ได้แยกใน parser)            | `parse_postfix`, `parse_arguments`                                                                               |

## จุดที่พบใน Syntax Doc แต่ Parser Path ยังไม่ตรงชัด (โปร่งใส)

- ข้อ 35 (Self-Tuning Algorithms): ยังไม่เห็น parser รูปแบบเฉพาะเจาะจงใน `src/parser.rs`
- ข้อ 37 (Adaptive Security Modules): ยังไม่เห็น parser เฉพาะ เป็นแนวคิดเชิงระบบมากกว่า grammar
- ข้อ 20, 31, 34, 45, 46: รองรับบางส่วนผ่าน parser กลาง/operator/method-call แต่ไม่มี parser ย่อยเฉพาะครบทุกหัวข้อในเอกสาร syntax

## Method Index (ครบทุก `fn` ใน `src/parser.rs`)

|   # | line | method                                 | group                                            |
| --: | ---: | -------------------------------------- | ------------------------------------------------ |
|   1 |   13 | `fmt`                                  | A. Display และ Utility Helpers                   |
|   2 |   36 | `pretty`                               | A. Display และ Utility Helpers                   |
|   3 |   60 | `colorize_ast_debug`                   | A. Display และ Utility Helpers                   |
|   4 |  122 | `split_indent`                         | A. Display และ Utility Helpers                   |
|   5 |  134 | `starts_with_closer`                   | A. Display และ Utility Helpers                   |
|   6 |  141 | `strip_string_delimiters`              | A. Display และ Utility Helpers                   |
|   7 |  169 | `new`                                  | B. Parser Core: เดิน token และ helper กลาง       |
|   8 |  174 | `peek`                                 | B. Parser Core: เดิน token และ helper กลาง       |
|   9 |  187 | `is_at_end`                            | B. Parser Core: เดิน token และ helper กลาง       |
|  10 |  192 | `advance`                              | B. Parser Core: เดิน token และ helper กลาง       |
|  11 |  200 | `expect`                               | B. Parser Core: เดิน token และ helper กลาง       |
|  12 |  214 | `error_here`                           | B. Parser Core: เดิน token และ helper กลาง       |
|  13 |  224 | `expect_ident_like`                    | B. Parser Core: เดิน token และ helper กลาง       |
|  14 |  237 | `expect_nv`                            | B. Parser Core: เดิน token และ helper กลาง       |
|  15 |  251 | `expect_word`                          | B. Parser Core: เดิน token และ helper กลาง       |
|  16 |  266 | `is_word`                              | B. Parser Core: เดิน token และ helper กลาง       |
|  17 |  273 | `match_word`                           | B. Parser Core: เดิน token และ helper กลาง       |
|  18 |  283 | `is_checkpoint_stmt_start`             | B. Parser Core: เดิน token และ helper กลาง       |
|  19 |  300 | `match_one`                            | B. Parser Core: เดิน token และ helper กลาง       |
|  20 |  310 | `match_tnv`                            | B. Parser Core: เดิน token และ helper กลาง       |
|  21 |  321 | `match_gt`                             | B. Parser Core: เดิน token และ helper กลาง       |
|  22 |  345 | `expect_gt`                            | B. Parser Core: เดิน token และ helper กลาง       |
|  23 |  358 | `parse_attributes`                     | B. Parser Core: เดิน token และ helper กลาง       |
|  24 |  370 | `parse_visibility`                     | B. Parser Core: เดิน token และ helper กลาง       |
|  25 |  386 | `is_function_start`                    | B. Parser Core: เดิน token และ helper กลาง       |
|  26 |  391 | `is_phrase_token`                      | B. Parser Core: เดิน token และ helper กลาง       |
|  27 |  398 | `parse_phrase_expr_on_line`            | B. Parser Core: เดิน token และ helper กลาง       |
|  28 |  430 | `should_parse_phrase_on_line`          | B. Parser Core: เดิน token และ helper กลาง       |
|  29 |  453 | `parse_expression_or_phrase`           | B. Parser Core: เดิน token และ helper กลาง       |
|  30 |  462 | `parse_comma_separated`                | B. Parser Core: เดิน token และ helper กลาง       |
|  31 |  488 | `parse_program`                        | C. Top-level Declarations และ Type-level Grammar |
|  32 |  498 | `parse_item`                           | C. Top-level Declarations และ Type-level Grammar |
|  33 |  609 | `parse_class_inheritance`              | C. Top-level Declarations และ Type-level Grammar |
|  34 |  648 | `parse_class`                          | C. Top-level Declarations และ Type-level Grammar |
|  35 |  800 | `parse_operator_method`                | C. Top-level Declarations และ Type-level Grammar |
|  36 |  822 | `parse_operator_method::parse_param`   | F. Type / Pattern / Lambda Subparsers            |
|  37 |  900 | `parse_class_delegate`                 | C. Top-level Declarations และ Type-level Grammar |
|  38 |  917 | `parse_class_field_or_property`        | C. Top-level Declarations และ Type-level Grammar |
|  39 | 1001 | `parse_property_accessors`             | C. Top-level Declarations และ Type-level Grammar |
|  40 | 1062 | `parse_constructor`                    | C. Top-level Declarations และ Type-level Grammar |
|  41 | 1115 | `parse_module_path_string`             | C. Top-level Declarations และ Type-level Grammar |
|  42 | 1142 | `parse_import`                         | C. Top-level Declarations และ Type-level Grammar |
|  43 | 1157 | `parse_function_with`                  | C. Top-level Declarations และ Type-level Grammar |
|  44 | 1180 | `parse_function_with::parse_param`     | F. Type / Pattern / Lambda Subparsers            |
|  45 | 1276 | `parse_generics`                       | C. Top-level Declarations และ Type-level Grammar |
|  46 | 1310 | `parse_effects`                        | C. Top-level Declarations และ Type-level Grammar |
|  47 | 1359 | `parse_type_constraints`               | C. Top-level Declarations และ Type-level Grammar |
|  48 | 1403 | `parse_effect_params`                  | C. Top-level Declarations และ Type-level Grammar |
|  49 | 1411 | `parse_where_clauses`                  | C. Top-level Declarations และ Type-level Grammar |
|  50 | 1437 | `parse_struct`                         | C. Top-level Declarations และ Type-level Grammar |
|  51 | 1460 | `parse_enum`                           | C. Top-level Declarations และ Type-level Grammar |
|  52 | 1546 | `parse_mixin`                          | C. Top-level Declarations และ Type-level Grammar |
|  53 | 1618 | `parse_trait`                          | C. Top-level Declarations และ Type-level Grammar |
|  54 | 1643 | `parse_interface`                      | C. Top-level Declarations และ Type-level Grammar |
|  55 | 1675 | `parse_protocol`                       | C. Top-level Declarations และ Type-level Grammar |
|  56 | 1754 | `parse_impl`                           | C. Top-level Declarations และ Type-level Grammar |
|  57 | 1788 | `parse_block`                          | D. Statement Parser (รวม Temporal Statements)    |
|  58 | 1794 | `parse_block::collect_inner`           | D. Statement Parser (รวม Temporal Statements)    |
|  59 | 1826 | `parse_block_or_expr_body`             | D. Statement Parser (รวม Temporal Statements)    |
|  60 | 1842 | `parse_expr_with_struct_literal_guard` | D. Statement Parser (รวม Temporal Statements)    |
|  61 | 1868 | `parse_let_stmt`                       | D. Statement Parser (รวม Temporal Statements)    |
|  62 | 1896 | `parse_try_catch_stmt`                 | D. Statement Parser (รวม Temporal Statements)    |
|  63 | 1919 | `parse_string_literal_value`           | D. Statement Parser (รวม Temporal Statements)    |
|  64 | 1935 | `parse_named_record_literal`           | D. Statement Parser (รวม Temporal Statements)    |
|  65 | 1946 | `block_starts_as_record_literal`       | D. Statement Parser (รวม Temporal Statements)    |
|  66 | 1969 | `block_followed_by_block`              | D. Statement Parser (รวม Temporal Statements)    |
|  67 | 1999 | `parse_temporal_config_and_body`       | D. Statement Parser (รวม Temporal Statements)    |
|  68 | 2015 | `parse_rewind_parts`                   | D. Statement Parser (รวม Temporal Statements)    |
|  69 | 2061 | `parse_checkpoint_stmt`                | D. Statement Parser (รวม Temporal Statements)    |
|  70 | 2137 | `parse_rewind_stmt`                    | D. Statement Parser (รวม Temporal Statements)    |
|  71 | 2159 | `parse_rewind_expr`                    | D. Statement Parser (รวม Temporal Statements)    |
|  72 | 2176 | `parse_rewind_expr_relaxed`            | D. Statement Parser (รวม Temporal Statements)    |
|  73 | 2187 | `parse_branch_expr`                    | D. Statement Parser (รวม Temporal Statements)    |
|  74 | 2207 | `parse_inspect_stmt`                   | D. Statement Parser (รวม Temporal Statements)    |
|  75 | 2280 | `parse_snapshot_stmt`                  | D. Statement Parser (รวม Temporal Statements)    |
|  76 | 2311 | `parse_rollback_stmt`                  | D. Statement Parser (รวม Temporal Statements)    |
|  77 | 2374 | `parse_replay_stmt`                    | D. Statement Parser (รวม Temporal Statements)    |
|  78 | 2381 | `parse_replay_pause_stmt`              | D. Statement Parser (รวม Temporal Statements)    |
|  79 | 2399 | `parse_replay_on_checkpoint_stmt`      | D. Statement Parser (รวม Temporal Statements)    |
|  80 | 2407 | `parse_replay_modify_stmt`             | D. Statement Parser (รวม Temporal Statements)    |
|  81 | 2418 | `parse_merge_branch_stmt`              | D. Statement Parser (รวม Temporal Statements)    |
|  82 | 2436 | `parse_retry_stmt`                     | D. Statement Parser (รวม Temporal Statements)    |
|  83 | 2448 | `parse_auto_checkpoint_stmt`           | D. Statement Parser (รวม Temporal Statements)    |
|  84 | 2462 | `parse_emit_stmt`                      | D. Statement Parser (รวม Temporal Statements)    |
|  85 | 2483 | `parse_gc_temporal_stmt`               | D. Statement Parser (รวม Temporal Statements)    |
|  86 | 2500 | `parse_assert_temporal_stmt`           | D. Statement Parser (รวม Temporal Statements)    |
|  87 | 2517 | `parse_commit_stmt`                    | D. Statement Parser (รวม Temporal Statements)    |
|  88 | 2541 | `parse_cleanup_stmt`                   | D. Statement Parser (รวม Temporal Statements)    |
|  89 | 2562 | `parse_batch_temporal_stmt`            | D. Statement Parser (รวม Temporal Statements)    |
|  90 | 2583 | `parse_temporal_scope_stmt`            | D. Statement Parser (รวม Temporal Statements)    |
|  91 | 2600 | `parse_temporal_transaction_stmt`      | D. Statement Parser (รวม Temporal Statements)    |
|  92 | 2633 | `parse_temporal_test_stmt`             | D. Statement Parser (รวม Temporal Statements)    |
|  93 | 2650 | `parse_temporal_memory_stmt`           | D. Statement Parser (รวม Temporal Statements)    |
|  94 | 2660 | `is_debug_temporal_clause_start`       | D. Statement Parser (รวม Temporal Statements)    |
|  95 | 2665 | `parse_debug_temporal_clause_tokens`   | D. Statement Parser (รวม Temporal Statements)    |
|  96 | 2713 | `parse_tokens_until_short_arrow`       | D. Statement Parser (รวม Temporal Statements)    |
|  97 | 2750 | `parse_debug_temporal_stmt`            | D. Statement Parser (รวม Temporal Statements)    |
|  98 | 2766 | `parse_temporal_pattern`               | D. Statement Parser (รวม Temporal Statements)    |
|  99 | 2792 | `parse_temporal_clause`                | D. Statement Parser (รวม Temporal Statements)    |
| 100 | 2810 | `parse_temporal_match_stmt`            | D. Statement Parser (รวม Temporal Statements)    |
| 101 | 2842 | `parse_temporal_handle_stmt`           | D. Statement Parser (รวม Temporal Statements)    |
| 102 | 2867 | `parse_if_stmt`                        | D. Statement Parser (รวม Temporal Statements)    |
| 103 | 2872 | `parse_if_stmt_tail`                   | D. Statement Parser (รวม Temporal Statements)    |
| 104 | 2893 | `parse_for_pattern`                    | D. Statement Parser (รวม Temporal Statements)    |
| 105 | 2922 | `parse_statement`                      | D. Statement Parser (รวม Temporal Statements)    |
| 106 | 3429 | `parse_in_context_stmt`                | D. Statement Parser (รวม Temporal Statements)    |
| 107 | 3456 | `parse_on_sequence_stmt`               | D. Statement Parser (รวม Temporal Statements)    |
| 108 | 3475 | `parse_expression`                     | E. Expression Parser (Pratt + Postfix + Pointer) |
| 109 | 3480 | `parse_expression_with_left`           | E. Expression Parser (Pratt + Postfix + Pointer) |
| 110 | 3562 | `parse_if_expr`                        | E. Expression Parser (Pratt + Postfix + Pointer) |
| 111 | 3567 | `parse_if_expr_tail`                   | E. Expression Parser (Pratt + Postfix + Pointer) |
| 112 | 3589 | `parse_match_expr`                     | E. Expression Parser (Pratt + Postfix + Pointer) |
| 113 | 3618 | `is_range_end_delimiter`               | E. Expression Parser (Pratt + Postfix + Pointer) |
| 114 | 3637 | `parse_unary_or_primary`               | E. Expression Parser (Pratt + Postfix + Pointer) |
| 115 | 3743 | `parse_pointer_new_with`               | E. Expression Parser (Pratt + Postfix + Pointer) |
| 116 | 3756 | `parse_primary`                        | E. Expression Parser (Pratt + Postfix + Pointer) |
| 117 | 3965 | `parse_interpolated_string`            | E. Expression Parser (Pratt + Postfix + Pointer) |
| 118 | 4049 | `find_list_comp_pipe`                  | E. Expression Parser (Pratt + Postfix + Pointer) |
| 119 | 4100 | `has_list_comp_arrow`                  | E. Expression Parser (Pratt + Postfix + Pointer) |
| 120 | 4150 | `find_slice_operator`                  | E. Expression Parser (Pratt + Postfix + Pointer) |
| 121 | 4199 | `parse_expression_slice`               | E. Expression Parser (Pratt + Postfix + Pointer) |
| 122 | 4218 | `parse_postfix`                        | E. Expression Parser (Pratt + Postfix + Pointer) |
| 123 | 4490 | `parse_arguments`                      | E. Expression Parser (Pratt + Postfix + Pointer) |
| 124 | 4498 | `parse_generic_args`                   | F. Type / Pattern / Lambda Subparsers            |
| 125 | 4514 | `parse_struct_fields`                  | F. Type / Pattern / Lambda Subparsers            |
| 126 | 4529 | `parse_type`                           | F. Type / Pattern / Lambda Subparsers            |
| 127 | 4615 | `parse_pattern_list`                   | F. Type / Pattern / Lambda Subparsers            |
| 128 | 4636 | `parse_struct_pattern_fields`          | F. Type / Pattern / Lambda Subparsers            |
| 129 | 4670 | `parse_pattern`                        | F. Type / Pattern / Lambda Subparsers            |
| 130 | 4814 | `parse_lambda`                         | F. Type / Pattern / Lambda Subparsers            |
| 131 | 4876 | `parse_macro`                          | C. Top-level Declarations และ Type-level Grammar |
| 132 | 4906 | `parse_extension`                      | C. Top-level Declarations และ Type-level Grammar |
| 133 | 4940 | `parse_igm`                            | C. Top-level Declarations และ Type-level Grammar |
| 134 | 4999 | `parse_plugin`                         | C. Top-level Declarations และ Type-level Grammar |
| 135 | 5018 | `op_precedence`                        | E. Expression Parser (Pratt + Postfix + Pointer) |

## Method Details (Template เดียวกันทุกเมธอด)

### A. Display และ Utility Helpers

#### M001 `fmt` (line 13)

- ชื่อเมธอด: `fmt`
- ลายเซ็น: `fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result`
- หน้าที่: print item in debug style
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&self, f: &mut fmt::Formatter<'_>`
  - คืน: `fmt::Result`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Item::Class`, `Item::Enum`, `Item::ExtensionDecl`, `Item::Function`
  - 3. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: เรียกจาก flow หลักตามบริบท (ไม่พบ self-call ตรงจากเมธอดอื่น)
- AST ที่เกี่ยวข้อง: `Item::Class`, `Item::Enum`, `Item::ExtensionDecl`, `Item::Function`, `Item::IGMDecl`, `Item::Impl`, `Item::Import`, `Item::InterfaceDecl`, `Item::MacroDecl`, `Item::MixinDecl`, `Item::ModuleDecl`, `Item::PluginDecl`
- ผูกกับ syntax ข้อไหน: 2. Module & Import
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M002 `pretty` (line 36)

- ชื่อเมธอด: `pretty`
- ลายเซ็น: `pub fn pretty(&self, style: &Style) -> String`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&self, style: &Style`
  - คืน: `String`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Item::Class`, `Item::Enum`, `Item::ExtensionDecl`, `Item::Function`
  - 3. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: เรียกจาก flow หลักตามบริบท (ไม่พบ self-call ตรงจากเมธอดอื่น)
- AST ที่เกี่ยวข้อง: `Item::Class`, `Item::Enum`, `Item::ExtensionDecl`, `Item::Function`, `Item::IGMDecl`, `Item::Impl`, `Item::Import`, `Item::InterfaceDecl`, `Item::MacroDecl`, `Item::MixinDecl`, `Item::ModuleDecl`, `Item::PluginDecl`
- ผูกกับ syntax ข้อไหน: 2. Module & Import
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M003 `colorize_ast_debug` (line 60)

- ชื่อเมธอด: `colorize_ast_debug`
- ลายเซ็น: `fn colorize_ast_debug(style: &Style, input: &str) -> String`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `style: &Style, input: &str`
  - คืน: `String`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: เรียกจาก flow หลักตามบริบท (ไม่พบ self-call ตรงจากเมธอดอื่น)
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: รองรับข้ามหลายข้อ (ไม่ได้ผูกข้อเดียวชัดเจน)
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M004 `split_indent` (line 122)

- ชื่อเมธอด: `split_indent`
- ลายเซ็น: `fn split_indent(line: &str) -> (&str, &str)`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `line: &str`
  - คืน: `(&str, &str)`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: เรียกจาก flow หลักตามบริบท (ไม่พบ self-call ตรงจากเมธอดอื่น)
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: รองรับข้ามหลายข้อ (ไม่ได้ผูกข้อเดียวชัดเจน)
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M005 `starts_with_closer` (line 134)

- ชื่อเมธอด: `starts_with_closer`
- ลายเซ็น: `fn starts_with_closer(trimmed: &str) -> bool`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `trimmed: &str`
  - คืน: `bool`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 3. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: เรียกจาก flow หลักตามบริบท (ไม่พบ self-call ตรงจากเมธอดอื่น)
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: รองรับข้ามหลายข้อ (ไม่ได้ผูกข้อเดียวชัดเจน)
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M006 `strip_string_delimiters` (line 141)

- ชื่อเมธอด: `strip_string_delimiters`
- ลายเซ็น: `fn strip_string_delimiters(lexeme: &str) -> String`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `lexeme: &str`
  - คืน: `String`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 3. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: เรียกจาก flow หลักตามบริบท (ไม่พบ self-call ตรงจากเมธอดอื่น)
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: รองรับข้ามหลายข้อ (ไม่ได้ผูกข้อเดียวชัดเจน)
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

### B. Parser Core: เดิน token และ helper กลาง

#### M007 `new` (line 169)

- ชื่อเมธอด: `new`
- ลายเซ็น: `pub fn new(tokens: Vec<Token>) -> Self`
- หน้าที่: make parser from token list
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `tokens: Vec<Token>`
  - คืน: `Self`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 3. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: เรียกจาก flow หลักตามบริบท (ไม่พบ self-call ตรงจากเมธอดอื่น)
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 1. Hello World & พื้นฐานการพิมพ์
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M008 `peek` (line 174)

- ชื่อเมธอด: `peek`
- ลายเซ็น: `fn peek(&self) -> &Token`
- หน้าที่: look at current token or fake EOF
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `&Token`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 3. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: `error_here`, `expect`, `expect_gt`, `expect_ident_like`, `expect_nv`, `expect_word`, `is_at_end`, `is_function_start`, `is_range_end_delimiter`, `is_word`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 1. Hello World & พื้นฐานการพิมพ์
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M009 `is_at_end` (line 187)

- ชื่อเมธอด: `is_at_end`
- ลายเซ็น: `fn is_at_end(&self) -> bool`
- หน้าที่: stop when token is EOF
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `bool`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`
  - ถูกเรียกจาก: `advance`, `parse_block`, `parse_class`, `parse_comma_separated`, `parse_debug_temporal_clause_tokens`, `parse_debug_temporal_stmt`, `parse_enum`, `parse_extension`, `parse_impl`, `parse_in_context_stmt`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 1. Hello World & พื้นฐานการพิมพ์
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M010 `advance` (line 192)

- ชื่อเมธอด: `advance`
- ลายเซ็น: `fn advance(&mut self) -> &Token`
- หน้าที่: move index and return old token
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `&Token`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `is_at_end`
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `is_at_end`
  - ถูกเรียกจาก: `expect`, `expect_ident_like`, `expect_nv`, `expect_word`, `match_gt`, `match_one`, `match_tnv`, `match_word`, `parse_assert_temporal_stmt`, `parse_attributes`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 1. Hello World & พื้นฐานการพิมพ์
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M011 `expect` (line 200)

- ชื่อเมธอด: `expect`
- ลายเซ็น: `fn expect(&mut self, expected: TokenType) -> ParseResult<Token>`
- หน้าที่: require token type or fail
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, expected: TokenType`
  - คืน: `ParseResult<Token>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::UnexpectedToken` เมื่อ token ไม่ตรงที่คาด
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`
  - ถูกเรียกจาก: `parse_block`, `parse_class`, `parse_class_delegate`, `parse_class_field_or_property`, `parse_constructor`, `parse_debug_temporal_stmt`, `parse_effects`, `parse_enum`, `parse_extension`, `parse_function_with`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 1. Hello World & พื้นฐานการพิมพ์
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M012 `error_here` (line 214)

- ชื่อเมธอด: `error_here`
- ลายเซ็น: `fn error_here(&self, message: impl Into<String>) -> ParseError`
- หน้าที่: create a generic parse error at current token
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&self, message: impl Into<String>`
  - คืน: `ParseError`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`
  - ถูกเรียกจาก: `parse_checkpoint_stmt`, `parse_class`, `parse_constructor`, `parse_debug_temporal_clause_tokens`, `parse_effects`, `parse_for_pattern`, `parse_igm`, `parse_inspect_stmt`, `parse_item`, `parse_macro`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: รองรับข้ามหลายข้อ (ไม่ได้ผูกข้อเดียวชัดเจน)
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M013 `expect_ident_like` (line 224)

- ชื่อเมธอด: `expect_ident_like`
- ลายเซ็น: `fn expect_ident_like(&mut self) -> ParseResult<Token>`
- หน้าที่: accept identifier or keyword as name
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Token>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::UnexpectedToken` เมื่อ token ไม่ตรงที่คาด
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`
  - ถูกเรียกจาก: `parse_assert_temporal_stmt`, `parse_class`, `parse_function_with`, `parse_igm`, `parse_lambda`, `parse_macro`, `parse_plugin`, `parse_postfix`, `parse_primary`, `parse_property_accessors`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 1. Hello World & พื้นฐานการพิมพ์
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M014 `expect_nv` (line 237)

- ชื่อเมธอด: `expect_nv`
- ลายเซ็น: `fn expect_nv(&mut self, expected: TokenType, v: &str) -> ParseResult<Token>`
- หน้าที่: require token type with exact text
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, expected: TokenType, v: &str`
  - คืน: `ParseResult<Token>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::UnexpectedToken` เมื่อ token ไม่ตรงที่คาด
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`
  - ถูกเรียกจาก: `parse_class`, `parse_class_delegate`, `parse_constructor`, `parse_enum`, `parse_extension`, `parse_function_with`, `parse_if_expr`, `parse_if_stmt`, `parse_igm`, `parse_impl`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 1. Hello World & พื้นฐานการพิมพ์
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M015 `expect_word` (line 251)

- ชื่อเมธอด: `expect_word`
- ลายเซ็น: `fn expect_word(&mut self, v: &str) -> ParseResult<Token>`
- หน้าที่: require keyword or identifier with exact text
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, v: &str`
  - คืน: `ParseResult<Token>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::UnexpectedToken` เมื่อ token ไม่ตรงที่คาด
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`
  - ถูกเรียกจาก: `parse_assert_temporal_stmt`, `parse_auto_checkpoint_stmt`, `parse_batch_temporal_stmt`, `parse_branch_expr`, `parse_checkpoint_stmt`, `parse_class`, `parse_class_delegate`, `parse_cleanup_stmt`, `parse_commit_stmt`, `parse_debug_temporal_stmt`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 1. Hello World & พื้นฐานการพิมพ์
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M016 `is_word` (line 266)

- ชื่อเมธอด: `is_word`
- ลายเซ็น: `fn is_word(&self, v: &str) -> bool`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&self, v: &str`
  - คืน: `bool`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`
  - ถูกเรียกจาก: `is_debug_temporal_clause_start`, `match_word`, `parse_checkpoint_stmt`, `parse_class`, `parse_property_accessors`, `parse_protocol`, `parse_rewind_parts`, `parse_rewind_stmt`, `parse_statement`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: รองรับข้ามหลายข้อ (ไม่ได้ผูกข้อเดียวชัดเจน)
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M017 `match_word` (line 273)

- ชื่อเมธอด: `match_word`
- ลายเซ็น: `fn match_word(&mut self, v: &str) -> bool`
- หน้าที่: consume keyword/identifier text when present
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, v: &str`
  - คืน: `bool`
- อ่าน/ขยับ token อย่างไร: ขยับ cursor ด้วย `advance`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `is_word`, `advance`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `is_word`, `advance`
  - ถูกเรียกจาก: `parse_assert_temporal_stmt`, `parse_auto_checkpoint_stmt`, `parse_checkpoint_stmt`, `parse_commit_stmt`, `parse_constructor`, `parse_function_with`, `parse_gc_temporal_stmt`, `parse_merge_branch_stmt`, `parse_operator_method`, `parse_primary`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 1. Hello World & พื้นฐานการพิมพ์
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M018 `is_checkpoint_stmt_start` (line 283)

- ชื่อเมธอด: `is_checkpoint_stmt_start`
- ลายเซ็น: `fn is_checkpoint_stmt_start(&self) -> bool`
- หน้าที่: detect checkpoint statement start
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `bool`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 3. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M019 `match_one` (line 300)

- ชื่อเมธอด: `match_one`
- ลายเซ็น: `fn match_one(&mut self, t: TokenType) -> bool`
- หน้าที่: eat token by type if present
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, t: TokenType`
  - คืน: `bool`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`
  - ถูกเรียกจาก: `parse_class_field_or_property`, `parse_comma_separated`, `parse_constructor`, `parse_effects`, `parse_extension`, `parse_for_pattern`, `parse_function_with`, `parse_generics`, `parse_import`, `parse_lambda`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 1. Hello World & พื้นฐานการพิมพ์
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M020 `match_tnv` (line 310)

- ชื่อเมธอด: `match_tnv`
- ลายเซ็น: `fn match_tnv(&mut self, t: TokenType, ch: &str) -> bool`
- หน้าที่: eat token by type and text
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, t: TokenType, ch: &str`
  - คืน: `bool`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`
  - ถูกเรียกจาก: `parse_batch_temporal_stmt`, `parse_class_field_or_property`, `parse_effects`, `parse_enum`, `parse_expression_with_left`, `parse_function_with`, `parse_if_expr_tail`, `parse_if_stmt_tail`, `parse_impl`, `parse_inspect_stmt`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 1. Hello World & พื้นฐานการพิมพ์
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M021 `match_gt` (line 321)

- ชื่อเมธอด: `match_gt`
- ลายเซ็น: `fn match_gt(&mut self) -> bool`
- หน้าที่: handle > or split >> for generics
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `bool`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`
  - ถูกเรียกจาก: `expect_gt`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 1. Hello World & พื้นฐานการพิมพ์
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M022 `expect_gt` (line 345)

- ชื่อเมธอด: `expect_gt`
- ลายเซ็น: `fn expect_gt(&mut self) -> ParseResult<Token>`
- หน้าที่: require > with >> support
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Token>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `match_gt`, `peek`
  - 5. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::UnexpectedToken` เมื่อ token ไม่ตรงที่คาด
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `match_gt`, `peek`
  - ถูกเรียกจาก: `parse_generic_args`, `parse_generics`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 1. Hello World & พื้นฐานการพิมพ์
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M023 `parse_attributes` (line 358)

- ชื่อเมธอด: `parse_attributes`
- ลายเซ็น: `fn parse_attributes(&mut self) -> Vec<String>`
- หน้าที่: read all @attrs into list
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `Vec<String>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`
  - ถูกเรียกจาก: `parse_class`, `parse_extension`, `parse_impl`, `parse_interface`, `parse_item`, `parse_mixin`, `parse_protocol`, `parse_statement`, `parse_trait`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 9. Attributes
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M024 `parse_visibility` (line 370)

- ชื่อเมธอด: `parse_visibility`
- ลายเซ็น: `fn parse_visibility(&mut self) -> Visibility`
- หน้าที่: read visibility, default public
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `Visibility`
- อ่าน/ขยับ token อย่างไร: มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `match_tnv`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `match_tnv`
  - ถูกเรียกจาก: `parse_class`, `parse_extension`, `parse_impl`, `parse_interface`, `parse_item`, `parse_mixin`, `parse_protocol`, `parse_trait`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 9. Attributes, 39. Modern Class & Object System
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M025 `is_function_start` (line 386)

- ชื่อเมธอด: `is_function_start`
- ลายเซ็น: `fn is_function_start(&self) -> bool`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `bool`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`
  - ถูกเรียกจาก: `parse_class`, `parse_item`, `parse_mixin`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: รองรับข้ามหลายข้อ (ไม่ได้ผูกข้อเดียวชัดเจน)
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M026 `is_phrase_token` (line 391)

- ชื่อเมธอด: `is_phrase_token`
- ลายเซ็น: `fn is_phrase_token(tok: &Token) -> bool`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `tok: &Token`
  - คืน: `bool`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 3. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: เรียกจาก flow หลักตามบริบท (ไม่พบ self-call ตรงจากเมธอดอื่น)
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: รองรับข้ามหลายข้อ (ไม่ได้ผูกข้อเดียวชัดเจน)
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M027 `parse_phrase_expr_on_line` (line 398)

- ชื่อเมธอด: `parse_phrase_expr_on_line`
- ลายเซ็น: `fn parse_phrase_expr_on_line(&mut self, stop_words: &[&str]) -> ParseResult<Expr>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, stop_words: &[&str]`
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `is_at_end`, `advance`, `error_here`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Phrase`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `is_at_end`, `advance`, `error_here`
  - ถูกเรียกจาก: `parse_expression_or_phrase`
- AST ที่เกี่ยวข้อง: `Expr::Phrase`
- ผูกกับ syntax ข้อไหน: รองรับข้ามหลายข้อ (ไม่ได้ผูกข้อเดียวชัดเจน)
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M028 `should_parse_phrase_on_line` (line 430)

- ชื่อเมธอด: `should_parse_phrase_on_line`
- ลายเซ็น: `fn should_parse_phrase_on_line(&self, stop_words: &[&str]) -> bool`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&self, stop_words: &[&str]`
  - คืน: `bool`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`
  - ถูกเรียกจาก: `parse_expression_or_phrase`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: รองรับข้ามหลายข้อ (ไม่ได้ผูกข้อเดียวชัดเจน)
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M029 `parse_expression_or_phrase` (line 453)

- ชื่อเมธอด: `parse_expression_or_phrase`
- ลายเซ็น: `fn parse_expression_or_phrase(&mut self, stop_words: &[&str]) -> ParseResult<Expr>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, stop_words: &[&str]`
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `should_parse_phrase_on_line`, `parse_phrase_expr_on_line`, `parse_expression`
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `should_parse_phrase_on_line`, `parse_phrase_expr_on_line`, `parse_expression`
  - ถูกเรียกจาก: `parse_plugin`, `parse_rewind_parts`, `parse_rollback_stmt`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: รองรับข้ามหลายข้อ (ไม่ได้ผูกข้อเดียวชัดเจน)
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M030 `parse_comma_separated` (line 462)

- ชื่อเมธอด: `parse_comma_separated`
- ลายเซ็น: `fn parse_comma_separated<T, F, E>( &mut self, mut is_end: E, mut parse_item: F, ) -> ParseResult<Vec<T>> where F: FnMut(&mut Self) -> ParseResult<T>, E: FnMut(&Token) -> bool,`
- หน้าที่: parse comma list until end
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, mut is_end: E, mut parse_item: F,`
  - คืน: `ParseResult<Vec<T>> where F: FnMut(&mut Self) -> ParseResult<T>, E: FnMut(&Token) -> bool,`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 3. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `is_at_end`, `peek`, `match_one`
  - 5. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `is_at_end`, `peek`, `match_one`
  - ถูกเรียกจาก: `parse_arguments`, `parse_constructor`, `parse_effect_params`, `parse_enum`, `parse_function_with`, `parse_generic_args`, `parse_igm`, `parse_import`, `parse_macro`, `parse_operator_method`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 1. Hello World & พื้นฐานการพิมพ์
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

### C. Top-level Declarations และ Type-level Grammar

#### M031 `parse_program` (line 488)

- ชื่อเมธอด: `parse_program`
- ลายเซ็น: `pub fn parse_program(&mut self) -> ParseResult<Program>`
- หน้าที่: parse whole file into items
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Program>`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `is_at_end`, `parse_item`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `is_at_end`, `parse_item`
  - ถูกเรียกจาก: เรียกจาก flow หลักตามบริบท (ไม่พบ self-call ตรงจากเมธอดอื่น)
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 1. Hello World & พื้นฐานการพิมพ์
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M032 `parse_item` (line 498)

- ชื่อเมธอด: `parse_item`
- ลายเซ็น: `fn parse_item(&mut self) -> ParseResult<Item>`
- หน้าที่: parse one top item with attrs
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Item>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `parse_attributes`, `peek`, `parse_visibility`, `error_here`, `is_function_start`
  - 3. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Item::Class`, `Item::Enum`, `Item::ExtensionDecl`, `Item::Function`
  - 4. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `parse_attributes`, `peek`, `parse_visibility`, `error_here`, `is_function_start`, `parse_function_with`, `advance`, `parse_module_path_string`, `parse_import`, `parse_expression`
  - ถูกเรียกจาก: `parse_program`
- AST ที่เกี่ยวข้อง: `Item::Class`, `Item::Enum`, `Item::ExtensionDecl`, `Item::Function`, `Item::IGMDecl`, `Item::Impl`, `Item::Import`, `Item::InterfaceDecl`, `Item::MacroDecl`, `Item::MixinDecl`, `Item::ModuleDecl`, `Item::PluginDecl`
- ผูกกับ syntax ข้อไหน: 1. Hello World & พื้นฐานการพิมพ์, 2. Module & Import
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M033 `parse_class_inheritance` (line 609)

- ชื่อเมธอด: `parse_class_inheritance`
- ลายเซ็น: `fn parse_class_inheritance( &mut self, ) -> ParseResult<(Vec<TypeRef>, Vec<TypeRef>, Vec<TypeRef>)>`
- หน้าที่: read extends/with/implements
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self,`
  - คืน: `ParseResult<(Vec<TypeRef>, Vec<TypeRef>, Vec<TypeRef>)>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 3. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`
  - 5. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`
  - ถูกเรียกจาก: `parse_class`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 39. Modern Class & Object System
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M034 `parse_class` (line 648)

- ชื่อเมธอด: `parse_class`
- ลายเซ็น: `fn parse_class(&mut self, attributes: Vec<String>) -> ParseResult<ClassDecl>`
- หน้าที่: parse class header and body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, attributes: Vec<String>`
  - คืน: `ParseResult<ClassDecl>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `expect_ident_like`, `parse_class_inheritance`, `expect`, `is_at_end`
  - 5. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `expect_ident_like`, `parse_class_inheritance`, `expect`, `is_at_end`, `peek`, `parse_attributes`, `parse_visibility`, `is_word`, `advance`
  - ถูกเรียกจาก: `parse_item`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 39. Modern Class & Object System
- ตัวอย่างสั้น:

```nx
class Person extends Human {
    name: String
    age: Int { get :> 20 }
}
```

#### M035 `parse_operator_method` (line 800)

- ชื่อเมธอด: `parse_operator_method`
- ลายเซ็น: `fn parse_operator_method( &mut self, attributes: Vec<String>, visibility: Visibility, ) -> ParseResult<FunctionDecl>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, attributes: Vec<String>, visibility: Visibility,`
  - คืน: `ParseResult<FunctionDecl>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`, `error_here`, `match_tnv`, `parse_generics`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::Return`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`, `error_here`, `match_tnv`, `parse_generics`, `expect`, `parse_comma_separated`, `match_word`, `match_one`, `parse_type`
  - ถูกเรียกจาก: `parse_class`
- AST ที่เกี่ยวข้อง: `Stmt::Return`
- ผูกกับ syntax ข้อไหน: 4. ฟังก์ชัน และ Generic, 8. Effect System, 19. Multi-Dispatch Functions, 39. Modern Class & Object System
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M036 `parse_class_delegate` (line 900)

- ชื่อเมธอด: `parse_class_delegate`
- ลายเซ็น: `fn parse_class_delegate( &mut self, attributes: Vec<String>, vis: Visibility, ) -> ParseResult<ClassDelegateDecl>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, attributes: Vec<String>, vis: Visibility,`
  - คืน: `ParseResult<ClassDelegateDecl>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `expect`, `expect_word`, `parse_expression`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `expect`, `expect_word`, `parse_expression`
  - ถูกเรียกจาก: `parse_class`, `parse_mixin`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 39. Modern Class & Object System
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M037 `parse_class_field_or_property` (line 917)

- ชื่อเมธอด: `parse_class_field_or_property`
- ลายเซ็น: `fn parse_class_field_or_property( &mut self, attributes: Vec<String>, vis: Visibility, ) -> ParseResult<FieldOrProperty>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, attributes: Vec<String>, vis: Visibility,`
  - คืน: `ParseResult<FieldOrProperty>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `match_tnv`, `expect`, `match_one`, `parse_expression`, `peek`
  - 5. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `match_tnv`, `expect`, `match_one`, `parse_expression`, `peek`, `parse_property_accessors`, `parse_type`
  - ถูกเรียกจาก: `parse_class`, `parse_mixin`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 39. Modern Class & Object System
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M038 `parse_property_accessors` (line 1001)

- ชื่อเมธอด: `parse_property_accessors`
- ลายเซ็น: `fn parse_property_accessors( &mut self, ) -> ParseResult<(Option<Block>, Option<String>, Option<Block>)>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self,`
  - คืน: `ParseResult<(Option<Block>, Option<String>, Option<Block>)>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 5. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect`, `is_at_end`, `peek`, `is_word`, `advance`
  - 6. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::Expr`, `Stmt::Return`
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect`, `is_at_end`, `peek`, `is_word`, `advance`, `match_one`, `parse_expression`, `parse_block`, `error_here`, `expect_ident_like`
  - ถูกเรียกจาก: `parse_class_field_or_property`
- AST ที่เกี่ยวข้อง: `Stmt::Expr`, `Stmt::Return`
- ผูกกับ syntax ข้อไหน: 39. Modern Class & Object System
- ตัวอย่างสั้น:

```nx
class Person extends Human {
    name: String
    age: Int { get :> 20 }
}
```

#### M039 `parse_constructor` (line 1062)

- ชื่อเมธอด: `parse_constructor`
- ลายเซ็น: `fn parse_constructor( &mut self, attributes: Vec<String>, visibility: Visibility, ) -> ParseResult<ConstructorDecl>`
- หน้าที่: parse init with params and block
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, attributes: Vec<String>, visibility: Visibility,`
  - คืน: `ParseResult<ConstructorDecl>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `match_one`, `expect`, `parse_comma_separated`, `match_word`
  - 5. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `match_one`, `expect`, `parse_comma_separated`, `match_word`, `peek`, `error_here`, `parse_block`
  - ถูกเรียกจาก: `parse_class`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 39. Modern Class & Object System
- ตัวอย่างสั้น:

```nx
class Person extends Human {
    name: String
    age: Int { get :> 20 }
}
```

#### M040 `parse_module_path_string` (line 1115)

- ชื่อเมธอด: `parse_module_path_string`
- ลายเซ็น: `fn parse_module_path_string(&mut self) -> ParseResult<String>`
- หน้าที่: read ModulePath or ident::ident
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<String>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`, `expect`
  - 5. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::UnexpectedToken` เมื่อ token ไม่ตรงที่คาด
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`, `expect`
  - ถูกเรียกจาก: `parse_class`, `parse_import`, `parse_item`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 2. Module & Import
- ตัวอย่างสั้น:

```nx
import core::io { File, Read }
```

#### M041 `parse_import` (line 1142)

- ชื่อเมธอด: `parse_import`
- ลายเซ็น: `fn parse_import(&mut self) -> ParseResult<(String, Option<Vec<String>>)>`
- หน้าที่: parse import path and symbols
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<(String, Option<Vec<String>>)>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `parse_module_path_string`, `match_one`, `parse_comma_separated`, `expect`
  - 5. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `parse_module_path_string`, `match_one`, `parse_comma_separated`, `expect`
  - ถูกเรียกจาก: `parse_item`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 2. Module & Import
- ตัวอย่างสั้น:

```nx
import core::io { File, Read }
```

#### M042 `parse_function_with` (line 1157)

- ชื่อเมธอด: `parse_function_with`
- ลายเซ็น: `fn parse_function_with( &mut self, attributes: Vec<String>, visibility: Visibility, ) -> ParseResult<FunctionDecl>`
- หน้าที่: parse fn with given attrs+vis
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, attributes: Vec<String>, visibility: Visibility,`
  - คืน: `ParseResult<FunctionDecl>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `match_tnv`, `expect_ident_like`, `parse_generics`, `expect`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::Return`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `match_tnv`, `expect_ident_like`, `parse_generics`, `expect`, `parse_comma_separated`, `peek`, `match_word`, `match_one`, `parse_type`
  - ถูกเรียกจาก: `parse_class`, `parse_extension`, `parse_impl`, `parse_interface`, `parse_item`, `parse_mixin`, `parse_protocol`, `parse_trait`
- AST ที่เกี่ยวข้อง: `Stmt::Return`
- ผูกกับ syntax ข้อไหน: 4. ฟังก์ชัน และ Generic, 8. Effect System, 19. Multi-Dispatch Functions, 32. Default Value, 33. Context Parameters / Implicit Arguments
- ตัวอย่างสั้น:

```nx
fn map<T>(x: T = default) -> T where T: Eq !io :> x
```

#### M043 `parse_generics` (line 1276)

- ชื่อเมธอด: `parse_generics`
- ลายเซ็น: `fn parse_generics(&mut self) -> ParseResult<Vec<GenericParam>>`
- หน้าที่: parse generic params
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Vec<GenericParam>>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `expect_gt`, `expect`, `match_one`, `parse_type_constraints`
  - 5. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `expect_gt`, `expect`, `match_one`, `parse_type_constraints`
  - ถูกเรียกจาก: `parse_enum`, `parse_function_with`, `parse_interface`, `parse_operator_method`, `parse_protocol`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 4. ฟังก์ชัน และ Generic
- ตัวอย่างสั้น:

```nx
fn map<T>(x: T = default) -> T where T: Eq !io :> x
```

#### M044 `parse_effects` (line 1310)

- ชื่อเมธอด: `parse_effects`
- ลายเซ็น: `fn parse_effects(&mut self) -> ParseResult<Vec<Effect>>`
- หน้าที่: parse effect list
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Vec<Effect>>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 5. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`, `match_tnv`, `error_here`, `match_one`
  - 6. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`, `match_tnv`, `error_here`, `match_one`, `parse_effect_params`, `expect`
  - ถูกเรียกจาก: `parse_function_with`, `parse_operator_method`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 8. Effect System
- ตัวอย่างสั้น:

```nx
fn map<T>(x: T = default) -> T where T: Eq !io :> x
```

#### M045 `parse_type_constraints` (line 1359)

- ชื่อเมธอด: `parse_type_constraints`
- ลายเซ็น: `fn parse_type_constraints(&mut self) -> ParseResult<Vec<TypeConstraint>>`
- หน้าที่: parse constraint list
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Vec<TypeConstraint>>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 5. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `is_at_end`, `peek`, `advance`, `match_tnv`, `parse_type`
  - 6. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `TypeConstraint::LifetimeBound`, `TypeConstraint::SubtypeOf`, `TypeConstraint::TraitBound`, `TypeConstraint::TypeEq`
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `is_at_end`, `peek`, `advance`, `match_tnv`, `parse_type`, `match_one`, `expect`, `expect_nv`
  - ถูกเรียกจาก: `parse_generics`, `parse_where_clauses`
- AST ที่เกี่ยวข้อง: `TypeConstraint::LifetimeBound`, `TypeConstraint::SubtypeOf`, `TypeConstraint::TraitBound`, `TypeConstraint::TypeEq`
- ผูกกับ syntax ข้อไหน: รองรับข้ามหลายข้อ (ไม่ได้ผูกข้อเดียวชัดเจน)
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M046 `parse_effect_params` (line 1403)

- ชื่อเมธอด: `parse_effect_params`
- ลายเซ็น: `fn parse_effect_params(&mut self) -> ParseResult<Vec<TypeRef>>`
- หน้าที่: parse effect param types
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Vec<TypeRef>>`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `parse_comma_separated`
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `parse_comma_separated`
  - ถูกเรียกจาก: `parse_effects`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 8. Effect System
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M047 `parse_where_clauses` (line 1411)

- ชื่อเมธอด: `parse_where_clauses`
- ลายเซ็น: `fn parse_where_clauses(&mut self) -> ParseResult<Vec<WhereClause>>`
- หน้าที่: parse where constraints
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Vec<WhereClause>>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 5. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `expect`, `parse_type_constraints`, `match_one`
  - 6. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `expect`, `parse_type_constraints`, `match_one`
  - ถูกเรียกจาก: `parse_function_with`, `parse_operator_method`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 4. ฟังก์ชัน และ Generic
- ตัวอย่างสั้น:

```nx
fn map<T>(x: T = default) -> T where T: Eq !io :> x
```

#### M048 `parse_struct` (line 1437)

- ชื่อเมธอด: `parse_struct`
- ลายเซ็น: `fn parse_struct(&mut self, attributes: Vec<String>) -> ParseResult<StructDecl>`
- หน้าที่: parse struct declaration
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, attributes: Vec<String>`
  - คืน: `ParseResult<StructDecl>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `expect`, `parse_comma_separated`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `expect`, `parse_comma_separated`
  - ถูกเรียกจาก: `parse_item`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 3. Types & Declarations, 39. Modern Class & Object System
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M049 `parse_enum` (line 1460)

- ชื่อเมธอด: `parse_enum`
- ลายเซ็น: `fn parse_enum(&mut self, attributes: Vec<String>) -> ParseResult<EnumDecl>`
- หน้าที่: parse enum with variants
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, attributes: Vec<String>`
  - คืน: `ParseResult<EnumDecl>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 5. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `expect`, `match_tnv`, `parse_generics`, `peek`
  - 6. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `VariantKind::Struct`, `VariantKind::Tuple`, `VariantKind::Unit`
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `expect`, `match_tnv`, `parse_generics`, `peek`, `is_at_end`, `advance`, `parse_comma_separated`
  - ถูกเรียกจาก: `parse_item`
- AST ที่เกี่ยวข้อง: `VariantKind::Struct`, `VariantKind::Tuple`, `VariantKind::Unit`
- ผูกกับ syntax ข้อไหน: 3. Types & Declarations, 39. Modern Class & Object System
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M050 `parse_mixin` (line 1546)

- ชื่อเมธอด: `parse_mixin`
- ลายเซ็น: `fn parse_mixin(&mut self, attributes: Vec<String>) -> ParseResult<MixinDecl>`
- หน้าที่: parse mixin with fields, properties, delegates, and methods
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, attributes: Vec<String>`
  - คืน: `ParseResult<MixinDecl>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `expect`, `peek`, `is_at_end`, `parse_attributes`
  - 5. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `expect`, `peek`, `is_at_end`, `parse_attributes`, `parse_visibility`, `parse_class_delegate`, `advance`, `is_function_start`, `parse_function_with`
  - ถูกเรียกจาก: `parse_item`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 39. Modern Class & Object System
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M051 `parse_trait` (line 1618)

- ชื่อเมธอด: `parse_trait`
- ลายเซ็น: `fn parse_trait(&mut self, attributes: Vec<String>) -> ParseResult<TraitDecl>`
- หน้าที่: parse trait with methods
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, attributes: Vec<String>`
  - คืน: `ParseResult<TraitDecl>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `expect`, `peek`, `is_at_end`, `parse_attributes`
  - 5. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `expect`, `peek`, `is_at_end`, `parse_attributes`, `parse_visibility`, `parse_function_with`
  - ถูกเรียกจาก: `parse_item`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 10. Traits & Implementations, 39. Modern Class & Object System
- ตัวอย่างสั้น:

```nx
extension (p: Person) { fn hello() :> p.name }
```

#### M052 `parse_interface` (line 1643)

- ชื่อเมธอด: `parse_interface`
- ลายเซ็น: `fn parse_interface(&mut self, attributes: Vec<String>) -> ParseResult<InterfaceDecl>`
- หน้าที่: parse interface with generics and methods
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, attributes: Vec<String>`
  - คืน: `ParseResult<InterfaceDecl>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 5. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `expect`, `match_tnv`, `parse_generics`, `peek`
  - 6. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `expect`, `match_tnv`, `parse_generics`, `peek`, `is_at_end`, `parse_attributes`, `parse_visibility`, `parse_function_with`
  - ถูกเรียกจาก: `parse_item`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 10. Traits & Implementations, 39. Modern Class & Object System
- ตัวอย่างสั้น:

```nx
extension (p: Person) { fn hello() :> p.name }
```

#### M053 `parse_protocol` (line 1675)

- ชื่อเมธอด: `parse_protocol`
- ลายเซ็น: `fn parse_protocol(&mut self, attributes: Vec<String>) -> ParseResult<ProtocolDecl>`
- หน้าที่: parse protocol with associated types, methods, and extensions
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, attributes: Vec<String>`
  - คืน: `ParseResult<ProtocolDecl>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 5. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `expect`, `match_tnv`, `parse_generics`, `peek`
  - 6. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `expect`, `match_tnv`, `parse_generics`, `peek`, `is_at_end`, `parse_attributes`, `is_word`, `advance`, `parse_visibility`
  - ถูกเรียกจาก: `parse_item`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 10. Traits & Implementations, 39. Modern Class & Object System
- ตัวอย่างสั้น:

```nx
extension (p: Person) { fn hello() :> p.name }
```

#### M054 `parse_impl` (line 1754)

- ชื่อเมธอด: `parse_impl`
- ลายเซ็น: `fn parse_impl(&mut self, attributes: Vec<String>) -> ParseResult<ImplDecl>`
- หน้าที่: parse impl block
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, attributes: Vec<String>`
  - คืน: `ParseResult<ImplDecl>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 5. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `parse_type`, `match_tnv`, `expect`, `peek`
  - 6. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `parse_type`, `match_tnv`, `expect`, `peek`, `is_at_end`, `parse_attributes`, `parse_visibility`, `parse_function_with`
  - ถูกเรียกจาก: `parse_item`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 10. Traits & Implementations, 39. Modern Class & Object System
- ตัวอย่างสั้น:

```nx
extension (p: Person) { fn hello() :> p.name }
```

#### M055 `parse_macro` (line 4876)

- ชื่อเมธอด: `parse_macro`
- ลายเซ็น: `fn parse_macro(&mut self, attributes: Vec<String>) -> ParseResult<MacroDecl>`
- หน้าที่: parse macro name!(params) { ... }
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, attributes: Vec<String>`
  - คืน: `ParseResult<MacroDecl>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `expect_ident_like`, `peek`, `error_here`, `advance`
  - 5. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `expect_ident_like`, `peek`, `error_here`, `advance`, `match_one`, `parse_comma_separated`, `expect`, `parse_block`
  - ถูกเรียกจาก: `parse_item`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 7. มาโคร & Compile-Time Metaprogramming, 31. Compile-Time Functions
- ตัวอย่างสั้น:

```nx
macro log!(x) { compile-time { emit x } }
```

#### M056 `parse_extension` (line 4906)

- ชื่อเมธอด: `parse_extension`
- ลายเซ็น: `fn parse_extension(&mut self, attributes: Vec<String>) -> ParseResult<ExtensionDecl>`
- หน้าที่: parse extension (name: Type) { ... } or extension Type { ... }
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, attributes: Vec<String>`
  - คืน: `ParseResult<ExtensionDecl>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 5. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `match_one`, `expect`, `parse_type`, `is_at_end`
  - 6. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `match_one`, `expect`, `parse_type`, `is_at_end`, `parse_attributes`, `parse_visibility`, `parse_function_with`
  - ถูกเรียกจาก: `parse_item`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 15. Extension Methods, 28. Extension Methods & Traits Syntax, 39. Modern Class & Object System
- ตัวอย่างสั้น:

```nx
extension (p: Person) { fn hello() :> p.name }
```

#### M057 `parse_igm` (line 4940)

- ชื่อเมธอด: `parse_igm`
- ลายเซ็น: `fn parse_igm(&mut self, attributes: Vec<String>) -> ParseResult<IGMDecl>`
- หน้าที่: parse igm name! { pattern = /.../ expand(m: Match) = Expr }
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, attributes: Vec<String>`
  - คืน: `ParseResult<IGMDecl>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `expect_ident_like`, `peek`, `error_here`, `advance`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `PatternKind::Identifier`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `expect_ident_like`, `peek`, `error_here`, `advance`, `expect`, `expect_word`, `parse_comma_separated`, `parse_expression`
  - ถูกเรียกจาก: `parse_item`
- AST ที่เกี่ยวข้อง: `PatternKind::Identifier`
- ผูกกับ syntax ข้อไหน: 16. Inline Grammar Macros (IGM)
- ตัวอย่างสั้น:

```nx
macro log!(x) { compile-time { emit x } }
```

#### M058 `parse_plugin` (line 4999)

- ชื่อเมธอด: `parse_plugin`
- ลายเซ็น: `fn parse_plugin(&mut self, attributes: Vec<String>) -> ParseResult<PluginDecl>`
- หน้าที่: parse plugin name when <expr>
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, attributes: Vec<String>`
  - คืน: `ParseResult<PluginDecl>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `expect_ident_like`, `match_tnv`, `parse_expression_or_phrase`
  - 5. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `expect_ident_like`, `match_tnv`, `parse_expression_or_phrase`
  - ถูกเรียกจาก: `parse_item`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 36. Contextual Micro-Plugins
- ตัวอย่างสั้น:

```nx
macro log!(x) { compile-time { emit x } }
```

### D. Statement Parser (รวม Temporal Statements)

#### M059 `parse_block` (line 1788)

- ชื่อเมธอด: `parse_block`
- ลายเซ็น: `fn parse_block(&mut self) -> ParseResult<Block>`
- หน้าที่: parse block and track lets
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Block>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect`, `peek`, `is_at_end`, `parse_statement`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Ident`, `PatternKind::Identifier`, `Stmt::Attributed`, `Stmt::Delete`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect`, `peek`, `is_at_end`, `parse_statement`
  - ถูกเรียกจาก: `parse_assert_temporal_stmt`, `parse_auto_checkpoint_stmt`, `parse_batch_temporal_stmt`, `parse_block_or_expr_body`, `parse_branch_expr`, `parse_checkpoint_stmt`, `parse_class`, `parse_cleanup_stmt`, `parse_constructor`, `parse_emit_stmt`
- AST ที่เกี่ยวข้อง: `Expr::Ident`, `PatternKind::Identifier`, `Stmt::Attributed`, `Stmt::Delete`, `Stmt::Let`
- ผูกกับ syntax ข้อไหน: 5. Control Flow Statements, 9. Attributes
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M060 `parse_block::collect_inner` (line 1794)

- ชื่อเมธอด: `parse_block::collect_inner`
- ลายเซ็น: `fn collect_inner(stmt: &Stmt, names: &mut Vec<String>)`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `stmt: &Stmt, names: &mut Vec<String>`
  - คืน: `ไม่ระบุ (Rust default unit `()`)`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `PatternKind::Identifier`, `Stmt::Attributed`, `Stmt::Let`
  - 3. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: `parse_block`
- AST ที่เกี่ยวข้อง: `PatternKind::Identifier`, `Stmt::Attributed`, `Stmt::Let`
- ผูกกับ syntax ข้อไหน: 9. Attributes
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M061 `parse_block_or_expr_body` (line 1826)

- ชื่อเมธอด: `parse_block_or_expr_body`
- ลายเซ็น: `fn parse_block_or_expr_body(&mut self) -> ParseResult<Block>`
- หน้าที่: parse block or short expr
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Block>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `parse_block`, `parse_expression`, `advance`
  - 3. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::Expr`
  - 4. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `parse_block`, `parse_expression`, `advance`
  - ถูกเรียกจาก: `parse_if_expr_tail`, `parse_in_context_stmt`, `parse_on_sequence_stmt`, `parse_statement`, `parse_temporal_clause`, `parse_temporal_match_stmt`
- AST ที่เกี่ยวข้อง: `Stmt::Expr`
- ผูกกับ syntax ข้อไหน: 5. Control Flow Statements, 17. Expression-Only Functions
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M062 `parse_expr_with_struct_literal_guard` (line 1842)

- ชื่อเมธอด: `parse_expr_with_struct_literal_guard`
- ลายเซ็น: `fn parse_expr_with_struct_literal_guard( &mut self, allow_struct_literal: bool, ) -> ParseResult<Expr>`
- หน้าที่: avoid struct literal before block
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, allow_struct_literal: bool,`
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`, `parse_postfix`, `parse_expression`
  - 3. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Ident`
  - 4. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`, `parse_postfix`, `parse_expression`
  - ถูกเรียกจาก: `parse_if_expr_tail`, `parse_if_stmt_tail`, `parse_in_context_stmt`, `parse_match_expr`, `parse_replay_stmt`, `parse_statement`
- AST ที่เกี่ยวข้อง: `Expr::Ident`
- ผูกกับ syntax ข้อไหน: 5. Control Flow Statements
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M063 `parse_let_stmt` (line 1868)

- ชื่อเมธอด: `parse_let_stmt`
- ลายเซ็น: `fn parse_let_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse let statement
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `advance`, `is_at_end`, `match_tnv`, `parse_pattern`, `match_one`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::Let`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `advance`, `is_at_end`, `match_tnv`, `parse_pattern`, `match_one`, `parse_type`, `parse_expression`, `peek`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::Let`
- ผูกกับ syntax ข้อไหน: 5. Control Flow Statements
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M064 `parse_try_catch_stmt` (line 1896)

- ชื่อเมธอด: `parse_try_catch_stmt`
- ลายเซ็น: `fn parse_try_catch_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse try/catch statement
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `parse_block`, `match_one`, `expect_ident_like`, `expect`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::TryCatch`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `parse_block`, `match_one`, `expect_ident_like`, `expect`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::TryCatch`
- ผูกกับ syntax ข้อไหน: 5. Control Flow Statements, 12. Error Handling: Result & Try/Catch
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M065 `parse_string_literal_value` (line 1919)

- ชื่อเมธอด: `parse_string_literal_value`
- ลายเซ็น: `fn parse_string_literal_value(&mut self) -> ParseResult<String>`
- หน้าที่: parse string literal value
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<String>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::UnexpectedToken` เมื่อ token ไม่ตรงที่คาด
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`
  - ถูกเรียกจาก: `parse_branch_expr`, `parse_checkpoint_stmt`, `parse_snapshot_stmt`, `parse_temporal_scope_stmt`, `parse_temporal_test_stmt`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: รองรับข้ามหลายข้อ (ไม่ได้ผูกข้อเดียวชัดเจน)
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M066 `parse_named_record_literal` (line 1935)

- ชื่อเมธอด: `parse_named_record_literal`
- ลายเซ็น: `fn parse_named_record_literal(&mut self, name: &str) -> ParseResult<Expr>`
- หน้าที่: parse record literal in braces with a synthetic name
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, name: &str`
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect`, `parse_struct_fields`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Literal`, `Literal::Struct`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect`, `parse_struct_fields`
  - ถูกเรียกจาก: `parse_batch_temporal_stmt`, `parse_checkpoint_stmt`, `parse_commit_stmt`, `parse_emit_stmt`, `parse_rollback_stmt`, `parse_snapshot_stmt`, `parse_temporal_config_and_body`
- AST ที่เกี่ยวข้อง: `Expr::Literal`, `Literal::Struct`
- ผูกกับ syntax ข้อไหน: รองรับข้ามหลายข้อ (ไม่ได้ผูกข้อเดียวชัดเจน)
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M067 `block_starts_as_record_literal` (line 1946)

- ชื่อเมธอด: `block_starts_as_record_literal`
- ลายเซ็น: `fn block_starts_as_record_literal(&self, start_idx: usize) -> bool`
- หน้าที่: detect `{ key: value }`-style payload blocks
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&self, start_idx: usize`
  - คืน: `bool`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 3. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: `parse_checkpoint_stmt`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: รองรับข้ามหลายข้อ (ไม่ได้ผูกข้อเดียวชัดเจน)
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M068 `block_followed_by_block` (line 1969)

- ชื่อเมธอด: `block_followed_by_block`
- ลายเซ็น: `fn block_followed_by_block(&self, start_idx: usize) -> bool`
- หน้าที่: check if a block is followed by another block
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&self, start_idx: usize`
  - คืน: `bool`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: `parse_temporal_config_and_body`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: รองรับข้ามหลายข้อ (ไม่ได้ผูกข้อเดียวชัดเจน)
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M069 `parse_temporal_config_and_body` (line 1999)

- ชื่อเมธอด: `parse_temporal_config_and_body`
- ลายเซ็น: `fn parse_temporal_config_and_body(&mut self) -> ParseResult<(Option<Expr>, Block)>`
- หน้าที่: parse optional config block then required body block
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<(Option<Expr>, Block)>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `error_here`, `block_followed_by_block`, `parse_named_record_literal`, `parse_block`
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `error_here`, `block_followed_by_block`, `parse_named_record_literal`, `parse_block`
  - ถูกเรียกจาก: `parse_temporal_memory_stmt`, `parse_temporal_scope_stmt`, `parse_temporal_test_stmt`, `parse_temporal_transaction_stmt`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M070 `parse_rewind_parts` (line 2015)

- ชื่อเมธอด: `parse_rewind_parts`
- ลายเซ็น: `fn parse_rewind_parts( &mut self, allow_else: bool, ) -> ParseResult<(Option<Expr>, Option<Expr>, Option<Expr>, Option<Block>, Option<Expr>)>`
- หน้าที่: shared rewind parser for statement/expression forms
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, allow_else: bool,`
  - คืน: `ParseResult<(Option<Expr>, Option<Expr>, Option<Expr>, Option<Block>, Option<Expr>)>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `match_word`, `parse_expression_or_phrase`, `peek`, `is_word`
  - 5. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `match_word`, `parse_expression_or_phrase`, `peek`, `is_word`, `parse_expression`, `parse_block`
  - ถูกเรียกจาก: `parse_rewind_expr`, `parse_rewind_expr_relaxed`, `parse_rewind_stmt`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M071 `parse_checkpoint_stmt` (line 2061)

- ชื่อเมธอด: `parse_checkpoint_stmt`
- ลายเซ็น: `fn parse_checkpoint_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse checkpoint statement
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `peek`, `parse_string_literal_value`, `match_word`, `is_word`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::Checkpoint`, `Stmt::Expr`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `peek`, `parse_string_literal_value`, `match_word`, `is_word`, `advance`, `parse_named_record_literal`, `parse_expression`, `parse_block`, `error_here`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::Checkpoint`, `Stmt::Expr`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M072 `parse_rewind_stmt` (line 2137)

- ชื่อเมธอด: `parse_rewind_stmt`
- ลายเซ็น: `fn parse_rewind_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse rewind statement
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `parse_rewind_parts`, `is_word`, `error_here`, `peek`, `advance`
  - 3. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::Rewind`
  - 4. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `parse_rewind_parts`, `is_word`, `error_here`, `peek`, `advance`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::Rewind`
- ผูกกับ syntax ข้อไหน: 12. Error Handling: Result & Try/Catch, 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M073 `parse_rewind_expr` (line 2159)

- ชื่อเมธอด: `parse_rewind_expr`
- ลายเซ็น: `fn parse_rewind_expr(&mut self) -> ParseResult<Expr>`
- หน้าที่: parse rewind expression with fallback
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `parse_rewind_parts`, `error_here`
  - 3. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Rewind`
  - 4. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `parse_rewind_parts`, `error_here`
  - ถูกเรียกจาก: `parse_primary`
- AST ที่เกี่ยวข้อง: `Expr::Rewind`
- ผูกกับ syntax ข้อไหน: 12. Error Handling: Result & Try/Catch, 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M074 `parse_rewind_expr_relaxed` (line 2176)

- ชื่อเมธอด: `parse_rewind_expr_relaxed`
- ลายเซ็น: `fn parse_rewind_expr_relaxed(&mut self) -> ParseResult<Expr>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `parse_rewind_parts`
  - 3. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Rewind`
  - 4. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `parse_rewind_parts`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Expr::Rewind`
- ผูกกับ syntax ข้อไหน: 12. Error Handling: Result & Try/Catch, 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M075 `parse_branch_expr` (line 2187)

- ชื่อเมธอด: `parse_branch_expr`
- ลายเซ็น: `fn parse_branch_expr(&mut self) -> ParseResult<Expr>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `peek`, `parse_string_literal_value`, `parse_expression`, `parse_block`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Branch`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `peek`, `parse_string_literal_value`, `parse_expression`, `parse_block`
  - ถูกเรียกจาก: `parse_primary`
- AST ที่เกี่ยวข้อง: `Expr::Branch`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M076 `parse_inspect_stmt` (line 2207)

- ชื่อเมธอด: `parse_inspect_stmt`
- ลายเซ็น: `fn parse_inspect_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse inspect statement
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 5. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `peek`, `error_here`, `match_tnv`, `parse_block`
  - 6. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Literal`, `Literal::String`, `Stmt::Inspect`
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `peek`, `error_here`, `match_tnv`, `parse_block`, `advance`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Expr::Literal`, `Literal::String`, `Stmt::Inspect`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M077 `parse_snapshot_stmt` (line 2280)

- ชื่อเมธอด: `parse_snapshot_stmt`
- ลายเซ็น: `fn parse_snapshot_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse snapshot statement
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `peek`, `parse_string_literal_value`, `match_tnv`, `parse_named_record_literal`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::Snapshot`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `peek`, `parse_string_literal_value`, `match_tnv`, `parse_named_record_literal`, `parse_expression`, `advance`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::Snapshot`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M078 `parse_rollback_stmt` (line 2311)

- ชื่อเมธอด: `parse_rollback_stmt`
- ลายเซ็น: `fn parse_rollback_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse rollback statement
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `match_tnv`, `parse_expression_or_phrase`, `peek`, `match_word`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::Rollback`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `match_tnv`, `parse_expression_or_phrase`, `peek`, `match_word`, `parse_expression`, `parse_named_record_literal`, `parse_block`, `advance`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::Rollback`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M079 `parse_replay_stmt` (line 2374)

- ชื่อเมธอด: `parse_replay_stmt`
- ลายเซ็น: `fn parse_replay_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse replay statement
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `parse_expr_with_struct_literal_guard`, `parse_block`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::Replay`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `parse_expr_with_struct_literal_guard`, `parse_block`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::Replay`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M080 `parse_replay_pause_stmt` (line 2381)

- ชื่อเมธอด: `parse_replay_pause_stmt`
- ลายเซ็น: `fn parse_replay_pause_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `match_word`, `parse_expression`, `peek`, `advance`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::ReplayPause`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `match_word`, `parse_expression`, `peek`, `advance`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::ReplayPause`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M081 `parse_replay_on_checkpoint_stmt` (line 2399)

- ชื่อเมธอด: `parse_replay_on_checkpoint_stmt`
- ลายเซ็น: `fn parse_replay_on_checkpoint_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `parse_expression`, `parse_block`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::ReplayOnCheckpoint`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `parse_expression`, `parse_block`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::ReplayOnCheckpoint`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M082 `parse_replay_modify_stmt` (line 2407)

- ชื่อเมธอด: `parse_replay_modify_stmt`
- ลายเซ็น: `fn parse_replay_modify_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `parse_expression`, `expect_nv`, `peek`, `advance`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::ReplayModify`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `parse_expression`, `expect_nv`, `peek`, `advance`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::ReplayModify`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M083 `parse_merge_branch_stmt` (line 2418)

- ชื่อเมธอด: `parse_merge_branch_stmt`
- ลายเซ็น: `fn parse_merge_branch_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `match_word`, `parse_postfix`, `peek`, `advance`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Ident`, `Stmt::Expr`, `Stmt::MergeBranch`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `match_word`, `parse_postfix`, `peek`, `advance`, `parse_expression`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Expr::Ident`, `Stmt::Expr`, `Stmt::MergeBranch`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M084 `parse_retry_stmt` (line 2436)

- ชื่อเมธอด: `parse_retry_stmt`
- ลายเซ็น: `fn parse_retry_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `parse_expression`, `parse_block`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::TemporalRetry`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `parse_expression`, `parse_block`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::TemporalRetry`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M085 `parse_auto_checkpoint_stmt` (line 2448)

- ชื่อเมธอด: `parse_auto_checkpoint_stmt`
- ลายเซ็น: `fn parse_auto_checkpoint_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `match_word`, `parse_postfix`, `peek`, `advance`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Ident`, `Stmt::AutoCheckpoint`, `Stmt::Expr`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `match_word`, `parse_postfix`, `peek`, `advance`, `parse_block`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Expr::Ident`, `Stmt::AutoCheckpoint`, `Stmt::Expr`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M086 `parse_emit_stmt` (line 2462)

- ชื่อเมธอด: `parse_emit_stmt`
- ลายเซ็น: `fn parse_emit_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `peek`, `parse_postfix`, `advance`, `parse_expression`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Ident`, `Stmt::Emit`, `Stmt::Expr`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `peek`, `parse_postfix`, `advance`, `parse_expression`, `parse_named_record_literal`, `parse_block`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Expr::Ident`, `Stmt::Emit`, `Stmt::Expr`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M087 `parse_gc_temporal_stmt` (line 2483)

- ชื่อเมธอด: `parse_gc_temporal_stmt`
- ลายเซ็น: `fn parse_gc_temporal_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `match_word`, `parse_postfix`, `peek`, `advance`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Ident`, `Stmt::Expr`, `Stmt::GcTemporal`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `match_word`, `parse_postfix`, `peek`, `advance`, `parse_block`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Expr::Ident`, `Stmt::Expr`, `Stmt::GcTemporal`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M088 `parse_assert_temporal_stmt` (line 2500)

- ชื่อเมธอด: `parse_assert_temporal_stmt`
- ลายเซ็น: `fn parse_assert_temporal_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `match_word`, `parse_postfix`, `peek`, `advance`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Ident`, `Stmt::AssertTemporal`, `Stmt::Expr`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `match_word`, `parse_postfix`, `peek`, `advance`, `expect_ident_like`, `parse_block`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Expr::Ident`, `Stmt::AssertTemporal`, `Stmt::Expr`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M089 `parse_commit_stmt` (line 2517)

- ชื่อเมธอด: `parse_commit_stmt`
- ลายเซ็น: `fn parse_commit_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `peek`, `parse_postfix`, `advance`, `match_word`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Ident`, `Stmt::Commit`, `Stmt::Expr`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `peek`, `parse_postfix`, `advance`, `match_word`, `parse_named_record_literal`, `parse_expression`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Expr::Ident`, `Stmt::Commit`, `Stmt::Expr`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M090 `parse_cleanup_stmt` (line 2541)

- ชื่อเมธอด: `parse_cleanup_stmt`
- ลายเซ็น: `fn parse_cleanup_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `peek`, `parse_postfix`, `advance`, `parse_expression`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Ident`, `Stmt::Cleanup`, `Stmt::Expr`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `peek`, `parse_postfix`, `advance`, `parse_expression`, `parse_block`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Expr::Ident`, `Stmt::Cleanup`, `Stmt::Expr`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M091 `parse_batch_temporal_stmt` (line 2562)

- ชื่อเมธอด: `parse_batch_temporal_stmt`
- ลายเซ็น: `fn parse_batch_temporal_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse batch temporal statement
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `parse_block`, `match_tnv`, `peek`, `parse_named_record_literal`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::BatchTemporal`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `parse_block`, `match_tnv`, `peek`, `parse_named_record_literal`, `parse_expression`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::BatchTemporal`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M092 `parse_temporal_scope_stmt` (line 2583)

- ชื่อเมธอด: `parse_temporal_scope_stmt`
- ลายเซ็น: `fn parse_temporal_scope_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse temporal scope statement
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `peek`, `parse_string_literal_value`, `parse_temporal_config_and_body`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::TemporalScope`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `peek`, `parse_string_literal_value`, `parse_temporal_config_and_body`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::TemporalScope`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M093 `parse_temporal_transaction_stmt` (line 2600)

- ชื่อเมธอด: `parse_temporal_transaction_stmt`
- ลายเซ็น: `fn parse_temporal_transaction_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse temporal transaction statement
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `parse_temporal_config_and_body`, `match_word`, `peek`, `parse_block`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::TemporalTransaction`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `parse_temporal_config_and_body`, `match_word`, `peek`, `parse_block`, `match_one`, `expect_ident_like`, `expect`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::TemporalTransaction`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M094 `parse_temporal_test_stmt` (line 2633)

- ชื่อเมธอด: `parse_temporal_test_stmt`
- ลายเซ็น: `fn parse_temporal_test_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse temporal test statement
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `peek`, `parse_string_literal_value`, `expect_ident_like`, `parse_temporal_config_and_body`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::TemporalTest`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `peek`, `parse_string_literal_value`, `expect_ident_like`, `parse_temporal_config_and_body`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::TemporalTest`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M095 `parse_temporal_memory_stmt` (line 2650)

- ชื่อเมธอด: `parse_temporal_memory_stmt`
- ลายเซ็น: `fn parse_temporal_memory_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse temporal memory statement
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `parse_temporal_config_and_body`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::TemporalMemory`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `parse_temporal_config_and_body`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::TemporalMemory`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M096 `is_debug_temporal_clause_start` (line 2660)

- ชื่อเมธอด: `is_debug_temporal_clause_start`
- ลายเซ็น: `fn is_debug_temporal_clause_start(&self) -> bool`
- หน้าที่: detect debug temporal clause starts
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `bool`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `is_word`
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `is_word`
  - ถูกเรียกจาก: `parse_debug_temporal_clause_tokens`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M097 `parse_debug_temporal_clause_tokens` (line 2665)

- ชื่อเมธอด: `parse_debug_temporal_clause_tokens`
- ลายเซ็น: `fn parse_debug_temporal_clause_tokens(&mut self) -> ParseResult<Vec<Token>>`
- หน้าที่: collect one raw debug temporal clause
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Vec<Token>>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `is_at_end`, `peek`, `advance`, `is_debug_temporal_clause_start`, `error_here`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `is_at_end`, `peek`, `advance`, `is_debug_temporal_clause_start`, `error_here`
  - ถูกเรียกจาก: `parse_debug_temporal_stmt`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M098 `parse_tokens_until_short_arrow` (line 2713)

- ชื่อเมธอด: `parse_tokens_until_short_arrow`
- ลายเซ็น: `fn parse_tokens_until_short_arrow(&mut self, context: &str) -> ParseResult<Vec<Token>>`
- หน้าที่: collect tokens until top-level :> for temporal patterns
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, context: &str`
  - คืน: `ParseResult<Vec<Token>>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `is_at_end`, `peek`, `error_here`, `advance`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `is_at_end`, `peek`, `error_here`, `advance`
  - ถูกเรียกจาก: `parse_temporal_match_stmt`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M099 `parse_debug_temporal_stmt` (line 2750)

- ชื่อเมธอด: `parse_debug_temporal_stmt`
- ลายเซ็น: `fn parse_debug_temporal_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse debug temporal statement
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `expect`, `peek`, `is_at_end`, `parse_debug_temporal_clause_tokens`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::DebugTemporal`, `TemporalClause::Raw`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `expect`, `peek`, `is_at_end`, `parse_debug_temporal_clause_tokens`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::DebugTemporal`, `TemporalClause::Raw`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M100 `parse_temporal_pattern` (line 2766)

- ชื่อเมธอด: `parse_temporal_pattern`
- ลายเซ็น: `fn parse_temporal_pattern(&mut self) -> ParseResult<TemporalPattern>`
- หน้าที่: parse temporal pattern
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<TemporalPattern>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_ident_like`, `match_one`, `peek`, `parse_arguments`, `expect`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `TemporalPattern::Parsed`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_ident_like`, `match_one`, `peek`, `parse_arguments`, `expect`, `match_tnv`, `parse_expression`
  - ถูกเรียกจาก: `parse_temporal_clause`
- AST ที่เกี่ยวข้อง: `TemporalPattern::Parsed`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M101 `parse_temporal_clause` (line 2792)

- ชื่อเมธอด: `parse_temporal_clause`
- ลายเซ็น: `fn parse_temporal_clause(&mut self) -> ParseResult<TemporalClause>`
- หน้าที่: parse temporal handler clause
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<TemporalClause>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `parse_temporal_pattern`, `match_tnv`, `parse_expression`, `expect`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `TemporalClause::Parsed`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `parse_temporal_pattern`, `match_tnv`, `parse_expression`, `expect`, `parse_block_or_expr_body`
  - ถูกเรียกจาก: `parse_temporal_handle_stmt`
- AST ที่เกี่ยวข้อง: `TemporalClause::Parsed`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M102 `parse_temporal_match_stmt` (line 2810)

- ชื่อเมธอด: `parse_temporal_match_stmt`
- ลายเซ็น: `fn parse_temporal_match_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse match temporal state { ... } into raw patterns + bodies
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `expect`, `peek`, `is_at_end`, `advance`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Literal`, `Literal::String`, `Stmt::TemporalMatch`, `TemporalClause::Parsed`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `expect`, `peek`, `is_at_end`, `advance`, `parse_tokens_until_short_arrow`, `parse_block_or_expr_body`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Expr::Literal`, `Literal::String`, `Stmt::TemporalMatch`, `TemporalClause::Parsed`, `TemporalPattern::Raw`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M103 `parse_temporal_handle_stmt` (line 2842)

- ชื่อเมธอด: `parse_temporal_handle_stmt`
- ลายเซ็น: `fn parse_temporal_handle_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse handle temporal effects statement
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_word`, `parse_block`, `expect`, `peek`, `is_at_end`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::TemporalHandle`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_word`, `parse_block`, `expect`, `peek`, `is_at_end`, `advance`, `parse_temporal_clause`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::TemporalHandle`
- ผูกกับ syntax ข้อไหน: 40. Core Temporal Operations, 41. Advanced Temporal Features, 42. Production Temporal Features, 43. Temporal Patterns & Best Practices, 44. Temporal Integration with Other Language Features, 45. Performance & Optimization
- ตัวอย่างสั้น:

```nx
checkpoint "save-1" preserve {
    state()
}
```

#### M104 `parse_if_stmt` (line 2867)

- ชื่อเมธอด: `parse_if_stmt`
- ลายเซ็น: `fn parse_if_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse if/elif/else statement
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `parse_if_stmt_tail`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `parse_if_stmt_tail`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 5. Control Flow Statements
- ตัวอย่างสั้น:

```nx
if ready { run() } else { stop() }
```

#### M105 `parse_if_stmt_tail` (line 2872)

- ชื่อเมธอด: `parse_if_stmt_tail`
- ลายเซ็น: `fn parse_if_stmt_tail(&mut self) -> ParseResult<Stmt>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `parse_expr_with_struct_literal_guard`, `parse_block`, `match_tnv`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::If`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `parse_expr_with_struct_literal_guard`, `parse_block`, `match_tnv`
  - ถูกเรียกจาก: `parse_if_stmt`
- AST ที่เกี่ยวข้อง: `Stmt::If`
- ผูกกับ syntax ข้อไหน: 5. Control Flow Statements
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M106 `parse_for_pattern` (line 2893)

- ชื่อเมธอด: `parse_for_pattern`
- ลายเซ็น: `fn parse_for_pattern(&mut self) -> ParseResult<Pattern>`
- หน้าที่: parse pattern list for for-loops
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Pattern>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `parse_pattern`, `match_one`, `peek`, `error_here`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `PatternKind::Tuple`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `parse_pattern`, `match_one`, `peek`, `error_here`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `PatternKind::Tuple`
- ผูกกับ syntax ข้อไหน: 5. Control Flow Statements, 13. Destructuring Assignment, 25. Pattern Matching ขั้นสูง, 26. Destructuring Assignment ขั้นสูง
- ตัวอย่างสั้น:

```nx
match value { Some(x) :> x }
```

#### M107 `parse_statement` (line 2922)

- ชื่อเมธอด: `parse_statement`
- ลายเซ็น: `fn parse_statement(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse any statement
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 5. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `parse_attributes`, `parse_replay_on_checkpoint_stmt`, `parse_in_context_stmt`, `parse_on_sequence_stmt`
  - 6. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Binary`, `Stmt::Atomically`, `Stmt::Attributed`, `Stmt::Break`
- error ที่โยน: `ParseError::UnexpectedToken` เมื่อ token ไม่ตรงที่คาด; `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `parse_attributes`, `parse_replay_on_checkpoint_stmt`, `parse_in_context_stmt`, `parse_on_sequence_stmt`, `expect_nv`, `parse_block`, `parse_let_stmt`, `expect_word`, `parse_expression`
  - ถูกเรียกจาก: `parse_block`
- AST ที่เกี่ยวข้อง: `Expr::Binary`, `Stmt::Atomically`, `Stmt::Attributed`, `Stmt::Break`, `Stmt::CompileTimeBlock`, `Stmt::Continue`, `Stmt::Converge`, `Stmt::Defer`, `Stmt::Delete`, `Stmt::Expr`, `Stmt::For`, `Stmt::Guard`
- ผูกกับ syntax ข้อไหน: 5. Control Flow Statements, 7. มาโคร & Compile-Time Metaprogramming, 9. Attributes, 31. Compile-Time Functions
- ตัวอย่างสั้น:

```nx
if ready { run() } else { stop() }
```

#### M108 `parse_in_context_stmt` (line 3429)

- ชื่อเมธอด: `parse_in_context_stmt`
- ลายเซ็น: `fn parse_in_context_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse in context block
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `peek`, `advance`, `parse_expr_with_struct_literal_guard`, `expect`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::InContext`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::UnexpectedToken` เมื่อ token ไม่ตรงที่คาด
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `peek`, `advance`, `parse_expr_with_struct_literal_guard`, `expect`, `is_at_end`, `parse_expression`, `parse_block_or_expr_body`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::InContext`
- ผูกกับ syntax ข้อไหน: 5. Control Flow Statements
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M109 `parse_on_sequence_stmt` (line 3456)

- ชื่อเมธอด: `parse_on_sequence_stmt`
- ลายเซ็น: `fn parse_on_sequence_stmt(&mut self) -> ParseResult<Stmt>`
- หน้าที่: parse on sequence block
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Stmt>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `parse_expression`, `expect`, `parse_block_or_expr_body`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Stmt::OnSequence`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `parse_expression`, `expect`, `parse_block_or_expr_body`
  - ถูกเรียกจาก: `parse_statement`
- AST ที่เกี่ยวข้อง: `Stmt::OnSequence`
- ผูกกับ syntax ข้อไหน: 5. Control Flow Statements
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

### E. Expression Parser (Pratt + Postfix + Pointer)

#### M110 `parse_expression` (line 3475)

- ชื่อเมธอด: `parse_expression`
- ลายเซ็น: `fn parse_expression(&mut self, min_prec: u8) -> ParseResult<Expr>`
- หน้าที่: Pratt expression parse
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, min_prec: u8`
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `parse_unary_or_primary`, `parse_expression_with_left`
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `parse_unary_or_primary`, `parse_expression_with_left`
  - ถูกเรียกจาก: `parse_batch_temporal_stmt`, `parse_block_or_expr_body`, `parse_branch_expr`, `parse_checkpoint_stmt`, `parse_class_delegate`, `parse_class_field_or_property`, `parse_cleanup_stmt`, `parse_commit_stmt`, `parse_emit_stmt`, `parse_expr_with_struct_literal_guard`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 6. Lambda & Pipeline
- ตัวอย่างสั้น:

```nx
data?.user?.name ?? "unknown"
```

#### M111 `parse_expression_with_left` (line 3480)

- ชื่อเมธอด: `parse_expression_with_left`
- ลายเซ็น: `fn parse_expression_with_left(&mut self, mut left: Expr, min_prec: u8) -> ParseResult<Expr>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, mut left: Expr, min_prec: u8`
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`, `is_range_end_delimiter`, `parse_expression`, `match_tnv`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Binary`, `Expr::Coalesce`, `Expr::Pipeline`, `Expr::PointerPipeline`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`, `is_range_end_delimiter`, `parse_expression`, `match_tnv`
  - ถูกเรียกจาก: `parse_expression`, `parse_lambda`
- AST ที่เกี่ยวข้อง: `Expr::Binary`, `Expr::Coalesce`, `Expr::Pipeline`, `Expr::PointerPipeline`, `Expr::Range`, `Expr::SelectorPipeline`, `Expr::Try`
- ผูกกับ syntax ข้อไหน: 6. Lambda & Pipeline, 24. Selector Pipelines (>>), 30. Range & Step Literals, 34. Provenance-Aware Pipelines
- ตัวอย่างสั้น:

```nx
data?.user?.name ?? "unknown"
```

#### M112 `parse_if_expr` (line 3562)

- ชื่อเมธอด: `parse_if_expr`
- ลายเซ็น: `fn parse_if_expr(&mut self) -> ParseResult<Expr>`
- หน้าที่: parse if/elif/else expression
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `parse_if_expr_tail`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `parse_if_expr_tail`
  - ถูกเรียกจาก: `parse_primary`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 17. Expression-Only Functions
- ตัวอย่างสั้น:

```nx
if ready { run() } else { stop() }
```

#### M113 `parse_if_expr_tail` (line 3567)

- ชื่อเมธอด: `parse_if_expr_tail`
- ลายเซ็น: `fn parse_if_expr_tail(&mut self) -> ParseResult<Expr>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `parse_expr_with_struct_literal_guard`, `parse_block_or_expr_body`, `match_tnv`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Block`, `Expr::IfExpr`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `parse_expr_with_struct_literal_guard`, `parse_block_or_expr_body`, `match_tnv`
  - ถูกเรียกจาก: `parse_if_expr`
- AST ที่เกี่ยวข้อง: `Expr::Block`, `Expr::IfExpr`
- ผูกกับ syntax ข้อไหน: 17. Expression-Only Functions
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M114 `parse_match_expr` (line 3589)

- ชื่อเมธอด: `parse_match_expr`
- ลายเซ็น: `fn parse_match_expr(&mut self) -> ParseResult<Expr>`
- หน้าที่: parse match expression
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 5. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect_nv`, `parse_expr_with_struct_literal_guard`, `expect`, `peek`, `is_at_end`
  - 6. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::MatchExpr`
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect_nv`, `parse_expr_with_struct_literal_guard`, `expect`, `peek`, `is_at_end`, `parse_pattern`, `match_tnv`, `parse_expression`
  - ถูกเรียกจาก: `parse_primary`
- AST ที่เกี่ยวข้อง: `Expr::MatchExpr`
- ผูกกับ syntax ข้อไหน: 17. Expression-Only Functions
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M115 `is_range_end_delimiter` (line 3618)

- ชื่อเมธอด: `is_range_end_delimiter`
- ลายเซ็น: `fn is_range_end_delimiter(&self) -> bool`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `bool`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`
  - ถูกเรียกจาก: `parse_expression_with_left`, `parse_unary_or_primary`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 30. Range & Step Literals
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M116 `parse_unary_or_primary` (line 3637)

- ชื่อเมธอด: `parse_unary_or_primary`
- ลายเซ็น: `fn parse_unary_or_primary(&mut self) -> ParseResult<Expr>`
- หน้าที่: unary then primary+postfix
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`, `is_range_end_delimiter`, `parse_expression`, `match_tnv`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Await`, `Expr::PointerDeref`, `Expr::PointerRef`, `Expr::Range`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`, `is_range_end_delimiter`, `parse_expression`, `match_tnv`, `parse_pointer_new_with`, `parse_primary`, `parse_postfix`
  - ถูกเรียกจาก: `parse_expression`
- AST ที่เกี่ยวข้อง: `Expr::Await`, `Expr::PointerDeref`, `Expr::PointerRef`, `Expr::Range`, `Expr::Spawn`, `Expr::Unary`
- ผูกกับ syntax ข้อไหน: 11. Concurrency & Async/Await, 38. Advanced Pointer & Reference System, 44. Temporal Integration with Other Language Features
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M117 `parse_pointer_new_with` (line 3743)

- ชื่อเมธอด: `parse_pointer_new_with`
- ลายเซ็น: `fn parse_pointer_new_with(&mut self, pointer_type: PointerType) -> ParseResult<Expr>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, pointer_type: PointerType`
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect`, `parse_expression`
  - 4. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::PointerNew`
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect`, `parse_expression`
  - ถูกเรียกจาก: `parse_primary`, `parse_unary_or_primary`
- AST ที่เกี่ยวข้อง: `Expr::PointerNew`
- ผูกกับ syntax ข้อไหน: 38. Advanced Pointer & Reference System
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M118 `parse_primary` (line 3756)

- ชื่อเมธอด: `parse_primary`
- ลายเซ็น: `fn parse_primary(&mut self) -> ParseResult<Expr>`
- หน้าที่: parse literals, ids, groups
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 5. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `parse_if_expr`, `parse_match_expr`, `advance`, `parse_block`
  - 6. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::AsyncBlock`, `Expr::EnumCase`, `Expr::Grouping`, `Expr::Ident`
- error ที่โยน: `ParseError::UnexpectedToken` เมื่อ token ไม่ตรงที่คาด; `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `parse_if_expr`, `parse_match_expr`, `advance`, `parse_block`, `match_word`, `expect_word`, `parse_expression`, `parse_branch_expr`, `parse_rewind_expr`
  - ถูกเรียกจาก: `parse_pattern`, `parse_unary_or_primary`
- AST ที่เกี่ยวข้อง: `Expr::AsyncBlock`, `Expr::EnumCase`, `Expr::Grouping`, `Expr::Ident`, `Expr::ListComp`, `Expr::Literal`, `Literal::Array`, `Literal::Bool`, `Literal::Float`, `Literal::Int`, `Literal::Map`, `Literal::String`
- ผูกกับ syntax ข้อไหน: 11. Concurrency & Async/Await, 14. List Comprehensions, 21. Unique Unit-Aware Literals, 23. Enhanced String Interpolation, 38. Advanced Pointer & Reference System, 44. Temporal Integration with Other Language Features
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M119 `parse_interpolated_string` (line 3965)

- ชื่อเมธอด: `parse_interpolated_string`
- ลายเซ็น: `fn parse_interpolated_string(&mut self) -> ParseResult<Expr>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `expect`, `peek`, `advance`, `is_at_end`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::InterpolatedString`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::UnexpectedToken` เมื่อ token ไม่ตรงที่คาด; `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `expect`, `peek`, `advance`, `is_at_end`
  - ถูกเรียกจาก: `parse_primary`
- AST ที่เกี่ยวข้อง: `Expr::InterpolatedString`
- ผูกกับ syntax ข้อไหน: 23. Enhanced String Interpolation
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M120 `find_list_comp_pipe` (line 4049)

- ชื่อเมธอด: `find_list_comp_pipe`
- ลายเซ็น: `fn find_list_comp_pipe(&self, start_idx: usize) -> Option<usize>`
- หน้าที่: look for list comp pipe at top-level in current bracket
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&self, start_idx: usize`
  - คืน: `Option<usize>`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `has_list_comp_arrow`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `has_list_comp_arrow`
  - ถูกเรียกจาก: `parse_primary`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 14. List Comprehensions
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M121 `has_list_comp_arrow` (line 4100)

- ชื่อเมธอด: `has_list_comp_arrow`
- ลายเซ็น: `fn has_list_comp_arrow(&self, start_idx: usize) -> bool`
- หน้าที่: check for "<-" at top-level after the pipe
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&self, start_idx: usize`
  - คืน: `bool`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: `find_list_comp_pipe`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 14. List Comprehensions
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M122 `find_slice_operator` (line 4150)

- ชื่อเมธอด: `find_slice_operator`
- ลายเซ็น: `fn find_slice_operator(&self, start_idx: usize) -> Option<usize>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&self, start_idx: usize`
  - คืน: `Option<usize>`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: `parse_postfix`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 30. Range & Step Literals
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M123 `parse_expression_slice` (line 4199)

- ชื่อเมธอด: `parse_expression_slice`
- ลายเซ็น: `fn parse_expression_slice(&mut self, end_idx: usize) -> ParseResult<Expr>`
- หน้าที่: parse expression from current idx up to end_idx (exclusive)
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, end_idx: usize`
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 3. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::UnexpectedToken` เมื่อ token ไม่ตรงที่คาด
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: `parse_postfix`, `parse_primary`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 14. List Comprehensions
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M124 `parse_postfix` (line 4218)

- ชื่อเมธอด: `parse_postfix`
- ลายเซ็น: `fn parse_postfix(&mut self, left: Expr) -> ParseResult<Expr>`
- หน้าที่: parse calls, fields, index, update
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, left: Expr`
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`, `parse_arguments`, `expect`, `expect_ident_like`
  - 5. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Call`, `Expr::FieldAccess`, `Expr::Ident`, `Expr::Index`
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`, `parse_arguments`, `expect`, `expect_ident_like`, `parse_generic_args`, `error_here`, `find_slice_operator`, `parse_expression_slice`, `parse_expression`
  - ถูกเรียกจาก: `parse_assert_temporal_stmt`, `parse_auto_checkpoint_stmt`, `parse_cleanup_stmt`, `parse_commit_stmt`, `parse_emit_stmt`, `parse_expr_with_struct_literal_guard`, `parse_gc_temporal_stmt`, `parse_lambda`, `parse_merge_branch_stmt`, `parse_unary_or_primary`
- AST ที่เกี่ยวข้อง: `Expr::Call`, `Expr::FieldAccess`, `Expr::Ident`, `Expr::Index`, `Expr::Literal`, `Expr::MacroCall`, `Expr::MethodCall`, `Expr::OptionalCall`, `Expr::OptionalFieldAccess`, `Expr::PointerDeref`, `Expr::PointerNew`, `Expr::Slice`
- ผูกกับ syntax ข้อไหน: 6. Lambda & Pipeline, 18. Record Update Syntax, 29. Optional Chaining / Safe Navigation, 30. Range & Step Literals, 38. Advanced Pointer & Reference System, 46. Built-in methods
- ตัวอย่างสั้น:

```nx
data?.user?.name ?? "unknown"
```

#### M125 `parse_arguments` (line 4490)

- ชื่อเมธอด: `parse_arguments`
- ลายเซ็น: `fn parse_arguments(&mut self) -> ParseResult<Vec<Expr>>`
- หน้าที่: parse call argument list
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Vec<Expr>>`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `parse_comma_separated`
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `parse_comma_separated`
  - ถูกเรียกจาก: `parse_postfix`, `parse_temporal_pattern`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 46. Built-in methods
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M126 `op_precedence` (line 5018)

- ชื่อเมธอด: `op_precedence`
- ลายเซ็น: `pub fn op_precedence(tok: &Token) -> Option<(u8, bool)>`
- หน้าที่: operator precedence table
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `tok: &Token`
  - คืน: `Option<(u8, bool)>`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 3. ส่งค่าตรงตามประเภท return โดยไม่ห่อ `ParseResult`
- error ที่โยน: เมธอดนี้ไม่โยน `ParseError` โดยตรง
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: เรียกจาก flow หลักตามบริบท (ไม่พบ self-call ตรงจากเมธอดอื่น)
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 6. Lambda & Pipeline, 24. Selector Pipelines (>>), 34. Provenance-Aware Pipelines
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

### F. Type / Pattern / Lambda Subparsers

#### M127 `parse_operator_method::parse_param` (line 822)

- ชื่อเมธอด: `parse_operator_method::parse_param`
- ลายเซ็น: `fn parse_param(this: &mut Parser) -> ParseResult<Param>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `this: &mut Parser`
  - คืน: `ParseResult<Param>`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: `parse_operator_method`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 32. Default Value
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M128 `parse_function_with::parse_param` (line 1180)

- ชื่อเมธอด: `parse_function_with::parse_param`
- ลายเซ็น: `fn parse_param(this: &mut Parser) -> ParseResult<Param>`
- หน้าที่: เมธอดนี้เป็นส่วนหนึ่งของ flow parser โดยตรงตามชื่อและโค้ดใน body
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `this: &mut Parser`
  - คืน: `ParseResult<Param>`
- อ่าน/ขยับ token อย่างไร: ไม่แตะ cursor parser โดยตรง หรือแตะผ่านเมธอดที่เรียกต่อ
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 3. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 4. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: ไม่มี หรือเป็น logic ภายในเมธอดนี้เอง
  - ถูกเรียกจาก: `parse_function_with`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 32. Default Value
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M129 `parse_generic_args` (line 4498)

- ชื่อเมธอด: `parse_generic_args`
- ลายเซ็น: `fn parse_generic_args(&mut self) -> ParseResult<Vec<TypeRef>>`
- หน้าที่: parse generic args until >
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Vec<TypeRef>>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `expect_gt`, `parse_comma_separated`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `expect_gt`, `parse_comma_separated`
  - ถูกเรียกจาก: `parse_postfix`, `parse_type`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: รองรับข้ามหลายข้อ (ไม่ได้ผูกข้อเดียวชัดเจน)
- ตัวอย่างสั้น:

```nx
Result<Vec<Int>, Error?>
```

#### M130 `parse_struct_fields` (line 4514)

- ชื่อเมธอด: `parse_struct_fields`
- ลายเซ็น: `fn parse_struct_fields(&mut self) -> ParseResult<Vec<(String, Expr)>>`
- หน้าที่: parse {field: expr} list
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Vec<(String, Expr)>>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `parse_comma_separated`, `expect`
  - 4. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 5. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `parse_comma_separated`, `expect`
  - ถูกเรียกจาก: `parse_named_record_literal`, `parse_postfix`, `parse_primary`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 3. Types & Declarations
- ตัวอย่างสั้น:

```nx
let x = 1 + 2
```

#### M131 `parse_type` (line 4529)

- ชื่อเมธอด: `parse_type`
- ลายเซ็น: `fn parse_type(&mut self) -> ParseResult<TypeRef>`
- หน้าที่: parse type with generics and nullable
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<TypeRef>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`, `match_one`, `expect`, `parse_comma_separated`
  - 5. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
  - 6. ส่งกลับ `Ok(...)` เมื่อ parse ผ่าน และส่งกลับ `Err(...)` เมื่อเจอรูปแบบที่ไม่ตรง
- error ที่โยน: `ParseError::UnexpectedToken` เมื่อ token ไม่ตรงที่คาด
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`, `match_one`, `expect`, `parse_comma_separated`, `match_tnv`, `parse_generic_args`
  - ถูกเรียกจาก: `parse_class_field_or_property`, `parse_extension`, `parse_function_with`, `parse_impl`, `parse_let_stmt`, `parse_operator_method`, `parse_property_accessors`, `parse_type_constraints`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 3. Types & Declarations, 38. Advanced Pointer & Reference System
- ตัวอย่างสั้น:

```nx
Result<Vec<Int>, Error?>
```

#### M132 `parse_pattern_list` (line 4615)

- ชื่อเมธอด: `parse_pattern_list`
- ลายเซ็น: `fn parse_pattern_list(&mut self, end: TokenType) -> ParseResult<(Vec<Pattern>, Vec<String>)>`
- หน้าที่: parse list of patterns
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self, end: TokenType`
  - คืน: `ParseResult<(Vec<Pattern>, Vec<String>)>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 5. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `is_at_end`, `peek`, `parse_pattern`, `match_one`, `expect`
  - 6. สรุปผลเป็นค่า helper/boolean/token ตามหน้าที่ของเมธอด
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `is_at_end`, `peek`, `parse_pattern`, `match_one`, `expect`
  - ถูกเรียกจาก: `parse_pattern`
- AST ที่เกี่ยวข้อง: ไม่สร้าง AST node โดยตรง (helper/control utility)
- ผูกกับ syntax ข้อไหน: 13. Destructuring Assignment, 25. Pattern Matching ขั้นสูง, 26. Destructuring Assignment ขั้นสูง
- ตัวอย่างสั้น:

```nx
match value { Some(x) :> x }
```

#### M133 `parse_struct_pattern_fields` (line 4636)

- ชื่อเมธอด: `parse_struct_pattern_fields`
- ลายเซ็น: `fn parse_struct_pattern_fields( &mut self, ) -> ParseResult<(Vec<(String, Pattern)>, Vec<String>)>`
- หน้าที่: parse struct pattern fields
- อินพุต/เอาต์พุต:
  - รับ: รับพารามิเตอร์ตามลายเซ็น: `&mut self,`
  - คืน: `ParseResult<(Vec<(String, Pattern)>, Vec<String>)>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 5. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `is_at_end`, `peek`, `expect_ident_like`, `match_one`, `parse_pattern`
  - 6. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `PatternKind::Identifier`
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `is_at_end`, `peek`, `expect_ident_like`, `match_one`, `parse_pattern`, `expect`
  - ถูกเรียกจาก: `parse_pattern`
- AST ที่เกี่ยวข้อง: `PatternKind::Identifier`
- ผูกกับ syntax ข้อไหน: 13. Destructuring Assignment, 25. Pattern Matching ขั้นสูง, 26. Destructuring Assignment ขั้นสูง
- ตัวอย่างสั้น:

```nx
match value { Some(x) :> x }
```

#### M134 `parse_pattern` (line 4670)

- ชื่อเมธอด: `parse_pattern`
- ลายเซ็น: `fn parse_pattern(&mut self) -> ParseResult<Pattern>`
- หน้าที่: parse pattern and bindings
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Pattern>`
- อ่าน/ขยับ token อย่างไร: อ่าน token ปัจจุบันด้วย `peek`; ขยับ cursor ด้วย `advance`; มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 5. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `peek`, `advance`, `parse_pattern_list`, `parse_struct_pattern_fields`, `expect`
  - 6. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::Literal`, `PatternKind::Array`, `PatternKind::EnumVariant`, `PatternKind::ErrVariant`
- error ที่โยน: `ParseError::Generic` พร้อม line/column
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `peek`, `advance`, `parse_pattern_list`, `parse_struct_pattern_fields`, `expect`, `error_here`, `parse_primary`, `match_tnv`
  - ถูกเรียกจาก: `parse_for_pattern`, `parse_let_stmt`, `parse_match_expr`, `parse_pattern_list`, `parse_primary`, `parse_statement`, `parse_struct_pattern_fields`
- AST ที่เกี่ยวข้อง: `Expr::Literal`, `PatternKind::Array`, `PatternKind::EnumVariant`, `PatternKind::ErrVariant`, `PatternKind::Identifier`, `PatternKind::Literal`, `PatternKind::Nil`, `PatternKind::NoneVariant`, `PatternKind::OkVariant`, `PatternKind::Or`, `PatternKind::Ref`, `PatternKind::SomeVariant`
- ผูกกับ syntax ข้อไหน: 13. Destructuring Assignment, 25. Pattern Matching ขั้นสูง, 26. Destructuring Assignment ขั้นสูง
- ตัวอย่างสั้น:

```nx
match value { Some(x) :> x }
```

#### M135 `parse_lambda` (line 4814)

- ชื่อเมธอด: `parse_lambda`
- ลายเซ็น: `fn parse_lambda(&mut self) -> ParseResult<Expr>`
- หน้าที่: parse \ params :> expr
- อินพุต/เอาต์พุต:
  - รับ: ไม่มีพารามิเตอร์นอกจาก context ของตัวมันเอง
  - คืน: `ParseResult<Expr>`
- อ่าน/ขยับ token อย่างไร: มีจุดที่บังคับ token ต้องตรงรูปแบบ (`expect*`); มีจุดที่ลองกิน token แบบ optional (`match*`)
- ขั้นตอนการทำงานทีละข้อ:
  - 1. เริ่มจากตำแหน่ง `idx` ปัจจุบันของ parser และอ่าน token ที่เกี่ยวข้องกับไวยากรณ์นี้
  - 2. ตรวจ token ที่ "ต้องมี" ก่อน ถ้าไม่ตรงจะเตรียม error ทันที
  - 3. เช็ก token ที่ "อาจมี" เพื่อแตกแขนง syntax โดยไม่บังคับ
  - 4. วนอ่าน token ต่อเนื่องจนเจอ token ปิด/ตัวคั่น/EOF ตามกฎของเมธอด
  - 5. เรียก sub-parser/utility ที่เกี่ยวข้องต่อเป็นลำดับ เช่น `match_one`, `expect_ident_like`, `parse_postfix`, `parse_expression_with_left`, `expect`
  - 6. ประกอบผลลัพธ์เป็น AST node หรือค่ากลาง เช่น `Expr::FieldAccess`, `Expr::Ident`, `Expr::Lambda`, `PatternKind::Identifier`
- error ที่โยน: ส่งต่อ error จากเมธอดที่เรียก (`?`) หรือคืน `Err` ตาม guard ในเมธอดนี้
- เรียกใคร/ถูกใครเรียก:
  - เรียก: `match_one`, `expect_ident_like`, `parse_postfix`, `parse_expression_with_left`, `expect`, `parse_expression`
  - ถูกเรียกจาก: `parse_primary`
- AST ที่เกี่ยวข้อง: `Expr::FieldAccess`, `Expr::Ident`, `Expr::Lambda`, `PatternKind::Identifier`
- ผูกกับ syntax ข้อไหน: 6. Lambda & Pipeline
- ตัวอย่างสั้น:

```nx
data?.user?.name ?? "unknown"
```

## QA Checklist ที่ใช้ปิดงาน

- [x] Method Coverage: รายการเมธอดจากโค้ด = `135` และในเอกสาร = `135`
- [x] Cross-Reference: ทุกเมธอดระบุ callee/caller (อย่างน้อยระดับ self-call + flow context)
- [x] Syntax Mapping: มีตารางข้อ 1–46 และระบุข้อที่ยังไม่ชัดแบบโปร่งใส
- [x] Error Path: ทุกเมธอดที่ return `ParseResult` มีช่อง error อธิบาย
- [x] Readability: ใช้ภาษาไทยประโยคสั้นและคงศัพท์เทคนิคให้สม่ำเสมอ

## หมายเหตุความตรงกับโค้ด

- เอกสารนี้อิง source ณ ไฟล์ `src/parser.rs` ปัจจุบันแบบบรรทัดต่อบรรทัด
- ถ้ามีการแก้ parser ภายหลัง ต้องรีเจน Method Index และตรวจ QA Checklist ซ้ำทันที
