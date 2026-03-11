use super::super::*;

impl Parser {
    pub(in crate::parser) fn is_function_modifier_keyword(kw: &str) -> bool {
        matches!(kw, "async" | "constexpr" | "comptime" | "scope")
    }
}
