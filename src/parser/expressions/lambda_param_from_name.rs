use super::super::*;

impl Parser {
    pub(in crate::parser) fn lambda_param_from_name(name: String) -> Param {
        Param {
            pattern: Pattern {
                kind: PatternKind::Identifier(name.clone()),
                bindings: vec![name],
            },
            typ: None,
            default: None,
        }
    }


}
