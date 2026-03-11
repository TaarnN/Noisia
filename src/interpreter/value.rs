use std::collections::BTreeMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Unit,
    Array(Vec<Value>),
    Tuple(Vec<Value>),
    Map(Vec<(Value, Value)>),
    Struct {
        name: String,
        fields: BTreeMap<String, Value>,
    },
    Function(String),
    Phrase(String),
    EnumCase(String),
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Int(_) => "Int",
            Value::Float(_) => "Float",
            Value::Bool(_) => "Bool",
            Value::String(_) => "String",
            Value::Unit => "Unit",
            Value::Array(_) => "Array",
            Value::Tuple(_) => "Tuple",
            Value::Map(_) => "Map",
            Value::Struct { .. } => "Struct",
            Value::Function(_) => "Function",
            Value::Phrase(_) => "Phrase",
            Value::EnumCase(_) => "EnumCase",
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(v) => *v,
            Value::Unit => false,
            Value::Int(v) => *v != 0,
            Value::Float(v) => *v != 0.0,
            Value::String(v) => !v.is_empty(),
            Value::Array(v) => !v.is_empty(),
            Value::Tuple(v) => !v.is_empty(),
            Value::Map(v) => !v.is_empty(),
            Value::Struct { .. } => true,
            Value::Function(_) => true,
            Value::Phrase(v) => !v.is_empty(),
            Value::EnumCase(_) => true,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{v}"),
            Value::Float(v) => write!(f, "{v}"),
            Value::Bool(v) => write!(f, "{v}"),
            Value::String(v) => write!(f, "{v}"),
            Value::Unit => write!(f, "()"),
            Value::Array(values) => {
                write!(f, "[")?;
                for (idx, value) in values.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{value}")?;
                }
                write!(f, "]")
            }
            Value::Tuple(values) => {
                write!(f, "(")?;
                for (idx, value) in values.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{value}")?;
                }
                write!(f, ")")
            }
            Value::Map(entries) => {
                write!(f, "[")?;
                for (idx, (key, value)) in entries.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key}: {value}")?;
                }
                write!(f, "]")
            }
            Value::Struct { name, fields } => {
                write!(f, "{name} {{")?;
                for (idx, (field, value)) in fields.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{field}: {value}")?;
                }
                write!(f, "}}")
            }
            Value::Function(name) => write!(f, "<fn {name}>"),
            Value::Phrase(v) => write!(f, "{v}"),
            Value::EnumCase(v) => write!(f, ".{v}"),
        }
    }
}
