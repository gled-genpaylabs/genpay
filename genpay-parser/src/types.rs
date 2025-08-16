//! # Types
//! **Types** is a designation of data size and it's avaible toolchain. <br/>
//! Deen has 14 basic types: 4 signed and 5 unsigned integers, 2 floats, char, bool and void types.
//! <br/>
//! Other types is _advanced_ (pointers, arrays, structs and etc.)

use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<'s> {
    SelfRef,
    Undefined,
    NoDrop,

    I8,
    I16,
    I32,
    I64,

    F32,
    F64,

    U8,
    U16,
    U32,
    U64,
    USIZE,

    Bool,
    Void,
    Null,
    Char,

    Pointer(Box<Type<'s>>),
    Array(Box<Type<'s>>, usize),
    DynamicArray(Box<Type<'s>>),

    Tuple(Vec<Type<'s>>),
    Alias(&'s str),

    // for semantical analyzer
    Function(Vec<Type<'s>>, Box<Type<'s>>, bool), // fn foo(a: i32, b: u32) string  --->  Function([I32, U32], String)
    Struct(BTreeMap<&'s str, Type<'s>>, BTreeMap<&'s str, Type<'s>>), // struct Abc { a: i32, b: bool, c: *u64 }  ---> Struct([I32, Bool, Pointer(U64)])
    Enum(Vec<&'s str>, BTreeMap<&'s str, Type<'s>>), // enum Abc { A, B, C } -> Enum([A, B, C])

    ImportObject(&'s str),
}

impl<'s> std::fmt::Display for Type<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::SelfRef => write!(f, "&self"),
            Type::Undefined => write!(f, "undefined"),
            Type::NoDrop => write!(f, "nodrop"),

            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),

            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),

            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::USIZE => write!(f, "usize"),

            Type::Char => write!(f, "char"),
            Type::Bool => write!(f, "bool"),
            Type::Null => write!(f, "null"),
            Type::Void => write!(f, "void"),

            Type::Pointer(inner) => write!(f, "*{inner}"),
            Type::Array(inner, size) => write!(f, "[{inner}; {size}]"),
            Type::DynamicArray(inner) => write!(f, "[]{inner}"),

            Type::Tuple(elements) => {
                write!(f, "(")?;
                for (i, ty) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{ty}")?;
                }
                write!(f, ")")
            }

            Type::Alias(alias) => write!(f, "{alias}"),
            Type::Function(args, functype, is_var_args) => write!(
                f,
                "{functype} ({}{})",
                args.iter()
                    .map(|a| format!("{a}"))
                    .collect::<Vec<String>>()
                    .join(", "),
                if *is_var_args { ", ..." } else { "" }
            ),
            Type::Struct(args, _) => write!(
                f,
                "struct {{ {} }}",
                args.iter()
                    .map(|a| format!("{}", a.1))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Type::Enum(args, _) => write!(
                f,
                "enum {{ {} }}",
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),

            Type::ImportObject(imp) => write!(f, "<{imp}>"),
        }
    }
}
