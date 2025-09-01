use phf_codegen::Map;
use std::env;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

fn main() {
    let path = Path::new(&env::var("OUT_DIR").unwrap()).join("keywords.rs");
    let mut file = BufWriter::new(File::create(&path).unwrap());

    writeln!(
        &mut file,
        "static KEYWORDS: phf::Map<&'static str, TokenType> = {}",
        Map::new()
            .entry("let", "TokenType::Keyword")
            .entry("pub", "TokenType::Keyword")
            .entry("fn", "TokenType::Keyword")
            .entry("import", "TokenType::Keyword")
            .entry("include", "TokenType::Keyword")
            .entry("extern", "TokenType::Keyword")
            .entry("return", "TokenType::Keyword")
            .entry("struct", "TokenType::Keyword")
            .entry("enum", "TokenType::Keyword")
            .entry("typedef", "TokenType::Keyword")
            .entry("if", "TokenType::Keyword")
            .entry("else", "TokenType::Keyword")
            .entry("while", "TokenType::Keyword")
            .entry("for", "TokenType::Keyword")
            .entry("break", "TokenType::Keyword")
            .entry("true", "TokenType::Boolean")
            .entry("false", "TokenType::Boolean")
            .entry("NULL", "TokenType::Null")
            .entry("i8", "TokenType::Type")
            .entry("i16", "TokenType::Type")
            .entry("i32", "TokenType::Type")
            .entry("i64", "TokenType::Type")
            .entry("u8", "TokenType::Type")
            .entry("u16", "TokenType::Type")
            .entry("u32", "TokenType::Type")
            .entry("u64", "TokenType::Type")
            .entry("usize", "TokenType::Type")
            .entry("f32", "TokenType::Type")
            .entry("f64", "TokenType::Type")
            .entry("bool", "TokenType::Type")
            .entry("char", "TokenType::Type")
            .entry("void", "TokenType::Type")
            .build()
    ).unwrap();
    writeln!(&mut file, ";").unwrap();
}
