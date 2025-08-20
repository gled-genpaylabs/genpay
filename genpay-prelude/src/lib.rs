use once_cell::sync::Lazy;
use std::sync::Mutex;
use genpay_lexer::token::Token;

/// Static, globally accessible vector for lexer tokens.
/// All crates should use this to avoid re-lexing or reading the file twice.
pub static TOKENS: Lazy<Mutex<Vec<Token<'static>>>> = Lazy::new(|| Mutex::new(Vec::new()));
