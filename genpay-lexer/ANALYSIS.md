# Genpay Lexer Analysis

This document provides a detailed analysis of the `genpay-lexer` crate.

## 1. Overview

The `genpay-lexer` is a foundational crate in the Genpay compiler toolchain. Its primary responsibility is to perform lexical analysis, which is the process of converting a stream of characters from the source code into a stream of tokens. These tokens represent the smallest meaningful units in the Genpay language, such as keywords, identifiers, literals, and operators.

The lexer is designed as a hand-written, iterator-based component. This approach provides a good balance of performance and flexibility, allowing for fine-grained control over the tokenization process.

## 2. Architecture and Implementation

The lexer's implementation is contained within the `genpay-lexer` crate and is organized into several key modules:

- **`lib.rs`**: This is the main module of the crate. It contains the `Lexer` struct, which is the core of the lexer. The `Lexer` is implemented as an iterator (`impl<'a> Iterator for Lexer<'a>`), which allows it to be used in a highly idiomatic and efficient way in Rust. The `next()` method of the iterator is where the main tokenization logic resides.

- **`token.rs`**: This module defines the `Token` struct. Each `Token` represents a single lexical unit and contains:
  - `value`: A string slice (`&'a str`) that holds the actual text of the token from the source code.
  - `token_type`: A `TokenType` enum that specifies the kind of the token.
  - `span`: A tuple `(usize, usize)` that marks the starting position and length of the token in the source code, which is crucial for error reporting.

- **`token_type.rs`**: This module defines the `TokenType` enum, which enumerates all possible kinds of tokens in the Genpay language. This includes:
  - Identifiers and keywords (e.g., `let`, `fn`).
  - Types (e.g., `i32`, `bool`).
  - Literals (e.g., numbers, strings, booleans).
  - Operators (e.g., `+`, `==`, `&&`).
  - Punctuation (e.g., parentheses, braces, semicolons).

- **`error.rs`**: This module defines the `LexerError` enum, which represents the various errors that can occur during lexical analysis, such as unterminated strings or unknown characters.

## 3. Tokenization Process

The tokenization process begins in the `next()` method of the `Lexer` iterator. The lexer consumes the source code character by character, making decisions based on the current character. The overall process can be summarized as follows:

1.  **Whitespace and Comments**: The lexer first skips any whitespace characters and single-line comments (`//`).

2.  **Character-based Tokenization**: The lexer then peeks at the current character to decide which kind of token to parse. It uses a `match` statement to handle different cases:
    - **Single-character tokens**: For simple, single-character tokens like `(`, `)`, `{`, `}`, `+`, `-`, etc., the lexer creates the corresponding token and advances its cursor.
    - **Multi-character tokens**: For tokens that can be one or two characters long (e.g., `=`, `==`, `!`, `!=`, `<`, `<=`), the lexer checks the next character to determine the correct token.
    - **Numbers**: If the character is a digit, the lexer consumes a sequence of digits to parse a number. It also handles floating-point numbers by looking for a decimal point.
    - **Strings and Chars**: Strings are parsed by consuming characters between double quotes (`"`), and characters are parsed between single quotes (`'`). The lexer handles basic escape sequences.
    - **Identifiers and Keywords**: If the character is alphabetic or an underscore, the lexer consumes a sequence of alphanumeric characters and underscores. It then uses the `lookup_identifier` function to determine if the identifier is a keyword (e.g., `if`, `let`), a built-in type (e.g., `i32`, `bool`), or a user-defined identifier.

## 4. Error Handling

The lexer is designed to be resilient and provide meaningful error messages. When it encounters a situation it cannot handle, it returns a `LexerError`. The `LexerError` enum includes variants for:
- Unterminated strings and characters.
- Unknown characters.
- Invalid number constants.

The use of the `miette` crate for error reporting suggests that the compiler will be able to produce user-friendly, graphical error messages that point directly to the location of the error in the source code.

## 5. Unit Tests

The lexer includes a comprehensive suite of unit tests within the `lib.rs` file. These tests cover a wide range of scenarios, including:
- Tokenizing different types of keywords and types.
- Parsing numbers (integers and floats).
- Handling strings with and without escape sequences.
- Recognizing all operators.
- Correctly handling comments and whitespace.
- Detecting and reporting errors like unterminated strings.

These tests are crucial for ensuring the correctness and stability of the lexer.

## 6. Potential Improvements

While the lexer is well-structured and functional, there are several areas where it could be improved:

1.  **Performance Optimization for Keyword Lookup**:
    - The `lookup_identifier` function currently uses a `match` statement to identify keywords. For a larger number of keywords, this could be less performant than using a `HashMap`. A `HashMap` would provide O(1) average time complexity for lookups, which could be a noticeable improvement if the language's keyword set grows.

2.  **Enhanced Error Reporting**:
    - The current error handling is good, but it could be made more user-friendly. For example, when an `UnknownCharacter` error is reported, the compiler could suggest possible corrections based on similarity to known operators or keywords (e.g., "did you mean `==`?" for a single `=`).

3.  **Support for More Number Formats**:
    - The lexer currently supports decimal integers and floating-point numbers. Adding support for other common number formats, such as hexadecimal (`0x...`), octal (`0o...`), and binary (`0b...`), would make the language more versatile, especially for systems programming tasks.

4.  **Support for Block Comments**:
    - The lexer only supports single-line comments (`// ...`). The addition of multi-line or block comments (`/* ... */`) would be a significant quality-of-life improvement for developers, allowing them to comment out large blocks of code easily.
