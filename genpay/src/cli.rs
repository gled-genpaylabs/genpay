use clap::{Parser, ValueEnum};
use colored::Colorize;
use std::path::PathBuf;

/// The compiler backend to use.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum Backend {
    Llvm,
    Cranelift,
}

/// Command Line Interface with [`clap`]
#[derive(Parser, Debug)]
#[command(
    author,
    version,
    about = "Compiler for Genpay Programming Language",
    long_about = None,
    help_template = "{options}",
)]
pub struct Args {
    /// Path to source code
    pub path: PathBuf,
    /// Path to output file
    pub output: PathBuf,

    /// `-n --no-warns` flag to disable compiler's warnings
    #[arg(short, long, action, help = "Disable compiler's warnings")]
    pub no_warns: bool,

    /// `-i --include` flag to link C library to linker
    #[arg(short, long, action, help = "Include C library to linker")]
    pub include: Vec<PathBuf>,

    /// Choose the compiler backend
    #[arg(long, value_enum, default_value_t = Backend::Llvm, help = "Choose the compiler backend")]
    pub backend: Backend,
}

/// Prints formatted red error message to _stderr_
pub fn error(message: &str) {
    eprintln!("{} {}", "Error:".red().bold(), message);
}

/// Prints formatted green info message to _stdout_
pub fn info(start: &str, message: &str) {
    println!("{} {}", start.green().bold(), message);
}

/// Prints formatted yellow warning message to _stdout_
pub fn warn(message: &str) {
    println!("{} {}", "Warning:".yellow().bold(), message);
}
