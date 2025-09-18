use bumpalo::Bump;
use clap::{CommandFactory, Parser};
use colored::Colorize;

/// Command Line Interface module
mod cli;

fn main() {
    // Getting args with advanced helper
    let args = cli::Args::try_parse().unwrap_or_else(|e| {
        let mut command = cli::Args::command();

        // Git commit hash where build was made. Check the `build.rs`
        let git_hash: String = env!("GIT_HASH").chars().take(8).collect();
        let version_fmt = format!("v{} {}", env!("CARGO_PKG_VERSION"), git_hash);

        let authors_env = env!("CARGO_PKG_AUTHORS");
        let authors_fmt = if authors_env.contains(":") {
            format!("\n| {}", authors_env.replace(":", "\n| "))
        } else {
            authors_env.to_owned()
        };

        match e.kind() {
            clap::error::ErrorKind::DisplayVersion => {
                // --version flag
                // Just return necessary information and exit with 0
                eprintln!("{}", "Genpay Programming Language".bold().cyan());
                eprintln!("| - version: {version_fmt}");
                eprintln!("| - authors: {authors_fmt}");

                std::process::exit(0);
            }
            _ => {
                // Wrong arguments or --help flag
                eprintln!("{}", "Genpay Programming Language".bold().cyan());
                eprintln!("| - version: {version_fmt}");
                eprintln!("| - authors: {authors_fmt}");
                eprintln!();
                eprintln!("{}", "Options:".bold().cyan());

                command.print_help().unwrap();

                eprintln!();
                eprintln!("{}", "Examples of usage:".bold().cyan());
                eprintln!("  genpay example.pay output");
                eprintln!("  genpay example.pay output --no-warns");
                eprintln!("  genpay example.pay output --include foo.c");

                // Checking for the error kind (if just --help flag returning without error)
                if e.kind() == clap::error::ErrorKind::DisplayHelp {
                    std::process::exit(0);
                }
                std::process::exit(1);
            }
        }
    });

    let no_warns = args.no_warns;

    // Getting filename from provided path
    let fname = args
        .path
        .file_name()
        .unwrap_or_else(|| {
            cli::error("Unable to find source file");
            std::process::exit(1);
        })
        .to_str()
        .unwrap_or_else(|| {
            cli::error("Unable to get source module name");
            std::process::exit(1);
        });

    cli::info(
        "Reading",
        &format!(
            "`{}` ({})",
            &fname,
            std::fs::canonicalize(&args.path)
                .unwrap_or_else(|_| {
                    cli::error(&format!("File `{}` does not exist", &fname));
                    std::process::exit(1);
                })
                .display()
        ),
    );

    // Reading it (on old devices it might take a little bit longer)
    let src = std::fs::read_to_string(&args.path).unwrap_or_else(|err| {
        eprintln!(
            "Unable to open path: {}. System error: {}",
            std::path::Path::new(&args.path).display(),
            err
        );
        std::process::exit(1);
    });

    cli::info(
        "Processing",
        &format!("tokens ({} lines of code)", src.lines().count()),
    );

    // `miette` graphical reporter (for this amazing error reports).
    let handler = miette::GraphicalReportHandler::new();
    let mut total_warns = 0;

    cli::info("Parsing", "syntax tree");

    let bump = Bump::new();

    // Syntax Analyzer initialization.
    let mut parser = genpay_parser::Parser::new(&src, fname.to_string(), &bump);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => {
            errors.iter().for_each(|e| {
                let mut buf = String::new();
                handler.render_report(&mut buf, e).unwrap();

                eprintln!("{buf}");
            });

            cli::error(&format!("`{}` returned {} errors", &fname, errors.len()));
            std::process::exit(1);
        }
    };

    cli::info(
        "Analyzing",
        &format!("processed code ({} global statements)", ast.len()),
    );

    // Semantical Analyzer initialization.
    //
    // Last argument `is_main` is used for imports functionality.
    // Imports aren't working currently | 20/06/2025 v0.0.4
    //
    // Analyzer takes only reference to AST (because we only provide checking)
    let analyzer = genpay_semantic::Analyzer::new(&src, fname, args.path.clone(), true, &bump);
    let (symtable, warns) = match analyzer.analyze(&ast) {
        Ok(res) => res,
        Err((errors, warns)) => {
            errors.iter().for_each(|e| {
                let mut buf = String::new();
                handler.render_report(&mut buf, e).unwrap();

                eprintln!("{buf}");
            });

            if !no_warns {
                warns.iter().for_each(|w| {
                    let mut buf = String::new();
                    handler.render_report(&mut buf, w).unwrap();

                    eprintln!("{buf}");
                });
            }

            cli::error(&format!("`{}` returned {} errors", &fname, errors.len()));
            if (!warns.is_empty() || total_warns > 0) && !no_warns {
                cli::warn(&format!(
                    "`{}` generated {} warnings",
                    &fname,
                    warns.len() + total_warns
                ));
            }

            std::process::exit(1);
        }
    };

    if !no_warns {
        warns.iter().for_each(|w| {
            let mut buf = String::new();
            handler.render_report(&mut buf, w).unwrap();

            eprintln!("{buf}");
        });
    }

    total_warns += warns.len();

    if total_warns > 0 && !no_warns {
        cli::warn(&format!("`{}` generated {} warnings", &fname, total_warns));
    }

    cli::info("Compiling", &format!("`{}` to binary", &fname,));

    // Extracting module name from filename (for codegen)
    let module_name = fname
        .split(".")
        .next()
        .map(|n| n.to_string())
        .unwrap_or(fname.replace(".pay", ""));

    // Combining linkages
    let linked_list: Vec<std::path::PathBuf> = symtable.linked.iter().cloned().collect();
    let external_linkages = [args.include, linked_list].concat();

    // Code Generator Initialization.
    // Creating custom context and a very big wrapper for builder.
    let ctx = genpay_codegen::CodeGen::create_context();
    let mut codegen = genpay_codegen::CodeGen::new(&ctx, &module_name, &src, symtable, &bump);

    // Compiling: AST -> LLVM IR Module Reference
    let (module_ref, _) = codegen.compile(ast.to_vec(), None);

    // --llvm argument allows user to export LLVM IR module into file
    if args.llvm {
        module_ref
            .print_to_file(format!("{}.ll", args.output.display()))
            .unwrap_or_else(|_| {
                cli::error("Unable to write LLVM IR file!");
                std::process::exit(1);
            });

        cli::info(
            "Successfully",
            &format!("compiled to LLVM IR: `{}.ll`", args.output.display()),
        )
    } else {
        genpay_linker::compiler::ObjectCompiler::compile_module(module_ref, &module_name);
        let compiler = genpay_linker::linker::ObjectLinker::link(
            &module_name,
            args.output.to_str().unwrap(),
            external_linkages,
        )
        .unwrap_or_else(|err| {
            let object_linker = genpay_linker::linker::ObjectLinker::detect_compiler()
                .unwrap_or(String::from("none"));
            cli::error(&format!(
                "Linker catched an error! (object linker: `{object_linker}`)"
            ));
            println!("\n{err}\n");

            cli::error(
                "Please make sure you linked all the necessary libraries (check the '-i' argument)",
            );
            std::process::exit(1);
        });

        let formatted_output = args.output.display().to_string();

        cli::info(
            "Successfully",
            &format!("compiled to binary (with `{compiler}`): `{formatted_output}`"),
        )
    }
}
