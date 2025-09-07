[Latest Release]: https://github.com/gled-genpaylabs/genpay/releases/latest

<div align="center">
  <picture>
    <img src="https://github.com/gled-genpaylabs/genpay/blob/main/assets/GenPayLogo.png" width="35%" />
    
  </picture>

  <div>
    <h1>Genpay Programming Language</h1>
    <i>A language for smart contract</i>
  </div>
  <br/>
  <div>
    <a href="https://github.com/gled-genpaylabs/genpay/releases/latest"><img src="https://img.shields.io/badge/dynamic/toml?url=https%3A%2F%2Fraw.githubusercontent.com%gled-genpaylabs%2Fgenpay%2Frefs%2Fheads%2Fmaster%2FCargo.toml%3Fraw%3Dtrue&query=workspace.package.version&logo=hackthebox&logoColor=fff&label=version&color=%2319a63e" /></a>
    <a href="https://github.com/gled-genpaylabs/genpay" /><img src="https://img.shields.io/github/actions/workflow/status/gled-genpaylabs/genpay/test.yml?logo=speedtest&logoColor=fff&label=tests&color=19a63e" /></a>
    <a href="https://github.com/gled-genpaylabs/genpay/blob/master/LICENSE"><img src="https://img.shields.io/github/license/gled-genpaylabs/genpay?style=flat&color=%2319a63e&logo=opensourcehardware&logoColor=fff" /></a>
  </div>
</div>

## Description
**Genpay** - a statically-typed compiling programming language for smart contracts and system tools. <br><br>
See official documentation here: [Genpay Documentation](https://genpay-site.vercel.app/)

##  Features
*  **Simplicity**. The language syntax is easy to read and write.
*  **Fast**. The compiler uses LLVM as a backend for the best performance.
*  **Clean**. Nothing superfluous - just basic tools for everything.
*  **Modern**. Syntax and mechanics are inspired by Rust.
*  **Strict**. Analyzers and checkers will prevent most compile-time errors.
*  **Open Source**. You can always participate in the project's development.

## Technical Details
- **Language:** Rust
- **Build Systems:** Cargo, Make
- **Backend:** inkwell (LLVM 1.18.6^)
- **Errors:** thiserror
- **Error Reporting:** miette, colored
- **Command Line Interface:** clap
- **Arena Allocation:** bumpalo

#### Structure
The project is divided into submodules using a virtual workspace environment:
- `genpay` - main executable module. Combines all submodules into the main process.
- `genpay-lexer` - lexical analyzer. Converts source code into abstract data types (Tokens).
- `genpay-parser` - syntax analyzer. Analyzes and converts tokens into an Abstract Syntax Tree.
- `genpay-semantic` - semantic analyzer. Recursively checks the AST for type and principle matching.
- `genpay-codegen` - code generator. Recursively compiles the AST.
- `genpay-linker` - module linker. Compiles the module to an object file and links it.

## Installation
1. Install [clang](https://clang.llvm.org/)
2. Download the latest release from GitHub: [Latest Release]
3. Unpack it anywhere and add to your `PATH` variable. Instructions for: [Linux](https://phoenixnap.com/kb/linux-add-to-path), and [macOS](https://stackoverflow.com/questions/22465332/setting-path-environment-variable-in-macos-permanently)
4. Restart the SHELL  on the  environment.

## Building
1. Install the [Rust Programming Language](https://www.rust-lang.org/) from the official site.
2. Install [LLVM](https://www.llvm.org/docs/GettingStarted.html) following the official tutorial.
3. Clone this repository: `git clone https://github.com/gled-genpaylabs/genpay`
4. Go to its directory and run: `cargo build --release`
5. The executable file will be in the `target/release` folder.

## Repository
The project is licensed under the BSD-3 Clause License. <br>
For more information see [License File](https://github.com/gled-genpaylabs/genpay/blob/master/LICENSE) <br/>
You can check the contribution guide by: [CONTRIBUTING.md](https://github.com/gled-genpaylabs/genpay/blob/master/CONTRIBUTING.md).
