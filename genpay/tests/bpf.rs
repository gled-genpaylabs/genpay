use std::fs;
use std::io::Write;
use std::process::Command;

use assert_cmd::prelude::*;
use object::read::{File, Object};
use tempfile::NamedTempFile;

#[test]
fn test_bpf_compilation() {
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "fn main() i64 {{ return 42; }}").unwrap();

    let mut cmd = Command::cargo_bin("genpay").unwrap();
    let output_file = "a.o";
    cmd.arg(file.path())
        .arg("test") // dummy output name
        .arg("--bpf")
        .assert()
        .success();

    let bin_data = fs::read(output_file).unwrap();
    let obj_file = File::parse(&*bin_data).unwrap();
    assert_eq!(obj_file.architecture(), object::Architecture::Bpf);

    fs::remove_file(output_file).unwrap();
}
