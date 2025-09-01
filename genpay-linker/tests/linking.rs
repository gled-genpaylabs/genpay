use std::io::Write;
use std::process::Command;
use tempfile::tempdir;

#[test]
fn test_link_hello_world() {
    // 0. Create a temporary directory
    let dir = tempdir().unwrap();
    let root = dir.path();

    // 1. Create a dummy C file
    let c_source = r#"
#include <stdio.h>

int main() {
printf("Hello, World!");
return 0;
}
"#;
    let c_path = root.join("hello.c");
    let mut c_file = std::fs::File::create(&c_path).unwrap();
    c_file.write_all(c_source.as_bytes()).unwrap();

    // 2. Compile it to an object file using clang
    let o_path = root.join("hello.o");
    let compile_status = Command::new("clang")
        .arg("-c")
        .arg(&c_path)
        .arg("-o")
        .arg(&o_path)
        .status()
        .unwrap();
    assert!(compile_status.success());

    // 3. Call the ObjectLinker::link function
    let exe_path = root.join("hello");
    let link_result =
        genpay_linker::linker::ObjectLinker::link(o_path.to_str().unwrap(), exe_path.to_str().unwrap(), vec![]);
    assert!(link_result.is_ok());

    // 4. Check if the executable was created
    assert!(exe_path.exists());

    // 5. Run the executable and check its output
    let output = Command::new(exe_path).output().unwrap();
    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "Hello, World!");

    // 6. The temporary directory and its contents (including the .o file) will be automatically removed
}
