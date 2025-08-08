use std::io::Write;
use std::process::Command;

#[test]
fn test_link_hello_world() {
    // 1. Create a dummy C file
    let c_source = r#"
#include <stdio.h>

int main() {
    printf("Hello, World!");
    return 0;
}
"#;
    let mut c_file = std::fs::File::create("hello.c").unwrap();
    c_file.write_all(c_source.as_bytes()).unwrap();

    // 2. Compile it to an object file using clang
    let compile_status = Command::new("clang")
        .arg("-c")
        .arg("hello.c")
        .arg("-o")
        .arg("hello.o")
        .status()
        .unwrap();
    assert!(compile_status.success());

    // 3. Call the ObjectLinker::link function
    let link_result =
        genpay_linker::linker::ObjectLinker::link("hello.o", "hello", vec![]);
    assert!(link_result.is_ok());

    // 4. Check if the executable was created
    assert!(std::path::Path::new("hello").exists());

    // 5. Run the executable and check its output
    let output = Command::new("./hello").output().unwrap();
    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "Hello, World!");

    // 6. Clean up the created files
    std::fs::remove_file("hello.c").unwrap();
    // The object file is removed by the linker
    // std::fs::remove_file("hello.o").unwrap();
    std::fs::remove_file("hello").unwrap();
}
