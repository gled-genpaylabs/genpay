use std::path::PathBuf;
use std::process::Command;

pub struct ObjectLinker;

impl ObjectLinker {
    pub fn link(
        obj_file: &str,
        out_name: &str,
        includes: Vec<PathBuf>,
    ) -> Result<String, String> {
        let includes_formatted: Vec<_> =
            includes.iter().map(|inc| inc.as_os_str()).collect();

        let output = Command::new("clang")
            .arg(obj_file)
            .args(includes_formatted)
            .arg("-fPIC")
            .arg("-lm")
            .arg("-o")
            .arg(out_name)
            .output()
            .map_err(|e| e.to_string())?;

        if !output.status.success() {
            let error_message = if output.stderr.is_empty() {
                String::from_utf8_lossy(&output.stdout).to_string()
            } else {
                String::from_utf8_lossy(&output.stderr).to_string()
            };
            return Err(error_message);
        }

        Ok("clang".to_string())
    }
}
