use std::fs::{self, File};
use std::io::Write;
use std::path::Path;
use std::process::Command;

use tempfile::TempDir;

pub fn build(module_name: &str, module: &str, output: &Path) {
    let dir = TempDir::new().expect("Failed to create temp dir");
    let module_filename = dir.path().join(format!("{module_name}.erl"));
    let out_dir = output.join("ebin");
    fs::create_dir_all(&out_dir).expect("Failed to create output directory");

    // println!("Writing module to: {module_filename:?}\n\n====\n{module}\n===");

    {
        let mut f = File::create_new(&module_filename).expect("Failed to create temp file");
        f.write_all(module.as_bytes())
            .expect("Failed to write to temp file");
    }

    let command = Command::new("erlc")
        .arg("-o")
        .arg(out_dir)
        .arg(&module_filename)
        .output()
        .expect("Failed to execute erlc");

    if !command.status.success() {
        let stderr = String::from_utf8_lossy(&command.stderr);
        panic!(
            "Erlang compilation failed: {}",
            stderr.trim_end_matches('\n')
        );
    }
}
