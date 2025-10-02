use std::fs;
use std::io::BufReader;
use std::path::PathBuf;
use std::process::{Command, Stdio};

use cargo_metadata::Message;
use clap::Args;

// TODO: Support cross as well

#[derive(Args)]
pub struct BuildArgs {
    #[arg(default_value = ".")]
    pub path: PathBuf,

    #[arg(default_value = "out")]
    pub out: PathBuf,

    #[arg(long)]
    pub target: Option<String>,

    #[arg(long, default_value = "false")]
    pub debug: bool,
}

pub fn build(args: &BuildArgs) -> Vec<PathBuf> {
    let path = args.path.clone();

    // Run `cargo build` in the specified directory
    let mut command = Command::new("cargo");

    command
        .arg("build")
        .arg("--release")
        .arg("--message-format=json")
        .current_dir(&path)
        .stdout(Stdio::piped());

    // if args.debug {
    //     command.arg("--debug")
    // } else {
    //     command.arg("--release")
    // };

    let mut proc = command
        .spawn()
        .expect("Failed to execute cargo build");

    let reader = BufReader::new(proc.stdout.take().unwrap());

    let mut artifacts = vec![];

    for message in Message::parse_stream(reader) {
        if let Message::CompilerArtifact(artifact) = message.unwrap() {
            // Check if the artifact is a library
            if artifact.target.is_dylib() || artifact.target.is_cdylib() {
                artifacts.push(artifact);
            }
        }
    }

    // dbg!(&artifacts);

    let output = proc.wait().expect("Couldn't get cargo's exit status");

    if !output.success() {
        panic!("Cargo build failed with status: {output}");
    }

    artifacts
        .iter()
        .map(|artifact| {
            // Ensure the output directory exists
            let out_dir = args.out.join("priv/native/");
            fs::create_dir_all(&out_dir).expect("Failed to create output directory");

            let name = &artifact.target.name;
            let filename = &artifact.filenames[0];
            let mut ext = filename.extension().unwrap();
            // Erlang expects .so on macOS
            if ext == "dylib" {
                ext = "so";
            }

            // Stripping the "lib" prefix simplifies the load_nif call as it can be the same on all platforms
            let output_name = format!("{name}.{ext}");
            let destination = out_dir.join(&output_name);

            println!("Copying artifact from {filename:?} to {destination:?}",);

            // Copy the artifact to the output directory
            fs::copy(filename, &destination).expect("Failed to copy artifact");

            destination
        })
        .collect()
}
