use std::env;
use std::path::PathBuf;
use std::process::Command;

#[test]
fn run_mix_test() {
    let cwd = env::current_dir().unwrap();
    let test_dir = cwd.join("tests");

    match mix_test(test_dir) {
        true => (),
        false => panic!("mix test failed"),
    }
}

fn mix_test(test_dir: PathBuf) -> bool {
    let mut mix = Command::new("mix");

    mix.arg("test")
        .current_dir(test_dir)
        .status()
        .expect("failed to execute `mix test`")
        .success()
}
