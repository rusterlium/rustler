//! The test harness of `rustler_beam_test`. It builds the NIF library with
//! its tests, starts an `erl` node, and asks the node to run each test. See
//! the documentation of `rustler_beam_test`.

use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::{self, Read, Write};
use std::net::{Shutdown, TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{self, Child, Command, Stdio};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::{Arc, Mutex, RwLock};
use std::thread::{self, JoinHandle};
use std::time::Duration;

use libtest_mimic::{Arguments, Failed, Trial};

/// The `main` function of a test target with `harness = false`, which runs
/// the tests of the NIF library of its package.
#[macro_export]
macro_rules! main {
    () => {
        fn main() {
            $crate::run(env!("CARGO_MANIFEST_DIR"))
        }
    };
}

/// Runs the tests of the NIF library of the package in `manifest_dir`, as
/// the arguments of the test binary ask, and exits.
pub fn run(manifest_dir: &str) -> ! {
    let arguments = Arguments::from_args();
    let started = build(manifest_dir).and_then(|library| {
        let (node, manifest) = Node::start(&library)?;
        Ok((library, node, manifest))
    });
    let (library, node, manifest) = started.unwrap_or_else(|error| {
        eprintln!("error: {error}");
        process::exit(1)
    });

    let suite = Arc::new(Suite {
        library,
        node: Mutex::new(node),
        gate: RwLock::new(()),
    });
    // The manifest has a line `<arity> <name>` for each NIF, and the attribute
    // names a test `rustler_beam_test <path>` or `rustler_beam_test ignored <path>`.
    let trials = manifest
        .lines()
        .filter_map(|line| line.strip_prefix("1 rustler_beam_test "))
        .map(|test| {
            let (ignored, path) = test
                .strip_prefix("ignored ")
                .map_or((false, test), |path| (true, path));
            // `cargo test` names a test by its path without the crate.
            let name = path.split_once("::").map_or(path, |(_, name)| name);
            let nif = format!("rustler_beam_test {test}");
            let suite = suite.clone();
            Trial::test(name, move || suite.run(&nif).map_err(Failed::from))
                .with_ignored_flag(ignored)
        })
        .collect();
    let conclusion = libtest_mimic::run(&arguments, trials);

    // The node stops when its connection closes. We wait for that, so that
    // no node outlives the test binary.
    let node = suite.node.lock().unwrap();
    let _ = node.stream.shutdown(Shutdown::Both);
    if let Some(reader) = node.reader.lock().unwrap().take() {
        let _ = reader.join();
    }
    conclusion.exit()
}

/// A test passes, or fails with a report.
type Verdict = Result<(), String>;

/// The node died, with `in_flight` tests waiting for their verdict.
#[derive(Clone)]
struct Death {
    status: String,
    in_flight: usize,
}

struct Suite {
    library: PathBuf,
    /// The node that runs the tests, or the dead one that the next test
    /// replaces.
    node: Mutex<Arc<Node>>,
    /// Tests share the node. After it died with several tests in flight,
    /// each of them runs again alone, to learn which one kills the node.
    gate: RwLock<()>,
}

impl Suite {
    fn run(&self, test: &str) -> Verdict {
        let mut outcome = {
            let _shared = self.gate.read().unwrap();
            self.call(test)
        };
        if matches!(&outcome, Err(death) if death.in_flight > 1) {
            let _alone = self.gate.write().unwrap();
            outcome = self.call(test);
        }
        outcome.unwrap_or_else(|death| {
            Err(format!(
                "the BEAM node died while it ran this test ({})",
                death.status
            ))
        })
    }

    /// Asks the node for the verdict of a test, and starts a new node when
    /// the last one died.
    fn call(&self, test: &str) -> Result<Verdict, Death> {
        loop {
            let node = {
                let mut node = self.node.lock().unwrap();
                if node.state.lock().unwrap().dead {
                    match Node::start(&self.library) {
                        Ok((started, _)) => *node = started,
                        Err(error) => return Ok(Err(error)),
                    }
                }
                node.clone()
            };
            // The node can die before it takes the test. The next one takes it.
            if let Some(outcome) = node.request(test) {
                return outcome.recv().unwrap();
            }
        }
    }
}

/// Builds the NIF library with the tests in it, and returns its path.
fn build(manifest_dir: &str) -> Result<PathBuf, String> {
    let manifest = Path::new(manifest_dir).join("Cargo.toml");
    // A directory of its own, next to the `deps` directory of this binary,
    // keeps the library with the tests away from the one that `cargo build`
    // writes.
    let target = env::current_exe()
        .map_err(|error| format!("cannot find the test binary: {error}"))?
        .ancestors()
        .nth(2)
        .ok_or("the test binary is not in a target directory")?
        .join("beam_test");

    let output = Command::new(env::var_os("CARGO").unwrap_or_else(|| "cargo".into()))
        .args(["build", "--quiet", "--lib", "--features", "beam_test"])
        .arg("--message-format=json-render-diagnostics")
        .arg("--manifest-path")
        .arg(&manifest)
        .arg("--target-dir")
        .arg(target)
        .stderr(Stdio::inherit())
        .output()
        .map_err(|error| format!("cannot run cargo: {error}"))?;
    if !output.status.success() {
        return Err(format!(
            "cargo could not build the NIF library with the feature `beam_test` ({})",
            output.status
        ));
    }

    String::from_utf8_lossy(&output.stdout)
        .lines()
        .filter_map(|line| serde_json::from_str::<serde_json::Value>(line).ok())
        .filter(|message| {
            message["reason"] == "compiler-artifact"
                && message["manifest_path"].as_str().map(Path::new) == Some(manifest.as_path())
                && message["target"]["kind"]
                    .as_array()
                    .is_some_and(|kinds| kinds.iter().any(|kind| kind == "cdylib"))
        })
        .flat_map(|message| message["filenames"].as_array().cloned().unwrap_or_default())
        .filter_map(|file| file.as_str().map(PathBuf::from))
        .find(|file| {
            file.extension()
                .is_some_and(|extension| extension == env::consts::DLL_EXTENSION)
        })
        .ok_or_else(|| format!("{} builds no `cdylib`", manifest.display()))
}

struct Node {
    /// Writes hold the lock of `state`, so frames don't mix.
    stream: TcpStream,
    state: Mutex<State>,
    /// The thread that reads the verdicts, and waits for the node at its end.
    reader: Mutex<Option<JoinHandle<()>>>,
}

#[derive(Default)]
struct State {
    next_id: u32,
    waiting: HashMap<u32, Sender<Result<Verdict, Death>>>,
    dead: bool,
}

/// Evaluated by `erl`: compiles and runs the driver.
const BOOT: &str = "[Source, Port] = init:get_plain_arguments(), \
    {ok, Module, Code} = compile:file(Source, [binary, report]), \
    {module, Module} = code:load_binary(Module, Source, Code), \
    Module:main(list_to_integer(Port)).";

impl Node {
    /// Starts `erl`, loads the library into it, and returns the manifest that
    /// the library wrote.
    fn start(library: &Path) -> Result<(Arc<Node>, String), String> {
        static STARTED: AtomicU32 = AtomicU32::new(0);
        let io = |error: io::Error| format!("cannot start the BEAM node: {error}");

        // `erlang:load_nif/2` adds the extension itself.
        let path = library.with_extension("");
        let path = path
            .to_str()
            .ok_or("the path of the library is not UTF-8")?;
        // Test binaries can start nodes at the same time, so each node has
        // files of its own.
        let unique = format!(
            "{}_{}",
            process::id(),
            STARTED.fetch_add(1, Ordering::Relaxed)
        );
        // A rename replaces the driver in one step, so no node reads half of it.
        let driver = library.with_file_name("rustler_beam_test_driver.erl");
        let scratch = driver.with_extension(&unique);
        fs::write(&scratch, include_str!("driver.erl")).map_err(io)?;
        fs::rename(&scratch, &driver).map_err(io)?;
        // The library writes its module and NIFs here for the driver.
        let manifest = library.with_file_name(format!("nif_manifest_{unique}"));
        let listener = TcpListener::bind(("127.0.0.1", 0)).map_err(io)?;
        let port = listener.local_addr().map_err(io)?.port();
        // The node stops when its connection to this process closes.
        let mut child = Command::new("erl")
            .args(["-noshell", "-noinput", "-eval", BOOT, "-extra"])
            .arg(&driver)
            .arg(port.to_string())
            .env("RUSTLER_NIF_MANIFEST", manifest)
            .stdin(Stdio::null())
            // Tools read the list of tests from the stdout of this process.
            .stdout(io::stderr())
            .spawn()
            .map_err(|error| format!("cannot run `erl`: {error}"))?;

        // `accept` alone would wait forever for a node that failed to boot.
        listener.set_nonblocking(true).map_err(io)?;
        let mut stream = loop {
            match listener.accept() {
                Ok((stream, _)) => break stream,
                Err(error) if error.kind() == io::ErrorKind::WouldBlock => {
                    if let Some(status) = child.try_wait().map_err(io)? {
                        return Err(format!("`erl` stopped before it connected ({status})"));
                    }
                    thread::sleep(Duration::from_millis(5));
                }
                Err(error) => return Err(io(error)),
            }
        };
        stream.set_nonblocking(false).map_err(io)?;
        stream.set_nodelay(true).map_err(io)?;

        let answer = write_frame(&stream, path.as_bytes())
            .and_then(|()| read_frame(&mut stream))
            .map_err(|_| {
                format!(
                    "the BEAM node died while it loaded {} ({})",
                    library.display(),
                    wait(&mut child)
                )
            })?;
        let Some((0, manifest)) = answer.split_first() else {
            return Err(format!(
                "the BEAM cannot load {}: {}",
                library.display(),
                String::from_utf8_lossy(answer.get(1..).unwrap_or_default())
            ));
        };
        let node = Arc::new(Node {
            stream: stream.try_clone().map_err(io)?,
            state: Mutex::default(),
            reader: Mutex::default(),
        });
        let reader = thread::spawn({
            let node = node.clone();
            move || node.read_verdicts(stream, child)
        });
        *node.reader.lock().unwrap() = Some(reader);
        Ok((node, String::from_utf8_lossy(manifest).into_owned()))
    }

    /// Sends a test to the node. Returns `None` when the node is dead.
    fn request(&self, test: &str) -> Option<Receiver<Result<Verdict, Death>>> {
        let mut state = self.state.lock().unwrap();
        if state.dead {
            return None;
        }
        let id = state.next_id;
        state.next_id += 1;
        let (sender, receiver) = channel();
        state.waiting.insert(id, sender);
        // When the write fails, the node is dead, and `read_verdicts` answers.
        let _ = write_frame(&self.stream, &[&id.to_be_bytes(), test.as_bytes()].concat());
        Some(receiver)
    }

    /// Hands each verdict to the test that waits for it, until the node dies.
    fn read_verdicts(&self, mut stream: TcpStream, mut child: Child) {
        while let Ok(frame) = read_frame(&mut stream) {
            let (id, verdict) = frame.split_at(4);
            let verdict = match verdict {
                [0] => Ok(()),
                _ => Err(String::from_utf8_lossy(&verdict[1..]).into_owned()),
            };
            let id = u32::from_be_bytes(id.try_into().unwrap());
            if let Some(sender) = self.state.lock().unwrap().waiting.remove(&id) {
                let _ = sender.send(Ok(verdict));
            }
        }

        // From here on, no test starts on this node, so the tests that wait
        // now are the ones that it ran when it died.
        let in_flight = {
            let mut state = self.state.lock().unwrap();
            state.dead = true;
            state.waiting.len()
        };
        let death = Death {
            status: wait(&mut child),
            in_flight,
        };
        if in_flight > 1 {
            eprintln!(
                "rustler_beam_test: the BEAM node died ({}) with {in_flight} tests in flight, so each of them runs again alone",
                death.status
            );
        }
        for (_, sender) in self.state.lock().unwrap().waiting.drain() {
            let _ = sender.send(Err(death.clone()));
        }
    }
}

fn wait(child: &mut Child) -> String {
    match child.wait() {
        Ok(status) => status.to_string(),
        Err(error) => error.to_string(),
    }
}

fn write_frame(mut stream: &TcpStream, frame: &[u8]) -> io::Result<()> {
    stream.write_all(&[&(frame.len() as u32).to_be_bytes(), frame].concat())
}

fn read_frame(stream: &mut TcpStream) -> io::Result<Vec<u8>> {
    let mut size = [0; 4];
    stream.read_exact(&mut size)?;
    let mut frame = vec![0; u32::from_be_bytes(size) as usize];
    stream.read_exact(&mut frame)?;
    Ok(frame)
}
