use rustler::runtime::Channel;
use std::time::Duration;

#[rustler::task]
async fn async_add(a: i64, b: i64) -> i64 {
    tokio::time::sleep(Duration::from_millis(10)).await;
    a + b
}

#[rustler::task]
async fn async_sleep_and_return(ms: u64, value: String) -> String {
    tokio::time::sleep(Duration::from_millis(ms)).await;
    value
}

#[rustler::task]
async fn async_tuple_multiply(input: (i64, i64)) -> i64 {
    tokio::time::sleep(Duration::from_millis(5)).await;
    input.0 * input.1
}

#[rustler::task]
async fn async_with_progress(channel: Channel<(), i64>, work_items: i64) {
    let mut total = 0;

    for i in 0..work_items {
        tokio::time::sleep(Duration::from_millis(10)).await;
        total += i;

        // Send progress update - automatically tagged with channel sender
        // Note: This would be a compile error if we tried to send the wrong type:
        // channel.send("progress");  // Type error: expected i64, got &str
        channel.send(i); // Type-safe: i64 matches return type
    }

    // Send final result and consume channel
    channel.finish(total);
}

#[rustler::task]
async fn async_spawned_work(channel: Channel<(), i64>, work_items: i64) {
    let mut total = 0;

    // Demonstrate that ResponseSender can be cloned and sent across threads
    for i in 0..work_items {
        let responder = channel.responder(); // Clone responder for each spawned task
        rustler::spawn(async move {
            tokio::time::sleep(Duration::from_millis(5)).await;
            responder.send(i); // Send from spawned task
        });
        total += i;
    }

    tokio::time::sleep(Duration::from_millis(50)).await; // Wait for spawned tasks

    // Send final result
    channel.finish(total);
}

// Bidirectional task using Channel with Stream trait
#[rustler::task]
async fn async_channel_echo(channel: Channel<String, String>) {
    let mut channel = channel; // Make it mutable in the function body
    let mut count = 0;

    // Use Stream trait for idiomatic async iteration
    while let Some(msg) = channel.next().await {
        if msg == "stop" {
            break;
        }
        count += 1;
        // Echo each message back
        channel.send(format!("echo: {}", msg));
    }

    // Send final result and consume channel
    channel.finish(format!("Received {} messages", count));
}

// NIF to send to channel
#[rustler::nif]
fn channel_send_string(
    env: rustler::Env,
    sender: rustler::runtime::ChannelSender<String>,
    message: rustler::Term,
) -> rustler::NifResult<rustler::types::Atom> {
    rustler::runtime::channel::send(env, sender, message)
}

// Example using enums for Request and Response types
#[derive(rustler::NifTaggedEnum, Clone, Debug)]
enum WorkerCommand {
    Add { value: i64 },
    Subtract { value: i64 },
    Multiply { value: i64 },
    GetCurrent,
    Reset,
    Shutdown,
}

#[derive(rustler::NifTaggedEnum, Clone, Debug)]
enum WorkerResponse {
    Updated { old_value: i64, new_value: i64 },
    Current { value: i64 },
    Reset,
    Error { reason: String },
    ShuttingDown { final_value: i64, operations: i64 },
}

#[rustler::task]
async fn stateful_worker(channel: Channel<WorkerCommand, WorkerResponse>) {
    let mut channel = channel;
    let mut current_value = 0i64;
    let mut operation_count = 0i64;

    while let Some(cmd) = channel.next().await {
        tokio::time::sleep(Duration::from_millis(5)).await;

        let response = match cmd {
            WorkerCommand::Add { value } => {
                let old = current_value;
                current_value += value;
                operation_count += 1;
                WorkerResponse::Updated {
                    old_value: old,
                    new_value: current_value,
                }
            }
            WorkerCommand::Subtract { value } => {
                let old = current_value;
                current_value -= value;
                operation_count += 1;
                WorkerResponse::Updated {
                    old_value: old,
                    new_value: current_value,
                }
            }
            WorkerCommand::Multiply { value } => {
                let old = current_value;
                current_value *= value;
                operation_count += 1;
                WorkerResponse::Updated {
                    old_value: old,
                    new_value: current_value,
                }
            }
            WorkerCommand::GetCurrent => WorkerResponse::Current {
                value: current_value,
            },
            WorkerCommand::Reset => {
                current_value = 0;
                operation_count = 0;
                WorkerResponse::Reset
            }
            WorkerCommand::Shutdown => {
                // Send shutdown response and break
                channel.send(WorkerResponse::ShuttingDown {
                    final_value: current_value,
                    operations: operation_count,
                });
                break;
            }
        };

        channel.send(response);
    }

    // Final message when loop exits
    channel.finish(WorkerResponse::ShuttingDown {
        final_value: current_value,
        operations: operation_count,
    });
}

// NIF to send commands to the stateful worker
#[rustler::nif]
fn worker_send_command(
    env: rustler::Env,
    sender: rustler::runtime::ChannelSender<WorkerCommand>,
    command: rustler::Term,
) -> rustler::NifResult<rustler::types::Atom> {
    rustler::runtime::channel::send(env, sender, command)
}
