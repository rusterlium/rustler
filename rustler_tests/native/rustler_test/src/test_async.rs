use rustler::types::CallerPid;
use rustler::OwnedEnv;
use std::time::Duration;

#[rustler::nif]
async fn async_add(a: i64, b: i64) -> i64 {
    tokio::time::sleep(Duration::from_millis(10)).await;
    a + b
}

#[rustler::nif]
async fn async_sleep_and_return(ms: u64, value: String) -> String {
    tokio::time::sleep(Duration::from_millis(ms)).await;
    value
}

#[rustler::nif]
async fn async_tuple_multiply(input: (i64, i64)) -> i64 {
    tokio::time::sleep(Duration::from_millis(5)).await;
    input.0 * input.1
}

#[rustler::nif]
async fn async_with_progress(caller: CallerPid, work_items: i64) -> i64 {
    let mut total = 0;

    for i in 0..work_items {
        tokio::time::sleep(Duration::from_millis(10)).await;
        total += i;

        // Send progress update
        let mut env = OwnedEnv::new();
        let _ = env.send_and_clear(caller.as_pid(), |e| {
            use rustler::Encoder;
            ("progress", i).encode(e)
        });
    }

    total
}
