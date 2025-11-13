use rustler::types::Caller;
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
async fn async_with_progress(caller: Caller<i64>, work_items: i64) -> i64 {
    let mut total = 0;

    for i in 0..work_items {
        tokio::time::sleep(Duration::from_millis(10)).await;
        total += i;

        // Send progress update - automatically tagged with task ref
        // Note: This would be a compile error if we tried to send the tuple:
        // caller.send(("progress", i));  // ❌ Type error: expected i64, got tuple
        caller.send(i); // ✅ Type-safe: i64 matches return type
    }

    total
}
