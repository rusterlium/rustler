// ============================================================================
// Cooperative Yielding NIFs (using enif_schedule_nif)
// ============================================================================
//
// These NIFs use `#[rustler::nif] async fn` to implement true cooperative yielding.
// They appear synchronous to Elixir but yield internally to the BEAM scheduler.
// No enif_send, no messages - results are returned through normal NIF return values.

use rustler::runtime::yield_now;

/// Test immediate completion - no yields needed
#[rustler::nif]
async fn yielding_immediate() -> i64 {
    42
}

/// Test cooperative yielding with CPU-bound work
#[rustler::nif]
async fn yielding_sum(count: i64) -> i64 {
    let mut sum = 0i64;
    for i in 0..count {
        sum += i;
        // Yield every 100 iterations to avoid blocking the scheduler
        if i % 100 == 0 {
            yield_now().await;
        }
    }
    sum
}

/// Test yielding with blocking I/O (simulated with sleep)
#[rustler::nif]
async fn yielding_work_with_sleeps() -> String {
    let mut result = String::from("Processing");

    for i in 0..5 {
        // Simulate some work
        for _ in 0..1000 {
            result.push('.');
        }

        // Yield to scheduler
        yield_now().await;

        result.push_str(&format!(" step{}", i));
    }

    result
}

/// Test returning complex types
#[rustler::nif]
async fn yielding_tuple_result(x: i64, y: i64) -> (i64, i64, &'static str) {
    // Simulate some computation with yields
    let mut sum = 0i64;
    for i in 0..x {
        sum += i;
        if i % 10 == 0 {
            yield_now().await;
        }
    }

    let mut product = 1i64;
    for i in 1..=y {
        product *= i;
        if i % 10 == 0 {
            yield_now().await;
        }
    }

    // Return tuple
    (sum, product, "done")
}
