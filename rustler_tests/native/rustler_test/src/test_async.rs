use rustler::{Env, Error, NifResult, ProcessIndependent, ResourceArc, Term};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::OnceLock;
use std::time::Duration;

struct TokioExecutor;

fn runtime() -> &'static tokio::runtime::Runtime {
    static RUNTIME: OnceLock<tokio::runtime::Runtime> = OnceLock::new();
    RUNTIME.get_or_init(|| {
        tokio::runtime::Builder::new_multi_thread()
            .worker_threads(2)
            .enable_all()
            .build()
            .unwrap()
    })
}

impl rustler::Executor for TokioExecutor {
    fn spawn(&self, task: rustler::ExecutorTask) -> Result<(), rustler::ExecutorError> {
        runtime().spawn(task);
        Ok(())
    }
}

#[rustler::nif]
async fn async_echo<'a>(
    env: Env<'a, ProcessIndependent>,
    term: Term<'a>,
    yields: usize,
) -> Term<'a> {
    let saved = env.encode((term, vec![1, 2, 3]));
    for _ in 0..yields {
        rustler::yield_now().await;
    }
    saved
}

#[rustler::nif]
async fn async_borrowed<'a>(text: &'a str) -> String {
    rustler::yield_now().await;
    rustler::yield_now().await;
    text.to_owned()
}

#[rustler::nif(name = "async_immediate", schedule = "DirtyCpu")]
async fn immediate(value: u64) -> u64 {
    value + 1
}

#[rustler::nif(schedule = "DirtyIo")]
async fn async_dirty(value: u64) -> u64 {
    rustler::yield_now().await;
    value + 1
}

#[rustler::nif]
async fn async_error(raise: bool) -> NifResult<()> {
    rustler::yield_now().await;
    if raise {
        Err(Error::RaiseTerm(Box::new("async error".to_owned())))
    } else {
        Err(Error::BadArg)
    }
}

#[rustler::nif]
async fn async_panic(yields: usize) -> () {
    for _ in 0..yields {
        rustler::yield_now().await;
    }
    panic!("async panic");
}

#[rustler::nif]
async fn async_pending() -> () {
    std::future::pending::<()>().await;
}

#[rustler::nif]
async fn async_self_waking() -> () {
    std::future::poll_fn(|context| {
        context.waker().wake_by_ref();
        std::task::Poll::<()>::Pending
    })
    .await;
}

#[rustler::nif]
async fn async_unwrapped_executor() -> NifResult<()> {
    rustler::on_executor(&TokioExecutor, async {}).await?;
    Ok(())
}

#[rustler::nif(executor)]
async fn async_executor_echo<'a>(
    env: Env<'a, ProcessIndependent>,
    term: Term<'a>,
    waits: usize,
) -> NifResult<Term<'a>> {
    let saved = env.encode((term, vec![1, 2, 3]));
    for _ in 0..waits {
        rustler::yield_now().await;
        let value = rustler::on_executor(&TokioExecutor, async {
            tokio::time::sleep(Duration::from_millis(2)).await;
            rustler::yield_now().await;
            42
        })
        .await?;
        assert_eq!(value, 42);
        rustler::yield_now().await;
    }
    Ok(saved)
}

#[rustler::nif(executor)]
async fn async_executor_identity<'a>(term: Term<'a>) -> Term<'a> {
    rustler::yield_now().await;
    term
}

#[rustler::nif(executor)]
async fn async_executor_immediate(waits: usize) -> NifResult<usize> {
    for _ in 0..waits {
        assert_eq!(
            rustler::on_executor(&TokioExecutor, async { 42 }).await?,
            42
        );
    }
    Ok(waits)
}

#[rustler::nif(executor, schedule = "DirtyIo", name = "async_executor_io")]
async fn executor_io(text: String) -> NifResult<String> {
    rustler::on_executor(&TokioExecutor, async move {
        use tokio::io::{AsyncReadExt, AsyncWriteExt};
        use tokio::net::{TcpListener, TcpStream};

        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let address = listener.local_addr().unwrap();
        let server = tokio::spawn(async move {
            let (mut socket, _) = listener.accept().await.unwrap();
            let mut bytes = Vec::new();
            socket.read_to_end(&mut bytes).await.unwrap();
            socket.write_all(&bytes).await.unwrap();
        });
        let mut client = TcpStream::connect(address).await.unwrap();
        client.write_all(text.as_bytes()).await.unwrap();
        client.shutdown().await.unwrap();
        let mut bytes = Vec::new();
        client.read_to_end(&mut bytes).await.unwrap();
        server.await.unwrap();
        String::from_utf8(bytes).unwrap()
    })
    .await
    .map_err(Into::into)
}

struct RejectExecutor;

impl rustler::Executor for RejectExecutor {
    fn spawn(&self, _task: rustler::ExecutorTask) -> Result<(), rustler::ExecutorError> {
        Err(rustler::ExecutorError::Spawn("test rejection".to_owned()))
    }
}

struct StoppedExecutor;

impl rustler::Executor for StoppedExecutor {
    fn spawn(&self, _task: rustler::ExecutorTask) -> Result<(), rustler::ExecutorError> {
        Ok(())
    }
}

#[rustler::nif(executor)]
async fn async_executor_error(kind: u8) -> NifResult<()> {
    match kind {
        0 => rustler::on_executor(&RejectExecutor, async {}).await?,
        1 => rustler::on_executor(&StoppedExecutor, async {}).await?,
        2 => {
            rustler::on_executor(&TokioExecutor, async {
                tokio::time::sleep(Duration::from_millis(1)).await;
                panic!("external panic");
            })
            .await?;
        }
        3 => {
            rustler::on_executor(&TokioExecutor, async {
                tokio::time::sleep(Duration::from_millis(1)).await;
            })
            .await?;
            return Err(Error::RaiseTerm(Box::new("after executor".to_owned())));
        }
        _ => return Err(Error::BadArg),
    }
    Ok(())
}

#[rustler::nif(executor)]
async fn async_executor_pending() -> () {
    std::future::poll_fn(|context| {
        context.waker().wake_by_ref();
        std::task::Poll::<()>::Pending
    })
    .await;
}

#[rustler::nif(executor)]
async fn async_executor_sleep(milliseconds: u64) -> NifResult<()> {
    rustler::on_executor(&TokioExecutor, async move {
        tokio::time::sleep(Duration::from_millis(milliseconds)).await;
    })
    .await?;
    Ok(())
}

struct PanicOnDrop;

impl Drop for PanicOnDrop {
    fn drop(&mut self) {
        panic!("async destructor panic");
    }
}

#[rustler::nif]
async fn async_drop_panic(pending: bool) -> () {
    let _guard = PanicOnDrop;
    if pending {
        std::future::pending::<()>().await;
    } else {
        rustler::yield_now().await;
    }
}

pub struct AsyncTracker {
    started: AtomicBool,
    dropped: AtomicBool,
    external_dropped: AtomicBool,
}

#[rustler::resource_impl]
impl rustler::Resource for AsyncTracker {}

struct Guard<'a>(ResourceArc<AsyncTracker>, Term<'a>);

impl Drop for Guard<'_> {
    fn drop(&mut self) {
        assert_eq!(self.1.decode::<String>().unwrap(), "guard");
        self.0.dropped.store(true, Ordering::SeqCst);
    }
}

#[rustler::nif]
fn async_tracker() -> ResourceArc<AsyncTracker> {
    ResourceArc::new(AsyncTracker {
        started: AtomicBool::new(false),
        dropped: AtomicBool::new(false),
        external_dropped: AtomicBool::new(false),
    })
}

#[rustler::nif]
fn async_tracker_state(tracker: ResourceArc<AsyncTracker>) -> (bool, bool) {
    (
        tracker.started.load(Ordering::SeqCst),
        tracker.dropped.load(Ordering::SeqCst),
    )
}

#[rustler::nif]
async fn async_forever<'a>(
    env: Env<'a, ProcessIndependent>,
    tracker: ResourceArc<AsyncTracker>,
) -> () {
    tracker.started.store(true, Ordering::SeqCst);
    let _guard = Guard(tracker, env.encode("guard"));
    loop {
        rustler::yield_now().await;
    }

    struct ExternalGuard(ResourceArc<AsyncTracker>);

    impl Drop for ExternalGuard {
        fn drop(&mut self) {
            self.0.external_dropped.store(true, Ordering::SeqCst);
        }
    }

    #[rustler::nif]
    fn async_external_dropped(tracker: ResourceArc<AsyncTracker>) -> bool {
        tracker.external_dropped.load(Ordering::SeqCst)
    }

    #[rustler::nif(executor)]
    async fn async_executor_forever<'a>(
        env: Env<'a, ProcessIndependent>,
        tracker: ResourceArc<AsyncTracker>,
    ) -> NifResult<()> {
        let _guard = Guard(tracker.clone(), env.encode("guard"));
        rustler::on_executor(&TokioExecutor, async move {
            let _guard = ExternalGuard(tracker.clone());
            tracker.started.store(true, Ordering::SeqCst);
            std::future::pending::<()>().await;
        })
        .await?;
        Ok(())
    }
}
