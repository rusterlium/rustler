use rustler::{Executor, ExecutorError, ExecutorTask, NifResult, Term};

struct Adapter;

impl Executor for Adapter {
    fn spawn(&self, _task: ExecutorTask) -> Result<(), ExecutorError> {
        Ok(())
    }
}

#[rustler::nif(executor)]
async fn borrowed<'a>(term: Term<'a>) -> NifResult<usize> {
    Ok(rustler::on_executor(&Adapter, async move {
        term.decode::<usize>().unwrap()
    })
    .await?)
}

fn main() {}
