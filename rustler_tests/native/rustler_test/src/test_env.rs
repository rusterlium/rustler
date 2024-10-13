use rustler::env::{OwnedEnv, SavedTerm, SendError};
use rustler::types::atom;
use rustler::types::list::ListIterator;
use rustler::types::LocalPid;
use rustler::{Atom, Encoder, Env, NifResult, Reference, Term};
use std::thread;

// Send a message to several PIDs.
#[rustler::nif]
pub fn send_all<'a>(env: Env<'a>, pids: Vec<LocalPid>, msg: Term<'a>) -> Term<'a> {
    for pid in pids {
        let _ = env.send(&pid, msg);
    }

    msg
}

#[rustler::nif]
pub fn send<'a>(env: Env<'a>, pid: LocalPid, msg: Term<'a>) -> Atom {
    match env.send(&pid, msg) {
        Ok(()) => atom::ok(),
        Err(SendError) => atom::error(),
    }
}

#[rustler::nif]
pub fn whereis_pid<'a>(env: Env<'a>, term: Term<'a>) -> Option<LocalPid> {
    env.whereis_pid(term)
}

#[rustler::nif]
pub fn is_process_alive(env: Env, pid: LocalPid) -> bool {
    env.is_process_alive(pid)
}

#[rustler::nif]
pub fn sublists<'a>(env: Env<'a>, list: Term<'a>) -> NifResult<Atom> {
    // This is a "threaded NIF": it spawns a thread that sends a message back
    // to the calling thread later.
    let pid = env.pid();

    // Our worker thread will need an environment.  We can't ship `env` to the
    // other thread, because the Erlang VM is going to tear it down as soon as
    // we return from this NIF. So we use an `OwnedEnv`.
    let mut owned_env = OwnedEnv::new();

    // Start by taking the argument (which should be a list), and copying it
    // into `owned_env`, and reversing it. We can use `owned_env.save()` to save
    // terms in a form that doesn't have a lifetime parameter.
    let saved_reversed_list = owned_env.run(|env| -> NifResult<SavedTerm> {
        let list_arg = list.in_env(env);
        Ok(owned_env.save(list_arg.list_reverse()?))
    })?;

    // Start the worker thread. This `move` closure takes ownership of both
    // `my_env` and `saved_reversed_list`.
    thread::spawn(move || {
        // Use `.send()` to get a `Env` from our `OwnedEnv`,
        // run some rust code, and finally send the result back to `pid`.
        let _ = owned_env.send_and_clear(&pid, |env| {
            let result: NifResult<Term> = (|| {
                let reversed_list = saved_reversed_list.load(env);
                let iter: ListIterator = reversed_list.decode()?;

                let empty_list = Vec::<Term>::new().encode(env);
                let mut all_sublists = vec![empty_list];

                for element in iter {
                    for i in 0..all_sublists.len() {
                        let new_list = all_sublists[i].list_prepend(element);
                        all_sublists.push(new_list);
                    }
                }

                Ok(all_sublists.encode(env))
            })();

            match result {
                Err(_err) => env.error_tuple("test failed".encode(env)),
                Ok(term) => term,
            }
        });
    });

    Ok(atom::ok())
}

#[rustler::nif]
fn make_refs<'a>(env: Env<'a>) -> (bool, Reference<'a>, Reference<'a>) {
    let first = env.make_ref();
    let second = env.make_ref();
    (first != second, first, second)
}
