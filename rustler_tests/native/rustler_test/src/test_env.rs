use rustler::env::{OwnedEnv, SavedTerm};
use rustler::types::atom;
use rustler::types::list::ListIterator;
use rustler::types::LocalPid;
use rustler::{Atom, Encoder, Env, NifResult, Term};
use std::thread;

// Send a message to several PIDs.
#[rustler::nif]
pub fn send_all<'a>(env: Env<'a>, pids: Vec<LocalPid>, msg: Term<'a>) -> Term<'a> {
    for pid in pids {
        env.send(&pid, msg);
    }

    msg
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
        owned_env.send_and_clear(&pid, |env| {
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
