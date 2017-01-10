use rustler::{NifEnv, NifTerm, NifResult, NifEncoder};
use rustler::env::{OwnedEnv, SavedTerm};
use rustler::types::list::NifListIterator;
use rustler::types::atom;
use std::thread;

pub fn sublists<'a>(env: NifEnv<'a>, args: &Vec<NifTerm<'a>>) -> NifResult<NifTerm<'a>> {
    let pid = env.pid();
    let mut my_env = OwnedEnv::new();
    let saved_reversed_list = my_env.run(|env| -> NifResult<SavedTerm> {
        let list_arg = args[0].in_env(env);
        Ok(my_env.save(list_arg.list_reverse()?))
    })?;

    thread::spawn(move || {
        my_env.send(&pid, |env| {
            let result: NifResult<NifTerm> = (|| {
                let reversed_list = saved_reversed_list.load(env);
                let iter: NifListIterator = try!(reversed_list.decode());

                let empty_list = Vec::<NifTerm>::new().encode(env);
                let mut all_sublists = vec![empty_list];

                for element in iter {
                    for i in 0 .. all_sublists.len() {
                        let new_list = all_sublists[i].list_prepend(element);
                        all_sublists.push(new_list);
                    }
                }

                Ok(all_sublists.encode(env))
            })();

            match result {
                Err(_err) => env.error_tuple("test failed".encode(env)),
                Ok(term) => term
            }
        });
    });

    Ok(atom::ok().to_term(env))
}
