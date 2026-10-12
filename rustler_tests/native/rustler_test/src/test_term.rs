use rustler::{
    terms, Atom, BoundTerm, Decoder, Encoder, Env, Error, FreeTerm, NifResult, OwnedEnv, Term,
    TypedTerm,
};
use std::cmp::Ordering;
use std::io::Write;

mod atoms {
    rustler::atoms! {
        equal,
        less,
        greater,
        // Term types
        atom,
        binary,
        bitstring,
        float,
        fun,
        integer,
        list,
        local_pid,
        map,
        pid,
        port,
        reference,
        tuple,
        unknown,
    }
}

#[rustler::nif]
pub fn typed_term_roundtrip<'a>(
    env: Env<'a>,
    term: TypedTerm<'a>,
) -> NifResult<(Atom, TypedTerm<'a>)> {
    let mut owned = OwnedEnv::new();
    let result = owned.run(|other_env| -> NifResult<(Atom, TypedTerm<'a>)> {
        let typed: TypedTerm = term.encode(other_env).decode()?;
        let typed = unsafe { TypedTerm::from_raw(typed.as_c_arg(), other_env) };
        let kind = match typed {
            TypedTerm::Atom(_) => atoms::atom(),
            TypedTerm::Binary(_) => atoms::binary(),
            TypedTerm::Bitstring(_) => atoms::bitstring(),
            TypedTerm::Float(_) => atoms::float(),
            TypedTerm::Fun(_) => atoms::fun(),
            TypedTerm::Integer(_) => atoms::integer(),
            TypedTerm::List(_) => atoms::list(),
            TypedTerm::LocalPid(_) => atoms::local_pid(),
            TypedTerm::Map(_) => atoms::map(),
            TypedTerm::Pid(_) => atoms::pid(),
            TypedTerm::Port(_) => atoms::port(),
            TypedTerm::Reference(_) => atoms::reference(),
            TypedTerm::Tuple(_) => atoms::tuple(),
        };
        Ok((kind, TypedTerm::try_from(typed.encode(env))?))
    })?;
    owned.clear();
    drop(owned);
    Ok(result)
}

fn raw_roundtrip<'a, T>(term: Term<'a>) -> NifResult<Term<'a>>
where
    T: BoundTerm<'a> + Decoder<'a> + Encoder,
{
    let wrapper: T = term.decode()?;
    let wrapper = unsafe { T::from_raw(wrapper.as_c_arg(), term.get_env()) };
    assert_eq!(unsafe { wrapper.to_term(term.get_env()) }, term);
    Ok(wrapper.encode(term.get_env()))
}

#[rustler::nif]
pub fn raw_bound_copy<'a>(term: Term<'a>) -> NifResult<Term<'a>> {
    let mut owned = OwnedEnv::new();
    let copied = owned.run(|other_env| -> NifResult<Term<'a>> {
        let wrapper: terms::Tuple = term.in_env(other_env).decode()?;
        Ok(wrapper.encode(term.get_env()))
    })?;
    owned.clear();
    drop(owned);
    Ok(copied)
}

#[rustler::nif]
pub fn raw_term_roundtrip<'a>(kind: Atom, term: Term<'a>) -> NifResult<Term<'a>> {
    if kind == atoms::atom() {
        raw_roundtrip::<terms::Atom>(term)
    } else if kind == atoms::binary() {
        raw_roundtrip::<terms::Binary>(term)
    } else if kind == atoms::bitstring() {
        #[cfg(feature = "nif_version_2_15")]
        return raw_roundtrip::<terms::Bitstring>(term);
        #[cfg(not(feature = "nif_version_2_15"))]
        return Err(Error::BadArg);
    } else if kind == atoms::float() {
        raw_roundtrip::<terms::Float>(term)
    } else if kind == atoms::fun() {
        raw_roundtrip::<terms::Fun>(term)
    } else if kind == atoms::integer() {
        raw_roundtrip::<terms::Integer>(term)
    } else if kind == atoms::list() {
        raw_roundtrip::<terms::List>(term)
    } else if kind == atoms::local_pid() {
        raw_roundtrip::<terms::LocalPid>(term)
    } else if kind == atoms::map() {
        raw_roundtrip::<terms::Map>(term)
    } else if kind == atoms::pid() {
        raw_roundtrip::<terms::Pid>(term)
    } else if kind == atoms::port() {
        raw_roundtrip::<terms::Port>(term)
    } else if kind == atoms::reference() {
        raw_roundtrip::<terms::Reference>(term)
    } else if kind == atoms::tuple() {
        raw_roundtrip::<terms::Tuple>(term)
    } else {
        Err(Error::BadArg)
    }
}

fn free_after_clear<'a, T>(env: Env<'a>, term: Term<'a>) -> NifResult<Term<'a>>
where
    T: FreeTerm + for<'b> Decoder<'b>,
{
    let mut owned = OwnedEnv::new();
    let wrapper: T = owned.run(|other_env| term.in_env(other_env).decode())?;
    owned.clear();
    drop(owned);
    Ok(wrapper.in_env(env))
}

#[rustler::nif]
pub fn free_term_after_clear<'a>(env: Env<'a>, kind: Atom, term: Term<'a>) -> NifResult<Term<'a>> {
    if kind == atoms::atom() {
        free_after_clear::<terms::Atom>(env, term)
    } else if kind == atoms::local_pid() {
        free_after_clear::<terms::LocalPid>(env, term)
    } else {
        Err(Error::BadArg)
    }
}

#[rustler::nif]
pub fn typed_map_operation<'a>(
    env: Env<'a>,
    operation: &str,
    map: terms::Map<'a>,
    key: Term<'a>,
    value: Term<'a>,
) -> NifResult<Term<'a>> {
    let mut owned = OwnedEnv::new();
    let result = owned.run(|other_env| -> NifResult<Term<'a>> {
        let map: terms::Map = map.encode(other_env).decode()?;
        match operation {
            "get" => map.get(env, key),
            "size" => Ok(map.size(env)?.encode(env)),
            "put" => Ok(map.put(env, key, value)?.encode(env)),
            "remove" => Ok(map.remove(env, key)?.encode(env)),
            "update" => Ok(map.update(env, key, value)?.encode(env)),
            "entries" => Ok(map.into_iterator(env).collect::<Vec<_>>().encode(env)),
            _ => Err(Error::BadArg),
        }
    })?;
    owned.clear();
    Ok(result)
}

#[rustler::nif]
pub fn typed_map_build<'a>(
    env: Env<'a>,
    keys: Vec<Term<'a>>,
    values: Vec<Term<'a>>,
) -> NifResult<(
    terms::Map<'a>,
    terms::Map<'a>,
    terms::Map<'a>,
    terms::Map<'a>,
)> {
    let arrays = terms::Map::from_arrays(env, &keys, &values)?;
    let pairs: Vec<_> = keys.iter().zip(&values).collect();
    let mut owned = OwnedEnv::new();
    let term_arrays = owned.run(|other_env| {
        let keys: Vec<_> = keys.iter().map(|term| term.in_env(other_env)).collect();
        let values: Vec<_> = values.iter().map(|term| term.in_env(other_env)).collect();
        terms::Map::from_term_arrays(env, &keys, &values)
    })?;
    owned.clear();
    Ok((
        terms::Map::new(env),
        arrays,
        term_arrays,
        terms::Map::from_pairs(env, &pairs)?,
    ))
}

#[rustler::nif]
pub fn typed_list_operation<'a>(
    env: Env<'a>,
    operation: &str,
    list: terms::List<'a>,
    head: Term<'a>,
) -> NifResult<Term<'a>> {
    let mut owned = OwnedEnv::new();
    let result = owned.run(|other_env| -> NifResult<Term<'a>> {
        let list: terms::List = list.encode(other_env).decode()?;
        match operation {
            "empty" => Ok(terms::List::new_empty(env).encode(env)),
            "length" => Ok(list.length(env)?.encode(env)),
            "cell" => Ok(list.get_cell(env)?.encode(env)),
            "reverse" => Ok(list.reverse(env)?.encode(env)),
            "prepend" => Ok(list.prepend(env, head).encode(env)),
            "entries" => Ok(list.into_iterator(env).collect::<Vec<_>>().encode(env)),
            _ => Err(Error::BadArg),
        }
    })?;
    owned.clear();
    Ok(result)
}

#[rustler::nif]
pub fn typed_term_views<'a>(
    env: Env<'a>,
    atom: terms::Atom,
    binary: terms::Binary<'a>,
    tuple: terms::Tuple<'a>,
) -> NifResult<(String, rustler::Binary<'a>, Vec<Term<'a>>, terms::Tuple<'a>)> {
    let mut owned = OwnedEnv::new();
    let atom_name = owned.run(|other_env| atom.to_string(other_env))?;
    let elements = owned.run(|other_env| -> NifResult<Vec<Term<'a>>> {
        let tuple: terms::Tuple = tuple.encode(other_env).decode()?;
        tuple.get_tuple(env)
    })?;
    let inspected = owned.run(|other_env| -> NifResult<rustler::Binary<'a>> {
        let binary: terms::Binary = binary.encode(other_env).decode()?;
        binary.into_binary(env)
    })?;
    let rebuilt = owned.run(|other_env| {
        let copied: Vec<_> = elements.iter().map(|term| term.in_env(other_env)).collect();
        terms::Tuple::make_tuple(env, &copied)
    });
    owned.clear();
    Ok((atom_name, inspected, elements, rebuilt))
}

#[rustler::nif]
pub fn typed_iterators_after_clear<'a>(
    env: Env<'a>,
    map: terms::Map<'a>,
    list: terms::List<'a>,
) -> NifResult<(Vec<(Term<'a>, Term<'a>)>, Vec<Term<'a>>)> {
    let mut owned = OwnedEnv::new();
    let (map_iter, list_iter) = owned.run(|other_env| -> NifResult<_> {
        let map: terms::Map = map.encode(other_env).decode()?;
        let list: terms::List = list.encode(other_env).decode()?;
        Ok((map.into_iterator(env), list.into_iterator(env)))
    })?;
    owned.clear();
    drop(owned);
    Ok((map_iter.collect(), list_iter.collect()))
}

#[rustler::nif]
pub fn term_debug(term: Term) -> String {
    let mut bytes: Vec<u8> = Vec::new();
    write!(&mut bytes, "{:?}", term).expect("debug formatting should succeed");
    String::from_utf8_lossy(&bytes).to_string()
}

#[rustler::nif]
pub fn term_eq<'a>(a: Term<'a>, b: Term<'a>) -> bool {
    a == b
}

#[rustler::nif]
pub fn term_cmp<'a>(a: Term<'a>, b: Term<'a>) -> Atom {
    match Ord::cmp(&a, &b) {
        Ordering::Equal => atoms::equal(),
        Ordering::Less => atoms::less(),
        Ordering::Greater => atoms::greater(),
    }
}

#[rustler::nif]
pub fn term_internal_hash(term: Term, salt: u64) -> u64 {
    term.hash_internal(salt)
}

#[rustler::nif]
pub fn term_phash2_hash(term: Term) -> u32 {
    term.hash_phash2()
}

#[rustler::nif]
pub fn term_type(term: Term) -> Atom {
    match term.get_type() {
        rustler::TermType::Atom => atoms::atom(),
        rustler::TermType::Binary => atoms::binary(),
        rustler::TermType::Fun => atoms::fun(),
        rustler::TermType::List => atoms::list(),
        rustler::TermType::Map => atoms::map(),
        rustler::TermType::Integer => atoms::integer(),
        rustler::TermType::Float => atoms::float(),
        rustler::TermType::Pid => atoms::pid(),
        rustler::TermType::Port => atoms::port(),
        rustler::TermType::Ref => atoms::reference(),
        rustler::TermType::Tuple => atoms::tuple(),
        rustler::TermType::Unknown => atoms::unknown(),
    }
}

#[cfg(feature = "nif_version_2_18")]
#[rustler::nif]
pub fn term_size(term: Term) -> usize {
    term.size()
}
