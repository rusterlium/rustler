use std::ffi::c_double;

#[cfg(feature = "nif_version_2_15")]
use rustler_sys::ErlNifTermType;

use crate::wrapper::check;
use crate::Term;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TermType {
    Atom,
    Binary,
    Fun,
    List,
    Map,
    Integer,
    Float,
    Pid,
    Port,
    Ref,
    Tuple,
    Unknown,
}

#[cfg(feature = "nif_version_2_15")]
impl From<ErlNifTermType> for TermType {
    fn from(term_type: ErlNifTermType) -> Self {
        use ErlNifTermType::*;
        use TermType::*;
        match term_type {
            ERL_NIF_TERM_TYPE_ATOM => Atom,
            ERL_NIF_TERM_TYPE_BITSTRING => Binary,
            ERL_NIF_TERM_TYPE_FLOAT => Float,
            ERL_NIF_TERM_TYPE_FUN => Fun,
            ERL_NIF_TERM_TYPE_INTEGER => Integer,
            ERL_NIF_TERM_TYPE_LIST => List,
            ERL_NIF_TERM_TYPE_MAP => Map,
            ERL_NIF_TERM_TYPE_PID => Pid,
            ERL_NIF_TERM_TYPE_PORT => Port,
            ERL_NIF_TERM_TYPE_REFERENCE => Ref,
            ERL_NIF_TERM_TYPE_TUPLE => Tuple,
            _ => Unknown,
        }
    }
}

pub fn get_type(term: Term) -> TermType {
    if cfg!(feature = "nif_version_2_15") && !cfg!(target_family = "windows") {
        term.get_erl_type().into()
    } else if term.is_atom() {
        TermType::Atom
    } else if term.is_binary() {
        TermType::Binary
    } else if term.is_fun() {
        TermType::Fun
    } else if term.is_list() || term.is_empty_list() {
        TermType::List
    } else if term.is_map() {
        TermType::Map
    } else if term.is_number() {
        if term.is_float() {
            TermType::Float
        } else {
            TermType::Integer
        }
    } else if term.is_pid() {
        TermType::Pid
    } else if term.is_port() {
        TermType::Port
    } else if term.is_ref() {
        TermType::Ref
    } else if term.is_tuple() {
        TermType::Tuple
    } else {
        TermType::Unknown
    }
}

macro_rules! impl_check {
    ($check_fun:ident) => {
        pub fn $check_fun(self) -> bool {
            unsafe { check::$check_fun(self.get_env().as_c_arg(), self.as_c_arg()) }
        }
    };
}

/// ## Type checks
impl<'a> Term<'a> {
    /// Returns an enum representing which type the term is.
    /// Note that using the individual `is_*` functions is more
    /// efficient for checking a single type.
    pub fn get_type(self) -> TermType {
        get_type(self)
    }

    impl_check!(is_atom);
    impl_check!(is_binary);
    impl_check!(is_empty_list);
    impl_check!(is_fun);
    impl_check!(is_list);
    impl_check!(is_map);
    impl_check!(is_number);
    impl_check!(is_pid);
    impl_check!(is_port);
    impl_check!(is_ref);
    impl_check!(is_tuple);

    pub fn is_float(self) -> bool {
        let mut val: c_double = 0.0;
        unsafe {
            rustler_sys::enif_get_double(self.get_env().as_c_arg(), self.as_c_arg(), &mut val) == 1
        }
    }

    pub fn is_integer(self) -> bool {
        self.is_number() && !self.is_float()
    }
}
