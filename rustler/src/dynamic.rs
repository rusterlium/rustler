use crate::wrapper::check;
use crate::Term;

#[derive(Debug)]
pub enum TermType {
    Atom,
    Binary,
    EmptyList,
    Exception,
    Fun,
    List,
    Map,
    Number,
    Pid,
    Port,
    Ref,
    Tuple,
    Unknown,
}

pub fn get_type(term: Term) -> TermType {
    if term.is_atom() {
        TermType::Atom
    } else if term.is_binary() {
        TermType::Binary
    } else if term.is_empty_list() {
        TermType::EmptyList
    } else if term.is_exception() {
        TermType::Exception
    } else if term.is_fun() {
        TermType::Fun
    } else if term.is_list() {
        TermType::List
    } else if term.is_map() {
        TermType::Map
    } else if term.is_number() {
        TermType::Number
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
    impl_check!(is_exception);
    impl_check!(is_fun);
    impl_check!(is_list);
    impl_check!(is_map);
    impl_check!(is_number);
    impl_check!(is_pid);
    impl_check!(is_port);
    impl_check!(is_ref);
    impl_check!(is_tuple);
}
