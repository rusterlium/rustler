use ::{NifTerm};
use ::wrapper::check;

pub enum TermType {
    Atom,
    Binary,
    EmptyList,
    Exception,
    Fun,
    List,
    Map,
    Pid,
    Port,
    Ref,
    Tuple,
    Unknown
}

pub fn get_type(term: NifTerm) -> TermType {
    let c_term = term.as_c_arg();
    let c_env = term.get_env().as_c_arg();

    if check::is_atom(c_env, c_term) {
        TermType::Atom
    } else if check::is_binary(c_env, c_term) {
        TermType::Binary
    } else if check::is_empty_list(c_env, c_term) {
        TermType::EmptyList
    } else if check::is_exception(c_env, c_term) {
        TermType::Exception
    } else if check::is_fun(c_env, c_term) {
        TermType::Fun
    } else if check::is_list(c_env, c_term) {
        TermType::List
    } else if check::is_map(c_env, c_term) {
        TermType::Map
    } else if check::is_pid(c_env, c_term) {
        TermType::Pid
    } else if check::is_port(c_env, c_term) {
        TermType::Port
    } else if check::is_ref(c_env, c_term) {
        TermType::Ref
    } else if check::is_tuple(c_env, c_term) {
        TermType::Tuple
    } else {
        TermType::Unknown
    }
}

macro_rules! impl_check {
    ($check_fun:ident) => {
        pub fn $check_fun(self) -> bool {
            check::$check_fun(self.get_env().as_c_arg(), self.as_c_arg())
        }
    }
}

/// ## Type checks
impl<'a> NifTerm<'a> {

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
    impl_check!(is_pid);
    impl_check!(is_port);
    impl_check!(is_ref);
    impl_check!(is_tuple);

}
