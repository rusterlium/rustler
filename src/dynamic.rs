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

pub fn get_type(term: &NifTerm) -> TermType {
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
