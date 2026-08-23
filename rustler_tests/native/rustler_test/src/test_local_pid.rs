use std::cmp::Ordering;
use std::hash::{DefaultHasher, Hash, Hasher};

use rustler::LocalPid;

#[rustler::nif]
pub fn compare_local_pids(lhs: LocalPid, rhs: LocalPid) -> i32 {
    match lhs.cmp(&rhs) {
        Ordering::Less => -1,
        Ordering::Equal => 0,
        Ordering::Greater => 1,
    }
}

#[rustler::nif]
pub fn are_equal_local_pids(lhs: LocalPid, rhs: LocalPid) -> bool {
    lhs == rhs
}

#[rustler::nif]
pub fn pid_hash(pid: LocalPid) -> u64 {
    let mut s = DefaultHasher::new();

    pid.hash(&mut s);

    return s.finish();
}
